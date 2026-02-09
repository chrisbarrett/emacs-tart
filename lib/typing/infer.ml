(** Constraint-based type inference.

    This module generates type constraints for Elisp expressions. The main entry
    point is [infer], which takes an expression and environment and returns
    (type, constraints).

    Constraints are equality constraints τ₁ = τ₂ that are later solved by
    unification. *)

open Core.Types
module Env = Core.Type_env
module C = Constraint
module G = Generalize
module Loc = Syntax.Location
module Sig_parser = Sig.Sig_parser
module Sig_loader = Sig.Sig_loader
module Sig_ast = Sig.Sig_ast
include Infer_types

(** Apply a single predicate narrowing to an env, returning (then_env,
    else_env). *)
let apply_predicate_narrowing (pred : Narrow.predicate_info) (env : Env.t) :
    Env.t * Env.t =
  match Env.lookup pred.var_name env with
  | Some scheme ->
      let original_ty = Env.instantiate scheme env in
      let then_ty = Narrow.narrow_type original_ty pred.narrowed_type in
      let else_ty = subtract_type original_ty pred.narrowed_type in
      ( Env.with_narrowed_var pred.var_name then_ty env,
        Env.with_narrowed_var pred.var_name else_ty env )
  | None -> (env, env)

(** Apply a single predicate narrowing to produce only the then-env. *)
let apply_predicate_then (pred : Narrow.predicate_info) (env : Env.t) : Env.t =
  match Env.lookup pred.var_name env with
  | Some scheme ->
      let original_ty = Env.instantiate scheme env in
      let then_ty = Narrow.narrow_type original_ty pred.narrowed_type in
      Env.with_narrowed_var pred.var_name then_ty env
  | None -> env

(** Apply a single predicate narrowing to produce only the else-env. *)
let apply_predicate_else (pred : Narrow.predicate_info) (env : Env.t) : Env.t =
  match Env.lookup pred.var_name env with
  | Some scheme ->
      let original_ty = Env.instantiate scheme env in
      let else_ty = subtract_type original_ty pred.narrowed_type in
      Env.with_narrowed_var pred.var_name else_ty env
  | None -> env

(** Apply a single feature guard to extend the then-env (Spec 49).

    - [FeatureGuard name]: loads all names from [name.tart] via the feature
      loader
    - [FboundGuard name]: loads the module for [name] (via prefix search), then
      exposes only function [name]
    - [BoundGuard name]: loads the module for [name], then exposes only variable
      [name]
    - [BoundTrueGuard name]: adds variable [name] with type [t] (truthy)

    The else-env is always unchanged (guards do not subtract from the env). *)
let apply_guard (guard : Narrow.guard_info) (env : Env.t) : Env.t =
  match guard with
  | Narrow.FeatureGuard name -> Env.load_feature name env
  | Narrow.FboundGuard name -> (
      (* Load the module for this function, then expose only this function *)
      let loaded_env = Env.load_feature name env in
      match Env.lookup_fn name loaded_env with
      | Some scheme -> Env.extend_fn name scheme env
      | None -> env)
  | Narrow.BoundGuard name -> (
      (* Load the module for this variable, then expose only this variable *)
      let loaded_env = Env.load_feature name env in
      match Env.lookup_var name loaded_env with
      | Some scheme -> Env.extend name scheme env
      | None -> (
          (* Also try function namespace for backward compat *)
          match Env.lookup_fn name loaded_env with
          | Some scheme -> Env.extend name scheme env
          | None -> env))
  | Narrow.BoundTrueGuard name ->
      (* Variable is bound and non-nil: type is t (truthy) *)
      Env.extend_mono name Prim.t env

(** Collect variable names whose binding is being tested by guards.

    These are names that appear in guard conditions like (bound-and-true-p v) or
    (boundp 'v). The checker should not flag these as undefined in the condition
    expression, since the whole point of the guard is to test whether they are
    bound. *)
let guard_suppressed_names (analysis : Narrow.condition_analysis) : string list
    =
  let from_guard = function
    | Narrow.BoundTrueGuard name -> [ name ]
    | Narrow.BoundGuard _name -> []
    | Narrow.FeatureGuard _ | Narrow.FboundGuard _ -> []
  in
  match analysis with
  | Narrow.Guard g -> from_guard g
  | Narrow.Guards gs -> List.concat_map from_guard gs
  | Narrow.PredicatesAndGuards (_, gs) -> List.concat_map from_guard gs
  | Narrow.Predicate _ | Narrow.Predicates _ | Narrow.NoPredicate -> []

(** Narrow an env for then/else branches based on condition analysis result.
    Returns [(then_env, else_env)]. *)
let narrow_env_from_analysis (analysis : Narrow.condition_analysis)
    (env : Env.t) : Env.t * Env.t =
  match analysis with
  | Narrow.Predicate pred -> apply_predicate_narrowing pred env
  | Narrow.Predicates preds ->
      let then_env =
        List.fold_left (fun e p -> apply_predicate_then p e) env preds
      in
      let else_env =
        List.fold_left (fun e p -> apply_predicate_else p e) env preds
      in
      (then_env, else_env)
  | Narrow.Guard guard ->
      let then_env = apply_guard guard env in
      (then_env, env)
  | Narrow.Guards guards ->
      let then_env = List.fold_left (fun e g -> apply_guard g e) env guards in
      (then_env, env)
  | Narrow.PredicatesAndGuards (preds, guards) ->
      let then_env =
        List.fold_left (fun e p -> apply_predicate_then p e) env preds
      in
      let then_env =
        List.fold_left (fun e g -> apply_guard g e) then_env guards
      in
      let else_env =
        List.fold_left (fun e p -> apply_predicate_else p e) env preds
      in
      (then_env, else_env)
  | Narrow.NoPredicate -> (env, env)

(** Narrow an env for then-branch only based on condition analysis. *)
let narrow_then_from_analysis (analysis : Narrow.condition_analysis)
    (env : Env.t) : Env.t =
  fst (narrow_env_from_analysis analysis env)

(** Narrow an env for else-branch only based on condition analysis. *)
let narrow_else_from_analysis (analysis : Narrow.condition_analysis)
    (env : Env.t) : Env.t =
  snd (narrow_env_from_analysis analysis env)

(** Infer the type of an S-expression.

    This generates constraints but does not solve them. Call [Unify.solve] on
    the constraints to unify type variables.

    Expressions handled:
    - Literals: produce base types (no constraints)
    - Variables: instantiate from environment (no constraints)
    - Lambda: introduce fresh type vars for params
    - Application: generate τ_fun = (τ_args...) -> τ_result
    - Quote: quoted expressions have type based on their structure
    - If: branches must unify; result is their common type
    - Let: generate constraints for bindings (generalization in R5)
    - Progn: result is type of last expression *)

(** Result of attempting union function dispatch (Spec 34 R11). *)
type union_fn_result =
  | UnionNotApplicable
      (** Function type is not a union of arrows — caller should use standard
          path *)
  | UnionMatched of typ  (** All variants accepted args; carries return type *)
  | UnionFailed of (param list * typ) list
      (** Function IS a union of arrows, but args don't satisfy all variants.
          Carries the arrow variants for error constraint generation. *)

(** {2 Row container helpers}

    Helpers for row-aware clause dispatch. Detect row-typed containers and
    literal keys from call-site argument types, and build expected container
    types for virtual clause generation. *)

(** Which kind of row-typed container was detected at the call site. *)
type container_kind = Plist | Alist | HashTable | Map

type clause_config = {
  cc_kind : container_kind;
  cc_container_index : int;
  cc_key_index : int;
}
(** Configuration derived from clause analysis for row dispatch. *)

let extract_plist_row ty =
  let plist_name = intrinsic "Plist" in
  match repr ty with
  | TApp (plist_con, [ _key_ty; value_ty ])
    when equal (repr plist_con) (TCon plist_name) -> (
      match repr value_ty with TRow row -> Some row | _ -> None)
  | TApp (list_con, [ union_ty ])
    when equal (repr list_con) (TCon (intrinsic "List")) -> (
      match repr union_ty with
      | TUnion members -> (
          let has_keyword =
            List.exists
              (fun m ->
                match repr m with
                | TCon n -> n = intrinsic "Keyword"
                | _ -> false)
              members
          in
          let row_member =
            List.find_map
              (fun m -> match repr m with TRow row -> Some row | _ -> None)
              members
          in
          match (has_keyword, row_member) with
          | true, Some row -> Some row
          | _ -> None)
      | _ -> None)
  | _ -> None

let extract_alist_row ty =
  match repr ty with
  | TApp (list_con, [ TApp (pair_con, [ _key_ty; value_ty ]) ])
    when equal (repr list_con) (TCon (intrinsic "List"))
         && equal (repr pair_con) (TCon (intrinsic "Pair")) -> (
      match repr value_ty with TRow row -> Some row | _ -> None)
  | _ -> None

let extract_hash_table_row ty =
  let ht_name = intrinsic "HashTable" in
  match repr ty with
  | TApp (ht_con, [ _key_ty; value_ty ]) when equal (repr ht_con) (TCon ht_name)
    -> (
      match repr value_ty with TRow row -> Some row | _ -> None)
  | _ -> None

let extract_map_row ty =
  let map_name = intrinsic "Map" in
  match repr ty with
  | TApp (map_con, [ value_ty ]) when equal (repr map_con) (TCon map_name) -> (
      match repr value_ty with TRow row -> Some row | _ -> None)
  | _ -> None

let container_extractors =
  [
    (extract_plist_row, Plist);
    (extract_alist_row, Alist);
    (extract_hash_table_row, HashTable);
    (extract_map_row, Map);
  ]

let detect_container_in_type ty =
  List.find_map
    (fun (extract, kind) -> Option.map (fun row -> (kind, row)) (extract ty))
    container_extractors

let build_expected_container kind row =
  match kind with
  | Plist -> plist_of Prim.keyword row
  | Alist -> list_of (pair_of Prim.symbol row)
  | HashTable -> hash_table_of Prim.symbol row
  | Map -> map_of row

let has_default kind =
  match kind with Plist -> false | Alist | HashTable | Map -> true

let detect_container_kind_in_param (ty : typ) : container_kind option =
  match ty with
  | TApp (TCon name, [ _; _ ]) when name = intrinsic "Plist" -> Some Plist
  | TApp (TCon list_name, [ TApp (TCon pair_name, [ _; _ ]) ])
    when list_name = intrinsic "List" && pair_name = intrinsic "Pair" ->
      Some Alist
  | TApp (TCon name, [ _; _ ]) when name = intrinsic "HashTable" ->
      Some HashTable
  | TApp (TCon name, [ _ ]) when name = intrinsic "Map" -> Some Map
  | _ -> None

let analyze_params (params : param list) : clause_config option =
  let rec find_container i = function
    | [] -> None
    | param :: rest -> (
        let ty =
          match param with
          | PPositional t | POptional t -> Some t
          | PRest _ | PKey _ | PLiteral _ -> None
        in
        match ty with
        | Some t -> (
            match detect_container_kind_in_param t with
            | Some kind -> Some (kind, i)
            | None -> find_container (i + 1) rest)
        | None -> find_container (i + 1) rest)
  in
  match find_container 0 params with
  | None -> None
  | Some (kind, container_index) ->
      let key_index =
        let rec find_key i = function
          | [] -> None
          | _ :: rest when i = container_index -> find_key (i + 1) rest
          | (PPositional _ | PLiteral _) :: _ -> Some i
          | _ :: rest -> find_key (i + 1) rest
        in
        find_key 0 params
      in
      Option.map
        (fun ki ->
          {
            cc_kind = kind;
            cc_container_index = container_index;
            cc_key_index = ki;
          })
        key_index

let analyze_clause (clause : Env.loaded_clause) : clause_config option =
  analyze_params clause.lc_params

(** Normalize a clause parameter to [PPositional] when the call-site provides an
    argument at the given index.

    Virtual clause params must use [PPositional] for positions with call-site
    arguments because clause dispatch wraps all call-site args as [PPositional]
    via [check_literals]. Mixing [POptional] clause params with [PPositional]
    args causes spurious unification failures. Params beyond the arg count keep
    their original kind so clause dispatch handles missing optional args
    correctly. *)
let positional_if_supplied ~(num_args : int) (i : int) (p : param) : param =
  if i < num_args then
    match p with POptional t | PKey (_, t) -> PPositional t | _ -> p
  else p

(** Generate virtual clauses from row fields for row-aware clause dispatch.

    When a clause has a row-typed container parameter and the call-site argument
    has a concrete row type, generates virtual clauses implementing the Spec 11
    R4 decision table:

    - Cases 1-2: Per-field clauses with [PLiteral] key and exact field return
      type for each field present in the row.
    - Case 3: Literal key absent from closed row, no default → returns [nil].
    - Case 4: Literal key absent from closed row, with default → returns the
      default argument's type.
    - Case 5: Literal key absent from open row → generates a clause with an
      extended row containing the key, returning [(T | nil)].
    - R8: Container type unknown (type variable) with literal key → generates a
      clause constraining the container to an open row containing the key.

    [arg_literals] provides the literal values of call-site arguments.
    [arg_types] provides the inferred types of call-site arguments.

    Virtual clause params use [PPositional] for all non-literal positions
    (including params that were [POptional] in the original clause). Clause
    dispatch wraps call-site args as [PPositional], so the param kinds must
    match to avoid spurious unification failures. *)
let generate_virtual_clauses (env : Env.t) (clauses : Env.loaded_clause list)
    (arg_types : typ list) (arg_literals : string option list) :
    Env.loaded_clause list =
  match clauses with
  | [ clause ] -> (
      match analyze_clause clause with
      | None -> []
      | Some config -> (
          let container_index = config.cc_container_index in
          let key_index = config.cc_key_index in
          if container_index >= List.length arg_types then []
          else
            let container_ty = List.nth arg_types container_index in
            match detect_container_in_type container_ty with
            | Some (kind, row) ->
                (* Cases 1-2: per-field clauses for fields present in the row *)
                let field_clauses =
                  List.filter_map
                    (fun (field_name, field_ty) ->
                      let row_var = fresh_tvar (Env.current_level env) in
                      let virtual_row =
                        open_row [ (field_name, field_ty) ] row_var
                      in
                      let container_param =
                        build_expected_container kind virtual_row
                      in
                      let params =
                        List.mapi
                          (fun i p ->
                            if i = container_index then
                              PPositional container_param
                            else if i = key_index then PLiteral field_name
                            else
                              positional_if_supplied
                                ~num_args:(List.length arg_types) i p)
                          clause.lc_params
                      in
                      Some
                        {
                          Env.lc_params = params;
                          lc_return = field_ty;
                          lc_diagnostic = None;
                        })
                    row.row_fields
                in
                (* Cases 3-5: absent-key clauses when a literal key is not in
                   the row. Detect the call-site key literal and check whether
                   it appears among the row's fields. *)
                let absent_key_clauses =
                  let key_literal =
                    if key_index < List.length arg_literals then
                      List.nth arg_literals key_index
                    else None
                  in
                  match key_literal with
                  | None -> [] (* No literal key — skip absent-key handling *)
                  | Some key_name -> (
                      let key_in_row =
                        List.exists (fun (n, _) -> n = key_name) row.row_fields
                      in
                      if key_in_row then []
                        (* Key found — Cases 1-2 clauses handle it *)
                      else
                        (* Key absent from row *)
                        let max_idx = max container_index key_index in
                        let rest_arg_types =
                          List.filteri (fun i _ -> i > max_idx) arg_types
                        in
                        match row.row_var with
                        | None ->
                            (* Cases 3-4: closed row, key absent *)
                            let result_ty =
                              if has_default kind then
                                match rest_arg_types with
                                | default_ty :: _ -> default_ty
                                | [] -> Prim.nil
                              else Prim.nil
                            in
                            let params =
                              List.mapi
                                (fun i p ->
                                  if i = container_index then
                                    PPositional container_ty
                                  else if i = key_index then PLiteral key_name
                                  else
                                    positional_if_supplied
                                      ~num_args:(List.length arg_types) i p)
                                clause.lc_params
                            in
                            [
                              {
                                Env.lc_params = params;
                                lc_return = result_ty;
                                lc_diagnostic = None;
                              };
                            ]
                        | Some _ ->
                            (* Case 5: open row, key absent — extend row with
                               {key: T & r'} and return (T | nil) *)
                            let field_ty = fresh_tvar (Env.current_level env) in
                            let row_var = fresh_tvar (Env.current_level env) in
                            let virtual_row =
                              open_row [ (key_name, field_ty) ] row_var
                            in
                            let container_param =
                              build_expected_container kind virtual_row
                            in
                            let params =
                              List.mapi
                                (fun i p ->
                                  if i = container_index then
                                    PPositional container_param
                                  else if i = key_index then PLiteral key_name
                                  else
                                    positional_if_supplied
                                      ~num_args:(List.length arg_types) i p)
                                clause.lc_params
                            in
                            [
                              {
                                Env.lc_params = params;
                                lc_return = option_of field_ty;
                                lc_diagnostic = None;
                              };
                            ])
                in
                field_clauses @ absent_key_clauses
            | None -> []))
  | _ -> []

(** Look up stored clauses and type variable names for a function.

    Returns [Some (clauses, tvar_names)] when the function has multi-clause
    overloads stored in the environment. This is the common lookup pattern used
    by clause dispatch and virtual clause generation. *)
let lookup_stored_clauses_and_vars (name : string) (env : Env.t) :
    (Env.loaded_clause list * string list) option =
  match Env.lookup_fn_clauses name env with
  | Some clauses -> (
      match Env.lookup_fn name env with
      | Some (Env.Poly (vars, _)) -> Some (clauses, vars)
      | Some (Env.Mono _) -> Some (clauses, [])
      | None -> None)
  | None -> None

(** Look up clauses and type variable names, synthesizing a single clause from
    the function type when no stored clauses exist.

    Extends {!lookup_stored_clauses_and_vars} by constructing a clause from the
    function's arrow type for single-clause polymorphic functions that don't
    store clause lists. Used by virtual clause generation. *)
let lookup_clauses_and_vars_with_synthesis (name : string) (env : Env.t) :
    (Env.loaded_clause list * string list) option =
  match lookup_stored_clauses_and_vars name env with
  | Some _ as result -> result
  | None -> (
      match Env.lookup_fn name env with
      | Some (Env.Poly (vars, ty)) -> (
          match ty with
          | TArrow (params, ret) | TForall (_, TArrow (params, ret)) ->
              let clause =
                {
                  Env.lc_params = params;
                  lc_return = ret;
                  lc_diagnostic = None;
                }
              in
              Some ([ clause ], vars)
          | _ -> None)
      | _ -> None)

let rec infer (env : Env.t) (sexp : Syntax.Sexp.t) : result =
  let open Syntax.Sexp in
  match sexp with
  (* === Literals === *)
  | Int (n, _) -> pure (TLiteral (LitInt n, Prim.int))
  | Float (f, _) -> pure (TLiteral (LitFloat f, Prim.float))
  | String (s, _) -> pure (TLiteral (LitString s, Prim.string))
  | Char (_, _) -> pure Prim.int (* Characters are integers in Elisp *)
  | Keyword (k, _) -> pure (TLiteral (LitKeyword k, Prim.keyword))
  (* === Special symbols nil and t === *)
  | Symbol ("nil", _) -> pure Prim.nil
  | Symbol ("t", _) -> pure Prim.t
  (* === Variables === *)
  | Symbol (name, span) -> (
      match Env.lookup name env with
      | Some scheme ->
          (* Instantiate polymorphic types with fresh type variables *)
          let ty = Env.instantiate scheme env in
          pure ty
      | None ->
          (* Unbound variable - track as undefined and give fresh type var
             so inference can continue and find more errors *)
          with_undefined (fresh_tvar (Env.current_level env)) name span)
  (* === Quoted expressions === *)
  | List ([ Symbol ("quote", _); quoted ], _) -> infer_quoted quoted
  (* === Function reference: #'name or (function name) === *)
  | List ([ Symbol ("function", _); Symbol (name, span) ], _) -> (
      match Env.lookup_fn name env with
      | Some scheme ->
          (* Look up in function namespace and instantiate *)
          let ty = Env.instantiate scheme env in
          pure ty
      | None -> (
          (* Fall back to variable namespace for backward compatibility *)
          match Env.lookup name env with
          | Some scheme ->
              let ty = Env.instantiate scheme env in
              pure ty
          | None ->
              (* Unbound function - track as undefined *)
              with_undefined (fresh_tvar (Env.current_level env)) name span))
  (* === Function reference with lambda: #'(lambda ...) === *)
  | List
      ( [ Symbol ("function", _); (List (Symbol ("lambda", _) :: _, _) as lam) ],
        _ ) ->
      infer env lam
  (* === Lambda expressions === *)
  | List (Symbol ("lambda", _) :: List (params, _) :: body, span) ->
      infer_lambda env params body span
  (* === If expressions === *)
  | List ([ Symbol ("if", _); cond; then_branch; else_branch ], span) ->
      infer_if env cond then_branch else_branch span
  | List ([ Symbol ("if", _); cond; then_branch ], span) ->
      (* If without else returns nil when condition is false *)
      infer_if_no_else env cond then_branch span
  (* === Let expressions === *)
  | List (Symbol ("let", _) :: List (bindings, _) :: body, span) ->
      infer_let env bindings body span
  | List (Symbol ("let*", _) :: List (bindings, _) :: body, span) ->
      infer_let_star env bindings body span
  (* === Defun - returns symbol, but binds function type === *)
  | List
      (Symbol ("defun", _) :: Symbol (name, _) :: List (params, _) :: body, span)
    ->
      infer_defun_as_expr env name params body span
  (* === Progn (implicit in many forms) === *)
  | List (Symbol ("progn", _) :: exprs, span) -> infer_progn env exprs span
  (* === Setq - assignment === *)
  | List (Symbol ("setq", _) :: rest, span) -> infer_setq env rest span
  (* === Cond === *)
  | List (Symbol ("cond", _) :: clauses, span) -> infer_cond env clauses span
  (* === And/Or === *)
  | List ([ Symbol ("and", _) ], _) ->
      pure Prim.t (* (and) with no args returns t *)
  | List (Symbol ("and", _) :: args, span) -> infer_and env args span
  | List ([ Symbol ("or", _) ], _) ->
      pure Prim.nil (* (or) with no args returns nil *)
  | List (Symbol ("or", _) :: args, span) -> infer_or env args span
  (* === Not === *)
  | List ([ Symbol ("not", _); arg ], span) -> infer_not env arg span
  (* === Tart explicit instantiation: (tart [T1 T2 ...] fn args...) === *)
  | List (Symbol ("tart", _) :: Vector (type_args, _) :: fn :: args, span) ->
      infer_explicit_instantiation env type_args fn args span
  (* === Tart type annotation: (tart TYPE FORM) === *)
  | List ([ Symbol ("tart", _); type_sexp; form ], span) ->
      infer_tart_annotation env type_sexp form span
  (* === Pcase pattern matching === *)
  | List (Symbol ("pcase", _) :: expr :: clauses, span) ->
      infer_pcase env expr clauses span
  | List (Symbol ("pcase-exhaustive", _) :: expr :: clauses, span) ->
      infer_pcase env expr clauses span
  (* === Pcase-let: (pcase-let ((PAT EXPR) ...) BODY...) === *)
  | List
      ( Symbol (("pcase-let" | "pcase-let*"), _) :: List (bindings, _) :: body,
        span ) ->
      infer_pcase_let env bindings body span
  (* === Funcall: (funcall f arg1 arg2 ...) === *)
  | List (Symbol ("funcall", _) :: fn_expr :: args, span) ->
      infer_funcall env fn_expr args span
  (* === Apply: (apply f arg1 arg2 ... list) === *)
  | List (Symbol ("apply", _) :: fn_expr :: args, span) when args <> [] ->
      infer_apply env fn_expr args span
  (* === When/Unless with predicate narrowing (Spec 52) === *)
  | List (Symbol ("when", _) :: cond :: body, span) ->
      infer_when env cond body span
  | List (Symbol ("unless", _) :: cond :: body, span) ->
      infer_unless env cond body span
  (* === Function application (catch-all for lists) === *)
  | List (fn :: args, span) -> infer_application env fn args span
  | List ([], _span) ->
      (* Empty list is nil *)
      pure Prim.nil
  (* === Vectors === *)
  | Vector (elems, span) -> infer_vector env elems span
  (* === Cons pairs - not valid expressions === *)
  | Cons (_, _, _span) ->
      (* Dotted pairs as expressions are typically errors,
         but we'll give them a fresh type variable *)
      pure (fresh_tvar (Env.current_level env))
  (* === Curly braces - type syntax, not valid expressions === *)
  | Curly (_, _span) -> pure (fresh_tvar (Env.current_level env))
  (* === Error nodes === *)
  | Error (_, _) -> pure (fresh_tvar (Env.current_level env))

(** Infer the type of a quoted expression.

    Quoted data has types based on structure:
    - Quoted symbol: Symbol
    - Quoted list: (List Any) for now (proper list inference later)
    - Other literals: their literal types *)
and infer_quoted (sexp : Syntax.Sexp.t) : result =
  let open Syntax.Sexp in
  match sexp with
  | Int (n, _) -> pure (TLiteral (LitInt n, Prim.int))
  | Float (f, _) -> pure (TLiteral (LitFloat f, Prim.float))
  | String (s, _) -> pure (TLiteral (LitString s, Prim.string))
  | Char (_, _) -> pure Prim.int
  | Keyword (k, _) -> pure (TLiteral (LitKeyword k, Prim.keyword))
  | Symbol (name, _) -> pure (TLiteral (LitSymbol name, Prim.symbol))
  | List (elts, _) ->
      (* Quoted lists infer as tuples with per-element types.
         Tuple-to-list subtyping (R9) ensures backward compatibility. *)
      let elem_types = List.map (fun e -> (infer_quoted e).ty) elts in
      pure (TTuple elem_types)
  | Vector (_, _) -> pure (vector_of Prim.any)
  | Curly (_, _) -> pure Prim.any (* Curly braces: Any *)
  | Cons (_, _, _) -> pure Prim.any (* Dotted pairs: Any *)
  | Error (_, _) -> pure Prim.any

(** Infer the type of a lambda expression.

    Creates fresh type variables for each parameter and infers the body type in
    the extended environment. *)
and infer_lambda env params body span =
  let open Syntax.Sexp in
  (* Create fresh type variables for each parameter *)
  let param_info =
    List.map
      (fun p ->
        match p with
        | Symbol (name, _) ->
            let tv = fresh_tvar (Env.current_level env) in
            (name, tv)
        | _ ->
            (* Non-symbol in parameter list - give it a fresh var *)
            let tv = fresh_tvar (Env.current_level env) in
            ("_", tv))
      params
  in

  (* Extend environment with parameter types *)
  let body_env = Env.extend_monos param_info env in

  (* Infer body as a progn *)
  let body_result = infer_progn body_env body span in

  (* Build function type *)
  let param_types = List.map (fun (_, ty) -> PPositional ty) param_info in
  let fn_type = TArrow (param_types, body_result.ty) in

  {
    ty = fn_type;
    constraints = body_result.constraints;
    undefineds = body_result.undefineds;
    clause_diagnostics = body_result.clause_diagnostics;
  }

(** Infer the type of an if expression with else branch.

    Generates constraint: then_type = else_type Result type is a fresh variable
    unified with both branches.

    Each branch constraint includes context about the other branch for better
    error messages when branches have incompatible types. *)
and infer_if env cond then_branch else_branch _span =
  let open Syntax.Sexp in
  let cond_result = infer env cond in

  (* Analyze condition for predicate narrowing (Spec 52) and
     feature guard env extension (Spec 49). *)
  let analysis = Narrow.analyze_condition cond env in
  let then_env, else_env = narrow_env_from_analysis analysis env in

  let then_result = infer then_env then_branch in
  let else_result = infer else_env else_branch in

  (* Result type is a fresh variable *)
  let result_ty = fresh_tvar (Env.current_level env) in

  (* Get spans for each branch *)
  let then_span = span_of then_branch in
  let else_span = span_of else_branch in

  (* Constraints: both branches unify with result, with context about the other
     branch for error messages. The then constraint goes first so if there's a
     mismatch, the error points to the else branch (which is typically where the
     user needs to look when they wrote the then branch correctly). *)
  let then_context =
    C.IfBranch
      {
        is_then = true;
        other_branch_span = else_span;
        other_branch_type = else_result.ty;
      }
  in
  let else_context =
    C.IfBranch
      {
        is_then = false;
        other_branch_span = then_span;
        other_branch_type = then_result.ty;
      }
  in
  let then_constraint =
    C.equal ~context:then_context result_ty then_result.ty then_span
  in
  let else_constraint =
    C.equal ~context:else_context result_ty else_result.ty else_span
  in

  let all_constraints =
    C.combine cond_result.constraints
      (C.combine then_result.constraints
         (C.combine else_result.constraints
            (C.add then_constraint (C.add else_constraint C.empty))))
  in
  (* Suppress undefineds for names proven by guards (e.g. bound-and-true-p v) *)
  let suppressed = guard_suppressed_names analysis in
  let cond_undefineds =
    List.filter
      (fun (u : undefined_var) -> not (List.mem u.name suppressed))
      cond_result.undefineds
  in
  let all_undefineds =
    cond_undefineds @ then_result.undefineds @ else_result.undefineds
  in

  {
    ty = result_ty;
    constraints = all_constraints;
    undefineds = all_undefineds;
    clause_diagnostics =
      cond_result.clause_diagnostics @ then_result.clause_diagnostics
      @ else_result.clause_diagnostics;
  }

(** Infer the type of an if expression without else branch.

    When else is missing, the false case returns nil. Result is (Option
    then_type) if then_type is truthy, otherwise Any. *)
and infer_if_no_else env cond then_branch span =
  let cond_result = infer env cond in

  (* Analyze condition for predicate narrowing (Spec 52) and
     feature guard env extension (Spec 49). *)
  let analysis = Narrow.analyze_condition cond env in
  let then_env = narrow_then_from_analysis analysis env in

  let then_result = infer then_env then_branch in

  (* Without else, result is union of then-type and nil.
     We represent this as a fresh variable that must unify with
     both the then-branch type and nil. *)
  let result_ty = fresh_tvar (Env.current_level env) in

  (* For now, just constrain to then-type. Proper union handling
     would require TUnion or a more sophisticated approach. *)
  let then_constraint = C.equal result_ty then_result.ty span in

  (* Note: We skip the nil constraint and rely on external knowledge
     that if-without-else can return nil. Proper handling would use
     union types: result = Or(then_type, Nil). *)
  let all_constraints =
    C.combine cond_result.constraints
      (C.combine then_result.constraints (C.add then_constraint C.empty))
  in
  (* Suppress undefineds for names proven by guards (e.g. bound-and-true-p v) *)
  let suppressed = guard_suppressed_names analysis in
  let cond_undefineds =
    List.filter
      (fun (u : undefined_var) -> not (List.mem u.name suppressed))
      cond_result.undefineds
  in
  let all_undefineds = cond_undefineds @ then_result.undefineds in

  (* The result type should really be (Or then_type Nil),
     but for now we just use the then type. *)
  {
    ty = result_ty;
    constraints = all_constraints;
    undefineds = all_undefineds;
    clause_diagnostics =
      cond_result.clause_diagnostics @ then_result.clause_diagnostics;
  }

(** Infer the type of a when expression with predicate narrowing.

    (when COND BODY...) evaluates BODY as a progn when COND is truthy. If COND
    is a predicate call like (stringp x), narrow x in the body. Result type is
    the body type (nil when condition is false is ignored for the same reason as
    infer_if_no_else). *)
and infer_when env cond body span =
  let cond_result = infer env cond in

  (* Analyze condition for predicate narrowing (Spec 52) and
     feature guard env extension (Spec 49). *)
  let analysis = Narrow.analyze_condition cond env in
  let body_env = narrow_then_from_analysis analysis env in

  let body_result = infer_progn body_env body span in

  let result_ty = fresh_tvar (Env.current_level env) in
  let body_constraint = C.equal result_ty body_result.ty span in

  let all_constraints =
    C.combine cond_result.constraints
      (C.combine body_result.constraints (C.add body_constraint C.empty))
  in
  (* Suppress undefineds for names proven by guards (e.g. bound-and-true-p v) *)
  let suppressed = guard_suppressed_names analysis in
  let cond_undefineds =
    List.filter
      (fun (u : undefined_var) -> not (List.mem u.name suppressed))
      cond_result.undefineds
  in
  let all_undefineds = cond_undefineds @ body_result.undefineds in
  {
    ty = result_ty;
    constraints = all_constraints;
    undefineds = all_undefineds;
    clause_diagnostics =
      cond_result.clause_diagnostics @ body_result.clause_diagnostics;
  }

(** Infer the type of an unless expression with predicate narrowing.

    (unless COND BODY...) evaluates BODY as a progn when COND is falsy. If COND
    is a predicate call like (stringp x), subtract string from x in the body
    (the body executes when the predicate is false). *)
and infer_unless env cond body span =
  let cond_result = infer env cond in

  (* Analyze condition for predicate narrowing (Spec 52) and
     feature guard env extension (Spec 49).
     Unless is the inverse of when: body runs when condition is false,
     so we subtract the narrowed type. *)
  let analysis = Narrow.analyze_condition cond env in
  let body_env = narrow_else_from_analysis analysis env in

  let body_result = infer_progn body_env body span in

  let result_ty = fresh_tvar (Env.current_level env) in
  let body_constraint = C.equal result_ty body_result.ty span in

  let all_constraints =
    C.combine cond_result.constraints
      (C.combine body_result.constraints (C.add body_constraint C.empty))
  in
  (* Suppress undefineds for names proven by guards *)
  let suppressed = guard_suppressed_names analysis in
  let cond_undefineds =
    List.filter
      (fun (u : undefined_var) -> not (List.mem u.name suppressed))
      cond_result.undefineds
  in
  let all_undefineds = cond_undefineds @ body_result.undefineds in
  {
    ty = result_ty;
    constraints = all_constraints;
    undefineds = all_undefineds;
    clause_diagnostics =
      cond_result.clause_diagnostics @ body_result.clause_diagnostics;
  }

(** Infer the type of a let expression with generalization.

    Let bindings use levels-based generalization: 1. Enter a new level for the
    binding scope 2. Infer each binding's type at the higher level 3. Solve
    constraints for each binding 4. Generalize type variables at level > outer
    level (if syntactic value) 5. Add generalized bindings to environment and
    infer body *)
and infer_let env bindings body span =
  let open Syntax.Sexp in
  let outer_level = Env.current_level env in
  let inner_env = Env.enter_level env in

  (* Process all bindings, inferring and solving each one *)
  let binding_schemes =
    List.map
      (fun binding ->
        match binding with
        | List ([ Symbol (name, _); expr ], _) ->
            let result = infer inner_env expr in
            (* Solve constraints immediately for this binding *)
            let _ = Unify.solve result.constraints in
            (* Generalize if it's a syntactic value *)
            let scheme = G.generalize_if_value outer_level result.ty expr in
            ( name,
              scheme,
              result.constraints,
              result.undefineds,
              result.clause_diagnostics )
        | List ([ Symbol (name, _) ], _) ->
            (* Binding without value: nil (not generalizable) *)
            (name, Env.Mono Prim.nil, C.empty, [], [])
        | _ ->
            (* Malformed binding *)
            ( "_",
              Env.Mono (fresh_tvar (Env.current_level inner_env)),
              C.empty,
              [],
              [] ))
      bindings
  in

  (* Collect constraints, undefineds, and clause diagnostics from bindings *)
  let binding_constraints =
    List.fold_left
      (fun acc (_, _, constraints, _, _) -> C.combine acc constraints)
      C.empty binding_schemes
  in
  let binding_undefineds =
    List.concat_map (fun (_, _, _, undefs, _) -> undefs) binding_schemes
  in
  let binding_clause_diags =
    List.concat_map (fun (_, _, _, _, cdiags) -> cdiags) binding_schemes
  in

  (* Extend environment with all bindings (now potentially polymorphic) *)
  let body_env =
    List.fold_left
      (fun env (name, scheme, _, _, _) -> Env.extend name scheme env)
      env binding_schemes
  in

  (* Infer body *)
  let body_result = infer_progn body_env body span in

  {
    ty = body_result.ty;
    constraints = C.combine binding_constraints body_result.constraints;
    undefineds = binding_undefineds @ body_result.undefineds;
    clause_diagnostics = binding_clause_diags @ body_result.clause_diagnostics;
  }

(** Infer the type of a let* expression with generalization.

    Each binding sees previous bindings (sequential), and each is generalized
    independently. *)
and infer_let_star env bindings body span =
  let open Syntax.Sexp in
  let outer_level = Env.current_level env in

  (* Process bindings sequentially, each in the extended environment *)
  let rec process_bindings env bindings constraints undefineds cdiags =
    match bindings with
    | [] -> (env, constraints, undefineds, cdiags)
    | binding :: rest -> (
        let inner_env = Env.enter_level env in
        match binding with
        | List ([ Symbol (name, _); expr ], _) ->
            let result = infer inner_env expr in
            (* Solve constraints immediately *)
            let _ = Unify.solve result.constraints in
            (* Generalize if syntactic value *)
            let scheme = G.generalize_if_value outer_level result.ty expr in
            let env' = Env.extend name scheme env in
            process_bindings env' rest
              (C.combine constraints result.constraints)
              (undefineds @ result.undefineds)
              (cdiags @ result.clause_diagnostics)
        | List ([ Symbol (name, _) ], _) ->
            let env' = Env.extend_mono name Prim.nil env in
            process_bindings env' rest constraints undefineds cdiags
        | _ -> process_bindings env rest constraints undefineds cdiags)
  in

  let body_env, binding_constraints, binding_undefineds, binding_cdiags =
    process_bindings env bindings C.empty [] []
  in
  let body_result = infer_progn body_env body span in

  {
    ty = body_result.ty;
    constraints = C.combine binding_constraints body_result.constraints;
    undefineds = binding_undefineds @ body_result.undefineds;
    clause_diagnostics = binding_cdiags @ body_result.clause_diagnostics;
  }

(** Infer the type of a progn expression.

    Returns the type of the last expression, or nil if empty.

    Hard require forms [(require 'X)] extend the environment for subsequent
    expressions in the progn (Spec 49 R5). Soft requires [(require 'X nil t)] do
    not extend the environment.

    Or-expressions with never-exit branches narrow subsequent code (Spec 52 R5).
    When [(or (pred x) (error "..."))] is encountered, [x] is narrowed by [pred]
    for subsequent forms because execution can only continue past the [or] if
    [pred] was truthy. *)
and infer_progn env exprs span =
  match exprs with
  | [] -> pure Prim.nil
  | [ e ] -> infer env e
  | e :: rest ->
      let e_result = infer env e in
      (* Hard require extends env for subsequent forms (Spec 49 R5) *)
      let rest_env =
        match Narrow.detect_hard_require e with
        | Some feature_name -> Env.load_feature feature_name env
        | None -> env
      in
      (* Or-expression never-exit narrows env for subsequent forms
         (Spec 52 R5) *)
      let rest_env =
        match Narrow.analyze_or_exit e rest_env with
        | [] -> rest_env
        | preds ->
            List.fold_left (fun e p -> apply_predicate_then p e) rest_env preds
      in
      let rest_result = infer_progn rest_env rest span in
      {
        ty = rest_result.ty;
        constraints = C.combine e_result.constraints rest_result.constraints;
        undefineds = e_result.undefineds @ rest_result.undefineds;
        clause_diagnostics =
          e_result.clause_diagnostics @ rest_result.clause_diagnostics;
      }

(** Infer the type of a setq expression.

    (setq var1 val1 var2 val2 ...) Returns the value of the last assignment. *)
and infer_setq env pairs span =
  let open Syntax.Sexp in
  let rec process_pairs pairs constraints undefineds last_ty =
    match pairs with
    | [] -> { ty = last_ty; constraints; undefineds; clause_diagnostics = [] }
    | [ _ ] ->
        (* Odd number of args - malformed, return nil *)
        { ty = Prim.nil; constraints; undefineds; clause_diagnostics = [] }
    | Symbol (name, _) :: value :: rest ->
        let result = infer env value in
        (* Check if variable exists in variable namespace only.
           Use lookup_var to avoid matching functions with the same name. *)
        let constraint_set =
          match Env.lookup_var name env with
          | Some scheme ->
              (* Generate constraint: value type = variable type *)
              let var_ty = Env.instantiate scheme env in
              C.add (C.equal var_ty result.ty span) result.constraints
          | None ->
              (* New variable - no constraint needed *)
              result.constraints
        in
        process_pairs rest
          (C.combine constraints constraint_set)
          (undefineds @ result.undefineds)
          result.ty
    | _ :: _ :: rest ->
        (* Non-symbol in var position *)
        process_pairs rest constraints undefineds last_ty
  in
  process_pairs pairs C.empty [] Prim.nil

(** Infer the type of a cond expression.

    Each clause is (test body...). Result unifies all clause bodies.

    Cumulative narrowing (Spec 52 R3): each clause's body sees the narrowing
    from its own test, and subsequent clauses see the accumulated subtractions
    from earlier predicate tests. *)
and infer_cond env clauses span =
  let open Syntax.Sexp in
  let result_ty = fresh_tvar (Env.current_level env) in

  let rec process_clauses acc_env clauses constraints undefineds =
    match clauses with
    | [] -> (constraints, undefineds)
    | List (test :: body, _) :: rest ->
        let test_result = infer acc_env test in
        (* Analyze test condition for predicate narrowing (Spec 52) and
           feature guard env extension (Spec 49).
           Body sees narrowed type; subsequent clauses see subtracted type. *)
        let analysis = Narrow.analyze_condition test acc_env in
        let body_env, next_env = narrow_env_from_analysis analysis acc_env in
        let body_result = infer_progn body_env body span in
        let body_constraint = C.equal result_ty body_result.ty span in
        let all =
          C.combine test_result.constraints
            (C.combine body_result.constraints
               (C.add body_constraint constraints))
        in
        (* Suppress undefineds for names proven by guards *)
        let suppressed = guard_suppressed_names analysis in
        let test_undefineds =
          List.filter
            (fun (u : undefined_var) -> not (List.mem u.name suppressed))
            test_result.undefineds
        in
        let all_undefineds =
          undefineds @ test_undefineds @ body_result.undefineds
        in
        process_clauses next_env rest all all_undefineds
    | _ :: rest -> process_clauses acc_env rest constraints undefineds
  in

  let all_constraints, all_undefineds =
    process_clauses env clauses C.empty []
  in
  {
    ty = result_ty;
    constraints = all_constraints;
    undefineds = all_undefineds;
    clause_diagnostics = [];
  }

(** Infer the type of an and expression with predicate narrowing (Spec 52 R4).

    Processes args sequentially: after each arg, if it is a predicate call like
    [(stringp x)], subsequent args see x narrowed to [string]. Returns the last
    value if all are truthy, or the first falsy value. *)
and infer_and env args _span =
  let rec process acc_env acc_results = function
    | [] -> List.rev acc_results
    | arg :: rest ->
        let result = infer acc_env arg in
        (* Narrow the env for subsequent args if this arg is a predicate call
           or feature guard (Spec 49). *)
        let analysis = Narrow.analyze_condition arg acc_env in
        let next_env = narrow_then_from_analysis analysis acc_env in
        (* Suppress undefineds for names proven by guards *)
        let suppressed = guard_suppressed_names analysis in
        let filtered_result =
          {
            result with
            undefineds =
              List.filter
                (fun (u : undefined_var) -> not (List.mem u.name suppressed))
                result.undefineds;
          }
        in
        process next_env (filtered_result :: acc_results) rest
  in
  let results = process env [] args in
  let constraints = combine_results results in
  let undefineds = combine_undefineds results in
  match List.rev results with
  | last :: _ ->
      {
        ty = last.ty;
        constraints;
        undefineds;
        clause_diagnostics = combine_clause_diagnostics results;
      }
  | [] -> pure Prim.t

(** Infer the type of an or expression.

    Returns the first truthy value, or the last value. Type is a union of all
    argument types (simplified to last). *)
and infer_or env args _span =
  let results = List.map (infer env) args in
  let constraints = combine_results results in
  let undefineds = combine_undefineds results in
  match List.rev results with
  | last :: _ ->
      {
        ty = last.ty;
        constraints;
        undefineds;
        clause_diagnostics = combine_clause_diagnostics results;
      }
  | [] -> pure Prim.nil

(** Infer the type of a not expression.

    (not x) always returns a boolean. *)
and infer_not env arg _span =
  let arg_result = infer env arg in
  {
    ty = Prim.bool;
    constraints = arg_result.constraints;
    undefineds = arg_result.undefineds;
    clause_diagnostics = arg_result.clause_diagnostics;
  }

(** {1 Pcase Pattern Matching}

    Delegated to {!Pcase_infer}. *)

and infer_pcase_let env bindings body span =
  Pcase_infer.infer_pcase_let ~infer ~infer_progn env bindings body span

and infer_pcase env expr clauses span =
  Pcase_infer.infer_pcase ~infer ~infer_progn env expr clauses span

(** Infer the type of a tart annotation expression.

    (tart TYPE FORM) checks that FORM's inferred type is compatible with TYPE.
    The result type is TYPE, allowing the annotation to refine the type. *)
and infer_tart_annotation env type_sexp form _span =
  let open Syntax.Sexp in
  (* Infer the type of the form *)
  let form_result = infer env form in

  (* Parse the type annotation *)
  match Defun_infer.parse_tart_type type_sexp with
  | Some sig_type ->
      (* Extract type variable names from explicit forall, if present *)
      let tvar_names, inner_type =
        match sig_type with
        | Sig_ast.STForall (binders, inner, _) ->
            (List.map (fun b -> b.Sig_ast.name) binders, inner)
        | _ -> ([], sig_type)
      in

      (* Create fresh type variables for polymorphic type parameters *)
      let tvar_subst =
        List.map
          (fun name -> (name, fresh_tvar (Env.current_level env)))
          tvar_names
      in

      (* Convert sig_type to typ and substitute fresh TVars *)
      let prelude = Sig.Prelude.prelude_alias_context () in
      let base_ty =
        Sig_loader.sig_type_to_typ_with_aliases prelude tvar_names inner_type
      in
      let declared_ty =
        Clause_dispatch.substitute_tvar_names tvar_subst base_ty
      in

      (* Create constraint: form's type = declared type *)
      let context = C.TartAnnotation { declared_type = declared_ty } in
      let annotation_constraint =
        C.equal ~context declared_ty form_result.ty (span_of form)
      in

      {
        ty = declared_ty;
        constraints = C.add annotation_constraint form_result.constraints;
        undefineds = form_result.undefineds;
        clause_diagnostics = form_result.clause_diagnostics;
      }
  | None ->
      (* Parse failed - return form's type unchanged with a fresh tvar *)
      { form_result with ty = fresh_tvar (Env.current_level env) }

(** Infer the type of an explicit type instantiation.

    (tart [T1 T2 ...] fn args...) explicitly instantiates the polymorphic
    function fn with the given type arguments, then applies it to args.

    The type arguments are parsed using sig_parser and applied to the function's
    type parameters in order. The underscore [_] can be used as a placeholder to
    let inference determine that type argument. *)
and infer_explicit_instantiation (env : Env.t)
    (type_arg_sexps : Syntax.Sexp.t list) (fn : Syntax.Sexp.t)
    (args : Syntax.Sexp.t list) (span : Loc.span) : result =
  let open Syntax.Sexp in
  (* Parse the type arguments.
     Note: We do NOT apply forall inference here because type arguments in tart
     instantiation are meant to be concrete types, not polymorphic. A simple
     name like "list" should become TCon "List", not a forall-wrapped type var. *)
  let parsed_type_args =
    List.map
      (fun sexp ->
        match sexp with
        | Symbol ("_", _) -> None (* Placeholder - infer this type *)
        | _ -> (
            match Sig_parser.parse_sig_type sexp with
            | Ok sig_type ->
                (* Convert directly without forall inference, using prelude aliases *)
                let prelude = Sig.Prelude.prelude_alias_context () in
                Some
                  (Sig_loader.sig_type_to_typ_with_aliases prelude [] sig_type)
            | Error _ -> None))
      type_arg_sexps
  in

  (* Look up the function's type *)
  let fn_name = get_fn_name fn in
  let fn_scheme =
    match fn_name with Some name -> Env.lookup name env | None -> None
  in

  (* Get the function's quantified type variables and body type *)
  let tvar_names, fn_body_type =
    match fn_scheme with
    | Some (Env.Poly (vars, ty)) -> (vars, ty)
    | Some (Env.Mono ty) -> ([], ty)
    | None -> ([], fresh_tvar (Env.current_level env))
  in

  (* Check type argument arity: number of type args must match type params *)
  let num_type_args = List.length parsed_type_args in
  let num_type_params = List.length tvar_names in
  let arity_error_constraint =
    if num_type_args <> num_type_params && num_type_params > 0 then
      (* Arity mismatch - create a constraint that will fail *)
      let context =
        C.TypeArgArity
          {
            fn_name = Option.value fn_name ~default:"<unknown>";
            expected = num_type_params;
            actual = num_type_args;
          }
      in
      (* Create a constraint between two different concrete types to force an error.
         We use Int and String as they will never unify. *)
      Some (C.equal ~context Prim.int Prim.string span)
    else None
  in

  (* Create substitution mapping type variable names to explicit/inferred types *)
  let tvar_subst =
    List.mapi
      (fun i name ->
        let explicit_ty =
          if i < List.length parsed_type_args then List.nth parsed_type_args i
          else None
        in
        match explicit_ty with
        | Some ty -> (name, ty)
        | None -> (name, fresh_tvar (Env.current_level env)))
      tvar_names
  in

  (* Apply substitution to get instantiated function type *)
  let instantiated_fn_type =
    Clause_dispatch.substitute_tvar_names tvar_subst fn_body_type
  in

  (* Now type-check the application with the instantiated type *)
  let arg_results = List.map (infer env) args in

  (* Fresh type variable for the result *)
  let result_ty = fresh_tvar (Env.current_level env) in

  (* Build expected function type with per-argument constraints *)
  let type_args_for_context = List.map (fun (_, ty) -> ty) tvar_subst in
  let arg_constraints_with_context =
    List.mapi
      (fun i arg_result ->
        let expected_param_ty = fresh_tvar (Env.current_level env) in
        let arg_expr = List.nth args i in
        let context =
          C.ExplicitInstantiation
            { type_args = type_args_for_context; arg_index = i }
        in
        ( expected_param_ty,
          C.equal ~context expected_param_ty arg_result.ty (span_of arg_expr) ))
      arg_results
  in

  (* Build the expected function type *)
  let param_types =
    List.map (fun (ty, _) -> PPositional ty) arg_constraints_with_context
  in
  let expected_fn_type = TArrow (param_types, result_ty) in

  (* Constraint: instantiated function type = expected function type *)
  let fn_constraint = C.equal instantiated_fn_type expected_fn_type span in

  (* Collect all constraints *)
  let arg_type_constraints =
    List.fold_left
      (fun acc (_, c) -> C.add c acc)
      C.empty arg_constraints_with_context
  in
  let arg_constraints = combine_results arg_results in
  let base_constraints =
    C.combine arg_constraints (C.add fn_constraint arg_type_constraints)
  in
  (* Add arity error constraint if present *)
  let all_constraints =
    match arity_error_constraint with
    | Some c -> C.add c base_constraints
    | None -> base_constraints
  in
  let all_undefineds = combine_undefineds arg_results in

  {
    ty = result_ty;
    constraints = all_constraints;
    undefineds = all_undefineds;
    clause_diagnostics = [];
  }

(** Extract function name from a function expression for error context *)
and get_fn_name (fn : Syntax.Sexp.t) : string option =
  let open Syntax.Sexp in
  match fn with Symbol (name, _) -> Some name | _ -> None

(** Extract the source name from an expression for error context.

    For variables, returns the variable name. For function calls, returns the
    function name. Used to provide "may return nil" messages for Option types.
*)
and get_expr_source (expr : Syntax.Sexp.t) : string option =
  let open Syntax.Sexp in
  match expr with
  | Symbol (name, _) -> Some name
  | List (Symbol (name, _) :: _, _) -> Some name
  | _ -> None

(** Detect when a function expression is a regular-quoted symbol ['name] and
    produce a hint diagnostic recommending [#'name] instead.

    Sharp-quote is semantically equivalent at runtime but tells the
    byte-compiler about the function reference. Returns [Some diagnostic] when
    the expression is a regular quote, [None] otherwise. *)
and quote_style_diagnostic (fn_expr : Syntax.Sexp.t) :
    resolved_clause_diagnostic option =
  let open Syntax.Sexp in
  match fn_expr with
  | List ([ Symbol ("quote", _); Symbol (name, span) ], _) ->
      Some
        {
          rcd_severity = Env.DiagNote;
          rcd_message =
            Printf.sprintf "use #'%s instead of '%s for function references"
              name name;
          rcd_span = span;
        }
  | _ -> None

(** Infer the function expression for funcall/apply.

    When [fn_expr] is a regular-quoted symbol ['name], look up [name] in the
    function namespace (same as [#'name]). This makes ['name] and [#'name]
    type-equivalent in funcall/apply position per R2/R5. *)
and infer_fn_expr env fn_expr =
  let open Syntax.Sexp in
  match fn_expr with
  | List ([ Symbol ("quote", _); Symbol (name, span) ], _) -> (
      match Env.lookup_fn name env with
      | Some scheme ->
          let ty = Env.instantiate scheme env in
          pure ty
      | None -> (
          match Env.lookup name env with
          | Some scheme ->
              let ty = Env.instantiate scheme env in
              pure ty
          | None ->
              with_undefined (fresh_tvar (Env.current_level env)) name span))
  | _ -> infer env fn_expr

(** Infer the type of an apply expression.

    (apply f arg1 arg2 ... list) applies function f to the fixed args followed
    by the elements of list.

    For functions with &rest parameters, we constrain:
    - All fixed args to have type T
    - The final list arg to have type (List T)

    For fixed-arity functions, we require the total argument count to match. *)
and infer_apply env fn_expr args span =
  let fn_result = infer_fn_expr env fn_expr in

  (* Split args into fixed args and the final list arg *)
  let fixed_args, list_arg =
    let rec split_last acc = function
      | [] -> ([], None)
      | [ x ] -> (List.rev acc, Some x)
      | x :: xs -> split_last (x :: acc) xs
    in
    split_last [] args
  in

  let fixed_results = List.map (infer env) fixed_args in
  let list_result =
    match list_arg with Some arg -> infer env arg | None -> pure Prim.nil
  in

  (* Try to extract a name for error context *)
  let fn_name =
    let open Syntax.Sexp in
    match fn_expr with
    | Symbol (name, _) -> Some name
    | List ([ Symbol ("function", _); Symbol (name, _) ], _) -> Some name
    | List ([ Symbol ("quote", _); Symbol (name, _) ], _) -> Some name
    | _ -> None
  in

  (* Fresh type variable for the result *)
  let result_ty = fresh_tvar (Env.current_level env) in

  (* Fresh type variable for the element type of the &rest parameter.
     This will be unified with all fixed args and the list element type. *)
  let rest_elem_ty = fresh_tvar (Env.current_level env) in

  (* Constrain each fixed arg to have the rest element type *)
  let fixed_constraints =
    List.mapi
      (fun i arg_result ->
        let arg_expr = List.nth fixed_args i in
        let arg_expr_source = get_expr_source arg_expr in
        let context =
          match fn_name with
          | Some name ->
              C.FunctionArg
                {
                  fn_name = name;
                  fn_type = fn_result.ty;
                  arg_index = i;
                  arg_expr_source;
                }
          | None ->
              C.FunctionArg
                {
                  fn_name = "apply";
                  fn_type = fn_result.ty;
                  arg_index = i;
                  arg_expr_source;
                }
        in
        C.equal ~context rest_elem_ty arg_result.ty
          (Syntax.Sexp.span_of arg_expr))
      fixed_results
  in

  (* Constrain the list arg to (List rest_elem_ty).
     Quoted lists infer as tuples, so tuple-to-list subtyping (R9) handles
     the unification: TTuple [t1;...;tn] <: (List rest_elem_ty). *)
  let list_constraint =
    match list_arg with
    | Some arg ->
        let expected_list_ty = list_of rest_elem_ty in
        let arg_expr_source = get_expr_source arg in
        let context =
          match fn_name with
          | Some name ->
              C.FunctionArg
                {
                  fn_name = name;
                  fn_type = fn_result.ty;
                  arg_index = List.length fixed_results;
                  arg_expr_source;
                }
          | None ->
              C.FunctionArg
                {
                  fn_name = "apply";
                  fn_type = fn_result.ty;
                  arg_index = List.length fixed_results;
                  arg_expr_source;
                }
        in
        [
          C.equal ~context expected_list_ty list_result.ty
            (Syntax.Sexp.span_of arg);
        ]
    | None -> []
  in

  (* Build expected function type: (&rest rest_elem_ty) -> result_ty *)
  let expected_fn_type = TArrow ([ PRest rest_elem_ty ], result_ty) in

  (* Constraint: actual function type = expected function type *)
  let fn_constraint = C.equal fn_result.ty expected_fn_type span in

  (* Combine all constraints.
     ORDER MATTERS: fn_constraint must be first so the function type establishes
     what the rest element type should be before we check args against it.
     The list constraint comes after fn_constraint but includes the list arg's
     own constraints so tuple-to-list subtyping can check element types. *)
  let fixed_constraint_set =
    List.fold_left (fun acc c -> C.add c acc) C.empty fixed_constraints
  in
  let all_constraints =
    [ fn_constraint ] @ fn_result.constraints @ fixed_constraint_set
    @ combine_results fixed_results
    @ list_constraint @ list_result.constraints
  in
  let all_undefineds =
    fn_result.undefineds @ list_result.undefineds
    @ combine_undefineds fixed_results
  in

  let quote_diag =
    match quote_style_diagnostic fn_expr with Some d -> [ d ] | None -> []
  in
  {
    ty = result_ty;
    constraints = all_constraints;
    undefineds = all_undefineds;
    clause_diagnostics = quote_diag;
  }

(** Try to dispatch a funcall through a union of function types (Spec 34 R11).

    When the function expression has a union type where all members are [TArrow]
    types, check arguments against every variant speculatively:
    - All variants must accept the arguments (intersection of param types)
    - Return type is the union of all variants' return types

    Returns [UnionMatched return_type] if all variants accept the args,
    [UnionFailed arrows] if the function IS a union of arrows but args don't
    satisfy all variants (carries the list of arrow variants for error
    reporting), or [UnionNotApplicable] if the function type is not a union of
    arrows. *)
and try_union_fn_dispatch (arg_types : typ list) (fn_ty : typ) (loc : Loc.span)
    : union_fn_result =
  match repr fn_ty with
  | TUnion members -> (
      let arrows =
        List.filter_map
          (fun m ->
            match repr m with TArrow (ps, r) -> Some (ps, r) | _ -> None)
          members
      in
      if List.length arrows <> List.length members || List.length arrows < 2
      then
        (* Not all members are arrows, or only one member — skip *)
        UnionNotApplicable
      else
        (* Try to unify args against each arrow variant speculatively *)
        let try_all_variants () =
          List.fold_left
            (fun acc (params, ret) ->
              match acc with
              | None -> None (* Already failed *)
              | Some returns ->
                  let param_types =
                    List.map (fun t -> PPositional t) arg_types
                  in
                  if
                    Result.is_ok (Unify.try_unify_params param_types params loc)
                  then Some (ret :: returns)
                  else None)
            (Some []) arrows
        in
        match try_all_variants () with
        | Some returns ->
            let returns = List.rev returns in
            (* Deduplicate return types: if all are the same, use that type
               directly instead of a redundant union like (Or int int). *)
            let deduped =
              List.fold_left
                (fun acc r ->
                  if List.exists (equal r) acc then acc else r :: acc)
                [] returns
              |> List.rev
            in
            let ret_ty = match deduped with [ t ] -> t | ts -> TUnion ts in
            UnionMatched ret_ty
        | None -> UnionFailed arrows)
  | _ -> UnionNotApplicable

and infer_funcall env fn_expr args span =
  let fn_result = infer_fn_expr env fn_expr in
  let arg_results = List.map (infer env) args in

  (* Try to extract a name for error context *)
  let fn_name =
    let open Syntax.Sexp in
    match fn_expr with
    | Symbol (name, _) -> Some name
    | List ([ Symbol ("function", _); Symbol (name, _) ], _) -> Some name
    | List ([ Symbol ("quote", _); Symbol (name, _) ], _) -> Some name
    | _ -> None
  in

  let arg_types = List.map (fun r -> r.ty) arg_results in

  (* Try union function dispatch first (Spec 34 R11).
     When the function type is a union of arrows, check args against all
     variants and return the union of return types. *)
  let union_dispatch_result =
    try_union_fn_dispatch arg_types fn_result.ty span
  in

  match union_dispatch_result with
  | UnionMatched union_ret_ty ->
      (* Union dispatch succeeded — use union of return types *)
      let arg_constraints = combine_results arg_results in
      let all_constraints = C.combine fn_result.constraints arg_constraints in
      let all_undefineds =
        fn_result.undefineds @ combine_undefineds arg_results
      in
      let quote_diag =
        match quote_style_diagnostic fn_expr with Some d -> [ d ] | None -> []
      in
      {
        ty = union_ret_ty;
        constraints = all_constraints;
        undefineds = all_undefineds;
        clause_diagnostics = quote_diag;
      }
  | UnionFailed arrows ->
      (* Function IS a union of arrows, but args don't satisfy all variants.
         Emit per-variant constraints: each arrow's params must match the args.
         The variant(s) that don't match will produce type errors during
         constraint solving. *)
      let result_ty = fresh_tvar (Env.current_level env) in
      let arg_constraints = combine_results arg_results in
      let variant_constraints =
        List.fold_left
          (fun acc (params, ret) ->
            let param_types = List.map (fun t -> PPositional t) arg_types in
            let expected_arrow = TArrow (param_types, ret) in
            let variant_arrow = TArrow (params, ret) in
            C.add (C.equal variant_arrow expected_arrow span) acc)
          C.empty arrows
      in
      (* Also constrain result to the first return type for error display *)
      let ret_constraint =
        match arrows with
        | (_, ret) :: _ -> C.add (C.equal result_ty ret span) C.empty
        | [] -> C.empty
      in
      let all_constraints =
        C.combine fn_result.constraints
          (C.combine arg_constraints
             (C.combine variant_constraints ret_constraint))
      in
      let all_undefineds =
        fn_result.undefineds @ combine_undefineds arg_results
      in
      let quote_diag =
        match quote_style_diagnostic fn_expr with Some d -> [ d ] | None -> []
      in
      {
        ty = result_ty;
        constraints = all_constraints;
        undefineds = all_undefineds;
        clause_diagnostics = quote_diag;
      }
  | UnionNotApplicable ->
      (* Standard constraint-based path *)

      (* Fresh type variable for the result *)
      let result_ty = fresh_tvar (Env.current_level env) in

      (* Build expected function type - one constraint per argument for better
         error messages *)
      let arg_constraints_with_context =
        List.mapi
          (fun i arg_result ->
            let expected_param_ty = fresh_tvar (Env.current_level env) in
            let arg_expr = List.nth args i in
            let arg_expr_source = get_expr_source arg_expr in
            let context =
              match fn_name with
              | Some name ->
                  C.FunctionArg
                    {
                      fn_name = name;
                      fn_type = fn_result.ty;
                      arg_index = i;
                      arg_expr_source;
                    }
              | None ->
                  (* For anonymous functions, use "funcall" as context *)
                  C.FunctionArg
                    {
                      fn_name = "funcall";
                      fn_type = fn_result.ty;
                      arg_index = i;
                      arg_expr_source;
                    }
            in
            ( expected_param_ty,
              C.equal ~context expected_param_ty arg_result.ty
                (Syntax.Sexp.span_of arg_expr) ))
          arg_results
      in

      (* Build the expected function type using the fresh param types *)
      let param_types =
        List.map (fun (ty, _) -> PPositional ty) arg_constraints_with_context
      in
      let expected_fn_type = TArrow (param_types, result_ty) in

      (* Constraint: actual function type = expected function type *)
      let fn_constraint = C.equal fn_result.ty expected_fn_type span in

      (* Collect all argument constraints *)
      let arg_type_constraints =
        List.fold_left
          (fun acc (_, c) -> C.add c acc)
          C.empty arg_constraints_with_context
      in

      (* Combine all constraints.
         Order matters for error context: fn_constraint must come BEFORE
         arg_type_constraints so that the function signature determines the
         expected type before we compare with actual argument types. *)
      let arg_constraints = combine_results arg_results in
      let all_constraints =
        C.combine fn_result.constraints
          (C.combine arg_constraints (C.add fn_constraint arg_type_constraints))
      in
      let all_undefineds =
        fn_result.undefineds @ combine_undefineds arg_results
      in

      let quote_diag =
        match quote_style_diagnostic fn_expr with Some d -> [ d ] | None -> []
      in
      {
        ty = result_ty;
        constraints = all_constraints;
        undefineds = all_undefineds;
        clause_diagnostics = quote_diag;
      }

(** Infer the type of a function application.

    Generates constraint: fn_type = (arg_types...) -> result_type.

    When the function has multi-clause signatures (from .tart files), attempts
    clause-by-clause dispatch first. For known row-typed accessors with literal
    keys, generates virtual dispatch from row fields. Falls back to the merged
    union-type constraint path when no clause matches or no clauses exist. *)
and infer_application env fn args span =
  let fn_result = infer env fn in
  let arg_results = List.map (infer env) args in
  let fn_name = get_fn_name fn in
  let arg_types = List.map (fun r -> r.ty) arg_results in
  let arg_literals = List.map Clause_dispatch.extract_arg_literal args in

  (* Try clause-by-clause dispatch for multi-clause functions *)
  let clause_result =
    match fn_name with
    | Some name -> (
        match lookup_stored_clauses_and_vars name env with
        | Some (clauses, vars) ->
            Clause_dispatch.try_dispatch env ~tvar_names:vars ~clauses
              ~arg_types ~arg_literals ~loc:span
        | None -> None)
    | None -> None
  in

  (* Try virtual clause dispatch for row-typed accessors. When a function's
     signature has a row-typed container parameter, generate virtual clauses
     implementing the Spec 11 R4 decision table (Cases 1-5 and R8). Virtual
     clauses are tried only when regular clause dispatch didn't match. *)
  let virtual_clause_result =
    match clause_result with
    | Some _ -> None (* Regular clause dispatch already succeeded *)
    | None -> (
        match fn_name with
        | Some name -> (
            match lookup_clauses_and_vars_with_synthesis name env with
            | Some (clauses, vars) -> (
                let virtual_clauses =
                  generate_virtual_clauses env clauses arg_types arg_literals
                in
                match virtual_clauses with
                | [] -> None
                | _ ->
                    Clause_dispatch.try_dispatch env ~tvar_names:vars
                      ~clauses:virtual_clauses ~arg_types ~arg_literals
                      ~loc:span)
            | None -> None)
        | None -> None)
  in

  (* R8: Container type unknown — infer row from literal key usage.

     When neither clause nor virtual clause dispatch matched and the function
     has a row-typed container parameter, check for a literal key and emit a
     constraint on the container argument to establish the row field. Unlike
     virtual clause dispatch (which unifies in-place), R8 emits a deferred
     constraint so that multiple R8 accesses on the same variable accumulate
     row fields independently. *)
  let r8_result =
    match (clause_result, virtual_clause_result) with
    | Some _, _ | _, Some _ -> None
    | None, None -> (
        match fn_name with
        | Some name -> (
            let config =
              match lookup_clauses_and_vars_with_synthesis name env with
              | Some (clauses, _) -> List.find_map analyze_clause clauses
              | None -> None
            in
            match config with
            | None -> None
            | Some cc -> (
                let key_literal =
                  if cc.cc_key_index < List.length arg_literals then
                    List.nth arg_literals cc.cc_key_index
                  else None
                in
                match key_literal with
                | None -> None
                | Some key_name -> (
                    let container_ty =
                      if cc.cc_container_index < List.length arg_types then
                        Some (List.nth arg_types cc.cc_container_index)
                      else None
                    in
                    let container_expr =
                      if cc.cc_container_index < List.length args then
                        Some (List.nth args cc.cc_container_index)
                      else None
                    in
                    match (container_ty, container_expr) with
                    | Some cty, Some cexpr ->
                        let field_ty = fresh_tvar (Env.current_level env) in
                        let row_var = fresh_tvar (Env.current_level env) in
                        let expected_row =
                          open_row [ (key_name, field_ty) ] row_var
                        in
                        let expected_ty =
                          build_expected_container cc.cc_kind expected_row
                        in
                        let c =
                          C.equal expected_ty cty (Syntax.Sexp.span_of cexpr)
                        in
                        Some (field_ty, Some c)
                    | Some _, None ->
                        let field_ty = fresh_tvar (Env.current_level env) in
                        Some (field_ty, None)
                    | _ -> None)))
        | None -> None)
  in

  (* Merge clause and virtual clause results: prefer explicit clause dispatch,
     then virtual clause dispatch *)
  let effective_clause_result =
    match clause_result with
    | Some _ -> clause_result
    | None -> virtual_clause_result
  in

  let result =
    match (effective_clause_result, r8_result) with
    | Some (clause_ret_ty, clause_diag_opt), _ ->
        (* Clause or virtual clause matched — use the return type directly.
         Still need to emit fn_result constraints and arg constraints for
         the sub-expressions, but the return type is determined by the clause
         rather than by unifying the full merged function type. *)
        let arg_constraints = combine_results arg_results in
        let all_constraints = C.combine fn_result.constraints arg_constraints in
        let all_undefineds =
          fn_result.undefineds @ combine_undefineds arg_results
        in
        let clause_diags =
          match clause_diag_opt with Some d -> [ d ] | None -> []
        in
        {
          ty = clause_ret_ty;
          constraints = all_constraints;
          undefineds = all_undefineds;
          clause_diagnostics =
            clause_diags @ fn_result.clause_diagnostics
            @ combine_clause_diagnostics arg_results;
        }
    | None, Some (r8_ret_ty, container_constraint) ->
        (* R8 dispatch matched — emit the container constraint. *)
        let arg_constraints = combine_results arg_results in
        let base_constraints =
          match container_constraint with
          | Some c -> C.add c arg_constraints
          | None -> arg_constraints
        in
        let all_constraints =
          C.combine fn_result.constraints base_constraints
        in
        let all_undefineds =
          fn_result.undefineds @ combine_undefineds arg_results
        in
        {
          ty = r8_ret_ty;
          constraints = all_constraints;
          undefineds = all_undefineds;
          clause_diagnostics =
            fn_result.clause_diagnostics
            @ combine_clause_diagnostics arg_results;
        }
    | None, None ->
        (* No clause or row dispatch — fall back to standard constraint path *)

        (* Fresh type variable for the result *)
        let result_ty = fresh_tvar (Env.current_level env) in

        (* Build expected function type - one constraint per argument for better
         error messages *)
        let arg_constraints_with_context =
          List.mapi
            (fun i arg_result ->
              let expected_param_ty = fresh_tvar (Env.current_level env) in
              let arg_expr = List.nth args i in
              let arg_expr_source = get_expr_source arg_expr in
              let context =
                match fn_name with
                | Some name ->
                    C.FunctionArg
                      {
                        fn_name = name;
                        fn_type = fn_result.ty;
                        arg_index = i;
                        arg_expr_source;
                      }
                | None -> C.NoContext
              in
              ( expected_param_ty,
                C.equal ~context expected_param_ty arg_result.ty
                  (Syntax.Sexp.span_of arg_expr) ))
            arg_results
        in

        (* Build the expected function type using the fresh param types *)
        let param_types =
          List.map (fun (ty, _) -> PPositional ty) arg_constraints_with_context
        in
        let expected_fn_type = TArrow (param_types, result_ty) in

        (* Constraint: actual function type = expected function type *)
        let fn_constraint = C.equal fn_result.ty expected_fn_type span in

        (* Collect all argument constraints *)
        let arg_type_constraints =
          List.fold_left
            (fun acc (_, c) -> C.add c acc)
            C.empty arg_constraints_with_context
        in

        (* Combine all constraints.
         Order matters for error context: fn_constraint must come BEFORE
         arg_type_constraints so that the function signature determines the
         expected type before we compare with actual argument types. *)
        let arg_constraints = combine_results arg_results in
        let all_constraints =
          C.combine fn_result.constraints
            (C.combine arg_constraints
               (C.add fn_constraint arg_type_constraints))
        in
        let all_undefineds =
          fn_result.undefineds @ combine_undefineds arg_results
        in

        {
          ty = result_ty;
          constraints = all_constraints;
          undefineds = all_undefineds;
          clause_diagnostics =
            fn_result.clause_diagnostics
            @ combine_clause_diagnostics arg_results;
        }
  in

  (* Post-processing: eq/eql disjointness checking (Spec 11 R14).
     After standard inference (including clause dispatch), check if the
     two argument types are provably disjoint. If so, inject a failing
     constraint with EqDisjointness context. This is complementary to the
     identity-safety clause diagnostic (Spec 48 R7). *)
  match (fn_name, arg_results) with
  | Some (("eq" | "eql") as name), [ arg1_result; arg2_result ] ->
      let arg1_ty = repr arg1_result.ty in
      let arg2_ty = repr arg2_result.ty in
      if Unify.types_disjoint arg1_ty arg2_ty then
        let context =
          C.EqDisjointness
            { fn_name = name; arg1_type = arg1_ty; arg2_type = arg2_ty }
        in
        let disjoint_constraint = C.equal ~context arg1_ty arg2_ty span in
        {
          result with
          constraints = C.add disjoint_constraint result.constraints;
        }
      else result
  | _ -> result

(** {1 Defun Inference}

    Delegated to {!Defun_infer}. *)

and infer_defun_as_expr env _name params body span =
  Defun_infer.infer_defun_as_expr ~infer_lambda env _name params body span

and infer_defun (env : Env.t) (sexp : Syntax.Sexp.t) : defun_result option =
  Defun_infer.infer_defun ~infer_progn env sexp

(** Infer the type of a vector literal.

    All elements must have the same type. *)
and infer_vector env elems span =
  match elems with
  | [] ->
      (* Empty vector: (Vector 'a) with fresh type variable *)
      let elem_ty = fresh_tvar (Env.current_level env) in
      pure (vector_of elem_ty)
  | first :: rest ->
      let first_result = infer env first in
      let rest_results = List.map (infer env) rest in

      (* Widen the element type: if the first element is a literal,
         use its base type so that vector types are stable. *)
      let elem_ty =
        match first_result.ty with TLiteral (_, base) -> base | ty -> ty
      in

      (* All remaining elements must unify with the element type *)
      let elem_constraints =
        List.map (fun r -> C.equal elem_ty r.ty span) rest_results
      in

      let all_constraints =
        C.combine first_result.constraints
          (C.combine
             (combine_results rest_results)
             (List.fold_left
                (fun acc c -> C.add c acc)
                C.empty elem_constraints))
      in
      let all_undefineds =
        first_result.undefineds @ combine_undefineds rest_results
      in

      {
        ty = vector_of elem_ty;
        constraints = all_constraints;
        undefineds = all_undefineds;
        clause_diagnostics =
          first_result.clause_diagnostics
          @ combine_clause_diagnostics rest_results;
      }
