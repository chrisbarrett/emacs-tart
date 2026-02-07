(** Top-level type checking API.

    This module provides the entry point for type checking Elisp programs. It
    handles sequences of top-level forms, accumulating type bindings from defun
    and other definition forms.

    By default, all type checking functions use an environment pre-populated
    with types for built-in functions (car, +, concat, etc.). This can be
    overridden by passing a custom ~env parameter. *)

open Core.Types
module Env = Core.Type_env
module C = Constraint
module Sig_parser = Sig.Sig_parser
module Sig_loader = Sig.Sig_loader
module Sig_ast = Sig.Sig_ast
module Loc = Syntax.Location

(** Default environment with built-in function types *)
let default_env () = Builtin_types.initial_env ()

(** Result of type-checking a single top-level form *)
type form_result =
  | DefunForm of { name : string; fn_type : typ }
  | DefvarForm of { name : string; var_type : typ }
  | TartDeclareForm of { name : string; var_type : typ }
  | TartTypeForm of { name : string; params : string list }
  | ExprForm of { ty : typ }

type check_result = {
  env : Env.t;  (** Final type environment with all bindings *)
  forms : form_result list;  (** Results for each top-level form *)
  errors : Unify.error list;  (** Any type errors encountered *)
  undefineds : Infer.undefined_var list;  (** Undefined variable references *)
  clause_diagnostics : Infer.resolved_clause_diagnostic list;
      (** Clause diagnostics emitted during multi-clause dispatch *)
  aliases : Sig_loader.alias_context;
      (** File-local type aliases from tart-type forms *)
}
(** Result of type-checking a program *)

type check_state = {
  st_env : Env.t;
  st_aliases : Sig_loader.alias_context;
  st_errors : Unify.error list;
  st_undefineds : Infer.undefined_var list;
  st_clause_diagnostics : Infer.resolved_clause_diagnostic list;
  st_forms : form_result list;
}
(** Internal state for type checking a program. Tracks environment, aliases, and
    accumulated errors. *)

(** Parse a tart type annotation from an S-expression.

    Returns the parsed sig_type or None if parsing fails. *)
let parse_tart_type (type_sexp : Syntax.Sexp.t) : Sig_ast.sig_type option =
  match Sig_parser.parse_sig_type type_sexp with
  | Ok sig_type -> Some sig_type
  | Error _ -> None

(** Substitute type variable names (as TCon) with actual TVars in a type.

    Used to convert signature types to inferable types. *)
let rec substitute_tvar_names (subst : (string * typ) list) (ty : typ) : typ =
  match ty with
  | TCon name -> (
      match List.assoc_opt name subst with Some tv -> tv | None -> ty)
  | TVar { contents = Link t } -> substitute_tvar_names subst t
  | TVar _ -> ty
  | TArrow (params, ret) ->
      let params' =
        List.map
          (function
            | PPositional t -> PPositional (substitute_tvar_names subst t)
            | POptional t -> POptional (substitute_tvar_names subst t)
            | PRest t -> PRest (substitute_tvar_names subst t)
            | PKey (n, t) -> PKey (n, substitute_tvar_names subst t)
            | PLiteral _ as p -> p)
          params
      in
      TArrow (params', substitute_tvar_names subst ret)
  | TApp (con, args) ->
      TApp
        ( substitute_tvar_names subst con,
          List.map (substitute_tvar_names subst) args )
  | TForall (vars, body) ->
      (* Remove substituted vars from the substitution to avoid capture *)
      let subst' = List.filter (fun (n, _) -> not (List.mem n vars)) subst in
      TForall (vars, substitute_tvar_names subst' body)
  | TUnion types -> TUnion (List.map (substitute_tvar_names subst) types)
  | TTuple types -> TTuple (List.map (substitute_tvar_names subst) types)
  | TRow { row_fields; row_var } ->
      TRow
        {
          row_fields =
            List.map
              (fun (n, t) -> (n, substitute_tvar_names subst t))
              row_fields;
          row_var = Option.map (substitute_tvar_names subst) row_var;
        }

(** Convert a sig_type to a typ at the given level with alias expansion.

    Creates fresh TVars for type parameters (from explicit quantifiers only) and
    substitutes them in the type body. [aliases] provides file-local type
    aliases for expansion. *)
let sig_type_to_typ_with_tvars ?(aliases = Sig_loader.empty_aliases)
    (level : int) (sig_type : Sig_ast.sig_type) : typ =
  (* Extract type variable names from explicit forall, if present *)
  let tvar_names, inner_type =
    match sig_type with
    | Sig_ast.STForall (binders, inner, _) ->
        (List.map (fun b -> b.Sig_ast.name) binders, inner)
    | _ -> ([], sig_type)
  in

  (* Create fresh type variables for polymorphic type parameters *)
  let tvar_subst = List.map (fun name -> (name, fresh_tvar level)) tvar_names in

  (* Convert sig_type to typ with alias expansion and substitute fresh TVars *)
  let base_ty =
    Sig_loader.sig_type_to_typ_with_aliases aliases tvar_names inner_type
  in
  substitute_tvar_names tvar_subst base_ty

(** Extract a tart annotation from defvar/defconst initialization.

    Checks if the init form is [(tart TYPE VALUE)] and returns the type and
    value if so. Returns [None] if no annotation is present. *)
let extract_defvar_tart_annotation (init : Syntax.Sexp.t) :
    (Syntax.Sexp.t * Syntax.Sexp.t) option =
  let open Syntax.Sexp in
  match init with
  | List ([ Symbol ("tart", _); type_sexp; value ], _) -> Some (type_sexp, value)
  | _ -> None

(** Check a defvar/defconst form with optional tart annotation.

    If the init is [(tart TYPE VALUE)], binds NAME with TYPE in the environment
    and checks VALUE against TYPE. Otherwise, infers the type from the init.
    [aliases] provides file-local type aliases for expansion. *)
let check_defvar ~(aliases : Sig_loader.alias_context) (env : Env.t)
    (name : string) (init : Syntax.Sexp.t option) (_span : Loc.span) :
    Env.t
    * form_result
    * Unify.error list
    * Infer.undefined_var list
    * Infer.resolved_clause_diagnostic list =
  match init with
  | Some init_expr -> (
      match extract_defvar_tart_annotation init_expr with
      | Some (type_sexp, value) -> (
          (* Has tart annotation: (defvar NAME (tart TYPE VALUE)) *)
          match parse_tart_type type_sexp with
          | Some sig_type ->
              let var_type =
                sig_type_to_typ_with_tvars ~aliases (Env.current_level env)
                  sig_type
              in

              (* Check the value against the declared type *)
              let value_result = Infer.infer env value in
              let value_span = Syntax.Sexp.span_of value in
              let annotation_constraint =
                C.equal
                  ~context:(C.TartAnnotation { declared_type = var_type })
                  var_type value_result.Infer.ty value_span
              in
              let all_constraints =
                C.add annotation_constraint value_result.Infer.constraints
              in
              let errors = Unify.solve_all all_constraints in

              (* Bind the variable with the declared type *)
              let scheme =
                Generalize.generalize (Env.current_level env) var_type
              in
              let env' = Env.extend name scheme env in

              ( env',
                DefvarForm { name; var_type },
                errors,
                value_result.Infer.undefineds,
                value_result.Infer.clause_diagnostics )
          | None ->
              (* Parse failed - fall through to regular inference *)
              let result = Infer.infer env init_expr in
              let errors = Unify.solve_all result.Infer.constraints in
              let scheme =
                Generalize.generalize (Env.current_level env) result.Infer.ty
              in
              let env' = Env.extend name scheme env in
              ( env',
                DefvarForm { name; var_type = result.Infer.ty },
                errors,
                result.Infer.undefineds,
                result.Infer.clause_diagnostics ))
      | None ->
          (* No tart annotation: infer the type from init *)
          let result = Infer.infer env init_expr in
          let errors = Unify.solve_all result.Infer.constraints in
          let scheme =
            Generalize.generalize (Env.current_level env) result.Infer.ty
          in
          let env' = Env.extend name scheme env in
          ( env',
            DefvarForm { name; var_type = result.Infer.ty },
            errors,
            result.Infer.undefineds,
            result.Infer.clause_diagnostics ))
  | None -> (
      (* (defvar NAME) with no init - respect existing tart-declare type if present *)
      match Env.lookup name env with
      | Some scheme ->
          (* Variable already declared (e.g., via tart-declare) - keep that type *)
          let var_type = Env.instantiate scheme env in
          (env, DefvarForm { name; var_type }, [], [], [])
      | None ->
          (* New variable without init - use Any type *)
          let var_type = Prim.any in
          let env' = Env.extend_mono name var_type env in
          (env', DefvarForm { name; var_type }, [], [], []))

(** Check a tart-declare form: (tart-declare NAME TYPE)

    Binds NAME with TYPE in the environment. Does not require an initial value.
    [aliases] provides file-local type aliases for expansion. *)
let check_tart_declare ~(aliases : Sig_loader.alias_context) (env : Env.t)
    (name : string) (type_sexp : Syntax.Sexp.t) :
    Env.t
    * form_result
    * Unify.error list
    * Infer.undefined_var list
    * Infer.resolved_clause_diagnostic list =
  match parse_tart_type type_sexp with
  | Some sig_type ->
      let var_type =
        sig_type_to_typ_with_tvars ~aliases (Env.current_level env) sig_type
      in
      let scheme = Generalize.generalize (Env.current_level env) var_type in
      let env' = Env.extend name scheme env in
      (env', TartDeclareForm { name; var_type }, [], [], [])
  | None ->
      (* Parse failed - use Any type *)
      let var_type = Prim.any in
      let env' = Env.extend_mono name var_type env in
      (env', TartDeclareForm { name; var_type }, [], [], [])

(** Try to match a defvar or defconst form.

    Returns [Some (name, init_opt, span)] if matched, [None] otherwise. *)
let match_defvar (sexp : Syntax.Sexp.t) :
    (string * Syntax.Sexp.t option * Loc.span) option =
  let open Syntax.Sexp in
  match sexp with
  | List
      ( (Symbol ("defvar", _) | Symbol ("defconst", _))
        :: Symbol (name, _)
        :: init :: _,
        span ) ->
      Some (name, Some init, span)
  | List
      ( (Symbol ("defvar", _) | Symbol ("defconst", _)) :: Symbol (name, _) :: _,
        span ) ->
      Some (name, None, span)
  | _ -> None

(** Try to match a tart-declare form.

    Returns [Some (name, type_sexp)] if matched, [None] otherwise. *)
let match_tart_declare (sexp : Syntax.Sexp.t) : (string * Syntax.Sexp.t) option
    =
  let open Syntax.Sexp in
  match sexp with
  | List ([ Symbol ("tart-declare", _); Symbol (name, _); type_sexp ], _) ->
      Some (name, type_sexp)
  | _ -> None

(** Try to match a tart-type form (file-local type alias).

    Handles two forms:
    - Simple: (tart-type NAME DEFINITION)
    - Parameterized: (tart-type NAME [VARS] DEFINITION)

    Returns [Some (name, params_opt, def_sexp)] if matched, [None] otherwise. *)
let match_tart_type (sexp : Syntax.Sexp.t) :
    (string * Syntax.Sexp.t option * Syntax.Sexp.t) option =
  let open Syntax.Sexp in
  match sexp with
  | List ([ Symbol ("tart-type", _); Symbol (name, _); def_sexp ], _) ->
      (* Simple alias: (tart-type NAME DEFINITION) *)
      Some (name, None, def_sexp)
  | List
      ([ Symbol ("tart-type", _); Symbol (name, _); params_sexp; def_sexp ], _)
    ->
      (* Parameterized alias: (tart-type NAME [VARS] DEFINITION) *)
      Some (name, Some params_sexp, def_sexp)
  | _ -> None

(** Parse type parameter list from bracket syntax [a b c].

    Returns the list of parameter names, or None if parsing fails. *)
let parse_tart_type_params (sexp : Syntax.Sexp.t) : string list option =
  let open Syntax.Sexp in
  match sexp with
  | Vector (items, _) ->
      let names =
        List.filter_map
          (function Symbol (name, _) -> Some name | _ -> None)
          items
      in
      if List.length names = List.length items then Some names else None
  | _ -> None

(** Check a tart-type form (file-local type alias).

    Parses the type definition and adds it to the alias context. Returns the
    updated alias context. Does not modify the value environment.

    Handles:
    - Simple: (tart-type int-pair (tuple int int))
    - Parameterized: (tart-type predicate [a] ((a) -> bool)) *)
let check_tart_type ~(aliases : Sig_loader.alias_context) (name : string)
    (params_sexp : Syntax.Sexp.t option) (def_sexp : Syntax.Sexp.t) :
    Sig_loader.alias_context * form_result =
  (* Parse type parameters if present *)
  let params =
    match params_sexp with
    | Some ps -> Option.value ~default:[] (parse_tart_type_params ps)
    | None -> []
  in

  (* Parse the type definition body *)
  match parse_tart_type def_sexp with
  | Some sig_type ->
      (* Create the alias entry.
         tart-type forms in .el files don't support bounds, so all are None. *)
      let alias_params =
        List.map
          (fun name -> { Sig_loader.ap_name = name; ap_bound = None })
          params
      in
      let alias : Sig_loader.type_alias =
        { alias_params; alias_body = sig_type }
      in
      let aliases' = Sig_loader.add_alias name alias aliases in
      (aliases', TartTypeForm { name; params })
  | None ->
      (* Parse failed - return unchanged aliases *)
      (aliases, TartTypeForm { name; params })

(** Check a single top-level form and update the state.

    Returns the updated state with new environment, aliases, form result, and
    accumulated errors/undefineds. *)
let check_form_with_state (state : check_state) (sexp : Syntax.Sexp.t) :
    check_state =
  reset_tvar_counter ();
  let aliases = state.st_aliases in
  let env = state.st_env in

  (* Try to match tart-type first (file-local type alias) *)
  match match_tart_type sexp with
  | Some (name, params_sexp, def_sexp) ->
      let aliases', result =
        check_tart_type ~aliases name params_sexp def_sexp
      in
      { state with st_aliases = aliases'; st_forms = result :: state.st_forms }
  | None -> (
      (* Try to match tart-declare *)
      match match_tart_declare sexp with
      | Some (name, type_sexp) ->
          let env', result, errors, undefs, cdiags =
            check_tart_declare ~aliases env name type_sexp
          in
          {
            st_env = env';
            st_aliases = aliases;
            st_errors = List.rev_append errors state.st_errors;
            st_undefineds = List.rev_append undefs state.st_undefineds;
            st_clause_diagnostics =
              List.rev_append cdiags state.st_clause_diagnostics;
            st_forms = result :: state.st_forms;
          }
      | None -> (
          (* Try to match defvar/defconst *)
          match match_defvar sexp with
          | Some (name, init, span) ->
              let env', result, errors, undefs, cdiags =
                check_defvar ~aliases env name init span
              in
              {
                st_env = env';
                st_aliases = aliases;
                st_errors = List.rev_append errors state.st_errors;
                st_undefineds = List.rev_append undefs state.st_undefineds;
                st_clause_diagnostics =
                  List.rev_append cdiags state.st_clause_diagnostics;
                st_forms = result :: state.st_forms;
              }
          | None -> (
              (* Try to handle it as a defun *)
              match Infer.infer_defun env sexp with
              | Some defun_result ->
                  (* Bind the function name in the function namespace *)
                  let scheme =
                    Generalize.generalize (Env.current_level env)
                      defun_result.Infer.fn_type
                  in
                  let env' = Env.extend_fn defun_result.Infer.name scheme env in
                  (* Solve constraints and collect errors *)
                  let errors =
                    Unify.solve_all defun_result.Infer.defun_constraints
                  in
                  let result =
                    DefunForm
                      {
                        name = defun_result.Infer.name;
                        fn_type = defun_result.Infer.fn_type;
                      }
                  in
                  {
                    st_env = env';
                    st_aliases = aliases;
                    st_errors = List.rev_append errors state.st_errors;
                    st_undefineds =
                      List.rev_append defun_result.Infer.defun_undefineds
                        state.st_undefineds;
                    st_clause_diagnostics =
                      List.rev_append
                        defun_result.Infer.defun_clause_diagnostics
                        state.st_clause_diagnostics;
                    st_forms = result :: state.st_forms;
                  }
              | None ->
                  (* Regular expression *)
                  let result = Infer.infer env sexp in
                  let errors = Unify.solve_all result.Infer.constraints in
                  {
                    st_env = env;
                    st_aliases = aliases;
                    st_errors = List.rev_append errors state.st_errors;
                    st_undefineds =
                      List.rev_append result.Infer.undefineds
                        state.st_undefineds;
                    st_clause_diagnostics =
                      List.rev_append result.Infer.clause_diagnostics
                        state.st_clause_diagnostics;
                    st_forms =
                      ExprForm { ty = result.Infer.ty } :: state.st_forms;
                  })))

(** Check a single top-level form and update the environment.

    Returns the updated environment, form result, errors, and undefined vars.
    This is the backward-compatible API that doesn't track file-local aliases.
*)
let check_form (env : Env.t) (sexp : Syntax.Sexp.t) :
    Env.t * form_result * Unify.error list * Infer.undefined_var list =
  let state =
    {
      st_env = env;
      st_aliases = Sig_loader.empty_aliases;
      st_errors = [];
      st_undefineds = [];
      st_clause_diagnostics = [];
      st_forms = [];
    }
  in
  let state' = check_form_with_state state sexp in
  let result = List.hd state'.st_forms in
  ( state'.st_env,
    result,
    List.rev state'.st_errors,
    List.rev state'.st_undefineds )

(** Check a sequence of top-level forms.

    Processes forms in order, accumulating type bindings from defuns and
    file-local type aliases from tart-type forms. Uses the default environment
    with built-in types unless overridden. *)
let check_program ?(env = default_env ()) (forms : Syntax.Sexp.t list) :
    check_result =
  (* Initialize aliases with prelude to recognize primitive type names *)
  let prelude_aliases = Sig.Prelude.prelude_alias_context () in
  let init_state =
    {
      st_env = env;
      st_aliases = prelude_aliases;
      st_errors = [];
      st_undefineds = [];
      st_clause_diagnostics = [];
      st_forms = [];
    }
  in
  let final_state = List.fold_left check_form_with_state init_state forms in
  {
    env = final_state.st_env;
    forms = List.rev final_state.st_forms;
    errors = List.rev final_state.st_errors;
    undefineds = List.rev final_state.st_undefineds;
    clause_diagnostics = List.rev final_state.st_clause_diagnostics;
    aliases = final_state.st_aliases;
  }

(** Check a single expression and return its type.

    This is a convenience function for checking a single expression without
    accumulating environment changes. Uses the default environment with built-in
    types unless overridden. *)
let check_expr ?(env = default_env ()) (sexp : Syntax.Sexp.t) :
    typ * Unify.error list =
  reset_tvar_counter ();
  let result = Infer.infer env sexp in
  let errors = Unify.solve_all result.Infer.constraints in
  (repr result.Infer.ty, errors)

(** Convert a form result to a string for display *)
let form_result_to_string = function
  | DefunForm { name; fn_type } ->
      Printf.sprintf "(defun %s %s)" name (to_string fn_type)
  | DefvarForm { name; var_type } ->
      Printf.sprintf "(defvar %s %s)" name (to_string var_type)
  | TartDeclareForm { name; var_type } ->
      Printf.sprintf "(tart-declare %s %s)" name (to_string var_type)
  | TartTypeForm { name; params } ->
      if params = [] then Printf.sprintf "(tart-type %s)" name
      else Printf.sprintf "(tart-type %s [%s])" name (String.concat " " params)
  | ExprForm { ty } -> to_string ty
