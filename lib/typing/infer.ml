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
module Forall_infer = Sig.Forall_infer
module Sig_ast = Sig.Sig_ast

type undefined_var = { name : string; span : Loc.span }
(** An undefined variable reference *)

type result = { ty : typ; constraints : C.set; undefineds : undefined_var list }
(** Result of inference: the inferred type, constraints, and undefined vars *)

type defun_result = {
  name : string;
  fn_type : typ;
  defun_constraints : C.set;
  defun_undefineds : undefined_var list;
}
(** Result of inferring a top-level definition *)

type pattern_binding = { pb_name : string; pb_type : typ }
(** A binding extracted from a pcase pattern: name and its type *)

(** Create a result with no constraints and no undefined vars *)
let pure ty = { ty; constraints = C.empty; undefineds = [] }

(** Create a result with a single constraint *)
let with_constraint ty c =
  { ty; constraints = C.add c C.empty; undefineds = [] }

(** Create a result with an undefined variable error *)
let with_undefined ty name span =
  { ty; constraints = C.empty; undefineds = [ { name; span } ] }

(** Combine results, merging constraints and undefined vars *)
let combine_results results =
  List.fold_left (fun acc r -> C.combine acc r.constraints) C.empty results

(** Combine undefined variables from results *)
let combine_undefineds results = List.concat_map (fun r -> r.undefineds) results

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
let rec infer (env : Env.t) (sexp : Syntax.Sexp.t) : result =
  let open Syntax.Sexp in
  match sexp with
  (* === Literals === *)
  | Int (_, _) -> pure Prim.int
  | Float (_, _) -> pure Prim.float
  | String (_, _) -> pure Prim.string
  | Char (_, _) -> pure Prim.int (* Characters are integers in Elisp *)
  | Keyword (_, _) -> pure Prim.keyword
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
  (* === Tart type annotation: (tart TYPE FORM) === *)
  | List ([ Symbol ("tart", _); type_sexp; form ], span) ->
      infer_tart_annotation env type_sexp form span
  (* === Pcase pattern matching === *)
  | List (Symbol ("pcase", _) :: expr :: clauses, span) ->
      infer_pcase env expr clauses span
  | List (Symbol ("pcase-exhaustive", _) :: expr :: clauses, span) ->
      infer_pcase env expr clauses span
  (* === Explicit type instantiation: (@type [T1 T2 ...] fn args...) === *)
  | List (Symbol ("@type", _) :: Vector (type_args, _) :: fn :: args, span) ->
      infer_explicit_instantiation env type_args fn args span
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
  | Int (_, _) -> pure Prim.int
  | Float (_, _) -> pure Prim.float
  | String (_, _) -> pure Prim.string
  | Char (_, _) -> pure Prim.int
  | Keyword (_, _) -> pure Prim.keyword
  | Symbol (_, _) -> pure Prim.symbol
  | List (_, _) -> pure (list_of Prim.any) (* Quoted lists: (List Any) *)
  | Vector (_, _) -> pure (vector_of Prim.any)
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
  }

(** Infer the type of an if expression with else branch.

    Generates constraint: then_type = else_type Result type is a fresh variable
    unified with both branches.

    Each branch constraint includes context about the other branch for better
    error messages when branches have incompatible types. *)
and infer_if env cond then_branch else_branch _span =
  let open Syntax.Sexp in
  let cond_result = infer env cond in
  let then_result = infer env then_branch in
  let else_result = infer env else_branch in

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
  let all_undefineds =
    combine_undefineds [ cond_result; then_result; else_result ]
  in

  { ty = result_ty; constraints = all_constraints; undefineds = all_undefineds }

(** Infer the type of an if expression without else branch.

    When else is missing, the false case returns nil. Result is (Option
    then_type) if then_type is truthy, otherwise Any. *)
and infer_if_no_else env cond then_branch span =
  let cond_result = infer env cond in
  let then_result = infer env then_branch in

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
  let all_undefineds = combine_undefineds [ cond_result; then_result ] in

  (* The result type should really be (Or then_type Nil),
     but for now we just use the then type. *)
  { ty = result_ty; constraints = all_constraints; undefineds = all_undefineds }

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
            (name, scheme, result.constraints, result.undefineds)
        | List ([ Symbol (name, _) ], _) ->
            (* Binding without value: nil (not generalizable) *)
            (name, Env.Mono Prim.nil, C.empty, [])
        | _ ->
            (* Malformed binding *)
            ( "_",
              Env.Mono (fresh_tvar (Env.current_level inner_env)),
              C.empty,
              [] ))
      bindings
  in

  (* Collect constraints and undefineds from all bindings *)
  let binding_constraints =
    List.fold_left
      (fun acc (_, _, constraints, _) -> C.combine acc constraints)
      C.empty binding_schemes
  in
  let binding_undefineds =
    List.concat_map (fun (_, _, _, undefs) -> undefs) binding_schemes
  in

  (* Extend environment with all bindings (now potentially polymorphic) *)
  let body_env =
    List.fold_left
      (fun env (name, scheme, _, _) -> Env.extend name scheme env)
      env binding_schemes
  in

  (* Infer body *)
  let body_result = infer_progn body_env body span in

  {
    ty = body_result.ty;
    constraints = C.combine binding_constraints body_result.constraints;
    undefineds = binding_undefineds @ body_result.undefineds;
  }

(** Infer the type of a let* expression with generalization.

    Each binding sees previous bindings (sequential), and each is generalized
    independently. *)
and infer_let_star env bindings body span =
  let open Syntax.Sexp in
  let outer_level = Env.current_level env in

  (* Process bindings sequentially, each in the extended environment *)
  let rec process_bindings env bindings constraints undefineds =
    match bindings with
    | [] -> (env, constraints, undefineds)
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
        | List ([ Symbol (name, _) ], _) ->
            let env' = Env.extend_mono name Prim.nil env in
            process_bindings env' rest constraints undefineds
        | _ -> process_bindings env rest constraints undefineds)
  in

  let body_env, binding_constraints, binding_undefineds =
    process_bindings env bindings C.empty []
  in
  let body_result = infer_progn body_env body span in

  {
    ty = body_result.ty;
    constraints = C.combine binding_constraints body_result.constraints;
    undefineds = binding_undefineds @ body_result.undefineds;
  }

(** Infer the type of a progn expression.

    Returns the type of the last expression, or nil if empty. *)
and infer_progn env exprs span =
  match exprs with
  | [] -> pure Prim.nil
  | [ e ] -> infer env e
  | e :: rest ->
      let e_result = infer env e in
      let rest_result = infer_progn env rest span in
      {
        ty = rest_result.ty;
        constraints = C.combine e_result.constraints rest_result.constraints;
        undefineds = e_result.undefineds @ rest_result.undefineds;
      }

(** Infer the type of a setq expression.

    (setq var1 val1 var2 val2 ...) Returns the value of the last assignment. *)
and infer_setq env pairs span =
  let open Syntax.Sexp in
  let rec process_pairs pairs constraints undefineds last_ty =
    match pairs with
    | [] -> { ty = last_ty; constraints; undefineds }
    | [ _ ] ->
        (* Odd number of args - malformed, return nil *)
        { ty = Prim.nil; constraints; undefineds }
    | Symbol (name, _) :: value :: rest ->
        let result = infer env value in
        (* Check if variable exists in env *)
        let constraint_set =
          match Env.lookup name env with
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

    Each clause is (test body...). Result unifies all clause bodies. *)
and infer_cond env clauses span =
  let open Syntax.Sexp in
  let result_ty = fresh_tvar (Env.current_level env) in

  let rec process_clauses clauses constraints undefineds =
    match clauses with
    | [] -> (constraints, undefineds)
    | List (test :: body, _) :: rest ->
        let test_result = infer env test in
        let body_result = infer_progn env body span in
        let body_constraint = C.equal result_ty body_result.ty span in
        let all =
          C.combine test_result.constraints
            (C.combine body_result.constraints
               (C.add body_constraint constraints))
        in
        let all_undefineds =
          undefineds @ test_result.undefineds @ body_result.undefineds
        in
        process_clauses rest all all_undefineds
    | _ :: rest -> process_clauses rest constraints undefineds
  in

  let all_constraints, all_undefineds = process_clauses clauses C.empty [] in
  { ty = result_ty; constraints = all_constraints; undefineds = all_undefineds }

(** Infer the type of an and expression.

    Returns the last value if all are truthy, or the first falsy value. Type is
    the type of the last argument (simplified). *)
and infer_and env args _span =
  let results = List.map (infer env) args in
  let constraints = combine_results results in
  let undefineds = combine_undefineds results in
  match List.rev results with
  | last :: _ -> { ty = last.ty; constraints; undefineds }
  | [] -> pure Prim.t

(** Infer the type of an or expression.

    Returns the first truthy value, or the last value. Type is a union of all
    argument types (simplified to last). *)
and infer_or env args _span =
  let results = List.map (infer env) args in
  let constraints = combine_results results in
  let undefineds = combine_undefineds results in
  match List.rev results with
  | last :: _ -> { ty = last.ty; constraints; undefineds }
  | [] -> pure Prim.nil

(** Infer the type of a not expression.

    (not x) always returns a boolean. *)
and infer_not env arg _span =
  let arg_result = infer env arg in
  {
    ty = Prim.bool;
    constraints = arg_result.constraints;
    undefineds = arg_result.undefineds;
  }

(** {1 Pcase Pattern Matching}

    Support for type narrowing in pcase patterns. ADT patterns like
    [`(ok . ,value)] bind variables with the appropriate field types. *)

(** Extract bindings from a pcase pattern.

    Recognizes:
    - [`(TAG . ,var)] - cons pattern with backquote, extracts var with field type
    - [`(TAG ,v1 ,v2 ...)] - vector pattern for multi-field constructors
    - [,var] - unquote captures the matched value
    - [_] - wildcard, no bindings
    - Other patterns - no type narrowing (treated as Any)

    The [scrutinee_ty] is used to determine field types when matching ADT
    constructors. *)
and extract_pattern_bindings (env : Env.t) (_scrutinee_ty : typ)
    (pattern : Syntax.Sexp.t) : pattern_binding list =
  let open Syntax.Sexp in
  let level = Env.current_level env in
  match pattern with
  (* Backquote pattern: `(tag . ,value) or `(tag ,v1 ,v2) *)
  | List ([ Symbol ("\\`", _); quoted_pat ], _) ->
      extract_backquote_bindings env level quoted_pat
  (* Underscore wildcard *)
  | Symbol ("_", _) -> []
  (* Literal patterns - no bindings *)
  | Int _ | Float _ | String _ | Keyword _ -> []
  (* Quoted symbol - no bindings *)
  | List ([ Symbol ("quote", _); Symbol (_, _) ], _) -> []
  (* Comma (unquote) at top level: ,var captures the whole value *)
  | List ([ Symbol (",", _); Symbol (name, _) ], _) ->
      [ { pb_name = name; pb_type = fresh_tvar level } ]
  (* pred pattern: (pred PREDICATE) - no bindings captured *)
  | List ([ Symbol ("pred", _); _ ], _) -> []
  (* guard pattern: (guard EXPR) - no bindings *)
  | List ([ Symbol ("guard", _); _ ], _) -> []
  (* and pattern: (and PAT1 PAT2 ...) - combine all bindings *)
  | List (Symbol ("and", _) :: pats, _) ->
      List.concat_map (extract_pattern_bindings env _scrutinee_ty) pats
  (* or pattern: (or PAT1 PAT2 ...) - use first pattern's bindings *)
  | List (Symbol ("or", _) :: pat :: _, _) ->
      extract_pattern_bindings env _scrutinee_ty pat
  (* let pattern: (let VAR EXPR) - binds var *)
  | List ([ Symbol ("let", _); Symbol (name, _); _ ], _) ->
      [ { pb_name = name; pb_type = fresh_tvar level } ]
  (* app pattern: (app FN PAT) - bindings from inner pattern *)
  | List ([ Symbol ("app", _); _; inner_pat ], _) ->
      extract_pattern_bindings env _scrutinee_ty inner_pat
  (* Other patterns - no bindings *)
  | _ -> []

(** Extract bindings from a backquoted pattern (inside backquote).

    Handles:
    - [(TAG . ,var)] - cons cell ADT pattern
    - [(TAG ,v1 ,v2 ...)] - list ADT pattern (multi-field)
    - [[TAG ,v1 ,v2 ...]] - vector ADT pattern
    - Nested structures *)
and extract_backquote_bindings (env : Env.t) (level : int)
    (pattern : Syntax.Sexp.t) : pattern_binding list =
  let open Syntax.Sexp in
  match pattern with
  (* Cons pattern: (tag . ,value) - ADT single-field pattern *)
  | Cons (Symbol (_tag, _), List ([ Symbol (",", _); Symbol (name, _) ], _), _)
    ->
      (* For now, give the binding a fresh type variable.
         In the future, we could look up the ADT definition and use the field type. *)
      [ { pb_name = name; pb_type = fresh_tvar level } ]
  (* Cons pattern with nested expr: (tag . ,expr) where expr is not a simple symbol *)
  | Cons (Symbol (_, _), List ([ Symbol (",", _); _ ], _), _) -> []
  (* List pattern with tag: (tag ,v1 ,v2 ...) *)
  | List (Symbol (_tag, _) :: rest, _) ->
      List.concat_map (extract_unquote_binding level) rest
  (* Vector pattern: [tag ,v1 ,v2 ...] *)
  | Vector (Symbol (_tag, _) :: rest, _) ->
      List.concat_map (extract_unquote_binding level) rest
  (* Nested cons: (a . (b . c)) *)
  | Cons (car, cdr, _) ->
      extract_backquote_bindings env level car
      @ extract_backquote_bindings env level cdr
  (* Plain cons pattern without symbol *)
  | List (elems, _) -> List.concat_map (extract_backquote_bindings env level) elems
  (* Unquoted variable at this level *)
  | _ -> extract_unquote_binding level pattern

(** Extract binding from an unquote expression: ,var *)
and extract_unquote_binding (level : int) (sexp : Syntax.Sexp.t) :
    pattern_binding list =
  let open Syntax.Sexp in
  match sexp with
  | List ([ Symbol (",", _); Symbol (name, _) ], _) ->
      [ { pb_name = name; pb_type = fresh_tvar level } ]
  | _ -> []

(** Infer the type of a pcase expression.

    (pcase EXPR (PAT1 BODY1...) (PAT2 BODY2...) ...)

    Each clause has a pattern and a body. Pattern-bound variables are available
    in the body with narrowed types. All branch bodies must have the same type
    (the result type of the pcase). *)
and infer_pcase (env : Env.t) (expr : Syntax.Sexp.t)
    (clauses : Syntax.Sexp.t list) (_span : Loc.span) : result =
  let open Syntax.Sexp in
  (* Infer the type of the scrutinee *)
  let expr_result = infer env expr in
  let scrutinee_ty = expr_result.ty in

  (* Result type is a fresh variable that all branches unify with *)
  let result_ty = fresh_tvar (Env.current_level env) in

  (* Process each clause *)
  let rec process_clauses clauses constraints undefineds =
    match clauses with
    | [] -> (constraints, undefineds)
    | List (pattern :: body, clause_span) :: rest ->
        (* Extract pattern bindings with types *)
        let bindings = extract_pattern_bindings env scrutinee_ty pattern in

        (* Extend environment with pattern bindings *)
        let body_env =
          List.fold_left
            (fun e { pb_name; pb_type } -> Env.extend_mono pb_name pb_type e)
            env bindings
        in

        (* Infer body type *)
        let body_result = infer_progn body_env body clause_span in

        (* Add constraint: body type = result type *)
        let body_constraint = C.equal result_ty body_result.ty clause_span in

        let all_constraints =
          C.combine body_result.constraints (C.add body_constraint constraints)
        in
        let all_undefineds = undefineds @ body_result.undefineds in

        process_clauses rest all_constraints all_undefineds
    | _ :: rest ->
        (* Malformed clause - skip it *)
        process_clauses rest constraints undefineds
  in

  let clause_constraints, clause_undefineds =
    process_clauses clauses C.empty []
  in

  (* Combine all constraints *)
  let all_constraints = C.combine expr_result.constraints clause_constraints in
  let all_undefineds = expr_result.undefineds @ clause_undefineds in

  { ty = result_ty; constraints = all_constraints; undefineds = all_undefineds }

(** Infer the type of a tart annotation expression.

    (tart TYPE FORM) checks that FORM's inferred type is compatible with TYPE.
    The result type is TYPE, allowing the annotation to refine the type. *)
and infer_tart_annotation env type_sexp form _span =
  let open Syntax.Sexp in
  (* Infer the type of the form *)
  let form_result = infer env form in

  (* Parse the type annotation *)
  match parse_tart_type type_sexp with
  | Some sig_type ->
      (* Apply forall inference to get implicit quantifiers *)
      let sig_type = Forall_infer.infer_sig_type ~known_types:[] sig_type in

      (* Extract type variable names from forall, if present *)
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
      let base_ty = Sig_loader.sig_type_to_typ tvar_names inner_type in
      let declared_ty = substitute_tvar_names tvar_subst base_ty in

      (* Create constraint: form's type = declared type *)
      let context = C.TartAnnotation { declared_type = declared_ty } in
      let annotation_constraint =
        C.equal ~context declared_ty form_result.ty (span_of form)
      in

      {
        ty = declared_ty;
        constraints = C.add annotation_constraint form_result.constraints;
        undefineds = form_result.undefineds;
      }
  | None ->
      (* Parse failed - return form's type unchanged with a fresh tvar *)
      { form_result with ty = fresh_tvar (Env.current_level env) }

(** Infer the type of an explicit type instantiation.

    (@type [T1 T2 ...] fn args...) explicitly instantiates the polymorphic
    function fn with the given type arguments, then applies it to args.

    The type arguments are parsed using sig_parser and applied to the function's
    type parameters in order. The underscore [_] can be used as a placeholder
    to let inference determine that type argument. *)
and infer_explicit_instantiation (env : Env.t) (type_arg_sexps : Syntax.Sexp.t list)
    (fn : Syntax.Sexp.t) (args : Syntax.Sexp.t list) (span : Loc.span) : result =
  let open Syntax.Sexp in
  (* Parse the type arguments.
     Note: We do NOT apply forall inference here because type arguments in @type
     are meant to be concrete instantiations, not polymorphic types. A simple
     name like "list" should become TCon "List", not a forall-wrapped type var. *)
  let parsed_type_args =
    List.map
      (fun sexp ->
        match sexp with
        | Symbol ("_", _) -> None (* Placeholder - infer this type *)
        | _ -> (
            match Sig_parser.parse_sig_type sexp with
            | Ok sig_type ->
                (* Convert directly without forall inference *)
                Some (Sig_loader.sig_type_to_typ [] sig_type)
            | Error _ -> None))
      type_arg_sexps
  in

  (* Look up the function's type *)
  let fn_name = get_fn_name fn in
  let fn_scheme =
    match fn_name with
    | Some name -> Env.lookup name env
    | None -> None
  in

  (* Get the function's quantified type variables and body type *)
  let tvar_names, fn_body_type =
    match fn_scheme with
    | Some (Env.Poly (vars, _constraints, ty)) -> (vars, ty)
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
  let instantiated_fn_type = substitute_tvar_names tvar_subst fn_body_type in

  (* Now type-check the application with the instantiated type *)
  let arg_results = List.map (infer env) args in

  (* Fresh type variable for the result *)
  let result_ty = fresh_tvar (Env.current_level env) in

  (* Build expected function type with per-argument constraints *)
  let type_args_for_context =
    List.map (fun (_, ty) -> ty) tvar_subst
  in
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
    List.fold_left (fun acc (_, c) -> C.add c acc) C.empty arg_constraints_with_context
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

  { ty = result_ty; constraints = all_constraints; undefineds = all_undefineds }

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

(** Infer the type of a function application.

    Generates constraint: fn_type = (arg_types...) -> result_type *)
and infer_application env fn args span =
  let fn_result = infer env fn in
  let arg_results = List.map (infer env) args in
  let fn_name = get_fn_name fn in

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
      (C.combine arg_constraints (C.add fn_constraint arg_type_constraints))
  in
  let all_undefineds = fn_result.undefineds @ combine_undefineds arg_results in

  { ty = result_ty; constraints = all_constraints; undefineds = all_undefineds }

(** Infer a defun as an expression.

    As an expression, defun returns a symbol (the function name). The function
    type is inferred like a lambda.

    For the side effect of binding the name to the type, use [infer_defun]. *)
(** Extract (declare (tart TYPE)) from a defun body.

    Returns [(Some type_sexp, remaining_body)] if a tart declaration is found,
    or [(None, body)] if not. The declare form must be the first expression
    in the body. *)
and extract_tart_declare (body : Syntax.Sexp.t list) :
    Syntax.Sexp.t option * Syntax.Sexp.t list =
  let open Syntax.Sexp in
  match body with
  | List ([ Symbol ("declare", _); List (Symbol ("tart", _) :: type_parts, _) ], _)
    :: rest ->
      (* Found (declare (tart ...)), extract the type expression.
         The type parts should form a single arrow type: (params) -> return
         or [vars] (params) -> return *)
      let type_sexp =
        match type_parts with
        | [ single ] -> single
        | _ ->
            (* Multiple parts - wrap in a list to form a type expression *)
            List (type_parts, Loc.dummy_span)
      in
      (Some type_sexp, rest)
  | _ -> (None, body)

(** Parse a tart type annotation from an S-expression.

    Returns the parsed sig_type or None if parsing fails. *)
and parse_tart_type (type_sexp : Syntax.Sexp.t) : Sig_ast.sig_type option =
  match Sig_parser.parse_sig_type type_sexp with
  | Ok sig_type -> Some sig_type
  | Error _ -> None

and infer_defun_as_expr env _name params body span =
  (* Infer as a lambda, but return Symbol as the expression type *)
  let fn_result = infer_lambda env params body span in
  (* defun returns the symbol naming the function, but propagate undefineds *)
  {
    ty = Prim.symbol;
    constraints = fn_result.constraints;
    undefineds = fn_result.undefineds;
  }

(** Infer the type of a defun and return the binding information.

    Unlike [infer_defun_as_expr], this returns the function name and type so
    callers can bind it in the environment.

    If the body contains [(declare (tart TYPE))], the declared type is used
    instead of inferring. The body is checked against the declared return type. *)
and infer_defun (env : Env.t) (sexp : Syntax.Sexp.t) : defun_result option =
  let open Syntax.Sexp in
  match sexp with
  | List
      (Symbol ("defun", _) :: Symbol (name, _) :: List (params, _) :: body, span)
    ->
      (* Check for inline type declaration *)
      let tart_decl, actual_body = extract_tart_declare body in
      (match tart_decl with
      | Some type_sexp -> (
          match parse_tart_type type_sexp with
          | Some sig_type ->
              (* Apply forall inference if no explicit quantifiers *)
              let sig_type =
                Forall_infer.infer_sig_type ~known_types:[] sig_type
              in
              infer_defun_with_declaration env name params actual_body span
                sig_type
          | None ->
              (* Parse failed - fall through to regular inference *)
              infer_defun_inferred env name params body span)
      | None ->
          (* No declaration - infer as usual *)
          infer_defun_inferred env name params body span)
  | _ -> None

(** Substitute type variable names (as TCon) with actual TVars in a type.
    Used to convert signature types to inferable types. *)
and substitute_tvar_names (subst : (string * typ) list) (ty : typ) : typ =
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
            | PKey (n, t) -> PKey (n, substitute_tvar_names subst t))
          params
      in
      TArrow (params', substitute_tvar_names subst ret)
  | TApp (con, args) ->
      TApp (substitute_tvar_names subst con, List.map (substitute_tvar_names subst) args)
  | TForall (vars, body) ->
      (* Remove substituted vars from the substitution to avoid capture *)
      let subst' = List.filter (fun (n, _) -> not (List.mem n vars)) subst in
      TForall (vars, substitute_tvar_names subst' body)
  | TUnion types -> TUnion (List.map (substitute_tvar_names subst) types)
  | TTuple types -> TTuple (List.map (substitute_tvar_names subst) types)

(** Infer a defun with a type declaration.

    Uses the declared type for parameters and checks the body against
    the declared return type. Creates fresh type variables for polymorphic
    type parameters so they can be properly generalized. *)
and infer_defun_with_declaration (env : Env.t) (name : string)
    (params : Syntax.Sexp.t list) (body : Syntax.Sexp.t list) (span : Loc.span)
    (sig_type : Sig_ast.sig_type) : defun_result option =
  (* Extract type variable names from forall, if present *)
  let tvar_names, arrow_type =
    match sig_type with
    | Sig_ast.STForall (binders, inner, _) ->
        (List.map (fun b -> b.Sig_ast.name) binders, inner)
    | _ -> ([], sig_type)
  in

  (* Extract parameter types and return type from arrow *)
  let sig_params, return_sig_type =
    match arrow_type with
    | Sig_ast.STArrow (params, ret, _) -> (params, ret)
    | _ ->
        (* Not an arrow type - treat as nullary function returning this type *)
        ([], arrow_type)
  in

  let inner_env = Env.enter_level env in

  (* Create fresh type variables for each declared type variable name.
     This is critical: we need actual TVar refs, not TCon names. *)
  let tvar_subst =
    List.map
      (fun name -> (name, fresh_tvar (Env.current_level inner_env)))
      tvar_names
  in

  (* Convert sig_params to types, then substitute fresh TVars for type var names *)
  let convert_and_subst sty =
    let base_ty = Sig_loader.sig_type_to_typ tvar_names sty in
    substitute_tvar_names tvar_subst base_ty
  in

  (* Bind parameters with declared types (with fresh TVars substituted) *)
  let param_info =
    let open Syntax.Sexp in
    let rec loop params sig_params acc =
      match (params, sig_params) with
      | [], _ | _, [] -> List.rev acc
      | Symbol (pname, _) :: ps, sp :: sps ->
          let ty =
            match sp with
            | Sig_ast.SPPositional sty | Sig_ast.SPOptional sty
            | Sig_ast.SPRest sty | Sig_ast.SPKey (_, sty) ->
                convert_and_subst sty
          in
          loop ps sps ((pname, ty) :: acc)
      | _ :: ps, _ :: sps -> loop ps sps acc
    in
    loop params sig_params []
  in

  (* Extend environment with parameter types *)
  let body_env = Env.extend_monos param_info inner_env in

  (* Infer body *)
  let body_result = infer_progn body_env body span in

  (* Convert declared return type (with fresh TVars) *)
  let declared_return = convert_and_subst return_sig_type in

  (* Add constraint: body type = declared return type *)
  let context = C.DeclaredReturn { fn_name = name; declared_type = declared_return } in
  let return_constraint = C.equal ~context body_result.ty declared_return span in
  let all_constraints = C.add return_constraint body_result.constraints in

  (* Solve constraints *)
  let _ = Unify.solve all_constraints in

  (* Build function type from declaration (using fresh TVars) *)
  let param_types =
    List.map
      (fun sp ->
        match sp with
        | Sig_ast.SPPositional sty -> PPositional (convert_and_subst sty)
        | Sig_ast.SPOptional sty -> POptional (convert_and_subst sty)
        | Sig_ast.SPRest sty -> PRest (convert_and_subst sty)
        | Sig_ast.SPKey (kname, sty) -> PKey (kname, convert_and_subst sty))
      sig_params
  in
  let fn_type = TArrow (param_types, declared_return) in

  (* Return the type with fresh TVars still unbound.
     check_form will call G.generalize to create the proper polymorphic scheme. *)
  Some
    {
      name;
      fn_type;
      defun_constraints = all_constraints;
      defun_undefineds = body_result.undefineds;
    }

(** Infer a defun without type declaration (original behavior). *)
and infer_defun_inferred (env : Env.t) (name : string)
    (params : Syntax.Sexp.t list) (body : Syntax.Sexp.t list) (span : Loc.span) :
    defun_result option =
  let open Syntax.Sexp in
  let outer_level = Env.current_level env in
  let inner_env = Env.enter_level env in

  (* Create fresh type variables for each parameter *)
  let param_info =
    List.map
      (fun p ->
        match p with
        | Symbol (pname, _) ->
            let tv = fresh_tvar (Env.current_level inner_env) in
            (pname, tv)
        | _ ->
            let tv = fresh_tvar (Env.current_level inner_env) in
            ("_", tv))
      params
  in

  (* Extend environment with parameter types *)
  let body_env = Env.extend_monos param_info inner_env in

  (* Infer body as a progn *)
  let body_result = infer_progn body_env body span in

  (* Solve constraints for the defun body *)
  let _ = Unify.solve body_result.constraints in

  (* Build function type *)
  let param_types = List.map (fun (_, ty) -> PPositional ty) param_info in
  let fn_type = TArrow (param_types, body_result.ty) in

  (* Generalize the function type (defun is always a syntactic value) *)
  let scheme = G.generalize outer_level fn_type in
  let generalized_ty =
    match scheme with
    | Env.Mono ty -> ty
    | Env.Poly (vars, _constraints, ty) -> TForall (vars, ty)
  in

  Some
    {
      name;
      fn_type = generalized_ty;
      defun_constraints = body_result.constraints;
      defun_undefineds = body_result.undefineds;
    }

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

      (* All elements must unify with the first *)
      let elem_constraints =
        List.map (fun r -> C.equal first_result.ty r.ty span) rest_results
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
        ty = vector_of first_result.ty;
        constraints = all_constraints;
        undefineds = all_undefineds;
      }
