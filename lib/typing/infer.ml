(** Constraint-based type inference.

    This module generates type constraints for Elisp expressions.
    The main entry point is [infer], which takes an expression and
    environment and returns (type, constraints).

    Constraints are equality constraints τ₁ = τ₂ that are later
    solved by unification.
*)

open Core.Types
module Env = Core.Type_env
module C = Constraint
module G = Generalize

(** Result of inference: the inferred type and generated constraints *)
type result = {
  ty : typ;
  constraints : C.set;
}

(** Result of inferring a top-level definition *)
type defun_result = {
  name : string;
  fn_type : typ;
  defun_constraints : C.set;
}

(** Create a result with no constraints *)
let pure ty = { ty; constraints = C.empty }

(** Create a result with a single constraint *)
let with_constraint ty c = { ty; constraints = C.add c C.empty }

(** Combine results, merging constraints *)
let combine_results results =
  List.fold_left
    (fun acc r -> C.combine acc r.constraints)
    C.empty results

(** Infer the type of an S-expression.

    This generates constraints but does not solve them.
    Call [Unify.solve] on the constraints to unify type variables.

    Expressions handled:
    - Literals: produce base types (no constraints)
    - Variables: instantiate from environment (no constraints)
    - Lambda: introduce fresh type vars for params
    - Application: generate τ_fun = (τ_args...) -> τ_result
    - Quote: quoted expressions have type based on their structure
    - If: branches must unify; result is their common type
    - Let: generate constraints for bindings (generalization in R5)
    - Progn: result is type of last expression
*)
let rec infer (env : Env.t) (sexp : Syntax.Sexp.t) : result =
  let open Syntax.Sexp in
  match sexp with
  (* === Literals === *)
  | Int (_, _) -> pure Prim.int
  | Float (_, _) -> pure Prim.float
  | String (_, _) -> pure Prim.string
  | Char (_, _) -> pure Prim.int  (* Characters are integers in Elisp *)
  | Keyword (_, _) -> pure Prim.keyword

  (* === Variables === *)
  | Symbol (name, _span) -> (
      match Env.lookup name env with
      | Some scheme ->
          (* Instantiate polymorphic types with fresh type variables *)
          let ty = Env.instantiate scheme env in
          pure ty
      | None ->
          (* Unbound variable - for now, give it a fresh type variable.
             Later, this should report an error. *)
          pure (fresh_tvar (Env.current_level env)))

  (* === Quoted expressions === *)
  | List ([Symbol ("quote", _); quoted], _) ->
      infer_quoted quoted

  (* === Lambda expressions === *)
  | List (Symbol ("lambda", _) :: List (params, _) :: body, span) ->
      infer_lambda env params body span

  (* === If expressions === *)
  | List ([Symbol ("if", _); cond; then_branch; else_branch], span) ->
      infer_if env cond then_branch else_branch span

  | List ([Symbol ("if", _); cond; then_branch], span) ->
      (* If without else returns nil when condition is false *)
      infer_if_no_else env cond then_branch span

  (* === Let expressions === *)
  | List (Symbol ("let", _) :: List (bindings, _) :: body, span) ->
      infer_let env bindings body span

  | List (Symbol ("let*", _) :: List (bindings, _) :: body, span) ->
      infer_let_star env bindings body span

  (* === Defun - returns symbol, but binds function type === *)
  | List (Symbol ("defun", _) :: Symbol (name, _) :: List (params, _) :: body, span) ->
      infer_defun_as_expr env name params body span

  (* === Progn (implicit in many forms) === *)
  | List (Symbol ("progn", _) :: exprs, span) ->
      infer_progn env exprs span

  (* === Setq - assignment === *)
  | List (Symbol ("setq", _) :: rest, span) ->
      infer_setq env rest span

  (* === Cond === *)
  | List (Symbol ("cond", _) :: clauses, span) ->
      infer_cond env clauses span

  (* === And/Or === *)
  | List ([Symbol ("and", _)], _) ->
      pure Prim.t  (* (and) with no args returns t *)
  | List (Symbol ("and", _) :: args, span) ->
      infer_and env args span

  | List ([Symbol ("or", _)], _) ->
      pure Prim.nil  (* (or) with no args returns nil *)
  | List (Symbol ("or", _) :: args, span) ->
      infer_or env args span

  (* === Not === *)
  | List ([Symbol ("not", _); arg], span) ->
      infer_not env arg span

  (* === Function application (catch-all for lists) === *)
  | List (fn :: args, span) ->
      infer_application env fn args span

  | List ([], _span) ->
      (* Empty list is nil *)
      pure Prim.nil

  (* === Vectors === *)
  | Vector (elems, span) ->
      infer_vector env elems span

  (* === Cons pairs - not valid expressions === *)
  | Cons (_, _, _span) ->
      (* Dotted pairs as expressions are typically errors,
         but we'll give them a fresh type variable *)
      pure (fresh_tvar (Env.current_level env))

  (* === Error nodes === *)
  | Error (_, _) ->
      pure (fresh_tvar (Env.current_level env))

(** Infer the type of a quoted expression.

    Quoted data has types based on structure:
    - Quoted symbol: Symbol
    - Quoted list: (List Any) for now (proper list inference later)
    - Other literals: their literal types
*)
and infer_quoted (sexp : Syntax.Sexp.t) : result =
  let open Syntax.Sexp in
  match sexp with
  | Int (_, _) -> pure Prim.int
  | Float (_, _) -> pure Prim.float
  | String (_, _) -> pure Prim.string
  | Char (_, _) -> pure Prim.int
  | Keyword (_, _) -> pure Prim.keyword
  | Symbol (_, _) -> pure Prim.symbol
  | List (_, _) -> pure (list_of Prim.any)  (* Quoted lists: (List Any) *)
  | Vector (_, _) -> pure (vector_of Prim.any)
  | Cons (_, _, _) -> pure Prim.any  (* Dotted pairs: Any *)
  | Error (_, _) -> pure Prim.any

(** Infer the type of a lambda expression.

    Creates fresh type variables for each parameter and infers
    the body type in the extended environment.
*)
and infer_lambda env params body span =
  let open Syntax.Sexp in
  (* Create fresh type variables for each parameter *)
  let param_info = List.map (fun p ->
    match p with
    | Symbol (name, _) ->
        let tv = fresh_tvar (Env.current_level env) in
        (name, tv)
    | _ ->
        (* Non-symbol in parameter list - give it a fresh var *)
        let tv = fresh_tvar (Env.current_level env) in
        ("_", tv)
  ) params in

  (* Extend environment with parameter types *)
  let body_env = Env.extend_monos param_info env in

  (* Infer body as a progn *)
  let body_result = infer_progn body_env body span in

  (* Build function type *)
  let param_types = List.map (fun (_, ty) -> PPositional ty) param_info in
  let fn_type = TArrow (param_types, body_result.ty) in

  { ty = fn_type; constraints = body_result.constraints }

(** Infer the type of an if expression with else branch.

    Generates constraint: then_type = else_type
    Result type is a fresh variable unified with both branches.
*)
and infer_if env cond then_branch else_branch span =
  let cond_result = infer env cond in
  let then_result = infer env then_branch in
  let else_result = infer env else_branch in

  (* Result type is a fresh variable *)
  let result_ty = fresh_tvar (Env.current_level env) in

  (* Constraints: both branches unify with result *)
  let then_constraint = C.equal result_ty then_result.ty span in
  let else_constraint = C.equal result_ty else_result.ty span in

  let all_constraints =
    C.combine cond_result.constraints
      (C.combine then_result.constraints
         (C.combine else_result.constraints
            (C.add then_constraint (C.add else_constraint C.empty))))
  in

  { ty = result_ty; constraints = all_constraints }

(** Infer the type of an if expression without else branch.

    When else is missing, the false case returns nil.
    Result is (Option then_type) if then_type is truthy, otherwise Any.
*)
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
      (C.combine then_result.constraints
         (C.add then_constraint C.empty))
  in

  (* The result type should really be (Or then_type Nil),
     but for now we just use the then type. *)
  { ty = result_ty; constraints = all_constraints }

(** Infer the type of a let expression with generalization.

    Let bindings use levels-based generalization:
    1. Enter a new level for the binding scope
    2. Infer each binding's type at the higher level
    3. Solve constraints for each binding
    4. Generalize type variables at level > outer level (if syntactic value)
    5. Add generalized bindings to environment and infer body
*)
and infer_let env bindings body span =
  let open Syntax.Sexp in
  let outer_level = Env.current_level env in
  let inner_env = Env.enter_level env in

  (* Process all bindings, inferring and solving each one *)
  let binding_schemes = List.map (fun binding ->
    match binding with
    | List ([Symbol (name, _); expr], _) ->
        let result = infer inner_env expr in
        (* Solve constraints immediately for this binding *)
        let _ = Unify.solve result.constraints in
        (* Generalize if it's a syntactic value *)
        let scheme = G.generalize_if_value outer_level result.ty expr in
        (name, scheme, result.constraints)
    | List ([Symbol (name, _)], _) ->
        (* Binding without value: nil (not generalizable) *)
        (name, Env.Mono Prim.nil, C.empty)
    | _ ->
        (* Malformed binding *)
        ("_", Env.Mono (fresh_tvar (Env.current_level inner_env)), C.empty)
  ) bindings in

  (* Collect constraints from all bindings *)
  let binding_constraints =
    List.fold_left
      (fun acc (_, _, constraints) -> C.combine acc constraints)
      C.empty binding_schemes
  in

  (* Extend environment with all bindings (now potentially polymorphic) *)
  let body_env = List.fold_left
    (fun env (name, scheme, _) -> Env.extend name scheme env)
    env binding_schemes
  in

  (* Infer body *)
  let body_result = infer_progn body_env body span in

  { ty = body_result.ty;
    constraints = C.combine binding_constraints body_result.constraints }

(** Infer the type of a let* expression with generalization.

    Each binding sees previous bindings (sequential), and each is
    generalized independently.
*)
and infer_let_star env bindings body span =
  let open Syntax.Sexp in
  let outer_level = Env.current_level env in

  (* Process bindings sequentially, each in the extended environment *)
  let rec process_bindings env bindings constraints =
    match bindings with
    | [] -> (env, constraints)
    | binding :: rest -> (
        let inner_env = Env.enter_level env in
        match binding with
        | List ([Symbol (name, _); expr], _) ->
            let result = infer inner_env expr in
            (* Solve constraints immediately *)
            let _ = Unify.solve result.constraints in
            (* Generalize if syntactic value *)
            let scheme = G.generalize_if_value outer_level result.ty expr in
            let env' = Env.extend name scheme env in
            process_bindings env' rest (C.combine constraints result.constraints)
        | List ([Symbol (name, _)], _) ->
            let env' = Env.extend_mono name Prim.nil env in
            process_bindings env' rest constraints
        | _ ->
            process_bindings env rest constraints)
  in

  let (body_env, binding_constraints) = process_bindings env bindings C.empty in
  let body_result = infer_progn body_env body span in

  { ty = body_result.ty;
    constraints = C.combine binding_constraints body_result.constraints }

(** Infer the type of a progn expression.

    Returns the type of the last expression, or nil if empty.
*)
and infer_progn env exprs span =
  match exprs with
  | [] -> pure Prim.nil
  | [e] -> infer env e
  | e :: rest ->
      let e_result = infer env e in
      let rest_result = infer_progn env rest span in
      { ty = rest_result.ty;
        constraints = C.combine e_result.constraints rest_result.constraints }

(** Infer the type of a setq expression.

    (setq var1 val1 var2 val2 ...)
    Returns the value of the last assignment.
*)
and infer_setq env pairs span =
  let open Syntax.Sexp in
  let rec process_pairs pairs constraints last_ty =
    match pairs with
    | [] -> { ty = last_ty; constraints }
    | [_] ->
        (* Odd number of args - malformed, return nil *)
        { ty = Prim.nil; constraints }
    | Symbol (name, _) :: value :: rest ->
        let result = infer env value in
        (* Check if variable exists in env *)
        let constraint_set = match Env.lookup name env with
          | Some scheme ->
              (* Generate constraint: value type = variable type *)
              let var_ty = Env.instantiate scheme env in
              C.add (C.equal var_ty result.ty span) result.constraints
          | None ->
              (* New variable - no constraint needed *)
              result.constraints
        in
        process_pairs rest (C.combine constraints constraint_set) result.ty
    | _ :: _ :: rest ->
        (* Non-symbol in var position *)
        process_pairs rest constraints last_ty
  in
  process_pairs pairs C.empty Prim.nil

(** Infer the type of a cond expression.

    Each clause is (test body...). Result unifies all clause bodies.
*)
and infer_cond env clauses span =
  let open Syntax.Sexp in
  let result_ty = fresh_tvar (Env.current_level env) in

  let rec process_clauses clauses constraints =
    match clauses with
    | [] -> constraints
    | List (test :: body, _) :: rest ->
        let test_result = infer env test in
        let body_result = infer_progn env body span in
        let body_constraint = C.equal result_ty body_result.ty span in
        let all = C.combine test_result.constraints
                    (C.combine body_result.constraints
                       (C.add body_constraint constraints)) in
        process_clauses rest all
    | _ :: rest ->
        process_clauses rest constraints
  in

  let all_constraints = process_clauses clauses C.empty in
  { ty = result_ty; constraints = all_constraints }

(** Infer the type of an and expression.

    Returns the last value if all are truthy, or the first falsy value.
    Type is the type of the last argument (simplified).
*)
and infer_and env args _span =
  let results = List.map (infer env) args in
  let constraints = combine_results results in
  match List.rev results with
  | last :: _ -> { ty = last.ty; constraints }
  | [] -> pure Prim.t

(** Infer the type of an or expression.

    Returns the first truthy value, or the last value.
    Type is a union of all argument types (simplified to last).
*)
and infer_or env args _span =
  let results = List.map (infer env) args in
  let constraints = combine_results results in
  match List.rev results with
  | last :: _ -> { ty = last.ty; constraints }
  | [] -> pure Prim.nil

(** Infer the type of a not expression.

    (not x) always returns a boolean.
*)
and infer_not env arg _span =
  let arg_result = infer env arg in
  { ty = Prim.bool; constraints = arg_result.constraints }

(** Infer the type of a function application.

    Generates constraint: fn_type = (arg_types...) -> result_type
*)
and infer_application env fn args span =
  let fn_result = infer env fn in
  let arg_results = List.map (infer env) args in

  (* Fresh type variable for the result *)
  let result_ty = fresh_tvar (Env.current_level env) in

  (* Build expected function type *)
  let arg_types = List.map (fun r -> PPositional r.ty) arg_results in
  let expected_fn_type = TArrow (arg_types, result_ty) in

  (* Constraint: actual function type = expected function type *)
  let fn_constraint = C.equal fn_result.ty expected_fn_type span in

  (* Combine all constraints *)
  let arg_constraints = combine_results arg_results in
  let all_constraints =
    C.combine fn_result.constraints
      (C.combine arg_constraints (C.add fn_constraint C.empty))
  in

  { ty = result_ty; constraints = all_constraints }

(** Infer a defun as an expression.

    As an expression, defun returns a symbol (the function name).
    The function type is inferred like a lambda.

    For the side effect of binding the name to the type, use [infer_defun].
*)
and infer_defun_as_expr env _name params body span =
  (* Infer as a lambda, but return Symbol as the expression type *)
  let _fn_result = infer_lambda env params body span in
  (* defun returns the symbol naming the function *)
  pure Prim.symbol

(** Infer the type of a defun and return the binding information.

    Unlike [infer_defun_as_expr], this returns the function name and type
    so callers can bind it in the environment.
*)
and infer_defun (env : Env.t) (sexp : Syntax.Sexp.t) : defun_result option =
  let open Syntax.Sexp in
  match sexp with
  | List (Symbol ("defun", _) :: Symbol (name, _) :: List (params, _) :: body, span) ->
      let outer_level = Env.current_level env in
      let inner_env = Env.enter_level env in

      (* Create fresh type variables for each parameter *)
      let param_info = List.map (fun p ->
        match p with
        | Symbol (pname, _) ->
            let tv = fresh_tvar (Env.current_level inner_env) in
            (pname, tv)
        | _ ->
            let tv = fresh_tvar (Env.current_level inner_env) in
            ("_", tv)
      ) params in

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
      let generalized_ty = match scheme with
        | Env.Mono ty -> ty
        | Env.Poly (vars, ty) -> TForall (vars, ty)
      in

      Some { name; fn_type = generalized_ty; defun_constraints = body_result.constraints }
  | _ -> None

(** Infer the type of a vector literal.

    All elements must have the same type.
*)
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
      let elem_constraints = List.map
        (fun r -> C.equal first_result.ty r.ty span)
        rest_results
      in

      let all_constraints =
        C.combine first_result.constraints
          (C.combine (combine_results rest_results)
             (List.fold_left (fun acc c -> C.add c acc) C.empty elem_constraints))
      in

      { ty = vector_of first_result.ty; constraints = all_constraints }
