(** Type environment for tracking variable bindings during type inference.

    The type environment maps variable names to their type schemes. It also
    tracks the current level for let-generalization.

    Additionally tracks predicate information for type narrowing (Spec 52). When
    a function is declared with a predicate return type like [(x is string)],
    the predicate info is stored so that conditionals can narrow variable types.
*)

open Types

type diagnostic_severity = DiagError | DiagWarn | DiagNote

type loaded_diagnostic = {
  ld_severity : diagnostic_severity;
  ld_message : string;
  ld_args : string list;
}

type loaded_clause = {
  lc_params : param list;  (** Parameter types for this clause *)
  lc_return : typ;  (** Return type for this clause *)
  lc_diagnostic : loaded_diagnostic option;
}
(** A single clause from a multi-clause defun, preserved through loading. *)

type predicate_info = {
  param_index : int;  (** Index of the parameter being narrowed (0-based) *)
  param_name : string;  (** Name of the parameter (for validation) *)
  narrowed_type : typ;  (** Type the parameter narrows to when true *)
}
(** Information about a type predicate function.

    When a function is declared as a predicate, it narrows the type of a
    specific parameter when called in a conditional. For example:
    {[
      (defun stringp (x) -> (x is string))
    ]}
    Declares [stringp] as narrowing parameter [x] to [string] when true. *)

(** A type scheme is a possibly-polymorphic type.

    [Mono ty] is a monomorphic type (no quantified variables). [Poly (vars, ty)]
    is a polymorphic type with bound type variables.

    Type schemes are created during let-generalization when the RHS is a
    syntactic value (lambda, literal, variable, constructor application). *)
type scheme = Mono of typ | Poly of string list * typ

type t = {
  bindings : (string * scheme) list;
      (** Variable namespace: let, setq, defvar, lambda params *)
  fn_bindings : (string * scheme) list;
      (** Function namespace: defun, defalias, flet *)
  fn_clauses : (string * loaded_clause list) list;
      (** Multi-clause function signatures for overload resolution (Spec 56) *)
  predicates : (string * predicate_info) list;
      (** Type predicates: maps function names to their predicate info *)
  feature_loader : (string -> t -> t) option;
      (** Optional callback to load a feature's signatures into the env (Spec
          49). Called with the feature name and current env; returns extended
          env. Set by module_check.ml before type checking begins. *)
  level : int;  (** Current scope level for generalization *)
}
(** Type environment with dual namespaces for Elisp's Lisp-2 semantics *)

(** Empty environment at level 0 *)
let empty =
  {
    bindings = [];
    fn_bindings = [];
    fn_clauses = [];
    predicates = [];
    feature_loader = None;
    level = 0;
  }

(** Create an environment with initial bindings (in variable namespace) *)
let of_list bindings =
  {
    bindings;
    fn_bindings = [];
    fn_clauses = [];
    predicates = [];
    feature_loader = None;
    level = 0;
  }

(** Get the current level *)
let current_level env = env.level

(** Enter a new scope (increment level) *)
let enter_level env = { env with level = env.level + 1 }

(** Exit a scope (decrement level) *)
let exit_level env = { env with level = max 0 (env.level - 1) }

(** Look up a name in the variable namespace.

    Falls back to function namespace if not found in variable namespace. This
    provides backward compatibility while supporting Lisp-2 semantics. *)
let lookup name env =
  match List.assoc_opt name env.bindings with
  | Some _ as result -> result
  | None -> List.assoc_opt name env.fn_bindings

(** Look up a name in the function namespace only *)
let lookup_fn name env = List.assoc_opt name env.fn_bindings

(** Look up a name in the variable namespace only (no fallback to functions).

    Use this for setq/assignment checking where we only want to check if a
    variable binding already exists, not if a function with that name exists. *)
let lookup_var name env = List.assoc_opt name env.bindings

(** Extend the variable namespace with a new binding *)
let extend name scheme env =
  { env with bindings = (name, scheme) :: env.bindings }

(** Extend variable namespace with a monomorphic binding *)
let extend_mono name ty env = extend name (Mono ty) env

(** Extend variable namespace with multiple monomorphic bindings *)
let extend_monos bindings env =
  List.fold_left (fun env (name, ty) -> extend_mono name ty env) env bindings

(** Extend variable namespace with a polymorphic binding *)
let extend_poly name vars ty env = extend name (Poly (vars, ty)) env

(** Extend the function namespace with a new binding *)
let extend_fn name scheme env =
  { env with fn_bindings = (name, scheme) :: env.fn_bindings }

(** Extend function namespace with a monomorphic binding *)
let extend_fn_mono name ty env = extend_fn name (Mono ty) env

(** Extend function namespace with a polymorphic binding *)
let extend_fn_poly name vars ty env = extend_fn name (Poly (vars, ty)) env

(** Register the preserved clause list for a multi-clause defun *)
let extend_fn_clauses name clauses env =
  { env with fn_clauses = (name, clauses) :: env.fn_clauses }

(** Look up the preserved clause list for a function *)
let lookup_fn_clauses name env = List.assoc_opt name env.fn_clauses

(** Look up predicate info for a function name *)
let lookup_predicate name env = List.assoc_opt name env.predicates

(** Register a function as a type predicate *)
let extend_predicate name info env =
  { env with predicates = (name, info) :: env.predicates }

(** Override a variable's type in the variable namespace for narrowing.

    Prepends a new monomorphic binding for [name] with [ty], shadowing any
    existing binding. Used by predicate narrowing to refine a variable's type in
    then/else branches. *)
let with_narrowed_var name ty env = extend_mono name ty env

(** Instantiate a type scheme at the current level.

    For monomorphic types, returns the type as-is. For polymorphic types,
    replaces bound variables with fresh type variables. *)
let rec instantiate scheme env =
  match scheme with
  | Mono ty -> ty
  | Poly (vars, ty) ->
      (* Create fresh type variables for each bound variable *)
      let subst = List.map (fun v -> (v, fresh_tvar env.level)) vars in
      (* Apply substitution to the body *)
      substitute subst ty

(** Substitute type variables in a type.

    This replaces [TCon v] where [v] is in the substitution with the
    corresponding fresh type variable.

    Note: In our representation, bound variables in forall are represented as
    strings that appear as TCon in the body. This function replaces them. *)
and substitute subst ty =
  match ty with
  | TVar tv -> (
      match !tv with Link ty' -> substitute subst ty' | Unbound _ -> ty)
  | TCon name -> (
      match List.assoc_opt name subst with Some ty' -> ty' | None -> ty)
  | TApp (con, args) ->
      TApp (substitute subst con, List.map (substitute subst) args)
  | TArrow (params, ret) ->
      TArrow (List.map (substitute_param subst) params, substitute subst ret)
  | TForall (vars, body) ->
      (* Don't substitute inside forall for shadowed variables *)
      let subst' = List.filter (fun (v, _) -> not (List.mem v vars)) subst in
      TForall (vars, substitute subst' body)
  | TUnion types -> TUnion (List.map (substitute subst) types)
  | TTuple types -> TTuple (List.map (substitute subst) types)
  | TRow { row_fields; row_var } ->
      TRow
        {
          row_fields =
            List.map (fun (n, t) -> (n, substitute subst t)) row_fields;
          row_var = Option.map (substitute subst) row_var;
        }
  | TLiteral _ -> ty

and substitute_param subst = function
  | PPositional ty -> PPositional (substitute subst ty)
  | POptional ty -> POptional (substitute subst ty)
  | PRest ty -> PRest (substitute subst ty)
  | PKey (name, ty) -> PKey (name, substitute subst ty)
  | PLiteral _ as p -> p

(** Convert a scheme to string for debugging *)
let scheme_to_string = function
  | Mono ty -> to_string ty
  | Poly (vars, ty) ->
      Printf.sprintf "(forall (%s) %s)" (String.concat " " vars) (to_string ty)

(** Set the feature loader callback (Spec 49).

    The loader is called with a feature name and the current env, and returns
    the env extended with the feature's signatures. *)
let set_feature_loader loader env = { env with feature_loader = Some loader }

(** Load a feature's signatures into the env using the feature loader.

    Returns the extended env if the loader is set and the feature is found, or
    the original env if no loader is set or the feature is not found. *)
let load_feature name env =
  match env.feature_loader with Some loader -> loader name env | None -> env

(** Get all names bound in the variable namespace *)
let names env = List.map fst env.bindings

(** Get all names bound in the function namespace *)
let fn_names env = List.map fst env.fn_bindings

(** Pretty-print the environment for debugging *)
let to_string env =
  let var_str =
    String.concat "\n"
      (List.map
         (fun (name, scheme) ->
           Printf.sprintf "  %s : %s" name (scheme_to_string scheme))
         env.bindings)
  in
  let fn_str =
    String.concat "\n"
      (List.map
         (fun (name, scheme) ->
           Printf.sprintf "  %s : %s" name (scheme_to_string scheme))
         env.fn_bindings)
  in
  Printf.sprintf "TypeEnv (level=%d):\n  Variables:\n%s\n  Functions:\n%s"
    env.level var_str fn_str
