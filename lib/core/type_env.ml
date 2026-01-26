(** Type environment for tracking variable bindings during type inference.

    The type environment maps variable names to their type schemes. It also
    tracks the current level for let-generalization. *)

open Types

(** A type scheme is a possibly-polymorphic type.

    [Mono ty] is a monomorphic type (no quantified variables). [Poly (vars, ty)]
    is a polymorphic type with bound type variables.

    Type schemes are created during let-generalization when the RHS is a
    syntactic value (lambda, literal, variable, constructor application). *)
type scheme = Mono of typ | Poly of string list * typ

type t = {
  bindings : (string * scheme) list;
  level : int;  (** Current scope level for generalization *)
}
(** Type environment: maps names to type schemes *)

(** Empty environment at level 0 *)
let empty = { bindings = []; level = 0 }

(** Create an environment with initial bindings *)
let of_list bindings = { bindings; level = 0 }

(** Get the current level *)
let current_level env = env.level

(** Enter a new scope (increment level) *)
let enter_level env = { env with level = env.level + 1 }

(** Exit a scope (decrement level) *)
let exit_level env = { env with level = max 0 (env.level - 1) }

(** Look up a name in the environment *)
let lookup name env = List.assoc_opt name env.bindings

(** Extend the environment with a new binding *)
let extend name scheme env =
  { env with bindings = (name, scheme) :: env.bindings }

(** Extend with a monomorphic binding *)
let extend_mono name ty env = extend name (Mono ty) env

(** Extend with multiple monomorphic bindings *)
let extend_monos bindings env =
  List.fold_left (fun env (name, ty) -> extend_mono name ty env) env bindings

(** Extend with a polymorphic binding *)
let extend_poly name vars ty env = extend name (Poly (vars, ty)) env

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
  | TApp (con, args) -> TApp (con, List.map (substitute subst) args)
  | TArrow (params, ret) ->
      TArrow (List.map (substitute_param subst) params, substitute subst ret)
  | TForall (vars, body) ->
      (* Don't substitute inside forall for shadowed variables *)
      let subst' = List.filter (fun (v, _) -> not (List.mem v vars)) subst in
      TForall (vars, substitute subst' body)
  | TUnion types -> TUnion (List.map (substitute subst) types)
  | TTuple types -> TTuple (List.map (substitute subst) types)

and substitute_param subst = function
  | PPositional ty -> PPositional (substitute subst ty)
  | POptional ty -> POptional (substitute subst ty)
  | PRest ty -> PRest (substitute subst ty)
  | PKey (name, ty) -> PKey (name, substitute subst ty)

(** Convert a scheme to string for debugging *)
let scheme_to_string = function
  | Mono ty -> to_string ty
  | Poly (vars, ty) ->
      Printf.sprintf "(forall (%s) %s)" (String.concat " " vars) (to_string ty)

(** Get all names bound in the environment *)
let names env = List.map fst env.bindings

(** Pretty-print the environment for debugging *)
let to_string env =
  let bindings_str =
    String.concat "\n"
      (List.map
         (fun (name, scheme) ->
           Printf.sprintf "  %s : %s" name (scheme_to_string scheme))
         env.bindings)
  in
  Printf.sprintf "TypeEnv (level=%d):\n%s" env.level bindings_str
