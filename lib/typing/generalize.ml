(** Levels-based let-generalization for Hindley-Milner type inference.

    This module implements the generalization step of HM inference using the
    levels-based approach. Type variables are generalized (become polymorphic)
    only if their level is greater than the current scope level.

    The key insight is that type variables with level > current_level were
    created inside the current let binding and are safe to generalize.

    Reference: "How OCaml type checker works" by Oleg Kiselyov *)

open Core.Types
module Env = Core.Type_env

(** Check if an expression is a syntactic value (eligible for generalization).

    The value restriction prevents unsound generalization of expressions that
    might have side effects. Only syntactic values can be generalized:
    - Lambda expressions
    - Literals (numbers, strings, etc.)
    - Variables
    - Quoted expressions

    Non-values (like function applications) remain monomorphic. *)
let rec is_syntactic_value (sexp : Syntax.Sexp.t) : bool =
  let open Syntax.Sexp in
  match sexp with
  (* Lambda is always a value *)
  | List (Symbol ("lambda", _) :: _, _) -> true
  (* Literals are values *)
  | Int _ | Float _ | String _ | Char _ | Keyword _ -> true
  (* Quoted expressions are values *)
  | List ([ Symbol ("quote", _); _ ], _) -> true
  (* Variables are values (they reference already-computed values) *)
  | Symbol (_, _) -> true
  (* Empty list (nil) is a value *)
  | List ([], _) -> true
  (* Vectors of values are values *)
  | Vector (elems, _) -> List.for_all is_syntactic_value elems
  (* Curly braces are type syntax, treat as non-value for safety *)
  | Curly _ -> false
  (* Function applications are NOT values - they might have effects *)
  | List (_ :: _, _) -> false
  (* Cons pairs and errors are not values *)
  | Cons _ | Error _ -> false

(** Collect all free type variables in a type at levels > given level.

    These are the variables that will be generalized. We collect them by
    traversing the type and finding unbound type variables whose level is
    strictly greater than the current scope level. *)
let rec collect_generalizable_tvars level ty acc =
  match repr ty with
  | TVar tv -> (
      match !tv with
      | Unbound (id, tvar_level) ->
          if tvar_level > level && not (List.mem id acc) then id :: acc else acc
      | Link _ -> failwith "repr should have followed link")
  | TCon _ -> acc
  | TApp (con, args) ->
      let acc = collect_generalizable_tvars level con acc in
      List.fold_left
        (fun acc ty -> collect_generalizable_tvars level ty acc)
        acc args
  | TArrow (params, ret) ->
      let acc =
        List.fold_left
          (fun acc param ->
            match param with
            | PPositional ty | POptional ty | PRest ty | PKey (_, ty) ->
                collect_generalizable_tvars level ty acc
            | PLiteral _ -> acc)
          acc params
      in
      collect_generalizable_tvars level ret acc
  | TForall (_, body) -> collect_generalizable_tvars level body acc
  | TUnion types | TTuple types ->
      List.fold_left
        (fun acc ty -> collect_generalizable_tvars level ty acc)
        acc types
  | TRow { row_fields; row_var } -> (
      let acc =
        List.fold_left
          (fun acc (_, ty) -> collect_generalizable_tvars level ty acc)
          acc row_fields
      in
      match row_var with
      | None -> acc
      | Some var -> collect_generalizable_tvars level var acc)

(** Generate fresh variable names for generalization.

    Converts numeric tvar IDs to alphabetic names: a, b, c, ..., z, a1, b1, ...
*)
let name_of_id id =
  let letter = Char.chr (Char.code 'a' + (id mod 26)) in
  let suffix = if id < 26 then "" else string_of_int (id / 26) in
  String.make 1 letter ^ suffix

(** Replace type variables with their named versions for forall binding.

    When we generalize, we replace TVar refs with TCon names that will be bound
    by the forall quantifier. *)
let rec replace_tvars_with_names var_map ty =
  match repr ty with
  | TVar tv -> (
      match !tv with
      | Unbound (id, _) -> (
          match List.assoc_opt id var_map with
          | Some name -> TCon name
          | None -> ty)
      | Link _ -> failwith "repr should have followed link")
  | TCon _ -> ty
  | TApp (con, args) ->
      TApp
        ( replace_tvars_with_names var_map con,
          List.map (replace_tvars_with_names var_map) args )
  | TArrow (params, ret) ->
      TArrow
        ( List.map (replace_tvar_in_param var_map) params,
          replace_tvars_with_names var_map ret )
  | TForall (vars, body) -> TForall (vars, replace_tvars_with_names var_map body)
  | TUnion types -> TUnion (List.map (replace_tvars_with_names var_map) types)
  | TTuple types -> TTuple (List.map (replace_tvars_with_names var_map) types)
  | TRow { row_fields; row_var } ->
      TRow
        {
          row_fields =
            List.map
              (fun (n, t) -> (n, replace_tvars_with_names var_map t))
              row_fields;
          row_var = Option.map (replace_tvars_with_names var_map) row_var;
        }

and replace_tvar_in_param var_map = function
  | PPositional ty -> PPositional (replace_tvars_with_names var_map ty)
  | POptional ty -> POptional (replace_tvars_with_names var_map ty)
  | PRest ty -> PRest (replace_tvars_with_names var_map ty)
  | PKey (name, ty) -> PKey (name, replace_tvars_with_names var_map ty)
  | PLiteral _ as p -> p

(** Generalize a type at the given level.

    Finds all type variables with level > given level, and wraps the type in a
    forall quantifier binding those variables.

    Returns a Mono scheme if no variables can be generalized, or a Poly scheme
    with the generalized type. *)
let generalize level ty : Env.scheme =
  let tvar_ids = collect_generalizable_tvars level ty [] in
  match tvar_ids with
  | [] ->
      (* No variables to generalize - return monomorphic *)
      Env.Mono ty
  | ids ->
      (* Create names for each generalizable variable *)
      let var_map = List.mapi (fun i id -> (id, name_of_id i)) ids in
      let names = List.map snd var_map in
      (* Replace tvars with named type constants *)
      let body = replace_tvars_with_names var_map ty in
      Env.Poly (names, body)

(** Generalize a type only if the expression is a syntactic value.

    This implements the value restriction: non-values remain monomorphic to
    ensure soundness in the presence of mutable state. *)
let generalize_if_value level ty (expr : Syntax.Sexp.t) : Env.scheme =
  if is_syntactic_value expr then generalize level ty else Env.Mono ty
