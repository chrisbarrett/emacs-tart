(** Unification with union-find for type inference.

    This module solves equality constraints by unifying types using the
    union-find data structure. Type variables are mutable refs that can either
    be Unbound or Link to another type.

    The occurs check prevents infinite types like `a = List a`. *)

open Core.Types
module C = Constraint

(** Unification errors *)
type error =
  | TypeMismatch of typ * typ * Syntax.Location.span
      (** Two concrete types that cannot unify *)
  | OccursCheck of tvar_id * typ * Syntax.Location.span
      (** Type variable occurs in the type it's being unified with *)
  | ArityMismatch of int * int * Syntax.Location.span
      (** Function arities don't match: expected, actual *)

(** Format an error for display *)
let error_to_string = function
  | TypeMismatch (t1, t2, _) ->
      Printf.sprintf "Type mismatch: cannot unify %s with %s" (to_string t1)
        (to_string t2)
  | OccursCheck (id, ty, _) ->
      Printf.sprintf "Occurs check: type variable '_%d occurs in %s" id
        (to_string ty)
  | ArityMismatch (expected, actual, _) ->
      Printf.sprintf "Arity mismatch: expected %d arguments but got %d" expected
        actual

(** Get the location from an error *)
let error_location = function
  | TypeMismatch (_, _, loc) -> loc
  | OccursCheck (_, _, loc) -> loc
  | ArityMismatch (_, _, loc) -> loc

type 'a result = ('a, error) Result.t
(** Result type for unification *)

(** Bind operator for Result monad *)
let ( let* ) = Result.bind

(** Occurs check: ensure a type variable does not occur in a type.

    This prevents infinite types like `a = List a`. Also updates levels for
    let-generalization: if we find a type variable with a higher level, we lower
    it to the current level. *)
let rec occurs_check tv_id tv_level ty loc : unit result =
  match repr ty with
  | TVar tv -> (
      match !tv with
      | Unbound (id, level) ->
          if id = tv_id then Error (OccursCheck (tv_id, ty, loc))
          else (
            (* Level adjustment: if the type variable is at a higher level,
               lower it to the current level. This is essential for
               let-generalization. *)
            if level > tv_level then tv := Unbound (id, tv_level);
            Ok ())
      | Link _ -> failwith "repr should have followed link")
  | TCon _ -> Ok ()
  | TApp (_, args) -> occurs_check_list tv_id tv_level args loc
  | TArrow (params, ret) ->
      let* () = occurs_check_params tv_id tv_level params loc in
      occurs_check tv_id tv_level ret loc
  | TForall (_, body) -> occurs_check tv_id tv_level body loc
  | TUnion types -> occurs_check_list tv_id tv_level types loc
  | TTuple types -> occurs_check_list tv_id tv_level types loc

and occurs_check_list tv_id tv_level types loc =
  List.fold_left
    (fun acc ty ->
      let* () = acc in
      occurs_check tv_id tv_level ty loc)
    (Ok ()) types

and occurs_check_params tv_id tv_level params loc =
  List.fold_left
    (fun acc param ->
      let* () = acc in
      let ty =
        match param with
        | PPositional ty | POptional ty | PRest ty | PKey (_, ty) -> ty
      in
      occurs_check tv_id tv_level ty loc)
    (Ok ()) params

(** Unify two types.

    This is the core unification algorithm. It follows links and handles each
    type constructor case. *)
let rec unify t1 t2 loc : unit result =
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then
    (* Physical equality - already unified *)
    Ok ()
  else
    match (t1, t2) with
    (* Type variable cases - link the variable to the other type *)
    | TVar tv, ty | ty, TVar tv -> (
        match !tv with
        | Link _ -> failwith "repr should have followed link"
        | Unbound (id, level) ->
            (* Occurs check before linking *)
            let* () = occurs_check id level ty loc in
            tv := Link ty;
            Ok ())
    (* Any is the top type - unifies with anything *)
    | TCon "Any", _ | _, TCon "Any" -> Ok ()
    (* Type constants must match exactly *)
    | TCon n1, TCon n2 ->
        if n1 = n2 then Ok () else Error (TypeMismatch (t1, t2, loc))
    (* Type applications: constructor and args must match *)
    | TApp (c1, args1), TApp (c2, args2) ->
        if c1 <> c2 then Error (TypeMismatch (t1, t2, loc))
        else if List.length args1 <> List.length args2 then
          Error (ArityMismatch (List.length args1, List.length args2, loc))
        else unify_list args1 args2 loc
    (* Function types: params and return must match.
       Handle rest/optional parameters specially. *)
    | TArrow (ps1, r1), TArrow (ps2, r2) ->
        let* () = unify_param_lists ps1 ps2 loc in
        unify r1 r2 loc
    (* Forall types: for now, require same structure.
       Full higher-rank polymorphism would need more sophisticated handling. *)
    | TForall (vs1, b1), TForall (vs2, b2) ->
        if List.length vs1 <> List.length vs2 then
          Error (TypeMismatch (t1, t2, loc))
        else
          (* Unify bodies directly - this is simplified.
             A full implementation would alpha-rename. *)
          unify b1 b2 loc
    (* Union types: for now, require structural equality.
       Full union handling would need subtyping. *)
    | TUnion ts1, TUnion ts2 ->
        if List.length ts1 <> List.length ts2 then
          Error (TypeMismatch (t1, t2, loc))
        else unify_list ts1 ts2 loc
    (* Tuple types: element-wise unification *)
    | TTuple ts1, TTuple ts2 ->
        if List.length ts1 <> List.length ts2 then
          Error (ArityMismatch (List.length ts1, List.length ts2, loc))
        else unify_list ts1 ts2 loc
    (* All other combinations are type mismatches *)
    | _ -> Error (TypeMismatch (t1, t2, loc))

and unify_list ts1 ts2 loc =
  List.fold_left2
    (fun acc t1 t2 ->
      let* () = acc in
      unify t1 t2 loc)
    (Ok ()) ts1 ts2

(** Check if a param is a rest param *)
and is_rest_param = function PRest _ -> true | _ -> false

(** Unify two parameter lists.

    Handles the complexity of rest arguments and optional parameters. The
    strategy is: 1. If both lists have the same length, unify element-wise 2. If
    one has rest args, consume extra args from the other side 3. Otherwise, it's
    an arity mismatch *)
and unify_param_lists ps1 ps2 loc =
  match (ps1, ps2) with
  (* Both empty - success *)
  | [], [] -> Ok ()
  (* Left side has rest param at end - consume all remaining from right *)
  | [ PRest elem_ty1 ], params2 ->
      (* Unify each remaining param's type with the rest element type *)
      List.fold_left
        (fun acc p2 ->
          let* () = acc in
          match p2 with
          | PPositional t2 | POptional t2 | PKey (_, t2) ->
              unify elem_ty1 t2 loc
          | PRest t2 -> unify elem_ty1 t2 loc)
        (Ok ()) params2
  (* Right side has rest param at end - consume all remaining from left *)
  | params1, [ PRest elem_ty2 ] ->
      List.fold_left
        (fun acc p1 ->
          let* () = acc in
          match p1 with
          | PPositional t1 | POptional t1 | PKey (_, t1) ->
              unify t1 elem_ty2 loc
          | PRest t1 -> unify t1 elem_ty2 loc)
        (Ok ()) params1
  (* Non-rest params on both sides - unify element-wise *)
  | p1 :: rest1, p2 :: rest2 ->
      if is_rest_param p1 || is_rest_param p2 then
        (* Rest param not at the end - handle case by case *)
        match (p1, p2) with
        | PRest elem_ty1, _ ->
            (* Left is rest, right is not - unify right with rest elem, continue *)
            let t2 =
              match p2 with
              | PPositional t | POptional t | PKey (_, t) | PRest t -> t
            in
            let* () = unify elem_ty1 t2 loc in
            unify_param_lists ps1 rest2 loc (* Keep consuming with same rest *)
        | _, PRest elem_ty2 ->
            (* Right is rest, left is not *)
            let t1 =
              match p1 with
              | PPositional t | POptional t | PKey (_, t) | PRest t -> t
            in
            let* () = unify t1 elem_ty2 loc in
            unify_param_lists rest1 ps2 loc (* Keep consuming with same rest *)
        | _, _ ->
            (* Neither is rest - unreachable given the guard, but needed for exhaustiveness *)
            let* () = unify_param p1 p2 loc in
            unify_param_lists rest1 rest2 loc
      else
        (* Neither is rest param - normal element-wise unification *)
        let* () = unify_param p1 p2 loc in
        unify_param_lists rest1 rest2 loc
  (* Left exhausted but right has more (and no rest on left) *)
  | [], _ :: _ -> Error (ArityMismatch (0, List.length ps2, loc))
  (* Right exhausted but left has more (and no rest on right) *)
  | _ :: _, [] -> Error (ArityMismatch (List.length ps1, 0, loc))

and unify_param p1 p2 loc =
  match (p1, p2) with
  | PPositional t1, PPositional t2 -> unify t1 t2 loc
  | POptional t1, POptional t2 -> unify t1 t2 loc
  | PRest t1, PRest t2 -> unify t1 t2 loc
  | PKey (n1, t1), PKey (n2, t2) ->
      if n1 = n2 then unify t1 t2 loc
      else
        Error
          (TypeMismatch
             (TArrow ([ p1 ], Prim.any), TArrow ([ p2 ], Prim.any), loc))
  | _ ->
      (* Different parameter kinds don't unify *)
      Error
        (TypeMismatch (TArrow ([ p1 ], Prim.any), TArrow ([ p2 ], Prim.any), loc))

(** Solve a set of constraints.

    Returns Ok () if all constraints can be satisfied, or the first error. *)
let solve (constraints : C.set) : unit result =
  List.fold_left
    (fun acc (c : C.t) ->
      let* () = acc in
      unify c.lhs c.rhs c.loc)
    (Ok ()) constraints

(** Solve constraints and return all errors (not just the first).

    Useful for reporting multiple type errors at once. *)
let solve_all (constraints : C.set) : error list =
  List.filter_map
    (fun (c : C.t) ->
      match unify c.lhs c.rhs c.loc with Ok () -> None | Error e -> Some e)
    constraints
