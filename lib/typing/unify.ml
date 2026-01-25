(** Unification with union-find for type inference.

    This module solves equality constraints by unifying types using
    the union-find data structure. Type variables are mutable refs
    that can either be Unbound or Link to another type.

    The occurs check prevents infinite types like `a = List a`.
*)

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
      Printf.sprintf "Type mismatch: cannot unify %s with %s"
        (to_string t1) (to_string t2)
  | OccursCheck (id, ty, _) ->
      Printf.sprintf "Occurs check: type variable '_%d occurs in %s"
        id (to_string ty)
  | ArityMismatch (expected, actual, _) ->
      Printf.sprintf "Arity mismatch: expected %d arguments but got %d"
        expected actual

(** Get the location from an error *)
let error_location = function
  | TypeMismatch (_, _, loc) -> loc
  | OccursCheck (_, _, loc) -> loc
  | ArityMismatch (_, _, loc) -> loc

(** Result type for unification *)
type 'a result = ('a, error) Result.t

(** Bind operator for Result monad *)
let ( let* ) = Result.bind

(** Occurs check: ensure a type variable does not occur in a type.

    This prevents infinite types like `a = List a`.
    Also updates levels for let-generalization: if we find a type variable
    with a higher level, we lower it to the current level.
*)
let rec occurs_check tv_id tv_level ty loc : unit result =
  match repr ty with
  | TVar tv -> (
      match !tv with
      | Unbound (id, level) ->
          if id = tv_id then
            Error (OccursCheck (tv_id, ty, loc))
          else begin
            (* Level adjustment: if the type variable is at a higher level,
               lower it to the current level. This is essential for
               let-generalization. *)
            if level > tv_level then
              tv := Unbound (id, tv_level);
            Ok ()
          end
      | Link _ -> failwith "repr should have followed link")
  | TCon _ -> Ok ()
  | TApp (_, args) ->
      occurs_check_list tv_id tv_level args loc
  | TArrow (params, ret) ->
      let* () = occurs_check_params tv_id tv_level params loc in
      occurs_check tv_id tv_level ret loc
  | TForall (_, body) ->
      occurs_check tv_id tv_level body loc
  | TUnion types ->
      occurs_check_list tv_id tv_level types loc
  | TTuple types ->
      occurs_check_list tv_id tv_level types loc

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
       let ty = match param with
         | PPositional ty | POptional ty | PRest ty | PKey (_, ty) -> ty
       in
       occurs_check tv_id tv_level ty loc)
    (Ok ()) params

(** Unify two types.

    This is the core unification algorithm. It follows links and
    handles each type constructor case.
*)
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

    (* Type constants must match exactly *)
    | TCon n1, TCon n2 ->
        if n1 = n2 then Ok ()
        else Error (TypeMismatch (t1, t2, loc))

    (* Type applications: constructor and args must match *)
    | TApp (c1, args1), TApp (c2, args2) ->
        if c1 <> c2 then
          Error (TypeMismatch (t1, t2, loc))
        else if List.length args1 <> List.length args2 then
          Error (ArityMismatch (List.length args1, List.length args2, loc))
        else
          unify_list args1 args2 loc

    (* Function types: params and return must match *)
    | TArrow (ps1, r1), TArrow (ps2, r2) ->
        if List.length ps1 <> List.length ps2 then
          Error (ArityMismatch (List.length ps1, List.length ps2, loc))
        else
          let* () = unify_params ps1 ps2 loc in
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
        else
          unify_list ts1 ts2 loc

    (* Tuple types: element-wise unification *)
    | TTuple ts1, TTuple ts2 ->
        if List.length ts1 <> List.length ts2 then
          Error (ArityMismatch (List.length ts1, List.length ts2, loc))
        else
          unify_list ts1 ts2 loc

    (* All other combinations are type mismatches *)
    | _ -> Error (TypeMismatch (t1, t2, loc))

and unify_list ts1 ts2 loc =
  List.fold_left2
    (fun acc t1 t2 ->
       let* () = acc in
       unify t1 t2 loc)
    (Ok ()) ts1 ts2

and unify_params ps1 ps2 loc =
  List.fold_left2
    (fun acc p1 p2 ->
       let* () = acc in
       unify_param p1 p2 loc)
    (Ok ()) ps1 ps2

and unify_param p1 p2 loc =
  match (p1, p2) with
  | PPositional t1, PPositional t2 -> unify t1 t2 loc
  | POptional t1, POptional t2 -> unify t1 t2 loc
  | PRest t1, PRest t2 -> unify t1 t2 loc
  | PKey (n1, t1), PKey (n2, t2) ->
      if n1 = n2 then unify t1 t2 loc
      else Error (TypeMismatch (TArrow ([p1], Prim.any), TArrow ([p2], Prim.any), loc))
  | _ ->
      (* Different parameter kinds don't unify *)
      Error (TypeMismatch (TArrow ([p1], Prim.any), TArrow ([p2], Prim.any), loc))

(** Solve a set of constraints.

    Returns Ok () if all constraints can be satisfied, or the first error.
*)
let solve (constraints : C.set) : unit result =
  List.fold_left
    (fun acc (c : C.t) ->
       let* () = acc in
       unify c.lhs c.rhs c.loc)
    (Ok ()) constraints

(** Solve constraints and return all errors (not just the first).

    Useful for reporting multiple type errors at once.
*)
let solve_all (constraints : C.set) : error list =
  List.filter_map
    (fun (c : C.t) ->
       match unify c.lhs c.rhs c.loc with
       | Ok () -> None
       | Error e -> Some e)
    constraints
