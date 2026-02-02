(** Unification with union-find for type inference.

    This module solves equality constraints by unifying types using the
    union-find data structure. Type variables are mutable refs that can either
    be Unbound or Link to another type.

    The occurs check prevents infinite types like `a = List a`. *)

open Core.Types
module C = Constraint

(** Unification errors *)
type error =
  | TypeMismatch of typ * typ * Syntax.Location.span * C.context
      (** Two concrete types that cannot unify, with optional context *)
  | OccursCheck of tvar_id * typ * Syntax.Location.span
      (** Type variable occurs in the type it's being unified with *)
  | ArityMismatch of int * int * Syntax.Location.span * C.context
      (** Function arities don't match: expected, actual, with optional context
      *)

(** Format an error for display *)
let error_to_string = function
  | TypeMismatch (t1, t2, _, _) ->
      Printf.sprintf "Type mismatch: cannot unify %s with %s" (to_string t1)
        (to_string t2)
  | OccursCheck (id, ty, _) ->
      Printf.sprintf "Occurs check: type variable '_%d occurs in %s" id
        (to_string ty)
  | ArityMismatch (expected, actual, _, _) ->
      Printf.sprintf "Arity mismatch: expected %d arguments but got %d" expected
        actual

(** Get the location from an error *)
let error_location = function
  | TypeMismatch (_, _, loc, _) -> loc
  | OccursCheck (_, _, loc) -> loc
  | ArityMismatch (_, _, loc, _) -> loc

(** Get the context from an error *)
let error_context = function
  | TypeMismatch (_, _, _, ctx) -> ctx
  | OccursCheck _ -> C.NoContext
  | ArityMismatch (_, _, _, ctx) -> ctx

type 'a result = ('a, error) Result.t
(** Result type for unification *)

(** Bind operator for Result monad *)
let ( let* ) = Result.bind

type occurs_error = tvar_id * typ * Syntax.Location.span
(** Internal occurs check error (without context) *)

(** Occurs check: ensure a type variable does not occur in a type.

    This prevents infinite types like `a = List a`. Also updates levels for
    let-generalization: if we find a type variable with a higher level, we lower
    it to the current level. *)
let rec occurs_check tv_id tv_level ty loc : (unit, occurs_error) Result.t =
  match repr ty with
  | TVar tv -> (
      match !tv with
      | Unbound (id, level) ->
          if id = tv_id then Error (tv_id, ty, loc)
          else (
            (* Level adjustment: if the type variable is at a higher level,
               lower it to the current level. This is essential for
               let-generalization. *)
            if level > tv_level then tv := Unbound (id, tv_level);
            Ok ())
      | Link _ -> failwith "repr should have followed link")
  | TCon _ -> Ok ()
  | TApp (con, args) ->
      let* () = occurs_check tv_id tv_level con loc in
      occurs_check_list tv_id tv_level args loc
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

(** Internal error type without context (used during unification) *)
type internal_error =
  | ITypeMismatch of typ * typ * Syntax.Location.span
  | IOccursCheck of tvar_id * typ * Syntax.Location.span
  | IArityMismatch of int * int * Syntax.Location.span

type 'a internal_result = ('a, internal_error) Result.t

(** Unify two types.

    This is the core unification algorithm. It follows links and handles each
    type constructor case.

    The [invariant] parameter controls whether Any acts as a wildcard. When true
    (inside type application arguments), Any must match exactly - this enforces
    invariance for parameterized types like [(list int)] not unifying with
    [(list any)]. *)
let rec unify ?(invariant = false) t1 t2 loc : unit internal_result =
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
        | Unbound (id, level) -> (
            (* Occurs check before linking *)
            match occurs_check id level ty loc with
            | Error (tv_id, ty, loc) -> Error (IOccursCheck (tv_id, ty, loc))
            | Ok () ->
                tv := Link ty;
                Ok ()))
    (* Any (truthy | nil) is the top type - unifies with anything, EXCEPT in
       invariant contexts (inside type application arguments). This enforces
       invariance for parameterized types: (list int) does NOT unify with
       (list any). *)
    | _ when is_any t1 || is_any t2 ->
        if invariant then
          (* In invariant context, Any only unifies with Any *)
          if is_any t1 && is_any t2 then Ok ()
          else Error (ITypeMismatch (t1, t2, loc))
        else Ok ()
    (* Type constants: handle numeric subtyping.
       Int <: Num and Float <: Num (Int/Float are subtypes of Num).
       This allows integer literals to be passed where Num is expected. *)
    | TCon n1, TCon n2 ->
        if n1 = n2 then Ok ()
        else if n1 = "Num" && (n2 = "Int" || n2 = "Float") then Ok ()
        else if n2 = "Num" && (n1 = "Int" || n1 = "Float") then Ok ()
        else Error (ITypeMismatch (t1, t2, loc))
    (* Type applications: constructor and args must match.
       For HK types, the constructor may be a type variable that unifies
       with a concrete constructor. Arguments are unified with invariant=true
       to enforce invariance. *)
    | TApp (c1, args1), TApp (c2, args2) ->
        if List.length args1 <> List.length args2 then
          Error (IArityMismatch (List.length args1, List.length args2, loc))
        else
          (* Unify constructors first (enables HK instantiation) *)
          let* () = unify ~invariant c1 c2 loc in
          unify_list ~invariant:true args1 args2 loc
    (* Function types: params and return must match.
       Handle rest/optional parameters specially. *)
    | TArrow (ps1, r1), TArrow (ps2, r2) ->
        let* () = unify_param_lists ~invariant ps1 ps2 loc in
        unify ~invariant r1 r2 loc
    (* Forall types: for now, require same structure.
       Full higher-rank polymorphism would need more sophisticated handling. *)
    | TForall (vs1, b1), TForall (vs2, b2) ->
        if List.length vs1 <> List.length vs2 then
          Error (ITypeMismatch (t1, t2, loc))
        else
          (* Unify bodies directly - this is simplified.
             A full implementation would alpha-rename. *)
          unify ~invariant b1 b2 loc
    (* Union types: structural equality for unions on both sides *)
    | TUnion ts1, TUnion ts2 ->
        if List.length ts1 = List.length ts2 then
          unify_list ~invariant ts1 ts2 loc
        else Error (ITypeMismatch (t1, t2, loc))
    (* Union on left (from signature), non-union on right (from arg):
       This is the "safe" direction - passing a more specific type where
       a union is expected. Succeeds if the arg type matches any member.

       Example: message expects (string | nil), we pass string → OK
       because string ⊆ (string | nil) *)
    | TUnion ts, t when not (is_union t) ->
        if List.exists (fun ti -> Result.is_ok (unify ~invariant ti t loc)) ts
        then Ok ()
        else Error (ITypeMismatch (t1, t2, loc))
    (* Non-union on left (from signature), union on right (from arg):
       This is UNSOUND - passing a potentially-nil value where non-nil
       is expected. We do NOT allow this. Falls through to catch-all.

       Example: upcase expects string, we pass (string | nil) → ERROR
       because nil might be passed at runtime *)
    (* Tuple types: element-wise unification *)
    | TTuple ts1, TTuple ts2 ->
        if List.length ts1 <> List.length ts2 then
          Error (IArityMismatch (List.length ts1, List.length ts2, loc))
        else unify_list ~invariant ts1 ts2 loc
    (* All other combinations are type mismatches *)
    | _ -> Error (ITypeMismatch (t1, t2, loc))

and unify_list ?(invariant = false) ts1 ts2 loc =
  List.fold_left2
    (fun acc t1 t2 ->
      let* () = acc in
      unify ~invariant t1 t2 loc)
    (Ok ()) ts1 ts2

(** Check if a param is a rest param *)
and is_rest_param = function PRest _ -> true | _ -> false

(** Unify two parameter lists.

    Handles the complexity of rest arguments and optional parameters. The
    strategy is: 1. If both lists have the same length, unify element-wise 2. If
    one has rest args, consume extra args from the other side 3. Otherwise, it's
    an arity mismatch *)
and unify_param_lists ?(invariant = false) ps1 ps2 loc =
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
              unify ~invariant elem_ty1 t2 loc
          | PRest t2 -> unify ~invariant elem_ty1 t2 loc)
        (Ok ()) params2
  (* Right side has rest param at end - consume all remaining from left *)
  | params1, [ PRest elem_ty2 ] ->
      List.fold_left
        (fun acc p1 ->
          let* () = acc in
          match p1 with
          | PPositional t1 | POptional t1 | PKey (_, t1) ->
              unify ~invariant t1 elem_ty2 loc
          | PRest t1 -> unify ~invariant t1 elem_ty2 loc)
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
            let* () = unify ~invariant elem_ty1 t2 loc in
            unify_param_lists ~invariant ps1 rest2 loc
            (* Keep consuming with same rest *)
        | _, PRest elem_ty2 ->
            (* Right is rest, left is not *)
            let t1 =
              match p1 with
              | PPositional t | POptional t | PKey (_, t) | PRest t -> t
            in
            let* () = unify ~invariant t1 elem_ty2 loc in
            unify_param_lists ~invariant rest1 ps2 loc
            (* Keep consuming with same rest *)
        | _, _ ->
            (* Neither is rest - unreachable given the guard, but needed for exhaustiveness *)
            let* () = unify_param ~invariant p1 p2 loc in
            unify_param_lists ~invariant rest1 rest2 loc
      else
        (* Neither is rest param - normal element-wise unification *)
        let* () = unify_param ~invariant p1 p2 loc in
        unify_param_lists ~invariant rest1 rest2 loc
  (* Left exhausted but right has more (and no rest on left) *)
  | [], remaining ->
      (* Check if all remaining params are optional or rest - if so, OK *)
      let all_optional =
        List.for_all
          (function POptional _ | PRest _ -> true | _ -> false)
          remaining
      in
      if all_optional then Ok ()
      else
        let required_count =
          List.length
            (List.filter
               (function PPositional _ | PKey _ -> true | _ -> false)
               remaining)
        in
        Error (IArityMismatch (0, required_count, loc))
  (* Right exhausted but left has more (and no rest on right) *)
  | remaining, [] ->
      (* Check if all remaining params on left are optional or rest - if so, OK
         (caller provided optional args that callee doesn't need) *)
      let all_optional =
        List.for_all
          (function POptional _ | PRest _ -> true | _ -> false)
          remaining
      in
      if all_optional then Ok ()
      else
        let required_count =
          List.length
            (List.filter
               (function PPositional _ | PKey _ -> true | _ -> false)
               remaining)
        in
        Error (IArityMismatch (required_count, 0, loc))

and unify_param ?(invariant = false) p1 p2 loc =
  match (p1, p2) with
  | PPositional t1, PPositional t2 -> unify ~invariant t1 t2 loc
  | POptional t1, POptional t2 -> unify ~invariant t1 t2 loc
  | PRest t1, PRest t2 -> unify ~invariant t1 t2 loc
  | PKey (n1, t1), PKey (n2, t2) ->
      if n1 = n2 then unify ~invariant t1 t2 loc
      else
        Error
          (ITypeMismatch
             (TArrow ([ p1 ], Prim.any), TArrow ([ p2 ], Prim.any), loc))
  | _ ->
      (* Different parameter kinds don't unify *)
      Error
        (ITypeMismatch
           (TArrow ([ p1 ], Prim.any), TArrow ([ p2 ], Prim.any), loc))

(** Convert an internal error to an external error with context *)
let to_external_error (ctx : C.context) : internal_error -> error = function
  | ITypeMismatch (t1, t2, loc) -> TypeMismatch (t1, t2, loc, ctx)
  | IOccursCheck (id, ty, loc) -> OccursCheck (id, ty, loc)
  | IArityMismatch (exp, act, loc) -> ArityMismatch (exp, act, loc, ctx)

(** Solve a set of constraints.

    Returns Ok () if all constraints can be satisfied, or the first error. *)
let solve (constraints : C.set) : unit result =
  List.fold_left
    (fun acc (c : C.t) ->
      let* () = acc in
      match unify c.lhs c.rhs c.loc with
      | Ok () -> Ok ()
      | Error e -> Error (to_external_error c.context e))
    (Ok ()) constraints

(** Solve constraints and return all errors (not just the first).

    Useful for reporting multiple type errors at once. *)
let solve_all (constraints : C.set) : error list =
  List.filter_map
    (fun (c : C.t) ->
      match unify c.lhs c.rhs c.loc with
      | Ok () -> None
      | Error e -> Some (to_external_error c.context e))
    constraints

(** Public unify function - wraps internal unify with NoContext *)
let unify t1 t2 loc =
  match unify t1 t2 loc with
  | Ok () -> Ok ()
  | Error e -> Error (to_external_error C.NoContext e)
