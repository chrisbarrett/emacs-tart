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
  | TRow { row_fields; row_var } -> (
      let* () =
        occurs_check_list tv_id tv_level (List.map snd row_fields) loc
      in
      match row_var with
      | None -> Ok ()
      | Some var -> occurs_check tv_id tv_level var loc)

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
       This allows integer literals to be passed where Num is expected.

       Uses intrinsic names (e.g., "%tart-intrinsic%Int") via Prim constants. *)
    | TCon n1, TCon n2 ->
        if n1 = n2 then Ok ()
        else
          let is_int name = name = Prim.int_name in
          let is_float name = name = Prim.float_name in
          let is_num name = name = Prim.num_name in
          (* Num accepts Int or Float *)
          if is_num n1 && (is_int n2 || is_float n2) then Ok ()
          else if is_num n2 && (is_int n1 || is_float n1) then Ok ()
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
    (* Row types: field-by-field unification with row variable handling *)
    | TRow r1, TRow r2 -> unify_rows ~invariant r1 r2 loc
    (* Row-to-homogeneous unification (Spec 11 R15):
       TRow {f1:t1, f2:t2, ...} ~ T succeeds when every ti ~ T.
       For open rows, the row variable is bound to T so that any
       future fields also satisfy the constraint. This enables
       row-typed alists to be compatible with homogeneous alists. *)
    | TRow r, t when not (is_row t) -> (
        let* () =
          List.fold_left
            (fun acc (_name, field_ty) ->
              let* () = acc in
              unify ~invariant field_ty t loc)
            (Ok ()) r.row_fields
        in
        match r.row_var with
        | None -> Ok ()
        | Some rv -> unify ~invariant rv t loc)
    | t, TRow r when not (is_row t) -> (
        let* () =
          List.fold_left
            (fun acc (_name, field_ty) ->
              let* () = acc in
              unify ~invariant t field_ty loc)
            (Ok ()) r.row_fields
        in
        match r.row_var with
        | None -> Ok ()
        | Some rv -> unify ~invariant t rv loc)
    (* All other combinations are type mismatches *)
    | _ -> Error (ITypeMismatch (t1, t2, loc))

and unify_list ?(invariant = false) ts1 ts2 loc =
  List.fold_left2
    (fun acc t1 t2 ->
      let* () = acc in
      unify ~invariant t1 t2 loc)
    (Ok ()) ts1 ts2

(** Unify two row types.

    Row unification rules (per Spec 11):
    1. Matching fields must have unifiable types
    2. Extra fields in one row can unify with the other's row variable
    3. Closed rows (no row variable) must have exactly the same fields
    4. Open rows can accept extra fields via their row variable

    Examples:
    - [{name string & r1}] ~ [{name string age int}] => r1 = {age int}
    - [{name string & r1}] ~ [{name string age int & r2}] => r1 = {age int & r2}
    - [{name string}] ~ [{name string age int}] => FAIL (closed rejects extra)
*)
and unify_rows ?(invariant = false) r1 r2 loc =
  let open Core.Types in
  (* Collect field names from both rows *)
  let names1 = List.map fst r1.row_fields in
  let names2 = List.map fst r2.row_fields in

  (* Find common fields and unify their types *)
  let common_names = List.filter (fun n -> List.mem n names2) names1 in
  let* () =
    List.fold_left
      (fun acc name ->
        let* () = acc in
        let t1 = List.assoc name r1.row_fields in
        let t2 = List.assoc name r2.row_fields in
        unify ~invariant t1 t2 loc)
      (Ok ()) common_names
  in

  (* Find extra fields in each row *)
  let extra1 =
    List.filter (fun (n, _) -> not (List.mem n names2)) r1.row_fields
  in
  let extra2 =
    List.filter (fun (n, _) -> not (List.mem n names1)) r2.row_fields
  in

  (* Handle extra fields based on row variables *)
  match (extra1, r2.row_var, extra2, r1.row_var) with
  (* No extra fields on either side - just unify row variables if present *)
  | [], _, [], _ -> unify_row_vars ~invariant r1.row_var r2.row_var loc
  (* Extra fields in r1, r2 must be open to accept them *)
  | _ :: _, None, _, _ ->
      (* r2 is closed but r1 has extra fields - error *)
      Error (ITypeMismatch (TRow r1, TRow r2, loc))
  (* Extra fields in r2, r1 must be open to accept them *)
  | _, _, _ :: _, None ->
      (* r1 is closed but r2 has extra fields - error *)
      Error (ITypeMismatch (TRow r1, TRow r2, loc))
  (* r1 has extra fields, r2 is open - bind r2's row var to extra1 + r1's row var *)
  | _ :: _, Some rv2, [], _ ->
      let new_row = TRow { row_fields = extra1; row_var = r1.row_var } in
      unify ~invariant rv2 new_row loc
  (* r2 has extra fields, r1 is open - bind r1's row var to extra2 + r2's row var *)
  | [], _, _ :: _, Some rv1 ->
      let new_row = TRow { row_fields = extra2; row_var = r2.row_var } in
      unify ~invariant rv1 new_row loc
  (* Both have extra fields and both are open - create intermediate row var *)
  | _ :: _, Some rv2, _ :: _, Some rv1 ->
      (* Create a fresh row variable that will capture any remaining fields *)
      let fresh_var = fresh_tvar 0 in
      let row_for_rv1 =
        TRow { row_fields = extra2; row_var = Some fresh_var }
      in
      let row_for_rv2 =
        TRow { row_fields = extra1; row_var = Some fresh_var }
      in
      let* () = unify ~invariant rv1 row_for_rv1 loc in
      unify ~invariant rv2 row_for_rv2 loc

(** Unify two optional row variables *)
and unify_row_vars ?(invariant = false) rv1 rv2 loc =
  match (rv1, rv2) with
  | None, None -> Ok ()
  | Some v1, Some v2 -> unify ~invariant v1 v2 loc
  | Some v, None | None, Some v ->
      (* One is open, other is closed - bind the open one to empty row *)
      let empty_row = Core.Types.TRow { row_fields = []; row_var = None } in
      unify ~invariant v empty_row loc

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
