(** Unification with union-find for type inference.

    This module solves equality constraints by unifying types using the
    union-find data structure. Type variables are mutable refs that can either
    be Unbound or Link to another type.

    The occurs check prevents infinite types like `a = List a`. *)

open Core.Types
module C = Constraint
module Log = Tart_log.Log

(** Controls context-sensitive unification rules. *)
type unify_context = Constraint_solving | Clause_matching

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
          if id = tv_id then (
            Log.debug "Occurs check failed: '_%d in %s" tv_id (to_string ty);
            Error (tv_id, ty, loc))
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
  | TLiteral _ -> Ok ()

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
      match param with
      | PPositional ty | POptional ty | PRest ty | PKey (_, ty) ->
          occurs_check tv_id tv_level ty loc
      | PLiteral _ -> Ok ())
    (Ok ()) params

(** Internal error type without context (used during unification) *)
type internal_error =
  | ITypeMismatch of typ * typ * Syntax.Location.span
  | IOccursCheck of tvar_id * typ * Syntax.Location.span
  | IArityMismatch of int * int * Syntax.Location.span

type 'a internal_result = ('a, internal_error) Result.t

(** Instantiate a TForall by replacing bound TCon names with fresh type
    variables.

    Used when unifying a TForall against a non-TForall type (implicit
    instantiation). Creates fresh type variables at level 0 for each bound
    variable, substitutes them into the body, and returns the instantiated type.
*)
let instantiate_forall vars body =
  let subst = List.map (fun v -> (v, fresh_tvar 0)) vars in
  let rec subst_ty subst ty =
    match ty with
    | TCon name -> (
        match List.assoc_opt name subst with Some tv -> tv | None -> ty)
    | TVar { contents = Link t } -> subst_ty subst t
    | TVar _ -> ty
    | TArrow (params, ret) ->
        TArrow (List.map (subst_param subst) params, subst_ty subst ret)
    | TApp (con, args) ->
        TApp (subst_ty subst con, List.map (subst_ty subst) args)
    | TForall (vs, b) ->
        let subst' = List.filter (fun (n, _) -> not (List.mem n vs)) subst in
        TForall (vs, subst_ty subst' b)
    | TUnion types -> TUnion (List.map (subst_ty subst) types)
    | TTuple types -> TTuple (List.map (subst_ty subst) types)
    | TRow { row_fields; row_var } ->
        TRow
          {
            row_fields =
              List.map (fun (n, t) -> (n, subst_ty subst t)) row_fields;
            row_var = Option.map (subst_ty subst) row_var;
          }
    | TLiteral _ -> ty
  and subst_param subst = function
    | PPositional t -> PPositional (subst_ty subst t)
    | POptional t -> POptional (subst_ty subst t)
    | PRest t -> PRest (subst_ty subst t)
    | PKey (n, t) -> PKey (n, subst_ty subst t)
    | PLiteral _ as p -> p
  in
  subst_ty subst body

let map_con_name = intrinsic "Map"
let plist_con_name = intrinsic "Plist"
let list_con_name = intrinsic "List"
let pair_con_name = intrinsic "Pair"
let nil_con_name = intrinsic "Nil"

(** Is the constructor the plist intrinsic? *)
let is_plist_con con =
  match repr con with TCon n -> n = plist_con_name | _ -> false

(** Is the constructor the list intrinsic? *)
let is_list_con con =
  match repr con with TCon n -> n = list_con_name | _ -> false

(** Is the constructor the pair intrinsic? *)
let is_pair_con con =
  match repr con with TCon n -> n = pair_con_name | _ -> false

(** Flatten a cons chain [(cons a (cons b ... nil))] into a list of elements.

    Returns [Some elems] for a well-formed chain terminated by nil, or [None] if
    the type is not a cons chain. *)
let flatten_cons_chain ty =
  let rec go acc t =
    match repr t with
    | TCon n when n = nil_con_name -> Some (List.rev acc)
    | TApp (con, [ car; cdr ]) when is_pair_con con -> go (car :: acc) cdr
    | _ -> None
  in
  go [] ty

(** Check if a type is a map supertype: [TApp(Map, [_])]. *)
let is_map ty =
  match repr ty with
  | TApp (con, [ _ ]) -> (
      match repr con with TCon n -> n = map_con_name | _ -> false)
  | _ -> false

(** Extract the row argument from a map type: [TApp(Map, [row])] → [Some row].
*)
let extract_map_row_arg ty =
  match repr ty with
  | TApp (con, [ row_arg ]) -> (
      match repr con with
      | TCon n when n = map_con_name -> Some row_arg
      | _ -> None)
  | _ -> None

(** Extract the row type from a concrete map form.

    Recognises expanded alist, plist, and hash-table structures:
    - alist: [(list (cons symbol TRow))] → [TRow]
    - plist: [(Plist keyword TRow)] → [TRow] (intrinsic form)
    - plist: [(list (keyword | TRow))] → [TRow] (legacy form)
    - hash-table: [(HashTable symbol TRow)] → [TRow] *)
let extract_concrete_map_row ty =
  let list_name = intrinsic "List" in
  let pair_name = intrinsic "Pair" in
  let keyword_name = intrinsic "Keyword" in
  let hash_name = intrinsic "HashTable" in
  let plist_name = intrinsic "Plist" in
  match repr ty with
  (* alist: (list (cons symbol TRow)) *)
  | TApp (list_con, [ TApp (pair_con, [ _key_ty; value_ty ]) ])
    when (match repr list_con with TCon n -> n = list_name | _ -> false)
         && match repr pair_con with TCon n -> n = pair_name | _ -> false -> (
      match repr value_ty with TRow _ -> Some value_ty | _ -> None)
  (* plist intrinsic: (Plist keyword TRow) *)
  | TApp (plist_con, [ _key_ty; value_ty ])
    when match repr plist_con with TCon n -> n = plist_name | _ -> false -> (
      match repr value_ty with TRow _ -> Some value_ty | _ -> None)
  (* plist legacy: (list (keyword | TRow)) *)
  | TApp (list_con, [ union_ty ])
    when match repr list_con with TCon n -> n = list_name | _ -> false -> (
      match repr union_ty with
      | TUnion members ->
          let has_keyword =
            List.exists
              (fun m ->
                match repr m with TCon n -> n = keyword_name | _ -> false)
              members
          in
          let row_member =
            List.find_map
              (fun m -> match repr m with TRow _ -> Some m | _ -> None)
              members
          in
          if has_keyword then row_member else None
      | _ -> None)
  (* hash-table: (HashTable symbol TRow) *)
  | TApp (ht_con, [ _key_ty; value_ty ])
    when match repr ht_con with TCon n -> n = hash_name | _ -> false -> (
      match repr value_ty with TRow _ -> Some value_ty | _ -> None)
  | _ -> None

(** Unify two types.

    This is the core unification algorithm. It follows links and handles each
    type constructor case.

    The [invariant] parameter controls whether Any acts as a wildcard. When true
    (inside type application arguments), Any must match exactly - this enforces
    invariance for parameterized types like [(list int)] not unifying with
    [(list any)].

    The [from_rest] parameter indicates whether this unification originates from
    a rest-parameter position. When true and a type variable is unified with a
    union type, the union is recorded as an upper bound rather than an equality
    link (bounded quantification, Spec 87). *)
let rec unify ?(invariant = false) ?(context = Constraint_solving)
    ?(from_rest = false) t1 t2 loc : unit internal_result =
  let t1 = repr t1 in
  let t2 = repr t2 in
  Log.debug "Unify: %s ~ %s" (to_string t1) (to_string t2);
  if t1 == t2 then
    (* Physical equality - already unified *)
    Ok ()
  else
    match (t1, t2) with
    (* Type variable cases - link the variable to the other type.
       When linking to a TLiteral, widen to the base type so that
       constraint-based contexts (vectors, function params, etc.)
       see the base type rather than a specific literal value.
       Direct inference (let bindings, bare expressions) still
       preserves the literal since no tvar is involved.

       Bounded quantification (Spec 87): when [from_rest] is true and [ty]
       is a union, record the union as an upper bound instead of linking.
       When a bounded tvar is later unified normally, check that the
       concrete type is a subtype of the bound. *)
    | TVar tv, ty | ty, TVar tv -> (
        match !tv with
        | Link _ -> failwith "repr should have followed link"
        | Unbound (id, level) -> (
            let link_ty =
              match ty with
              | TLiteral (_, base) when context <> Clause_matching -> base
              | _ -> ty
            in
            (* Occurs check before linking *)
            match occurs_check id level link_ty loc with
            | Error (tv_id, ty, loc) -> Error (IOccursCheck (tv_id, ty, loc))
            | Ok () ->
                if from_rest && is_union link_ty then (
                  (* Rest-param union: record as upper bound, leave tvar unbound *)
                  Log.debug "Bound: '_%d <: %s" id (to_string link_ty);
                  set_tvar_bound id link_ty;
                  Ok ())
                else (
                  (* Check existing upper bound before linking *)
                  (match get_tvar_bound id with
                  | None -> ()
                  | Some _bound ->
                      (* Bound will be checked after linking via
                         check_bound_after_link *)
                      ());
                  Log.debug "Link: '_%d = %s" id (to_string link_ty);
                  tv := Link link_ty;
                  check_bound_after_link id link_ty loc)))
    (* Never is the bottom type - subtype of every type.
       never <: T for all T, so unification always succeeds. *)
    | TCon n, _ when n = Prim.never_name -> Ok ()
    | _, TCon n when n = Prim.never_name -> Ok ()
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
    (* Nil-List subtyping (Spec 81): nil <: (list a) for all a.
       Emacs Lisp treats nil as the empty list — (listp nil) returns t.
       The rule is unconditional: nil is a valid (list a) regardless of
       the element type, because the empty list contains no elements. *)
    | TCon n, TApp (c, [ _ ]) when n = nil_con_name && is_list_con c -> Ok ()
    | TApp (c, [ _ ]), TCon n when is_list_con c && n = nil_con_name -> Ok ()
    (* Type applications: constructor and args must match.
       For HK types, the constructor may be a type variable that unifies
       with a concrete constructor. Arguments are unified with invariant=true
       to enforce invariance.

       Map supertype subtyping (Spec 11 R12): when one side is a map type
       and the other is a concrete map form (alist, plist, hash-table),
       extract the row from the concrete form and unify with the map's row.
       This must be checked before normal TApp unification because the
       constructors differ (Map vs List/HashTable). *)
    | TApp (c1, args1), TApp (c2, args2) ->
        if is_map t1 || is_map t2 then
          let map_ty, other = if is_map t1 then (t1, t2) else (t2, t1) in
          match
            (extract_map_row_arg map_ty, extract_concrete_map_row other)
          with
          | Some map_r, Some concrete_r ->
              unify ~invariant ~context map_r concrete_r loc
          | _ ->
              (* Not a map subtyping situation — fall through to normal TApp *)
              if List.length args1 <> List.length args2 then
                Error
                  (IArityMismatch (List.length args1, List.length args2, loc))
              else
                let* () = unify ~invariant ~context c1 c2 loc in
                unify_list ~invariant:true ~context args1 args2 loc
        else if context <> Clause_matching && is_plist_con c1 && is_list_con c2
        then
          (* Plist → list subsumption: (Plist k v) widens to (list (k | v)).
             Decompose the plist into its underlying list-of-union form.
             Suppressed during clause matching so plist and list clauses
             remain distinct. *)
          match (args1, args2) with
          | [ k; v ], [ elem ] ->
              unify ~invariant ~context (TUnion [ k; v ]) elem loc
          | _ -> Error (ITypeMismatch (t1, t2, loc))
        else if context <> Clause_matching && is_plist_con c1 && is_pair_con c2
        then
          (* Cons chain → plist promotion: flatten (cons k (cons v ... nil))
             and check alternating key-value structure.
             Suppressed during clause matching. *)
          match (args1, flatten_cons_chain t2) with
          | [ k; v ], Some elems
            when List.length elems mod 2 = 0 && List.length elems > 0 ->
              unify_cons_chain_as_plist ~invariant ~context k v elems loc
          | _ -> Error (ITypeMismatch (t1, t2, loc))
        else if context <> Clause_matching && is_pair_con c1 && is_plist_con c2
        then
          (* Symmetric: cons chain on left, plist on right.
             Suppressed during clause matching. *)
          match (flatten_cons_chain t1, args2) with
          | Some elems, [ k; v ]
            when List.length elems mod 2 = 0 && List.length elems > 0 ->
              unify_cons_chain_as_plist ~invariant ~context k v elems loc
          | _ -> Error (ITypeMismatch (t1, t2, loc))
        else if List.length args1 <> List.length args2 then
          Error (IArityMismatch (List.length args1, List.length args2, loc))
        else
          (* Unify constructors first (enables HK instantiation) *)
          let* () = unify ~invariant ~context c1 c2 loc in
          unify_list ~invariant:true ~context args1 args2 loc
    (* Function types: params and return must match.
       Handle rest/optional parameters specially. *)
    | TArrow (ps1, r1), TArrow (ps2, r2) ->
        let* () = unify_param_lists ~invariant ~context ps1 ps2 loc in
        unify ~invariant ~context r1 r2 loc
    (* Forall types: for now, require same structure.
       Full higher-rank polymorphism would need more sophisticated handling. *)
    | TForall (vs1, b1), TForall (vs2, b2) ->
        if List.length vs1 <> List.length vs2 then
          Error (ITypeMismatch (t1, t2, loc))
        else
          (* Unify bodies directly - this is simplified.
             A full implementation would alpha-rename. *)
          unify ~invariant ~context b1 b2 loc
    (* Implicit instantiation: TForall vs non-TForall (Spec 83).
       When one side is a universally quantified type (e.g. from a generalized
       defun) and the other is a concrete type, instantiate the forall with
       fresh type variables and unify the body. This enables generalized
       function types like (forall (a) (-> (any &rest a) string)) to match
       expected types like (-> (&rest any) any). *)
    | TForall (vars, body), t ->
        let instantiated = instantiate_forall vars body in
        unify ~invariant ~context instantiated t loc
    | t, TForall (vars, body) ->
        let instantiated = instantiate_forall vars body in
        unify ~invariant ~context t instantiated loc
    (* Union types: structural equality for unions on both sides *)
    | TUnion ts1, TUnion ts2 ->
        if List.length ts1 = List.length ts2 then
          unify_list ~invariant ~context ts1 ts2 loc
        else Error (ITypeMismatch (t1, t2, loc))
    (* Union on left (from signature), non-union on right (from arg):
       This is the "safe" direction - passing a more specific type where
       a union is expected. Succeeds if the arg type matches any member.

       Example: message expects (string | nil), we pass string → OK
       because string ⊆ (string | nil) *)
    | TUnion ts, t when not (is_union t) ->
        if
          List.exists
            (fun ti -> Result.is_ok (unify ~invariant ~context ti t loc))
            ts
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
        else unify_list ~invariant ~context ts1 ts2 loc
    (* Tuple-to-list subtyping (Spec 34 R9):
       TTuple [t1; t2; ...; tn] widens to (List elem) when each ti ~ elem.
       This is one-directional: tuple <: list, but not list <: tuple. *)
    | TTuple elems, TApp (c, [ elem ]) when is_list_con c ->
        unify_list_to_single ~invariant ~context elems elem loc
    | TApp (c, [ elem ]), TTuple elems when is_list_con c ->
        unify_list_to_single ~invariant ~context elems elem loc
    (* Row types: field-by-field unification with row variable handling *)
    | TRow r1, TRow r2 -> unify_rows ~invariant ~context r1 r2 loc
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
              unify ~invariant ~context field_ty t loc)
            (Ok ()) r.row_fields
        in
        match r.row_var with
        | None -> Ok ()
        | Some rv -> unify ~invariant ~context rv t loc)
    | t, TRow r when not (is_row t) -> (
        let* () =
          List.fold_left
            (fun acc (_name, field_ty) ->
              let* () = acc in
              unify ~invariant ~context t field_ty loc)
            (Ok ()) r.row_fields
        in
        match r.row_var with
        | None -> Ok ()
        | Some rv -> unify ~invariant ~context t rv loc)
    (* Literal types: same value → Ok, different value → widen to base types.
       TLiteral(_, base) widens to base type when the other side demands it.
       TVar case is handled above (links tvar to base type for proper widening). *)
    | TLiteral (v1, _), TLiteral (v2, _) when literal_value_equal v1 v2 -> Ok ()
    | TLiteral (_, base1), TLiteral (_, base2) ->
        unify ~invariant ~context base1 base2 loc
    | TLiteral (_, base), other | other, TLiteral (_, base) ->
        unify ~invariant ~context other base loc
    (* Map supertype subtyping (Spec 11 R12):
       (map {row}) is a supertype of alist, plist, and hash-table.
       When unifying a concrete map form with (Map row), extract the row
       from the concrete form and unify it with the map's row. *)
    | _ when is_map t1 || is_map t2 -> (
        let map_row, other = if is_map t1 then (t1, t2) else (t2, t1) in
        match extract_map_row_arg map_row with
        | Some map_r -> (
            match extract_concrete_map_row other with
            | Some concrete_r -> unify ~invariant ~context map_r concrete_r loc
            | None -> Error (ITypeMismatch (t1, t2, loc)))
        | None -> Error (ITypeMismatch (t1, t2, loc)))
    (* All other combinations are type mismatches *)
    | _ -> Error (ITypeMismatch (t1, t2, loc))

(** Check that a concrete type satisfies an upper bound after linking.

    When a bounded tvar (one that acquired an upper bound from rest-param
    unification) is linked to a concrete type [S], verify that [S <: U] where
    [U] is the bound. For union bounds, [S] must match at least one member of
    the union. On success, the bound is cleared. On failure, returns a type
    mismatch error. *)
and check_bound_after_link id link_ty loc : unit internal_result =
  match get_tvar_bound id with
  | None -> Ok ()
  | Some bound -> (
      Log.debug "Check bound: '_%d = %s <: %s" id (to_string link_ty)
        (to_string bound);
      (* Use the existing unify logic: unify bound link_ty.
         For TUnion bounds, the TUnion-vs-non-union arm in unify handles
         S <: (A | B | C) by checking if S matches any member.
         We do this speculatively to avoid mutating the bound's tvars. *)
      let snapshot_bound = collect_tvar_refs_flat [] bound in
      let snapshot_link = collect_tvar_refs_flat [] link_ty in
      let snapshot = snapshot_bound @ snapshot_link in
      match unify bound link_ty loc with
      | Ok () ->
          (* Restore — we don't want the bound check to permanently mutate
             tvars inside the bound type. The tvar is already linked. *)
          List.iter (fun (tv, saved) -> tv := saved) snapshot;
          remove_tvar_bound id;
          Ok ()
      | Error _ ->
          List.iter (fun (tv, saved) -> tv := saved) snapshot;
          Error (ITypeMismatch (link_ty, bound, loc)))

(** Collect tvar refs without deduplication (fast, for snapshots). *)
and collect_tvar_refs_flat acc ty =
  match ty with
  | TVar tv -> (
      let saved = !tv in
      let acc = (tv, saved) :: acc in
      match saved with
      | Link ty' -> collect_tvar_refs_flat acc ty'
      | Unbound _ -> acc)
  | TCon _ -> acc
  | TApp (con, args) ->
      List.fold_left collect_tvar_refs_flat
        (collect_tvar_refs_flat acc con)
        args
  | TArrow (params, ret) ->
      let acc =
        List.fold_left
          (fun a p ->
            match p with
            | PPositional t | POptional t | PRest t | PKey (_, t) ->
                collect_tvar_refs_flat a t
            | PLiteral _ -> a)
          acc params
      in
      collect_tvar_refs_flat acc ret
  | TForall (_, body) -> collect_tvar_refs_flat acc body
  | TUnion types | TTuple types ->
      List.fold_left collect_tvar_refs_flat acc types
  | TRow { row_fields; row_var } -> (
      let acc =
        List.fold_left
          (fun a (_, t) -> collect_tvar_refs_flat a t)
          acc row_fields
      in
      match row_var with None -> acc | Some v -> collect_tvar_refs_flat acc v)
  | TLiteral _ -> acc

and unify_list ?(invariant = false) ?(context = Constraint_solving) ts1 ts2 loc
    =
  List.fold_left2
    (fun acc t1 t2 ->
      let* () = acc in
      unify ~invariant ~context t1 t2 loc)
    (Ok ()) ts1 ts2

(** Unify each element of a list of types with a single target type.

    Used for tuple-to-list subtyping: each element of the tuple must unify with
    the list's element type. *)
and unify_list_to_single ~invariant ~context elems target loc =
  List.fold_left
    (fun acc elem ->
      let* () = acc in
      unify ~invariant ~context target elem loc)
    (Ok ()) elems

(** Unify a flattened cons chain with a plist type [(Plist k v)].

    Even-position elements (0, 2, ...) are unified with [k], odd-position
    elements (1, 3, ...) are unified with [v]. The chain must have even length
    (checked by caller). *)
and unify_cons_chain_as_plist ~invariant ~context k v elems loc =
  let rec go i = function
    | [] -> Ok ()
    | elem :: rest ->
        let target = if i mod 2 = 0 then k else v in
        let* () = unify ~invariant ~context target elem loc in
        go (i + 1) rest
  in
  go 0 elems

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
and unify_rows ?(invariant = false) ?(context = Constraint_solving) r1 r2 loc =
  let open Core.Types in
  (* Collect field names from both rows *)
  let names1 = List.map fst r1.row_fields in
  let names2 = List.map fst r2.row_fields in

  (* Find common fields and unify their types *)
  let common_names = List.filter (fun n -> List.mem n names2) names1 in
  let extra1_names = List.filter (fun n -> not (List.mem n names2)) names1 in
  let extra2_names = List.filter (fun n -> not (List.mem n names1)) names2 in
  Log.debug "Rows: common=[%s] left-only=[%s] right-only=[%s]"
    (String.concat ", " common_names)
    (String.concat ", " extra1_names)
    (String.concat ", " extra2_names);
  let* () =
    List.fold_left
      (fun acc name ->
        let* () = acc in
        let t1 = List.assoc name r1.row_fields in
        let t2 = List.assoc name r2.row_fields in
        unify ~invariant ~context t1 t2 loc)
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
  | [], _, [], _ -> unify_row_vars ~invariant ~context r1.row_var r2.row_var loc
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
      unify ~invariant ~context rv2 new_row loc
  (* r2 has extra fields, r1 is open - bind r1's row var to extra2 + r2's row var *)
  | [], _, _ :: _, Some rv1 ->
      let new_row = TRow { row_fields = extra2; row_var = r2.row_var } in
      unify ~invariant ~context rv1 new_row loc
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
      let* () = unify ~invariant ~context rv1 row_for_rv1 loc in
      unify ~invariant ~context rv2 row_for_rv2 loc

(** Unify two optional row variables *)
and unify_row_vars ?(invariant = false) ?(context = Constraint_solving) rv1 rv2
    loc =
  match (rv1, rv2) with
  | None, None -> Ok ()
  | Some v1, Some v2 -> unify ~invariant ~context v1 v2 loc
  | Some v, None | None, Some v ->
      (* One is open, other is closed - bind the open one to empty row *)
      let empty_row = Core.Types.TRow { row_fields = []; row_var = None } in
      unify ~invariant ~context v empty_row loc

(** Check if a param is a rest param *)
and is_rest_param = function PRest _ -> true | _ -> false

(** Unify two parameter lists.

    Handles the complexity of rest arguments and optional parameters. The
    strategy is: 1. If both lists have the same length, unify element-wise 2. If
    one has rest args, consume extra args from the other side 3. Otherwise, it's
    an arity mismatch *)
and unify_param_lists ?(invariant = false) ?(context = Constraint_solving) ps1
    ps2 loc =
  Log.debug "Params: %d vs %d" (List.length ps1) (List.length ps2);
  match (ps1, ps2) with
  (* Both empty - success *)
  | [], [] -> Ok ()
  (* Left side has rest param at end - consume all remaining from right.
     Pass ~from_rest:true so that union rest element types produce upper
     bounds instead of equality links (bounded quantification, Spec 87). *)
  | [ PRest elem_ty1 ], params2 ->
      Log.debug "Params: left &rest consuming %d right params"
        (List.length params2);
      (* Unify each remaining param's type with the rest element type *)
      List.fold_left
        (fun acc p2 ->
          let* () = acc in
          match p2 with
          | PPositional t2 | POptional t2 | PKey (_, t2) ->
              unify ~invariant ~context ~from_rest:true elem_ty1 t2 loc
          | PRest t2 ->
              unify ~invariant ~context ~from_rest:true elem_ty1 t2 loc
          | PLiteral _ -> Ok ())
        (Ok ()) params2
  (* Right side has rest param at end - consume all remaining from left.
     Pass ~from_rest:true (see above). *)
  | params1, [ PRest elem_ty2 ] ->
      Log.debug "Params: right &rest consuming %d left params"
        (List.length params1);
      List.fold_left
        (fun acc p1 ->
          let* () = acc in
          match p1 with
          | PPositional t1 | POptional t1 | PKey (_, t1) ->
              unify ~invariant ~context ~from_rest:true t1 elem_ty2 loc
          | PRest t1 ->
              unify ~invariant ~context ~from_rest:true t1 elem_ty2 loc
          | PLiteral _ -> Ok ())
        (Ok ()) params1
  (* Non-rest params on both sides - unify element-wise *)
  | p1 :: rest1, p2 :: rest2 ->
      if is_rest_param p1 || is_rest_param p2 then
        (* Rest param not at the end - handle case by case *)
        match (p1, p2) with
        | PRest elem_ty1, _ ->
            (* Left is rest, right is not - unify right with rest elem, continue.
               Pass ~from_rest:true for bounded quantification (Spec 87). *)
            let t2 =
              match p2 with
              | PPositional t | POptional t | PKey (_, t) | PRest t -> t
              | PLiteral _ -> Prim.any
            in
            let* () =
              unify ~invariant ~context ~from_rest:true elem_ty1 t2 loc
            in
            unify_param_lists ~invariant ~context ps1 rest2 loc
            (* Keep consuming with same rest *)
        | _, PRest elem_ty2 ->
            (* Right is rest, left is not.
               Pass ~from_rest:true for bounded quantification (Spec 87). *)
            let t1 =
              match p1 with
              | PPositional t | POptional t | PKey (_, t) | PRest t -> t
              | PLiteral _ -> Prim.any
            in
            let* () =
              unify ~invariant ~context ~from_rest:true t1 elem_ty2 loc
            in
            unify_param_lists ~invariant ~context rest1 ps2 loc
            (* Keep consuming with same rest *)
        | _, _ ->
            (* Neither is rest - unreachable given the guard, but needed for exhaustiveness *)
            let* () = unify_param ~invariant ~context p1 p2 loc in
            unify_param_lists ~invariant ~context rest1 rest2 loc
      else
        (* Neither is rest param - normal element-wise unification *)
        let* () = unify_param ~invariant ~context p1 p2 loc in
        unify_param_lists ~invariant ~context rest1 rest2 loc
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
               (function
                 | PPositional _ | PKey _ | PLiteral _ -> true | _ -> false)
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
               (function
                 | PPositional _ | PKey _ | PLiteral _ -> true | _ -> false)
               remaining)
        in
        Error (IArityMismatch (required_count, 0, loc))

and unify_param ?(invariant = false) ?(context = Constraint_solving) p1 p2 loc =
  match (p1, p2) with
  | PPositional t1, PPositional t2 -> unify ~invariant ~context t1 t2 loc
  | POptional t1, POptional t2 -> unify ~invariant ~context t1 t2 loc
  | PRest t1, PRest t2 -> unify ~invariant ~context t1 t2 loc
  | PKey (n1, t1), PKey (n2, t2) ->
      if n1 = n2 then unify ~invariant ~context t1 t2 loc
      else
        Error
          (ITypeMismatch
             (TArrow ([ p1 ], Prim.any), TArrow ([ p2 ], Prim.any), loc))
  | PLiteral v1, PLiteral v2 ->
      (* Two literal params match if they have the same value *)
      if v1 = v2 then Ok ()
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
  Log.debug "Solve: %d constraints" (List.length constraints);
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

(** {1 Speculative Unification} *)

(** Collect all tvar ref cells reachable from a type, with their current states.
    Used for snapshot/rollback during speculative unification. *)
let rec collect_tvar_refs acc ty =
  match ty with
  | TVar tv -> (
      if List.exists (fun (tv', _) -> tv == tv') acc then acc
      else
        let saved = !tv in
        let acc = (tv, saved) :: acc in
        match saved with
        | Link ty' -> collect_tvar_refs acc ty'
        | Unbound _ -> acc)
  | TCon _ -> acc
  | TApp (con, args) ->
      List.fold_left collect_tvar_refs (collect_tvar_refs acc con) args
  | TArrow (params, ret) ->
      let acc =
        List.fold_left
          (fun a p ->
            match p with
            | PPositional t | POptional t | PRest t | PKey (_, t) ->
                collect_tvar_refs a t
            | PLiteral _ -> a)
          acc params
      in
      collect_tvar_refs acc ret
  | TForall (_, body) -> collect_tvar_refs acc body
  | TUnion types | TTuple types -> List.fold_left collect_tvar_refs acc types
  | TRow { row_fields; row_var } -> (
      let acc =
        List.fold_left (fun a (_, t) -> collect_tvar_refs a t) acc row_fields
      in
      match row_var with None -> acc | Some v -> collect_tvar_refs acc v)
  | TLiteral _ -> acc

(** Restore tvar refs to their saved states. *)
let restore_tvars snapshot = List.iter (fun (tv, saved) -> tv := saved) snapshot

(** Snapshot tvar bounds for tvars reachable from a tvar snapshot.

    Collects the bound state (Some bound or None) for each unique tvar id in the
    snapshot, so bounds can be restored on rollback. *)
let snapshot_tvar_bounds (tvar_snapshot : (tvar ref * tvar) list) :
    (tvar_id * typ option) list =
  List.filter_map
    (fun (_, saved) ->
      match saved with
      | Unbound (id, _) -> Some (id, get_tvar_bound id)
      | Link _ -> None)
    tvar_snapshot

(** Restore tvar bounds from a snapshot. *)
let restore_tvar_bounds (bound_snapshot : (tvar_id * typ option) list) : unit =
  List.iter
    (fun (id, saved_bound) ->
      match saved_bound with
      | Some b -> set_tvar_bound id b
      | None -> remove_tvar_bound id)
    bound_snapshot

(** Attempt unification speculatively. If it fails, all tvar mutations and bound
    changes are rolled back and the types remain unchanged. Returns [Ok ()] on
    success or [Error] on failure. On success, the unification side-effects are
    committed (tvars remain linked). *)
let try_unify t1 t2 loc : unit result =
  Log.debug "Try-unify: %s ~ %s" (to_string t1) (to_string t2);
  let snapshot = collect_tvar_refs (collect_tvar_refs [] t1) t2 in
  let bound_snapshot = snapshot_tvar_bounds snapshot in
  match unify t1 t2 loc with
  | Ok () ->
      Log.debug "Try-unify: success";
      Ok ()
  | Error e ->
      Log.debug "Try-unify: rollback";
      restore_tvars snapshot;
      restore_tvar_bounds bound_snapshot;
      Error (to_external_error C.NoContext e)

(** Attempt to unify two param lists speculatively. Rolls back on failure.

    When [context] is [Clause_matching], cross-constructor subsumption rules are
    suppressed during unification. *)
let try_unify_params ?(context = Constraint_solving) ps1 ps2 loc : unit result =
  let snapshot =
    List.fold_left
      (fun acc p ->
        match p with
        | PPositional t | POptional t | PRest t | PKey (_, t) ->
            collect_tvar_refs acc t
        | PLiteral _ -> acc)
      [] (ps1 @ ps2)
  in
  let bound_snapshot = snapshot_tvar_bounds snapshot in
  match unify_param_lists ~context ps1 ps2 loc with
  | Ok () -> Ok ()
  | Error e ->
      restore_tvars snapshot;
      restore_tvar_bounds bound_snapshot;
      Error (to_external_error C.NoContext e)

(** Attempt to unify all types in a list with a single element variable
    speculatively. Takes a snapshot of all tvars across all types plus the
    element variable, attempts to unify each type with the element variable, and
    either commits all mutations (returns [true]) or rolls back everything
    (returns [false]).

    Used by the [list] intrinsic (Spec 84) to check whether argument types are
    homogeneous. *)
let try_unify_all_to_element types elem_var loc : bool =
  let snapshot =
    List.fold_left collect_tvar_refs (collect_tvar_refs [] elem_var) types
  in
  let bound_snapshot = snapshot_tvar_bounds snapshot in
  let ok =
    List.for_all
      (fun t ->
        match unify elem_var t loc with Ok () -> true | Error _ -> false)
      types
  in
  if ok then true
  else (
    restore_tvars snapshot;
    restore_tvar_bounds bound_snapshot;
    false)

(** Check if two types are provably disjoint (empty intersection).

    Returns [true] when the types cannot share any value at runtime:
    - Different base types ([int] vs [symbol]) are disjoint
    - Numeric subtyping is respected: [int] and [num] are NOT disjoint
    - Unions are disjoint from [T] only when every member is disjoint from [T]
    - Type variables are conservatively non-disjoint
    - [any] overlaps with everything
    - [nil] only overlaps with [nil], unions containing [nil], and [any]

    Used by eq/eql disjointness checking (Spec 11 R14). *)
let rec types_disjoint t1 t2 : bool =
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then false
  else
    match (t1, t2) with
    (* Type variables: conservatively non-disjoint *)
    | TVar _, _ | _, TVar _ -> false
    (* Never is uninhabited: disjoint with everything *)
    | TCon n, _ when n = Prim.never_name -> true
    | _, TCon n when n = Prim.never_name -> true
    (* Any overlaps with everything *)
    | _ when is_any t1 || is_any t2 -> false
    (* Base types: disjoint if different and no subtype relationship *)
    | TCon n1, TCon n2 ->
        if n1 = n2 then false
        else
          let is_int n = n = Prim.int_name in
          let is_float n = n = Prim.float_name in
          let is_num n = n = Prim.num_name in
          let is_truthy n = n = intrinsic "Truthy" in
          (* int <: num and float <: num, so not disjoint *)
          if
            (is_num n1 || is_num n2)
            && (is_int n1 || is_int n2 || is_float n1 || is_float n2)
          then false
          else if
            (* Truthy overlaps with all non-nil concrete types *)
            is_truthy n1 || is_truthy n2
          then
            (* Truthy is disjoint only with Nil *)
            let other = if is_truthy n1 then n2 else n1 in
            other = intrinsic "Nil"
          else (* Different concrete types with no subtype relationship *)
            true
    (* Literal types: delegate to base type for disjointness *)
    | TLiteral (_, base), other | other, TLiteral (_, base) ->
        types_disjoint base other
    (* Union on either side: disjoint iff ALL members are disjoint *)
    | TUnion ts, t -> List.for_all (fun ti -> types_disjoint ti t) ts
    | t, TUnion ts -> List.for_all (fun ti -> types_disjoint t ti) ts
    (* Type applications: disjoint if constructors are disjoint *)
    | TApp (c1, _), TApp (c2, _) -> types_disjoint c1 c2
    (* Function types are never disjoint with each other *)
    | TArrow _, TArrow _ -> false
    (* Different type forms are disjoint *)
    | TCon _, TApp _ | TApp _, TCon _ -> true
    | TCon _, TArrow _ | TArrow _, TCon _ -> true
    | TApp _, TArrow _ | TArrow _, TApp _ -> true
    (* Row, Tuple, Forall: conservatively non-disjoint *)
    | _ -> false

(** Public unify function - wraps internal unify with NoContext *)
let unify t1 t2 loc =
  match unify t1 t2 loc with
  | Ok () -> Ok ()
  | Error e -> Error (to_external_error C.NoContext e)
