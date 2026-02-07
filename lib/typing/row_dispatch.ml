(** Row accessor dispatch for functions taking row-typed containers.

    When a function call has an argument with a row-typed container (plist,
    alist, hash-table, or map) and a literal key argument, this module
    implements the decision table from Spec 11 R4-R8 to produce precise
    field-level return types.

    The dispatch is signature-driven: rather than recognising hard-coded
    function names, it detects row-typed containers and literal keys from the
    actual argument types at each call site. Any function whose .tart signature
    includes a row-typed container parameter benefits automatically. *)

open Core.Types
module Env = Core.Type_env
module C = Constraint

(** Which kind of row-typed container was detected at the call site. *)
type container_kind = Plist | Alist | HashTable | Map

type detected_container = {
  dc_kind : container_kind;
  dc_index : int;  (** Index of the container argument (0-based) *)
  dc_row : row;  (** The row type extracted from the container *)
  dc_ty : typ;  (** The full container type *)
}
(** Information about a detected row-typed container argument. *)

type detected_key = {
  dk_index : int;  (** Index of the key argument (0-based) *)
  dk_literal : string;  (** The literal value (e.g. ":name" or "name") *)
}
(** Information about a detected literal key argument. *)

type dispatch_result = { result_ty : typ; container_constraint : C.t option }
(** Result of row accessor dispatch. *)

(** {1 Row extraction}

    These functions try to extract a row type from various container types. They
    are used both during detection (to find row-typed arguments) and during
    dispatch (to look up fields). *)

(** Try to extract a row type from a plist type.

    Recognises:
    - [(Plist k TRow)] — the intrinsic form
    - [(list (keyword | TRow))] — legacy expanded form

    Returns [Some row] if the type is a row-typed plist, [None] otherwise. *)
let extract_plist_row ty =
  let plist_name = intrinsic "Plist" in
  match repr ty with
  (* Intrinsic form: (Plist k v) where v is a row *)
  | TApp (plist_con, [ _key_ty; value_ty ])
    when equal (repr plist_con) (TCon plist_name) -> (
      match repr value_ty with TRow row -> Some row | _ -> None)
  (* Legacy form: (list (keyword | TRow)) *)
  | TApp (list_con, [ union_ty ])
    when equal (repr list_con) (TCon (intrinsic "List")) -> (
      match repr union_ty with
      | TUnion members -> (
          (* Look for a (keyword | TRow) union *)
          let has_keyword =
            List.exists
              (fun m ->
                match repr m with
                | TCon n -> n = intrinsic "Keyword"
                | _ -> false)
              members
          in
          let row_member =
            List.find_map
              (fun m -> match repr m with TRow row -> Some row | _ -> None)
              members
          in
          match (has_keyword, row_member) with
          | true, Some row -> Some row
          | _ -> None)
      | _ -> None)
  | _ -> None

(** Try to extract a row type from an alist type: [(list (cons symbol TRow))].

    Returns [Some row] if the type is a row-typed alist, [None] otherwise. *)
let extract_alist_row ty =
  match repr ty with
  | TApp (list_con, [ TApp (pair_con, [ _key_ty; value_ty ]) ])
    when equal (repr list_con) (TCon (intrinsic "List"))
         && equal (repr pair_con) (TCon (intrinsic "Pair")) -> (
      match repr value_ty with TRow row -> Some row | _ -> None)
  | _ -> None

(** Try to extract a row type from a hash-table type: [(HashTable symbol TRow)].

    Returns [Some row] if the type is a row-typed hash-table, [None] otherwise.
*)
let extract_hash_table_row ty =
  let ht_name = intrinsic "HashTable" in
  match repr ty with
  | TApp (ht_con, [ _key_ty; value_ty ]) when equal (repr ht_con) (TCon ht_name)
    -> (
      match repr value_ty with TRow row -> Some row | _ -> None)
  | _ -> None

(** Try to extract a row type from a map supertype: [(Map TRow)].

    Returns [Some row] if the type is a row-typed map, [None] otherwise. *)
let extract_map_row ty =
  let map_name = intrinsic "Map" in
  match repr ty with
  | TApp (map_con, [ value_ty ]) when equal (repr map_con) (TCon map_name) -> (
      match repr value_ty with TRow row -> Some row | _ -> None)
  | _ -> None

(** Build the expected container type for a given container kind from a row.

    Used for the R8 (unknown container) and Case 5 (open row, absent key) paths,
    where we constrain the container to have an open row containing the accessed
    key. *)
let build_expected_container kind row =
  match kind with
  | Plist -> plist_of Prim.keyword row
  | Alist -> list_of (pair_of Prim.symbol row)
  | HashTable -> hash_table_of Prim.symbol row
  | Map -> map_of row

(** {1 Detection}

    Detect row-typed containers and literal keys from call-site arguments. This
    replaces the hard-coded get_config lookup with type-driven detection that
    works for any function. *)

(** Try to detect a row-typed container in an argument's type.

    Returns [Some (kind, row)] if the argument has a known row-typed container
    type, [None] otherwise. *)
let detect_container_in_type ty =
  match extract_plist_row ty with
  | Some row -> Some (Plist, row)
  | None -> (
      match extract_alist_row ty with
      | Some row -> Some (Alist, row)
      | None -> (
          match extract_hash_table_row ty with
          | Some row -> Some (HashTable, row)
          | None -> (
              match extract_map_row ty with
              | Some row -> Some (Map, row)
              | None -> None)))

(** Scan argument types to find a row-typed container argument.

    Returns [Some detected_container] for the first argument whose type contains
    a row, [None] if no row-typed container is found. *)
let detect_container (arg_types : typ list) : detected_container option =
  let rec go i = function
    | [] -> None
    | ty :: rest -> (
        match detect_container_in_type ty with
        | Some (kind, row) ->
            Some { dc_kind = kind; dc_index = i; dc_row = row; dc_ty = ty }
        | None -> go (i + 1) rest)
  in
  go 0 arg_types

(** Scan argument literals to find a literal key argument.

    Returns [Some detected_key] for the first literal argument, skipping the
    container argument at [skip_index]. *)
let detect_key ~(skip_index : int) (arg_literals : string option list) :
    detected_key option =
  let rec go i = function
    | [] -> None
    | lit :: rest -> (
        if i = skip_index then go (i + 1) rest
        else
          match lit with
          | Some literal -> Some { dk_index = i; dk_literal = literal }
          | None -> go (i + 1) rest)
  in
  go 0 arg_literals

(** Whether a container kind supports a DEFAULT argument.

    plist-get has no default (only an optional predicate); the others do.

    Note: this is a heuristic. For signature-driven dispatch, the presence of a
    default is determined by whether additional optional arguments exist beyond
    the container and key. *)
let has_default kind =
  match kind with Plist -> false | Alist | HashTable | Map -> true

(** {1 Clause analysis}

    Detect the expected container kind and parameter layout from a loaded
    clause's type structure. Used for R8 dispatch when the container type is
    unknown at the call site. *)

type clause_config = {
  cc_kind : container_kind;
  cc_container_index : int;
  cc_key_index : int;
}
(** Configuration derived from clause analysis. *)

(** Detect the expected container kind from a clause parameter type.

    Looks for type constructors that represent row-typed containers:
    - [(Plist k v)] → [Some Plist]
    - [(list (cons k v))] → [Some Alist]
    - [(HashTable k v)] → [Some HashTable]
    - [(Map v)] → [Some Map]

    Works on uninstantiated clause types where type variables are [TCon] names.
*)
let detect_container_kind_in_param (ty : typ) : container_kind option =
  match ty with
  | TApp (TCon name, [ _; _ ]) when name = intrinsic "Plist" -> Some Plist
  | TApp (TCon list_name, [ TApp (TCon pair_name, [ _; _ ]) ])
    when list_name = intrinsic "List" && pair_name = intrinsic "Pair" ->
      Some Alist
  | TApp (TCon name, [ _; _ ]) when name = intrinsic "HashTable" ->
      Some HashTable
  | TApp (TCon name, [ _ ]) when name = intrinsic "Map" -> Some Map
  | _ -> None

(** Analyze a parameter list to detect row-typed container configuration.

    Walks positional parameters looking for a row-typed container pattern and a
    key parameter. Returns [Some clause_config] if found.

    The key parameter is identified as the first non-container positional
    parameter (heuristic that works for all current accessor functions). *)
let analyze_params (params : param list) : clause_config option =
  let rec find_container i = function
    | [] -> None
    | param :: rest -> (
        let ty =
          match param with
          | PPositional t | POptional t -> Some t
          | PRest _ | PKey _ | PLiteral _ -> None
        in
        match ty with
        | Some t -> (
            match detect_container_kind_in_param t with
            | Some kind -> Some (kind, i)
            | None -> find_container (i + 1) rest)
        | None -> find_container (i + 1) rest)
  in
  match find_container 0 params with
  | None -> None
  | Some (kind, container_index) ->
      (* Key is the first positional param that isn't the container *)
      let key_index =
        let rec find_key i = function
          | [] -> None
          | _ :: rest when i = container_index -> find_key (i + 1) rest
          | (PPositional _ | PLiteral _) :: _ -> Some i
          | _ :: rest -> find_key (i + 1) rest
        in
        find_key 0 params
      in
      Option.map
        (fun ki ->
          {
            cc_kind = kind;
            cc_container_index = container_index;
            cc_key_index = ki;
          })
        key_index

(** Analyze a loaded clause to detect row-typed container configuration. *)
let analyze_clause (clause : Env.loaded_clause) : clause_config option =
  analyze_params clause.lc_params

(** Analyze a function type to detect row-typed container configuration.

    Extracts parameter types from [TArrow] or [TForall ... TArrow] and delegates
    to {!analyze_params}. Used for R8 dispatch when clauses are not stored
    (single-clause functions). *)
let analyze_fn_type (ty : typ) : clause_config option =
  match ty with
  | TArrow (params, _) -> analyze_params params
  | TForall (_, TArrow (params, _)) -> analyze_params params
  | _ -> None

(** {1 Dispatch}

    The core decision table implementing Spec 11 R4-R8. *)

(** Try row accessor dispatch based on detected container and key.

    This is the signature-driven replacement for the old name-based dispatch. It
    detects row-typed container arguments and literal keys from the actual
    call-site types, then applies the decision table:

    - Cases 1-2: literal key found in row → [field_type]
    - Case 3: literal key absent, closed row, no default → [nil]
    - Case 4: literal key absent, closed row, with default → default type
    - Case 5: literal key absent, open row → [(α | nil)]
    - R8: container type unknown → constrain to open row, return [field_ty]

    Returns [Some result] if dispatched, [None] to fall through. *)
let try_dispatch (env : Env.t) ~(arg_types : typ list)
    ~(arg_literals : string option list) ~(args : Syntax.Sexp.t list) :
    dispatch_result option =
  match detect_container arg_types with
  | Some container -> (
      match detect_key ~skip_index:container.dc_index arg_literals with
      | None -> None (* No literal key — fall through *)
      | Some key ->
          let kind = container.dc_kind in
          let key_name = key.dk_literal in
          let cty = container.dc_ty in
          let container_expr =
            if container.dc_index < List.length args then
              Some (List.nth args container.dc_index)
            else None
          in
          (* rest_args: arguments after both container and key *)
          let max_idx = max container.dc_index key.dk_index in
          let rest_args_exprs = List.filteri (fun i _ -> i > max_idx) args in
          let rest_arg_types =
            List.filteri (fun i _ -> i > max_idx) arg_types
          in
          let row = container.dc_row in
          let result_ty, container_constraint =
            match row_lookup row key_name with
            | Some field_ty ->
                (* Cases 1-2: key is in the row → return field_type *)
                (field_ty, None)
            | None -> (
                match row.row_var with
                | None ->
                    (* Cases 3-4: key absent from closed row *)
                    let result_ty =
                      if has_default kind then
                        match rest_args_exprs with
                        | _ :: _ -> (
                            (* Has default arg — use its type *)
                            match rest_arg_types with
                            | default_ty :: _ -> default_ty
                            | [] -> Prim.nil)
                        | [] -> Prim.nil
                      else Prim.nil
                    in
                    (result_ty, None)
                | Some _ -> (
                    match container_expr with
                    | Some cexpr ->
                        (* Case 5: key absent from open row →
                            constrain and return (α | nil) *)
                        let field_ty = fresh_tvar (Env.current_level env) in
                        let row_var = fresh_tvar (Env.current_level env) in
                        let expected_row =
                          open_row [ (key_name, field_ty) ] row_var
                        in
                        let expected_ty =
                          build_expected_container kind expected_row
                        in
                        let c =
                          C.equal expected_ty cty (Syntax.Sexp.span_of cexpr)
                        in
                        (option_of field_ty, Some c)
                    | None -> (Prim.nil, None)))
          in
          Some { result_ty; container_constraint })
  | None -> None

(** Try row accessor dispatch for a call where the container type may be unknown
    (R8: infer row from usage).

    This handles the case where the container argument is a type variable (not
    yet constrained to a specific container type). It requires knowing which
    container kind to expect, which is determined from the function's .tart
    clause structure.

    [container_kind] is the kind of container expected from the clause analysis.
    [container_index] and [key_index] locate the relevant arguments.

    Returns [Some result] with a constraint on the container type, or [None] if
    this case doesn't apply. *)
let try_dispatch_infer (env : Env.t) ~(container_kind : container_kind)
    ~(container_index : int) ~(key_index : int) ~(arg_types : typ list)
    ~(arg_literals : string option list) ~(args : Syntax.Sexp.t list) :
    dispatch_result option =
  let key_literal =
    if key_index < List.length arg_literals then List.nth arg_literals key_index
    else None
  in
  match key_literal with
  | None -> None
  | Some key_name -> (
      let container_ty =
        if container_index < List.length arg_types then
          Some (List.nth arg_types container_index)
        else None
      in
      let container_expr =
        if container_index < List.length args then
          Some (List.nth args container_index)
        else None
      in
      match (container_ty, container_expr) with
      | Some cty, Some cexpr ->
          (* R8: Type not yet known — infer row from field access *)
          let field_ty = fresh_tvar (Env.current_level env) in
          let row_var = fresh_tvar (Env.current_level env) in
          let expected_row = open_row [ (key_name, field_ty) ] row_var in
          let expected_ty =
            build_expected_container container_kind expected_row
          in
          let c = C.equal expected_ty cty (Syntax.Sexp.span_of cexpr) in
          Some { result_ty = field_ty; container_constraint = Some c }
      | Some _, None ->
          let field_ty = fresh_tvar (Env.current_level env) in
          Some { result_ty = field_ty; container_constraint = None }
      | _ -> None)
