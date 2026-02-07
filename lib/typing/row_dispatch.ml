(** Row accessor dispatch for plist-get, alist-get, gethash, and map-elt.

    When a call targets a known row-typed accessor and the key argument is a
    literal, this module implements the decision table from Spec 11 R4-R8
    without falling through to the generic constraint path. *)

open Core.Types
module Env = Core.Type_env
module C = Constraint

(** Which row-typed accessor is being called. *)
type accessor_kind = PlistGet | AlistGet | Gethash | MapElt

type config = {
  kind : accessor_kind;
  container_arg : int;  (** Index of the container argument (0-based) *)
  key_arg : int;  (** Index of the key argument (0-based) *)
}
(** Configuration for a row-typed accessor function.

    Describes the parameter layout and behavior of each accessor so that
    dispatch can handle all four accessors uniformly. *)

(** Look up whether a function name is a row-typed accessor.

    Returns the accessor configuration if recognized, [None] otherwise. *)
let get_config = function
  | "plist-get" -> Some { kind = PlistGet; container_arg = 0; key_arg = 1 }
  | "alist-get" -> Some { kind = AlistGet; container_arg = 1; key_arg = 0 }
  | "gethash" -> Some { kind = Gethash; container_arg = 1; key_arg = 0 }
  | "map-elt" -> Some { kind = MapElt; container_arg = 0; key_arg = 1 }
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

(** Try to extract a row type from a map supertype: [(Map TRow)].

    Returns [Some row] if the type is a row-typed map, [None] otherwise. *)
let extract_map_row ty =
  let map_name = intrinsic "Map" in
  match repr ty with
  | TApp (map_con, [ value_ty ]) when equal (repr map_con) (TCon map_name) -> (
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

(** Extract a row from a container type, dispatching on accessor kind. *)
let extract_row kind ty =
  match kind with
  | PlistGet -> extract_plist_row ty
  | AlistGet -> extract_alist_row ty
  | Gethash -> extract_hash_table_row ty
  | MapElt -> extract_map_row ty

(** Build the expected container type for a given accessor kind from a row.

    Used for the R8 (unknown container) and Case 5 (open row, absent key) paths,
    where we constrain the container to have an open row containing the accessed
    key. *)
let build_expected_container kind row =
  match kind with
  | PlistGet -> plist_of Prim.keyword row
  | AlistGet -> list_of (pair_of Prim.symbol row)
  | Gethash -> hash_table_of Prim.symbol row
  | MapElt -> map_of row

(** Whether an accessor kind supports a DEFAULT argument.

    plist-get has no default (only an optional predicate); the others do. *)
let has_default kind =
  match kind with PlistGet -> false | AlistGet | Gethash | MapElt -> true

type dispatch_result = { result_ty : typ; container_constraint : C.t option }
(** Result of row accessor dispatch. *)

(** Try row accessor dispatch for a known row-typed accessor function.

    When a call is to a known row accessor ([plist-get], [alist-get], etc.) and
    the key argument is a literal, this implements the decision table from Spec
    11 R4-R8:

    - Cases 1-2: literal key found in row → [field_type]
    - Case 3: literal key absent, closed row, no default → [nil]
    - Case 4: literal key absent, closed row, with default → default type
    - Case 5: literal key absent, open row → [(α | nil)]
    - R8: container type unknown → constrain to open row, return [field_ty]

    Returns [Some result] if the call was dispatched, [None] if the function is
    not a row accessor or the key is not a literal (caller should try clause
    dispatch or fall back to constraint path). *)
let try_dispatch (env : Env.t) (config : config) ~(arg_types : typ list)
    ~(arg_literals : string option list) ~(args : Syntax.Sexp.t list)
    ~(rest_arg_types : typ list) : dispatch_result option =
  let kind = config.kind in
  let key_literal =
    if config.key_arg < List.length arg_literals then
      List.nth arg_literals config.key_arg
    else None
  in
  match key_literal with
  | None -> None (* Non-literal key — fall through to generic dispatch *)
  | Some key_name -> (
      let container_ty =
        if config.container_arg < List.length arg_types then
          Some (List.nth arg_types config.container_arg)
        else None
      in
      let container_expr =
        if config.container_arg < List.length args then
          Some (List.nth args config.container_arg)
        else None
      in
      (* rest_args_exprs: arg expressions after both container and key *)
      let max_idx = max config.container_arg config.key_arg in
      let rest_args_exprs = List.filteri (fun i _ -> i > max_idx) args in
      match container_ty with
      | None -> None
      | Some cty ->
          let result_ty, container_constraint =
            match extract_row kind cty with
            | Some row -> (
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
                              C.equal expected_ty cty
                                (Syntax.Sexp.span_of cexpr)
                            in
                            (option_of field_ty, Some c)
                        | None -> (Prim.nil, None))))
            | None -> (
                match container_expr with
                | Some cexpr ->
                    (* R8: Type not yet known — infer row from field access *)
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
                    (field_ty, Some c)
                | None ->
                    let field_ty = fresh_tvar (Env.current_level env) in
                    (field_ty, None))
          in
          Some { result_ty; container_constraint })
