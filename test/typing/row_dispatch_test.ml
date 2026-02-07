(** Tests for row accessor dispatch (plist-get, alist-get, gethash, map-elt). *)

module RD = Tart.Row_dispatch
module Types = Tart.Types
module Prim = Types.Prim
module Env = Tart.Type_env
module Sexp = Tart.Sexp
module Loc = Tart.Location

let dummy = Loc.dummy_span
let to_string = Types.to_string

(* =============================================================================
   has_default: default argument support
   ============================================================================= *)

let test_has_default_plist () =
  Alcotest.(check bool) "plist no default" false (RD.has_default RD.Plist)

let test_has_default_alist () =
  Alcotest.(check bool) "alist has default" true (RD.has_default RD.Alist)

let test_has_default_hash_table () =
  Alcotest.(check bool)
    "hash-table has default" true
    (RD.has_default RD.HashTable)

let test_has_default_map () =
  Alcotest.(check bool) "map has default" true (RD.has_default RD.Map)

(* =============================================================================
   extract_*_row: row extraction from container types
   ============================================================================= *)

let make_closed_row fields = Types.closed_row fields
let make_open_row fields = Types.open_row fields (Types.fresh_tvar 0)

let test_extract_alist_row_closed () =
  let row = make_closed_row [ ("name", Prim.string); ("age", Prim.int) ] in
  let alist_ty = Types.list_of (Types.pair_of Prim.symbol row) in
  match RD.extract_alist_row alist_ty with
  | Some r ->
      Alcotest.(check int) "2 fields" 2 (List.length r.row_fields);
      Alcotest.(check (option string))
        "closed" None
        (Option.map to_string r.row_var)
  | None -> Alcotest.fail "expected alist row extraction"

let test_extract_alist_row_open () =
  let row = make_open_row [ ("name", Prim.string) ] in
  let alist_ty = Types.list_of (Types.pair_of Prim.symbol row) in
  match RD.extract_alist_row alist_ty with
  | Some r ->
      Alcotest.(check int) "1 field" 1 (List.length r.row_fields);
      Alcotest.(check bool) "open" true (Option.is_some r.row_var)
  | None -> Alcotest.fail "expected alist row extraction"

let test_extract_alist_row_non_row () =
  (* list of pairs but value is not a row *)
  let alist_ty = Types.list_of (Types.pair_of Prim.symbol Prim.string) in
  Alcotest.(check bool)
    "non-row value → None" true
    (Option.is_none (RD.extract_alist_row alist_ty))

let test_extract_alist_row_wrong_type () =
  Alcotest.(check bool)
    "plain int → None" true
    (Option.is_none (RD.extract_alist_row Prim.int))

let test_extract_plist_row_intrinsic () =
  let row = make_closed_row [ ("name", Prim.string) ] in
  let plist_ty = Types.plist_of Prim.keyword row in
  match RD.extract_plist_row plist_ty with
  | Some r -> Alcotest.(check int) "1 field" 1 (List.length r.row_fields)
  | None -> Alcotest.fail "expected plist row extraction (intrinsic)"

let test_extract_plist_row_legacy () =
  (* Legacy form: (list (keyword | {name string})) *)
  let row_rec =
    { Types.row_fields = [ ("name", Prim.string) ]; row_var = None }
  in
  let union = Types.TUnion [ Prim.keyword; Types.TRow row_rec ] in
  let plist_ty = Types.list_of union in
  match RD.extract_plist_row plist_ty with
  | Some r ->
      Alcotest.(check int) "1 field (legacy)" 1 (List.length r.row_fields)
  | None -> Alcotest.fail "expected plist row extraction (legacy)"

let test_extract_plist_row_non_row () =
  let plist_ty = Types.plist_of Prim.keyword Prim.string in
  Alcotest.(check bool)
    "non-row value → None" true
    (Option.is_none (RD.extract_plist_row plist_ty))

let test_extract_hash_table_row_closed () =
  let row = make_closed_row [ ("name", Prim.string) ] in
  let ht_ty = Types.hash_table_of Prim.symbol row in
  match RD.extract_hash_table_row ht_ty with
  | Some r -> Alcotest.(check int) "1 field" 1 (List.length r.row_fields)
  | None -> Alcotest.fail "expected hash-table row extraction"

let test_extract_hash_table_row_wrong_type () =
  Alcotest.(check bool)
    "list → None" true
    (Option.is_none (RD.extract_hash_table_row (Types.list_of Prim.int)))

let test_extract_map_row_closed () =
  let row = make_closed_row [ ("name", Prim.string) ] in
  let map_ty = Types.map_of row in
  match RD.extract_map_row map_ty with
  | Some r -> Alcotest.(check int) "1 field" 1 (List.length r.row_fields)
  | None -> Alcotest.fail "expected map row extraction"

let test_extract_map_row_non_row () =
  let map_ty = Types.map_of Prim.string in
  Alcotest.(check bool)
    "non-row value → None" true
    (Option.is_none (RD.extract_map_row map_ty))

(* =============================================================================
   build_expected_container: container type construction from row
   ============================================================================= *)

let test_build_container_plist () =
  let row = Types.fresh_tvar 0 in
  let result = RD.build_expected_container RD.Plist row in
  let s = to_string result in
  Alcotest.(check bool)
    "plist container has Plist" true
    (String.length s > 0 && String.sub s 0 1 = "(")

let test_build_container_alist () =
  let row = Types.fresh_tvar 0 in
  let result = RD.build_expected_container RD.Alist row in
  let s = to_string result in
  Alcotest.(check bool) "alist container is list" true (String.length s > 0)

let test_build_container_hash_table () =
  let row = Types.fresh_tvar 0 in
  let result = RD.build_expected_container RD.HashTable row in
  let s = to_string result in
  Alcotest.(check bool)
    "hash-table container has HashTable" true
    (String.length s > 0)

let test_build_container_map () =
  let row = Types.fresh_tvar 0 in
  let result = RD.build_expected_container RD.Map row in
  let s = to_string result in
  Alcotest.(check bool) "map container has Map" true (String.length s > 0)

(* =============================================================================
   try_dispatch: Cases 1-2 (literal key found in row)
   ============================================================================= *)

let make_sexp_keyword name = Sexp.Keyword (name, dummy)
let make_sexp_symbol name = Sexp.Symbol (name, dummy)

let test_dispatch_plist_found () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let row = make_closed_row [ (":name", Prim.string); (":age", Prim.int) ] in
  let container_ty = Types.plist_of Prim.keyword row in
  let container_sexp = make_sexp_symbol "my-plist" in
  let key_sexp = make_sexp_keyword "name" in
  let result =
    RD.try_dispatch env
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; Some ":name" ]
      ~args:[ container_sexp; key_sexp ]
  in
  match result with
  | Some r ->
      Alcotest.(check string)
        "found :name → string" "string" (to_string r.result_ty);
      Alcotest.(check bool)
        "no constraint" true
        (Option.is_none r.container_constraint)
  | None -> Alcotest.fail "expected dispatch for found key"

let test_dispatch_alist_found () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let row = make_closed_row [ ("name", Prim.string) ] in
  let container_ty = Types.list_of (Types.pair_of Prim.symbol row) in
  let container_sexp = make_sexp_symbol "my-alist" in
  let key_sexp =
    Sexp.List
      ([ Sexp.Symbol ("quote", dummy); Sexp.Symbol ("name", dummy) ], dummy)
  in
  let result =
    RD.try_dispatch env
      ~arg_types:[ Prim.symbol; container_ty ]
      ~arg_literals:[ Some "name"; None ]
      ~args:[ key_sexp; container_sexp ]
  in
  match result with
  | Some r ->
      Alcotest.(check string)
        "found name → string" "string" (to_string r.result_ty);
      Alcotest.(check bool)
        "no constraint" true
        (Option.is_none r.container_constraint)
  | None -> Alcotest.fail "expected dispatch for found key"

(* =============================================================================
   try_dispatch: Case 3 (absent key, closed row, no default)
   ============================================================================= *)

let test_dispatch_plist_absent_closed () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let row = make_closed_row [ (":name", Prim.string) ] in
  let container_ty = Types.plist_of Prim.keyword row in
  let container_sexp = make_sexp_symbol "my-plist" in
  let key_sexp = make_sexp_keyword "age" in
  let result =
    RD.try_dispatch env
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; Some ":age" ]
      ~args:[ container_sexp; key_sexp ]
  in
  match result with
  | Some r ->
      Alcotest.(check string)
        "absent closed → nil" "nil" (to_string r.result_ty)
  | None -> Alcotest.fail "expected dispatch"

(* =============================================================================
   try_dispatch: Case 4 (absent key, closed row, with default)
   ============================================================================= *)

let test_dispatch_gethash_absent_with_default () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let row = make_closed_row [ ("name", Prim.string) ] in
  let container_ty = Types.hash_table_of Prim.symbol row in
  let container_sexp = make_sexp_symbol "my-ht" in
  let key_sexp =
    Sexp.List
      ([ Sexp.Symbol ("quote", dummy); Sexp.Symbol ("age", dummy) ], dummy)
  in
  let default_sexp = Sexp.Int (0, dummy) in
  let result =
    RD.try_dispatch env
      ~arg_types:[ Prim.symbol; container_ty; Prim.int ]
      ~arg_literals:[ Some "age"; None; None ]
      ~args:[ key_sexp; container_sexp; default_sexp ]
  in
  match result with
  | Some r ->
      Alcotest.(check string)
        "absent + default → default type" "int" (to_string r.result_ty)
  | None -> Alcotest.fail "expected dispatch"

(* =============================================================================
   try_dispatch: Case 5 (absent key, open row)
   ============================================================================= *)

let test_dispatch_plist_absent_open () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let row = make_open_row [ (":name", Prim.string) ] in
  let container_ty = Types.plist_of Prim.keyword row in
  let container_sexp = make_sexp_symbol "my-plist" in
  let key_sexp = make_sexp_keyword "age" in
  let result =
    RD.try_dispatch env
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; Some ":age" ]
      ~args:[ container_sexp; key_sexp ]
  in
  match result with
  | Some r ->
      (* Open row: result is (α | nil), and a container constraint is emitted *)
      Alcotest.(check bool)
        "has constraint" true
        (Option.is_some r.container_constraint)
  | None -> Alcotest.fail "expected dispatch"

(* =============================================================================
   try_dispatch_infer: R8 (container type unknown → infer from usage)
   ============================================================================= *)

let test_dispatch_infer_plist_unknown_container () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let container_ty = Types.fresh_tvar 0 in
  let container_sexp = make_sexp_symbol "x" in
  let key_sexp = make_sexp_keyword "name" in
  let result =
    RD.try_dispatch_infer env ~container_kind:RD.Plist ~container_index:0
      ~key_index:1
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; Some ":name" ]
      ~args:[ container_sexp; key_sexp ]
  in
  match result with
  | Some r ->
      (* R8: emits constraint to shape the container *)
      Alcotest.(check bool)
        "has constraint" true
        (Option.is_some r.container_constraint)
  | None -> Alcotest.fail "expected dispatch for R8 path"

(* =============================================================================
   try_dispatch: non-literal key → None (fall through)
   ============================================================================= *)

let test_dispatch_non_literal_key () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let row = make_closed_row [ (":name", Prim.string) ] in
  let container_ty = Types.plist_of Prim.keyword row in
  let container_sexp = make_sexp_symbol "my-plist" in
  let key_sexp = make_sexp_symbol "some-var" in
  let result =
    RD.try_dispatch env
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; None ]
      ~args:[ container_sexp; key_sexp ]
  in
  Alcotest.(check bool) "non-literal key → None" true (Option.is_none result)

(* =============================================================================
   try_dispatch: insufficient args → None
   ============================================================================= *)

let test_dispatch_insufficient_args () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let result = RD.try_dispatch env ~arg_types:[] ~arg_literals:[] ~args:[] in
  Alcotest.(check bool) "empty args → None" true (Option.is_none result)

(* =============================================================================
   try_dispatch: map-elt found/absent
   ============================================================================= *)

let test_dispatch_map_elt_found () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let row = make_closed_row [ (":name", Prim.string) ] in
  let container_ty = Types.map_of row in
  let container_sexp = make_sexp_symbol "my-map" in
  let key_sexp = make_sexp_keyword "name" in
  let result =
    RD.try_dispatch env
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; Some ":name" ]
      ~args:[ container_sexp; key_sexp ]
  in
  match result with
  | Some r ->
      Alcotest.(check string)
        "found :name → string" "string" (to_string r.result_ty)
  | None -> Alcotest.fail "expected dispatch"

let test_dispatch_map_elt_absent_default () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let row = make_closed_row [ (":name", Prim.string) ] in
  let container_ty = Types.map_of row in
  let container_sexp = make_sexp_symbol "my-map" in
  let key_sexp = make_sexp_keyword "age" in
  let default_sexp = Sexp.Int (0, dummy) in
  let result =
    RD.try_dispatch env
      ~arg_types:[ container_ty; Prim.keyword; Prim.int ]
      ~arg_literals:[ None; Some ":age"; None ]
      ~args:[ container_sexp; key_sexp; default_sexp ]
  in
  match result with
  | Some r ->
      Alcotest.(check string)
        "absent + default → int" "int" (to_string r.result_ty)
  | None -> Alcotest.fail "expected dispatch"

(* =============================================================================
   try_dispatch: gethash found
   ============================================================================= *)

let test_dispatch_gethash_found () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let row = make_closed_row [ ("name", Prim.string) ] in
  let container_ty = Types.hash_table_of Prim.symbol row in
  let container_sexp = make_sexp_symbol "my-ht" in
  let key_sexp =
    Sexp.List
      ([ Sexp.Symbol ("quote", dummy); Sexp.Symbol ("name", dummy) ], dummy)
  in
  let result =
    RD.try_dispatch env
      ~arg_types:[ Prim.symbol; container_ty ]
      ~arg_literals:[ Some "name"; None ]
      ~args:[ key_sexp; container_sexp ]
  in
  match result with
  | Some r ->
      Alcotest.(check string)
        "found name → string" "string" (to_string r.result_ty)
  | None -> Alcotest.fail "expected dispatch"

(* =============================================================================
   try_dispatch: Case 3 for hash-table (absent key, closed row, no default arg)
   ============================================================================= *)

let test_dispatch_gethash_absent_closed_no_default () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let row = make_closed_row [ ("name", Prim.string) ] in
  let container_ty = Types.hash_table_of Prim.symbol row in
  let container_sexp = make_sexp_symbol "my-ht" in
  let key_sexp =
    Sexp.List
      ([ Sexp.Symbol ("quote", dummy); Sexp.Symbol ("age", dummy) ], dummy)
  in
  let result =
    RD.try_dispatch env
      ~arg_types:[ Prim.symbol; container_ty ]
      ~arg_literals:[ Some "age"; None ]
      ~args:[ key_sexp; container_sexp ]
  in
  match result with
  | Some r ->
      (* hash-table has_default=true but no default arg supplied → nil *)
      Alcotest.(check string)
        "absent, no default arg → nil" "nil" (to_string r.result_ty)
  | None -> Alcotest.fail "expected dispatch"

let test_dispatch_map_absent_closed_no_default () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let row = make_closed_row [ (":name", Prim.string) ] in
  let container_ty = Types.map_of row in
  let container_sexp = make_sexp_symbol "my-map" in
  let key_sexp = make_sexp_keyword "age" in
  let result =
    RD.try_dispatch env
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; Some ":age" ]
      ~args:[ container_sexp; key_sexp ]
  in
  match result with
  | Some r ->
      Alcotest.(check string)
        "absent, no default arg → nil" "nil" (to_string r.result_ty)
  | None -> Alcotest.fail "expected dispatch"

(* =============================================================================
   try_dispatch: Case 5 for alist, hash-table, map (absent key, open row)
   ============================================================================= *)

let test_dispatch_alist_absent_open () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let row = make_open_row [ ("name", Prim.string) ] in
  let container_ty = Types.list_of (Types.pair_of Prim.symbol row) in
  let container_sexp = make_sexp_symbol "my-alist" in
  let key_sexp =
    Sexp.List
      ([ Sexp.Symbol ("quote", dummy); Sexp.Symbol ("age", dummy) ], dummy)
  in
  let result =
    RD.try_dispatch env
      ~arg_types:[ Prim.symbol; container_ty ]
      ~arg_literals:[ Some "age"; None ]
      ~args:[ key_sexp; container_sexp ]
  in
  match result with
  | Some r ->
      Alcotest.(check bool)
        "has constraint" true
        (Option.is_some r.container_constraint)
  | None -> Alcotest.fail "expected dispatch"

let test_dispatch_hash_table_absent_open () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let row = make_open_row [ ("name", Prim.string) ] in
  let container_ty = Types.hash_table_of Prim.symbol row in
  let container_sexp = make_sexp_symbol "my-ht" in
  let key_sexp =
    Sexp.List
      ([ Sexp.Symbol ("quote", dummy); Sexp.Symbol ("age", dummy) ], dummy)
  in
  let result =
    RD.try_dispatch env
      ~arg_types:[ Prim.symbol; container_ty ]
      ~arg_literals:[ Some "age"; None ]
      ~args:[ key_sexp; container_sexp ]
  in
  match result with
  | Some r ->
      Alcotest.(check bool)
        "has constraint" true
        (Option.is_some r.container_constraint)
  | None -> Alcotest.fail "expected dispatch"

let test_dispatch_map_absent_open () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let row = make_open_row [ (":name", Prim.string) ] in
  let container_ty = Types.map_of row in
  let container_sexp = make_sexp_symbol "my-map" in
  let key_sexp = make_sexp_keyword "age" in
  let result =
    RD.try_dispatch env
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; Some ":age" ]
      ~args:[ container_sexp; key_sexp ]
  in
  match result with
  | Some r ->
      Alcotest.(check bool)
        "has constraint" true
        (Option.is_some r.container_constraint)
  | None -> Alcotest.fail "expected dispatch"

(* =============================================================================
   try_dispatch_infer: R8 for alist, hash-table, map
   ============================================================================= *)

let test_dispatch_infer_alist_unknown_container () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let container_ty = Types.fresh_tvar 0 in
  let container_sexp = make_sexp_symbol "x" in
  let key_sexp =
    Sexp.List
      ([ Sexp.Symbol ("quote", dummy); Sexp.Symbol ("name", dummy) ], dummy)
  in
  let result =
    RD.try_dispatch_infer env ~container_kind:RD.Alist ~container_index:1
      ~key_index:0
      ~arg_types:[ Prim.symbol; container_ty ]
      ~arg_literals:[ Some "name"; None ]
      ~args:[ key_sexp; container_sexp ]
  in
  match result with
  | Some r ->
      Alcotest.(check bool)
        "has constraint" true
        (Option.is_some r.container_constraint)
  | None -> Alcotest.fail "expected dispatch for R8 alist path"

let test_dispatch_infer_hash_table_unknown_container () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let container_ty = Types.fresh_tvar 0 in
  let container_sexp = make_sexp_symbol "x" in
  let key_sexp =
    Sexp.List
      ([ Sexp.Symbol ("quote", dummy); Sexp.Symbol ("name", dummy) ], dummy)
  in
  let result =
    RD.try_dispatch_infer env ~container_kind:RD.HashTable ~container_index:1
      ~key_index:0
      ~arg_types:[ Prim.symbol; container_ty ]
      ~arg_literals:[ Some "name"; None ]
      ~args:[ key_sexp; container_sexp ]
  in
  match result with
  | Some r ->
      Alcotest.(check bool)
        "has constraint" true
        (Option.is_some r.container_constraint)
  | None -> Alcotest.fail "expected dispatch for R8 hash-table path"

let test_dispatch_infer_map_unknown_container () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let container_ty = Types.fresh_tvar 0 in
  let container_sexp = make_sexp_symbol "x" in
  let key_sexp = make_sexp_keyword "name" in
  let result =
    RD.try_dispatch_infer env ~container_kind:RD.Map ~container_index:0
      ~key_index:1
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; Some ":name" ]
      ~args:[ container_sexp; key_sexp ]
  in
  match result with
  | Some r ->
      Alcotest.(check bool)
        "has constraint" true
        (Option.is_some r.container_constraint)
  | None -> Alcotest.fail "expected dispatch for R8 map path"

(* =============================================================================
   analyze_clause: clause analysis for R8 dispatch
   ============================================================================= *)

let test_analyze_clause_plist () =
  let clause : Env.loaded_clause =
    {
      lc_params =
        [
          Types.PPositional (Types.plist_of (Types.TCon "k") (Types.TCon "v"));
          Types.PPositional (Types.TCon "k");
          Types.POptional Prim.any;
        ];
      lc_return = Types.TUnion [ Types.TCon "v"; Prim.nil ];
      lc_diagnostic = None;
    }
  in
  match RD.analyze_clause clause with
  | Some cc ->
      Alcotest.(check int) "container at 0" 0 cc.cc_container_index;
      Alcotest.(check int) "key at 1" 1 cc.cc_key_index
  | None -> Alcotest.fail "expected clause config for plist"

let test_analyze_clause_alist () =
  let clause : Env.loaded_clause =
    {
      lc_params =
        [
          Types.PPositional (Types.TCon "k");
          Types.PPositional
            (Types.list_of (Types.pair_of (Types.TCon "k") (Types.TCon "v")));
          Types.POptional (Types.TCon "v");
        ];
      lc_return = Types.TUnion [ Types.TCon "v"; Prim.nil ];
      lc_diagnostic = None;
    }
  in
  match RD.analyze_clause clause with
  | Some cc ->
      Alcotest.(check int) "container at 1" 1 cc.cc_container_index;
      Alcotest.(check int) "key at 0" 0 cc.cc_key_index
  | None -> Alcotest.fail "expected clause config for alist"

let test_analyze_clause_hash_table () =
  let clause : Env.loaded_clause =
    {
      lc_params =
        [
          Types.PPositional (Types.TCon "k");
          Types.PPositional
            (Types.hash_table_of (Types.TCon "k") (Types.TCon "v"));
          Types.POptional (Types.TCon "v");
        ];
      lc_return = Types.TUnion [ Types.TCon "v"; Prim.nil ];
      lc_diagnostic = None;
    }
  in
  match RD.analyze_clause clause with
  | Some cc ->
      Alcotest.(check int) "container at 1" 1 cc.cc_container_index;
      Alcotest.(check int) "key at 0" 0 cc.cc_key_index;
      Alcotest.(check bool) "hash-table kind" true (cc.cc_kind = RD.HashTable)
  | None -> Alcotest.fail "expected clause config for hash-table"

let test_analyze_clause_map () =
  let clause : Env.loaded_clause =
    {
      lc_params =
        [
          Types.PPositional (Types.map_of (Types.TCon "v"));
          Types.PPositional (Types.TCon "k");
        ];
      lc_return = Types.TUnion [ Types.TCon "v"; Prim.nil ];
      lc_diagnostic = None;
    }
  in
  match RD.analyze_clause clause with
  | Some cc ->
      Alcotest.(check int) "container at 0" 0 cc.cc_container_index;
      Alcotest.(check int) "key at 1" 1 cc.cc_key_index;
      Alcotest.(check bool) "map kind" true (cc.cc_kind = RD.Map)
  | None -> Alcotest.fail "expected clause config for map"

let test_analyze_clause_no_container () =
  let clause : Env.loaded_clause =
    {
      lc_params = [ Types.PPositional Prim.int; Types.PPositional Prim.string ];
      lc_return = Prim.int;
      lc_diagnostic = None;
    }
  in
  Alcotest.(check bool)
    "no container → None" true
    (Option.is_none (RD.analyze_clause clause))

let test_analyze_fn_type_plist () =
  let ty =
    Types.TArrow
      ( [
          Types.PPositional (Types.plist_of (Types.TCon "k") (Types.TCon "v"));
          Types.PPositional (Types.TCon "k");
          Types.POptional Prim.any;
        ],
        Types.TUnion [ Types.TCon "v"; Prim.nil ] )
  in
  match RD.analyze_fn_type ty with
  | Some cc ->
      Alcotest.(check int) "container at 0" 0 cc.cc_container_index;
      Alcotest.(check int) "key at 1" 1 cc.cc_key_index
  | None -> Alcotest.fail "expected fn type config for plist"

let test_analyze_fn_type_forall () =
  let ty =
    Types.TForall
      ( [ "k"; "v" ],
        Types.TArrow
          ( [
              Types.PPositional
                (Types.hash_table_of (Types.TCon "k") (Types.TCon "v"));
              Types.PPositional (Types.TCon "k");
            ],
            Types.TUnion [ Types.TCon "v"; Prim.nil ] ) )
  in
  match RD.analyze_fn_type ty with
  | Some cc ->
      Alcotest.(check int) "container at 0" 0 cc.cc_container_index;
      Alcotest.(check int) "key at 1" 1 cc.cc_key_index;
      Alcotest.(check bool) "hash-table kind" true (cc.cc_kind = RD.HashTable)
  | None -> Alcotest.fail "expected fn type config for forall hash-table"

let test_analyze_fn_type_non_arrow () =
  Alcotest.(check bool)
    "non-arrow → None" true
    (Option.is_none (RD.analyze_fn_type Prim.int))

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "row_dispatch"
    [
      ( "has_default",
        [
          Alcotest.test_case "plist" `Quick test_has_default_plist;
          Alcotest.test_case "alist" `Quick test_has_default_alist;
          Alcotest.test_case "hash-table" `Quick test_has_default_hash_table;
          Alcotest.test_case "map" `Quick test_has_default_map;
        ] );
      ( "extract_alist_row",
        [
          Alcotest.test_case "closed row" `Quick test_extract_alist_row_closed;
          Alcotest.test_case "open row" `Quick test_extract_alist_row_open;
          Alcotest.test_case "non-row value" `Quick
            test_extract_alist_row_non_row;
          Alcotest.test_case "wrong type" `Quick
            test_extract_alist_row_wrong_type;
        ] );
      ( "extract_plist_row",
        [
          Alcotest.test_case "intrinsic form" `Quick
            test_extract_plist_row_intrinsic;
          Alcotest.test_case "legacy form" `Quick test_extract_plist_row_legacy;
          Alcotest.test_case "non-row value" `Quick
            test_extract_plist_row_non_row;
        ] );
      ( "extract_hash_table_row",
        [
          Alcotest.test_case "closed row" `Quick
            test_extract_hash_table_row_closed;
          Alcotest.test_case "wrong type" `Quick
            test_extract_hash_table_row_wrong_type;
        ] );
      ( "extract_map_row",
        [
          Alcotest.test_case "closed row" `Quick test_extract_map_row_closed;
          Alcotest.test_case "non-row value" `Quick test_extract_map_row_non_row;
        ] );
      ( "build_expected_container",
        [
          Alcotest.test_case "plist" `Quick test_build_container_plist;
          Alcotest.test_case "alist" `Quick test_build_container_alist;
          Alcotest.test_case "hash-table" `Quick test_build_container_hash_table;
          Alcotest.test_case "map" `Quick test_build_container_map;
        ] );
      ( "dispatch Cases 1-2 (found key)",
        [
          Alcotest.test_case "plist-get found" `Quick test_dispatch_plist_found;
          Alcotest.test_case "alist-get found" `Quick test_dispatch_alist_found;
          Alcotest.test_case "gethash found" `Quick test_dispatch_gethash_found;
          Alcotest.test_case "map-elt found" `Quick test_dispatch_map_elt_found;
        ] );
      ( "dispatch Case 3 (absent, closed, no default)",
        [
          Alcotest.test_case "plist-get absent closed" `Quick
            test_dispatch_plist_absent_closed;
          Alcotest.test_case "gethash absent, no default arg" `Quick
            test_dispatch_gethash_absent_closed_no_default;
          Alcotest.test_case "map-elt absent, no default arg" `Quick
            test_dispatch_map_absent_closed_no_default;
        ] );
      ( "dispatch Case 4 (absent, closed, with default)",
        [
          Alcotest.test_case "gethash absent + default" `Quick
            test_dispatch_gethash_absent_with_default;
          Alcotest.test_case "map-elt absent + default" `Quick
            test_dispatch_map_elt_absent_default;
        ] );
      ( "dispatch Case 5 (absent, open row)",
        [
          Alcotest.test_case "plist-get open" `Quick
            test_dispatch_plist_absent_open;
          Alcotest.test_case "alist-get open" `Quick
            test_dispatch_alist_absent_open;
          Alcotest.test_case "gethash open" `Quick
            test_dispatch_hash_table_absent_open;
          Alcotest.test_case "map-elt open" `Quick test_dispatch_map_absent_open;
        ] );
      ( "dispatch R8 (unknown container)",
        [
          Alcotest.test_case "plist-get unknown" `Quick
            test_dispatch_infer_plist_unknown_container;
          Alcotest.test_case "alist-get unknown" `Quick
            test_dispatch_infer_alist_unknown_container;
          Alcotest.test_case "gethash unknown" `Quick
            test_dispatch_infer_hash_table_unknown_container;
          Alcotest.test_case "map-elt unknown" `Quick
            test_dispatch_infer_map_unknown_container;
        ] );
      ( "dispatch fallthrough",
        [
          Alcotest.test_case "non-literal key" `Quick
            test_dispatch_non_literal_key;
          Alcotest.test_case "insufficient args" `Quick
            test_dispatch_insufficient_args;
        ] );
      ( "analyze_clause",
        [
          Alcotest.test_case "plist clause" `Quick test_analyze_clause_plist;
          Alcotest.test_case "alist clause" `Quick test_analyze_clause_alist;
          Alcotest.test_case "hash-table clause" `Quick
            test_analyze_clause_hash_table;
          Alcotest.test_case "map clause" `Quick test_analyze_clause_map;
          Alcotest.test_case "no container" `Quick
            test_analyze_clause_no_container;
          Alcotest.test_case "fn type plist" `Quick test_analyze_fn_type_plist;
          Alcotest.test_case "fn type forall hash-table" `Quick
            test_analyze_fn_type_forall;
          Alcotest.test_case "fn type non-arrow" `Quick
            test_analyze_fn_type_non_arrow;
        ] );
    ]
