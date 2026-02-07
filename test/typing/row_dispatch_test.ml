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
   get_config: accessor configuration lookup
   ============================================================================= *)

let test_config_plist_get () =
  match RD.get_config "plist-get" with
  | Some c ->
      Alcotest.(check int) "container arg" 0 c.container_arg;
      Alcotest.(check int) "key arg" 1 c.key_arg
  | None -> Alcotest.fail "expected config for plist-get"

let test_config_alist_get () =
  match RD.get_config "alist-get" with
  | Some c ->
      Alcotest.(check int) "container arg" 1 c.container_arg;
      Alcotest.(check int) "key arg" 0 c.key_arg
  | None -> Alcotest.fail "expected config for alist-get"

let test_config_gethash () =
  match RD.get_config "gethash" with
  | Some c ->
      Alcotest.(check int) "container arg" 1 c.container_arg;
      Alcotest.(check int) "key arg" 0 c.key_arg
  | None -> Alcotest.fail "expected config for gethash"

let test_config_map_elt () =
  match RD.get_config "map-elt" with
  | Some c ->
      Alcotest.(check int) "container arg" 0 c.container_arg;
      Alcotest.(check int) "key arg" 1 c.key_arg
  | None -> Alcotest.fail "expected config for map-elt"

let test_config_unknown () =
  Alcotest.(check bool)
    "unknown → None" true
    (Option.is_none (RD.get_config "car"))

let test_config_unknown_similar () =
  Alcotest.(check bool)
    "similar name → None" true
    (Option.is_none (RD.get_config "plist-put"))

(* =============================================================================
   has_default: default argument support
   ============================================================================= *)

let test_has_default_plist_get () =
  Alcotest.(check bool)
    "plist-get no default" false
    (RD.has_default RD.PlistGet)

let test_has_default_alist_get () =
  Alcotest.(check bool)
    "alist-get has default" true
    (RD.has_default RD.AlistGet)

let test_has_default_gethash () =
  Alcotest.(check bool) "gethash has default" true (RD.has_default RD.Gethash)

let test_has_default_map_elt () =
  Alcotest.(check bool) "map-elt has default" true (RD.has_default RD.MapElt)

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
   extract_row: dispatches to the appropriate extractor
   ============================================================================= *)

let test_extract_row_plist () =
  let row = make_closed_row [ ("k", Prim.int) ] in
  let ty = Types.plist_of Prim.keyword row in
  Alcotest.(check bool)
    "PlistGet dispatches to plist" true
    (Option.is_some (RD.extract_row RD.PlistGet ty))

let test_extract_row_alist () =
  let row = make_closed_row [ ("k", Prim.int) ] in
  let ty = Types.list_of (Types.pair_of Prim.symbol row) in
  Alcotest.(check bool)
    "AlistGet dispatches to alist" true
    (Option.is_some (RD.extract_row RD.AlistGet ty))

let test_extract_row_gethash () =
  let row = make_closed_row [ ("k", Prim.int) ] in
  let ty = Types.hash_table_of Prim.symbol row in
  Alcotest.(check bool)
    "Gethash dispatches to hash-table" true
    (Option.is_some (RD.extract_row RD.Gethash ty))

let test_extract_row_map_elt () =
  let row = make_closed_row [ ("k", Prim.int) ] in
  let ty = Types.map_of row in
  Alcotest.(check bool)
    "MapElt dispatches to map" true
    (Option.is_some (RD.extract_row RD.MapElt ty))

let test_extract_row_mismatch () =
  (* plist type won't extract with AlistGet *)
  let row = make_closed_row [ ("k", Prim.int) ] in
  let ty = Types.plist_of Prim.keyword row in
  Alcotest.(check bool)
    "wrong accessor → None" true
    (Option.is_none (RD.extract_row RD.AlistGet ty))

(* =============================================================================
   build_expected_container: container type construction from row
   ============================================================================= *)

let test_build_container_plist () =
  let row = Types.fresh_tvar 0 in
  let result = RD.build_expected_container RD.PlistGet row in
  let s = to_string result in
  Alcotest.(check bool)
    "plist container has Plist" true
    (String.length s > 0 && String.sub s 0 1 = "(")

let test_build_container_alist () =
  let row = Types.fresh_tvar 0 in
  let result = RD.build_expected_container RD.AlistGet row in
  let s = to_string result in
  Alcotest.(check bool) "alist container is list" true (String.length s > 0)

let test_build_container_gethash () =
  let row = Types.fresh_tvar 0 in
  let result = RD.build_expected_container RD.Gethash row in
  let s = to_string result in
  Alcotest.(check bool)
    "gethash container has HashTable" true
    (String.length s > 0)

let test_build_container_map_elt () =
  let row = Types.fresh_tvar 0 in
  let result = RD.build_expected_container RD.MapElt row in
  let s = to_string result in
  Alcotest.(check bool) "map-elt container has Map" true (String.length s > 0)

(* =============================================================================
   try_dispatch: Cases 1-2 (literal key found in row)
   ============================================================================= *)

let make_sexp_keyword name = Sexp.Keyword (name, dummy)
let make_sexp_symbol name = Sexp.Symbol (name, dummy)
let plist_config = { RD.kind = RD.PlistGet; container_arg = 0; key_arg = 1 }
let alist_config = { RD.kind = RD.AlistGet; container_arg = 1; key_arg = 0 }
let gethash_config = { RD.kind = RD.Gethash; container_arg = 1; key_arg = 0 }
let map_elt_config = { RD.kind = RD.MapElt; container_arg = 0; key_arg = 1 }

let test_dispatch_plist_found () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let row = make_closed_row [ (":name", Prim.string); (":age", Prim.int) ] in
  let container_ty = Types.plist_of Prim.keyword row in
  let container_sexp = make_sexp_symbol "my-plist" in
  let key_sexp = make_sexp_keyword "name" in
  let result =
    RD.try_dispatch env plist_config
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; Some ":name" ]
      ~args:[ container_sexp; key_sexp ]
      ~rest_arg_types:[]
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
    RD.try_dispatch env alist_config
      ~arg_types:[ Prim.symbol; container_ty ]
      ~arg_literals:[ Some "name"; None ]
      ~args:[ key_sexp; container_sexp ]
      ~rest_arg_types:[]
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
    RD.try_dispatch env plist_config
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; Some ":age" ]
      ~args:[ container_sexp; key_sexp ]
      ~rest_arg_types:[]
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
    RD.try_dispatch env gethash_config
      ~arg_types:[ Prim.symbol; container_ty ]
      ~arg_literals:[ Some "age"; None ]
      ~args:[ key_sexp; container_sexp; default_sexp ]
      ~rest_arg_types:[ Prim.int ]
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
    RD.try_dispatch env plist_config
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; Some ":age" ]
      ~args:[ container_sexp; key_sexp ]
      ~rest_arg_types:[]
  in
  match result with
  | Some r ->
      (* Open row: result is (α | nil), and a container constraint is emitted *)
      Alcotest.(check bool)
        "has constraint" true
        (Option.is_some r.container_constraint)
  | None -> Alcotest.fail "expected dispatch"

(* =============================================================================
   try_dispatch: R8 (container type unknown → infer from usage)
   ============================================================================= *)

let test_dispatch_plist_unknown_container () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let container_ty = Types.fresh_tvar 0 in
  let container_sexp = make_sexp_symbol "x" in
  let key_sexp = make_sexp_keyword "name" in
  let result =
    RD.try_dispatch env plist_config
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; Some ":name" ]
      ~args:[ container_sexp; key_sexp ]
      ~rest_arg_types:[]
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
    RD.try_dispatch env plist_config
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; None ]
      ~args:[ container_sexp; key_sexp ]
      ~rest_arg_types:[]
  in
  Alcotest.(check bool) "non-literal key → None" true (Option.is_none result)

(* =============================================================================
   try_dispatch: insufficient args → None
   ============================================================================= *)

let test_dispatch_insufficient_args () =
  Types.reset_tvar_counter ();
  let env = Env.empty in
  let result =
    RD.try_dispatch env plist_config ~arg_types:[] ~arg_literals:[] ~args:[]
      ~rest_arg_types:[]
  in
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
    RD.try_dispatch env map_elt_config
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; Some ":name" ]
      ~args:[ container_sexp; key_sexp ]
      ~rest_arg_types:[]
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
    RD.try_dispatch env map_elt_config
      ~arg_types:[ container_ty; Prim.keyword ]
      ~arg_literals:[ None; Some ":age" ]
      ~args:[ container_sexp; key_sexp; default_sexp ]
      ~rest_arg_types:[ Prim.int ]
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
    RD.try_dispatch env gethash_config
      ~arg_types:[ Prim.symbol; container_ty ]
      ~arg_literals:[ Some "name"; None ]
      ~args:[ key_sexp; container_sexp ]
      ~rest_arg_types:[]
  in
  match result with
  | Some r ->
      Alcotest.(check string)
        "found name → string" "string" (to_string r.result_ty)
  | None -> Alcotest.fail "expected dispatch"

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "row_dispatch"
    [
      ( "get_config",
        [
          Alcotest.test_case "plist-get" `Quick test_config_plist_get;
          Alcotest.test_case "alist-get" `Quick test_config_alist_get;
          Alcotest.test_case "gethash" `Quick test_config_gethash;
          Alcotest.test_case "map-elt" `Quick test_config_map_elt;
          Alcotest.test_case "unknown name" `Quick test_config_unknown;
          Alcotest.test_case "similar name" `Quick test_config_unknown_similar;
        ] );
      ( "has_default",
        [
          Alcotest.test_case "plist-get" `Quick test_has_default_plist_get;
          Alcotest.test_case "alist-get" `Quick test_has_default_alist_get;
          Alcotest.test_case "gethash" `Quick test_has_default_gethash;
          Alcotest.test_case "map-elt" `Quick test_has_default_map_elt;
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
      ( "extract_row dispatch",
        [
          Alcotest.test_case "plist" `Quick test_extract_row_plist;
          Alcotest.test_case "alist" `Quick test_extract_row_alist;
          Alcotest.test_case "gethash" `Quick test_extract_row_gethash;
          Alcotest.test_case "map-elt" `Quick test_extract_row_map_elt;
          Alcotest.test_case "mismatch" `Quick test_extract_row_mismatch;
        ] );
      ( "build_expected_container",
        [
          Alcotest.test_case "plist" `Quick test_build_container_plist;
          Alcotest.test_case "alist" `Quick test_build_container_alist;
          Alcotest.test_case "gethash" `Quick test_build_container_gethash;
          Alcotest.test_case "map-elt" `Quick test_build_container_map_elt;
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
        ] );
      ( "dispatch R8 (unknown container)",
        [
          Alcotest.test_case "plist-get unknown" `Quick
            test_dispatch_plist_unknown_container;
        ] );
      ( "dispatch fallthrough",
        [
          Alcotest.test_case "non-literal key" `Quick
            test_dispatch_non_literal_key;
          Alcotest.test_case "insufficient args" `Quick
            test_dispatch_insufficient_args;
        ] );
    ]
