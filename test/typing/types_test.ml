(** Tests for type representation *)

open Tart.Types

(* =============================================================================
   Type Variable Tests
   ============================================================================= *)

let test_fresh_tvar () =
  reset_tvar_counter ();
  let tv1 = fresh_tvar 0 in
  let tv2 = fresh_tvar 0 in
  (* Should have different IDs *)
  Alcotest.(check bool) "different vars" true (to_string tv1 <> to_string tv2)

let test_tvar_level () =
  reset_tvar_counter ();
  match fresh_tvar 5 with
  | TVar tv ->
      Alcotest.(check (option int)) "level is 5" (Some 5) (tvar_level tv)
  | _ -> Alcotest.fail "expected TVar"

let test_tvar_id () =
  reset_tvar_counter ();
  match fresh_tvar 0 with
  | TVar tv -> Alcotest.(check (option int)) "id is 0" (Some 0) (tvar_id tv)
  | _ -> Alcotest.fail "expected TVar"

(* =============================================================================
   Union-Find / Path Compression Tests
   ============================================================================= *)

let test_repr_unbound () =
  reset_tvar_counter ();
  let tv = fresh_tvar 0 in
  (* repr of unbound var is itself *)
  Alcotest.(check bool) "repr returns same" true (repr tv == tv)

let test_repr_linked () =
  reset_tvar_counter ();
  let tv = fresh_tvar 0 in
  match tv with
  | TVar tvar_ref ->
      (* Link to Int *)
      tvar_ref := Link Prim.int;
      let result = repr tv in
      (* Intrinsic types display as lowercase for user-facing output *)
      Alcotest.(check string) "repr follows link" "int" (to_string result)
  | _ -> Alcotest.fail "expected TVar"

let test_repr_chain () =
  reset_tvar_counter ();
  (* Create chain: tv1 -> tv2 -> tv3 -> Int *)
  let tv1 = fresh_tvar 0 in
  let tv2 = fresh_tvar 0 in
  let tv3 = fresh_tvar 0 in
  (match (tv1, tv2, tv3) with
  | TVar r1, TVar r2, TVar r3 ->
      r3 := Link Prim.int;
      r2 := Link tv3;
      r1 := Link tv2
  | _ -> Alcotest.fail "expected TVars");
  (* repr should reach Int through the chain *)
  (* Intrinsic types display as lowercase for user-facing output *)
  Alcotest.(check string) "repr follows chain" "int" (to_string (repr tv1));
  (* Path compression should update links *)
  match tv1 with
  | TVar r1 -> (
      match !r1 with
      | Link t -> Alcotest.(check string) "path compressed" "int" (to_string t)
      | _ -> Alcotest.fail "expected Link after repr")
  | _ -> Alcotest.fail "expected TVar"

(* =============================================================================
   Type Construction Tests
   ============================================================================= *)

let test_primitive_types () =
  (* Intrinsic types display as lowercase for user-facing output *)
  Alcotest.(check string) "int" "int" (to_string Prim.int);
  Alcotest.(check string) "string" "string" (to_string Prim.string);
  Alcotest.(check string) "nil" "nil" (to_string Prim.nil);
  Alcotest.(check string) "t" "t" (to_string Prim.t);
  Alcotest.(check string) "bool" "(Or t nil)" (to_string Prim.bool);
  Alcotest.(check string) "any" "(Or truthy nil)" (to_string Prim.any);
  Alcotest.(check string) "truthy" "truthy" (to_string Prim.truthy);
  Alcotest.(check string) "never" "never" (to_string Prim.never)

let test_list_type () =
  let ty = list_of Prim.int in
  (* Intrinsic types display as lowercase for user-facing output *)
  Alcotest.(check string) "list int" "(list int)" (to_string ty)

let test_option_type () =
  let ty = option_of Prim.string in
  (* Intrinsic types display as lowercase for user-facing output *)
  Alcotest.(check string) "option string" "(Or string nil)" (to_string ty)

let test_pair_type () =
  let ty = pair_of Prim.string Prim.int in
  (* Intrinsic types display as lowercase for user-facing output *)
  Alcotest.(check string) "cons string int" "(cons string int)" (to_string ty)

let test_nested_type () =
  let ty = list_of (option_of Prim.int) in
  (* Intrinsic types display as lowercase for user-facing output *)
  Alcotest.(check string)
    "list (option int)" "(list (Or int nil))" (to_string ty)

(* =============================================================================
   Function Type Tests
   ============================================================================= *)

let test_arrow_simple () =
  let ty = arrow [ Prim.int; Prim.int ] Prim.int in
  (* Intrinsic types display as lowercase for user-facing output *)
  Alcotest.(check string)
    "int -> int -> int" "(-> (int int) int)" (to_string ty)

let test_arrow_no_args () =
  let ty = arrow [] Prim.int in
  (* Intrinsic types display as lowercase for user-facing output *)
  Alcotest.(check string) "() -> int" "(-> () int)" (to_string ty)

let test_arrow_with_optional () =
  let ty =
    TArrow
      ([ PPositional Prim.string; POptional (option_of Prim.int) ], Prim.string)
  in
  (* Intrinsic types display as lowercase for user-facing output *)
  Alcotest.(check string)
    "with optional" "(-> (string &optional (Or int nil)) string)" (to_string ty)

let test_arrow_with_rest () =
  let ty = TArrow ([ PRest Prim.string ], Prim.string) in
  (* Intrinsic types display as lowercase for user-facing output *)
  Alcotest.(check string)
    "with rest" "(-> (&rest string) string)" (to_string ty)

let test_arrow_with_key () =
  let ty = TArrow ([ PKey (":name", Prim.string) ], Prim.nil) in
  (* Intrinsic types display as lowercase for user-facing output *)
  Alcotest.(check string)
    "with keyword" "(-> (&key :name string) nil)" (to_string ty)

(* =============================================================================
   Polymorphic Type Tests
   ============================================================================= *)

let test_forall_identity () =
  let ty = forall [ "a" ] (arrow [ TCon "a" ] (TCon "a")) in
  Alcotest.(check string)
    "identity type" "(forall (a) (-> (a) a))" (to_string ty)

let test_forall_multi_vars () =
  let ty = forall [ "a"; "b" ] (arrow [ TCon "a"; TCon "b" ] (TCon "a")) in
  Alcotest.(check string)
    "multi-var forall" "(forall (a b) (-> (a b) a))" (to_string ty)

let test_forall_list_length () =
  let ty = forall [ "a" ] (arrow [ list_of (TCon "a") ] Prim.int) in
  (* Intrinsic types display as lowercase for user-facing output *)
  Alcotest.(check string)
    "list length type" "(forall (a) (-> ((list a)) int))" (to_string ty)

(* =============================================================================
   Union Type Tests
   ============================================================================= *)

let test_union_simple () =
  let ty = TUnion [ Prim.int; Prim.string ] in
  (* Intrinsic types display as lowercase for user-facing output *)
  Alcotest.(check string) "Or int string" "(Or int string)" (to_string ty)

let test_tuple_type () =
  let ty = TTuple [ Prim.string; Prim.int; Prim.bool ] in
  (* Intrinsic types display as lowercase for user-facing output *)
  Alcotest.(check string)
    "Tuple string int bool" "(Tuple string int (Or t nil))" (to_string ty)

(* =============================================================================
   Type Equality Tests
   ============================================================================= *)

let test_equal_primitives () =
  Alcotest.(check bool) "Int = int" true (equal Prim.int Prim.int);
  Alcotest.(check bool) "Int <> string" false (equal Prim.int Prim.string)

let test_equal_tapp () =
  let t1 = list_of Prim.int in
  let t2 = list_of Prim.int in
  let t3 = list_of Prim.string in
  Alcotest.(check bool) "List Int = List int" true (equal t1 t2);
  Alcotest.(check bool) "List Int <> List string" false (equal t1 t3)

let test_equal_arrow () =
  let t1 = arrow [ Prim.int ] Prim.int in
  let t2 = arrow [ Prim.int ] Prim.int in
  let t3 = arrow [ Prim.string ] Prim.int in
  Alcotest.(check bool) "same arrow" true (equal t1 t2);
  Alcotest.(check bool) "diff arrow" false (equal t1 t3)

let test_equal_tvars () =
  reset_tvar_counter ();
  let tv1 = fresh_tvar 0 in
  let tv2 = fresh_tvar 0 in
  Alcotest.(check bool) "same tvar" true (equal tv1 tv1);
  Alcotest.(check bool) "diff tvars" false (equal tv1 tv2)

let test_equal_linked_tvar () =
  reset_tvar_counter ();
  let tv = fresh_tvar 0 in
  (match tv with TVar r -> r := Link Prim.int | _ -> ());
  Alcotest.(check bool) "linked tvar equals target" true (equal tv Prim.int)

(* =============================================================================
   Truthiness Tests
   ============================================================================= *)

let test_truthy_primitives () =
  (* truthy primitives *)
  Alcotest.(check bool) "Int is truthy" true (is_truthy Prim.int);
  Alcotest.(check bool) "Float is truthy" true (is_truthy Prim.float);
  Alcotest.(check bool) "Num is truthy" true (is_truthy Prim.num);
  Alcotest.(check bool) "String is truthy" true (is_truthy Prim.string);
  Alcotest.(check bool) "Symbol is truthy" true (is_truthy Prim.symbol);
  Alcotest.(check bool) "Keyword is truthy" true (is_truthy Prim.keyword);
  Alcotest.(check bool) "T is truthy" true (is_truthy Prim.t);
  Alcotest.(check bool) "truthy is truthy" true (is_truthy Prim.truthy);
  Alcotest.(check bool) "Never is truthy" true (is_truthy Prim.never)

let test_falsy_types () =
  (* Falsy primitives *)
  Alcotest.(check bool) "Nil is NOT truthy" false (is_truthy Prim.nil);
  Alcotest.(check bool) "Bool is NOT truthy" false (is_truthy Prim.bool);
  Alcotest.(check bool) "Any is NOT truthy" false (is_truthy Prim.any)

let test_truthy_containers () =
  (* Container types are truthy *)
  Alcotest.(check bool) "List is truthy" true (is_truthy (list_of Prim.int));
  Alcotest.(check bool)
    "Vector is truthy" true
    (is_truthy (vector_of Prim.string));
  Alcotest.(check bool)
    "Pair is truthy" true
    (is_truthy (pair_of Prim.int Prim.string));
  Alcotest.(check bool)
    "HashTable is truthy" true
    (is_truthy (hash_table_of Prim.symbol Prim.string))

let test_option_not_truthy () =
  (* Option types are NOT truthy (they include nil) *)
  Alcotest.(check bool)
    "Option String is NOT truthy" false
    (is_truthy (option_of Prim.string))

let test_arrow_truthy () =
  (* Function types are truthy *)
  let fn_type = arrow [ Prim.int ] Prim.string in
  Alcotest.(check bool) "arrow is truthy" true (is_truthy fn_type)

let test_tuple_truthy () =
  (* Tuple types are truthy *)
  let tuple_type = TTuple [ Prim.int; Prim.string ] in
  Alcotest.(check bool) "tuple is truthy" true (is_truthy tuple_type)

let test_forall_truthy () =
  (* Forall types inherit truthiness from body *)
  let truthy_forall = forall [ "a" ] (arrow [ TCon "a" ] (TCon "a")) in
  let falsy_forall = forall [ "a" ] Prim.any in
  Alcotest.(check bool)
    "forall with truthy body is truthy" true (is_truthy truthy_forall);
  Alcotest.(check bool)
    "forall with Any body is NOT truthy" false (is_truthy falsy_forall)

let test_union_truthy () =
  (* Union is truthy only if ALL members are truthy *)
  let all_truthy = TUnion [ Prim.int; Prim.string ] in
  let has_nil = TUnion [ Prim.int; Prim.nil ] in
  let has_any = TUnion [ Prim.string; Prim.any ] in
  Alcotest.(check bool)
    "union of truthy types is truthy" true (is_truthy all_truthy);
  Alcotest.(check bool) "union with Nil is NOT truthy" false (is_truthy has_nil);
  Alcotest.(check bool) "union with Any is NOT truthy" false (is_truthy has_any)

let test_tvar_truthy () =
  (* Unresolved type variables are conservatively NOT truthy *)
  reset_tvar_counter ();
  let tv = fresh_tvar 0 in
  Alcotest.(check bool) "unresolved tvar is NOT truthy" false (is_truthy tv)

let test_linked_tvar_truthy () =
  (* Linked type variables inherit truthiness from target *)
  reset_tvar_counter ();
  let tv1 = fresh_tvar 0 in
  let tv2 = fresh_tvar 0 in
  (match (tv1, tv2) with
  | TVar r1, TVar r2 ->
      r1 := Link Prim.int;
      r2 := Link Prim.nil
  | _ -> ());
  Alcotest.(check bool) "tvar linked to Int is truthy" true (is_truthy tv1);
  Alcotest.(check bool) "tvar linked to Nil is NOT truthy" false (is_truthy tv2)

(* =============================================================================
   Option Validation Tests
   ============================================================================= *)

let test_option_of_checked_string () =
  (* String is truthy, so Option String should succeed *)
  match option_of_checked Prim.string with
  | Ok ty ->
      (* Intrinsic types display as lowercase for user-facing output *)
      Alcotest.(check string)
        "option string valid" "(Or string nil)" (to_string ty)
  | Error _ -> Alcotest.fail "option string should be valid"

let test_option_of_checked_int () =
  (* Int is truthy, so Option Int should succeed *)
  match option_of_checked Prim.int with
  | Ok ty ->
      (* Intrinsic types display as lowercase for user-facing output *)
      Alcotest.(check string) "option int valid" "(Or int nil)" (to_string ty)
  | Error _ -> Alcotest.fail "option int should be valid"

let test_option_of_checked_list () =
  (* List is truthy, so Option (list a) should succeed *)
  match option_of_checked (list_of Prim.int) with
  | Ok ty ->
      (* Intrinsic types display as lowercase for user-facing output *)
      Alcotest.(check string)
        "option (list int) valid" "(Or (list int) nil)" (to_string ty)
  | Error _ -> Alcotest.fail "option (list int) should be valid"

let test_option_of_checked_nil_fails () =
  (* Nil is NOT truthy, so Option Nil should fail *)
  match option_of_checked Prim.nil with
  | Ok _ -> Alcotest.fail "option nil should be invalid"
  | Error (NonTruthyOptionArg ty) ->
      (* Intrinsic types display as lowercase for user-facing output *)
      Alcotest.(check string) "error contains nil" "nil" (to_string ty)

let test_option_of_checked_any_fails () =
  (* Any is NOT truthy, so Option Any should fail *)
  match option_of_checked Prim.any with
  | Ok _ -> Alcotest.fail "option any should be invalid"
  | Error (NonTruthyOptionArg ty) ->
      (* Intrinsic types display as lowercase for user-facing output *)
      Alcotest.(check string)
        "error contains any" "(Or truthy nil)" (to_string ty)

let test_option_of_checked_bool_fails () =
  (* Bool is NOT truthy (includes nil), so Option Bool should fail *)
  match option_of_checked Prim.bool with
  | Ok _ -> Alcotest.fail "option bool should be invalid"
  | Error (NonTruthyOptionArg ty) ->
      (* Intrinsic types display as lowercase for user-facing output *)
      Alcotest.(check string) "error contains bool" "(Or t nil)" (to_string ty)

let test_option_of_checked_option_fails () =
  (* Option String is NOT truthy, so Option (Option string) should fail *)
  match option_of_checked (option_of Prim.string) with
  | Ok _ -> Alcotest.fail "option (option string) should be invalid"
  | Error (NonTruthyOptionArg ty) ->
      (* Intrinsic types display as lowercase for user-facing output *)
      Alcotest.(check string)
        "error contains option" "(Or string nil)" (to_string ty)

let test_validation_error_message () =
  (* Test error message formatting *)
  let err = NonTruthyOptionArg Prim.nil in
  let msg = validation_error_to_string err in
  Alcotest.(check bool)
    "message mentions truthy" true
    (String.length msg > 0 && String.sub msg 0 6 = "Option")

(* =============================================================================
   Row Type Tests
   ============================================================================= *)

let test_closed_row () =
  let row = closed_row [ ("name", Prim.string); ("age", Prim.int) ] in
  Alcotest.(check string)
    "closed row format" "{name string age int}" (to_string row)

let test_open_row () =
  reset_tvar_counter ();
  let row_var = fresh_tvar 0 in
  let row = open_row [ ("name", Prim.string) ] row_var in
  Alcotest.(check string)
    "open row format" "{name string & '_0}" (to_string row)

let test_row_lookup () =
  let row =
    match closed_row [ ("name", Prim.string); ("age", Prim.int) ] with
    | TRow r -> r
    | _ -> Alcotest.fail "expected TRow"
  in
  Alcotest.(check (option string))
    "lookup name" (Some "string")
    (Option.map to_string (row_lookup row "name"));
  Alcotest.(check (option string))
    "lookup missing" None
    (Option.map to_string (row_lookup row "missing"))

let test_row_has_field () =
  let row =
    match closed_row [ ("name", Prim.string) ] with
    | TRow r -> r
    | _ -> Alcotest.fail "expected TRow"
  in
  Alcotest.(check bool) "has name" true (row_has_field row "name");
  Alcotest.(check bool) "no age" false (row_has_field row "age")

let test_row_is_row () =
  let row = closed_row [ ("x", Prim.int) ] in
  Alcotest.(check bool) "row is row" true (is_row row);
  Alcotest.(check bool) "int is not row" false (is_row Prim.int)

let test_row_is_open () =
  reset_tvar_counter ();
  let closed = closed_row [ ("x", Prim.int) ] in
  let open_ = open_row [ ("x", Prim.int) ] (fresh_tvar 0) in
  Alcotest.(check bool) "closed is not open" false (is_open_row closed);
  Alcotest.(check bool) "open is open" true (is_open_row open_)

let test_row_truthy () =
  let row = closed_row [ ("name", Prim.string) ] in
  Alcotest.(check bool) "row is truthy" true (is_truthy row)

let test_row_equal () =
  let r1 = closed_row [ ("name", Prim.string); ("age", Prim.int) ] in
  let r2 = closed_row [ ("name", Prim.string); ("age", Prim.int) ] in
  let r3 = closed_row [ ("name", Prim.string) ] in
  Alcotest.(check bool) "same rows equal" true (equal r1 r2);
  Alcotest.(check bool) "different rows not equal" false (equal r1 r3)

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "types"
    [
      ( "tvar",
        [
          Alcotest.test_case "fresh_tvar different ids" `Quick test_fresh_tvar;
          Alcotest.test_case "tvar level" `Quick test_tvar_level;
          Alcotest.test_case "tvar id" `Quick test_tvar_id;
        ] );
      ( "union-find",
        [
          Alcotest.test_case "repr unbound" `Quick test_repr_unbound;
          Alcotest.test_case "repr linked" `Quick test_repr_linked;
          Alcotest.test_case "repr chain" `Quick test_repr_chain;
        ] );
      ( "construction",
        [
          Alcotest.test_case "primitive types" `Quick test_primitive_types;
          Alcotest.test_case "list type" `Quick test_list_type;
          Alcotest.test_case "option type" `Quick test_option_type;
          Alcotest.test_case "pair type" `Quick test_pair_type;
          Alcotest.test_case "nested type" `Quick test_nested_type;
        ] );
      ( "arrow",
        [
          Alcotest.test_case "simple arrow" `Quick test_arrow_simple;
          Alcotest.test_case "no args" `Quick test_arrow_no_args;
          Alcotest.test_case "with optional" `Quick test_arrow_with_optional;
          Alcotest.test_case "with rest" `Quick test_arrow_with_rest;
          Alcotest.test_case "with keyword" `Quick test_arrow_with_key;
        ] );
      ( "forall",
        [
          Alcotest.test_case "identity" `Quick test_forall_identity;
          Alcotest.test_case "multi vars" `Quick test_forall_multi_vars;
          Alcotest.test_case "list length" `Quick test_forall_list_length;
        ] );
      ( "union",
        [
          Alcotest.test_case "simple union" `Quick test_union_simple;
          Alcotest.test_case "tuple type" `Quick test_tuple_type;
        ] );
      ( "equality",
        [
          Alcotest.test_case "primitives" `Quick test_equal_primitives;
          Alcotest.test_case "type application" `Quick test_equal_tapp;
          Alcotest.test_case "arrow types" `Quick test_equal_arrow;
          Alcotest.test_case "type vars" `Quick test_equal_tvars;
          Alcotest.test_case "linked tvar" `Quick test_equal_linked_tvar;
        ] );
      ( "truthiness",
        [
          Alcotest.test_case "truthy primitives" `Quick test_truthy_primitives;
          Alcotest.test_case "falsy types" `Quick test_falsy_types;
          Alcotest.test_case "truthy containers" `Quick test_truthy_containers;
          Alcotest.test_case "option not truthy" `Quick test_option_not_truthy;
          Alcotest.test_case "arrow truthy" `Quick test_arrow_truthy;
          Alcotest.test_case "tuple truthy" `Quick test_tuple_truthy;
          Alcotest.test_case "forall truthy" `Quick test_forall_truthy;
          Alcotest.test_case "union truthy" `Quick test_union_truthy;
          Alcotest.test_case "tvar not truthy" `Quick test_tvar_truthy;
          Alcotest.test_case "linked tvar truthy" `Quick test_linked_tvar_truthy;
        ] );
      ( "option-validation",
        [
          Alcotest.test_case "Option String succeeds" `Quick
            test_option_of_checked_string;
          Alcotest.test_case "Option Int succeeds" `Quick
            test_option_of_checked_int;
          Alcotest.test_case "Option (list int) succeeds" `Quick
            test_option_of_checked_list;
          Alcotest.test_case "Option Nil fails" `Quick
            test_option_of_checked_nil_fails;
          Alcotest.test_case "Option Any fails" `Quick
            test_option_of_checked_any_fails;
          Alcotest.test_case "Option Bool fails" `Quick
            test_option_of_checked_bool_fails;
          Alcotest.test_case "Option (Option a) fails" `Quick
            test_option_of_checked_option_fails;
          Alcotest.test_case "validation error message" `Quick
            test_validation_error_message;
        ] );
      ( "row",
        [
          Alcotest.test_case "closed row" `Quick test_closed_row;
          Alcotest.test_case "open row" `Quick test_open_row;
          Alcotest.test_case "row lookup" `Quick test_row_lookup;
          Alcotest.test_case "row has field" `Quick test_row_has_field;
          Alcotest.test_case "is row" `Quick test_row_is_row;
          Alcotest.test_case "is open row" `Quick test_row_is_open;
          Alcotest.test_case "row truthy" `Quick test_row_truthy;
          Alcotest.test_case "row equal" `Quick test_row_equal;
        ] );
    ]
