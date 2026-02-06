(** Tests for type narrowing (Narrow.narrow_type) *)

open Tart.Types
module Narrow = Tart.Narrow

(** Helper to reset state before each test *)
let setup () = reset_tvar_counter ()

(* =============================================================================
   narrow_type: any ∩ T = T
   ============================================================================= *)

let test_any_narrows_to_string () =
  setup ();
  let result = Narrow.narrow_type Prim.any Prim.string in
  Alcotest.(check string) "any ∩ string = string" "string" (to_string result)

let test_any_narrows_to_int () =
  setup ();
  let result = Narrow.narrow_type Prim.any Prim.int in
  Alcotest.(check string) "any ∩ int = int" "int" (to_string result)

let test_any_narrows_to_nil () =
  setup ();
  let result = Narrow.narrow_type Prim.any Prim.nil in
  Alcotest.(check string) "any ∩ nil = nil" "nil" (to_string result)

(* =============================================================================
   narrow_type: T ∩ T = T
   ============================================================================= *)

let test_same_type () =
  setup ();
  let result = Narrow.narrow_type Prim.string Prim.string in
  Alcotest.(check string) "string ∩ string = string" "string" (to_string result)

let test_same_list () =
  setup ();
  let ty = list_of Prim.int in
  let result = Narrow.narrow_type ty ty in
  Alcotest.(check string)
    "(list int) ∩ (list int) = (list int)" "(list int)" (to_string result)

(* =============================================================================
   narrow_type: union filtering
   ============================================================================= *)

let test_union_filter_single_member () =
  setup ();
  let original = TUnion [ Prim.string; Prim.int; Prim.nil ] in
  let result = Narrow.narrow_type original Prim.string in
  Alcotest.(check string)
    "(string | int | nil) ∩ string = string" "string" (to_string result)

let test_union_filter_multiple_overlapping () =
  setup ();
  (* (int | float | string) ∩ num → (int | float) because int <: num, float <: num *)
  let original = TUnion [ Prim.int; Prim.float; Prim.string ] in
  let result = Narrow.narrow_type original Prim.num in
  Alcotest.(check string)
    "(int | float | string) ∩ num" "(Or int float)" (to_string result)

let test_union_truthy_filters_nil () =
  setup ();
  (* (string | nil) ∩ truthy = string *)
  let original = TUnion [ Prim.string; Prim.nil ] in
  let result = Narrow.narrow_type original Prim.truthy in
  Alcotest.(check string)
    "(string | nil) ∩ truthy = string" "string" (to_string result)

let test_union_truthy_filters_nil_multi () =
  setup ();
  (* (string | int | nil) ∩ truthy = (string | int) *)
  let original = TUnion [ Prim.string; Prim.int; Prim.nil ] in
  let result = Narrow.narrow_type original Prim.truthy in
  Alcotest.(check string)
    "(string | int | nil) ∩ truthy = (string | int)" "(Or string int)"
    (to_string result)

let test_union_empty_intersection () =
  setup ();
  (* (string | symbol) ∩ int = empty *)
  let original = TUnion [ Prim.string; Prim.symbol ] in
  let result = Narrow.narrow_type original Prim.int in
  Alcotest.(check string)
    "(string | symbol) ∩ int = empty" "(Or)" (to_string result)

let test_bool_truthy () =
  setup ();
  (* (t | nil) ∩ truthy = t *)
  let result = Narrow.narrow_type Prim.bool Prim.truthy in
  Alcotest.(check string) "bool ∩ truthy = t" "t" (to_string result)

(* =============================================================================
   narrow_type: non-union overlapping
   ============================================================================= *)

let test_nonunion_overlapping () =
  setup ();
  (* int ∩ num → int (int overlaps with num via subtype) *)
  let result = Narrow.narrow_type Prim.int Prim.num in
  Alcotest.(check string) "int ∩ num = int" "int" (to_string result)

let test_nonunion_disjoint_fallback () =
  setup ();
  (* string ∩ int → int (disjoint, fallback to target) *)
  let result = Narrow.narrow_type Prim.string Prim.int in
  Alcotest.(check string)
    "string ∩ int = int (fallback)" "int" (to_string result)

let test_truthy_nonunion () =
  setup ();
  (* string ∩ truthy → string (string is truthy) *)
  let result = Narrow.narrow_type Prim.string Prim.truthy in
  Alcotest.(check string) "string ∩ truthy = string" "string" (to_string result)

let test_nil_truthy () =
  setup ();
  (* nil ∩ truthy → truthy (nil is disjoint with truthy, fallback) *)
  let result = Narrow.narrow_type Prim.nil Prim.truthy in
  Alcotest.(check string)
    "nil ∩ truthy = truthy (fallback)" "truthy" (to_string result)

(* =============================================================================
   narrow_type: linked type variables
   ============================================================================= *)

let test_linked_tvar () =
  setup ();
  let tv = fresh_tvar 0 in
  (match tv with
  | TVar r -> r := Link (TUnion [ Prim.string; Prim.nil ])
  | _ -> ());
  let result = Narrow.narrow_type tv Prim.truthy in
  Alcotest.(check string)
    "linked tvar (string | nil) ∩ truthy = string" "string" (to_string result)

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "narrow"
    [
      ( "any intersection",
        [
          Alcotest.test_case "any ∩ string" `Quick test_any_narrows_to_string;
          Alcotest.test_case "any ∩ int" `Quick test_any_narrows_to_int;
          Alcotest.test_case "any ∩ nil" `Quick test_any_narrows_to_nil;
        ] );
      ( "same type",
        [
          Alcotest.test_case "T ∩ T" `Quick test_same_type;
          Alcotest.test_case "list ∩ list" `Quick test_same_list;
        ] );
      ( "union filtering",
        [
          Alcotest.test_case "single match" `Quick
            test_union_filter_single_member;
          Alcotest.test_case "multiple overlapping" `Quick
            test_union_filter_multiple_overlapping;
          Alcotest.test_case "truthy filters nil" `Quick
            test_union_truthy_filters_nil;
          Alcotest.test_case "truthy multi" `Quick
            test_union_truthy_filters_nil_multi;
          Alcotest.test_case "empty intersection" `Quick
            test_union_empty_intersection;
          Alcotest.test_case "bool ∩ truthy" `Quick test_bool_truthy;
        ] );
      ( "non-union",
        [
          Alcotest.test_case "overlapping (int ∩ num)" `Quick
            test_nonunion_overlapping;
          Alcotest.test_case "disjoint fallback" `Quick
            test_nonunion_disjoint_fallback;
          Alcotest.test_case "truthy non-union" `Quick test_truthy_nonunion;
          Alcotest.test_case "nil ∩ truthy" `Quick test_nil_truthy;
        ] );
      ( "linked tvars",
        [ Alcotest.test_case "linked tvar narrows" `Quick test_linked_tvar ] );
    ]
