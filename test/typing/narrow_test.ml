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

let test_nonunion_disjoint_empty () =
  setup ();
  (* string ∩ int → empty (disjoint types, contradiction) *)
  let result = Narrow.narrow_type Prim.string Prim.int in
  Alcotest.(check string)
    "string ∩ int = empty (disjoint)" "(Or)" (to_string result)

let test_truthy_nonunion () =
  setup ();
  (* string ∩ truthy → string (string is truthy) *)
  let result = Narrow.narrow_type Prim.string Prim.truthy in
  Alcotest.(check string) "string ∩ truthy = string" "string" (to_string result)

let test_nil_truthy () =
  setup ();
  (* nil ∩ truthy → empty (nil is disjoint with truthy, contradiction) *)
  let result = Narrow.narrow_type Prim.nil Prim.truthy in
  Alcotest.(check string)
    "nil ∩ truthy = empty (disjoint)" "(Or)" (to_string result)

(* =============================================================================
   narrow_type: union target (multi-type predicates)
   ============================================================================= *)

let test_union_intersect_partial_overlap () =
  setup ();
  (* (string | int | (list any)) ∩ ((list any) | (vector any) | string) →
     (string | (list any)) — only members overlapping with target union kept *)
  let original = TUnion [ Prim.string; Prim.int; list_of Prim.any ] in
  let target = TUnion [ list_of Prim.any; vector_of Prim.any; Prim.string ] in
  let result = Narrow.narrow_type original target in
  Alcotest.(check string)
    "partial overlap with union target" "(Or string (list (Or truthy nil)))"
    (to_string result)

let test_union_intersect_no_overlap () =
  setup ();
  (* (int | symbol) ∩ ((list any) | string) → empty *)
  let original = TUnion [ Prim.int; Prim.symbol ] in
  let target = TUnion [ list_of Prim.any; Prim.string ] in
  let result = Narrow.narrow_type original target in
  Alcotest.(check string)
    "no overlap with union target" "(Or)" (to_string result)

let test_union_intersect_full_overlap () =
  setup ();
  (* (string | int) ∩ (string | int | symbol) → (string | int) *)
  let original = TUnion [ Prim.string; Prim.int ] in
  let target = TUnion [ Prim.string; Prim.int; Prim.symbol ] in
  let result = Narrow.narrow_type original target in
  Alcotest.(check string)
    "full overlap with union target" "(Or string int)" (to_string result)

(* =============================================================================
   subtract_type: union subtrahend
   ============================================================================= *)

let test_subtract_union_from_union () =
  setup ();
  (* (string | int | (list any)) - ((list any) | (vector any) | string) → int *)
  let minuend = TUnion [ Prim.string; Prim.int; list_of Prim.any ] in
  let subtrahend =
    TUnion [ list_of Prim.any; vector_of Prim.any; Prim.string ]
  in
  let result = subtract_type minuend subtrahend in
  Alcotest.(check string) "subtract union from union" "int" (to_string result)

let test_subtract_union_single_type () =
  setup ();
  (* string - (string | int) → empty *)
  let result = subtract_type Prim.string (TUnion [ Prim.string; Prim.int ]) in
  Alcotest.(check string)
    "subtract union from single type" "(Or)" (to_string result)

let test_subtract_union_no_match () =
  setup ();
  (* (string | int) - ((list any) | symbol) → (string | int) *)
  let minuend = TUnion [ Prim.string; Prim.int ] in
  let subtrahend = TUnion [ list_of Prim.any; Prim.symbol ] in
  let result = subtract_type minuend subtrahend in
  Alcotest.(check string)
    "subtract union no match" "(Or string int)" (to_string result)

let test_subtract_single_not_in_union () =
  setup ();
  (* symbol - (string | int) → symbol *)
  let result = subtract_type Prim.symbol (TUnion [ Prim.string; Prim.int ]) in
  Alcotest.(check string)
    "single type not in union subtrahend" "symbol" (to_string result)

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
          Alcotest.test_case "disjoint empty" `Quick
            test_nonunion_disjoint_empty;
          Alcotest.test_case "truthy non-union" `Quick test_truthy_nonunion;
          Alcotest.test_case "nil ∩ truthy" `Quick test_nil_truthy;
        ] );
      ( "union target",
        [
          Alcotest.test_case "partial overlap" `Quick
            test_union_intersect_partial_overlap;
          Alcotest.test_case "no overlap" `Quick test_union_intersect_no_overlap;
          Alcotest.test_case "full overlap" `Quick
            test_union_intersect_full_overlap;
        ] );
      ( "subtract union",
        [
          Alcotest.test_case "union from union" `Quick
            test_subtract_union_from_union;
          Alcotest.test_case "union from single" `Quick
            test_subtract_union_single_type;
          Alcotest.test_case "union no match" `Quick
            test_subtract_union_no_match;
          Alcotest.test_case "single not in union" `Quick
            test_subtract_single_not_in_union;
        ] );
      ( "linked tvars",
        [ Alcotest.test_case "linked tvar narrows" `Quick test_linked_tvar ] );
    ]
