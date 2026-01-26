(** Tests for unification *)

open Tart.Types
module Unify = Tart.Unify
module C = Tart.Constraint

(** Dummy location for tests *)
let dummy_loc = Syntax.Location.dummy_span

(** Helper to reset state before each test *)
let setup () = reset_tvar_counter ()

(** Helper to create a fresh type variable at level 0 *)
let fresh () = fresh_tvar 0

(** Helper to unify and check success *)
let unify_ok t1 t2 =
  setup ();
  match Unify.unify t1 t2 dummy_loc with
  | Ok () -> true
  | Error e ->
      Printf.printf "Unify error: %s\n" (Unify.error_to_string e);
      false

(** Helper to check unification fails *)
let unify_fails t1 t2 =
  setup ();
  match Unify.unify t1 t2 dummy_loc with Ok () -> false | Error _ -> true

(* =============================================================================
   Basic Type Constant Tests
   ============================================================================= *)

let test_same_tcon () =
  Alcotest.(check bool) "Int = Int" true (unify_ok Prim.int Prim.int)

let test_different_tcon () =
  Alcotest.(check bool) "Int != String" true (unify_fails Prim.int Prim.string)

let test_nil_nil () =
  Alcotest.(check bool) "Nil = Nil" true (unify_ok Prim.nil Prim.nil)

let test_bool_bool () =
  Alcotest.(check bool) "Bool = Bool" true (unify_ok Prim.bool Prim.bool)

(* =============================================================================
   Type Variable Tests
   ============================================================================= *)

let test_tvar_unifies_with_tcon () =
  setup ();
  let tv = fresh () in
  Alcotest.(check bool) "'a = Int" true (unify_ok tv Prim.int);
  (* After unification, tv should resolve to Int *)
  Alcotest.(check string) "tv resolves to Int" "Int" (to_string tv)

let test_tvar_unifies_with_tvar () =
  setup ();
  let tv1 = fresh () in
  let tv2 = fresh () in
  Alcotest.(check bool) "'a = 'b" true (unify_ok tv1 tv2);
  (* Both should resolve to the same type *)
  let s1 = to_string tv1 in
  let s2 = to_string tv2 in
  Alcotest.(check string) "tv1 = tv2" s1 s2

let test_tvar_with_itself () =
  setup ();
  let tv = fresh () in
  Alcotest.(check bool) "'a = 'a" true (unify_ok tv tv)

let test_linked_tvar () =
  setup ();
  let tv1 = fresh () in
  let tv2 = fresh () in
  let tv3 = fresh () in
  (* tv1 = tv2, tv2 = Int -> all should be Int *)
  let _ = unify_ok tv1 tv2 in
  let _ = unify_ok tv2 Prim.int in
  let _ = unify_ok tv3 tv1 in
  Alcotest.(check string) "chained unification" "Int" (to_string tv3)

(* =============================================================================
   Occurs Check Tests
   ============================================================================= *)

let test_occurs_check_simple () =
  setup ();
  let tv = fresh () in
  (* tv = List tv should fail *)
  let list_tv = list_of tv in
  Alcotest.(check bool)
    "occurs check: 'a = List 'a" true (unify_fails tv list_tv)

let test_occurs_check_nested () =
  setup ();
  let tv = fresh () in
  (* tv = Option (List tv) should fail *)
  let nested = option_of (list_of tv) in
  Alcotest.(check bool) "occurs check nested" true (unify_fails tv nested)

let test_occurs_check_in_arrow () =
  setup ();
  let tv = fresh () in
  (* tv = (tv -> Int) should fail *)
  let fn_type = arrow [ tv ] Prim.int in
  Alcotest.(check bool) "occurs check in arrow" true (unify_fails tv fn_type)

let test_no_occurs_check_different_tvars () =
  setup ();
  let tv1 = fresh () in
  let tv2 = fresh () in
  (* tv1 = List tv2 should succeed *)
  let list_tv2 = list_of tv2 in
  Alcotest.(check bool) "'a = List 'b" true (unify_ok tv1 list_tv2)

(* =============================================================================
   Type Application Tests
   ============================================================================= *)

let test_list_int_list_int () =
  Alcotest.(check bool)
    "List Int = List Int" true
    (unify_ok (list_of Prim.int) (list_of Prim.int))

let test_list_int_list_string () =
  Alcotest.(check bool)
    "List Int != List String" true
    (unify_fails (list_of Prim.int) (list_of Prim.string))

let test_list_tvar_list_int () =
  setup ();
  let tv = fresh () in
  Alcotest.(check bool)
    "List 'a = List Int" true
    (unify_ok (list_of tv) (list_of Prim.int));
  Alcotest.(check string) "tv = Int" "Int" (to_string tv)

let test_option_string () =
  Alcotest.(check bool)
    "Option String = Option String" true
    (unify_ok (option_of Prim.string) (option_of Prim.string))

let test_different_constructors () =
  Alcotest.(check bool)
    "List Int != Option Int" true
    (unify_fails (list_of Prim.int) (option_of Prim.int))

let test_pair_unification () =
  setup ();
  let tv1 = fresh () in
  let tv2 = fresh () in
  let p1 = pair_of tv1 tv2 in
  let p2 = pair_of Prim.int Prim.string in
  Alcotest.(check bool) "Pair 'a 'b = Pair Int String" true (unify_ok p1 p2);
  Alcotest.(check string) "tv1 = Int" "Int" (to_string tv1);
  Alcotest.(check string) "tv2 = String" "String" (to_string tv2)

(* =============================================================================
   Arrow Type Tests
   ============================================================================= *)

let test_arrow_same () =
  let fn = arrow [ Prim.int ] Prim.string in
  Alcotest.(check bool)
    "(Int -> String) = (Int -> String)" true (unify_ok fn fn)

let test_arrow_different_return () =
  let fn1 = arrow [ Prim.int ] Prim.string in
  let fn2 = arrow [ Prim.int ] Prim.int in
  Alcotest.(check bool)
    "(Int -> String) != (Int -> Int)" true (unify_fails fn1 fn2)

let test_arrow_different_param () =
  let fn1 = arrow [ Prim.int ] Prim.string in
  let fn2 = arrow [ Prim.string ] Prim.string in
  Alcotest.(check bool)
    "(Int -> String) != (String -> String)" true (unify_fails fn1 fn2)

let test_arrow_different_arity () =
  let fn1 = arrow [ Prim.int ] Prim.string in
  let fn2 = arrow [ Prim.int; Prim.int ] Prim.string in
  Alcotest.(check bool)
    "(Int -> String) != (Int Int -> String)" true (unify_fails fn1 fn2)

let test_arrow_with_tvars () =
  setup ();
  let tv1 = fresh () in
  let tv2 = fresh () in
  let fn1 = arrow [ tv1 ] tv2 in
  let fn2 = arrow [ Prim.int ] Prim.string in
  Alcotest.(check bool) "('a -> 'b) = (Int -> String)" true (unify_ok fn1 fn2);
  Alcotest.(check string) "tv1 = Int" "Int" (to_string tv1);
  Alcotest.(check string) "tv2 = String" "String" (to_string tv2)

let test_arrow_nullary () =
  let fn1 = arrow [] Prim.int in
  let fn2 = arrow [] Prim.int in
  Alcotest.(check bool) "(() -> Int) = (() -> Int)" true (unify_ok fn1 fn2)

(* =============================================================================
   Constraint Solving Tests
   ============================================================================= *)

let test_solve_empty () =
  Alcotest.(check bool)
    "solve empty constraints" true
    (match Unify.solve [] with Ok () -> true | Error _ -> false)

let test_solve_single () =
  setup ();
  let tv = fresh () in
  let c = C.equal tv Prim.int dummy_loc in
  Alcotest.(check bool)
    "solve single constraint" true
    (match Unify.solve [ c ] with Ok () -> true | Error _ -> false);
  Alcotest.(check string) "tv = Int" "Int" (to_string tv)

let test_solve_multiple () =
  setup ();
  let tv1 = fresh () in
  let tv2 = fresh () in
  let c1 = C.equal tv1 Prim.int dummy_loc in
  let c2 = C.equal tv2 Prim.string dummy_loc in
  Alcotest.(check bool)
    "solve multiple constraints" true
    (match Unify.solve [ c1; c2 ] with Ok () -> true | Error _ -> false);
  Alcotest.(check string) "tv1 = Int" "Int" (to_string tv1);
  Alcotest.(check string) "tv2 = String" "String" (to_string tv2)

let test_solve_chained () =
  setup ();
  let tv1 = fresh () in
  let tv2 = fresh () in
  let c1 = C.equal tv1 tv2 dummy_loc in
  let c2 = C.equal tv2 Prim.int dummy_loc in
  Alcotest.(check bool)
    "solve chained constraints" true
    (match Unify.solve [ c1; c2 ] with Ok () -> true | Error _ -> false);
  Alcotest.(check string) "tv1 = Int" "Int" (to_string tv1)

let test_solve_conflicting () =
  setup ();
  let tv = fresh () in
  let c1 = C.equal tv Prim.int dummy_loc in
  let c2 = C.equal tv Prim.string dummy_loc in
  Alcotest.(check bool)
    "conflicting constraints fail" true
    (match Unify.solve [ c1; c2 ] with Ok () -> false | Error _ -> true)

(* =============================================================================
   Tuple Tests
   ============================================================================= *)

let test_tuple_same () =
  let t1 = TTuple [ Prim.int; Prim.string ] in
  let t2 = TTuple [ Prim.int; Prim.string ] in
  Alcotest.(check bool) "(Int, String) = (Int, String)" true (unify_ok t1 t2)

let test_tuple_different () =
  let t1 = TTuple [ Prim.int; Prim.string ] in
  let t2 = TTuple [ Prim.string; Prim.int ] in
  Alcotest.(check bool)
    "(Int, String) != (String, Int)" true (unify_fails t1 t2)

let test_tuple_different_length () =
  let t1 = TTuple [ Prim.int ] in
  let t2 = TTuple [ Prim.int; Prim.string ] in
  Alcotest.(check bool) "(Int) != (Int, String)" true (unify_fails t1 t2)

let test_tuple_with_tvars () =
  setup ();
  let tv = fresh () in
  let t1 = TTuple [ tv; Prim.string ] in
  let t2 = TTuple [ Prim.int; Prim.string ] in
  Alcotest.(check bool) "('a, String) = (Int, String)" true (unify_ok t1 t2);
  Alcotest.(check string) "tv = Int" "Int" (to_string tv)

(* =============================================================================
   Union Tests
   ============================================================================= *)

let test_union_same () =
  let t1 = TUnion [ Prim.int; Prim.string ] in
  let t2 = TUnion [ Prim.int; Prim.string ] in
  Alcotest.(check bool) "(Int | String) = (Int | String)" true (unify_ok t1 t2)

let test_union_different_order () =
  (* For now, unions must be structurally equal *)
  let t1 = TUnion [ Prim.int; Prim.string ] in
  let t2 = TUnion [ Prim.string; Prim.int ] in
  Alcotest.(check bool)
    "(Int | String) != (String | Int)" true (unify_fails t1 t2)

(* =============================================================================
   Complex Tests
   ============================================================================= *)

let test_identity_function () =
  setup ();
  let a = fresh () in
  let b = fresh () in
  (* Unify (a -> a) with (Int -> b) *)
  let fn1 = arrow [ a ] a in
  let fn2 = arrow [ Prim.int ] b in
  Alcotest.(check bool) "('a -> 'a) = (Int -> 'b)" true (unify_ok fn1 fn2);
  (* Both a and b should be Int *)
  Alcotest.(check string) "a = Int" "Int" (to_string a);
  Alcotest.(check string) "b = Int" "Int" (to_string b)

let test_map_function () =
  setup ();
  let a = fresh () in
  let b = fresh () in
  (* map: (a -> b) -> List a -> List b *)
  let map_type = arrow [ arrow [ a ] b; list_of a ] (list_of b) in
  let c = fresh () in
  let d = fresh () in
  let expected = arrow [ arrow [ c ] d; list_of c ] (list_of d) in
  Alcotest.(check bool) "map type unifies" true (unify_ok map_type expected)

let test_nested_application () =
  setup ();
  let a = fresh () in
  (* List (Option a) = List (Option Int) *)
  let t1 = list_of (option_of a) in
  let t2 = list_of (option_of Prim.int) in
  Alcotest.(check bool)
    "List (Option 'a) = List (Option Int)" true (unify_ok t1 t2);
  Alcotest.(check string) "a = Int" "Int" (to_string a)

(* =============================================================================
   solve_all Tests
   ============================================================================= *)

let test_solve_all_no_errors () =
  setup ();
  let tv = fresh () in
  let c = C.equal tv Prim.int dummy_loc in
  let errors = Unify.solve_all [ c ] in
  Alcotest.(check int) "no errors" 0 (List.length errors)

let test_solve_all_multiple_errors () =
  setup ();
  let c1 = C.equal Prim.int Prim.string dummy_loc in
  let c2 = C.equal Prim.float Prim.bool dummy_loc in
  let errors = Unify.solve_all [ c1; c2 ] in
  Alcotest.(check int) "two errors" 2 (List.length errors)

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "unify"
    [
      ( "type constants",
        [
          Alcotest.test_case "same TCon" `Quick test_same_tcon;
          Alcotest.test_case "different TCon" `Quick test_different_tcon;
          Alcotest.test_case "Nil = Nil" `Quick test_nil_nil;
          Alcotest.test_case "Bool = Bool" `Quick test_bool_bool;
        ] );
      ( "type variables",
        [
          Alcotest.test_case "tvar = TCon" `Quick test_tvar_unifies_with_tcon;
          Alcotest.test_case "tvar = tvar" `Quick test_tvar_unifies_with_tvar;
          Alcotest.test_case "tvar = itself" `Quick test_tvar_with_itself;
          Alcotest.test_case "linked tvar" `Quick test_linked_tvar;
        ] );
      ( "occurs check",
        [
          Alcotest.test_case "simple" `Quick test_occurs_check_simple;
          Alcotest.test_case "nested" `Quick test_occurs_check_nested;
          Alcotest.test_case "in arrow" `Quick test_occurs_check_in_arrow;
          Alcotest.test_case "different tvars ok" `Quick
            test_no_occurs_check_different_tvars;
        ] );
      ( "type applications",
        [
          Alcotest.test_case "List Int = List Int" `Quick test_list_int_list_int;
          Alcotest.test_case "List Int != List String" `Quick
            test_list_int_list_string;
          Alcotest.test_case "List 'a = List Int" `Quick test_list_tvar_list_int;
          Alcotest.test_case "Option String" `Quick test_option_string;
          Alcotest.test_case "different constructors" `Quick
            test_different_constructors;
          Alcotest.test_case "pair unification" `Quick test_pair_unification;
        ] );
      ( "arrow types",
        [
          Alcotest.test_case "same arrow" `Quick test_arrow_same;
          Alcotest.test_case "different return" `Quick
            test_arrow_different_return;
          Alcotest.test_case "different param" `Quick test_arrow_different_param;
          Alcotest.test_case "different arity" `Quick test_arrow_different_arity;
          Alcotest.test_case "with tvars" `Quick test_arrow_with_tvars;
          Alcotest.test_case "nullary" `Quick test_arrow_nullary;
        ] );
      ( "constraint solving",
        [
          Alcotest.test_case "empty" `Quick test_solve_empty;
          Alcotest.test_case "single" `Quick test_solve_single;
          Alcotest.test_case "multiple" `Quick test_solve_multiple;
          Alcotest.test_case "chained" `Quick test_solve_chained;
          Alcotest.test_case "conflicting" `Quick test_solve_conflicting;
        ] );
      ( "tuples",
        [
          Alcotest.test_case "same tuple" `Quick test_tuple_same;
          Alcotest.test_case "different tuple" `Quick test_tuple_different;
          Alcotest.test_case "different length" `Quick
            test_tuple_different_length;
          Alcotest.test_case "with tvars" `Quick test_tuple_with_tvars;
        ] );
      ( "unions",
        [
          Alcotest.test_case "same union" `Quick test_union_same;
          Alcotest.test_case "different order" `Quick test_union_different_order;
        ] );
      ( "complex",
        [
          Alcotest.test_case "identity function" `Quick test_identity_function;
          Alcotest.test_case "map function" `Quick test_map_function;
          Alcotest.test_case "nested application" `Quick test_nested_application;
        ] );
      ( "solve_all",
        [
          Alcotest.test_case "no errors" `Quick test_solve_all_no_errors;
          Alcotest.test_case "multiple errors" `Quick
            test_solve_all_multiple_errors;
        ] );
    ]
