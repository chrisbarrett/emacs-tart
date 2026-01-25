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
  Alcotest.(check bool) "different vars" true
    (to_string tv1 <> to_string tv2)

let test_tvar_level () =
  reset_tvar_counter ();
  match fresh_tvar 5 with
  | TVar tv ->
      Alcotest.(check (option int)) "level is 5" (Some 5) (tvar_level tv)
  | _ -> Alcotest.fail "expected TVar"

let test_tvar_id () =
  reset_tvar_counter ();
  match fresh_tvar 0 with
  | TVar tv ->
      Alcotest.(check (option int)) "id is 0" (Some 0) (tvar_id tv)
  | _ -> Alcotest.fail "expected TVar"

(* =============================================================================
   Union-Find / Path Compression Tests
   ============================================================================= *)

let test_repr_unbound () =
  reset_tvar_counter ();
  let tv = fresh_tvar 0 in
  (* repr of unbound var is itself *)
  Alcotest.(check bool) "repr returns same" true
    (repr tv == tv)

let test_repr_linked () =
  reset_tvar_counter ();
  let tv = fresh_tvar 0 in
  match tv with
  | TVar tvar_ref ->
      (* Link to Int *)
      tvar_ref := Link Prim.int;
      let result = repr tv in
      Alcotest.(check string) "repr follows link" "Int" (to_string result)
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
  Alcotest.(check string) "repr follows chain" "Int" (to_string (repr tv1));
  (* Path compression should update links *)
  (match tv1 with
   | TVar r1 ->
       (match !r1 with
        | Link t -> Alcotest.(check string) "path compressed" "Int" (to_string t)
        | _ -> Alcotest.fail "expected Link after repr")
   | _ -> Alcotest.fail "expected TVar")

(* =============================================================================
   Type Construction Tests
   ============================================================================= *)

let test_primitive_types () =
  Alcotest.(check string) "Int" "Int" (to_string Prim.int);
  Alcotest.(check string) "String" "String" (to_string Prim.string);
  Alcotest.(check string) "Nil" "Nil" (to_string Prim.nil);
  Alcotest.(check string) "T" "T" (to_string Prim.t);
  Alcotest.(check string) "Bool" "Bool" (to_string Prim.bool);
  Alcotest.(check string) "Any" "Any" (to_string Prim.any);
  Alcotest.(check string) "Truthy" "Truthy" (to_string Prim.truthy);
  Alcotest.(check string) "Never" "Never" (to_string Prim.never)

let test_list_type () =
  let ty = list_of Prim.int in
  Alcotest.(check string) "List Int" "(List Int)" (to_string ty)

let test_option_type () =
  let ty = option_of Prim.string in
  Alcotest.(check string) "Option String" "(Option String)" (to_string ty)

let test_pair_type () =
  let ty = pair_of Prim.string Prim.int in
  Alcotest.(check string) "Pair String Int" "(Pair String Int)" (to_string ty)

let test_nested_type () =
  let ty = list_of (option_of Prim.int) in
  Alcotest.(check string) "List (Option Int)" "(List (Option Int))" (to_string ty)

(* =============================================================================
   Function Type Tests
   ============================================================================= *)

let test_arrow_simple () =
  let ty = arrow [Prim.int; Prim.int] Prim.int in
  Alcotest.(check string) "Int -> Int -> Int" "(-> (Int Int) Int)" (to_string ty)

let test_arrow_no_args () =
  let ty = arrow [] Prim.int in
  Alcotest.(check string) "() -> Int" "(-> () Int)" (to_string ty)

let test_arrow_with_optional () =
  let ty = TArrow ([PPositional Prim.string; POptional (option_of Prim.int)], Prim.string) in
  Alcotest.(check string) "with optional"
    "(-> (String &optional (Option Int)) String)"
    (to_string ty)

let test_arrow_with_rest () =
  let ty = TArrow ([PRest Prim.string], Prim.string) in
  Alcotest.(check string) "with rest"
    "(-> (&rest String) String)"
    (to_string ty)

let test_arrow_with_key () =
  let ty = TArrow ([PKey (":name", Prim.string)], Prim.nil) in
  Alcotest.(check string) "with keyword"
    "(-> (&key :name String) Nil)"
    (to_string ty)

(* =============================================================================
   Polymorphic Type Tests
   ============================================================================= *)

let test_forall_identity () =
  let ty = forall ["a"] (arrow [TCon "a"] (TCon "a")) in
  Alcotest.(check string) "identity type"
    "(forall (a) (-> (a) a))"
    (to_string ty)

let test_forall_multi_vars () =
  let ty = forall ["a"; "b"] (arrow [TCon "a"; TCon "b"] (TCon "a")) in
  Alcotest.(check string) "multi-var forall"
    "(forall (a b) (-> (a b) a))"
    (to_string ty)

let test_forall_list_length () =
  let ty = forall ["a"] (arrow [list_of (TCon "a")] Prim.int) in
  Alcotest.(check string) "list length type"
    "(forall (a) (-> ((List a)) Int))"
    (to_string ty)

(* =============================================================================
   Union Type Tests
   ============================================================================= *)

let test_union_simple () =
  let ty = TUnion [Prim.int; Prim.string] in
  Alcotest.(check string) "Or Int String" "(Or Int String)" (to_string ty)

let test_tuple_type () =
  let ty = TTuple [Prim.string; Prim.int; Prim.bool] in
  Alcotest.(check string) "Tuple String Int Bool"
    "(Tuple String Int Bool)"
    (to_string ty)

(* =============================================================================
   Type Equality Tests
   ============================================================================= *)

let test_equal_primitives () =
  Alcotest.(check bool) "Int = Int" true (equal Prim.int Prim.int);
  Alcotest.(check bool) "Int <> String" false (equal Prim.int Prim.string)

let test_equal_tapp () =
  let t1 = list_of Prim.int in
  let t2 = list_of Prim.int in
  let t3 = list_of Prim.string in
  Alcotest.(check bool) "List Int = List Int" true (equal t1 t2);
  Alcotest.(check bool) "List Int <> List String" false (equal t1 t3)

let test_equal_arrow () =
  let t1 = arrow [Prim.int] Prim.int in
  let t2 = arrow [Prim.int] Prim.int in
  let t3 = arrow [Prim.string] Prim.int in
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
  (match tv with
   | TVar r -> r := Link Prim.int
   | _ -> ());
  Alcotest.(check bool) "linked tvar equals target" true (equal tv Prim.int)

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
    ]
