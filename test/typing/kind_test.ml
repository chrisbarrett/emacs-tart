(** Tests for kind representation *)

open Tart.Kind

(* =============================================================================
   Kind Construction Tests
   ============================================================================= *)

let test_star () = Alcotest.(check string) "star to_string" "*" (to_string star)

let test_arrow () =
  let k = star @-> star in
  Alcotest.(check string) "arrow to_string" "* -> *" (to_string k)

let test_nested_arrow () =
  let k = star @-> star @-> star in
  Alcotest.(check string) "nested arrow" "* -> * -> *" (to_string k)

let test_arrow_left_parens () =
  let k = (star @-> star) @-> star in
  Alcotest.(check string) "left arrow parens" "(* -> *) -> *" (to_string k)

let test_arity_0 () =
  Alcotest.(check string) "arity 0" "*" (to_string (arity 0))

let test_arity_1 () =
  Alcotest.(check string) "arity 1" "* -> *" (to_string (arity 1))

let test_arity_2 () =
  Alcotest.(check string) "arity 2" "* -> * -> *" (to_string (arity 2))

let test_arity_3 () =
  Alcotest.(check string) "arity 3" "* -> * -> * -> *" (to_string (arity 3))

(* =============================================================================
   Kind Equality Tests
   ============================================================================= *)

let test_star_equal () =
  Alcotest.(check bool) "star equals star" true (equal_kind KStar KStar)

let test_arrow_equal () =
  let k1 = star @-> star in
  let k2 = star @-> star in
  Alcotest.(check bool) "arrow equals arrow" true (equal_kind k1 k2)

let test_different_arity_not_equal () =
  let k1 = star @-> star in
  let k2 = star @-> star @-> star in
  Alcotest.(check bool) "different arity not equal" false (equal_kind k1 k2)

let test_star_not_equal_arrow () =
  Alcotest.(check bool)
    "star not equal arrow" false
    (equal_kind KStar (star @-> star))

(* =============================================================================
   Kind Variable Tests
   ============================================================================= *)

let test_fresh_kvar () =
  reset_kvar_counter ();
  let kv1 = fresh_kvar () in
  let kv2 = fresh_kvar () in
  Alcotest.(check string) "fresh kvars different" "?k0" (scheme_to_string kv1);
  Alcotest.(check string) "fresh kvars different" "?k1" (scheme_to_string kv2)

let test_kvar_to_kind_none () =
  reset_kvar_counter ();
  let kv = fresh_kvar () in
  Alcotest.(check (option string))
    "unbound kvar has no kind" None
    (Option.map to_string (to_kind kv))

let test_default_to_star () =
  reset_kvar_counter ();
  let kv = fresh_kvar () in
  let k = default_to_star kv in
  Alcotest.(check string) "defaults to star" "*" (to_string k)

let test_default_to_star_concrete () =
  let ks = KConcrete (star @-> star) in
  let k = default_to_star ks in
  Alcotest.(check string) "concrete unchanged" "* -> *" (to_string k)

let test_concrete_scheme () =
  let ks = KConcrete (star @-> star @-> star) in
  Alcotest.(check string)
    "concrete scheme to_string" "* -> * -> *" (scheme_to_string ks)

(* =============================================================================
   Test Suite
   ============================================================================= *)

let construction_tests =
  [
    ("star", `Quick, test_star);
    ("arrow", `Quick, test_arrow);
    ("nested_arrow", `Quick, test_nested_arrow);
    ("arrow_left_parens", `Quick, test_arrow_left_parens);
    ("arity_0", `Quick, test_arity_0);
    ("arity_1", `Quick, test_arity_1);
    ("arity_2", `Quick, test_arity_2);
    ("arity_3", `Quick, test_arity_3);
  ]

let equality_tests =
  [
    ("star_equal", `Quick, test_star_equal);
    ("arrow_equal", `Quick, test_arrow_equal);
    ("different_arity_not_equal", `Quick, test_different_arity_not_equal);
    ("star_not_equal_arrow", `Quick, test_star_not_equal_arrow);
  ]

let kvar_tests =
  [
    ("fresh_kvar", `Quick, test_fresh_kvar);
    ("kvar_to_kind_none", `Quick, test_kvar_to_kind_none);
    ("default_to_star", `Quick, test_default_to_star);
    ("default_to_star_concrete", `Quick, test_default_to_star_concrete);
    ("concrete_scheme", `Quick, test_concrete_scheme);
  ]

(* =============================================================================
   Kind Environment Tests
   ============================================================================= *)

let test_empty_env () =
  reset_kvar_counter ();
  let env = empty_env in
  Alcotest.(check (list string)) "empty env has no names" [] (names env)

let test_extend_star () =
  reset_kvar_counter ();
  let env = extend_star "a" empty_env in
  let k = lookup_defaulted "a" env in
  Alcotest.(check string) "lookup returns star" "*" (to_string k)

let test_lookup_unknown_defaults_to_star () =
  (* R5: type variables used in concrete position default to kind * *)
  reset_kvar_counter ();
  let k = lookup_defaulted "unknown" empty_env in
  Alcotest.(check string) "unknown defaults to star" "*" (to_string k)

let test_extend_fresh () =
  reset_kvar_counter ();
  let env = extend_fresh [ "a"; "b" ] empty_env in
  Alcotest.(check (list string)) "has both names" [ "b"; "a" ] (names env)

let test_extend_env_with_arrow_kind () =
  reset_kvar_counter ();
  let env = extend_env "f" (KConcrete (star @-> star)) empty_env in
  let k = lookup_defaulted "f" env in
  Alcotest.(check string) "lookup returns arrow" "* -> *" (to_string k)

let test_default_all () =
  (* After kind inference, all unconstrained variables default to * *)
  reset_kvar_counter ();
  let env = extend_fresh [ "a"; "b" ] empty_env in
  default_all env;
  (* Now all should resolve to * *)
  let k_a = lookup_defaulted "a" env in
  let k_b = lookup_defaulted "b" env in
  Alcotest.(check string) "a defaults to star" "*" (to_string k_a);
  Alcotest.(check string) "b defaults to star" "*" (to_string k_b)

let test_lookup_preserves_concrete () =
  (* Concrete kinds are not affected by defaulting *)
  reset_kvar_counter ();
  let env = extend_env "f" (KConcrete (star @-> star @-> star)) empty_env in
  let k = lookup_defaulted "f" env in
  Alcotest.(check string) "concrete preserved" "* -> * -> *" (to_string k)

let test_existing_signature_unchanged () =
  (* R5: (defun identity [a] (a) -> a) ; a : * (unchanged behavior) *)
  reset_kvar_counter ();
  (* Simulate processing type variable 'a' from a signature *)
  let env = extend_fresh [ "a" ] empty_env in
  (* After "kind inference" (which doesn't constrain 'a'), default to * *)
  default_all env;
  let k = lookup_defaulted "a" env in
  Alcotest.(check string) "identity's 'a' has kind *" "*" (to_string k)

let env_tests =
  [
    ("empty_env", `Quick, test_empty_env);
    ("extend_star", `Quick, test_extend_star);
    ( "lookup_unknown_defaults_to_star",
      `Quick,
      test_lookup_unknown_defaults_to_star );
    ("extend_fresh", `Quick, test_extend_fresh);
    ("extend_env_with_arrow_kind", `Quick, test_extend_env_with_arrow_kind);
    ("default_all", `Quick, test_default_all);
    ("lookup_preserves_concrete", `Quick, test_lookup_preserves_concrete);
    ("existing_signature_unchanged", `Quick, test_existing_signature_unchanged);
  ]

let () =
  Alcotest.run "Kind"
    [
      ("construction", construction_tests);
      ("equality", equality_tests);
      ("kvar", kvar_tests);
      ("env", env_tests);
    ]
