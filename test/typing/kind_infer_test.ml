(** Tests for kind inference *)

open Tart.Kind
open Tart.Kind_infer
module Read = Tart.Read
module Sig_parser = Tart.Sig_parser
module Sig_ast = Tart.Sig_ast

(** Helper to parse a declaration string into a signature *)
let parse_decl_to_sig src =
  let parse_result = Read.parse_string src in
  match parse_result.sexps with
  | [ sexp ] -> (
      match Sig_parser.parse_decl sexp with
      | Ok decl ->
          Ok
            {
              Sig_ast.sig_module = "test";
              sig_decls = [ decl ];
              sig_loc = Tart.Location.dummy_span;
            }
      | Error e -> Error e.message)
  | _ -> Error "Expected single expression"

(** Helper to parse a defun declaration from source text. *)
let parse_defun src =
  match parse_decl_to_sig src with
  | Ok sig_file -> (
      match sig_file.sig_decls with
      | [ Sig_ast.DDefun d ] -> d
      | _ -> failwith "Expected single defun declaration")
  | Error msg -> failwith ("Parse error: " ^ msg)

(** Helper to parse a type declaration from source text. *)
let parse_type_decl src =
  match parse_decl_to_sig src with
  | Ok sig_file -> (
      match sig_file.sig_decls with
      | [ Sig_ast.DType d ] -> d
      | _ -> failwith "Expected single type declaration")
  | Error msg -> failwith ("Parse error: " ^ msg)

(** Helper to parse a data declaration from source text. *)
let parse_data src =
  match parse_decl_to_sig src with
  | Ok sig_file -> (
      match sig_file.sig_decls with
      | [ Sig_ast.DData d ] -> d
      | _ -> failwith "Expected single data declaration")
  | Error msg -> failwith ("Parse error: " ^ msg)

(* =============================================================================
   Basic Kind Inference Tests
   ============================================================================= *)

let test_identity_kind () =
  (* (defun identity [a] (a) -> a)
     a is used only in concrete type positions, so a : * *)
  let d = parse_defun "(defun identity [a] (a) -> a)" in
  let result = infer_defun_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k = lookup_kind result "a" in
  Alcotest.(check string) "a has kind *" "*" (to_string k)

let test_map_kind () =
  (* (defun map [a b] (((a -> b)) (list a)) -> (list b))
     a and b are used as type arguments to list, so both a : * and b : * *)
  let d = parse_defun "(defun map [a b] (((a -> b)) (list a)) -> (list b))" in
  let result = infer_defun_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_a = lookup_kind result "a" in
  let k_b = lookup_kind result "b" in
  Alcotest.(check string) "a has kind *" "*" (to_string k_a);
  Alcotest.(check string) "b has kind *" "*" (to_string k_b)

let test_fmap_kind () =
  (* (defun fmap [f a b] (((a -> b)) (f a)) -> (f b))
     f is applied to one argument, so f : * -> *
     a and b are used as arguments to f, so a : * and b : * *)
  let d = parse_defun "(defun fmap [f a b] (((a -> b)) (f a)) -> (f b))" in
  let result = infer_defun_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_f = lookup_kind result "f" in
  let k_a = lookup_kind result "a" in
  let k_b = lookup_kind result "b" in
  Alcotest.(check string) "f has kind * -> *" "* -> *" (to_string k_f);
  Alcotest.(check string) "a has kind *" "*" (to_string k_a);
  Alcotest.(check string) "b has kind *" "*" (to_string k_b)

let test_bimap_kind () =
  (* (defun bimap [f a b c d] (((a -> b)) ((c -> d)) (f a c)) -> (f b d))
     f is applied to two arguments, so f : * -> * -> * *)
  let d =
    parse_defun
      "(defun bimap [f a b c d] (((a -> b)) ((c -> d)) (f a c)) -> (f b d))"
  in
  let result = infer_defun_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_f = lookup_kind result "f" in
  Alcotest.(check string) "f has kind * -> * -> *" "* -> * -> *" (to_string k_f)

let test_nested_application_kind () =
  (* (defun nested [f g a] ((f (g a))) -> (f (g a)))
     g : * -> *, f : * -> * *)
  let d = parse_defun "(defun nested [f g a] ((f (g a))) -> (f (g a)))" in
  let result = infer_defun_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_f = lookup_kind result "f" in
  let k_g = lookup_kind result "g" in
  let k_a = lookup_kind result "a" in
  Alcotest.(check string) "f has kind * -> *" "* -> *" (to_string k_f);
  Alcotest.(check string) "g has kind * -> *" "* -> *" (to_string k_g);
  Alcotest.(check string) "a has kind *" "*" (to_string k_a)

(* =============================================================================
   Type Declaration Kind Inference Tests
   ============================================================================= *)

let test_simple_type_alias_kind () =
  (* (type int-list (list int))
     No type parameters, nothing to infer *)
  let d = parse_type_decl "(type int-list (list int))" in
  let result = infer_type_decl_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors)

let test_parameterized_alias_kind () =
  (* (type result [a e] ((ok a) | (err e)))
     a and e are used in concrete positions, so both a : * and e : * *)
  let d = parse_type_decl "(type result [a e] ((ok a) | (err e)))" in
  let result = infer_type_decl_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_a = lookup_kind result "a" in
  let k_e = lookup_kind result "e" in
  Alcotest.(check string) "a has kind *" "*" (to_string k_a);
  Alcotest.(check string) "e has kind *" "*" (to_string k_e)

let test_hk_type_alias_kind () =
  (* (type wrapped [f a] (f a))
     f is applied to a, so f : * -> *, a : * *)
  let d = parse_type_decl "(type wrapped [f a] (f a))" in
  let result = infer_type_decl_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_f = lookup_kind result "f" in
  let k_a = lookup_kind result "a" in
  Alcotest.(check string) "f has kind * -> *" "* -> *" (to_string k_f);
  Alcotest.(check string) "a has kind *" "*" (to_string k_a)

let test_opaque_type_kind () =
  (* (type buffer)
     Opaque type, no body, nothing to infer *)
  let d = parse_type_decl "(type buffer)" in
  let result = infer_type_decl_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors)

let test_opaque_with_params_kind () =
  (* (type tagged [a])
     Opaque type with phantom parameter - a defaults to * *)
  let d = parse_type_decl "(type tagged [a])" in
  let result = infer_type_decl_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_a = lookup_kind result "a" in
  Alcotest.(check string) "a defaults to *" "*" (to_string k_a)

(* =============================================================================
   Data Declaration Kind Inference Tests
   ============================================================================= *)

let test_simple_data_kind () =
  (* (data result [a e] (Ok a) (Err e))
     a and e are used as constructor fields, so both a : * and e : * *)
  let d = parse_data "(data result [a e] (Ok a) (Err e))" in
  let result = infer_data_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_a = lookup_kind result "a" in
  let k_e = lookup_kind result "e" in
  Alcotest.(check string) "a has kind *" "*" (to_string k_a);
  Alcotest.(check string) "e has kind *" "*" (to_string k_e)

let test_hk_data_kind () =
  (* (data container [f a] (Container (f a)))
     f is applied to one argument, so f : * -> * *)
  let d = parse_data "(data container [f a] (Container (f a)))" in
  let result = infer_data_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_f = lookup_kind result "f" in
  let k_a = lookup_kind result "a" in
  Alcotest.(check string) "f has kind * -> *" "* -> *" (to_string k_f);
  Alcotest.(check string) "a has kind *" "*" (to_string k_a)

(* =============================================================================
   Unconstrained Variables Default to *
   ============================================================================= *)

let test_unconstrained_defaults_to_star () =
  (* (defun phantom [a] () -> int)
     a is not used anywhere, so it defaults to * *)
  let d = parse_defun "(defun phantom [a] () -> int)" in
  let result = infer_defun_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_a = lookup_kind result "a" in
  Alcotest.(check string) "unused a defaults to *" "*" (to_string k_a)

let test_multiple_unconstrained_default () =
  (* (defun multi-phantom [a b c] () -> int)
     All type variables are unused, so all default to * *)
  let d = parse_defun "(defun multi-phantom [a b c] () -> int)" in
  let result = infer_defun_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_a = lookup_kind result "a" in
  let k_b = lookup_kind result "b" in
  let k_c = lookup_kind result "c" in
  Alcotest.(check string) "a defaults to *" "*" (to_string k_a);
  Alcotest.(check string) "b defaults to *" "*" (to_string k_b);
  Alcotest.(check string) "c defaults to *" "*" (to_string k_c)

(* =============================================================================
   Kind Error Detection Tests (R3 from spec 17)
   ============================================================================= *)

let test_conflicting_kind_constraints () =
  (* (defun bad [a] ((a int)) -> (list a))
     a is used both as * -> * (applied to int) and as * (argument to list)
     This should produce a kind mismatch error *)
  let d = parse_defun "(defun bad [a] ((a int)) -> (list a))" in
  let result = infer_defun_kinds d in
  Alcotest.(check bool)
    "should have kind error" true
    (List.length result.errors > 0);
  let error_strings = List.map kind_error_to_string result.errors in
  let has_mismatch = List.exists (fun s -> String.length s > 0) error_strings in
  Alcotest.(check bool) "has error message" true has_mismatch

let test_nested_conflicting_kinds () =
  (* A type variable used at different arities in the same signature *)
  let d =
    parse_defun "(defun nested-conflict [f a] ((f a) (f a int)) -> int)"
  in
  let result = infer_defun_kinds d in
  (* f is applied to 1 arg in (f a) but 2 args in (f a int) - should error *)
  Alcotest.(check bool)
    "should have kind error for inconsistent arity" true
    (List.length result.errors > 0)

let test_valid_higher_kinded_type () =
  (* (defun fmap [f a b] (((a -> b)) (f a)) -> (f b))
     This is valid - f consistently has kind * -> * *)
  let d = parse_defun "(defun fmap [f a b] (((a -> b)) (f a)) -> (f b))" in
  let result = infer_defun_kinds d in
  Alcotest.(check (list string))
    "no errors for valid HKT" []
    (List.map kind_error_to_string result.errors)

(* =============================================================================
   Backward Compatibility Tests
   ============================================================================= *)

let test_existing_identity_unchanged () =
  (* R5: (defun identity [a] (a) -> a) ; a : * (unchanged behavior) *)
  let d = parse_defun "(defun identity [a] (a) -> a)" in
  let result = infer_defun_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k = lookup_kind result "a" in
  Alcotest.(check string) "identity's 'a' has kind *" "*" (to_string k)

let test_existing_compose_unchanged () =
  (* (defun compose [a b c] (((b -> c)) ((a -> b))) -> ((a -> c)))
     All type variables used in arrow types, all a : *, b : *, c : * *)
  let d =
    parse_defun "(defun compose [a b c] (((b -> c)) ((a -> b))) -> ((a -> c)))"
  in
  let result = infer_defun_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_a = lookup_kind result "a" in
  let k_b = lookup_kind result "b" in
  let k_c = lookup_kind result "c" in
  Alcotest.(check string) "a has kind *" "*" (to_string k_a);
  Alcotest.(check string) "b has kind *" "*" (to_string k_b);
  Alcotest.(check string) "c has kind *" "*" (to_string k_c)

(* =============================================================================
   Test Suite
   ============================================================================= *)

let basic_tests =
  [
    ("identity_kind", `Quick, test_identity_kind);
    ("map_kind", `Quick, test_map_kind);
    ("fmap_kind", `Quick, test_fmap_kind);
    ("bimap_kind", `Quick, test_bimap_kind);
    ("nested_application_kind", `Quick, test_nested_application_kind);
  ]

let type_decl_tests =
  [
    ("simple_type_alias_kind", `Quick, test_simple_type_alias_kind);
    ("parameterized_alias_kind", `Quick, test_parameterized_alias_kind);
    ("hk_type_alias_kind", `Quick, test_hk_type_alias_kind);
    ("opaque_type_kind", `Quick, test_opaque_type_kind);
    ("opaque_with_params_kind", `Quick, test_opaque_with_params_kind);
  ]

let data_tests =
  [
    ("simple_data_kind", `Quick, test_simple_data_kind);
    ("hk_data_kind", `Quick, test_hk_data_kind);
  ]

let default_tests =
  [
    ( "unconstrained_defaults_to_star",
      `Quick,
      test_unconstrained_defaults_to_star );
    ( "multiple_unconstrained_default",
      `Quick,
      test_multiple_unconstrained_default );
  ]

let backward_compat_tests =
  [
    ("existing_identity_unchanged", `Quick, test_existing_identity_unchanged);
    ("existing_compose_unchanged", `Quick, test_existing_compose_unchanged);
  ]

let kind_error_tests =
  [
    ("conflicting_kind_constraints", `Quick, test_conflicting_kind_constraints);
    ("nested_conflicting_kinds", `Quick, test_nested_conflicting_kinds);
    ("valid_higher_kinded_type", `Quick, test_valid_higher_kinded_type);
  ]

let () =
  Alcotest.run "Kind Inference"
    [
      ("basic", basic_tests);
      ("type_decl", type_decl_tests);
      ("data", data_tests);
      ("default", default_tests);
      ("backward_compat", backward_compat_tests);
      ("kind_errors", kind_error_tests);
    ]
