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
   Explicit Kind Annotation Tests (R4 from spec 17)
   ============================================================================= *)

let test_explicit_kind_star () =
  (* (defun identity [(a : [*])] (a) -> a)
     Explicit kind annotation [*] should be respected *)
  let d = parse_defun "(defun identity [(a : *)] (a) -> a)" in
  let result = infer_defun_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k = lookup_kind result "a" in
  Alcotest.(check string) "a has explicit kind *" "*" (to_string k)

let test_explicit_kind_arrow () =
  (* (defun fmap [(f : [* -> *]) a b] (((a -> b)) (f a)) -> (f b))
     Explicit kind annotation [* -> *] for f, all in single vector *)
  let d =
    parse_defun "(defun fmap [(f : (* -> *)) a b] (((a -> b)) (f a)) -> (f b))"
  in
  let result = infer_defun_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_f = lookup_kind result "f" in
  let k_a = lookup_kind result "a" in
  Alcotest.(check string) "f has explicit kind * -> *" "* -> *" (to_string k_f);
  Alcotest.(check string) "a inferred as *" "*" (to_string k_a)

let test_explicit_kind_binary () =
  (* (defun bimap [(f : [* -> * -> *]) a b c d] (...) -> (...))
     Explicit kind annotation [* -> * -> *] for f *)
  let d =
    parse_defun
      "(defun bimap [(f : (* -> * -> *)) a b c d] (((a -> b)) ((c -> d)) (f a \
       c)) -> (f b d))"
  in
  let result = infer_defun_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_f = lookup_kind result "f" in
  Alcotest.(check string)
    "f has explicit kind * -> * -> *" "* -> * -> *" (to_string k_f)

let test_explicit_kind_constrains_inference () =
  (* (defun wrap [(f : [* -> *]) a] (a) -> (f a))
     Even though f is only used once, explicit annotation constrains it *)
  let d = parse_defun "(defun wrap [(f : (* -> *)) a] (a) -> (f a))" in
  let result = infer_defun_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_f = lookup_kind result "f" in
  Alcotest.(check string)
    "f has explicit kind even with minimal usage" "* -> *" (to_string k_f)

let test_explicit_kind_wrong_arity () =
  (* (defun bad [(f : [*]) a] ((f a)) -> int)
     f has explicit kind [*] but is applied to an argument - should error *)
  let d = parse_defun "(defun bad [(f : *) a] ((f a)) -> int)" in
  let result = infer_defun_kinds d in
  Alcotest.(check bool)
    "should have kind error" true
    (List.length result.errors > 0)

let test_explicit_kind_in_type_decl () =
  (* (type wrapped [(f : [* -> *]) a] (f a))
     Explicit kind annotation in type declaration *)
  let d = parse_type_decl "(type wrapped [(f : (* -> *)) a] (f a))" in
  let result = infer_type_decl_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_f = lookup_kind result "f" in
  Alcotest.(check string) "f has explicit kind * -> *" "* -> *" (to_string k_f)

let test_explicit_kind_in_data_decl () =
  (* (data container [(f : [* -> *]) a] (Container (f a)))
     Explicit kind annotation in data declaration *)
  let d = parse_data "(data container [(f : (* -> *)) a] (Container (f a)))" in
  let result = infer_data_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_f = lookup_kind result "f" in
  Alcotest.(check string) "f has explicit kind * -> *" "* -> *" (to_string k_f)

let test_mixed_explicit_and_inferred () =
  (* (defun traverse [(f : [* -> *]) a b] (((a -> (f b))) (list a)) -> (f (list b)))
     f has explicit kind, a and b are inferred *)
  let d =
    parse_defun
      "(defun traverse [(f : (* -> *)) a b] (((a -> (f b))) (list a)) -> (f \
       (list b)))"
  in
  let result = infer_defun_kinds d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_f = lookup_kind result "f" in
  let k_a = lookup_kind result "a" in
  let k_b = lookup_kind result "b" in
  Alcotest.(check string) "f has explicit kind * -> *" "* -> *" (to_string k_f);
  Alcotest.(check string) "a inferred as *" "*" (to_string k_a);
  Alcotest.(check string) "b inferred as *" "*" (to_string k_b)

(* =============================================================================
   Scope-Aware Kind Inference Tests (R4 from spec 19)
   ============================================================================= *)

(** Helper to build a scope kind environment from binder specifications. Takes a
    list of (name, kind_option) pairs. *)
let build_test_scope_env (binders : (string * string option) list) :
    Tart.Kind.env =
  List.fold_left
    (fun env (name, kind_opt) ->
      match kind_opt with
      | Some "* -> *" ->
          Tart.Kind.extend_env name
            (Tart.Kind.KConcrete (KArrow (KStar, KStar)))
            env
      | Some "* -> * -> *" ->
          Tart.Kind.extend_env name
            (Tart.Kind.KConcrete (KArrow (KStar, KArrow (KStar, KStar))))
            env
      | Some "*" | None ->
          Tart.Kind.extend_env name (Tart.Kind.KConcrete KStar) env
      | Some k -> failwith ("Unknown test kind: " ^ k))
    Tart.Kind.empty_env binders

let test_scope_kind_hk_variable () =
  (* Defun inside scope with HK scope variable:
     scope [(f : (* -> *))]
       (defun use-f [a] ((f a)) -> int)
     The scope's f should be available in the defun's kind environment *)
  let d = parse_defun "(defun use-f [a] ((f a)) -> int)" in
  let scope_env = build_test_scope_env [ ("f", Some "* -> *") ] in
  let result = infer_defun_kinds_with_scope scope_env d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_f = lookup_kind result "f" in
  let k_a = lookup_kind result "a" in
  Alcotest.(check string)
    "f has kind * -> * from scope" "* -> *" (to_string k_f);
  Alcotest.(check string) "a inferred as *" "*" (to_string k_a)

let test_scope_kind_hk_used_correctly () =
  (* Scope variable f : (* -> *) used consistently *)
  let d = parse_defun "(defun fmap-scope [a b] (((a -> b)) (f a)) -> (f b))" in
  let scope_env = build_test_scope_env [ ("f", Some "* -> *") ] in
  let result = infer_defun_kinds_with_scope scope_env d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors)

let test_scope_kind_hk_mismatch () =
  (* Scope declares f : [*], but defun uses it as (f a) expecting [* -> *] *)
  let d = parse_defun "(defun bad-f [a] ((f a)) -> int)" in
  let scope_env = build_test_scope_env [ ("f", Some "*") ] in
  let result = infer_defun_kinds_with_scope scope_env d in
  Alcotest.(check bool)
    "should have kind error" true
    (List.length result.errors > 0)

let test_scope_kind_combined_with_local () =
  (* Scope variable f : (* -> *), plus defun's own type var a *)
  let d = parse_defun "(defun pure-scope [a] (a) -> (f a))" in
  let scope_env = build_test_scope_env [ ("f", Some "* -> *") ] in
  let result = infer_defun_kinds_with_scope scope_env d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors);
  let k_f = lookup_kind result "f" in
  let k_a = lookup_kind result "a" in
  Alcotest.(check string) "f from scope" "* -> *" (to_string k_f);
  Alcotest.(check string) "a from defun" "*" (to_string k_a)

let test_scope_kind_binary_constructor () =
  (* Scope variable f : (* -> * -> *) for binary type constructor *)
  let d =
    parse_defun
      "(defun bimap-scope [a b c d] (((a -> b)) ((c -> d)) (f a c)) -> (f b d))"
  in
  let scope_env = build_test_scope_env [ ("f", Some "* -> * -> *") ] in
  let result = infer_defun_kinds_with_scope scope_env d in
  Alcotest.(check (list string))
    "no errors" []
    (List.map kind_error_to_string result.errors)

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

let explicit_kind_tests =
  [
    ("explicit_kind_star", `Quick, test_explicit_kind_star);
    ("explicit_kind_arrow", `Quick, test_explicit_kind_arrow);
    ("explicit_kind_binary", `Quick, test_explicit_kind_binary);
    ( "explicit_kind_constrains_inference",
      `Quick,
      test_explicit_kind_constrains_inference );
    ("explicit_kind_wrong_arity", `Quick, test_explicit_kind_wrong_arity);
    ("explicit_kind_in_type_decl", `Quick, test_explicit_kind_in_type_decl);
    ("explicit_kind_in_data_decl", `Quick, test_explicit_kind_in_data_decl);
    ("mixed_explicit_and_inferred", `Quick, test_mixed_explicit_and_inferred);
  ]

let scope_kind_tests =
  [
    ("scope_kind_hk_variable", `Quick, test_scope_kind_hk_variable);
    ("scope_kind_hk_used_correctly", `Quick, test_scope_kind_hk_used_correctly);
    ("scope_kind_hk_mismatch", `Quick, test_scope_kind_hk_mismatch);
    ( "scope_kind_combined_with_local",
      `Quick,
      test_scope_kind_combined_with_local );
    ("scope_kind_binary_constructor", `Quick, test_scope_kind_binary_constructor);
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
      ("explicit_kinds", explicit_kind_tests);
      ("scope_kinds", scope_kind_tests);
    ]
