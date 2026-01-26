(** Tests for signature loader/validator *)

open Sig

(** Helper to parse and validate a signature *)
let validate_str s =
  let parse_result = Syntax.Read.parse_string s in
  match Sig_parser.parse_signature ~module_name:"test" parse_result.sexps with
  | Error _ -> failwith "Parse error in test"
  | Ok sig_file -> Sig_loader.validate_signature sig_file

(** Helper to parse and validate, expecting an error containing a message *)
let expect_error_containing msg s =
  match validate_str s with
  | Ok () -> Alcotest.fail (Printf.sprintf "Expected error containing '%s' but got Ok" msg)
  | Error e ->
      if String.sub e.message 0 (min (String.length msg) (String.length e.message)) <> msg &&
         not (String.length e.message >= String.length msg &&
              String.sub e.message 0 (String.length msg) = msg) then
        (* Check if message contains the expected substring *)
        let found = try
          let _ = Str.search_forward (Str.regexp_string msg) e.message 0 in
          true
        with Not_found -> false
        in
        if not found then
          Alcotest.fail (Printf.sprintf "Expected error containing '%s' but got '%s'" msg e.message)

(** {1 Explicit Quantification Tests} *)

let test_bound_type_var_ok () =
  (* [a] (a) -> a is valid - a is bound *)
  let src = "(defun identity [a] (a) -> a)" in
  match validate_str src with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "Unexpected error: %s" e.message)

let test_unbound_type_var_error () =
  (* (a) -> a without quantifier is invalid *)
  let src = "(defun bad (a) -> a)" in
  expect_error_containing "Unbound type variable" src

let test_multiple_bound_vars_ok () =
  (* [a b] with both vars used is valid *)
  let src = "(defun pair [a b] (a b) -> (tuple a b))" in
  match validate_str src with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "Unexpected error: %s" e.message)

let test_partial_binding_error () =
  (* [a] but using unbound b *)
  let src = "(defun bad [a] (a) -> b)" in
  expect_error_containing "Unbound type variable" src

let test_primitive_not_var () =
  (* int is primitive, not a type variable *)
  let src = "(defun foo (int) -> string)" in
  match validate_str src with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "Unexpected error: %s" e.message)

let test_type_decl_defines_type () =
  (* Type declarations make names available *)
  let src = {|
    (type buffer)
    (defun get-buffer (string) -> buffer)
  |} in
  match validate_str src with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "Unexpected error: %s" e.message)

let test_unknown_type_constructor_allowed () =
  (* Unknown type constructors are allowed - they could be user-defined.
     Note: ((user-type int)) is a type application as parameter type *)
  let src = "(defun foo ((user-type int)) -> nil)" in
  match validate_str src with
  | Ok () -> ()  (* This is expected - unknown constructors are allowed *)
  | Error e -> Alcotest.fail (Printf.sprintf "Unexpected error: %s" e.message)

let test_defvar_validation () =
  (* defvar type must be valid *)
  let src = "(defvar my-var unbound-type)" in
  expect_error_containing "Unbound type variable" src

let test_type_params_in_scope () =
  (* Type parameters are in scope in body *)
  let src = "(type result [a e] ((ok a) | (err e)))" in
  match validate_str src with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "Unexpected error: %s" e.message)

let test_type_params_unbound_in_body () =
  (* Using unbound var in parameterized type body *)
  let src = "(type bad [a] (list b))" in
  expect_error_containing "Unbound type variable" src

let test_nested_forall_scoping () =
  (* Type variables from outer scope work in nested types *)
  let src = "(defun foo [a] ((list a)) -> (list a))" in
  match validate_str src with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "Unexpected error: %s" e.message)

(** {1 Bounded Quantifier Tests} *)

let test_bounded_quantifier_ok () =
  (* Bounded quantifier with valid bound *)
  let src = "(type option [(a : truthy)] (a | nil))" in
  match validate_str src with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "Unexpected error: %s" e.message)

(** {1 Validate All Errors Test} *)

let test_validate_all_multiple_errors () =
  let src = {|
    (defun bad1 (a) -> a)
    (defun bad2 (b) -> b)
  |} in
  let parse_result = Syntax.Read.parse_string src in
  match Sig_parser.parse_signature ~module_name:"test" parse_result.sexps with
  | Error _ -> Alcotest.fail "Parse error"
  | Ok sig_file ->
      let errors = Sig_loader.validate_signature_all sig_file in
      Alcotest.(check int) "should have 2 errors" 2 (List.length errors)

let () =
  Alcotest.run "sig_loader"
    [
      ( "explicit-quantification",
        [
          Alcotest.test_case "bound type var ok" `Quick test_bound_type_var_ok;
          Alcotest.test_case "unbound type var error" `Quick test_unbound_type_var_error;
          Alcotest.test_case "multiple bound vars ok" `Quick test_multiple_bound_vars_ok;
          Alcotest.test_case "partial binding error" `Quick test_partial_binding_error;
          Alcotest.test_case "primitive not var" `Quick test_primitive_not_var;
          Alcotest.test_case "type decl defines type" `Quick test_type_decl_defines_type;
          Alcotest.test_case "unknown type constructor allowed" `Quick test_unknown_type_constructor_allowed;
          Alcotest.test_case "defvar validation" `Quick test_defvar_validation;
          Alcotest.test_case "type params in scope" `Quick test_type_params_in_scope;
          Alcotest.test_case "type params unbound in body" `Quick test_type_params_unbound_in_body;
          Alcotest.test_case "nested forall scoping" `Quick test_nested_forall_scoping;
        ] );
      ( "bounded-quantifiers",
        [
          Alcotest.test_case "bounded quantifier ok" `Quick test_bounded_quantifier_ok;
        ] );
      ( "validate-all",
        [
          Alcotest.test_case "multiple errors" `Quick test_validate_all_multiple_errors;
        ] );
    ]
