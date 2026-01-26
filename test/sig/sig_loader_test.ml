(** Tests for signature loader/validator *)

open Sig
module Types = Core.Types
module Type_env = Core.Type_env
module Check = Typing.Check
module Unify = Typing.Unify

(** Helper to parse and validate a signature *)
let validate_str s =
  let parse_result = Syntax.Read.parse_string s in
  match Sig_parser.parse_signature ~module_name:"test" parse_result.sexps with
  | Error _ -> failwith "Parse error in test"
  | Ok sig_file -> Sig_loader.validate_signature sig_file

(** Helper to parse a signature string and load it into an environment *)
let load_sig_str ?(env = Type_env.empty) s =
  let parse_result = Syntax.Read.parse_string s in
  match Sig_parser.parse_signature ~module_name:"test" parse_result.sexps with
  | Error _ -> failwith "Parse error in test"
  | Ok sig_file ->
      match Sig_loader.validate_signature sig_file with
      | Error e -> failwith ("Validation error: " ^ e.message)
      | Ok () -> Sig_loader.load_signature env sig_file

(** Helper to parse and type-check an expression *)
let check_expr_str ~env s =
  Types.reset_tvar_counter ();
  match Syntax.Read.parse_one ~filename:"<test>" s with
  | Error msg -> failwith ("parse error: " ^ msg)
  | Ok sexp -> Check.check_expr ~env sexp

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

(** {1 End-to-End Signature Loading Tests (R5, R6)}

    These tests verify that loaded signatures are actually used by the
    type checker for calls and variable references. *)

(** Test that defun signatures are loaded and used for type checking calls.
    R5: "Verify: Signature loaded; type checker uses it for calls to foo" *)
let test_defun_signature_used_for_calls () =
  let sig_src = "(defun my-add (int int) -> int)" in
  let env = load_sig_str sig_src in
  (* Call with correct types should succeed *)
  let ty, errors = check_expr_str ~env "(my-add 1 2)" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string) "result type is Int" "Int" (Types.to_string ty)

(** Test that defun signature causes type error with wrong argument types.
    R5: Type checker uses loaded signature to detect type errors. *)
let test_defun_signature_type_error () =
  let sig_src = "(defun string-len (string) -> int)" in
  let env = load_sig_str sig_src in
  (* Call with wrong type should produce error *)
  let _, errors = check_expr_str ~env "(string-len 42)" in
  Alcotest.(check bool) "has type error" true (List.length errors > 0)

(** Test that polymorphic defun signatures work correctly.
    R5: Polymorphic functions can be instantiated at call sites. *)
let test_poly_defun_signature () =
  let sig_src = "(defun identity [a] (a) -> a)" in
  let env = load_sig_str sig_src in
  (* Call with int *)
  let ty1, errors1 = check_expr_str ~env "(identity 42)" in
  Alcotest.(check int) "no errors for int" 0 (List.length errors1);
  Alcotest.(check string) "returns Int" "Int" (Types.to_string ty1);
  (* Call with string *)
  let ty2, errors2 = check_expr_str ~env "(identity \"hello\")" in
  Alcotest.(check int) "no errors for string" 0 (List.length errors2);
  Alcotest.(check string) "returns String" "String" (Types.to_string ty2)

(** Test that defvar declarations are loaded and used for variable references.
    R6: "Verify: References to my-var have type string" *)
let test_defvar_type_used () =
  let sig_src = "(defvar my-config string)" in
  let env = load_sig_str sig_src in
  (* Variable reference should have declared type *)
  let ty, errors = check_expr_str ~env "my-config" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string) "variable type is String" "String" (Types.to_string ty)

(** Test that defvar with complex type works.
    R6: Function-typed variables are usable. *)
let test_defvar_function_type () =
  let sig_src = "(defvar my-handler ((string) -> int))" in
  let env = load_sig_str sig_src in
  (* Variable is a function, can be called *)
  let ty, errors = check_expr_str ~env "(my-handler \"test\")" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string) "result type is Int" "Int" (Types.to_string ty)

(** Test that multiple declarations can be loaded together.
    R5, R6: Both defun and defvar work in the same signature. *)
let test_combined_declarations () =
  let sig_src = {|
    (defvar debug-mode bool)
    (defun process (string) -> int)
  |} in
  let env = load_sig_str sig_src in
  (* Check defvar *)
  let ty1, errors1 = check_expr_str ~env "debug-mode" in
  Alcotest.(check int) "no errors for defvar" 0 (List.length errors1);
  Alcotest.(check string) "defvar type is Bool" "Bool" (Types.to_string ty1);
  (* Check defun *)
  let ty2, errors2 = check_expr_str ~env "(process \"input\")" in
  Alcotest.(check int) "no errors for defun" 0 (List.length errors2);
  Alcotest.(check string) "defun result is Int" "Int" (Types.to_string ty2)

(** {1 Type Alias Tests (R7, R8)}

    These tests verify that type aliases are expanded correctly during loading. *)

(** Test simple type alias expansion.
    R7: int-list expands to (list int) *)
let test_simple_type_alias () =
  let sig_src = {|
    (type int-list (list int))
    (defun sum (int-list) -> int)
  |} in
  let env = load_sig_str sig_src in
  (* Function parameter should accept (list int) *)
  let ty, errors = check_expr_str ~env "(sum (list 1 2 3))" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string) "result type is Int" "Int" (Types.to_string ty)

(** Test type alias used in return type.
    R7: Alias in return position expands correctly. *)
let test_type_alias_return () =
  let sig_src = {|
    (type int-list (list int))
    (defun make-ints () -> int-list)
  |} in
  let env = load_sig_str sig_src in
  let ty, errors = check_expr_str ~env "(make-ints)" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  (* Return type should be (List Int) after expansion *)
  Alcotest.(check string) "result is list int" "(List Int)" (Types.to_string ty)

(** Test parameterized type alias.
    R8: (result int string) expands with substitution. *)
let test_parameterized_type_alias () =
  let sig_src = {|
    (type result [a e] ((ok a) | (err e)))
    (defun parse (string) -> (result int string))
  |} in
  let env = load_sig_str sig_src in
  let ty, errors = check_expr_str ~env "(parse \"42\")" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  (* Should expand to union type (Or (ok Int) (err String)) *)
  Alcotest.(check string) "result is union" "(Or (ok Int) (err String))" (Types.to_string ty)

(** Test type alias in variable declaration.
    R7: defvar with alias type works. *)
let test_type_alias_defvar () =
  let sig_src = {|
    (type string-list (list string))
    (defvar names string-list)
  |} in
  let env = load_sig_str sig_src in
  let ty, errors = check_expr_str ~env "names" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string) "variable type is list string" "(List String)" (Types.to_string ty)

(** Test nested type alias expansion.
    R7, R8: Alias referencing another alias. *)
let test_nested_type_alias () =
  let sig_src = {|
    (type pair [a b] (tuple a b))
    (type int-pair (pair int int))
    (defun make-pair () -> int-pair)
  |} in
  let env = load_sig_str sig_src in
  let ty, errors = check_expr_str ~env "(make-pair)" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  (* int-pair -> (pair int int) -> (tuple int int) *)
  Alcotest.(check string) "result is tuple" "(Tuple Int Int)" (Types.to_string ty)

(** Test type alias with polymorphic function.
    R7, R8: Alias works within polymorphic signatures.
    Note: This test checks that alias expansion in return position works,
    but union types with polymorphic variables may not fully unify yet. *)
let test_type_alias_with_poly_fn () =
  let sig_src = {|
    (type wrapper [a] (list a))
    (defun wrap [a] (a) -> (wrapper a))
  |} in
  let env = load_sig_str sig_src in
  let ty, errors = check_expr_str ~env "(wrap 42)" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  (* (wrapper Int) expands to (List Int) *)
  Alcotest.(check string) "result is list int" "(List Int)" (Types.to_string ty)

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
      ( "end-to-end-loading",
        [
          Alcotest.test_case "defun signature used for calls" `Quick test_defun_signature_used_for_calls;
          Alcotest.test_case "defun signature type error" `Quick test_defun_signature_type_error;
          Alcotest.test_case "poly defun signature" `Quick test_poly_defun_signature;
          Alcotest.test_case "defvar type used" `Quick test_defvar_type_used;
          Alcotest.test_case "defvar function type" `Quick test_defvar_function_type;
          Alcotest.test_case "combined declarations" `Quick test_combined_declarations;
        ] );
      ( "type-aliases",
        [
          Alcotest.test_case "simple type alias" `Quick test_simple_type_alias;
          Alcotest.test_case "type alias return" `Quick test_type_alias_return;
          Alcotest.test_case "parameterized type alias" `Quick test_parameterized_type_alias;
          Alcotest.test_case "type alias defvar" `Quick test_type_alias_defvar;
          Alcotest.test_case "nested type alias" `Quick test_nested_type_alias;
          Alcotest.test_case "type alias with poly fn" `Quick test_type_alias_with_poly_fn;
        ] );
    ]
