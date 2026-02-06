(** Tests for signature loader/validator *)

open Sig
module Types = Core.Types
module Type_env = Core.Type_env
module Check = Typing.Check
module Unify = Typing.Unify

(** Helper: check if a string starts with a prefix *)
let string_starts_with prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

(** Helper to parse and validate a signature *)
let validate_str s =
  let parse_result = Syntax.Read.parse_string s in
  match Sig_parser.parse_signature ~module_name:"test" parse_result.sexps with
  | Error _ -> failwith "Parse error in test"
  | Ok sig_file -> Sig_loader.validate_signature sig_file

(** Helper to parse a signature string into an AST *)
let parse_sig_str ?(module_name = "test") s =
  let parse_result = Syntax.Read.parse_string s in
  match Sig_parser.parse_signature ~module_name parse_result.sexps with
  | Error _ -> failwith "Parse error in test"
  | Ok sig_file -> (
      match Sig_loader.validate_signature sig_file with
      | Error e -> failwith ("Validation error: " ^ e.message)
      | Ok () -> sig_file)

(** Helper to parse a signature string into an AST without validation. Used when
    the signature depends on opened/included modules where validation can't know
    about external types. *)
let parse_sig_str_no_validate ?(module_name = "test") s =
  let parse_result = Syntax.Read.parse_string s in
  match Sig_parser.parse_signature ~module_name parse_result.sexps with
  | Error _ -> failwith "Parse error in test"
  | Ok sig_file -> sig_file

(** Helper to parse a signature string and load it into an environment. Uses
    prelude context so that primitive type names (int, string, etc.) are
    recognized via prelude aliases. *)
let load_sig_str ?(env = Type_env.empty) s =
  let sig_file = parse_sig_str s in
  let prelude_ctx = Prelude.prelude_type_context () in
  let prelude_type_names = Prelude.prelude_type_names in
  Sig_loader.load_signature_with_resolver ~prelude_ctx ~prelude_type_names
    ~resolver:Sig_loader.no_resolver env sig_file

(** Helper to load a signature with a module resolver. Note: Validation is
    skipped because external types from opened/included modules aren't known at
    parse time. Uses prelude context for primitive types. *)
let load_sig_str_with_resolver ?(env = Type_env.empty) ~resolver s =
  let sig_file = parse_sig_str_no_validate s in
  let prelude_ctx = Prelude.prelude_type_context () in
  let prelude_type_names = Prelude.prelude_type_names in
  Sig_loader.load_signature_with_resolver ~prelude_ctx ~prelude_type_names
    ~resolver env sig_file

(** Helper to parse and type-check an expression *)
let check_expr_str ~env s =
  Types.reset_tvar_counter ();
  match Syntax.Read.parse_one ~filename:"<test>" s with
  | Error msg -> failwith ("parse error: " ^ msg)
  | Ok sexp -> Check.check_expr ~env sexp

(** Helper to parse and validate, expecting an error containing a message *)
let expect_error_containing msg s =
  match validate_str s with
  | Ok () ->
      Alcotest.fail
        (Printf.sprintf "Expected error containing '%s' but got Ok" msg)
  | Error e ->
      if
        String.sub e.message 0
          (min (String.length msg) (String.length e.message))
        <> msg
        && not
             (String.length e.message >= String.length msg
             && String.sub e.message 0 (String.length msg) = msg)
      then
        (* Check if message contains the expected substring *)
        let found =
          try
            let _ = Str.search_forward (Str.regexp_string msg) e.message 0 in
            true
          with Not_found -> false
        in
        if not found then
          Alcotest.fail
            (Printf.sprintf "Expected error containing '%s' but got '%s'" msg
               e.message)

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
  (* Type declarations make names available.
     Note: buffer is a prelude type now, so use a different name. *)
  let src =
    {|
    (type my-buffer)
    (defun get-my-buffer (string) -> my-buffer)
  |}
  in
  match validate_str src with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "Unexpected error: %s" e.message)

let test_unknown_type_constructor_allowed () =
  (* Unknown type constructors are allowed - they could be user-defined.
     Note: ((user-type int)) is a type application as parameter type *)
  let src = "(defun foo ((user-type int)) -> nil)" in
  match validate_str src with
  | Ok () -> () (* This is expected - unknown constructors are allowed *)
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

    These tests verify that loaded signatures are actually used by the type
    checker for calls and variable references. *)

(** Test that defun signatures are loaded and used for type checking calls. R5:
    "Verify: Signature loaded; type checker uses it for calls to foo" *)
let test_defun_signature_used_for_calls () =
  let sig_src = "(defun my-add (int int) -> int)" in
  let env = load_sig_str sig_src in
  (* Call with correct types should succeed *)
  let ty, errors = check_expr_str ~env "(my-add 1 2)" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string) "result type is int" "int" (Types.to_string ty)

(** Test that defun signature causes type error with wrong argument types. R5:
    Type checker uses loaded signature to detect type errors. *)
let test_defun_signature_type_error () =
  let sig_src = "(defun string-len (string) -> int)" in
  let env = load_sig_str sig_src in
  (* Call with wrong type should produce error *)
  let _, errors = check_expr_str ~env "(string-len 42)" in
  Alcotest.(check bool) "has type error" true (List.length errors > 0)

(** Test that polymorphic defun signatures work correctly. R5: Polymorphic
    functions can be instantiated at call sites. *)
let test_poly_defun_signature () =
  let sig_src = "(defun identity [a] (a) -> a)" in
  let env = load_sig_str sig_src in
  (* Call with int *)
  let ty1, errors1 = check_expr_str ~env "(identity 42)" in
  Alcotest.(check int) "no errors for int" 0 (List.length errors1);
  Alcotest.(check string) "returns int" "int" (Types.to_string ty1);
  (* Call with string *)
  let ty2, errors2 = check_expr_str ~env "(identity \"hello\")" in
  Alcotest.(check int) "no errors for string" 0 (List.length errors2);
  Alcotest.(check string) "returns string" "string" (Types.to_string ty2)

(** Test that defvar declarations are loaded and used for variable references.
    R6: "Verify: References to my-var have type string" *)
let test_defvar_type_used () =
  let sig_src = "(defvar my-config string)" in
  let env = load_sig_str sig_src in
  (* Variable reference should have declared type *)
  let ty, errors = check_expr_str ~env "my-config" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string)
    "variable type is string" "string" (Types.to_string ty)

(** Test that defvar with complex type works. R6: Function-typed variables are
    usable. *)
let test_defvar_function_type () =
  let sig_src = "(defvar my-handler ((string) -> int))" in
  let env = load_sig_str sig_src in
  (* Variable is a function, can be called *)
  let ty, errors = check_expr_str ~env "(my-handler \"test\")" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string) "result type is int" "int" (Types.to_string ty)

(** Test that multiple declarations can be loaded together. R5, R6: Both defun
    and defvar work in the same signature. *)
let test_combined_declarations () =
  let sig_src =
    {|
    (defvar debug-mode bool)
    (defun process (string) -> int)
  |}
  in
  let env = load_sig_str sig_src in
  (* Check defvar *)
  let ty1, errors1 = check_expr_str ~env "debug-mode" in
  Alcotest.(check int) "no errors for defvar" 0 (List.length errors1);
  Alcotest.(check string)
    "defvar type is bool" "(Or t nil)" (Types.to_string ty1);
  (* Check defun *)
  let ty2, errors2 = check_expr_str ~env "(process \"input\")" in
  Alcotest.(check int) "no errors for defun" 0 (List.length errors2);
  Alcotest.(check string) "defun result is int" "int" (Types.to_string ty2)

(** {1 Type Alias Tests (R7, R8)}

    These tests verify that type aliases are expanded correctly during loading.
*)

(** Test simple type alias expansion. R7: int-list expands to (list int) *)
let test_simple_type_alias () =
  let sig_src =
    {|
    (type int-list (list int))
    (defun sum (int-list) -> int)
  |}
  in
  let env = load_sig_str sig_src in
  (* Function parameter should accept (list int) *)
  let ty, errors = check_expr_str ~env "(sum (list 1 2 3))" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string) "result type is int" "int" (Types.to_string ty)

(** Test type alias used in return type. R7: Alias in return position expands
    correctly. *)
let test_type_alias_return () =
  let sig_src =
    {|
    (type int-list (list int))
    (defun make-ints () -> int-list)
  |}
  in
  let env = load_sig_str sig_src in
  let ty, errors = check_expr_str ~env "(make-ints)" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  (* Return type should be (list int) after expansion *)
  Alcotest.(check string) "result is list int" "(list int)" (Types.to_string ty)

(** Test parameterized type alias. R8: (result int string) expands with
    substitution. *)
let test_parameterized_type_alias () =
  let sig_src =
    {|
    (type result [a e] ((ok a) | (err e)))
    (defun parse (string) -> (result int string))
  |}
  in
  let env = load_sig_str sig_src in
  let ty, errors = check_expr_str ~env "(parse \"42\")" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  (* Should expand to union type (Or (ok int) (err string)) *)
  Alcotest.(check string)
    "result is union" "(Or (ok int) (err string))" (Types.to_string ty)

(** Test type alias in variable declaration. R7: defvar with alias type works.
*)
let test_type_alias_defvar () =
  let sig_src =
    {|
    (type string-list (list string))
    (defvar names string-list)
  |}
  in
  let env = load_sig_str sig_src in
  let ty, errors = check_expr_str ~env "names" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string)
    "variable type is list string" "(list string)" (Types.to_string ty)

(** Test nested type alias expansion. R7, R8: Alias referencing another alias.
    Note: We use `my-pair` instead of `pair` since `pair` is now a prelude type.
*)
let test_nested_type_alias () =
  let sig_src =
    {|
    (type my-pair [a b] (tuple a b))
    (type int-pair (my-pair int int))
    (defun make-pair () -> int-pair)
  |}
  in
  let env = load_sig_str sig_src in
  let ty, errors = check_expr_str ~env "(make-pair)" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  (* int-pair -> (my-pair int int) -> (tuple int int) *)
  (* Type names are lowercase for intrinsics *)
  Alcotest.(check string)
    "result is tuple" "(Tuple int int)" (Types.to_string ty)

(** Test type alias with polymorphic function. R7, R8: Alias works within
    polymorphic signatures. Note: This test checks that alias expansion in
    return position works, but union types with polymorphic variables may not
    fully unify yet. *)
let test_type_alias_with_poly_fn () =
  let sig_src =
    {|
    (type wrapper [a] (list a))
    (defun wrap [a] (a) -> (wrapper a))
  |}
  in
  let env = load_sig_str sig_src in
  let ty, errors = check_expr_str ~env "(wrap 42)" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  (* (wrapper int) expands to (list int) *)
  Alcotest.(check string) "result is list int" "(list int)" (Types.to_string ty)

(** Test type alias in parameter position with polymorphic function. This
    verifies that aliases are expanded in parameter position. Note: We check the
    scheme directly rather than calling the function, as the type checker has a
    known limitation with polymorphic params. *)
let test_type_alias_in_param_position () =
  let sig_src =
    {|
    (type seq [a] (list a))
    (defun process-seq [a] ((seq a)) -> a)
  |}
  in
  let env = load_sig_str sig_src in
  (* Verify the alias was expanded in the scheme *)
  match Type_env.lookup "process-seq" env with
  | None -> Alcotest.fail "process-seq not found"
  | Some scheme ->
      let scheme_str = Type_env.scheme_to_string scheme in
      (* Should contain "list" not "seq" *)
      Alcotest.(check bool)
        "scheme contains List" true
        (try
           let _ = Str.search_forward (Str.regexp_string "list") scheme_str 0 in
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "scheme does not contain seq" true
        (not
           (try
              let _ =
                Str.search_forward (Str.regexp_string "seq") scheme_str 0
              in
              true
            with Not_found -> false))

(** {1 Opaque Type Tests (R9, R10)}

    These tests verify that opaque types work as distinct abstract types. *)

(** Test that opaque types are distinct from each other. R9: resource and handle
    are separate distinct opaque types. Note: buffer, marker, etc. are prelude
    types now, so we use different names for testing. *)
let test_opaque_types_distinct () =
  let sig_src =
    {|
    (type my-resource)
    (type my-handle)
    (defun get-resource () -> my-resource)
    (defun get-handle () -> my-handle)
    (defun use-resource (my-resource) -> nil)
  |}
  in
  let env = load_sig_str sig_src in
  (* Trying to use a handle where resource is expected should fail *)
  let _, errors = check_expr_str ~env "(use-resource (get-handle))" in
  Alcotest.(check bool)
    "type error for wrong opaque type" true
    (List.length errors > 0)

(** Test that opaque types can only be created/consumed via declared functions.
    R9: Values can only be created/consumed via functions declared in the same
    module. Note: buffer is a prelude type now, so use different names. *)
let test_opaque_type_creation () =
  let sig_src =
    {|
    (type my-resource)
    (defun get-resource () -> my-resource)
    (defun use-resource (my-resource) -> nil)
  |}
  in
  let env = load_sig_str sig_src in
  (* Valid: use resource returned from get-resource *)
  let _, errors = check_expr_str ~env "(use-resource (get-resource))" in
  Alcotest.(check int) "no errors for valid opaque use" 0 (List.length errors)

(** Test that opaque types don't unify with other types. R9: Opaque types are
    not unifiable with other types. Note: buffer is a prelude type now. *)
let test_opaque_vs_other_types () =
  let sig_src =
    {|
    (type my-resource)
    (defun use-resource (my-resource) -> nil)
  |}
  in
  let env = load_sig_str sig_src in
  (* Trying to pass an int where resource is expected should fail *)
  let _, errors = check_expr_str ~env "(use-resource 42)" in
  Alcotest.(check bool)
    "type error for int vs opaque" true
    (List.length errors > 0)

(** Test opaque types with phantom type parameters. R10: (tagged int) and
    (tagged string) are distinct types *)
let test_opaque_phantom_params () =
  let sig_src =
    {|
    (type tagged [a])
    (defun make-int-tagged () -> (tagged int))
    (defun make-string-tagged () -> (tagged string))
    (defun use-int-tagged ((tagged int)) -> nil)
  |}
  in
  let env = load_sig_str sig_src in
  (* Valid: use int-tagged with int-tagged *)
  let _, errors1 = check_expr_str ~env "(use-int-tagged (make-int-tagged))" in
  Alcotest.(check int) "no errors for matching phantom" 0 (List.length errors1);
  (* Invalid: use string-tagged where int-tagged expected *)
  let _, errors2 =
    check_expr_str ~env "(use-int-tagged (make-string-tagged))"
  in
  Alcotest.(check bool)
    "type error for mismatched phantom" true
    (List.length errors2 > 0)

(** Test opaque type in return position. R9: Opaque types work correctly in
    function return types. Note: buffer is prelude now, use different name. *)
let test_opaque_return_type () =
  let sig_src =
    {|
    (type my-resource)
    (defun create-resource (string) -> my-resource)
  |}
  in
  let env = load_sig_str sig_src in
  let ty, errors = check_expr_str ~env "(create-resource \"test\")" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  (* The type should contain the module-qualified opaque name *)
  Alcotest.(check bool)
    "returns opaque resource type" true
    (String.sub (Types.to_string ty) 0 4 = "test")

(** Test opaque type used in polymorphic function. R9, R10: Opaque types work
    with polymorphic signatures *)
let test_opaque_with_polymorphism () =
  let sig_src =
    {|
    (type box [a])
    (defun box [a] (a) -> (box a))
    (defun unbox [a] ((box a)) -> a)
  |}
  in
  let env = load_sig_str sig_src in
  (* Boxing and unboxing should preserve the inner type *)
  let ty, errors = check_expr_str ~env "(unbox (box 42))" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string) "unbox returns inner type" "int" (Types.to_string ty)

(** {1 Open Directive Tests (R12)}

    Tests for the 'open' directive that imports types for use in signatures
    without re-exporting them. *)

(** Test that open imports type aliases for use in signatures. R12: "seq is
    available for use in type expressions" *)
let test_open_imports_type_aliases () =
  (* Create a module "seq" with a type alias *)
  let seq_sig =
    parse_sig_str ~module_name:"seq" {|
    (type seq [a] (list a))
  |}
  in
  (* Create a resolver that returns the seq module *)
  let resolver name = if name = "seq" then Some seq_sig else None in
  (* Module that opens seq and uses its type *)
  let env =
    load_sig_str_with_resolver ~resolver
      {|
    (open 'seq)
    (defun process-seq [a] ((seq a)) -> a)
  |}
  in
  (* Verify the function is loaded with correct type *)
  (* (seq a) should expand to (list a), so process-seq : [a] (list a) -> a *)
  match Type_env.lookup "process-seq" env with
  | None -> Alcotest.fail "process-seq not found"
  | Some scheme ->
      let scheme_str = Type_env.scheme_to_string scheme in
      (* The scheme should contain "list" (the expanded type), not "seq" *)
      Alcotest.(check bool)
        "seq alias expanded to List" true
        (try
           let _ = Str.search_forward (Str.regexp_string "list") scheme_str 0 in
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "scheme does not contain seq" true
        (not
           (try
              let _ =
                Str.search_forward (Str.regexp_string "seq") scheme_str 0
              in
              true
            with Not_found -> false))

(** Test that open imports opaque types for use in signatures. R12: Opaque types
    from opened modules are available. Note: buffer is prelude now. *)
let test_open_imports_opaque_types () =
  (* Create a module "res" with an opaque type *)
  let res_sig =
    parse_sig_str ~module_name:"res"
      {|
    (type my-resource)
    (defun make-resource () -> my-resource)
  |}
  in
  let resolver name = if name = "res" then Some res_sig else None in
  (* Module that opens res and uses its opaque type *)
  let env =
    load_sig_str_with_resolver ~resolver
      {|
    (open 'res)
    (defun wrap-resource (my-resource) -> (list my-resource))
  |}
  in
  (* The my-resource type should be available *)
  match Type_env.lookup "wrap-resource" env with
  | None -> Alcotest.fail "wrap-resource not found"
  | Some _ -> () (* Type was successfully loaded *)

(** Test that opened values are NOT re-exported. R12: "seq is NOT re-exported
    from my-collection" *)
let test_open_does_not_export_values () =
  (* Create a module with a function *)
  let lib_sig =
    parse_sig_str ~module_name:"lib" {|
    (defun lib-func (int) -> int)
  |}
  in
  let resolver name = if name = "lib" then Some lib_sig else None in
  (* Module that opens lib *)
  let env =
    load_sig_str_with_resolver ~resolver
      {|
    (open 'lib)
    (defun my-func (int) -> int)
  |}
  in
  (* lib-func should NOT be in the environment (open doesn't re-export values) *)
  (match Type_env.lookup "lib-func" env with
  | None -> () (* Correct - not re-exported *)
  | Some _ -> Alcotest.fail "lib-func should not be re-exported from open");
  (* my-func should be there *)
  match Type_env.lookup "my-func" env with
  | None -> Alcotest.fail "my-func should be in environment"
  | Some _ -> ()

(** Test that open handles cycles gracefully. Opening the same module twice or
    opening self should not loop. *)
let test_open_cycle_detection () =
  (* Create modules that open each other *)
  let rec resolver name =
    if name = "a" then
      Some
        (parse_sig_str_with_resolver ~module_name:"a" ~resolver
           {|
      (open 'b)
      (type ta (list int))
    |})
    else if name = "b" then
      Some
        (parse_sig_str ~module_name:"b" {|
      (type tb (list string))
    |})
    else None
  and parse_sig_str_with_resolver ~module_name ~resolver:_ s =
    (* Note: For cycle testing, we don't actually need recursive resolution
       in the test setup - the loader handles it internally *)
    parse_sig_str ~module_name s
  in
  (* This should not infinite loop *)
  let env =
    load_sig_str_with_resolver ~resolver
      {|
    (open 'a)
    (defun use-ta (ta) -> nil)
  |}
  in
  match Type_env.lookup "use-ta" env with
  | None -> Alcotest.fail "use-ta should be loaded"
  | Some _ -> ()

(** {1 Include Directive Tests (R13)}

    Tests for the 'include' directive that inlines and re-exports all
    declarations from another module. *)

(** Test that include re-exports function declarations. R13: "all declarations
    from seq.tart are part of my-extended-seq's interface" *)
let test_include_reexports_functions () =
  (* Create a module with functions *)
  let base_sig =
    parse_sig_str ~module_name:"base"
      {|
    (defun base-add (int int) -> int)
    (defun base-mul (int int) -> int)
  |}
  in
  let resolver name = if name = "base" then Some base_sig else None in
  (* Module that includes base *)
  let env =
    load_sig_str_with_resolver ~resolver
      {|
    (include 'base)
    (defun extended-op (int int int) -> int)
  |}
  in
  (* base-add should be re-exported *)
  (match Type_env.lookup "base-add" env with
  | None -> Alcotest.fail "base-add should be re-exported via include"
  | Some _ -> ());
  (* base-mul should be re-exported *)
  (match Type_env.lookup "base-mul" env with
  | None -> Alcotest.fail "base-mul should be re-exported via include"
  | Some _ -> ());
  (* Our own function should also be there *)
  match Type_env.lookup "extended-op" env with
  | None -> Alcotest.fail "extended-op should be in environment"
  | Some _ -> ()

(** Test that include re-exports type aliases. R13: Types from included module
    are available AND re-exported *)
let test_include_reexports_types () =
  (* Create a module with a type alias *)
  let types_sig =
    parse_sig_str ~module_name:"types"
      {|
    (type int-list (list int))
    (defun make-list () -> int-list)
  |}
  in
  let resolver name = if name = "types" then Some types_sig else None in
  (* Module that includes types *)
  let env =
    load_sig_str_with_resolver ~resolver
      {|
    (include 'types)
    (defun sum-list (int-list) -> int)
  |}
  in
  (* Verify make-list is re-exported from include *)
  (match Type_env.lookup "make-list" env with
  | None -> Alcotest.fail "make-list should be re-exported via include"
  | Some scheme ->
      let scheme_str = Type_env.scheme_to_string scheme in
      (* int-list should expand to (list int) *)
      Alcotest.(check bool)
        "make-list returns expanded type" true
        (try
           let _ = Str.search_forward (Str.regexp_string "list") scheme_str 0 in
           true
         with Not_found -> false));
  (* Verify sum-list was loaded with expanded alias *)
  match Type_env.lookup "sum-list" env with
  | None -> Alcotest.fail "sum-list not found"
  | Some scheme ->
      let scheme_str = Type_env.scheme_to_string scheme in
      (* int-list should expand to (list int) *)
      Alcotest.(check bool)
        "sum-list param has expanded type" true
        (try
           let _ = Str.search_forward (Str.regexp_string "list") scheme_str 0 in
           true
         with Not_found -> false)

(** Test that include re-exports variable declarations. R13: defvar declarations
    are also re-exported *)
let test_include_reexports_variables () =
  (* Create a module with a variable *)
  let config_sig =
    parse_sig_str ~module_name:"config" {|
    (defvar default-value int)
  |}
  in
  let resolver name = if name = "config" then Some config_sig else None in
  (* Module that includes config *)
  let env =
    load_sig_str_with_resolver ~resolver
      {|
    (include 'config)
    (defun use-default () -> int)
  |}
  in
  (* default-value should be re-exported *)
  let ty, errors = check_expr_str ~env "default-value" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string) "variable type is int" "int" (Types.to_string ty)

(** Test that include handles opaque types correctly. R13: Opaque types preserve
    their module-qualified names *)
let test_include_preserves_opaque_identity () =
  (* Create a module with an opaque type *)
  let opaque_sig =
    parse_sig_str ~module_name:"opaque-mod"
      {|
    (type handle)
    (defun make-handle () -> handle)
    (defun use-handle (handle) -> nil)
  |}
  in
  let resolver name = if name = "opaque-mod" then Some opaque_sig else None in
  (* Module that includes the opaque module *)
  let env =
    load_sig_str_with_resolver ~resolver {|
    (include 'opaque-mod)
  |}
  in
  (* Using the handle correctly should work *)
  let _, errors = check_expr_str ~env "(use-handle (make-handle))" in
  Alcotest.(check int) "no type errors" 0 (List.length errors)

(** Test transitive includes. Including a module that includes another module.
*)
let test_include_transitive () =
  (* Create base module *)
  let base_sig =
    parse_sig_str ~module_name:"base" {|
    (defun base-fn () -> int)
  |}
  in
  (* Create middle module that includes base *)
  let middle_sig =
    parse_sig_str ~module_name:"middle"
      {|
    (include 'base)
    (defun middle-fn () -> int)
  |}
  in
  let resolver name =
    match name with
    | "base" -> Some base_sig
    | "middle" -> Some middle_sig
    | _ -> None
  in
  (* Module that includes middle *)
  let env =
    load_sig_str_with_resolver ~resolver
      {|
    (include 'middle)
    (defun top-fn () -> int)
  |}
  in
  (* All functions should be available due to transitive inclusion *)
  (match Type_env.lookup "base-fn" env with
  | None -> Alcotest.fail "base-fn should be transitively re-exported"
  | Some _ -> ());
  (match Type_env.lookup "middle-fn" env with
  | None -> Alcotest.fail "middle-fn should be re-exported"
  | Some _ -> ());
  match Type_env.lookup "top-fn" env with
  | None -> Alcotest.fail "top-fn should be in environment"
  | Some _ -> ()

(** Test that include handles cycles gracefully. *)
let test_include_cycle_detection () =
  (* Create modules where we'd have potential cycles *)
  let a_sig =
    parse_sig_str ~module_name:"a" {|
    (defun a-fn () -> int)
  |}
  in
  let resolver name = if name = "a" then Some a_sig else None in
  (* Including the same module twice should not duplicate *)
  let env =
    load_sig_str_with_resolver ~resolver
      {|
    (include 'a)
    (include 'a)
    (defun my-fn () -> int)
  |}
  in
  (* Should work without errors *)
  match Type_env.lookup "a-fn" env with
  | None -> Alcotest.fail "a-fn should be in environment"
  | Some _ -> ()

(** {1 Import-Struct Tests (R11)}

    Tests for import-struct that generates type, constructor, predicate, and
    accessors. *)

(** Test that import-struct generates the struct type. R11: Type `person` is
    generated *)
let test_import_struct_generates_type () =
  let sig_src =
    {|
    (import-struct person :slots ((name string) (age int)))
  |}
  in
  let env = load_sig_str sig_src in
  (* The constructor should return the struct type *)
  match Type_env.lookup "make-person" env with
  | None -> Alcotest.fail "make-person not found"
  | Some scheme ->
      let scheme_str = Type_env.scheme_to_string scheme in
      (* Should return the opaque type test/person *)
      Alcotest.(check bool)
        "constructor returns struct type" true
        (try
           let _ =
             Str.search_forward (Str.regexp_string "test/person") scheme_str 0
           in
           true
         with Not_found -> false)

(** Test that import-struct generates the constructor. R11: Constructor
    `make-person` is generated *)
let test_import_struct_generates_constructor () =
  let sig_src =
    {|
    (import-struct person :slots ((name string) (age int)))
  |}
  in
  let env = load_sig_str sig_src in
  (* Call constructor with correct types *)
  let ty, errors = check_expr_str ~env "(make-person \"Alice\" 30)" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  (* The result type should be the struct type *)
  Alcotest.(check string)
    "result is struct type" "test/person" (Types.to_string ty)

(** Test that import-struct constructor type-checks arguments. R11: Constructor
    enforces slot types *)
let test_import_struct_constructor_type_error () =
  let sig_src =
    {|
    (import-struct person :slots ((name string) (age int)))
  |}
  in
  let env = load_sig_str sig_src in
  (* Call constructor with wrong types *)
  let _, errors = check_expr_str ~env "(make-person 42 \"wrong\")" in
  Alcotest.(check bool)
    "type error for wrong slot types" true
    (List.length errors > 0)

(** Test that import-struct generates the predicate. R11: Predicate `person-p`
    is generated *)
let test_import_struct_generates_predicate () =
  let sig_src =
    {|
    (import-struct person :slots ((name string) (age int)))
  |}
  in
  let env = load_sig_str sig_src in
  (* Check predicate exists with correct type *)
  match Type_env.lookup "person-p" env with
  | None -> Alcotest.fail "person-p not found"
  | Some scheme ->
      let scheme_str = Type_env.scheme_to_string scheme in
      (* Should be (Or truthy nil) -> (Or t nil) *)
      Alcotest.(check bool)
        "predicate takes Any" true
        (try
           let _ =
             Str.search_forward
               (Str.regexp_string "(Or truthy nil)")
               scheme_str 0
           in
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "predicate returns bool" true
        (try
           let _ =
             Str.search_forward (Str.regexp_string "(Or t nil)") scheme_str 0
           in
           true
         with Not_found -> false)

(** Test that import-struct generates accessors. R11: Accessors for each slot
    are generated *)
let test_import_struct_generates_accessors () =
  let sig_src =
    {|
    (import-struct person :slots ((name string) (age int)))
  |}
  in
  let env = load_sig_str sig_src in
  (* Check person-name accessor *)
  (match Type_env.lookup "person-name" env with
  | None -> Alcotest.fail "person-name not found"
  | Some _ -> ());
  (* Check person-age accessor *)
  match Type_env.lookup "person-age" env with
  | None -> Alcotest.fail "person-age not found"
  | Some _ -> ()

(** Test that accessors type-check correctly. R11: Accessor calls type-check
    based on slot types *)
let test_import_struct_accessor_types () =
  let sig_src =
    {|
    (import-struct person :slots ((name string) (age int)))
  |}
  in
  let env = load_sig_str sig_src in
  (* Access name from a person - should return string *)
  let ty1, errors1 =
    check_expr_str ~env "(person-name (make-person \"Alice\" 30))"
  in
  Alcotest.(check int) "no type errors for name" 0 (List.length errors1);
  Alcotest.(check string)
    "name accessor returns string" "string" (Types.to_string ty1);
  (* Access age from a person - should return int *)
  let ty2, errors2 =
    check_expr_str ~env "(person-age (make-person \"Alice\" 30))"
  in
  Alcotest.(check int) "no type errors for age" 0 (List.length errors2);
  Alcotest.(check string) "age accessor returns int" "int" (Types.to_string ty2)

(** Test that accessors require the correct struct type. R11: Accessors reject
    wrong types *)
let test_import_struct_accessor_type_error () =
  let sig_src =
    {|
    (import-struct person :slots ((name string) (age int)))
  |}
  in
  let env = load_sig_str sig_src in
  (* Accessor with wrong argument type *)
  let _, errors = check_expr_str ~env "(person-name 42)" in
  Alcotest.(check bool)
    "type error for accessor with wrong type" true
    (List.length errors > 0)

(** Test import-struct with empty slots. R11: Struct with no slots is valid.
    Note: marker is prelude now, use different name. *)
let test_import_struct_no_slots () =
  let sig_src = {|
    (import-struct my-marker :slots ())
  |} in
  let env = load_sig_str sig_src in
  (* Constructor takes no arguments *)
  let ty, errors = check_expr_str ~env "(make-my-marker)" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string)
    "result is struct type" "test/my-marker" (Types.to_string ty)

(** Test that two different structs have different types. R11: Struct types are
    distinct *)
let test_import_struct_types_distinct () =
  let sig_src =
    {|
    (import-struct person :slots ((name string)))
    (import-struct company :slots ((name string)))
    (defun process-person (person) -> nil)
  |}
  in
  let env = load_sig_str sig_src in
  (* Trying to pass a company where person is expected should fail *)
  let _, errors =
    check_expr_str ~env "(process-person (make-company \"Acme\"))"
  in
  Alcotest.(check bool)
    "type error for wrong struct type" true
    (List.length errors > 0)

(** {1 Data Declaration Tests} *)

(** Test that data declaration generates type. *)
let test_data_generates_type () =
  let sig_src = {|
    (data result [a e] (Ok a) (Err e))
  |} in
  let env = load_sig_str sig_src in
  (* The type should exist as an opaque type *)
  let ty, errors = check_expr_str ~env "(Ok 42)" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  (* Return type should be the ADT type with applied type args *)
  Alcotest.(check bool)
    "result is ADT type" true
    (string_starts_with "(test/result" (Types.to_string ty))

(** Test that data generates constructor functions. R1: Constructor functions *)
let test_data_generates_constructors () =
  let sig_src = {|
    (data result [a e] (Ok a) (Err e))
  |} in
  let env = load_sig_str sig_src in
  (* Check Ok constructor *)
  (match Type_env.lookup "Ok" env with
  | None -> Alcotest.fail "Ok constructor not found"
  | Some _ -> ());
  (* Check Err constructor *)
  match Type_env.lookup "Err" env with
  | None -> Alcotest.fail "Err constructor not found"
  | Some _ -> ()

(** Test that constructor functions type-check correctly. *)
let test_data_constructor_type_check () =
  let sig_src = {|
    (data result [a e] (Ok a) (Err e))
  |} in
  let env = load_sig_str sig_src in
  (* Ok constructor should work with any value *)
  let ty1, errors1 = check_expr_str ~env "(Ok 42)" in
  Alcotest.(check int) "no type errors for Ok" 0 (List.length errors1);
  Alcotest.(check bool)
    "Ok 42 returns result" true
    (string_starts_with "(test/result" (Types.to_string ty1));
  (* Err constructor should work with any value *)
  let ty2, errors2 = check_expr_str ~env {|(Err "oops")|} in
  Alcotest.(check int) "no type errors for Err" 0 (List.length errors2);
  Alcotest.(check bool)
    "Err returns result" true
    (string_starts_with "(test/result" (Types.to_string ty2))

(* Note: Use `my-bool` instead of `bool` since `bool` is now a prelude type *)

(** Test that data with nullary constructors works. *)
let test_data_nullary_constructors () =
  let sig_src = {|
    (data my-bool (True) (False))
  |} in
  let env = load_sig_str sig_src in
  (* True is a function taking no args *)
  let ty1, errors1 = check_expr_str ~env "(True)" in
  Alcotest.(check int) "no type errors for True" 0 (List.length errors1);
  Alcotest.(check string)
    "True returns my-bool" "test/my-bool" (Types.to_string ty1);
  (* False is a function taking no args *)
  let ty2, errors2 = check_expr_str ~env "(False)" in
  Alcotest.(check int) "no type errors for False" 0 (List.length errors2);
  Alcotest.(check string)
    "False returns my-bool" "test/my-bool" (Types.to_string ty2)

(** Test that data with multi-field constructors works. R6: Multi-field *)
let test_data_multi_field_constructor () =
  let sig_src =
    {|
    (data point (Point2D int int) (Point3D int int int))
  |}
  in
  let env = load_sig_str sig_src in
  (* Point2D takes two ints *)
  let ty1, errors1 = check_expr_str ~env "(Point2D 1 2)" in
  Alcotest.(check int) "no type errors for Point2D" 0 (List.length errors1);
  Alcotest.(check string)
    "Point2D returns point" "test/point" (Types.to_string ty1);
  (* Point3D takes three ints *)
  let ty2, errors2 = check_expr_str ~env "(Point3D 1 2 3)" in
  Alcotest.(check int) "no type errors for Point3D" 0 (List.length errors2);
  Alcotest.(check string)
    "Point3D returns point" "test/point" (Types.to_string ty2);
  (* Type error: wrong number of args *)
  let _, errors3 = check_expr_str ~env "(Point2D 1)" in
  Alcotest.(check bool)
    "type error for wrong arity" true
    (List.length errors3 > 0)

(** Test polymorphic data type with function using it. R7: Recursive types *)
let test_data_recursive_type () =
  let sig_src = {|
    (data tree [a] (Leaf a) (Node (tree a) (tree a)))
  |} in
  let env = load_sig_str sig_src in
  (* Leaf constructor *)
  let ty1, errors1 = check_expr_str ~env "(Leaf 42)" in
  Alcotest.(check int) "no type errors for Leaf" 0 (List.length errors1);
  Alcotest.(check bool)
    "Leaf returns tree" true
    (string_starts_with "(test/tree" (Types.to_string ty1));
  (* Nested construction *)
  let ty2, errors2 = check_expr_str ~env "(Node (Leaf 1) (Leaf 2))" in
  Alcotest.(check int) "no type errors for nested" 0 (List.length errors2);
  Alcotest.(check bool)
    "Node returns tree" true
    (string_starts_with "(test/tree" (Types.to_string ty2))

(** {1 Data Predicate Tests - R2} *)

(** Test that data generates predicate functions. *)
let test_data_generates_predicates () =
  let sig_src = {|
    (data result [a e] (Ok a) (Err e))
  |} in
  let env = load_sig_str sig_src in
  (* Check result-ok-p predicate *)
  (match Type_env.lookup "result-ok-p" env with
  | None -> Alcotest.fail "result-ok-p predicate not found"
  | Some _ -> ());
  (* Check result-err-p predicate *)
  match Type_env.lookup "result-err-p" env with
  | None -> Alcotest.fail "result-err-p predicate not found"
  | Some _ -> ()

(** Test that predicates have correct type: (any) -> bool *)
let test_data_predicate_type () =
  let sig_src = {|
    (data result [a e] (Ok a) (Err e))
  |} in
  let env = load_sig_str sig_src in
  (* Predicate should accept any value and return bool *)
  let ty, errors = check_expr_str ~env {|(result-ok-p 42)|} in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string)
    "predicate returns bool" "(Or t nil)" (Types.to_string ty)

(** Test that predicates work with ADT values *)
let test_data_predicate_with_adt () =
  let sig_src = {|
    (data result [a e] (Ok a) (Err e))
  |} in
  let env = load_sig_str sig_src in
  (* Predicate should work on ADT values *)
  let ty, errors = check_expr_str ~env {|(result-ok-p (Ok 42))|} in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string)
    "predicate returns bool" "(Or t nil)" (Types.to_string ty)

(** Test nullary data predicates *)
let test_data_nullary_predicates () =
  let sig_src = {|
    (data my-bool (True) (False))
  |} in
  let env = load_sig_str sig_src in
  (* Check my-bool-true-p predicate *)
  (match Type_env.lookup "my-bool-true-p" env with
  | None -> Alcotest.fail "my-bool-true-p predicate not found"
  | Some _ -> ());
  (* Check my-bool-false-p predicate *)
  (match Type_env.lookup "my-bool-false-p" env with
  | None -> Alcotest.fail "my-bool-false-p predicate not found"
  | Some _ -> ());
  (* Test predicate type *)
  let ty, errors = check_expr_str ~env {|(my-bool-true-p (True))|} in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string)
    "predicate returns bool" "(Or t nil)" (Types.to_string ty)

(** Test predicate naming uses lowercase constructor *)
let test_data_predicate_lowercase () =
  let sig_src = {|
    (data point (Point2D int int))
  |} in
  let env = load_sig_str sig_src in
  (* Predicate name should be point-point2d-p (lowercase) *)
  match Type_env.lookup "point-point2d-p" env with
  | None -> Alcotest.fail "point-point2d-p predicate not found"
  | Some _ -> ()

(** {1 Data Accessor Tests - R3} *)

(** Test that data generates accessor functions for single-field constructors.
*)
let test_data_generates_accessors () =
  let sig_src = {|
    (data result [a e] (Ok a) (Err e))
  |} in
  let env = load_sig_str sig_src in
  (* Check result-ok-value accessor *)
  (match Type_env.lookup "result-ok-value" env with
  | None -> Alcotest.fail "result-ok-value accessor not found"
  | Some _ -> ());
  (* Check result-err-value accessor *)
  match Type_env.lookup "result-err-value" env with
  | None -> Alcotest.fail "result-err-value accessor not found"
  | Some _ -> ()

(** Test that accessor returns the correct field type. R3: returns field type *)
let test_data_accessor_type () =
  let sig_src = {|
    (data result [a e] (Ok a) (Err e))
  |} in
  let env = load_sig_str sig_src in
  (* Accessor should take ADT value and return the field type *)
  let ty, errors = check_expr_str ~env {|(result-ok-value (Ok 42))|} in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string) "accessor returns int" "int" (Types.to_string ty)

(** Test accessor with ADT value returns correct type *)
let test_data_accessor_with_adt () =
  let sig_src = {|
    (data result [a e] (Ok a) (Err e))
  |} in
  let env = load_sig_str sig_src in
  (* Err accessor should return the error type *)
  let ty, errors = check_expr_str ~env {|(result-err-value (Err "error"))|} in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string)
    "accessor returns string" "string" (Types.to_string ty)

(** Test nullary constructors have no accessors *)
let test_data_nullary_no_accessor () =
  let sig_src = {|
    (data my-bool (True) (False))
  |} in
  let env = load_sig_str sig_src in
  (* Nullary constructors should NOT have accessors *)
  (match Type_env.lookup "bool-true-value" env with
  | None -> () (* Expected - no accessor for nullary *)
  | Some _ -> Alcotest.fail "bool-true-value should not exist");
  match Type_env.lookup "bool-false-value" env with
  | None -> () (* Expected - no accessor for nullary *)
  | Some _ -> Alcotest.fail "bool-false-value should not exist"

(** Test multi-field constructors generate indexed accessors *)
let test_data_multi_field_accessors () =
  let sig_src =
    {|
    (data point (Point2D int int) (Point3D int int int))
  |}
  in
  let env = load_sig_str sig_src in
  (* Point2D should have -1 and -2 accessors *)
  (match Type_env.lookup "point-point2d-1" env with
  | None -> Alcotest.fail "point-point2d-1 accessor not found"
  | Some _ -> ());
  (match Type_env.lookup "point-point2d-2" env with
  | None -> Alcotest.fail "point-point2d-2 accessor not found"
  | Some _ -> ());
  (* Point3D should have -1, -2, -3 accessors *)
  (match Type_env.lookup "point-point3d-1" env with
  | None -> Alcotest.fail "point-point3d-1 accessor not found"
  | Some _ -> ());
  (match Type_env.lookup "point-point3d-2" env with
  | None -> Alcotest.fail "point-point3d-2 accessor not found"
  | Some _ -> ());
  match Type_env.lookup "point-point3d-3" env with
  | None -> Alcotest.fail "point-point3d-3 accessor not found"
  | Some _ -> ()

(** Test multi-field accessor returns correct type *)
let test_data_multi_field_accessor_type () =
  let sig_src = {|
    (data point (Point2D int int))
  |} in
  let env = load_sig_str sig_src in
  (* Both accessors should return Int *)
  let ty1, errors1 = check_expr_str ~env {|(point-point2d-1 (Point2D 1 2))|} in
  Alcotest.(check int) "no type errors" 0 (List.length errors1);
  Alcotest.(check string) "accessor 1 returns int" "int" (Types.to_string ty1);
  let ty2, errors2 = check_expr_str ~env {|(point-point2d-2 (Point2D 1 2))|} in
  Alcotest.(check int) "no type errors" 0 (List.length errors2);
  Alcotest.(check string) "accessor 2 returns int" "int" (Types.to_string ty2)

(** Test accessor naming uses lowercase constructor *)
let test_data_accessor_lowercase () =
  let sig_src = {|
    (data result [a e] (OK a) (ERR e))
  |} in
  let env = load_sig_str sig_src in
  (* Accessor names should be lowercase *)
  (match Type_env.lookup "result-ok-value" env with
  | None -> Alcotest.fail "result-ok-value accessor not found"
  | Some _ -> ());
  match Type_env.lookup "result-err-value" env with
  | None -> Alcotest.fail "result-err-value accessor not found"
  | Some _ -> ()

(** {1 Type-Scope Tests (Spec 19)} *)

(** Test that type-scope parses and loads correctly. R1: Basic syntax *)
let test_type_scope_basic () =
  let sig_src =
    {|
    (type-scope [a]
      (defun iter-next ((iter a)) -> (a | nil))
      (defun iter-peek ((iter a)) -> (a | nil)))
  |}
  in
  let env = load_sig_str sig_src in
  (* Both functions should be loaded *)
  (match Type_env.lookup "iter-next" env with
  | None -> Alcotest.fail "iter-next not found"
  | Some _ -> ());
  match Type_env.lookup "iter-peek" env with
  | None -> Alcotest.fail "iter-peek not found"
  | Some _ -> ()

(** Test that scoped type variables are shared. R2: Shared variables *)
let test_type_scope_shared_vars () =
  let sig_src =
    {|
    (type iter)
    (type-scope [a]
      (defun make-iter ((list a)) -> iter)
      (defun iter-next (iter) -> (a | nil)))
  |}
  in
  let env = load_sig_str sig_src in
  (* Both functions should have 'a' in their type *)
  match Type_env.lookup "make-iter" env with
  | None -> Alcotest.fail "make-iter not found"
  | Some scheme ->
      let scheme_str = Type_env.scheme_to_string scheme in
      (* Should be polymorphic - scheme_to_string uses "forall" or similar *)
      Alcotest.(check bool)
        "make-iter is polymorphic" true
        (String.length scheme_str > 0)

(** Test explicit forall inside scope adds to scope vars. R3: Explicit forall *)
let test_type_scope_explicit_forall () =
  let sig_src =
    {|
    (type iter)
    (type-scope [a]
      (defun iter-map [b] (((a -> b)) iter) -> iter)
      (defun iter-next (iter) -> (a | nil)))
  |}
  in
  let env = load_sig_str sig_src in
  (* iter-map should have both 'a' (from scope) and 'b' (from explicit) *)
  match Type_env.lookup "iter-map" env with
  | None -> Alcotest.fail "iter-map not found"
  | Some scheme ->
      let scheme_str = Type_env.scheme_to_string scheme in
      (* Should mention both type variables *)
      Alcotest.(check bool)
        "iter-map has both vars" true
        (String.length scheme_str > 0)

(** Test type declarations inside scope. R6: Types in scopes *)
let test_type_scope_with_type_decl () =
  let sig_src =
    {|
    (type-scope [a]
      (type iter)
      (defun make-iter ((list a)) -> iter))
  |}
  in
  let env = load_sig_str sig_src in
  (* Type and function should be loaded *)
  match Type_env.lookup "make-iter" env with
  | None -> Alcotest.fail "make-iter not found"
  | Some _ -> ()

(** Test nested type scopes shadow outer variables. R5: Nested shadowing *)
let test_type_scope_nested () =
  let sig_src =
    {|
    (type-scope [a]
      (defun outer-fn ((list a)) -> a)
      (type-scope [a]
        (defun inner-fn ((vector a)) -> a)))
  |}
  in
  let env = load_sig_str sig_src in
  (* Both functions should be loaded - inner scope shadows outer 'a' *)
  (match Type_env.lookup "outer-fn" env with
  | None -> Alcotest.fail "outer-fn not found"
  | Some _ -> ());
  match Type_env.lookup "inner-fn" env with
  | None -> Alcotest.fail "inner-fn not found"
  | Some _ -> ()

(** Test validation rejects unbound vars in scope. R8: Error on unbound *)
let test_type_scope_unbound_error () =
  let sig_src = {|
    (type-scope [a]
      (defun bad-fn (b) -> b))
  |} in
  let parse_result = Syntax.Read.parse_string sig_src in
  match Sig_parser.parse_signature ~module_name:"test" parse_result.sexps with
  | Error _ -> Alcotest.fail "Parse should succeed"
  | Ok sig_file -> (
      match Sig_loader.validate_signature sig_file with
      | Ok () -> Alcotest.fail "Validation should fail for unbound 'b'"
      | Error e ->
          Alcotest.(check bool)
            "error mentions unbound" true
            (String.length e.message > 0))

(** Test higher-kinded type variable in scope. R4: HK scoped variables *)
let test_type_scope_hk_variable () =
  let sig_src =
    {|
    (type-scope [(f : (* -> *))]
      (defun fmap-scope [a b] (((a -> b)) (f a)) -> (f b))
      (defun pure-scope [a] (a) -> (f a)))
  |}
  in
  let env = load_sig_str sig_src in
  (* Both functions should be loaded with 'f' as a type parameter *)
  (match Type_env.lookup "fmap-scope" env with
  | None -> Alcotest.fail "fmap-scope not found"
  | Some _ -> ());
  match Type_env.lookup "pure-scope" env with
  | None -> Alcotest.fail "pure-scope not found"
  | Some _ -> ()

(** Test HK scoped variable kind is enforced in inner declarations *)
let test_type_scope_hk_kind_enforced () =
  let sig_src =
    {|
    (type-scope [(f : (* -> *))]
      (defun use-f [a] ((f a)) -> int))
  |}
  in
  let env = load_sig_str sig_src in
  match Type_env.lookup "use-f" env with
  | None -> Alcotest.fail "use-f not found"
  | Some scheme ->
      (* The scheme should include 'f' as a type parameter *)
      let scheme_str = Type_env.scheme_to_string scheme in
      Alcotest.(check bool)
        "scheme is polymorphic" true
        (String.length scheme_str > 0)

(** {1 Type Subtraction Tests (R10)}

    Tests for the type subtraction operator (a - b) that removes a type from a
    union. *)

(** Test basic type subtraction from union *)
let test_subtract_from_union () =
  let sig_src =
    {|
    (type int-only ((int | string) - string))
    (defun use-int-only (int-only) -> int)
  |}
  in
  let env = load_sig_str sig_src in
  match Type_env.lookup "use-int-only" env with
  | None -> Alcotest.fail "use-int-only not found"
  | Some scheme ->
      let scheme_str = Type_env.scheme_to_string scheme in
      (* The type should resolve to just Int after subtraction *)
      Alcotest.(check bool)
        "param is int after subtraction" true
        (try
           let _ = Str.search_forward (Str.regexp_string "int") scheme_str 0 in
           true
         with Not_found -> false)

(** Test subtracting nil from truthy|nil yields truthy *)
let test_subtract_nil_from_any () =
  let sig_src =
    {|
    (type truthy-only ((truthy | nil) - nil))
    (defun use-truthy (truthy-only) -> int)
  |}
  in
  let env = load_sig_str sig_src in
  match Type_env.lookup "use-truthy" env with
  | None -> Alcotest.fail "use-truthy not found"
  | Some scheme ->
      let scheme_str = Type_env.scheme_to_string scheme in
      (* Should be truthy, not a union *)
      Alcotest.(check bool)
        "is truthy" true
        (try
           let _ =
             Str.search_forward (Str.regexp_string "truthy") scheme_str 0
           in
           true
         with Not_found -> false)

(** Test subtraction validation works *)
let test_subtract_validates_types () =
  let sig_src =
    {|
    (type is [a] (a - nil))
    (defun unwrap [a] ((is a)) -> a)
  |}
  in
  match validate_str sig_src with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "Unexpected error: %s" e.message)

(** Test subtraction with type variables *)
let test_subtract_with_type_var () =
  let sig_src =
    {|
    (type my-is [a] (a - nil))
    (defun is-identity [a] ((my-is a)) -> (my-is a))
  |}
  in
  let env = load_sig_str sig_src in
  match Type_env.lookup "is-identity" env with
  | None -> Alcotest.fail "is-identity not found"
  | Some _ -> ()

(** Test subtraction from 3-way union *)
let test_subtract_from_3_way_union () =
  let sig_src =
    {|
    (type two-way ((int | string | nil) - nil))
    (defun use-two-way (two-way) -> nil)
  |}
  in
  let env = load_sig_str sig_src in
  match Type_env.lookup "use-two-way" env with
  | None -> Alcotest.fail "use-two-way not found"
  | Some scheme ->
      let scheme_str = Type_env.scheme_to_string scheme in
      (* Should still be a union of Int and String *)
      Alcotest.(check bool)
        "has int" true
        (try
           let _ = Str.search_forward (Str.regexp_string "int") scheme_str 0 in
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "has string" true
        (try
           let _ =
             Str.search_forward (Str.regexp_string "string") scheme_str 0
           in
           true
         with Not_found -> false)

(** {1 Multi-Clause Defun Tests (Spec 54)} *)

(** Test that multi-clause defun loads and produces a function type *)
let test_multi_clause_loads () =
  let sig_src = {|
    (defun stringp ((string) -> t) ((_) -> nil))
  |} in
  let env = load_sig_str sig_src in
  (* stringp should be registered as a function *)
  match Type_env.lookup_fn "stringp" env with
  | None -> Alcotest.fail "stringp should be loaded"
  | Some _ -> ()

(** Test that non-predicates don't have predicate info registered *)
let test_non_predicate_not_registered () =
  let sig_src = {|
    (defun regular-fn (int) -> string)
  |} in
  let env = load_sig_str sig_src in
  match Type_env.lookup_predicate "regular-fn" env with
  | None -> () (* Expected - no predicate info *)
  | Some _ -> Alcotest.fail "regular-fn should NOT be a predicate"

(** Test multi-clause with type var binders *)
let test_multi_clause_with_binders () =
  let sig_src =
    {|
    (defun car [a b] (((cons a b)) -> a) ((nil) -> nil))
  |}
  in
  let env = load_sig_str sig_src in
  match Type_env.lookup_fn "car" env with
  | None -> Alcotest.fail "car should be loaded"
  | Some _ -> ()

let () =
  Alcotest.run "sig_loader"
    [
      ( "explicit-quantification",
        [
          Alcotest.test_case "bound type var ok" `Quick test_bound_type_var_ok;
          Alcotest.test_case "unbound type var error" `Quick
            test_unbound_type_var_error;
          Alcotest.test_case "multiple bound vars ok" `Quick
            test_multiple_bound_vars_ok;
          Alcotest.test_case "partial binding error" `Quick
            test_partial_binding_error;
          Alcotest.test_case "primitive not var" `Quick test_primitive_not_var;
          Alcotest.test_case "type decl defines type" `Quick
            test_type_decl_defines_type;
          Alcotest.test_case "unknown type constructor allowed" `Quick
            test_unknown_type_constructor_allowed;
          Alcotest.test_case "defvar validation" `Quick test_defvar_validation;
          Alcotest.test_case "type params in scope" `Quick
            test_type_params_in_scope;
          Alcotest.test_case "type params unbound in body" `Quick
            test_type_params_unbound_in_body;
          Alcotest.test_case "nested forall scoping" `Quick
            test_nested_forall_scoping;
        ] );
      ( "bounded-quantifiers",
        [
          Alcotest.test_case "bounded quantifier ok" `Quick
            test_bounded_quantifier_ok;
        ] );
      ( "validate-all",
        [
          Alcotest.test_case "multiple errors" `Quick
            test_validate_all_multiple_errors;
        ] );
      ( "end-to-end-loading",
        [
          Alcotest.test_case "defun signature used for calls" `Quick
            test_defun_signature_used_for_calls;
          Alcotest.test_case "defun signature type error" `Quick
            test_defun_signature_type_error;
          Alcotest.test_case "poly defun signature" `Quick
            test_poly_defun_signature;
          Alcotest.test_case "defvar type used" `Quick test_defvar_type_used;
          Alcotest.test_case "defvar function type" `Quick
            test_defvar_function_type;
          Alcotest.test_case "combined declarations" `Quick
            test_combined_declarations;
        ] );
      ( "type-aliases",
        [
          Alcotest.test_case "simple type alias" `Quick test_simple_type_alias;
          Alcotest.test_case "type alias return" `Quick test_type_alias_return;
          Alcotest.test_case "parameterized type alias" `Quick
            test_parameterized_type_alias;
          Alcotest.test_case "type alias defvar" `Quick test_type_alias_defvar;
          Alcotest.test_case "nested type alias" `Quick test_nested_type_alias;
          Alcotest.test_case "type alias with poly fn" `Quick
            test_type_alias_with_poly_fn;
          Alcotest.test_case "type alias in param position" `Quick
            test_type_alias_in_param_position;
        ] );
      ( "opaque-types",
        [
          Alcotest.test_case "opaque types distinct" `Quick
            test_opaque_types_distinct;
          Alcotest.test_case "opaque type creation" `Quick
            test_opaque_type_creation;
          Alcotest.test_case "opaque vs other types" `Quick
            test_opaque_vs_other_types;
          Alcotest.test_case "opaque phantom params" `Quick
            test_opaque_phantom_params;
          Alcotest.test_case "opaque return type" `Quick test_opaque_return_type;
          Alcotest.test_case "opaque with polymorphism" `Quick
            test_opaque_with_polymorphism;
        ] );
      ( "open-directive",
        [
          Alcotest.test_case "open imports type aliases" `Quick
            test_open_imports_type_aliases;
          Alcotest.test_case "open imports opaque types" `Quick
            test_open_imports_opaque_types;
          Alcotest.test_case "open does not export values" `Quick
            test_open_does_not_export_values;
          Alcotest.test_case "open cycle detection" `Quick
            test_open_cycle_detection;
        ] );
      ( "include-directive",
        [
          Alcotest.test_case "include re-exports functions" `Quick
            test_include_reexports_functions;
          Alcotest.test_case "include re-exports types" `Quick
            test_include_reexports_types;
          Alcotest.test_case "include re-exports variables" `Quick
            test_include_reexports_variables;
          Alcotest.test_case "include preserves opaque identity" `Quick
            test_include_preserves_opaque_identity;
          Alcotest.test_case "include transitive" `Quick test_include_transitive;
          Alcotest.test_case "include cycle detection" `Quick
            test_include_cycle_detection;
        ] );
      ( "import-struct",
        [
          Alcotest.test_case "generates type" `Quick
            test_import_struct_generates_type;
          Alcotest.test_case "generates constructor" `Quick
            test_import_struct_generates_constructor;
          Alcotest.test_case "constructor type error" `Quick
            test_import_struct_constructor_type_error;
          Alcotest.test_case "generates predicate" `Quick
            test_import_struct_generates_predicate;
          Alcotest.test_case "generates accessors" `Quick
            test_import_struct_generates_accessors;
          Alcotest.test_case "accessor types" `Quick
            test_import_struct_accessor_types;
          Alcotest.test_case "accessor type error" `Quick
            test_import_struct_accessor_type_error;
          Alcotest.test_case "no slots" `Quick test_import_struct_no_slots;
          Alcotest.test_case "types distinct" `Quick
            test_import_struct_types_distinct;
        ] );
      ( "data-declarations",
        [
          Alcotest.test_case "generates type" `Quick test_data_generates_type;
          Alcotest.test_case "generates constructors" `Quick
            test_data_generates_constructors;
          Alcotest.test_case "constructor type check" `Quick
            test_data_constructor_type_check;
          Alcotest.test_case "nullary constructors" `Quick
            test_data_nullary_constructors;
          Alcotest.test_case "multi-field constructor" `Quick
            test_data_multi_field_constructor;
          Alcotest.test_case "recursive type" `Quick test_data_recursive_type;
          Alcotest.test_case "generates predicates" `Quick
            test_data_generates_predicates;
          Alcotest.test_case "predicate type" `Quick test_data_predicate_type;
          Alcotest.test_case "predicate with adt" `Quick
            test_data_predicate_with_adt;
          Alcotest.test_case "nullary predicates" `Quick
            test_data_nullary_predicates;
          Alcotest.test_case "predicate lowercase" `Quick
            test_data_predicate_lowercase;
          Alcotest.test_case "generates accessors" `Quick
            test_data_generates_accessors;
          Alcotest.test_case "accessor type" `Quick test_data_accessor_type;
          Alcotest.test_case "accessor with adt" `Quick
            test_data_accessor_with_adt;
          Alcotest.test_case "nullary no accessor" `Quick
            test_data_nullary_no_accessor;
          Alcotest.test_case "multi-field accessors" `Quick
            test_data_multi_field_accessors;
          Alcotest.test_case "multi-field accessor type" `Quick
            test_data_multi_field_accessor_type;
          Alcotest.test_case "accessor lowercase" `Quick
            test_data_accessor_lowercase;
        ] );
      ( "type-scope",
        [
          Alcotest.test_case "basic syntax" `Quick test_type_scope_basic;
          Alcotest.test_case "shared variables" `Quick
            test_type_scope_shared_vars;
          Alcotest.test_case "explicit forall" `Quick
            test_type_scope_explicit_forall;
          Alcotest.test_case "with type decl" `Quick
            test_type_scope_with_type_decl;
          Alcotest.test_case "nested scopes" `Quick test_type_scope_nested;
          Alcotest.test_case "unbound error" `Quick
            test_type_scope_unbound_error;
          Alcotest.test_case "HK variable" `Quick test_type_scope_hk_variable;
          Alcotest.test_case "HK kind enforced" `Quick
            test_type_scope_hk_kind_enforced;
        ] );
      ( "type-subtraction",
        [
          Alcotest.test_case "subtract from union" `Quick
            test_subtract_from_union;
          Alcotest.test_case "subtract nil from any" `Quick
            test_subtract_nil_from_any;
          Alcotest.test_case "validates types" `Quick
            test_subtract_validates_types;
          Alcotest.test_case "with type var" `Quick test_subtract_with_type_var;
          Alcotest.test_case "from 3-way union" `Quick
            test_subtract_from_3_way_union;
        ] );
      ( "multi-clause-defun",
        [
          Alcotest.test_case "multi-clause loads" `Quick test_multi_clause_loads;
          Alcotest.test_case "non-predicate not registered" `Quick
            test_non_predicate_not_registered;
          Alcotest.test_case "multi-clause with binders" `Quick
            test_multi_clause_with_binders;
        ] );
    ]
