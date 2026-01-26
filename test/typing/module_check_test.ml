(** Tests for module-aware type checking *)

module Module_check = Tart.Module_check
module Env = Tart.Type_env

(** {1 Test Helpers} *)

(** Create a temp directory with specified files *)
let with_temp_dir files f =
  let dir = Filename.temp_dir "tart_test" "" in
  (* Clean up function *)
  let cleanup () =
    List.iter
      (fun (name, _) ->
        let path = Filename.concat dir name in
        try Sys.remove path with _ -> ())
      files;
    try Unix.rmdir dir with _ -> ()
  in
  try
    (* Create files *)
    List.iter
      (fun (name, content) ->
        let path = Filename.concat dir name in
        let oc = open_out path in
        output_string oc content;
        close_out oc)
      files;
    let result = f dir in
    cleanup ();
    result
  with e ->
    cleanup ();
    raise e

(** Parse a string to S-expressions for testing *)
let parse str = Tart.Read.parse_string_exn ~filename:"<test>" str

(* =============================================================================
   extract_requires Tests
   ============================================================================= *)

let test_extract_requires_quoted_symbol () =
  let sexps = parse "(require 'cl-lib)" in
  let requires = Module_check.extract_requires sexps in
  Alcotest.(check (list string)) "finds require" [ "cl-lib" ] requires

let test_extract_requires_quoted_list () =
  let sexps = parse "(require '(seq))" in
  (* This form isn't supported - only (require 'module) *)
  let requires = Module_check.extract_requires sexps in
  Alcotest.(check (list string)) "no match for list form" [] requires

let test_extract_requires_multiple () =
  let sexps = parse "(require 'cl-lib)\n(require 'seq)" in
  let requires = Module_check.extract_requires sexps in
  Alcotest.(check (list string)) "finds both" [ "cl-lib"; "seq" ] requires

let test_extract_requires_none () =
  let sexps = parse "(defun foo () 42)" in
  let requires = Module_check.extract_requires sexps in
  Alcotest.(check (list string)) "no requires" [] requires

(* =============================================================================
   check_module Tests - Basic Type Checking
   ============================================================================= *)

let test_check_module_no_signature () =
  let config = Module_check.default_config () in
  let sexps = parse "(defun foo () 42)" in
  let result =
    Module_check.check_module ~config ~filename:"/tmp/test.el" sexps
  in
  Alcotest.(check int) "no type errors" 0 (List.length result.type_errors);
  Alcotest.(check int)
    "no mismatch errors" 0
    (List.length result.mismatch_errors);
  Alcotest.(check bool)
    "no signature loaded" false
    (Option.is_some result.signature_env)

let test_check_module_type_error () =
  let config = Module_check.default_config () in
  (* Plus with wrong types should error *)
  let sexps = parse "(+ 1 \"hello\")" in
  let result =
    Module_check.check_module ~config ~filename:"/tmp/test.el" sexps
  in
  Alcotest.(check bool)
    "has type error" true
    (List.length result.type_errors > 0)

(* =============================================================================
   check_module Tests - Signature Loading
   ============================================================================= *)

let test_check_module_with_signature () =
  let files =
    [
      ("foo.el", "(defun foo-add (a b) (+ a b))");
      ("foo.tart", "(defun foo-add (int int) -> int)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "foo.el" in
      let sexps = parse "(defun foo-add (a b) (+ a b))" in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      Alcotest.(check bool)
        "signature loaded" true
        (Option.is_some result.signature_env);
      Alcotest.(check int) "no type errors" 0 (List.length result.type_errors);
      Alcotest.(check int)
        "no mismatch errors" 0
        (List.length result.mismatch_errors))

let test_check_module_signature_mismatch () =
  let files =
    [
      ("bar.el", "(defun bar-concat (a b) (concat a b))");
      ("bar.tart", "(defun bar-concat (int int) -> int)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "bar.el" in
      (* The implementation uses concat which expects strings, but signature says int *)
      let sexps = parse "(defun bar-concat (a b) (concat a b))" in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      Alcotest.(check bool)
        "signature loaded" true
        (Option.is_some result.signature_env);
      (* Should have mismatch because concat returns string, not int *)
      Alcotest.(check bool)
        "has mismatch or type error" true
        (List.length result.mismatch_errors > 0
        || List.length result.type_errors > 0))

(* =============================================================================
   check_module Tests - Required Modules
   ============================================================================= *)

let test_check_module_with_require () =
  let files =
    [
      ("myutil.el", "(defun myutil-double (x) (+ x x))");
      ("myutil.tart", "(defun myutil-double (int) -> int)");
      ("main.el", "(require 'myutil)\n(myutil-double 42)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "main.el" in
      let main_content = "(require 'myutil)\n(myutil-double 42)" in
      let sexps = parse main_content in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      (* Should type-check successfully since myutil.tart is found as sibling *)
      Alcotest.(check int) "no type errors" 0 (List.length result.type_errors))

let test_check_module_require_type_error () =
  let files =
    [
      ("myutil.el", "(defun myutil-double (x) (+ x x))");
      ("myutil.tart", "(defun myutil-double (int) -> int)");
      ("main.el", "(require 'myutil)\n(myutil-double \"wrong\")");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "main.el" in
      let main_content = "(require 'myutil)\n(myutil-double \"wrong\")" in
      let sexps = parse main_content in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      (* Should have type error for passing string to int parameter *)
      Alcotest.(check bool)
        "has type error" true
        (List.length result.type_errors > 0))

(* =============================================================================
   diagnostics_of_result Tests
   ============================================================================= *)

let test_diagnostics_empty () =
  let result =
    {
      Module_check.type_errors = [];
      mismatch_errors = [];
      signature_env = None;
    }
  in
  let diagnostics = Module_check.diagnostics_of_result result in
  Alcotest.(check int) "no diagnostics" 0 (List.length diagnostics)

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "module_check"
    [
      ( "extract_requires",
        [
          Alcotest.test_case "quoted symbol" `Quick
            test_extract_requires_quoted_symbol;
          Alcotest.test_case "quoted list" `Quick
            test_extract_requires_quoted_list;
          Alcotest.test_case "multiple" `Quick test_extract_requires_multiple;
          Alcotest.test_case "none" `Quick test_extract_requires_none;
        ] );
      ( "check_module_basic",
        [
          Alcotest.test_case "no signature" `Quick
            test_check_module_no_signature;
          Alcotest.test_case "type error" `Quick test_check_module_type_error;
        ] );
      ( "check_module_signatures",
        [
          Alcotest.test_case "with signature" `Quick
            test_check_module_with_signature;
          Alcotest.test_case "signature mismatch" `Quick
            test_check_module_signature_mismatch;
        ] );
      ( "check_module_requires",
        [
          Alcotest.test_case "with require" `Quick
            test_check_module_with_require;
          Alcotest.test_case "require type error" `Quick
            test_check_module_require_type_error;
        ] );
      ( "diagnostics",
        [ Alcotest.test_case "empty" `Quick test_diagnostics_empty ] );
    ]
