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
      missing_signature_warnings = [];
      undefined_errors = [];
      exhaustiveness_warnings = [];
      signature_env = None;
      final_env = Core.Type_env.empty;
    }
  in
  let diagnostics = Module_check.diagnostics_of_result result in
  Alcotest.(check int) "no diagnostics" 0 (List.length diagnostics)

(* =============================================================================
   Missing Signature Warning Tests (R5, R8)
   ============================================================================= *)

(** R8: Public function not in signature file should generate warning *)
let test_missing_signature_warning () =
  let files =
    [
      ("foo.el", "(defun foo-public (x) (+ x 1))\n(defun foo-other (x) (* x 2))");
      ("foo.tart", "(defun foo-public (int) -> int)");
      (* foo-other is NOT in the signature *)
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "foo.el" in
      let sexps =
        parse "(defun foo-public (x) (+ x 1))\n(defun foo-other (x) (* x 2))"
      in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      Alcotest.(check int)
        "one missing signature warning" 1
        (List.length result.missing_signature_warnings);
      let warning = List.hd result.missing_signature_warnings in
      Alcotest.(check string) "warning for foo-other" "foo-other" warning.name)

(** R5: Internal functions (with --) should NOT generate warning *)
let test_internal_function_no_warning () =
  let files =
    [
      ( "foo.el",
        "(defun foo-public (x) (+ x 1))\n(defun foo--internal (x) (* x 2))" );
      ("foo.tart", "(defun foo-public (int) -> int)");
      (* foo--internal is internal, so no warning expected *)
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "foo.el" in
      let sexps =
        parse
          "(defun foo-public (x) (+ x 1))\n(defun foo--internal (x) (* x 2))"
      in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      Alcotest.(check int)
        "no missing signature warnings" 0
        (List.length result.missing_signature_warnings))

(** No warnings when all functions are in signature *)
let test_all_functions_in_signature () =
  let files =
    [
      ("foo.el", "(defun foo-a (x) (+ x 1))\n(defun foo-b (x) (* x 2))");
      ("foo.tart", "(defun foo-a (int) -> int)\n(defun foo-b (int) -> int)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "foo.el" in
      let sexps =
        parse "(defun foo-a (x) (+ x 1))\n(defun foo-b (x) (* x 2))"
      in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      Alcotest.(check int)
        "no missing signature warnings" 0
        (List.length result.missing_signature_warnings))

(** No warnings when no signature file exists *)
let test_no_signature_no_warning () =
  let config = Module_check.default_config () in
  let sexps = parse "(defun foo-missing (x) (+ x 1))" in
  let result =
    Module_check.check_module ~config ~filename:"/tmp/test.el" sexps
  in
  Alcotest.(check int)
    "no warnings without signature file" 0
    (List.length result.missing_signature_warnings)

(** Mixed internal and public missing functions *)
let test_mixed_internal_public () =
  let files =
    [
      ( "foo.el",
        "(defun foo-public (x) x)\n\
         (defun foo--helper (x) x)\n\
         (defun foo-another (x) x)\n\
         (defun foo--util (x) x)" );
      ("foo.tart", "(defun foo-public (int) -> int)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "foo.el" in
      let sexps =
        parse
          "(defun foo-public (x) x)\n\
           (defun foo--helper (x) x)\n\
           (defun foo-another (x) x)\n\
           (defun foo--util (x) x)"
      in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      (* Only foo-another should have warning, not foo--helper or foo--util *)
      Alcotest.(check int)
        "one missing signature warning" 1
        (List.length result.missing_signature_warnings);
      let warning = List.hd result.missing_signature_warnings in
      Alcotest.(check string)
        "warning for foo-another" "foo-another" warning.name)

(** Missing signature diagnostics are created correctly *)
let test_missing_signature_diagnostic () =
  let files =
    [
      ("foo.el", "(defun foo-missing (x) x)");
      ("foo.tart", "(defun foo-other (int) -> int)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "foo.el" in
      let sexps = parse "(defun foo-missing (x) x)" in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      let diagnostics = Module_check.diagnostics_of_result result in
      (* Should have a warning diagnostic *)
      Alcotest.(check int) "one diagnostic" 1 (List.length diagnostics);
      let diag = List.hd diagnostics in
      Alcotest.(check bool) "is warning" true (diag.severity = Warning))

(** Signature mismatch diagnostic shows both .el and .tart locations (R6) *)
let test_signature_mismatch_has_both_locations () =
  let files =
    [
      ("bar.el", "(defun bar-fn (x) (+ x 1))");
      (* signature says string, impl returns int *)
      ("bar.tart", "(defun bar-fn (int) -> string)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "bar.el" in
      let sexps = parse "(defun bar-fn (x) (+ x 1))" in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      (* Should have a mismatch error *)
      Alcotest.(check bool)
        "has mismatch error" true
        (List.length result.mismatch_errors > 0);
      let err = List.hd result.mismatch_errors in
      (* Verify mismatch_error has both spans *)
      Alcotest.(check bool)
        "impl_span has .el file" true
        (String.length err.impl_span.start_pos.file > 0);
      Alcotest.(check bool)
        "sig_span has .tart file" true
        (String.length err.sig_span.start_pos.file > 0);
      (* Convert to diagnostic and verify related info *)
      let diag = Module_check.mismatch_to_diagnostic err in
      Alcotest.(check bool)
        "diagnostic has related info" true
        (List.length diag.related > 0))

(* =============================================================================
   Autoload Detection Tests (R7)
   ============================================================================= *)

(** extract_module_prefixes: basic three-part name *)
let test_extract_module_prefixes_basic () =
  let prefixes = Module_check.extract_module_prefixes "my-package-fn" in
  Alcotest.(check (list string))
    "yields two prefixes" [ "my-package"; "my" ] prefixes

(** extract_module_prefixes: four-part name *)
let test_extract_module_prefixes_long () =
  let prefixes =
    Module_check.extract_module_prefixes "my-package-autoload-fn"
  in
  Alcotest.(check (list string))
    "yields three prefixes"
    [ "my-package-autoload"; "my-package"; "my" ]
    prefixes

(** extract_module_prefixes: two-part name *)
let test_extract_module_prefixes_short () =
  let prefixes = Module_check.extract_module_prefixes "foo-bar" in
  Alcotest.(check (list string)) "yields one prefix" [ "foo" ] prefixes

(** extract_module_prefixes: single word returns empty list *)
let test_extract_module_prefixes_single () =
  let prefixes = Module_check.extract_module_prefixes "foo" in
  Alcotest.(check (list string)) "no prefixes" [] prefixes

(** collect_all_call_symbols: collects function calls *)
let test_collect_call_symbols () =
  let sexps = parse "(foo (bar 1) (baz 2))" in
  let symbols = Module_check.collect_all_call_symbols sexps in
  Alcotest.(check (list string))
    "collects all calls" [ "bar"; "baz"; "foo" ] symbols

(** collect_all_call_symbols: handles nested calls *)
let test_collect_call_symbols_nested () =
  let sexps = parse "(a (b (c (d 1))))" in
  let symbols = Module_check.collect_all_call_symbols sexps in
  Alcotest.(check (list string))
    "collects nested" [ "a"; "b"; "c"; "d" ] symbols

(** collect_all_call_symbols: deduplicates *)
let test_collect_call_symbols_dedup () =
  let sexps = parse "(foo (foo (foo 1)))" in
  let symbols = Module_check.collect_all_call_symbols sexps in
  Alcotest.(check (list string)) "deduplicated" [ "foo" ] symbols

(** Autoload lookup: function from unloaded module *)
let test_autoload_lookup () =
  let files =
    [
      ("main.el", "(my-package-fn 42)");
      ("my-package.tart", "(defun my-package-fn (int) -> string)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "main.el" in
      let sexps = parse "(my-package-fn 42)" in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      (* Should type check successfully - my-package.tart loaded via prefix *)
      Alcotest.(check int) "no type errors" 0 (List.length result.type_errors))

(** Autoload lookup: type error on wrong argument type *)
let test_autoload_type_error () =
  let files =
    [
      ("main.el", "(my-package-fn \"wrong\")");
      ("my-package.tart", "(defun my-package-fn (int) -> string)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "main.el" in
      let sexps = parse "(my-package-fn \"wrong\")" in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      (* Should have type error - string passed to int parameter *)
      Alcotest.(check bool)
        "has type error" true
        (List.length result.type_errors > 0))

(** Autoload lookup: doesn't reload already-required modules *)
let test_autoload_skip_required () =
  let files =
    [
      ("main.el", "(require 'my-package)\n(my-package-fn 42)");
      ("my-package.tart", "(defun my-package-fn (int) -> string)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "main.el" in
      let sexps = parse "(require 'my-package)\n(my-package-fn 42)" in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      (* Should type check successfully - module loaded via require, not autoload *)
      Alcotest.(check int) "no type errors" 0 (List.length result.type_errors))

(** Autoload lookup: longer prefix tried first *)
let test_autoload_longer_prefix_first () =
  (* If we call my-pkg-sub-fn, we should find my-pkg-sub.tart before my-pkg.tart *)
  let files =
    [
      ("main.el", "(my-pkg-sub-fn 42)");
      (* my-pkg-sub has the function, returns string *)
      ("my-pkg-sub.tart", "(defun my-pkg-sub-fn (int) -> string)");
      (* my-pkg does NOT have it *)
      ("my-pkg.tart", "(defun my-pkg-other (int) -> int)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "main.el" in
      let sexps = parse "(my-pkg-sub-fn 42)" in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      (* my-pkg-sub.tart should be found first and used *)
      Alcotest.(check int) "no type errors" 0 (List.length result.type_errors))

(** Autoload lookup: uses search path *)
let test_autoload_search_path () =
  (* Create a separate search directory *)
  let search_dir = Filename.temp_dir "tart_search" "" in
  let cleanup () = try Unix.rmdir search_dir with _ -> () in
  try
    (* Write signature to search path *)
    let sig_path = Filename.concat search_dir "ext-lib.tart" in
    let oc = open_out sig_path in
    output_string oc "(defun ext-lib-process (string) -> int)";
    close_out oc;

    let files = [ ("main.el", "(ext-lib-process \"hello\")") ] in
    with_temp_dir files (fun dir ->
        let config =
          Module_check.default_config ()
          |> Module_check.with_search_dirs [ search_dir ]
        in
        let el_path = Filename.concat dir "main.el" in
        let sexps = parse "(ext-lib-process \"hello\")" in
        let result =
          Module_check.check_module ~config ~filename:el_path sexps
        in
        (* Should find ext-lib.tart in search path *)
        Alcotest.(check int) "no type errors" 0 (List.length result.type_errors));
    Sys.remove sig_path;
    cleanup ()
  with e ->
    cleanup ();
    raise e

(* =============================================================================
   Inline Annotation / .tart File Mismatch Tests (R10)
   ============================================================================= *)

(** R10: Inline annotation that matches .tart declaration is OK *)
let test_inline_matches_tart_ok () =
  let files =
    [
      ( "foo.el",
        "(defun foo-add (a b)\n  (declare (tart (int int) -> int))\n  (+ a b))"
      );
      ("foo.tart", "(defun foo-add (int int) -> int)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "foo.el" in
      let sexps =
        parse
          "(defun foo-add (a b)\n\
          \  (declare (tart (int int) -> int))\n\
          \  (+ a b))"
      in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      Alcotest.(check int)
        "no mismatch errors" 0
        (List.length result.mismatch_errors);
      Alcotest.(check int) "no type errors" 0 (List.length result.type_errors))

(** R10: Inline annotation that conflicts with .tart declaration is an error *)
let test_inline_conflicts_with_tart () =
  let files =
    [
      ( "foo.el",
        "(defun foo-add (a b)\n\
        \  (declare (tart (string string) -> string))\n\
        \  (concat a b))" );
      (* .tart says int, inline says string - MISMATCH! *)
      ("foo.tart", "(defun foo-add (int int) -> int)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "foo.el" in
      let sexps =
        parse
          "(defun foo-add (a b)\n\
          \  (declare (tart (string string) -> string))\n\
          \  (concat a b))"
      in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      (* Should have mismatch because inline says string, .tart says int *)
      Alcotest.(check bool)
        "has mismatch error" true
        (List.length result.mismatch_errors > 0))

(** R10: Polymorphic inline annotation matching .tart is OK *)
let test_inline_polymorphic_matches_tart () =
  let files =
    [
      ("foo.el", "(defun foo-id (x)\n  (declare (tart [a] (a) -> a))\n  x)");
      ("foo.tart", "(defun foo-id [a] (a) -> a)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "foo.el" in
      let sexps =
        parse "(defun foo-id (x)\n  (declare (tart [a] (a) -> a))\n  x)"
      in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      Alcotest.(check int)
        "no mismatch errors" 0
        (List.length result.mismatch_errors);
      Alcotest.(check int) "no type errors" 0 (List.length result.type_errors))

(** R10: When no inline annotation, inferred type checked against .tart *)
let test_no_inline_uses_inferred () =
  let files =
    [
      (* No inline annotation, body infers to int -> int *)
      ("foo.el", "(defun foo-inc (x) (+ x 1))");
      ("foo.tart", "(defun foo-inc (int) -> int)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "foo.el" in
      let sexps = parse "(defun foo-inc (x) (+ x 1))" in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      Alcotest.(check int)
        "no mismatch errors" 0
        (List.length result.mismatch_errors);
      Alcotest.(check int) "no type errors" 0 (List.length result.type_errors))

(* =============================================================================
   Circular Dependency Tests (R9)
   ============================================================================= *)

(** R9: Circular dependencies via open directives are handled *)
let test_circular_open_dependencies () =
  (* Create two modules that open each other.
     a.tart opens b, b.tart opens a.
     This tests that the sig_loader's cycle detection works through module_check. *)
  let files =
    [
      (* Module A opens B and uses B's type *)
      ("a.el", "(defun a-fn (x) x)");
      ("a.tart", "(open 'b)\n(defun a-fn (btype) -> btype)");
      (* Module B opens A (cycle!) and defines its own type *)
      ("b.el", "(defun b-fn (x) x)");
      ("b.tart", "(open 'a)\n(type btype (list int))\n(defun b-fn (int) -> int)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      let el_path = Filename.concat dir "a.el" in
      let sexps = parse "(defun a-fn (x) x)" in
      (* Should not infinite loop - cycle detection should kick in *)
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      (* The key assertion is that we don't infinite loop.
         The type checking may or may not have errors depending on load order,
         but it should complete. *)
      let _ = result in
      ())

(** R9: Mutual require dependencies between modules work *)
let test_mutual_require_dependencies () =
  (* Two modules that require each other.
     The .tart files should be loadable even with this pattern. *)
  let files =
    [
      ("a.el", "(require 'b)\n(defun a-fn () (b-fn))");
      ("a.tart", "(defun a-fn () -> int)");
      ("b.el", "(require 'a)\n(defun b-fn () (a-fn))");
      ("b.tart", "(defun b-fn () -> int)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      (* Check module A which requires B *)
      let el_path_a = Filename.concat dir "a.el" in
      let sexps_a = parse "(require 'b)\n(defun a-fn () (b-fn))" in
      let result_a =
        Module_check.check_module ~config ~filename:el_path_a sexps_a
      in
      (* Should complete without infinite loop *)
      Alcotest.(check int)
        "no type errors for A" 0
        (List.length result_a.type_errors);
      (* Check module B which requires A *)
      let el_path_b = Filename.concat dir "b.el" in
      let sexps_b = parse "(require 'a)\n(defun b-fn () (a-fn))" in
      let result_b =
        Module_check.check_module ~config ~filename:el_path_b sexps_b
      in
      (* Should complete without infinite loop *)
      Alcotest.(check int)
        "no type errors for B" 0
        (List.length result_b.type_errors))

(** R9: Three-way circular dependencies work *)
let test_three_way_circular_deps () =
  (* A -> B -> C -> A via include directives *)
  let files =
    [
      ("a.el", "(defun a-fn () 42)");
      ("a.tart", "(include 'c)\n(defun a-fn () -> int)");
      ("b.el", "(defun b-fn () 42)");
      ("b.tart", "(include 'a)\n(defun b-fn () -> int)");
      ("c.el", "(defun c-fn () 42)");
      ("c.tart", "(include 'b)\n(defun c-fn () -> int)");
    ]
  in
  with_temp_dir files (fun dir ->
      let config = Module_check.default_config () in
      (* Check module A which eventually includes itself via C -> B -> A *)
      let el_path = Filename.concat dir "a.el" in
      let sexps = parse "(defun a-fn () 42)" in
      let result = Module_check.check_module ~config ~filename:el_path sexps in
      (* Should complete without infinite loop due to cycle detection *)
      let _ = result in
      ())

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
      ( "missing_signature",
        [
          Alcotest.test_case "public not in signature" `Quick
            test_missing_signature_warning;
          Alcotest.test_case "internal no warning" `Quick
            test_internal_function_no_warning;
          Alcotest.test_case "all in signature" `Quick
            test_all_functions_in_signature;
          Alcotest.test_case "no signature file" `Quick
            test_no_signature_no_warning;
          Alcotest.test_case "mixed internal public" `Quick
            test_mixed_internal_public;
          Alcotest.test_case "diagnostic created" `Quick
            test_missing_signature_diagnostic;
          Alcotest.test_case "signature mismatch both locations" `Quick
            test_signature_mismatch_has_both_locations;
        ] );
      ( "extract_module_prefixes",
        [
          Alcotest.test_case "basic three-part name" `Quick
            test_extract_module_prefixes_basic;
          Alcotest.test_case "four-part name" `Quick
            test_extract_module_prefixes_long;
          Alcotest.test_case "two-part name" `Quick
            test_extract_module_prefixes_short;
          Alcotest.test_case "single word" `Quick
            test_extract_module_prefixes_single;
        ] );
      ( "collect_call_symbols",
        [
          Alcotest.test_case "collects function calls" `Quick
            test_collect_call_symbols;
          Alcotest.test_case "handles nested calls" `Quick
            test_collect_call_symbols_nested;
          Alcotest.test_case "deduplicates" `Quick
            test_collect_call_symbols_dedup;
        ] );
      ( "autoload_lookup",
        [
          Alcotest.test_case "function from unloaded module" `Quick
            test_autoload_lookup;
          Alcotest.test_case "type error on wrong argument" `Quick
            test_autoload_type_error;
          Alcotest.test_case "skip already-required modules" `Quick
            test_autoload_skip_required;
          Alcotest.test_case "longer prefix tried first" `Quick
            test_autoload_longer_prefix_first;
          Alcotest.test_case "uses search path" `Quick test_autoload_search_path;
        ] );
      ( "circular_dependencies",
        [
          Alcotest.test_case "circular open dependencies" `Quick
            test_circular_open_dependencies;
          Alcotest.test_case "mutual require dependencies" `Quick
            test_mutual_require_dependencies;
          Alcotest.test_case "three-way circular deps" `Quick
            test_three_way_circular_deps;
        ] );
      ( "inline_annotation_vs_tart",
        [
          Alcotest.test_case "inline matches tart ok" `Quick
            test_inline_matches_tart_ok;
          Alcotest.test_case "inline conflicts with tart" `Quick
            test_inline_conflicts_with_tart;
          Alcotest.test_case "polymorphic inline matches tart" `Quick
            test_inline_polymorphic_matches_tart;
          Alcotest.test_case "no inline uses inferred" `Quick
            test_no_inline_uses_inferred;
        ] );
    ]
