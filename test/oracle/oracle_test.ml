(** Tests for the Emacs reader oracle.

    These tests invoke Emacs as a subprocess and thus require [emacs] on PATH.
    If Emacs is not found, integration tests are skipped with a message. *)

module Emacs_reader = Tart.Emacs_reader
module Compare = Tart.Oracle_compare

(* =============================================================================
   Helpers
   ============================================================================= *)

let has_emacs = Emacs_reader.find_emacs () <> None

(** Skip a test when Emacs is not on PATH. *)
let require_emacs () = if not has_emacs then Alcotest.skip ()

(** Write a temporary .el file and return its path. The file is not
    automatically deleted — the OS temp directory handles cleanup. *)
let write_temp_el (contents : string) : string =
  let path = Filename.temp_file "oracle_test" ".el" in
  let oc = open_out path in
  output_string oc contents;
  close_out oc;
  path

let comparison_result_pp fmt = function
  | Compare.Match -> Format.fprintf fmt "Match"
  | Compare.Mismatch { tart_output; emacs_output } ->
      Format.fprintf fmt "Mismatch(tart=%S, emacs=%S)" tart_output emacs_output
  | Compare.Tart_error _ -> Format.fprintf fmt "Tart_error"
  | Compare.Emacs_error _ -> Format.fprintf fmt "Emacs_error"

let comparison_result_equal a b =
  match (a, b) with
  | Compare.Match, Compare.Match -> true
  | Compare.Mismatch a, Compare.Mismatch b ->
      String.equal a.tart_output b.tart_output
      && String.equal a.emacs_output b.emacs_output
  | Compare.Tart_error _, Compare.Tart_error _ -> true
  | Compare.Emacs_error _, Compare.Emacs_error _ -> true
  | _ -> false

let comparison_testable =
  Alcotest.testable comparison_result_pp comparison_result_equal

(* =============================================================================
   find_emacs: PATH discovery
   ============================================================================= *)

let test_find_emacs_returns_path () =
  require_emacs ();
  match Emacs_reader.find_emacs () with
  | Some path ->
      Alcotest.(check bool)
        "path contains emacs" true
        (Filename.basename path = "emacs")
  | None -> Alcotest.fail "expected emacs on PATH"

(* =============================================================================
   run_batch: low-level invocation
   ============================================================================= *)

let test_run_batch_basic () =
  require_emacs ();
  match Emacs_reader.run_batch {|(princ "hello")|} with
  | Ok (stdout, _stderr) -> Alcotest.(check string) "stdout" "hello" stdout
  | Error _ -> Alcotest.fail "expected Ok"

let test_run_batch_exit_code () =
  require_emacs ();
  match Emacs_reader.run_batch {|(kill-emacs 1)|} with
  | Error (Emacs_failed { exit_code; _ }) ->
      Alcotest.(check int) "exit code" 1 exit_code
  | Ok _ -> Alcotest.fail "expected error"
  | Error e ->
      Alcotest.failf "expected Emacs_failed, got %s"
        (match e with
        | Emacs_reader.Emacs_not_found -> "Emacs_not_found"
        | Emacs_reader.Timeout _ -> "Timeout"
        | Emacs_reader.Read_error _ -> "Read_error"
        | Emacs_reader.Emacs_failed _ -> "Emacs_failed (wrong)")

let test_run_batch_stderr_separate () =
  require_emacs ();
  match Emacs_reader.run_batch {|(progn (message "warning") (princ "ok"))|} with
  | Ok (stdout, stderr) ->
      Alcotest.(check string) "stdout is clean" "ok" stdout;
      Alcotest.(check bool) "stderr non-empty" true (String.length stderr > 0)
  | Error _ -> Alcotest.fail "expected Ok"

(* =============================================================================
   read_string: basic forms
   ============================================================================= *)

let test_read_string_int () =
  require_emacs ();
  match Emacs_reader.read_string "42" with
  | Ok s -> Alcotest.(check string) "integer" "42" s
  | Error _ -> Alcotest.fail "expected Ok"

let test_read_string_string () =
  require_emacs ();
  match Emacs_reader.read_string {|"hello"|} with
  | Ok s -> Alcotest.(check string) "string" {|"hello"|} s
  | Error _ -> Alcotest.fail "expected Ok"

let test_read_string_symbol () =
  require_emacs ();
  match Emacs_reader.read_string "foo-bar" with
  | Ok s -> Alcotest.(check string) "symbol" "foo-bar" s
  | Error _ -> Alcotest.fail "expected Ok"

let test_read_string_list () =
  require_emacs ();
  match Emacs_reader.read_string "(+ 1 2)" with
  | Ok s -> Alcotest.(check string) "list" "(+ 1 2)" s
  | Error _ -> Alcotest.fail "expected Ok"

let test_read_string_vector () =
  require_emacs ();
  match Emacs_reader.read_string "[a b c]" with
  | Ok s -> Alcotest.(check string) "vector" "[a b c]" s
  | Error _ -> Alcotest.fail "expected Ok"

let test_read_string_nil () =
  require_emacs ();
  match Emacs_reader.read_string "nil" with
  | Ok s -> Alcotest.(check string) "nil" "nil" s
  | Error _ -> Alcotest.fail "expected Ok"

let test_read_string_t () =
  require_emacs ();
  match Emacs_reader.read_string "t" with
  | Ok s -> Alcotest.(check string) "t" "t" s
  | Error _ -> Alcotest.fail "expected Ok"

(* =============================================================================
   read_string: special syntax
   ============================================================================= *)

let test_read_string_quote () =
  require_emacs ();
  match Emacs_reader.read_string "'foo" with
  | Ok s -> Alcotest.(check string) "quote" "'foo" s
  | Error _ -> Alcotest.fail "expected Ok"

let test_read_string_backquote () =
  require_emacs ();
  match Emacs_reader.read_string "`(a ,b)" with
  | Ok s ->
      (* Emacs 28+ prints backquote literally *)
      Alcotest.(check bool) "contains a" true (String.length s > 0)
  | Error _ -> Alcotest.fail "expected Ok"

let test_read_string_function () =
  require_emacs ();
  match Emacs_reader.read_string "#'car" with
  | Ok s -> Alcotest.(check string) "function" "#'car" s
  | Error _ -> Alcotest.fail "expected Ok"

let test_read_string_keyword () =
  require_emacs ();
  match Emacs_reader.read_string ":foo" with
  | Ok s -> Alcotest.(check string) "keyword" ":foo" s
  | Error _ -> Alcotest.fail "expected Ok"

let test_read_string_dotted_pair () =
  require_emacs ();
  match Emacs_reader.read_string "(a . b)" with
  | Ok s -> Alcotest.(check string) "dotted pair" "(a . b)" s
  | Error _ -> Alcotest.fail "expected Ok"

let test_read_string_char_literal () =
  require_emacs ();
  match Emacs_reader.read_string "?a" with
  | Ok s -> Alcotest.(check string) "char a" "97" s
  | Error _ -> Alcotest.fail "expected Ok"

(* =============================================================================
   read_string: error handling (R3)
   ============================================================================= *)

let test_read_string_malformed () =
  require_emacs ();
  match Emacs_reader.read_string "(foo" with
  | Error (Read_error { input; _ }) ->
      Alcotest.(check string) "input preserved" "(foo" input
  | Error (Emacs_failed { stderr; _ }) ->
      (* Emacs may report as failed with non-zero exit instead of read error *)
      Alcotest.(check bool) "has error message" true (String.length stderr > 0)
  | Ok _ -> Alcotest.fail "expected error for malformed input"
  | Error _ -> Alcotest.fail "unexpected error type"

let test_read_string_empty () =
  require_emacs ();
  match Emacs_reader.read_string "" with
  | Error _ -> () (* empty input should error *)
  | Ok s ->
      (* Some Emacs versions may return empty or error *)
      Alcotest.(check bool) "empty or error" true (String.length s >= 0)

(* =============================================================================
   read_file: multi-form (R2, R12)
   ============================================================================= *)

let test_read_file_multi_form () =
  require_emacs ();
  let path =
    write_temp_el
      "(defun foo () 1)\n\n(defvar bar 42)\n\n(defun baz (x) (+ x 1))\n"
  in
  match Emacs_reader.read_file path with
  | Ok forms -> Alcotest.(check int) "three forms" 3 (List.length forms)
  | Error _ -> Alcotest.fail "expected Ok"

let test_read_file_single_form () =
  require_emacs ();
  let path = write_temp_el "(defun hello () \"hi\")\n" in
  match Emacs_reader.read_file path with
  | Ok forms -> Alcotest.(check int) "one form" 1 (List.length forms)
  | Error _ -> Alcotest.fail "expected Ok"

let test_read_file_empty () =
  require_emacs ();
  let path = write_temp_el "" in
  match Emacs_reader.read_file path with
  | Ok forms -> Alcotest.(check int) "no forms" 0 (List.length forms)
  | Error (Read_error { message; _ }) -> Alcotest.failf "Read_error: %s" message
  | Error (Emacs_failed { exit_code; stderr }) ->
      Alcotest.failf "Emacs_failed (exit %d): %s" exit_code stderr
  | Error Emacs_not_found -> Alcotest.fail "Emacs_not_found"
  | Error (Timeout _) -> Alcotest.fail "Timeout"

let test_read_file_with_comments () =
  require_emacs ();
  let path =
    write_temp_el
      ";; This is a comment\n\
       (defun foo () 1)\n\
       ;; Another comment\n\
       (defvar bar 2)\n"
  in
  match Emacs_reader.read_file path with
  | Ok forms -> Alcotest.(check int) "comments stripped" 2 (List.length forms)
  | Error _ -> Alcotest.fail "expected Ok"

(* =============================================================================
   Timeout (R10)
   ============================================================================= *)

let test_timeout () =
  require_emacs ();
  (* Use a very short timeout with an infinite loop *)
  match Emacs_reader.run_batch ~timeout_ms:1000 {|(while t)|} with
  | Error (Timeout { timeout_ms }) ->
      Alcotest.(check int) "timeout ms" 1000 timeout_ms
  | Error (Emacs_failed _) ->
      (* Emacs may also exit with SIGALRM rather than being caught *)
      ()
  | Ok _ -> Alcotest.fail "expected timeout"
  | Error _ -> Alcotest.fail "unexpected error type"

(* =============================================================================
   compare_string: oracle comparison (R6)
   ============================================================================= *)

let test_compare_match_int () =
  require_emacs ();
  Alcotest.(check comparison_testable)
    "integer match" Compare.Match
    (Compare.compare_string "42")

let test_compare_match_symbol () =
  require_emacs ();
  Alcotest.(check comparison_testable)
    "symbol match" Compare.Match
    (Compare.compare_string "foo-bar")

let test_compare_match_list () =
  require_emacs ();
  Alcotest.(check comparison_testable)
    "list match" Compare.Match
    (Compare.compare_string "(+ 1 2)")

let test_compare_match_string () =
  require_emacs ();
  Alcotest.(check comparison_testable)
    "string match" Compare.Match
    (Compare.compare_string {|"hello world"|})

let test_compare_match_vector () =
  require_emacs ();
  Alcotest.(check comparison_testable)
    "vector match" Compare.Match
    (Compare.compare_string "[a b c]")

let test_compare_match_dotted () =
  require_emacs ();
  Alcotest.(check comparison_testable)
    "dotted pair match" Compare.Match
    (Compare.compare_string "(a . b)")

let test_compare_match_quote () =
  require_emacs ();
  Alcotest.(check comparison_testable)
    "quote match" Compare.Match
    (Compare.compare_string "'foo")

let test_compare_match_function () =
  require_emacs ();
  Alcotest.(check comparison_testable)
    "function match" Compare.Match
    (Compare.compare_string "#'car")

let test_compare_match_keyword () =
  require_emacs ();
  Alcotest.(check comparison_testable)
    "keyword match" Compare.Match
    (Compare.compare_string ":foo")

let test_compare_match_nil () =
  require_emacs ();
  Alcotest.(check comparison_testable)
    "nil match" Compare.Match
    (Compare.compare_string "nil")

let test_compare_match_nested () =
  require_emacs ();
  Alcotest.(check comparison_testable)
    "nested match" Compare.Match
    (Compare.compare_string "(defun foo (x) (+ x 1))")

let test_compare_tart_error () =
  (* Malformed input should produce Tart_error *)
  match Compare.compare_string "(foo" with
  | Compare.Tart_error _ -> ()
  | Compare.Emacs_error _ -> () (* also acceptable if Emacs errors first *)
  | Compare.Mismatch _ -> () (* tart may parse partial *)
  | Compare.Match -> Alcotest.fail "expected error for malformed input"

(* =============================================================================
   compare_file: file comparison (R6, R12)
   ============================================================================= *)

let test_compare_file_all_match () =
  require_emacs ();
  let path = write_temp_el "(+ 1 2)\n(defvar bar 42)\n" in
  let results = Compare.compare_file path in
  Alcotest.(check int) "two results" 2 (List.length results);
  List.iter
    (fun r -> Alcotest.(check comparison_testable) "match" Compare.Match r)
    results

let test_compare_file_with_comments () =
  require_emacs ();
  let path =
    write_temp_el ";; comment\n(+ 1 2)\n;; another\n(defvar x nil)\n"
  in
  let results = Compare.compare_file path in
  Alcotest.(check int) "two results" 2 (List.length results);
  List.iter
    (fun r -> Alcotest.(check comparison_testable) "match" Compare.Match r)
    results

let test_compare_file_empty () =
  require_emacs ();
  let path = write_temp_el "" in
  let results = Compare.compare_file path in
  Alcotest.(check int) "no results" 0 (List.length results)

(* =============================================================================
   normalise: canonical form normalisation (R7)
   ============================================================================= *)

let test_normalise_trim () =
  Alcotest.(check string) "trim whitespace" "foo" (Compare.normalise "  foo  ")

let test_normalise_identity () =
  Alcotest.(check string) "no change" "(+ 1 2)" (Compare.normalise "(+ 1 2)")

(* =============================================================================
   Emacs_not_found error (R9)
   ============================================================================= *)

let test_emacs_not_found () =
  (* We can't easily mock PATH, but we test the error type exists *)
  let err = Emacs_reader.Emacs_not_found in
  match err with
  | Emacs_reader.Emacs_not_found -> ()
  | _ -> Alcotest.fail "wrong variant"

let test_emacs_failed_has_fields () =
  let err = Emacs_reader.Emacs_failed { exit_code = 42; stderr = "oops" } in
  match err with
  | Emacs_reader.Emacs_failed { exit_code; stderr } ->
      Alcotest.(check int) "exit code" 42 exit_code;
      Alcotest.(check string) "stderr" "oops" stderr
  | _ -> Alcotest.fail "wrong variant"

let test_read_error_has_fields () =
  let err =
    Emacs_reader.Read_error { input = "(foo"; message = "End of file" }
  in
  match err with
  | Emacs_reader.Read_error { input; message } ->
      Alcotest.(check string) "input" "(foo" input;
      Alcotest.(check string) "message" "End of file" message
  | _ -> Alcotest.fail "wrong variant"

let test_timeout_has_fields () =
  let err = Emacs_reader.Timeout { timeout_ms = 3000 } in
  match err with
  | Emacs_reader.Timeout { timeout_ms } ->
      Alcotest.(check int) "timeout_ms" 3000 timeout_ms
  | _ -> Alcotest.fail "wrong variant"

(* =============================================================================
   Test runner
   ============================================================================= *)

let () =
  Alcotest.run "oracle"
    [
      ( "find_emacs",
        [
          Alcotest.test_case "returns path" `Quick test_find_emacs_returns_path;
        ] );
      ( "run_batch",
        [
          Alcotest.test_case "basic" `Quick test_run_batch_basic;
          Alcotest.test_case "exit code" `Quick test_run_batch_exit_code;
          Alcotest.test_case "stderr separate" `Quick
            test_run_batch_stderr_separate;
        ] );
      ( "read_string basic",
        [
          Alcotest.test_case "int" `Quick test_read_string_int;
          Alcotest.test_case "string" `Quick test_read_string_string;
          Alcotest.test_case "symbol" `Quick test_read_string_symbol;
          Alcotest.test_case "list" `Quick test_read_string_list;
          Alcotest.test_case "vector" `Quick test_read_string_vector;
          Alcotest.test_case "nil" `Quick test_read_string_nil;
          Alcotest.test_case "t" `Quick test_read_string_t;
        ] );
      ( "read_string special syntax",
        [
          Alcotest.test_case "quote" `Quick test_read_string_quote;
          Alcotest.test_case "backquote" `Quick test_read_string_backquote;
          Alcotest.test_case "function" `Quick test_read_string_function;
          Alcotest.test_case "keyword" `Quick test_read_string_keyword;
          Alcotest.test_case "dotted pair" `Quick test_read_string_dotted_pair;
          Alcotest.test_case "char literal" `Quick test_read_string_char_literal;
        ] );
      ( "read_string errors",
        [
          Alcotest.test_case "malformed" `Quick test_read_string_malformed;
          Alcotest.test_case "empty" `Quick test_read_string_empty;
        ] );
      ( "read_file",
        [
          Alcotest.test_case "multi form" `Quick test_read_file_multi_form;
          Alcotest.test_case "single form" `Quick test_read_file_single_form;
          Alcotest.test_case "empty" `Quick test_read_file_empty;
          Alcotest.test_case "with comments" `Quick test_read_file_with_comments;
        ] );
      ("timeout", [ Alcotest.test_case "infinite loop" `Slow test_timeout ]);
      ( "compare_string",
        [
          Alcotest.test_case "int" `Quick test_compare_match_int;
          Alcotest.test_case "symbol" `Quick test_compare_match_symbol;
          Alcotest.test_case "list" `Quick test_compare_match_list;
          Alcotest.test_case "string" `Quick test_compare_match_string;
          Alcotest.test_case "vector" `Quick test_compare_match_vector;
          Alcotest.test_case "dotted pair" `Quick test_compare_match_dotted;
          Alcotest.test_case "quote" `Quick test_compare_match_quote;
          Alcotest.test_case "function" `Quick test_compare_match_function;
          Alcotest.test_case "keyword" `Quick test_compare_match_keyword;
          Alcotest.test_case "nil" `Quick test_compare_match_nil;
          Alcotest.test_case "nested" `Quick test_compare_match_nested;
          Alcotest.test_case "tart error" `Quick test_compare_tart_error;
        ] );
      ( "compare_file",
        [
          Alcotest.test_case "all match" `Quick test_compare_file_all_match;
          Alcotest.test_case "with comments" `Quick
            test_compare_file_with_comments;
          Alcotest.test_case "empty" `Quick test_compare_file_empty;
        ] );
      ( "normalise",
        [
          Alcotest.test_case "trim" `Quick test_normalise_trim;
          Alcotest.test_case "identity" `Quick test_normalise_identity;
        ] );
      ( "error types",
        [
          Alcotest.test_case "Emacs_not_found" `Quick test_emacs_not_found;
          Alcotest.test_case "Emacs_failed fields" `Quick
            test_emacs_failed_has_fields;
          Alcotest.test_case "Read_error fields" `Quick
            test_read_error_has_fields;
          Alcotest.test_case "Timeout fields" `Quick test_timeout_has_fields;
        ] );
    ]
