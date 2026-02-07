(** Unit tests for Roundtrip. *)

module RT = Tart.Roundtrip
module Cache = Tart.Content_cache

(* =============================================================================
   Helpers
   ============================================================================= *)

(** Create a fresh temporary directory for test isolation. *)
let make_temp_dir prefix =
  let base = Filename.get_temp_dir_name () in
  let name =
    Printf.sprintf "%s_%d_%f" prefix (Unix.getpid ()) (Unix.gettimeofday ())
  in
  let dir = Filename.concat base name in
  Unix.mkdir dir 0o755;
  dir

(** Write a temporary .el file with [contents] and return its path. *)
let write_el contents =
  let path = Filename.temp_file "roundtrip_test" ".el" in
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc;
  path

(** Run with an isolated cache directory. *)
let with_cache_dir f =
  let dir = make_temp_dir "rt_cache" in
  Unix.putenv "XDG_CACHE_HOME" dir;
  let result = f () in
  Unix.putenv "XDG_CACHE_HOME" "";
  result

(* =============================================================================
   check_file: structural round-trip
   ============================================================================= *)

let test_check_file_valid () =
  let path = write_el "(defun foo (x) (+ x 1))\n" in
  let result = RT.check_file path in
  Alcotest.(check bool) "valid .el passes" true (result = RT.Pass)

let test_check_file_multiple_forms () =
  let path = write_el "(defvar x 1)\n(defvar y 2)\n(+ x y)\n" in
  let result = RT.check_file path in
  Alcotest.(check bool) "multiple forms pass" true (result = RT.Pass)

let test_check_file_empty () =
  let path = write_el "" in
  let result = RT.check_file path in
  Alcotest.(check bool) "empty file passes" true (result = RT.Pass)

let test_check_file_comments () =
  let path = write_el ";; comment\n(foo)\n;; another\n(bar)\n" in
  let result = RT.check_file path in
  Alcotest.(check bool) "comments stripped, forms pass" true (result = RT.Pass)

let test_check_file_parse_error () =
  let path = write_el "(defun foo (" in
  let result = RT.check_file path in
  match result with
  | RT.Parse_error { path = p; error = _ } ->
      Alcotest.(check string) "path preserved" path p
  | _ -> Alcotest.fail "expected Parse_error"

let test_check_file_parse_error_has_message () =
  let path = write_el "(defun foo (" in
  let result = RT.check_file path in
  match result with
  | RT.Parse_error { error; _ } ->
      Alcotest.(check bool) "error non-empty" true (String.length error > 0)
  | _ -> Alcotest.fail "expected Parse_error"

let test_check_file_complex_forms () =
  let path =
    write_el
      "(let ((x 1)\n      (y \"hello\"))\n  (list x y :key 'sym #'func))\n"
  in
  let result = RT.check_file path in
  Alcotest.(check bool) "complex forms pass" true (result = RT.Pass)

let test_check_file_vectors () =
  let path = write_el "[1 2 3]\n[\"a\" \"b\"]\n" in
  let result = RT.check_file path in
  Alcotest.(check bool) "vectors pass" true (result = RT.Pass)

let test_check_file_quoted_forms () =
  let path = write_el "'(1 2 3)\n`(,x ,@rest)\n#'name\n" in
  let result = RT.check_file path in
  Alcotest.(check bool) "quoted forms pass" true (result = RT.Pass)

let check_file_tests =
  [
    ("valid .el passes", `Quick, test_check_file_valid);
    ("multiple forms pass", `Quick, test_check_file_multiple_forms);
    ("empty file passes", `Quick, test_check_file_empty);
    ("comments stripped", `Quick, test_check_file_comments);
    ("parse error returns Parse_error", `Quick, test_check_file_parse_error);
    ("parse error has message", `Quick, test_check_file_parse_error_has_message);
    ("complex forms pass", `Quick, test_check_file_complex_forms);
    ("vectors pass", `Quick, test_check_file_vectors);
    ("quoted forms pass", `Quick, test_check_file_quoted_forms);
  ]

(* =============================================================================
   check_file_with_emacs: oracle verification
   ============================================================================= *)

let has_emacs () =
  match Oracle.Emacs_reader.find_emacs () with Some _ -> true | None -> false

let test_emacs_match () =
  if not (has_emacs ()) then Alcotest.skip ()
  else begin
    let path = write_el "(+ 1 2)\n" in
    let result = RT.check_file_with_emacs path in
    Alcotest.(check bool) "Emacs match" true (result = RT.Pass)
  end

let test_emacs_complex () =
  if not (has_emacs ()) then Alcotest.skip ()
  else begin
    let path = write_el "'(a b c)\n(list 1 \"hello\" :key)\n" in
    let result = RT.check_file_with_emacs path in
    Alcotest.(check bool) "complex Emacs match" true (result = RT.Pass)
  end

let check_file_with_emacs_tests =
  [
    ("simple form matches Emacs", `Quick, test_emacs_match);
    ("complex forms match Emacs", `Quick, test_emacs_complex);
  ]

(* =============================================================================
   check_file_cached: cache hit/miss behaviour
   ============================================================================= *)

let test_cached_miss_runs_check () =
  with_cache_dir (fun () ->
      let path = write_el "(foo bar)\n" in
      let result = RT.check_file_cached path in
      Alcotest.(check bool) "cache miss runs check" true (result = RT.Pass))

let test_cached_hit_returns_cached () =
  with_cache_dir (fun () ->
      let path = write_el "(foo bar)\n" in
      (* First call: miss, runs check *)
      let r1 = RT.check_file_cached path in
      Alcotest.(check bool) "first call passes" true (r1 = RT.Pass);
      (* Second call: cache hit *)
      let r2 = RT.check_file_cached path in
      Alcotest.(check bool) "second call cached" true (r2 = RT.Cached))

let test_cached_no_cache_flag () =
  with_cache_dir (fun () ->
      let path = write_el "(foo bar)\n" in
      (* Populate cache *)
      let _ = RT.check_file_cached path in
      (* With no_cache, should re-run *)
      let result = RT.check_file_cached ~no_cache:true path in
      Alcotest.(check bool) "no_cache bypasses cache" true (result = RT.Pass))

let test_cached_error_not_stored () =
  with_cache_dir (fun () ->
      let path = write_el "(broken (" in
      let r1 = RT.check_file_cached path in
      (match r1 with
      | RT.Parse_error _ -> ()
      | _ -> Alcotest.fail "expected Parse_error");
      (* Second call should still run (not cached) *)
      let r2 = RT.check_file_cached path in
      match r2 with
      | RT.Parse_error _ -> ()
      | RT.Cached -> Alcotest.fail "errors should not be cached"
      | _ -> Alcotest.fail "expected Parse_error")

let check_file_cached_tests =
  [
    ("cache miss runs check", `Quick, test_cached_miss_runs_check);
    ("cache hit returns Cached", `Quick, test_cached_hit_returns_cached);
    ("no_cache flag bypasses cache", `Quick, test_cached_no_cache_flag);
    ("errors not stored in cache", `Quick, test_cached_error_not_stored);
  ]

(* =============================================================================
   run_corpus: summary counts
   ============================================================================= *)

let test_run_corpus_all_pass () =
  with_cache_dir (fun () ->
      let f1 = write_el "(foo)\n" in
      let f2 = write_el "(bar)\n" in
      let f3 = write_el "(baz)\n" in
      let summary = RT.run_corpus ~no_cache:true [ f1; f2; f3 ] in
      Alcotest.(check int) "total" 3 summary.total;
      Alcotest.(check int) "passed" 3 summary.passed;
      Alcotest.(check int) "failed" 0 summary.failed;
      Alcotest.(check int) "cached" 0 summary.cached;
      Alcotest.(check int) "no failures" 0 (List.length summary.failures))

let test_run_corpus_with_failure () =
  with_cache_dir (fun () ->
      let f1 = write_el "(ok)\n" in
      let f2 = write_el "(broken (" in
      let f3 = write_el "(also-ok)\n" in
      let summary = RT.run_corpus ~no_cache:true [ f1; f2; f3 ] in
      Alcotest.(check int) "total" 3 summary.total;
      Alcotest.(check int) "passed" 2 summary.passed;
      Alcotest.(check int) "failed" 1 summary.failed;
      Alcotest.(check int) "one failure" 1 (List.length summary.failures))

let test_run_corpus_cached () =
  with_cache_dir (fun () ->
      let f1 = write_el "(foo)\n" in
      let f2 = write_el "(bar)\n" in
      (* First run: all pass *)
      let _ = RT.run_corpus [ f1; f2 ] in
      (* Second run: all cached *)
      let summary = RT.run_corpus [ f1; f2 ] in
      Alcotest.(check int) "total" 2 summary.total;
      Alcotest.(check int) "passed" 0 summary.passed;
      Alcotest.(check int) "cached" 2 summary.cached;
      Alcotest.(check int) "failed" 0 summary.failed)

let test_run_corpus_empty () =
  with_cache_dir (fun () ->
      let summary = RT.run_corpus ~no_cache:true [] in
      Alcotest.(check int) "total" 0 summary.total;
      Alcotest.(check int) "passed" 0 summary.passed;
      Alcotest.(check int) "failed" 0 summary.failed;
      Alcotest.(check int) "cached" 0 summary.cached)

let test_run_corpus_failure_has_path () =
  with_cache_dir (fun () ->
      let f1 = write_el "(broken (" in
      let summary = RT.run_corpus ~no_cache:true [ f1 ] in
      match summary.failures with
      | [ (path, _) ] -> Alcotest.(check string) "failure path" f1 path
      | _ -> Alcotest.fail "expected one failure with path")

let run_corpus_tests =
  [
    ("all pass", `Quick, test_run_corpus_all_pass);
    ("with failure", `Quick, test_run_corpus_with_failure);
    ("cached on second run", `Quick, test_run_corpus_cached);
    ("empty file list", `Quick, test_run_corpus_empty);
    ("failure includes path", `Quick, test_run_corpus_failure_has_path);
  ]

(* =============================================================================
   make_diff: expected vs actual diff
   ============================================================================= *)

let test_diff_identical () =
  let diff = RT.make_diff ~expected:"foo\nbar" ~actual:"foo\nbar" in
  Alcotest.(check bool)
    "no -/+ lines" true
    ((not (String.contains diff '-')) && not (String.contains diff '+'))

let test_diff_different () =
  let diff = RT.make_diff ~expected:"foo" ~actual:"bar" in
  Alcotest.(check bool) "has - line" true (String.length diff > 0);
  let has_minus =
    String.split_on_char '\n' diff
    |> List.exists (fun l -> String.length l > 0 && l.[0] = '-')
  in
  let has_plus =
    String.split_on_char '\n' diff
    |> List.exists (fun l -> String.length l > 0 && l.[0] = '+')
  in
  Alcotest.(check bool) "has - line" true has_minus;
  Alcotest.(check bool) "has + line" true has_plus

let test_diff_multiline () =
  let diff = RT.make_diff ~expected:"a\nb\nc" ~actual:"a\nX\nc" in
  let lines = String.split_on_char '\n' diff in
  (* Line 1 unchanged, line 2 changed, line 3 unchanged *)
  let unchanged =
    List.filter (fun l -> String.length l > 0 && l.[0] = ' ') lines
  in
  let changed_minus =
    List.filter (fun l -> String.length l > 0 && l.[0] = '-') lines
  in
  Alcotest.(check int) "2 unchanged lines" 2 (List.length unchanged);
  Alcotest.(check int) "1 minus line" 1 (List.length changed_minus)

let test_diff_empty_expected () =
  let diff = RT.make_diff ~expected:"" ~actual:"foo" in
  Alcotest.(check bool) "non-empty diff" true (String.length diff > 0)

let test_diff_empty_actual () =
  let diff = RT.make_diff ~expected:"foo" ~actual:"" in
  Alcotest.(check bool) "non-empty diff" true (String.length diff > 0)

let make_diff_tests =
  [
    ("identical = no changes", `Quick, test_diff_identical);
    ("different lines show -/+", `Quick, test_diff_different);
    ("multiline partial diff", `Quick, test_diff_multiline);
    ("empty expected", `Quick, test_diff_empty_expected);
    ("empty actual", `Quick, test_diff_empty_actual);
  ]

(* =============================================================================
   summary_to_string: correct format
   ============================================================================= *)

let test_summary_format () =
  let s =
    { RT.total = 10; passed = 7; failed = 2; cached = 1; failures = [] }
  in
  let str = RT.summary_to_string s in
  Alcotest.(check string) "format" "10 total, 7 passed, 2 failed, 1 cached" str

let test_summary_zeros () =
  let s = { RT.total = 0; passed = 0; failed = 0; cached = 0; failures = [] } in
  let str = RT.summary_to_string s in
  Alcotest.(check string)
    "all zeros" "0 total, 0 passed, 0 failed, 0 cached" str

let test_summary_all_cached () =
  let s = { RT.total = 5; passed = 0; failed = 0; cached = 5; failures = [] } in
  let str = RT.summary_to_string s in
  Alcotest.(check string)
    "all cached" "5 total, 0 passed, 0 failed, 5 cached" str

let summary_to_string_tests =
  [
    ("standard format", `Quick, test_summary_format);
    ("all zeros", `Quick, test_summary_zeros);
    ("all cached", `Quick, test_summary_all_cached);
  ]

(* =============================================================================
   Result type: structural tests
   ============================================================================= *)

let test_result_pass () =
  Alcotest.(check bool) "Pass is Pass" true (RT.Pass = RT.Pass)

let test_result_cached () =
  Alcotest.(check bool) "Cached is Cached" true (RT.Cached = RT.Cached)

let test_result_parse_error () =
  let r = RT.Parse_error { path = "/test.el"; error = "unexpected EOF" } in
  match r with
  | RT.Parse_error { path; error } ->
      Alcotest.(check string) "path" "/test.el" path;
      Alcotest.(check string) "error" "unexpected EOF" error
  | _ -> Alcotest.fail "expected Parse_error"

let test_result_mismatch () =
  let r =
    RT.Mismatch
      {
        path = "/test.el";
        form_index = 3;
        expected = "(foo)";
        actual = "(bar)";
        diff = "- (foo)\n+ (bar)\n";
      }
  in
  match r with
  | RT.Mismatch { form_index; diff; _ } ->
      Alcotest.(check int) "form index" 3 form_index;
      Alcotest.(check bool) "diff non-empty" true (String.length diff > 0)
  | _ -> Alcotest.fail "expected Mismatch"

let test_result_emacs_mismatch () =
  let r =
    RT.Emacs_mismatch
      { path = "/test.el"; tart_output = "(foo)"; emacs_output = "(bar)" }
  in
  match r with
  | RT.Emacs_mismatch { tart_output; emacs_output; _ } ->
      Alcotest.(check string) "tart" "(foo)" tart_output;
      Alcotest.(check string) "emacs" "(bar)" emacs_output
  | _ -> Alcotest.fail "expected Emacs_mismatch"

let result_type_tests =
  [
    ("Pass", `Quick, test_result_pass);
    ("Cached", `Quick, test_result_cached);
    ("Parse_error fields", `Quick, test_result_parse_error);
    ("Mismatch fields", `Quick, test_result_mismatch);
    ("Emacs_mismatch fields", `Quick, test_result_emacs_mismatch);
  ]

(* =============================================================================
   Runner
   ============================================================================= *)

let () =
  Alcotest.run "Roundtrip"
    [
      ("check_file", check_file_tests);
      ("check_file_with_emacs", check_file_with_emacs_tests);
      ("check_file_cached", check_file_cached_tests);
      ("run_corpus", run_corpus_tests);
      ("make_diff", make_diff_tests);
      ("summary_to_string", summary_to_string_tests);
      ("result types", result_type_tests);
    ]
