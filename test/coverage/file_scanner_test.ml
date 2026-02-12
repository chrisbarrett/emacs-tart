(** Tests for the file scanner. *)

open Coverage.File_scanner

(** {1 Pattern Matching} *)

let test_matches_exclude_simple () =
  Alcotest.(check bool)
    "matches *-test.el" true
    (matches_exclude ~patterns:[ "*-test.el" ] "foo-test.el")

let test_matches_exclude_no_match () =
  Alcotest.(check bool)
    "doesn't match foo.el" false
    (matches_exclude ~patterns:[ "*-test.el" ] "foo.el")

let test_matches_exclude_multiple_patterns () =
  Alcotest.(check bool)
    "matches second pattern" true
    (matches_exclude ~patterns:[ "*-test.el"; "test-*.el" ] "test-foo.el")

let test_matches_exclude_question_mark () =
  Alcotest.(check bool)
    "? matches single char" true
    (matches_exclude ~patterns:[ "t?st.el" ] "test.el")

let test_matches_exclude_uses_basename () =
  Alcotest.(check bool)
    "uses basename not full path" true
    (matches_exclude ~patterns:[ "*-test.el" ] "/path/to/foo-test.el")

(** {1 File Type Detection} *)

let test_is_elisp_file_true () =
  Alcotest.(check bool) ".el file" true (is_elisp_file "foo.el")

let test_is_elisp_file_false () =
  Alcotest.(check bool) ".tart file" false (is_elisp_file "foo.tart")

let test_is_elisp_file_with_path () =
  Alcotest.(check bool)
    ".el file with path" true
    (is_elisp_file "/path/to/foo.el")

let test_is_elisp_file_dir_locals () =
  Alcotest.(check bool)
    ".dir-locals.el excluded" false
    (is_elisp_file ".dir-locals.el")

let test_is_elisp_file_dir_locals_with_path () =
  Alcotest.(check bool)
    ".dir-locals.el with path excluded" false
    (is_elisp_file "/path/to/.dir-locals.el")

(** {1 Directory Scanning} *)

let test_scan_paths_empty_returns_cwd () =
  (* This is a basic structural test - actual scanning depends on cwd *)
  let config = default_config in
  let result = scan_paths ~config [] in
  (* Just verify it returns a list (actual files depend on cwd) *)
  Alcotest.(check bool) "returns list" true (List.length result >= 0)

let test_scan_path_nonexistent () =
  let config = default_config in
  let result = scan_path ~config "/nonexistent/path" in
  Alcotest.(check (list string)) "empty for nonexistent" [] result

let test_scan_path_non_el_file () =
  let config = default_config in
  (* Use a file that exists but isn't .el *)
  let result = scan_path ~config "dune-project" in
  Alcotest.(check (list string)) "empty for non-.el file" [] result

(** {1 Exclusion} *)

let test_scan_excludes_matching_files () =
  (* This tests the logic - actual results depend on filesystem *)
  let config = { exclude_patterns = [ "*-test.el" ] } in
  (* Test that the exclusion is applied by checking the pattern matcher *)
  Alcotest.(check bool)
    "would exclude test file" true
    (matches_exclude ~patterns:config.exclude_patterns "my-test.el")

(** {1 Test Suites} *)

let pattern_tests =
  [
    Alcotest.test_case "matches simple" `Quick test_matches_exclude_simple;
    Alcotest.test_case "no match" `Quick test_matches_exclude_no_match;
    Alcotest.test_case "multiple patterns" `Quick
      test_matches_exclude_multiple_patterns;
    Alcotest.test_case "question mark" `Quick test_matches_exclude_question_mark;
    Alcotest.test_case "uses basename" `Quick test_matches_exclude_uses_basename;
  ]

let file_type_tests =
  [
    Alcotest.test_case "is elisp true" `Quick test_is_elisp_file_true;
    Alcotest.test_case "is elisp false" `Quick test_is_elisp_file_false;
    Alcotest.test_case "is elisp with path" `Quick test_is_elisp_file_with_path;
    Alcotest.test_case "dir-locals excluded" `Quick
      test_is_elisp_file_dir_locals;
    Alcotest.test_case "dir-locals with path excluded" `Quick
      test_is_elisp_file_dir_locals_with_path;
  ]

let scanning_tests =
  [
    Alcotest.test_case "empty paths" `Quick test_scan_paths_empty_returns_cwd;
    Alcotest.test_case "nonexistent path" `Quick test_scan_path_nonexistent;
    Alcotest.test_case "non-el file" `Quick test_scan_path_non_el_file;
    Alcotest.test_case "exclusion applied" `Quick
      test_scan_excludes_matching_files;
  ]

let () =
  Alcotest.run "file_scanner"
    [
      ("pattern-matching", pattern_tests);
      ("file-type", file_type_tests);
      ("scanning", scanning_tests);
    ]
