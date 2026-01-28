(** Tests for the acceptance test harness. *)

open Test_harness.Acceptance

(** {1 Expected File Parser Tests} *)

let test_parse_pass_no_diagnostics () =
  let content = "PASS\n" in
  match parse_expected_file content with
  | None -> Alcotest.fail "should parse valid file"
  | Some ef ->
      Alcotest.(check bool) "is Pass" true (ef.outcome = Pass);
      Alcotest.(check (list string)) "no diagnostics" [] ef.diagnostics

let test_parse_fail_no_diagnostics () =
  let content = "FAIL\n" in
  match parse_expected_file content with
  | None -> Alcotest.fail "should parse valid file"
  | Some ef ->
      Alcotest.(check bool) "is Fail" true (ef.outcome = Fail);
      Alcotest.(check (list string)) "no diagnostics" [] ef.diagnostics

let test_parse_fail_with_diagnostics () =
  let content = "FAIL\n6:1: error: type mismatch\nexpected Int, got String\n" in
  match parse_expected_file content with
  | None -> Alcotest.fail "should parse valid file"
  | Some ef ->
      Alcotest.(check bool) "is Fail" true (ef.outcome = Fail);
      Alcotest.(check int) "two diagnostics" 2 (List.length ef.diagnostics);
      Alcotest.(check string)
        "first diagnostic" "6:1: error: type mismatch" (List.hd ef.diagnostics);
      Alcotest.(check string)
        "second diagnostic" "expected Int, got String"
        (List.nth ef.diagnostics 1)

let test_parse_case_insensitive () =
  let content = "pass\n" in
  match parse_expected_file content with
  | None -> Alcotest.fail "should parse valid file"
  | Some ef -> Alcotest.(check bool) "is Pass" true (ef.outcome = Pass)

let test_parse_with_whitespace () =
  let content = "  PASS  \n" in
  match parse_expected_file content with
  | None -> Alcotest.fail "should parse valid file"
  | Some ef -> Alcotest.(check bool) "is Pass" true (ef.outcome = Pass)

let test_parse_empty_lines_filtered () =
  let content = "FAIL\n\ndiag1\n\ndiag2\n\n" in
  match parse_expected_file content with
  | None -> Alcotest.fail "should parse valid file"
  | Some ef ->
      Alcotest.(check (list string))
        "filters empty" [ "diag1"; "diag2" ] ef.diagnostics

let test_parse_invalid_outcome () =
  let content = "MAYBE\nsome diagnostic\n" in
  match parse_expected_file content with
  | Some _ -> Alcotest.fail "should reject invalid outcome"
  | None -> ()

let test_parse_empty_file () =
  let content = "" in
  match parse_expected_file content with
  | Some _ -> Alcotest.fail "should reject empty file"
  | None -> ()

(** {1 Fixture Discovery Tests} *)

let test_expected_path_of_el () =
  Alcotest.(check string)
    "converts .el to .expected" "test.expected"
    (expected_path_of_el "test.el")

let test_expected_path_of_el_with_dir () =
  Alcotest.(check string)
    "preserves directory" "dir/test.expected"
    (expected_path_of_el "dir/test.el")

(** {1 Test Suites} *)

let expected_file_parser_tests =
  [
    Alcotest.test_case "pass no diagnostics" `Quick
      test_parse_pass_no_diagnostics;
    Alcotest.test_case "fail no diagnostics" `Quick
      test_parse_fail_no_diagnostics;
    Alcotest.test_case "fail with diagnostics" `Quick
      test_parse_fail_with_diagnostics;
    Alcotest.test_case "case insensitive" `Quick test_parse_case_insensitive;
    Alcotest.test_case "with whitespace" `Quick test_parse_with_whitespace;
    Alcotest.test_case "empty lines filtered" `Quick
      test_parse_empty_lines_filtered;
    Alcotest.test_case "invalid outcome" `Quick test_parse_invalid_outcome;
    Alcotest.test_case "empty file" `Quick test_parse_empty_file;
  ]

let fixture_discovery_tests =
  [
    Alcotest.test_case "expected path conversion" `Quick
      test_expected_path_of_el;
    Alcotest.test_case "expected path with dir" `Quick
      test_expected_path_of_el_with_dir;
  ]

let () =
  Alcotest.run "acceptance"
    [
      ("expected-file-parser", expected_file_parser_tests);
      ("fixture-discovery", fixture_discovery_tests);
    ]
