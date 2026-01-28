(** Tests for coverage report. *)

open Coverage.Coverage_report
open Coverage.Definition_extractor

(** {1 Test Helpers} *)

let dummy_span : Syntax.Location.span =
  let pos =
    { Syntax.Location.file = "test.el"; line = 1; col = 0; offset = 0 }
  in
  { start_pos = pos; end_pos = { pos with col = 10; offset = 10 } }

let make_def ?(is_private = false) name kind =
  { name; kind; span = dummy_span; is_private }

let make_item ?(is_private = false) name kind status source_file =
  { definition = make_def ~is_private name kind; source_file; status }

(** {1 Signature Matching} *)

let test_has_signature_empty_env () =
  let env = Core.Type_env.empty in
  Alcotest.(check bool) "not found in empty" false (has_signature env "foo")

let test_has_signature_found () =
  let env = Core.Type_env.empty in
  let scheme = Core.Type_env.Mono (Core.Types.TCon "Int") in
  let env = Core.Type_env.extend "foo" scheme env in
  Alcotest.(check bool) "found" true (has_signature env "foo")

let test_sibling_tart_path () =
  Alcotest.(check string)
    "replaces .el with .tart" "foo.tart"
    (sibling_tart_path "foo.el")

let test_sibling_tart_path_with_dir () =
  Alcotest.(check string)
    "preserves directory" "/path/to/foo.tart"
    (sibling_tart_path "/path/to/foo.el")

(** {1 Summary Statistics} *)

let test_summarize_empty () =
  let result = { items = []; files_scanned = 0 } in
  let summary = summarize result in
  Alcotest.(check int) "total public" 0 summary.total_public;
  Alcotest.(check int) "covered public" 0 summary.covered_public;
  Alcotest.(check int) "total private" 0 summary.total_private

let test_summarize_mixed () =
  let items =
    [
      make_item "public1" Function Covered "test.el";
      make_item "public2" Function Uncovered "test.el";
      make_item ~is_private:true "private1" Function Covered "test.el";
    ]
  in
  let result = { items; files_scanned = 1 } in
  let summary = summarize result in
  Alcotest.(check int) "total public" 2 summary.total_public;
  Alcotest.(check int) "covered public" 1 summary.covered_public;
  Alcotest.(check int) "total private" 1 summary.total_private

(** {1 Coverage Percentage} *)

let test_coverage_percentage_empty () =
  let summary = { total_public = 0; covered_public = 0; total_private = 0 } in
  Alcotest.(check (float 0.01))
    "empty is 100%" 100.0
    (coverage_percentage summary)

let test_coverage_percentage_all_covered () =
  let summary = { total_public = 10; covered_public = 10; total_private = 0 } in
  Alcotest.(check (float 0.01))
    "all covered" 100.0
    (coverage_percentage summary)

let test_coverage_percentage_half () =
  let summary = { total_public = 10; covered_public = 5; total_private = 0 } in
  Alcotest.(check (float 0.01))
    "half covered" 50.0
    (coverage_percentage summary)

(** {1 Filtering} *)

let test_uncovered_public () =
  let items =
    [
      make_item "pub_cov" Function Covered "test.el";
      make_item "pub_uncov" Function Uncovered "test.el";
      make_item ~is_private:true "priv" Function Uncovered "test.el";
    ]
  in
  let result = { items; files_scanned = 1 } in
  let uncovered = uncovered_public result in
  Alcotest.(check int) "one uncovered public" 1 (List.length uncovered);
  Alcotest.(check string)
    "correct name" "pub_uncov" (List.hd uncovered).definition.name

let test_private_definitions () =
  let items =
    [
      make_item "pub" Function Covered "test.el";
      make_item ~is_private:true "priv1" Function Covered "test.el";
      make_item ~is_private:true "priv2" Variable Uncovered "test.el";
    ]
  in
  let result = { items; files_scanned = 1 } in
  let privates = private_definitions result in
  Alcotest.(check int) "two private" 2 (List.length privates)

let test_covered_public () =
  let items =
    [
      make_item "cov1" Function Covered "test.el";
      make_item "cov2" Variable Covered "test.el";
      make_item "uncov" Function Uncovered "test.el";
    ]
  in
  let result = { items; files_scanned = 1 } in
  let covered = covered_public result in
  Alcotest.(check int) "two covered" 2 (List.length covered)

(** {1 Test Suites} *)

let signature_tests =
  [
    Alcotest.test_case "empty env" `Quick test_has_signature_empty_env;
    Alcotest.test_case "found" `Quick test_has_signature_found;
    Alcotest.test_case "sibling path" `Quick test_sibling_tart_path;
    Alcotest.test_case "sibling path dir" `Quick test_sibling_tart_path_with_dir;
  ]

let summary_tests =
  [
    Alcotest.test_case "empty" `Quick test_summarize_empty;
    Alcotest.test_case "mixed" `Quick test_summarize_mixed;
  ]

let percentage_tests =
  [
    Alcotest.test_case "empty" `Quick test_coverage_percentage_empty;
    Alcotest.test_case "all covered" `Quick test_coverage_percentage_all_covered;
    Alcotest.test_case "half" `Quick test_coverage_percentage_half;
  ]

let filtering_tests =
  [
    Alcotest.test_case "uncovered public" `Quick test_uncovered_public;
    Alcotest.test_case "private defs" `Quick test_private_definitions;
    Alcotest.test_case "covered public" `Quick test_covered_public;
  ]

let () =
  Alcotest.run "coverage_report"
    [
      ("signature", signature_tests);
      ("summary", summary_tests);
      ("percentage", percentage_tests);
      ("filtering", filtering_tests);
    ]
