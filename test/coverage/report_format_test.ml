(** Tests for report formatting. *)

open Coverage.Report_format
open Coverage.Coverage_report
open Coverage.Definition_extractor

(** Helper for substring check *)
module String = struct
  include String

  let is_substring ~sub s =
    try
      let _ = Str.search_forward (Str.regexp_string sub) s 0 in
      true
    with Not_found -> false
end

(** {1 Test Helpers} *)

let dummy_span line : Syntax.Location.span =
  let pos = { Syntax.Location.file = "test.el"; line; col = 0; offset = 0 } in
  { start_pos = pos; end_pos = { pos with col = 10; offset = 10 } }

let make_def ?(is_private = false) name kind line =
  { name; kind; span = dummy_span line; is_private }

let make_item ?(is_private = false) name kind status source_file line =
  { definition = make_def ~is_private name kind line; source_file; status }

(** {1 Location Formatting} *)

let test_format_location () =
  let item = make_item "foo" Function Covered "/path/to/test.el" 42 in
  Alcotest.(check string) "file:line format" "test.el:42" (format_location item)

(** {1 Summary Formatting} *)

let test_format_summary () =
  let summary = { total_public = 10; covered_public = 7; total_private = 3 } in
  let output = format_summary ~files:5 ~summary in
  Alcotest.(check bool)
    "contains title" true
    (String.length output > 0 && String.sub output 0 15 = "Coverage Report");
  Alcotest.(check bool)
    "contains files" true
    (String.is_substring ~sub:"Files scanned: 5" output);
  Alcotest.(check bool)
    "contains percentage" true
    (String.is_substring ~sub:"70.0%" output)

(** {1 JSON Escaping} *)

let test_json_escape_simple () =
  Alcotest.(check string) "no escaping needed" "hello" (json_escape "hello")

let test_json_escape_quotes () =
  Alcotest.(check string)
    "escapes quotes" "say \\\"hi\\\"" (json_escape "say \"hi\"")

let test_json_escape_backslash () =
  Alcotest.(check string)
    "escapes backslash" "path\\\\to" (json_escape "path\\to")

let test_json_escape_newline () =
  Alcotest.(check string)
    "escapes newline" "line1\\nline2"
    (json_escape "line1\nline2")

(** {1 JSON Array} *)

let test_json_string_array_empty () =
  Alcotest.(check string) "empty array" "[]" (json_string_array [])

let test_json_string_array_single () =
  Alcotest.(check string)
    "single item" "[\"foo\"]"
    (json_string_array [ "foo" ])

let test_json_string_array_multiple () =
  Alcotest.(check string)
    "multiple items" "[\"a\", \"b\", \"c\"]"
    (json_string_array [ "a"; "b"; "c" ])

(** {1 Full Report} *)

let test_format_report_human () =
  let items =
    [
      make_item "covered-fn" Function Covered "test.el" 10;
      make_item "uncovered-fn" Function Uncovered "test.el" 20;
    ]
  in
  let result = { items; files_scanned = 1 } in
  let config = { format = Human; verbose = false } in
  let output = format_report ~config result in
  Alcotest.(check bool)
    "contains header" true
    (String.is_substring ~sub:"Coverage Report" output);
  Alcotest.(check bool)
    "contains uncovered" true
    (String.is_substring ~sub:"uncovered-fn" output)

let test_format_report_json () =
  let items =
    [
      make_item "covered-fn" Function Covered "test.el" 10;
      make_item "uncovered-fn" Function Uncovered "test.el" 20;
    ]
  in
  let result = { items; files_scanned = 1 } in
  let config = { format = Json; verbose = false } in
  let output = format_report ~config result in
  Alcotest.(check bool)
    "valid json start" true
    (String.length output > 0 && output.[0] = '{');
  Alcotest.(check bool)
    "contains files_scanned" true
    (String.is_substring ~sub:"\"files_scanned\": 1" output);
  Alcotest.(check bool)
    "contains uncovered" true
    (String.is_substring ~sub:"\"uncovered-fn\"" output)

(** {1 Test Suites} *)

let location_tests =
  [ Alcotest.test_case "format location" `Quick test_format_location ]

let summary_tests =
  [ Alcotest.test_case "format summary" `Quick test_format_summary ]

let json_escape_tests =
  [
    Alcotest.test_case "simple" `Quick test_json_escape_simple;
    Alcotest.test_case "quotes" `Quick test_json_escape_quotes;
    Alcotest.test_case "backslash" `Quick test_json_escape_backslash;
    Alcotest.test_case "newline" `Quick test_json_escape_newline;
  ]

let json_array_tests =
  [
    Alcotest.test_case "empty" `Quick test_json_string_array_empty;
    Alcotest.test_case "single" `Quick test_json_string_array_single;
    Alcotest.test_case "multiple" `Quick test_json_string_array_multiple;
  ]

let report_tests =
  [
    Alcotest.test_case "human format" `Quick test_format_report_human;
    Alcotest.test_case "json format" `Quick test_format_report_json;
  ]

let () =
  Alcotest.run "report_format"
    [
      ("location", location_tests);
      ("summary", summary_tests);
      ("json-escape", json_escape_tests);
      ("json-array", json_array_tests);
      ("report", report_tests);
    ]
