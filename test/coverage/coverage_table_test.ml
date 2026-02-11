(** Tests for coverage table renderer. *)

open Coverage.Coverage_table
module Emacs_coverage = Coverage.Emacs_coverage
module C_scanner = Coverage.C_scanner
module Definition_extractor = Coverage.Definition_extractor

(** {1 Test Helpers} *)

let no_color_config : table_config = { color = Off; format = Human }
let color_config : table_config = { color = Always; format = Human }

let contains ~sub s =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

let make_row ?(private_count = 0) ?(uncovered_names = [])
    ?(uncovered_details = []) filename public_covered public_total =
  let coverage_pct =
    if public_total = 0 then 100.0
    else float_of_int public_covered /. float_of_int public_total *. 100.0
  in
  {
    filename;
    private_count;
    public_covered;
    public_total;
    coverage_pct;
    uncovered_names;
    uncovered_details;
  }

(** {1 Table Alignment Tests} *)

let test_table_header () =
  let rows = [ make_row "alloc.c" 30 45 ] in
  let output =
    render_table ~config:no_color_config ~emacs_version:"31.0" rows
  in
  Alcotest.(check bool)
    "has FILENAME header" true
    (contains ~sub:"FILENAME" output);
  Alcotest.(check bool)
    "has PRIVATE header" true
    (contains ~sub:"PRIVATE" output);
  Alcotest.(check bool) "has PUBLIC header" true (contains ~sub:"PUBLIC" output);
  Alcotest.(check bool)
    "has COVERAGE header" true
    (contains ~sub:"COVERAGE" output)

let test_table_data_row () =
  let rows = [ make_row ~private_count:5 "alloc.c" 30 45 ] in
  let output =
    render_table ~config:no_color_config ~emacs_version:"31.0" rows
  in
  Alcotest.(check bool) "has filename" true (contains ~sub:"alloc.c" output);
  Alcotest.(check bool) "has private count" true (contains ~sub:"5" output);
  Alcotest.(check bool) "has public ratio" true (contains ~sub:"30/45" output);
  Alcotest.(check bool) "has percentage" true (contains ~sub:"66.7%" output)

let test_table_multiple_rows () =
  let rows =
    [
      make_row ~private_count:2 "alloc.c" 10 10;
      make_row ~private_count:0 "data.c" 3 6;
    ]
  in
  let output =
    render_table ~config:no_color_config ~emacs_version:"31.0" rows
  in
  Alcotest.(check bool) "has first file" true (contains ~sub:"alloc.c" output);
  Alcotest.(check bool) "has second file" true (contains ~sub:"data.c" output);
  Alcotest.(check bool) "has 100%" true (contains ~sub:"100.0%" output);
  Alcotest.(check bool) "has 50%" true (contains ~sub:"50.0%" output)

let test_table_alignment () =
  let rows =
    [
      make_row ~private_count:12 "buffer.c" 100 200;
      make_row ~private_count:0 "x.c" 1 2;
    ]
  in
  let output =
    render_table ~config:no_color_config ~emacs_version:"31.0" rows
  in
  let lines = String.split_on_char '\n' output in
  (* All lines should have the same visible width *)
  match lines with
  | header :: row1 :: row2 :: _ ->
      Alcotest.(check int)
        "row1 length matches header" (String.length header) (String.length row1);
      Alcotest.(check int)
        "row2 length matches header" (String.length header) (String.length row2)
  | _ -> Alcotest.fail "expected at least 3 lines"

let test_empty_table () =
  let output = render_table ~config:no_color_config ~emacs_version:"31.0" [] in
  Alcotest.(check bool) "has headers" true (contains ~sub:"FILENAME" output);
  let lines = String.split_on_char '\n' output in
  Alcotest.(check int) "only header line" 1 (List.length lines)

(** {1 Color Tests} *)

let test_color_green () =
  let rows = [ make_row "alloc.c" 100 100 ] in
  let output = render_table ~config:color_config ~emacs_version:"31.0" rows in
  (* Green ANSI code: \027[32m *)
  Alcotest.(check bool) "has green" true (contains ~sub:"\027[32m" output);
  Alcotest.(check bool) "has reset" true (contains ~sub:"\027[0m" output)

let test_color_yellow () =
  let rows = [ make_row "alloc.c" 50 100 ] in
  let output = render_table ~config:color_config ~emacs_version:"31.0" rows in
  Alcotest.(check bool) "has yellow" true (contains ~sub:"\027[33m" output)

let test_color_red () =
  let rows = [ make_row "alloc.c" 10 100 ] in
  let output = render_table ~config:color_config ~emacs_version:"31.0" rows in
  Alcotest.(check bool) "has red" true (contains ~sub:"\027[31m" output)

let test_no_color () =
  let rows = [ make_row "alloc.c" 10 100 ] in
  let output =
    render_table ~config:no_color_config ~emacs_version:"31.0" rows
  in
  Alcotest.(check bool) "no ANSI codes" false (contains ~sub:"\027[" output)

let test_color_threshold_boundary_95 () =
  (* Exactly 95% → green *)
  let rows = [ make_row "x.c" 95 100 ] in
  let output = render_table ~config:color_config ~emacs_version:"31.0" rows in
  Alcotest.(check bool) "95% is green" true (contains ~sub:"\027[32m" output)

let test_color_threshold_boundary_50 () =
  (* Exactly 50% → yellow *)
  let rows = [ make_row "x.c" 50 100 ] in
  let output = render_table ~config:color_config ~emacs_version:"31.0" rows in
  Alcotest.(check bool) "50% is yellow" true (contains ~sub:"\027[33m" output)

let test_color_threshold_boundary_49 () =
  (* 49% → red *)
  let rows = [ make_row "x.c" 49 100 ] in
  let output = render_table ~config:color_config ~emacs_version:"31.0" rows in
  Alcotest.(check bool) "49% is red" true (contains ~sub:"\027[31m" output)

(** {1 Sort Tests} *)

let test_default_sort_c_before_el () =
  let rows =
    [
      make_row "simple.el" 1 2;
      make_row "alloc.c" 3 4;
      make_row "buffer.c" 5 6;
      make_row "subr.el" 7 8;
    ]
  in
  let sorted = default_sort rows in
  let names = List.map (fun r -> r.filename) sorted in
  Alcotest.(check (list string))
    "c first then el"
    [ "alloc.c"; "buffer.c"; "simple.el"; "subr.el" ]
    names

let test_default_sort_alphabetical_within_group () =
  let rows =
    [ make_row "data.c" 1 2; make_row "alloc.c" 3 4; make_row "buffer.c" 5 6 ]
  in
  let sorted = default_sort rows in
  let names = List.map (fun r -> r.filename) sorted in
  Alcotest.(check (list string))
    "alphabetical c files"
    [ "alloc.c"; "buffer.c"; "data.c" ]
    names

let test_default_sort_empty () =
  let sorted = default_sort [] in
  Alcotest.(check int) "empty sort" 0 (List.length sorted)

(** {1 JSON Tests} *)

let test_json_structure () =
  let rows =
    [
      make_row ~private_count:12 ~uncovered_names:[ "foo"; "bar" ] "alloc.c" 30
        45;
    ]
  in
  let output = render_json ~emacs_version:"31.0" rows in
  Alcotest.(check bool)
    "has emacs_version" true
    (contains ~sub:"\"emacs_version\": \"31.0\"" output);
  Alcotest.(check bool)
    "has files array" true
    (contains ~sub:"\"files\":" output);
  Alcotest.(check bool) "has totals" true (contains ~sub:"\"totals\":" output)

let test_json_file_entry () =
  let rows =
    [
      make_row ~private_count:12
        ~uncovered_names:[ "identifier1"; "identifier2" ]
        "alloc.c" 30 45;
    ]
  in
  let output = render_json ~emacs_version:"31.0" rows in
  Alcotest.(check bool)
    "has filename" true
    (contains ~sub:"\"filename\": \"alloc.c\"" output);
  Alcotest.(check bool)
    "has private" true
    (contains ~sub:"\"private\": 12" output);
  Alcotest.(check bool)
    "has public_covered" true
    (contains ~sub:"\"public_covered\": 30" output);
  Alcotest.(check bool)
    "has public_total" true
    (contains ~sub:"\"public_total\": 45" output);
  Alcotest.(check bool)
    "has coverage_pct" true
    (contains ~sub:"\"coverage_pct\": 66.7" output);
  Alcotest.(check bool)
    "has uncovered" true
    (contains ~sub:"\"identifier1\"" output);
  Alcotest.(check bool)
    "has uncovered 2" true
    (contains ~sub:"\"identifier2\"" output)

let test_json_totals () =
  let rows =
    [
      make_row ~private_count:5 "alloc.c" 20 30;
      make_row ~private_count:3 "data.c" 10 20;
    ]
  in
  let output = render_json ~emacs_version:"31.0" rows in
  (* Totals: private=8, covered=30, total=50, pct=60.0 *)
  Alcotest.(check bool)
    "total private" true
    (contains ~sub:"\"private\": 8" output);
  Alcotest.(check bool)
    "total covered" true
    (contains ~sub:"\"public_covered\": 30" output);
  Alcotest.(check bool)
    "total public" true
    (contains ~sub:"\"public_total\": 50" output);
  Alcotest.(check bool)
    "total pct" true
    (contains ~sub:"\"coverage_pct\": 60.0" output)

let test_json_empty_rows () =
  let output = render_json ~emacs_version:"31.0" [] in
  Alcotest.(check bool)
    "has emacs_version" true
    (contains ~sub:"\"emacs_version\"" output);
  Alcotest.(check bool)
    "has empty files" true
    (contains ~sub:"\"files\": [\n  ]" output);
  Alcotest.(check bool)
    "totals 100%" true
    (contains ~sub:"\"coverage_pct\": 100.0" output)

let test_json_escaping () =
  let rows = [ make_row ~uncovered_names:[ "foo\"bar" ] "test.c" 0 1 ] in
  let output = render_json ~emacs_version:"31.0" rows in
  Alcotest.(check bool)
    "escapes quotes in names" true
    (contains ~sub:"foo\\\"bar" output)

(** {1 Row Construction Tests} *)

let test_rows_of_c_result () =
  let items : Emacs_coverage.c_coverage_item list =
    [
      {
        definition =
          {
            C_scanner.name = "car";
            kind = C_scanner.Defun;
            file = "data.c";
            line = 10;
          };
        status = Emacs_coverage.Covered;
      };
      {
        definition =
          {
            C_scanner.name = "cdr";
            kind = C_scanner.Defun;
            file = "data.c";
            line = 20;
          };
        status = Emacs_coverage.Uncovered;
      };
      {
        definition =
          {
            C_scanner.name = "internal--thing";
            kind = C_scanner.Defun;
            file = "data.c";
            line = 30;
          };
        status = Emacs_coverage.Uncovered;
      };
    ]
  in
  let result : Emacs_coverage.c_coverage_result =
    { items; source_dir = "/tmp"; emacs_version = "31.0"; files_scanned = 1 }
  in
  let rows = rows_of_c_result result in
  Alcotest.(check int) "one row per file" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.(check string) "filename" "data.c" row.filename;
  Alcotest.(check int) "private" 1 row.private_count;
  Alcotest.(check int) "public covered" 1 row.public_covered;
  Alcotest.(check int) "public total" 2 row.public_total;
  Alcotest.(check (list string)) "uncovered names" [ "cdr" ] row.uncovered_names

let test_rows_of_elisp_result () =
  let span : Syntax.Location.span =
    let pos =
      { Syntax.Location.file = "simple.el"; line = 1; col = 0; offset = 0 }
    in
    { start_pos = pos; end_pos = pos }
  in
  let file_results : Emacs_coverage.elisp_file_result list =
    [
      {
        filename = "simple.el";
        items =
          [
            {
              definition =
                {
                  Definition_extractor.name = "my-func";
                  kind = Definition_extractor.Function;
                  span;
                  is_private = false;
                };
              status = Emacs_coverage.Covered;
            };
            {
              definition =
                {
                  Definition_extractor.name = "my--internal";
                  kind = Definition_extractor.Function;
                  span;
                  is_private = true;
                };
              status = Emacs_coverage.Uncovered;
            };
            {
              definition =
                {
                  Definition_extractor.name = "other-func";
                  kind = Definition_extractor.Function;
                  span;
                  is_private = false;
                };
              status = Emacs_coverage.Uncovered;
            };
          ];
      };
    ]
  in
  let result : Emacs_coverage.elisp_coverage_result =
    { file_results; source_dir = "/tmp"; emacs_version = "31.0" }
  in
  let rows = rows_of_elisp_result result in
  Alcotest.(check int) "one row" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.(check string) "filename" "simple.el" row.filename;
  Alcotest.(check int) "private" 1 row.private_count;
  Alcotest.(check int) "public covered" 1 row.public_covered;
  Alcotest.(check int) "public total" 2 row.public_total;
  Alcotest.(check (list string))
    "uncovered names" [ "other-func" ] row.uncovered_names

(** {1 sort_rows Tests} *)

let test_sort_by_name () =
  let rows =
    [ make_row "data.c" 1 2; make_row "alloc.c" 3 4; make_row "buffer.el" 5 6 ]
  in
  let sorted = sort_rows ~column:Name ~reverse:false rows in
  let names = List.map (fun r -> r.filename) sorted in
  Alcotest.(check (list string))
    "alphabetical"
    [ "alloc.c"; "buffer.el"; "data.c" ]
    names

let test_sort_by_coverage () =
  let rows =
    [ make_row "high.c" 9 10; make_row "low.c" 1 10; make_row "mid.c" 5 10 ]
  in
  let sorted = sort_rows ~column:Coverage ~reverse:false rows in
  let names = List.map (fun r -> r.filename) sorted in
  Alcotest.(check (list string))
    "ascending coverage"
    [ "low.c"; "mid.c"; "high.c" ]
    names

let test_sort_by_public () =
  let rows =
    [ make_row "a.c" 10 20; make_row "b.c" 5 20; make_row "c.c" 15 20 ]
  in
  let sorted = sort_rows ~column:Public ~reverse:false rows in
  let names = List.map (fun r -> r.filename) sorted in
  Alcotest.(check (list string))
    "ascending public covered" [ "b.c"; "a.c"; "c.c" ] names

let test_sort_by_private () =
  let rows =
    [
      make_row ~private_count:3 "a.c" 1 2;
      make_row ~private_count:1 "b.c" 1 2;
      make_row ~private_count:5 "c.c" 1 2;
    ]
  in
  let sorted = sort_rows ~column:Private ~reverse:false rows in
  let names = List.map (fun r -> r.filename) sorted in
  Alcotest.(check (list string))
    "ascending private" [ "b.c"; "a.c"; "c.c" ] names

let test_sort_reverse () =
  let rows =
    [ make_row "high.c" 9 10; make_row "low.c" 1 10; make_row "mid.c" 5 10 ]
  in
  let sorted = sort_rows ~column:Coverage ~reverse:true rows in
  let names = List.map (fun r -> r.filename) sorted in
  Alcotest.(check (list string))
    "descending coverage"
    [ "high.c"; "mid.c"; "low.c" ]
    names

let test_sort_default_matches_default_sort () =
  let rows =
    [
      make_row "simple.el" 1 2; make_row "alloc.c" 3 4; make_row "buffer.c" 5 6;
    ]
  in
  let sorted1 = default_sort rows in
  let sorted2 = sort_rows ~column:Default ~reverse:false rows in
  let names1 = List.map (fun r -> r.filename) sorted1 in
  let names2 = List.map (fun r -> r.filename) sorted2 in
  Alcotest.(check (list string)) "same as default_sort" names1 names2

(** {1 filter_rows Tests} *)

let test_filter_min_pct () =
  let rows =
    [ make_row "high.c" 9 10; make_row "low.c" 1 10; make_row "mid.c" 5 10 ]
  in
  let filtered = filter_rows ~min_pct:50.0 rows in
  let names = List.map (fun r -> r.filename) filtered in
  Alcotest.(check (list string)) "min 50%" [ "high.c"; "mid.c" ] names

let test_filter_max_pct () =
  let rows =
    [ make_row "high.c" 9 10; make_row "low.c" 1 10; make_row "mid.c" 5 10 ]
  in
  let filtered = filter_rows ~max_pct:50.0 rows in
  let names = List.map (fun r -> r.filename) filtered in
  Alcotest.(check (list string)) "max 50%" [ "low.c"; "mid.c" ] names

let test_filter_range () =
  let rows =
    [ make_row "high.c" 9 10; make_row "low.c" 1 10; make_row "mid.c" 5 10 ]
  in
  let filtered = filter_rows ~min_pct:20.0 ~max_pct:60.0 rows in
  let names = List.map (fun r -> r.filename) filtered in
  Alcotest.(check (list string)) "20-60%" [ "mid.c" ] names

let test_filter_no_constraints () =
  let rows = [ make_row "a.c" 1 10; make_row "b.c" 9 10 ] in
  let filtered = filter_rows rows in
  Alcotest.(check int) "all rows" 2 (List.length filtered)

(** {1 Positional Matching Tests} *)

let test_match_glob_star () =
  let rows =
    [
      make_row "buffer.c" 1 2; make_row "buffer.el" 3 4; make_row "alloc.c" 5 6;
    ]
  in
  let matched = match_positional [ "buf*" ] rows in
  let names = List.map (fun r -> r.filename) matched in
  Alcotest.(check (list string)) "glob buf*" [ "buffer.c"; "buffer.el" ] names

let test_match_glob_question () =
  let rows =
    [ make_row "data.c" 1 2; make_row "dafa.c" 3 4; make_row "alloc.c" 5 6 ]
  in
  let matched = match_positional [ "da?a.c" ] rows in
  let names = List.map (fun r -> r.filename) matched in
  Alcotest.(check (list string)) "glob da?a.c" [ "data.c"; "dafa.c" ] names

let test_match_filename_exact () =
  let rows =
    [
      make_row "buffer.c" 1 2; make_row "buffer.el" 3 4; make_row "alloc.c" 5 6;
    ]
  in
  let matched = match_positional [ "buffer.c" ] rows in
  let names = List.map (fun r -> r.filename) matched in
  Alcotest.(check (list string)) "exact filename" [ "buffer.c" ] names

let test_match_feature_name () =
  let rows =
    [
      make_row "buffer.c" 1 2; make_row "buffer.el" 3 4; make_row "alloc.c" 5 6;
    ]
  in
  let matched = match_positional [ "buffer" ] rows in
  let names = List.map (fun r -> r.filename) matched in
  Alcotest.(check (list string))
    "feature name"
    [ "buffer.c"; "buffer.el" ]
    names

let test_match_multiple_args () =
  let rows =
    [ make_row "buffer.c" 1 2; make_row "alloc.c" 3 4; make_row "data.c" 5 6 ]
  in
  let matched = match_positional [ "buffer"; "data.c" ] rows in
  let names = List.map (fun r -> r.filename) matched in
  Alcotest.(check (list string)) "multiple args" [ "buffer.c"; "data.c" ] names

let test_match_empty_args () =
  let rows = [ make_row "buffer.c" 1 2; make_row "alloc.c" 3 4 ] in
  let matched = match_positional [] rows in
  Alcotest.(check int) "no filter" 2 (List.length matched)

(** {1 Drill-Down Tests} *)

let make_detail file line col identifier = { file; line; col; identifier }

let test_drilldown_human_basic () =
  let details =
    [ make_detail "data.c" 10 0 "cdr"; make_detail "data.c" 20 0 "foo" ]
  in
  let rows =
    [
      make_row ~uncovered_names:[ "cdr"; "foo" ] ~uncovered_details:details
        "data.c" 1 3;
    ]
  in
  let output =
    render_drilldown_human ~config:no_color_config ~emacs_version:"31.0" rows
  in
  (* Table part *)
  Alcotest.(check bool)
    "has table header" true
    (contains ~sub:"FILENAME" output);
  (* Drill-down lines *)
  Alcotest.(check bool)
    "has cdr line" true
    (contains ~sub:"data.c:10:0: cdr" output);
  Alcotest.(check bool)
    "has foo line" true
    (contains ~sub:"data.c:20:0: foo" output)

let test_drilldown_human_no_uncovered () =
  let rows = [ make_row "data.c" 3 3 ] in
  let output =
    render_drilldown_human ~config:no_color_config ~emacs_version:"31.0" rows
  in
  (* Should just be the table, no blank line or drilldown *)
  Alcotest.(check bool)
    "has table header" true
    (contains ~sub:"FILENAME" output);
  Alcotest.(check bool) "no double newline" false (contains ~sub:"\n\n" output)

let test_drilldown_json_structure () =
  let details =
    [
      make_detail "buffer.c" 42 0 "get-buffer";
      make_detail "buffer.c" 100 5 "set-buffer";
    ]
  in
  let rows =
    [
      make_row
        ~uncovered_names:[ "get-buffer"; "set-buffer" ]
        ~uncovered_details:details "buffer.c" 5 7;
    ]
  in
  let output = render_drilldown_json ~emacs_version:"31.0" rows in
  Alcotest.(check bool)
    "has uncovered_details" true
    (contains ~sub:"\"uncovered_details\":" output);
  Alcotest.(check bool)
    "has file field" true
    (contains ~sub:"\"file\": \"buffer.c\"" output);
  Alcotest.(check bool) "has line 42" true (contains ~sub:"\"line\": 42" output);
  Alcotest.(check bool) "has col 5" true (contains ~sub:"\"col\": 5" output);
  Alcotest.(check bool)
    "has identifier" true
    (contains ~sub:"\"identifier\": \"set-buffer\"" output);
  (* Also has normal parts *)
  Alcotest.(check bool)
    "has files array" true
    (contains ~sub:"\"files\":" output);
  Alcotest.(check bool) "has totals" true (contains ~sub:"\"totals\":" output)

let test_drilldown_json_empty_details () =
  let rows = [ make_row "data.c" 3 3 ] in
  let output = render_drilldown_json ~emacs_version:"31.0" rows in
  Alcotest.(check bool)
    "has uncovered_details" true
    (contains ~sub:"\"uncovered_details\": [\n  ]" output)

let test_drilldown_human_sorted_by_file_line () =
  let details_a =
    [ make_detail "buffer.c" 50 0 "zeta"; make_detail "buffer.c" 10 0 "alpha" ]
  in
  let details_b = [ make_detail "alloc.c" 5 0 "xmalloc" ] in
  let rows =
    [
      make_row ~uncovered_names:[ "alpha"; "zeta" ] ~uncovered_details:details_a
        "buffer.c" 1 3;
      make_row ~uncovered_names:[ "xmalloc" ] ~uncovered_details:details_b
        "alloc.c" 0 1;
    ]
  in
  let output =
    render_drilldown_human ~config:no_color_config ~emacs_version:"31.0" rows
  in
  (* Details should be sorted: alloc.c:5 then buffer.c:10 then buffer.c:50 *)
  let lines = String.split_on_char '\n' output in
  let detail_lines =
    List.filter
      (fun l ->
        contains ~sub:": " l
        && (not (contains ~sub:"FILENAME" l))
        && not (contains ~sub:"/" l))
      lines
  in
  (* Check the order of detail lines *)
  Alcotest.(check int) "three detail lines" 3 (List.length detail_lines);
  let first = List.nth detail_lines 0 in
  let second = List.nth detail_lines 1 in
  let third = List.nth detail_lines 2 in
  Alcotest.(check bool)
    "first is alloc" true
    (contains ~sub:"alloc.c:5:0: xmalloc" first);
  Alcotest.(check bool)
    "second is buffer:10" true
    (contains ~sub:"buffer.c:10:0: alpha" second);
  Alcotest.(check bool)
    "third is buffer:50" true
    (contains ~sub:"buffer.c:50:0: zeta" third)

(** {1 Test Suites} *)

let alignment_tests =
  [
    Alcotest.test_case "table header" `Quick test_table_header;
    Alcotest.test_case "data row" `Quick test_table_data_row;
    Alcotest.test_case "multiple rows" `Quick test_table_multiple_rows;
    Alcotest.test_case "column alignment" `Quick test_table_alignment;
    Alcotest.test_case "empty table" `Quick test_empty_table;
  ]

let color_tests =
  [
    Alcotest.test_case "green ≥95%" `Quick test_color_green;
    Alcotest.test_case "yellow ≥50%" `Quick test_color_yellow;
    Alcotest.test_case "red <50%" `Quick test_color_red;
    Alcotest.test_case "no color" `Quick test_no_color;
    Alcotest.test_case "95% boundary" `Quick test_color_threshold_boundary_95;
    Alcotest.test_case "50% boundary" `Quick test_color_threshold_boundary_50;
    Alcotest.test_case "49% boundary" `Quick test_color_threshold_boundary_49;
  ]

let sort_tests =
  [
    Alcotest.test_case "c before el" `Quick test_default_sort_c_before_el;
    Alcotest.test_case "alphabetical" `Quick
      test_default_sort_alphabetical_within_group;
    Alcotest.test_case "empty" `Quick test_default_sort_empty;
    Alcotest.test_case "sort by name" `Quick test_sort_by_name;
    Alcotest.test_case "sort by coverage" `Quick test_sort_by_coverage;
    Alcotest.test_case "sort by public" `Quick test_sort_by_public;
    Alcotest.test_case "sort by private" `Quick test_sort_by_private;
    Alcotest.test_case "sort reverse" `Quick test_sort_reverse;
    Alcotest.test_case "sort default = default_sort" `Quick
      test_sort_default_matches_default_sort;
  ]

let filter_tests =
  [
    Alcotest.test_case "min percentage" `Quick test_filter_min_pct;
    Alcotest.test_case "max percentage" `Quick test_filter_max_pct;
    Alcotest.test_case "range" `Quick test_filter_range;
    Alcotest.test_case "no constraints" `Quick test_filter_no_constraints;
  ]

let positional_tests =
  [
    Alcotest.test_case "glob star" `Quick test_match_glob_star;
    Alcotest.test_case "glob question" `Quick test_match_glob_question;
    Alcotest.test_case "exact filename" `Quick test_match_filename_exact;
    Alcotest.test_case "feature name" `Quick test_match_feature_name;
    Alcotest.test_case "multiple args" `Quick test_match_multiple_args;
    Alcotest.test_case "empty args" `Quick test_match_empty_args;
  ]

let json_tests =
  [
    Alcotest.test_case "structure" `Quick test_json_structure;
    Alcotest.test_case "file entry" `Quick test_json_file_entry;
    Alcotest.test_case "totals" `Quick test_json_totals;
    Alcotest.test_case "empty rows" `Quick test_json_empty_rows;
    Alcotest.test_case "escaping" `Quick test_json_escaping;
  ]

let drilldown_tests =
  [
    Alcotest.test_case "human basic" `Quick test_drilldown_human_basic;
    Alcotest.test_case "human no uncovered" `Quick
      test_drilldown_human_no_uncovered;
    Alcotest.test_case "json structure" `Quick test_drilldown_json_structure;
    Alcotest.test_case "json empty details" `Quick
      test_drilldown_json_empty_details;
    Alcotest.test_case "human sorted by file:line" `Quick
      test_drilldown_human_sorted_by_file_line;
  ]

let row_construction_tests =
  [
    Alcotest.test_case "C result rows" `Quick test_rows_of_c_result;
    Alcotest.test_case "Elisp result rows" `Quick test_rows_of_elisp_result;
  ]

let () =
  Alcotest.run "coverage_table"
    [
      ("alignment", alignment_tests);
      ("color", color_tests);
      ("sort", sort_tests);
      ("filter", filter_tests);
      ("positional", positional_tests);
      ("json", json_tests);
      ("drilldown", drilldown_tests);
      ("row-construction", row_construction_tests);
    ]
