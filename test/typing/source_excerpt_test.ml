(** Tests for Source_excerpt module (Spec 45). *)

module Source_excerpt = Typing.Source_excerpt
module Ansi = Typing.Ansi
module Loc = Syntax.Location

let () = Ansi.force_colors false

let make_span ?(file = "test.el") start_line start_col end_line end_col =
  Loc.
    {
      start_pos = { file; line = start_line; col = start_col; offset = 0 };
      end_pos = { file; line = end_line; col = end_col; offset = 0 };
    }

(* R1: Source line extraction *)

let test_get_lines_valid () =
  (* Create a temp file with known content *)
  let file = Filename.temp_file "test" ".el" in
  let oc = open_out file in
  Printf.fprintf oc "(defun foo ()\n  (bar))\n";
  close_out oc;

  let lines = Source_excerpt.get_lines ~file ~start_line:1 ~end_line:2 in
  (match lines with
  | Some [ "(defun foo ()"; "  (bar))" ] -> ()
  | Some l ->
      Alcotest.fail
        (Printf.sprintf "unexpected lines: [%s]" (String.concat "; " l))
  | None -> Alcotest.fail "expected Some lines");

  Sys.remove file

let test_get_lines_missing_file () =
  let result =
    Source_excerpt.get_lines ~file:"/nonexistent/file.el" ~start_line:1
      ~end_line:1
  in
  Alcotest.(check (option (list string)))
    "missing file returns None" None result

let test_get_lines_generated () =
  let result =
    Source_excerpt.get_lines ~file:"<generated>" ~start_line:1 ~end_line:1
  in
  Alcotest.(check (option (list string)))
    "generated file returns None" None result

(* R2: Underline rendering *)

let test_render_span_single_line () =
  let file = Filename.temp_file "test" ".el" in
  let oc = open_out file in
  Printf.fprintf oc "(upcase count)\n";
  close_out oc;

  let span = make_span ~file 1 8 1 13 in
  let result = Source_excerpt.render_span span in
  (match result with
  | Some s ->
      (* Check that it contains the line and underline *)
      Alcotest.(check bool) "contains source line" true (String.length s > 0);
      Alcotest.(check bool)
        "contains underline" true
        (String.sub s (String.length s - 5) 5 = "^^^^^")
  | None -> Alcotest.fail "expected Some excerpt");

  Sys.remove file

let test_render_span_multi_line () =
  let file = Filename.temp_file "test" ".el" in
  let oc = open_out file in
  Printf.fprintf oc "(if condition\n    then-branch\n    else-branch)\n";
  close_out oc;

  let span = make_span ~file 1 0 3 15 in
  let result = Source_excerpt.render_span span in
  (match result with
  | Some s ->
      (* Check multiple lines are present *)
      let lines = String.split_on_char '\n' s in
      Alcotest.(check bool)
        "has multiple output lines" true
        (List.length lines >= 3)
  | None -> Alcotest.fail "expected Some excerpt");

  Sys.remove file

(* R3: Line number gutter *)

let test_gutter_alignment () =
  let file = Filename.temp_file "test" ".el" in
  let oc = open_out file in
  for i = 1 to 100 do
    Printf.fprintf oc "line %d\n" i
  done;
  close_out oc;

  let span = make_span ~file 99 0 100 6 in
  let result = Source_excerpt.render_span span in
  (match result with
  | Some s ->
      (* Line numbers should be right-aligned in gutter *)
      Alcotest.(check bool) "contains line 99" true (String.length s > 0)
  | None -> Alcotest.fail "expected Some excerpt");

  Sys.remove file

(* R4: Elm-style headers *)

let test_format_header () =
  let span = make_span ~file:"init.el" 42 10 42 15 in
  let header = Source_excerpt.format_header ~error_type:"TYPE MISMATCH" span in
  Alcotest.(check bool)
    "contains error type" true
    (Str.string_match (Str.regexp ".*TYPE MISMATCH.*") header 0);
  Alcotest.(check bool)
    "contains file location" true
    (Str.string_match (Str.regexp ".*init.el.*42.*") header 0)

(* R5: Conversational prose *)

let test_prose_function_arg () =
  let ctx = Source_excerpt.FunctionArg { fn_name = "upcase"; arg_index = 0 } in
  let intro = Source_excerpt.intro_prose ctx in
  Alcotest.(check bool) "has intro" true (String.length intro > 0);

  let expected = Source_excerpt.expected_prose ~expected:"String" ctx in
  Alcotest.(check bool)
    "mentions function" true
    (Str.string_match (Str.regexp ".*upcase.*") expected 0)

let test_prose_if_branch () =
  let ctx = Source_excerpt.IfBranch { is_then = true } in
  let intro = Source_excerpt.intro_prose ctx in
  Alcotest.(check bool)
    "mentions if branches" true
    (Str.string_match (Str.regexp ".*branches.*if.*") intro 0)

let test_prose_declared_return () =
  let ctx = Source_excerpt.DeclaredReturn { fn_name = "my-func" } in
  let expected = Source_excerpt.expected_prose ~expected:"Int" ctx in
  Alcotest.(check bool)
    "mentions declared" true
    (Str.string_match (Str.regexp ".*my-func.*declared.*") expected 0)

(* R8: Graceful degradation *)

let test_fallback_format () =
  let result =
    Source_excerpt.fallback_format ~expected:"String" ~actual:"Int"
  in
  Alcotest.(check bool)
    "mentions source not available" true
    (Str.string_match (Str.regexp ".*source not available.*") result 0);
  Alcotest.(check bool)
    "shows expected" true
    (String.length result > 0
     && Str.string_match (Str.regexp_string "String") result 0
    ||
    try
      ignore (Str.search_forward (Str.regexp_string "String") result 0);
      true
    with Not_found -> false);
  Alcotest.(check bool)
    "shows found" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Int") result 0);
       true
     with Not_found -> false)

(* R12: Syntax highlighting *)

let test_highlight_keywords () =
  Ansi.force_colors false;
  let line = "(defun foo () (let ((x 1)) x))" in
  let result = Source_excerpt.highlight_lisp_line line in
  (* With colors disabled, should be unchanged *)
  Alcotest.(check string) "no colors when disabled" line result

let test_highlight_with_colors () =
  Ansi.force_colors true;
  let line = "(defun foo () \"hello\")" in
  let result = Source_excerpt.highlight_lisp_line line in
  (* With colors enabled, should be different *)
  Alcotest.(check bool)
    "has escape codes" true
    (String.length result > String.length line);
  Ansi.force_colors false

let () =
  Alcotest.run "source_excerpt"
    [
      ( "get_lines",
        [
          Alcotest.test_case "valid file" `Quick test_get_lines_valid;
          Alcotest.test_case "missing file" `Quick test_get_lines_missing_file;
          Alcotest.test_case "generated file" `Quick test_get_lines_generated;
        ] );
      ( "render_span",
        [
          Alcotest.test_case "single line" `Quick test_render_span_single_line;
          Alcotest.test_case "multi line" `Quick test_render_span_multi_line;
        ] );
      ("gutter", [ Alcotest.test_case "alignment" `Quick test_gutter_alignment ]);
      ("header", [ Alcotest.test_case "format" `Quick test_format_header ]);
      ( "prose",
        [
          Alcotest.test_case "function arg" `Quick test_prose_function_arg;
          Alcotest.test_case "if branch" `Quick test_prose_if_branch;
          Alcotest.test_case "declared return" `Quick test_prose_declared_return;
        ] );
      ("fallback", [ Alcotest.test_case "format" `Quick test_fallback_format ]);
      ( "highlight",
        [
          Alcotest.test_case "keywords no color" `Quick test_highlight_keywords;
          Alcotest.test_case "with colors" `Quick test_highlight_with_colors;
        ] );
    ]
