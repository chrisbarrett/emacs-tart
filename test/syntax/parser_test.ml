(** Tests for the Elisp S-expression parser *)

open Tart.Sexp
open Tart.Read

(** Helper to parse a single sexp and return its string representation *)
let parse s =
  match parse_one s with Ok sexp -> to_string sexp | Error msg -> failwith msg

(** Helper to check parsing produces expected sexp *)
let check_parse expected input () =
  let result = parse input in
  Alcotest.(check string) "parse result" expected result

(** Helper to check parsing a list of sexps *)
let check_parse_all expected input () =
  let result = parse_string input in
  match result.errors with
  | [] ->
      let actual = String.concat " " (List.map to_string result.sexps) in
      Alcotest.(check string) "parse result" expected actual
  | err :: _ -> Alcotest.fail ("parse error: " ^ err.message)

(** Test basic atoms *)
let test_integers () =
  check_parse "42" "42" ();
  check_parse "-17" "-17" ();
  check_parse "0" "0" ()

let test_floats () =
  check_parse "3.14" "3.14" ();
  check_parse "-2.5" "-2.5" ();
  check_parse "1." "1." ()

let test_hex_numbers () =
  check_parse "255" "0xff" ();
  check_parse "16" "0x10" ()

let test_octal_numbers () =
  check_parse "8" "0o10" ();
  check_parse "63" "0o77" ()

let test_symbols () =
  check_parse "foo" "foo" ();
  check_parse "bar-baz" "bar-baz" ();
  check_parse "+" "+" ();
  check_parse "*" "*" ();
  check_parse "1+" "1+" ();
  check_parse "string<" "string<" ()

let test_keywords () =
  check_parse ":foo" ":foo" ();
  check_parse ":bar-baz" ":bar-baz" ()

let test_strings () =
  check_parse "\"hello\"" "\"hello\"" ();
  check_parse "\"hello world\"" "\"hello world\"" ()

let test_string_escapes () =
  check_parse "\"hello\\nworld\"" "\"hello\\nworld\"" ();
  check_parse "\"tab\\there\"" "\"tab\\there\"" ();
  check_parse "\"quote: \\\"\"" "\"quote: \\\"\"" ()

(** Test character literals *)
let test_char_literals () =
  check_parse "?a" "?a" ();
  check_parse "?Z" "?Z" ()

let test_char_escapes () =
  check_parse "?\\x0a" "?\\n" ();
  check_parse "?\\x09" "?\\t" ();
  check_parse "?\\\\" "?\\\\" ()

let test_char_hex () = check_parse "?A" "?\\x41" ()

let test_char_control () =
  (* ?\C-a should produce control-a (ASCII 1) *)
  let result = parse_one "?\\C-a" in
  match result with
  | Ok (Char (c, _)) -> Alcotest.(check int) "control-a" 1 c
  | Ok _ -> Alcotest.fail "expected Char"
  | Error msg -> Alcotest.fail msg

let test_char_meta () =
  (* ?\M-x should produce meta-x (x with meta bit) *)
  let result = parse_one "?\\M-x" in
  match result with
  | Ok (Char (c, _)) ->
      Alcotest.(check int) "meta-x" (0x8000000 lor Char.code 'x') c
  | Ok _ -> Alcotest.fail "expected Char"
  | Error msg -> Alcotest.fail msg

(** Test lists *)
let test_empty_list () = check_parse "()" "()" ()

let test_simple_list () = check_parse "(a b c)" "(a b c)" ()
let test_nested_list () = check_parse "(a (b c) d)" "(a (b c) d)" ()
let test_dotted_pair () = check_parse "(a . b)" "(a . b)" ()
let test_improper_list () = check_parse "(a b . c)" "(a b . c)" ()

(** Test vectors *)
let test_vector () = check_parse "#(1 2 3)" "#(1 2 3)" ()

let test_bracket_vector () = check_parse "#(a b c)" "[a b c]" ()

(** Test reader macros *)
let test_quote () = check_parse "'x" "'x" ()

let test_backquote () = check_parse "`x" "`x" ()
let test_unquote () = check_parse ",x" ",x" ()
let test_unquote_splicing () = check_parse ",@x" ",@x" ()
let test_function () = check_parse "#'foo" "#'foo" ()

let test_nested_quotes () =
  check_parse "'(a b c)" "'(a b c)" ();
  check_parse "`(a ,b ,@c)" "`(a ,b ,@c)" ()

(** Test comments *)
let test_line_comment () = check_parse "foo" "; comment\nfoo" ()

let test_block_comment () = check_parse "foo" "#| block comment |# foo" ()

let test_nested_block_comment () =
  check_parse "foo" "#| outer #| inner |# outer |# foo" ()

(** Test multiple expressions *)
let test_multiple_sexps () = check_parse_all "a b c" "a b c" ()

let test_multiple_with_comments () = check_parse_all "a b" "a ; comment\n b" ()

(** Test source locations *)
let test_location () =
  let result = parse_one "foo" in
  match result with
  | Ok (Symbol (_, span)) ->
      Alcotest.(check int) "start line" 1 span.start_pos.line;
      Alcotest.(check int) "start col" 0 span.start_pos.col;
      Alcotest.(check int) "end col" 3 span.end_pos.col
  | Ok _ -> Alcotest.fail "expected Symbol"
  | Error msg -> Alcotest.fail msg

let test_location_multiline () =
  let result = parse_string "(foo\n  bar)" in
  match (result.errors, result.sexps) with
  | [], [ List (_, span) ] ->
      Alcotest.(check int) "start line" 1 span.start_pos.line;
      Alcotest.(check int) "end line" 2 span.end_pos.line
  | [], _ -> Alcotest.fail "expected single List"
  | err :: _, _ -> Alcotest.fail err.message

(** Test error recovery *)
let test_error_recovery () =
  let result = parse_string "(foo (bar) baz" in
  (* Should have an error but still parse what it can *)
  Alcotest.(check bool) "has errors" true (List.length result.errors > 0)

(** Test fixture: real Elisp code *)
let test_defun () =
  check_parse "(defun foo (x) (+ x 1))" "(defun foo (x) (+ x 1))" ()

let test_let () =
  check_parse "(let ((x 1) (y 2)) (+ x y))" "(let ((x 1) (y 2)) (+ x y))" ()

let test_lambda () =
  check_parse "(lambda (x) (* x x))" "(lambda (x) (* x x))" ()

let test_if () = check_parse "(if test then else)" "(if test then else)" ()

let test_cond () =
  check_parse "(cond ((= x 0) 'zero) (t 'other))"
    "(cond ((= x 0) 'zero) (t 'other))" ()

let () =
  Alcotest.run "parser"
    [
      ( "integers",
        [
          Alcotest.test_case "basic" `Quick test_integers;
          Alcotest.test_case "hex" `Quick test_hex_numbers;
          Alcotest.test_case "octal" `Quick test_octal_numbers;
        ] );
      ("floats", [ Alcotest.test_case "basic" `Quick test_floats ]);
      ("symbols", [ Alcotest.test_case "basic" `Quick test_symbols ]);
      ("keywords", [ Alcotest.test_case "basic" `Quick test_keywords ]);
      ( "strings",
        [
          Alcotest.test_case "basic" `Quick test_strings;
          Alcotest.test_case "escapes" `Quick test_string_escapes;
        ] );
      ( "characters",
        [
          Alcotest.test_case "basic" `Quick test_char_literals;
          Alcotest.test_case "escapes" `Quick test_char_escapes;
          Alcotest.test_case "hex" `Quick test_char_hex;
          Alcotest.test_case "control" `Quick test_char_control;
          Alcotest.test_case "meta" `Quick test_char_meta;
        ] );
      ( "lists",
        [
          Alcotest.test_case "empty" `Quick test_empty_list;
          Alcotest.test_case "simple" `Quick test_simple_list;
          Alcotest.test_case "nested" `Quick test_nested_list;
          Alcotest.test_case "dotted" `Quick test_dotted_pair;
          Alcotest.test_case "improper" `Quick test_improper_list;
        ] );
      ( "vectors",
        [
          Alcotest.test_case "hash-paren" `Quick test_vector;
          Alcotest.test_case "brackets" `Quick test_bracket_vector;
        ] );
      ( "reader-macros",
        [
          Alcotest.test_case "quote" `Quick test_quote;
          Alcotest.test_case "backquote" `Quick test_backquote;
          Alcotest.test_case "unquote" `Quick test_unquote;
          Alcotest.test_case "unquote-splicing" `Quick test_unquote_splicing;
          Alcotest.test_case "function" `Quick test_function;
          Alcotest.test_case "nested" `Quick test_nested_quotes;
        ] );
      ( "comments",
        [
          Alcotest.test_case "line" `Quick test_line_comment;
          Alcotest.test_case "block" `Quick test_block_comment;
          Alcotest.test_case "nested-block" `Quick test_nested_block_comment;
        ] );
      ( "multiple",
        [
          Alcotest.test_case "sexps" `Quick test_multiple_sexps;
          Alcotest.test_case "with-comments" `Quick test_multiple_with_comments;
        ] );
      ( "locations",
        [
          Alcotest.test_case "basic" `Quick test_location;
          Alcotest.test_case "multiline" `Quick test_location_multiline;
        ] );
      ("recovery", [ Alcotest.test_case "error" `Quick test_error_recovery ]);
      ( "fixtures",
        [
          Alcotest.test_case "defun" `Quick test_defun;
          Alcotest.test_case "let" `Quick test_let;
          Alcotest.test_case "lambda" `Quick test_lambda;
          Alcotest.test_case "if" `Quick test_if;
          Alcotest.test_case "cond" `Quick test_cond;
        ] );
    ]
