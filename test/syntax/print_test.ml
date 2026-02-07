(** Tests for the Elisp S-expression printer. *)

module Print = Tart.Print
module Sexp = Tart.Sexp
module Read = Tart.Read

(** Dummy span for constructing test AST nodes. *)
let s = Tart.Location.dummy_span

(* =============================================================================
   escape_string_char: special escapes
   ============================================================================= *)

let test_escape_newline () =
  Alcotest.(check string) "newline" "\\n" (Print.escape_string_char '\n')

let test_escape_tab () =
  Alcotest.(check string) "tab" "\\t" (Print.escape_string_char '\t')

let test_escape_cr () =
  Alcotest.(check string)
    "carriage return" "\\r"
    (Print.escape_string_char '\r')

let test_escape_backslash () =
  Alcotest.(check string) "backslash" "\\\\" (Print.escape_string_char '\\')

let test_escape_dquote () =
  Alcotest.(check string) "double quote" "\\\"" (Print.escape_string_char '"')

let test_escape_bell () =
  Alcotest.(check string) "bell" "\\a" (Print.escape_string_char '\007')

let test_escape_backspace () =
  Alcotest.(check string) "backspace" "\\b" (Print.escape_string_char '\b')

let test_escape_formfeed () =
  Alcotest.(check string) "form feed" "\\f" (Print.escape_string_char '\012')

let test_escape_esc () =
  Alcotest.(check string) "escape" "\\e" (Print.escape_string_char '\027')

let test_escape_printable () =
  Alcotest.(check string) "printable a" "a" (Print.escape_string_char 'a');
  Alcotest.(check string) "printable z" "z" (Print.escape_string_char 'z');
  Alcotest.(check string) "printable space" " " (Print.escape_string_char ' ');
  Alcotest.(check string) "printable tilde" "~" (Print.escape_string_char '~')

let test_escape_hex () =
  (* Control chars without named escapes use \xNN *)
  Alcotest.(check string) "NUL" "\\x00" (Print.escape_string_char '\000');
  Alcotest.(check string) "SOH" "\\x01" (Print.escape_string_char '\001')

(* =============================================================================
   escape_string: full strings
   ============================================================================= *)

let test_escape_string_plain () =
  Alcotest.(check string) "plain" "hello" (Print.escape_string "hello")

let test_escape_string_empty () =
  Alcotest.(check string) "empty" "" (Print.escape_string "")

let test_escape_string_mixed () =
  Alcotest.(check string)
    "mixed" "line1\\nline2\\t\\\"end\\\\"
    (Print.escape_string "line1\nline2\t\"end\\")

let test_escape_string_all_special () =
  Alcotest.(check string)
    "all special" "\\a\\b\\f\\e\\r\\n\\t"
    (Print.escape_string "\007\b\012\027\r\n\t")

(* =============================================================================
   print_char: simple characters
   ============================================================================= *)

let test_char_printable () =
  Alcotest.(check string) "?a" "?a" (Print.print_char (Char.code 'a'));
  Alcotest.(check string) "?z" "?z" (Print.print_char (Char.code 'z'));
  Alcotest.(check string) "?A" "?A" (Print.print_char (Char.code 'A'))

let test_char_special () =
  (* Bare control codes 9/10/13 are ≤31 so they go through the control-char path *)
  Alcotest.(check string) "newline as C-J" "?\\C-J" (Print.print_char 10);
  Alcotest.(check string) "tab as C-I" "?\\C-I" (Print.print_char 9);
  Alcotest.(check string) "return as C-M" "?\\C-M" (Print.print_char 13);
  Alcotest.(check string) "backslash" "?\\\\" (Print.print_char 92);
  Alcotest.(check string) "space" "?\\s" (Print.print_char 32);
  Alcotest.(check string) "delete" "?\\d" (Print.print_char 127)

let test_char_control () =
  (* Control-X = char code 24 *)
  Alcotest.(check string) "C-x" "?\\C-X" (Print.print_char 24);
  (* Control-A = char code 1 *)
  Alcotest.(check string) "C-a" "?\\C-A" (Print.print_char 1)

(* =============================================================================
   print_char: modifier bits
   ============================================================================= *)

let test_char_meta () =
  let code = Print.Modifiers.meta_bit lor Char.code 'x' in
  Alcotest.(check string) "M-x" "?\\M-x" (Print.print_char code)

let test_char_control_explicit () =
  let code = Print.Modifiers.control_bit lor Char.code 'x' in
  Alcotest.(check string) "C-x (explicit)" "?\\C-x" (Print.print_char code)

let test_char_shift () =
  let code = Print.Modifiers.shift_bit lor Char.code 'x' in
  Alcotest.(check string) "S-x" "?\\S-x" (Print.print_char code)

let test_char_hyper () =
  let code = Print.Modifiers.hyper_bit lor Char.code 'x' in
  Alcotest.(check string) "H-x" "?\\H-x" (Print.print_char code)

let test_char_super () =
  let code = Print.Modifiers.super_bit lor Char.code 'x' in
  Alcotest.(check string) "s-x" "?\\s-x" (Print.print_char code)

let test_char_alt () =
  let code = Print.Modifiers.alt_bit lor Char.code 'x' in
  Alcotest.(check string) "A-x" "?\\A-x" (Print.print_char code)

let test_char_meta_control () =
  let code =
    Print.Modifiers.meta_bit lor Print.Modifiers.control_bit lor Char.code 'x'
  in
  Alcotest.(check string) "C-M-x" "?\\M-\\C-x" (Print.print_char code)

(* =============================================================================
   Modifiers.extract: round-trip modifier encoding
   ============================================================================= *)

let test_extract_no_modifiers () =
  let base, mods = Print.Modifiers.extract (Char.code 'a') in
  Alcotest.(check int) "base" (Char.code 'a') base;
  Alcotest.(check int) "no mods" 0 (List.length mods)

let test_extract_meta () =
  let code = Print.Modifiers.meta_bit lor Char.code 'x' in
  let base, mods = Print.Modifiers.extract code in
  Alcotest.(check int) "base" (Char.code 'x') base;
  Alcotest.(check int) "1 mod" 1 (List.length mods);
  Alcotest.(check bool) "is Meta" true (List.mem `Meta mods)

let test_extract_multi () =
  let code =
    Print.Modifiers.meta_bit lor Print.Modifiers.control_bit lor Char.code 'a'
  in
  let base, mods = Print.Modifiers.extract code in
  Alcotest.(check int) "base" (Char.code 'a') base;
  Alcotest.(check int) "2 mods" 2 (List.length mods);
  Alcotest.(check bool) "has Meta" true (List.mem `Meta mods);
  Alcotest.(check bool) "has Control" true (List.mem `Control mods)

let test_extract_all_six () =
  let code =
    Print.Modifiers.meta_bit lor Print.Modifiers.control_bit
    lor Print.Modifiers.shift_bit lor Print.Modifiers.hyper_bit
    lor Print.Modifiers.super_bit lor Print.Modifiers.alt_bit lor Char.code 'a'
  in
  let base, mods = Print.Modifiers.extract code in
  Alcotest.(check int) "base" (Char.code 'a') base;
  Alcotest.(check int) "6 mods" 6 (List.length mods)

(* =============================================================================
   to_string: atoms
   ============================================================================= *)

let test_print_int () =
  Alcotest.(check string) "42" "42" (Print.to_string (Sexp.Int (42, s)));
  Alcotest.(check string) "-1" "-1" (Print.to_string (Sexp.Int (-1, s)));
  Alcotest.(check string) "0" "0" (Print.to_string (Sexp.Int (0, s)))

let test_print_float () =
  Alcotest.(check string) "3.14" "3.14" (Print.to_string (Sexp.Float (3.14, s)));
  Alcotest.(check string) "-2.5" "-2.5" (Print.to_string (Sexp.Float (-2.5, s)))

let test_print_string () =
  Alcotest.(check string)
    "hello" "\"hello\""
    (Print.to_string (Sexp.String ("hello", s)));
  Alcotest.(check string) "empty" "\"\"" (Print.to_string (Sexp.String ("", s)));
  Alcotest.(check string)
    "escaped" "\"a\\nb\""
    (Print.to_string (Sexp.String ("a\nb", s)))

let test_print_symbol () =
  Alcotest.(check string) "foo" "foo" (Print.to_string (Sexp.Symbol ("foo", s)));
  Alcotest.(check string) "nil" "nil" (Print.to_string (Sexp.Symbol ("nil", s)));
  Alcotest.(check string) "+" "+" (Print.to_string (Sexp.Symbol ("+", s)))

let test_print_keyword () =
  Alcotest.(check string)
    ":foo" ":foo"
    (Print.to_string (Sexp.Keyword ("foo", s)));
  Alcotest.(check string)
    ":bar" ":bar"
    (Print.to_string (Sexp.Keyword ("bar", s)))

let test_print_char_literal () =
  Alcotest.(check string)
    "?a" "?a"
    (Print.to_string (Sexp.Char (Char.code 'a', s)))

(* =============================================================================
   to_string: reader macros
   ============================================================================= *)

let test_print_quote () =
  let sexp =
    Sexp.List ([ Sexp.Symbol ("quote", s); Sexp.Symbol ("x", s) ], s)
  in
  Alcotest.(check string) "'x" "'x" (Print.to_string sexp)

let test_print_backquote () =
  let sexp =
    Sexp.List ([ Sexp.Symbol ("backquote", s); Sexp.Symbol ("x", s) ], s)
  in
  Alcotest.(check string) "`x" "`x" (Print.to_string sexp)

let test_print_unquote () =
  let sexp =
    Sexp.List ([ Sexp.Symbol ("unquote", s); Sexp.Symbol ("x", s) ], s)
  in
  Alcotest.(check string) ",x" ",x" (Print.to_string sexp)

let test_print_unquote_splicing () =
  let sexp =
    Sexp.List ([ Sexp.Symbol ("unquote-splicing", s); Sexp.Symbol ("x", s) ], s)
  in
  Alcotest.(check string) ",@x" ",@x" (Print.to_string sexp)

let test_print_function () =
  let sexp =
    Sexp.List ([ Sexp.Symbol ("function", s); Sexp.Symbol ("car", s) ], s)
  in
  Alcotest.(check string) "#'car" "#'car" (Print.to_string sexp)

(* =============================================================================
   to_string: compound forms
   ============================================================================= *)

let test_print_list () =
  let sexp =
    Sexp.List ([ Sexp.Symbol ("+", s); Sexp.Int (1, s); Sexp.Int (2, s) ], s)
  in
  Alcotest.(check string) "(+ 1 2)" "(+ 1 2)" (Print.to_string sexp)

let test_print_empty_list () =
  Alcotest.(check string) "()" "()" (Print.to_string (Sexp.List ([], s)))

let test_print_nested_list () =
  let inner = Sexp.List ([ Sexp.Int (1, s); Sexp.Int (2, s) ], s) in
  let outer = Sexp.List ([ Sexp.Symbol ("list", s); inner ], s) in
  Alcotest.(check string) "(list (1 2))" "(list (1 2))" (Print.to_string outer)

let test_print_vector () =
  let sexp = Sexp.Vector ([ Sexp.Int (1, s); Sexp.Int (2, s) ], s) in
  Alcotest.(check string) "[1 2]" "[1 2]" (Print.to_string sexp)

let test_print_empty_vector () =
  Alcotest.(check string) "[]" "[]" (Print.to_string (Sexp.Vector ([], s)))

let test_print_curly () =
  let sexp = Sexp.Curly ([ Sexp.Keyword ("a", s); Sexp.Int (1, s) ], s) in
  Alcotest.(check string) "{:a 1}" "{:a 1}" (Print.to_string sexp)

let test_print_cons () =
  let sexp = Sexp.Cons (Sexp.Int (1, s), Sexp.Int (2, s), s) in
  Alcotest.(check string) "(1 . 2)" "(1 . 2)" (Print.to_string sexp)

let test_print_nested_cons () =
  let sexp =
    Sexp.Cons
      (Sexp.Int (1, s), Sexp.Cons (Sexp.Int (2, s), Sexp.Int (3, s), s), s)
  in
  Alcotest.(check string) "(1 2 . 3)" "(1 2 . 3)" (Print.to_string sexp)

let test_print_error () =
  let sexp = Sexp.Error ("bad", s) in
  Alcotest.(check string) "error" "#<error: bad>" (Print.to_string sexp)

(* =============================================================================
   Round-trip: to_string then parse_one yields equivalent AST
   ============================================================================= *)

(** Parse a string, print it, re-parse, and check string equality. *)
let round_trip input () =
  match Read.parse_one input with
  | Error msg -> Alcotest.fail ("initial parse failed: " ^ msg)
  | Ok sexp -> (
      let printed = Print.to_string sexp in
      match Read.parse_one printed with
      | Error msg ->
          Alcotest.fail (Printf.sprintf "re-parse of %S failed: %s" printed msg)
      | Ok sexp2 ->
          let reprinted = Print.to_string sexp2 in
          Alcotest.(check string)
            (Printf.sprintf "round-trip %S" input)
            printed reprinted)

let test_round_trip_int () = round_trip "42" ()
let test_round_trip_float () = round_trip "3.14" ()
let test_round_trip_neg_int () = round_trip "-17" ()
let test_round_trip_symbol () = round_trip "foo-bar" ()
let test_round_trip_keyword () = round_trip ":baz" ()
let test_round_trip_string () = round_trip "\"hello world\"" ()
let test_round_trip_string_escaped () = round_trip "\"line1\\nline2\"" ()
let test_round_trip_list () = round_trip "(+ 1 2)" ()
let test_round_trip_nested () = round_trip "(defun f (x) (+ x 1))" ()
let test_round_trip_quote () = round_trip "'foo" ()
let test_round_trip_backquote () = round_trip "`(a ,b ,@c)" ()
let test_round_trip_vector () = round_trip "#(1 2 3)" ()
let test_round_trip_cons () = round_trip "(1 . 2)" ()
let test_round_trip_char () = round_trip "?a" ()
let test_round_trip_empty_list () = round_trip "()" ()
let test_round_trip_function () = round_trip "#'car" ()

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "print"
    [
      ( "escape_string_char",
        [
          Alcotest.test_case "newline" `Quick test_escape_newline;
          Alcotest.test_case "tab" `Quick test_escape_tab;
          Alcotest.test_case "carriage return" `Quick test_escape_cr;
          Alcotest.test_case "backslash" `Quick test_escape_backslash;
          Alcotest.test_case "double quote" `Quick test_escape_dquote;
          Alcotest.test_case "bell" `Quick test_escape_bell;
          Alcotest.test_case "backspace" `Quick test_escape_backspace;
          Alcotest.test_case "form feed" `Quick test_escape_formfeed;
          Alcotest.test_case "escape char" `Quick test_escape_esc;
          Alcotest.test_case "printable" `Quick test_escape_printable;
          Alcotest.test_case "hex" `Quick test_escape_hex;
        ] );
      ( "escape_string",
        [
          Alcotest.test_case "plain" `Quick test_escape_string_plain;
          Alcotest.test_case "empty" `Quick test_escape_string_empty;
          Alcotest.test_case "mixed" `Quick test_escape_string_mixed;
          Alcotest.test_case "all special" `Quick test_escape_string_all_special;
        ] );
      ( "print_char: simple",
        [
          Alcotest.test_case "printable" `Quick test_char_printable;
          Alcotest.test_case "special" `Quick test_char_special;
          Alcotest.test_case "control" `Quick test_char_control;
        ] );
      ( "print_char: modifiers",
        [
          Alcotest.test_case "Meta" `Quick test_char_meta;
          Alcotest.test_case "Control (explicit)" `Quick
            test_char_control_explicit;
          Alcotest.test_case "Shift" `Quick test_char_shift;
          Alcotest.test_case "Hyper" `Quick test_char_hyper;
          Alcotest.test_case "Super" `Quick test_char_super;
          Alcotest.test_case "Alt" `Quick test_char_alt;
          Alcotest.test_case "Meta+Control" `Quick test_char_meta_control;
        ] );
      ( "Modifiers.extract",
        [
          Alcotest.test_case "no modifiers" `Quick test_extract_no_modifiers;
          Alcotest.test_case "Meta" `Quick test_extract_meta;
          Alcotest.test_case "multi" `Quick test_extract_multi;
          Alcotest.test_case "all six" `Quick test_extract_all_six;
        ] );
      ( "to_string: atoms",
        [
          Alcotest.test_case "int" `Quick test_print_int;
          Alcotest.test_case "float" `Quick test_print_float;
          Alcotest.test_case "string" `Quick test_print_string;
          Alcotest.test_case "symbol" `Quick test_print_symbol;
          Alcotest.test_case "keyword" `Quick test_print_keyword;
          Alcotest.test_case "char" `Quick test_print_char_literal;
        ] );
      ( "to_string: reader macros",
        [
          Alcotest.test_case "quote" `Quick test_print_quote;
          Alcotest.test_case "backquote" `Quick test_print_backquote;
          Alcotest.test_case "unquote" `Quick test_print_unquote;
          Alcotest.test_case "unquote-splicing" `Quick
            test_print_unquote_splicing;
          Alcotest.test_case "function" `Quick test_print_function;
        ] );
      ( "to_string: compound",
        [
          Alcotest.test_case "list" `Quick test_print_list;
          Alcotest.test_case "empty list" `Quick test_print_empty_list;
          Alcotest.test_case "nested list" `Quick test_print_nested_list;
          Alcotest.test_case "vector" `Quick test_print_vector;
          Alcotest.test_case "empty vector" `Quick test_print_empty_vector;
          Alcotest.test_case "curly" `Quick test_print_curly;
          Alcotest.test_case "cons" `Quick test_print_cons;
          Alcotest.test_case "nested cons" `Quick test_print_nested_cons;
          Alcotest.test_case "error" `Quick test_print_error;
        ] );
      ( "round-trip",
        [
          Alcotest.test_case "int" `Quick test_round_trip_int;
          Alcotest.test_case "float" `Quick test_round_trip_float;
          Alcotest.test_case "negative int" `Quick test_round_trip_neg_int;
          Alcotest.test_case "symbol" `Quick test_round_trip_symbol;
          Alcotest.test_case "keyword" `Quick test_round_trip_keyword;
          Alcotest.test_case "string" `Quick test_round_trip_string;
          Alcotest.test_case "escaped string" `Quick
            test_round_trip_string_escaped;
          Alcotest.test_case "list" `Quick test_round_trip_list;
          Alcotest.test_case "nested" `Quick test_round_trip_nested;
          Alcotest.test_case "quote" `Quick test_round_trip_quote;
          Alcotest.test_case "backquote" `Quick test_round_trip_backquote;
          Alcotest.test_case "vector" `Quick test_round_trip_vector;
          Alcotest.test_case "cons" `Quick test_round_trip_cons;
          Alcotest.test_case "char" `Quick test_round_trip_char;
          Alcotest.test_case "empty list" `Quick test_round_trip_empty_list;
          Alcotest.test_case "function" `Quick test_round_trip_function;
        ] );
    ]
