(** Tests for the Elisp interpreter *)

open Tart.Value
open Tart.Eval

(** Helper to evaluate a string and check the result *)
let eval_check expected source () =
  let global = make_interpreter () in
  match eval_string global source with
  | Result.Ok value ->
      Alcotest.(check string) "eval result" expected (to_string value)
  | Result.Error e ->
      Alcotest.fail
        (Printf.sprintf "eval error at %d:%d: %s" e.span.start_pos.line
           e.span.start_pos.col e.message)

(** Helper to check evaluation produces an error *)
let _eval_error_check msg_contains source () =
  let global = make_interpreter () in
  match eval_string global source with
  | Result.Ok value ->
      Alcotest.fail (Printf.sprintf "expected error, got %s" (to_string value))
  | Result.Error e ->
      if
        not
          (String.length msg_contains = 0
          || String.sub e.message 0
               (min (String.length msg_contains) (String.length e.message))
             = msg_contains
          || String.sub e.message 0
               (min (String.length e.message) (String.length msg_contains))
             = msg_contains)
      then
        Alcotest.fail
          (Printf.sprintf "error message mismatch: expected '%s' in '%s'"
             msg_contains e.message)

(* =============================================================================
   Literals
   ============================================================================= *)

let test_int () = eval_check "42" "42" ()
let test_negative_int () = eval_check "-17" "-17" ()
let test_float () = eval_check "3.14" "3.14" ()
let test_string () = eval_check "\"hello\"" "\"hello\"" ()
let test_nil () = eval_check "nil" "nil" ()
let test_t () = eval_check "t" "t" ()
let test_keyword () = eval_check ":foo" ":foo" ()

(* =============================================================================
   Quote
   ============================================================================= *)

let test_quote_symbol () = eval_check "foo" "'foo" ()
let test_quote_list () = eval_check "(1 2 3)" "'(1 2 3)" ()
let test_quote_nested () = eval_check "((a b) (c d))" "'((a b) (c d))" ()

(* =============================================================================
   Arithmetic
   ============================================================================= *)

let test_add () = eval_check "6" "(+ 1 2 3)" ()
let test_add_zero () = eval_check "0" "(+)" ()
let test_sub () = eval_check "5" "(- 10 3 2)" ()
let test_sub_negate () = eval_check "-5" "(- 5)" ()
let test_mul () = eval_check "24" "(* 2 3 4)" ()
let test_mul_one () = eval_check "1" "(*)" ()
let test_div () = eval_check "5" "(/ 20 2 2)" ()
let test_mod () = eval_check "1" "(mod 7 3)" ()
let test_1plus () = eval_check "6" "(1+ 5)" ()
let test_1minus () = eval_check "4" "(1- 5)" ()

(* =============================================================================
   Comparison
   ============================================================================= *)

let test_lt () = eval_check "t" "(< 1 2 3)" ()
let test_lt_false () = eval_check "nil" "(< 1 3 2)" ()
let test_gt () = eval_check "t" "(> 3 2 1)" ()
let test_le () = eval_check "t" "(<= 1 1 2)" ()
let test_ge () = eval_check "t" "(>= 3 3 2)" ()
let test_eq_num () = eval_check "t" "(= 5 5 5)" ()
let test_eq_num_false () = eval_check "nil" "(= 1 2)" ()

(* =============================================================================
   Predicates
   ============================================================================= *)

let test_null_nil () = eval_check "t" "(null nil)" ()
let test_null_list () = eval_check "nil" "(null '(1))" ()
let test_atom_symbol () = eval_check "t" "(atom 'foo)" ()
let test_atom_list () = eval_check "nil" "(atom '(1 2))" ()
let test_listp_list () = eval_check "t" "(listp '(1 2))" ()
let test_listp_nil () = eval_check "t" "(listp nil)" ()
let test_listp_atom () = eval_check "nil" "(listp 42)" ()
let test_stringp () = eval_check "t" "(stringp \"hello\")" ()
let test_numberp () = eval_check "t" "(numberp 42)" ()
let test_integerp () = eval_check "t" "(integerp 42)" ()
let test_not_nil () = eval_check "t" "(not nil)" ()
let test_not_t () = eval_check "nil" "(not t)" ()

(* =============================================================================
   List operations
   ============================================================================= *)

let test_car () = eval_check "1" "(car '(1 2 3))" ()
let test_car_nil () = eval_check "nil" "(car nil)" ()
let test_cdr () = eval_check "(2 3)" "(cdr '(1 2 3))" ()
let test_cdr_nil () = eval_check "nil" "(cdr nil)" ()
let test_cons () = eval_check "(1 2 3)" "(cons 1 '(2 3))" ()
let test_cons_dotted () = eval_check "(1 . 2)" "(cons 1 2)" ()
let test_list_fn () = eval_check "(1 2 3)" "(list 1 2 3)" ()
let test_list_empty () = eval_check "nil" "(list)" ()
let test_length () = eval_check "3" "(length '(a b c))" ()
let test_nth () = eval_check "b" "(nth 1 '(a b c))" ()
let test_append () = eval_check "(1 2 3 4)" "(append '(1 2) '(3 4))" ()
let test_reverse () = eval_check "(3 2 1)" "(reverse '(1 2 3))" ()

(* =============================================================================
   String operations
   ============================================================================= *)

let test_concat () =
  eval_check "\"hello world\"" "(concat \"hello\" \" \" \"world\")" ()

let test_substring () = eval_check "\"ell\"" "(substring \"hello\" 1 4)" ()
let test_upcase () = eval_check "\"HELLO\"" "(upcase \"hello\")" ()
let test_downcase () = eval_check "\"hello\"" "(downcase \"HELLO\")" ()

(* =============================================================================
   Control flow
   ============================================================================= *)

let test_if_then () = eval_check "1" "(if t 1 2)" ()
let test_if_else () = eval_check "2" "(if nil 1 2)" ()
let test_if_no_else () = eval_check "nil" "(if nil 1)" ()
let test_if_multiple_else () = eval_check "3" "(if nil 1 2 3)" ()
let test_and_empty () = eval_check "t" "(and)" ()
let test_and_true () = eval_check "3" "(and 1 2 3)" ()
let test_and_short () = eval_check "nil" "(and 1 nil 3)" ()
let test_or_empty () = eval_check "nil" "(or)" ()
let test_or_first () = eval_check "1" "(or 1 2 3)" ()
let test_or_skip () = eval_check "2" "(or nil 2 3)" ()
let test_progn () = eval_check "3" "(progn 1 2 3)" ()
let test_progn_empty () = eval_check "nil" "(progn)" ()

let test_cond () =
  eval_check "one" "(cond ((= 1 2) 'wrong) ((= 1 1) 'one) (t 'default))" ()

let test_cond_default () =
  eval_check "default" "(cond ((= 1 2) 'wrong) (t 'default))" ()

(* =============================================================================
   Let bindings
   ============================================================================= *)

let test_let () = eval_check "3" "(let ((x 1) (y 2)) (+ x y))" ()
let test_let_empty_body () = eval_check "nil" "(let ((x 1)))" ()
let test_let_shadow () = eval_check "2" "(let ((x 1)) (let ((x 2)) x))" ()

let test_let_parallel () =
  (* In let, bindings are parallel - y should see the outer x *)
  eval_check "11" "(let ((x 10)) (let ((x 1) (y x)) (+ x y)))" ()

let test_let_star () = eval_check "3" "(let* ((x 1) (y (+ x 1))) (+ x y))" ()

let test_let_star_seq () =
  (* In let*, y sees the new x *)
  eval_check "2" "(let* ((x 1) (y x)) (+ x y))" ()

(* =============================================================================
   Lambda and function calls
   ============================================================================= *)

let test_lambda () = eval_check "3" "((lambda (x) (+ x 1)) 2)" ()
let test_lambda_no_args () = eval_check "42" "((lambda () 42))" ()

let test_lambda_multiple () =
  eval_check "6" "((lambda (a b c) (+ a b c)) 1 2 3)" ()

let test_defun () =
  eval_check "6" "(progn (defun double (x) (* x 2)) (double 3))" ()

let test_defun_recursive () =
  eval_check "6"
    "(progn (defun fact (n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 3))" ()

let test_funcall () = eval_check "4" "(funcall (lambda (x) (* x 2)) 2)" ()
let test_apply () = eval_check "6" "(apply '+ '(1 2 3))" ()
let test_mapcar () = eval_check "(2 3 4)" "(mapcar '1+ '(1 2 3))" ()

(* =============================================================================
   Setq
   ============================================================================= *)

let test_setq () = eval_check "5" "(let ((x 1)) (setq x 5) x)" ()

let test_setq_multiple () =
  eval_check "3" "(let ((x 0) (y 0)) (setq x 1 y 2) (+ x y))" ()

let test_setq_global () =
  eval_check "42" "(progn (setq my-global 42) my-global)" ()

(* =============================================================================
   Macros
   ============================================================================= *)

let test_defmacro () =
  eval_check "5"
    "(progn (defmacro incf (x) (list 'setq x (list '+ x 1))) (let ((y 4)) \
     (incf y) y))"
    ()

let test_when_macro () =
  eval_check "done"
    "(progn (defmacro when (test &rest body) (list 'if test (cons 'progn \
     body))) (when t 'done))"
    ()

(* =============================================================================
   Test runner
   ============================================================================= *)

let () =
  Alcotest.run "eval"
    [
      ( "literals",
        [
          Alcotest.test_case "int" `Quick test_int;
          Alcotest.test_case "negative-int" `Quick test_negative_int;
          Alcotest.test_case "float" `Quick test_float;
          Alcotest.test_case "string" `Quick test_string;
          Alcotest.test_case "nil" `Quick test_nil;
          Alcotest.test_case "t" `Quick test_t;
          Alcotest.test_case "keyword" `Quick test_keyword;
        ] );
      ( "quote",
        [
          Alcotest.test_case "symbol" `Quick test_quote_symbol;
          Alcotest.test_case "list" `Quick test_quote_list;
          Alcotest.test_case "nested" `Quick test_quote_nested;
        ] );
      ( "arithmetic",
        [
          Alcotest.test_case "add" `Quick test_add;
          Alcotest.test_case "add-zero" `Quick test_add_zero;
          Alcotest.test_case "sub" `Quick test_sub;
          Alcotest.test_case "sub-negate" `Quick test_sub_negate;
          Alcotest.test_case "mul" `Quick test_mul;
          Alcotest.test_case "mul-one" `Quick test_mul_one;
          Alcotest.test_case "div" `Quick test_div;
          Alcotest.test_case "mod" `Quick test_mod;
          Alcotest.test_case "1+" `Quick test_1plus;
          Alcotest.test_case "1-" `Quick test_1minus;
        ] );
      ( "comparison",
        [
          Alcotest.test_case "<" `Quick test_lt;
          Alcotest.test_case "<-false" `Quick test_lt_false;
          Alcotest.test_case ">" `Quick test_gt;
          Alcotest.test_case "<=" `Quick test_le;
          Alcotest.test_case ">=" `Quick test_ge;
          Alcotest.test_case "=" `Quick test_eq_num;
          Alcotest.test_case "=-false" `Quick test_eq_num_false;
        ] );
      ( "predicates",
        [
          Alcotest.test_case "null-nil" `Quick test_null_nil;
          Alcotest.test_case "null-list" `Quick test_null_list;
          Alcotest.test_case "atom-symbol" `Quick test_atom_symbol;
          Alcotest.test_case "atom-list" `Quick test_atom_list;
          Alcotest.test_case "listp-list" `Quick test_listp_list;
          Alcotest.test_case "listp-nil" `Quick test_listp_nil;
          Alcotest.test_case "listp-atom" `Quick test_listp_atom;
          Alcotest.test_case "stringp" `Quick test_stringp;
          Alcotest.test_case "numberp" `Quick test_numberp;
          Alcotest.test_case "integerp" `Quick test_integerp;
          Alcotest.test_case "not-nil" `Quick test_not_nil;
          Alcotest.test_case "not-t" `Quick test_not_t;
        ] );
      ( "list-ops",
        [
          Alcotest.test_case "car" `Quick test_car;
          Alcotest.test_case "car-nil" `Quick test_car_nil;
          Alcotest.test_case "cdr" `Quick test_cdr;
          Alcotest.test_case "cdr-nil" `Quick test_cdr_nil;
          Alcotest.test_case "cons" `Quick test_cons;
          Alcotest.test_case "cons-dotted" `Quick test_cons_dotted;
          Alcotest.test_case "list" `Quick test_list_fn;
          Alcotest.test_case "list-empty" `Quick test_list_empty;
          Alcotest.test_case "length" `Quick test_length;
          Alcotest.test_case "nth" `Quick test_nth;
          Alcotest.test_case "append" `Quick test_append;
          Alcotest.test_case "reverse" `Quick test_reverse;
        ] );
      ( "string-ops",
        [
          Alcotest.test_case "concat" `Quick test_concat;
          Alcotest.test_case "substring" `Quick test_substring;
          Alcotest.test_case "upcase" `Quick test_upcase;
          Alcotest.test_case "downcase" `Quick test_downcase;
        ] );
      ( "control-flow",
        [
          Alcotest.test_case "if-then" `Quick test_if_then;
          Alcotest.test_case "if-else" `Quick test_if_else;
          Alcotest.test_case "if-no-else" `Quick test_if_no_else;
          Alcotest.test_case "if-multi-else" `Quick test_if_multiple_else;
          Alcotest.test_case "and-empty" `Quick test_and_empty;
          Alcotest.test_case "and-true" `Quick test_and_true;
          Alcotest.test_case "and-short" `Quick test_and_short;
          Alcotest.test_case "or-empty" `Quick test_or_empty;
          Alcotest.test_case "or-first" `Quick test_or_first;
          Alcotest.test_case "or-skip" `Quick test_or_skip;
          Alcotest.test_case "progn" `Quick test_progn;
          Alcotest.test_case "progn-empty" `Quick test_progn_empty;
          Alcotest.test_case "cond" `Quick test_cond;
          Alcotest.test_case "cond-default" `Quick test_cond_default;
        ] );
      ( "let",
        [
          Alcotest.test_case "basic" `Quick test_let;
          Alcotest.test_case "empty-body" `Quick test_let_empty_body;
          Alcotest.test_case "shadow" `Quick test_let_shadow;
          Alcotest.test_case "parallel" `Quick test_let_parallel;
          Alcotest.test_case "let*" `Quick test_let_star;
          Alcotest.test_case "let*-seq" `Quick test_let_star_seq;
        ] );
      ( "functions",
        [
          Alcotest.test_case "lambda" `Quick test_lambda;
          Alcotest.test_case "lambda-no-args" `Quick test_lambda_no_args;
          Alcotest.test_case "lambda-multi" `Quick test_lambda_multiple;
          Alcotest.test_case "defun" `Quick test_defun;
          Alcotest.test_case "defun-recursive" `Quick test_defun_recursive;
          Alcotest.test_case "funcall" `Quick test_funcall;
          Alcotest.test_case "apply" `Quick test_apply;
          Alcotest.test_case "mapcar" `Quick test_mapcar;
        ] );
      ( "setq",
        [
          Alcotest.test_case "basic" `Quick test_setq;
          Alcotest.test_case "multiple" `Quick test_setq_multiple;
          Alcotest.test_case "global" `Quick test_setq_global;
        ] );
      ( "macros",
        [
          Alcotest.test_case "defmacro" `Quick test_defmacro;
          Alcotest.test_case "when" `Quick test_when_macro;
        ] );
    ]
