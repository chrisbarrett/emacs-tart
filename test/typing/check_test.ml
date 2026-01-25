(** Tests for the top-level type checking API *)

open Tart.Types
module Env = Tart.Type_env
module Check = Tart.Check

(** Parse a string to S-expression for testing *)
let parse str =
  match Tart.Read.parse_one ~filename:"<test>" str with
  | Ok sexp -> sexp
  | Error msg -> failwith ("parse error: " ^ msg)

let parse_many str =
  Tart.Read.parse_string_exn ~filename:"<test>" str

(* =============================================================================
   check_expr Tests
   ============================================================================= *)

let test_check_expr_literal () =
  let sexp = parse "42" in
  let ty, errors = Check.check_expr sexp in
  Alcotest.(check string) "int type" "Int" (to_string ty);
  Alcotest.(check int) "no errors" 0 (List.length errors)

let test_check_expr_application () =
  let sexp = parse "(f 1)" in
  let env = Env.extend_mono "f" (arrow [Prim.int] Prim.string) Env.empty in
  let ty, errors = Check.check_expr ~env sexp in
  Alcotest.(check string) "return type" "String" (to_string ty);
  Alcotest.(check int) "no errors" 0 (List.length errors)

let test_check_expr_type_error () =
  let sexp = parse "(+ 1 \"hello\")" in
  let env = Env.extend_mono "+" (arrow [Prim.int; Prim.int] Prim.int) Env.empty in
  let _, errors = Check.check_expr ~env sexp in
  Alcotest.(check bool) "has error" true (List.length errors > 0)

(* =============================================================================
   check_form Tests
   ============================================================================= *)

let test_check_form_defun () =
  let sexp = parse "(defun foo () 42)" in
  let env', result, _ = Check.check_form Env.empty sexp in
  (* Should bind foo in environment *)
  Alcotest.(check bool) "foo bound" true
    (Option.is_some (Env.lookup "foo" env'));
  (* Result should be DefunForm *)
  match result with
  | Check.DefunForm { name; _ } ->
      Alcotest.(check string) "defun name" "foo" name
  | Check.ExprForm _ ->
      Alcotest.fail "expected DefunForm"

let test_check_form_expr () =
  let sexp = parse "42" in
  let env', result, _ = Check.check_form Env.empty sexp in
  (* Environment unchanged *)
  Alcotest.(check int) "env unchanged" 0
    (List.length env'.Env.bindings);
  (* Result should be ExprForm *)
  match result with
  | Check.ExprForm { ty } ->
      Alcotest.(check string) "expr type" "Int" (to_string ty)
  | Check.DefunForm _ ->
      Alcotest.fail "expected ExprForm"

(* =============================================================================
   check_program Tests
   ============================================================================= *)

let test_check_program_empty () =
  let result = Check.check_program [] in
  Alcotest.(check int) "no forms" 0 (List.length result.forms);
  Alcotest.(check int) "no errors" 0 (List.length result.errors)

let test_check_program_single_defun () =
  let sexps = parse_many "(defun add1 (x) (+ x 1))" in
  let env = Env.extend_mono "+" (arrow [Prim.int; Prim.int] Prim.int) Env.empty in
  let result = Check.check_program ~env sexps in
  Alcotest.(check int) "one form" 1 (List.length result.forms);
  (* add1 should be bound in final env *)
  Alcotest.(check bool) "add1 bound" true
    (Option.is_some (Env.lookup "add1" result.env))

let test_check_program_defun_sequence () =
  let sexps = parse_many "(defun foo () 1) (defun bar () (foo))" in
  let result = Check.check_program sexps in
  Alcotest.(check int) "two forms" 2 (List.length result.forms);
  (* Both should be bound *)
  Alcotest.(check bool) "foo bound" true
    (Option.is_some (Env.lookup "foo" result.env));
  Alcotest.(check bool) "bar bound" true
    (Option.is_some (Env.lookup "bar" result.env))

let test_check_program_defun_calls_previous () =
  (* bar calls foo - should type check correctly *)
  let sexps = parse_many "(defun foo () 42) (defun bar () (+ (foo) 1))" in
  let env = Env.extend_mono "+" (arrow [Prim.int; Prim.int] Prim.int) Env.empty in
  let result = Check.check_program ~env sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors)

(* =============================================================================
   form_result_to_string Tests
   ============================================================================= *)

let test_form_result_defun_string () =
  let result = Check.DefunForm { name = "foo"; fn_type = arrow [Prim.int] Prim.string } in
  let str = Check.form_result_to_string result in
  Alcotest.(check bool) "contains defun" true
    (String.sub str 0 6 = "(defun")

let test_form_result_expr_string () =
  let result = Check.ExprForm { ty = Prim.int } in
  let str = Check.form_result_to_string result in
  Alcotest.(check string) "Int" "Int" str

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "check"
    [
      ( "check_expr",
        [
          Alcotest.test_case "literal" `Quick test_check_expr_literal;
          Alcotest.test_case "application" `Quick test_check_expr_application;
          Alcotest.test_case "type error" `Quick test_check_expr_type_error;
        ] );
      ( "check_form",
        [
          Alcotest.test_case "defun" `Quick test_check_form_defun;
          Alcotest.test_case "expr" `Quick test_check_form_expr;
        ] );
      ( "check_program",
        [
          Alcotest.test_case "empty" `Quick test_check_program_empty;
          Alcotest.test_case "single defun" `Quick test_check_program_single_defun;
          Alcotest.test_case "defun sequence" `Quick test_check_program_defun_sequence;
          Alcotest.test_case "defun calls previous" `Quick test_check_program_defun_calls_previous;
        ] );
      ( "form_result_to_string",
        [
          Alcotest.test_case "defun" `Quick test_form_result_defun_string;
          Alcotest.test_case "expr" `Quick test_form_result_expr_string;
        ] );
    ]
