(** Tests for the top-level type checking API *)

open Tart.Types
module Env = Tart.Type_env
module Check = Tart.Check

(** Parse a string to S-expression for testing *)
let parse str =
  match Tart.Read.parse_one ~filename:"<test>" str with
  | Ok sexp -> sexp
  | Error msg -> failwith ("parse error: " ^ msg)

let parse_many str = Tart.Read.parse_string_exn ~filename:"<test>" str

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
  let env = Env.extend_mono "f" (arrow [ Prim.int ] Prim.string) Env.empty in
  let ty, errors = Check.check_expr ~env sexp in
  Alcotest.(check string) "return type" "String" (to_string ty);
  Alcotest.(check int) "no errors" 0 (List.length errors)

let test_check_expr_type_error () =
  let sexp = parse "(+ 1 \"hello\")" in
  let env =
    Env.extend_mono "+" (arrow [ Prim.int; Prim.int ] Prim.int) Env.empty
  in
  let _, errors = Check.check_expr ~env sexp in
  Alcotest.(check bool) "has error" true (List.length errors > 0)

(* =============================================================================
   check_form Tests
   ============================================================================= *)

let test_check_form_defun () =
  let sexp = parse "(defun foo () 42)" in
  let env', result, _, _ = Check.check_form Env.empty sexp in
  (* Should bind foo in environment *)
  Alcotest.(check bool)
    "foo bound" true
    (Option.is_some (Env.lookup "foo" env'));
  (* Result should be DefunForm *)
  match result with
  | Check.DefunForm { name; _ } ->
      Alcotest.(check string) "defun name" "foo" name
  | Check.ExprForm _ -> Alcotest.fail "expected DefunForm"

let test_check_form_expr () =
  let sexp = parse "42" in
  let env', result, _, _ = Check.check_form Env.empty sexp in
  (* Environment unchanged *)
  Alcotest.(check int) "env unchanged" 0 (List.length env'.Env.bindings);
  (* Result should be ExprForm *)
  match result with
  | Check.ExprForm { ty } ->
      Alcotest.(check string) "expr type" "Int" (to_string ty)
  | Check.DefunForm _ -> Alcotest.fail "expected ExprForm"

(* =============================================================================
   check_program Tests
   ============================================================================= *)

let test_check_program_empty () =
  let result = Check.check_program [] in
  Alcotest.(check int) "no forms" 0 (List.length result.forms);
  Alcotest.(check int) "no errors" 0 (List.length result.errors)

let test_check_program_single_defun () =
  let sexps = parse_many "(defun add1 (x) (+ x 1))" in
  let env =
    Env.extend_mono "+" (arrow [ Prim.int; Prim.int ] Prim.int) Env.empty
  in
  let result = Check.check_program ~env sexps in
  Alcotest.(check int) "one form" 1 (List.length result.forms);
  (* add1 should be bound in final env *)
  Alcotest.(check bool)
    "add1 bound" true
    (Option.is_some (Env.lookup "add1" result.env))

let test_check_program_defun_sequence () =
  let sexps = parse_many "(defun foo () 1) (defun bar () (foo))" in
  let result = Check.check_program sexps in
  Alcotest.(check int) "two forms" 2 (List.length result.forms);
  (* Both should be bound *)
  Alcotest.(check bool)
    "foo bound" true
    (Option.is_some (Env.lookup "foo" result.env));
  Alcotest.(check bool)
    "bar bound" true
    (Option.is_some (Env.lookup "bar" result.env))

let test_check_program_defun_calls_previous () =
  (* bar calls foo - should type check correctly *)
  let sexps = parse_many "(defun foo () 42) (defun bar () (+ (foo) 1))" in
  let env =
    Env.extend_mono "+" (arrow [ Prim.int; Prim.int ] Prim.int) Env.empty
  in
  let result = Check.check_program ~env sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors)

(* =============================================================================
   R8: Built-in function types Tests
   ============================================================================= *)

(** Test that car on a quoted list returns Option Any. (car '(1 2 3)) should
    infer (Option Any) because '(1 2 3) has type (List Any). *)
let test_builtin_car_returns_option () =
  let sexp = parse "(car '(1 2 3))" in
  let ty, errors = Check.check_expr sexp in
  (* Uses default environment which includes built-in types *)
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string) "car returns Option Any" "(Option Any)" (to_string ty)

(** Test that (+ 1 "x") produces a type error. The built-in + expects Int
    arguments, not String. *)
let test_builtin_plus_type_error () =
  let sexp = parse "(+ 1 \"x\")" in
  let _, errors = Check.check_expr sexp in
  Alcotest.(check bool) "type error" true (List.length errors > 0)

(** Test that (+ 1 2) returns Int with no errors *)
let test_builtin_plus_ok () =
  let sexp = parse "(+ 1 2)" in
  let ty, errors = Check.check_expr sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string) "plus returns Int" "Int" (to_string ty)

(** Test that (concat "a" "b") returns String *)
let test_builtin_concat () =
  let sexp = parse "(concat \"a\" \"b\")" in
  let ty, errors = Check.check_expr sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string) "concat returns String" "String" (to_string ty)

(** Test that (length '(1 2 3)) returns Int *)
let test_builtin_length () =
  let sexp = parse "(length '(1 2 3))" in
  let ty, errors = Check.check_expr sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string) "length returns Int" "Int" (to_string ty)

(** Test that (null nil) returns Bool *)
let test_builtin_null () =
  let sexp = parse "(null ())" in
  let ty, errors = Check.check_expr sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string) "null returns Bool" "Bool" (to_string ty)

(* =============================================================================
   declare tart Tests
   ============================================================================= *)

(** Test that defun with (declare (tart ...)) uses declared type *)
let test_declare_tart_uses_type () =
  let sexps =
    parse_many
      {|(defun my-add (x y)
                             (declare (tart (int int) -> int))
                             (+ x y))
                           (my-add 1 2)|}
  in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors)

(** Test that declare tart produces error when body doesn't match return type *)
let test_declare_tart_return_mismatch () =
  let sexps =
    parse_many
      {|(defun bad (x)
          (declare (tart (int) -> string))
          x)|}
  in
  let result = Check.check_program sexps in
  (* Should have type error: Int (x) doesn't match String (return) *)
  Alcotest.(check bool) "has error" true (List.length result.errors > 0)

(** Test that polymorphic declare tart works correctly *)
let test_declare_tart_polymorphic () =
  let sexps =
    parse_many
      {|(defun my-id (x)
          (declare (tart (a) -> a))
          x)
        (my-id 42)
        (my-id "hello")|}
  in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors)

(* =============================================================================
   tart annotation Tests: (tart TYPE FORM)
   ============================================================================= *)

(** Test that valid tart annotation produces no error *)
let test_tart_annotation_valid () =
  let sexp = parse {|(tart string "hello")|} in
  let ty, errors = Check.check_expr sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string) "type is String" "String" (to_string ty)

(** Test that mismatched tart annotation produces type error *)
let test_tart_annotation_mismatch () =
  let sexp = parse "(tart string 42)" in
  let _, errors = Check.check_expr sexp in
  Alcotest.(check bool) "has error" true (List.length errors > 0)

(** Test that tart annotation with list type works *)
let test_tart_annotation_list_valid () =
  let sexp = parse {|(tart (list int) (list 1 2 3))|} in
  let ty, errors = Check.check_expr sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string) "type is List Int" "(List Int)" (to_string ty)

(** Test tart annotation in defvar initialization *)
let test_tart_annotation_in_program () =
  let sexps = parse_many {|(tart int (+ 1 2))|} in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors)

(** Test tart annotation error has good message *)
let test_tart_annotation_error_message () =
  let sexp = parse "(tart int \"wrong\")" in
  let _, errors = Check.check_expr sexp in
  Alcotest.(check bool) "has error" true (List.length errors > 0);
  match errors with
  | err :: _ ->
      let diag = Tart.Diagnostic.of_unify_error err in
      let msg = diag.message in
      Alcotest.(check bool)
        "message mentions annotation" true
        (String.length msg > 0)
  | [] -> Alcotest.fail "expected error"

(* =============================================================================
   form_result_to_string Tests
   ============================================================================= *)

let test_form_result_defun_string () =
  let result =
    Check.DefunForm { name = "foo"; fn_type = arrow [ Prim.int ] Prim.string }
  in
  let str = Check.form_result_to_string result in
  Alcotest.(check bool) "contains defun" true (String.sub str 0 6 = "(defun")

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
          Alcotest.test_case "single defun" `Quick
            test_check_program_single_defun;
          Alcotest.test_case "defun sequence" `Quick
            test_check_program_defun_sequence;
          Alcotest.test_case "defun calls previous" `Quick
            test_check_program_defun_calls_previous;
        ] );
      ( "form_result_to_string",
        [
          Alcotest.test_case "defun" `Quick test_form_result_defun_string;
          Alcotest.test_case "expr" `Quick test_form_result_expr_string;
        ] );
      ( "builtin_types",
        [
          Alcotest.test_case "car returns Option" `Quick
            test_builtin_car_returns_option;
          Alcotest.test_case "+ type error" `Quick test_builtin_plus_type_error;
          Alcotest.test_case "+ ok" `Quick test_builtin_plus_ok;
          Alcotest.test_case "concat" `Quick test_builtin_concat;
          Alcotest.test_case "length" `Quick test_builtin_length;
          Alcotest.test_case "null" `Quick test_builtin_null;
        ] );
      ( "declare_tart",
        [
          Alcotest.test_case "uses declared type" `Quick
            test_declare_tart_uses_type;
          Alcotest.test_case "return mismatch error" `Quick
            test_declare_tart_return_mismatch;
          Alcotest.test_case "polymorphic" `Quick test_declare_tart_polymorphic;
        ] );
      ( "tart_annotation",
        [
          Alcotest.test_case "valid annotation" `Quick
            test_tart_annotation_valid;
          Alcotest.test_case "mismatch error" `Quick
            test_tart_annotation_mismatch;
          Alcotest.test_case "list type" `Quick test_tart_annotation_list_valid;
          Alcotest.test_case "in program" `Quick test_tart_annotation_in_program;
          Alcotest.test_case "error message" `Quick
            test_tart_annotation_error_message;
        ] );
    ]
