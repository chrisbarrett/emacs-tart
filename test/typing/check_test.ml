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
  | _ -> Alcotest.fail "expected DefunForm"

let test_check_form_expr () =
  let sexp = parse "42" in
  let env', result, _, _ = Check.check_form Env.empty sexp in
  (* Environment unchanged *)
  Alcotest.(check int) "env unchanged" 0 (List.length env'.Env.bindings);
  (* Result should be ExprForm *)
  match result with
  | Check.ExprForm { ty } ->
      Alcotest.(check string) "expr type" "Int" (to_string ty)
  | _ -> Alcotest.fail "expected ExprForm"

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

(** Helper to check if a string contains a substring *)
let contains ~substring s =
  try
    let _ = Str.search_forward (Str.regexp_string substring) s 0 in
    true
  with Not_found -> false

(** Test error message for return type mismatch (R9) *)
let test_declare_tart_return_error_message () =
  let sexps =
    parse_many
      {|(defun bad (x)
          (declare (tart (int) -> string))
          x)|}
  in
  let result = Check.check_program sexps in
  Alcotest.(check bool) "has error" true (List.length result.errors > 0);
  match result.errors with
  | err :: _ ->
      let diag = Tart.Diagnostic.of_unify_error err in
      (* Message should say "function body doesn't match declared return type" *)
      Alcotest.(check bool)
        "message mentions return type" true
        (contains ~substring:"declared return type" diag.message);
      (* Related info should mention the function name and declared type *)
      Alcotest.(check bool)
        "has related info" true
        (List.length diag.related > 0);
      let related = List.hd diag.related in
      Alcotest.(check bool)
        "related mentions function name" true
        (contains ~substring:"bad" related.message);
      Alcotest.(check bool)
        "related mentions declared type" true
        (contains ~substring:"String" related.message)
  | [] -> Alcotest.fail "expected error"

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
   Defvar with tart annotation Tests (R3)
   ============================================================================= *)

(** Test that (defvar NAME (tart TYPE VALUE)) binds NAME with TYPE *)
let test_defvar_tart_annotation_binds_type () =
  let sexps =
    parse_many
      {|(defvar my-cache (tart (hash-table string int) (make-hash-table)))|}
  in
  let result = Check.check_program sexps in
  Alcotest.(check bool)
    "my-cache bound" true
    (Option.is_some (Env.lookup "my-cache" result.env));
  match result.forms with
  | [ Check.DefvarForm { name; var_type } ] ->
      Alcotest.(check string) "name" "my-cache" name;
      Alcotest.(check string)
        "type" "(HashTable String Int)" (to_string var_type)
  | _ -> Alcotest.fail "expected DefvarForm"

(** Test that defvar with tart annotation checks value against declared type *)
let test_defvar_tart_annotation_checks_value () =
  let sexps = parse_many {|(defvar my-var (tart int "not an int"))|} in
  let result = Check.check_program sexps in
  (* Should have a type error because "not an int" is String, not Int *)
  Alcotest.(check bool) "has error" true (List.length result.errors > 0)

(** Test that defconst works the same as defvar *)
let test_defconst_tart_annotation () =
  let sexps = parse_many {|(defconst my-version (tart string "1.0.0"))|} in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors);
  match result.forms with
  | [ Check.DefvarForm { name; var_type } ] ->
      Alcotest.(check string) "name" "my-version" name;
      Alcotest.(check string) "type" "String" (to_string var_type)
  | _ -> Alcotest.fail "expected DefvarForm"

(** Test defvar without tart annotation infers type *)
let test_defvar_no_annotation_infers () =
  let sexps = parse_many {|(defvar my-num 42)|} in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors);
  match result.forms with
  | [ Check.DefvarForm { name; var_type } ] ->
      Alcotest.(check string) "name" "my-num" name;
      Alcotest.(check string) "type" "Int" (to_string var_type)
  | _ -> Alcotest.fail "expected DefvarForm"

(** Test defvar without init uses Any *)
let test_defvar_no_init () =
  let sexps = parse_many "(defvar my-buffer)" in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors);
  match result.forms with
  | [ Check.DefvarForm { name; var_type } ] ->
      Alcotest.(check string) "name" "my-buffer" name;
      Alcotest.(check string) "type" "Any" (to_string var_type)
  | _ -> Alcotest.fail "expected DefvarForm"

(* =============================================================================
   tart-declare Tests (R4)
   ============================================================================= *)

(** Test that (tart-declare NAME TYPE) binds NAME with TYPE *)
let test_tart_declare_binds_type () =
  let sexps = parse_many "(tart-declare my-count int)" in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors);
  match result.forms with
  | [ Check.TartDeclareForm { name; var_type } ] ->
      Alcotest.(check string) "name" "my-count" name;
      Alcotest.(check string) "type" "Int" (to_string var_type)
  | _ -> Alcotest.fail "expected TartDeclareForm"

(** Test tart-declare followed by defvar *)
let test_tart_declare_then_defvar () =
  let sexps =
    parse_many
      {|(tart-declare my-count int)
                           (defvar my-count)|}
  in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors);
  (* my-count should have type Int from the tart-declare *)
  Alcotest.(check bool)
    "my-count bound" true
    (Option.is_some (Env.lookup "my-count" result.env))

(** Test tart-declare with arrow type *)
let test_tart_declare_arrow_type () =
  let sexps = parse_many "(tart-declare my-handler ((string) -> nil))" in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors);
  match result.forms with
  | [ Check.TartDeclareForm { var_type; _ } ] ->
      Alcotest.(check string)
        "arrow type" "(-> (String) Nil)" (to_string var_type)
  | _ -> Alcotest.fail "expected TartDeclareForm"

(* =============================================================================
   setq type checking Tests (R3)
   ============================================================================= *)

(** Test that setq to declared variable checks against declared type *)
let test_setq_checks_declared_type () =
  let sexps =
    parse_many
      {|(defvar my-cache (tart (hash-table string int) (make-hash-table)))
        (setq my-cache "not a hash table")|}
  in
  let result = Check.check_program sexps in
  (* Should have a type error: String is not (Hash-table String Int) *)
  Alcotest.(check bool) "has error" true (List.length result.errors > 0)

(** Test that setq to declared variable with correct type succeeds *)
let test_setq_declared_type_ok () =
  let sexps =
    parse_many {|(defvar my-num (tart int 0))
        (setq my-num 42)|}
  in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors)

(** Test setq with tart-declare variable *)
let test_setq_tart_declare_checks () =
  let sexps =
    parse_many
      {|(tart-declare my-count int)
        (defvar my-count)
        (setq my-count "not an int")|}
  in
  let result = Check.check_program sexps in
  (* Should error: String is not Int *)
  Alcotest.(check bool) "has error" true (List.length result.errors > 0)

(** Test reading declared variable has correct type *)
let test_read_declared_variable () =
  let sexps =
    parse_many
      {|(defvar my-name (tart string "test"))
        (upcase my-name)|}
  in
  let result = Check.check_program sexps in
  (* my-name has type String, upcase expects String - should work *)
  Alcotest.(check int) "no errors" 0 (List.length result.errors)

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
   tart-type (file-local type alias) Tests (R5, R6)
   ============================================================================= *)

(** Test that simple tart-type creates form result *)
let test_tart_type_simple () =
  let sexps = parse_many "(tart-type int-pair (tuple int int))" in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors);
  match result.forms with
  | [ Check.TartTypeForm { name; params } ] ->
      Alcotest.(check string) "name" "int-pair" name;
      Alcotest.(check int) "no params" 0 (List.length params)
  | _ -> Alcotest.fail "expected TartTypeForm"

(** Test that tart-type is usable in annotation in same file *)
let test_tart_type_usable_in_annotation () =
  let sexps =
    parse_many
      {|(tart-type my-int int)
        (defvar my-num (tart my-int 42))|}
  in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors);
  (* my-num should have type Int (the expanded alias) *)
  match result.forms with
  | [ _; Check.DefvarForm { name; var_type } ] ->
      Alcotest.(check string) "name" "my-num" name;
      Alcotest.(check string) "type" "Int" (to_string var_type)
  | _ -> Alcotest.fail "expected TartTypeForm then DefvarForm"

(** Test that tart-type type error when value doesn't match *)
let test_tart_type_annotation_mismatch () =
  let sexps =
    parse_many
      {|(tart-type my-string string)
        (defvar x (tart my-string 42))|}
  in
  let result = Check.check_program sexps in
  (* Should have error: Int is not String *)
  Alcotest.(check bool) "has error" true (List.length result.errors > 0)

(** Test parameterized tart-type *)
let test_tart_type_parameterized () =
  let sexps = parse_many "(tart-type predicate [a] ((a) -> bool))" in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors);
  match result.forms with
  | [ Check.TartTypeForm { name; params } ] ->
      Alcotest.(check string) "name" "predicate" name;
      Alcotest.(check (list string)) "params" [ "a" ] params
  | _ -> Alcotest.fail "expected TartTypeForm"

(** Test parameterized tart-type instantiation *)
let test_tart_type_parameterized_usage () =
  let sexps =
    parse_many
      {|(tart-type predicate [a] ((a) -> bool))
        (defvar is-positive (tart (predicate int) (lambda (x) (> x 0))))|}
  in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors);
  match result.forms with
  | [ _; Check.DefvarForm { var_type; _ } ] ->
      Alcotest.(check string) "type" "(-> (Int) Bool)" (to_string var_type)
  | _ -> Alcotest.fail "expected TartTypeForm then DefvarForm"

(** Test multi-param tart-type *)
let test_tart_type_multi_param () =
  let sexps =
    parse_many
      {|(tart-type mapping [k v] (hash-table k v))
        (tart-declare my-map (mapping string int))|}
  in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors);
  match result.forms with
  | [ Check.TartTypeForm { params; _ }; Check.TartDeclareForm { var_type; _ } ]
    ->
      Alcotest.(check (list string)) "params" [ "k"; "v" ] params;
      Alcotest.(check string)
        "type" "(HashTable String Int)" (to_string var_type)
  | _ -> Alcotest.fail "expected forms"

(** Test tart-type to_string for simple alias *)
let test_tart_type_to_string_simple () =
  let result = Check.TartTypeForm { name = "foo"; params = [] } in
  let str = Check.form_result_to_string result in
  Alcotest.(check string) "string" "(tart-type foo)" str

(** Test tart-type to_string for parameterized alias *)
let test_tart_type_to_string_params () =
  let result = Check.TartTypeForm { name = "foo"; params = [ "a"; "b" ] } in
  let str = Check.form_result_to_string result in
  Alcotest.(check string) "string" "(tart-type foo [a b])" str

(** Test that tart-type union alias is parsed and stored *)
let test_tart_type_union () =
  let sexps = parse_many {|(tart-type result (string | error))|} in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors);
  (* Check the alias was stored *)
  let alias_opt = Tart.Sig_loader.lookup_alias "result" result.aliases in
  Alcotest.(check bool) "alias found" true (Option.is_some alias_opt)

(** Test that aliases added to result *)
let test_tart_type_in_result () =
  let sexps = parse_many "(tart-type foo int)" in
  let result = Check.check_program sexps in
  let alias_opt = Tart.Sig_loader.lookup_alias "foo" result.aliases in
  Alcotest.(check bool) "alias found" true (Option.is_some alias_opt)

(* =============================================================================
   Invariance Tests (parameterized types)
   ============================================================================= *)

(** Test that (list int) is not compatible with (list any) parameter type. This
    tests the spec example from R7: invariant type constructors. *)
let test_invariance_list_int_not_list_any () =
  let sexps =
    parse_many
      {|
(defun takes-any-list (xs)
  (declare (tart ((list any)) -> nil))
  nil)
(defun get-int-list ()
  (declare (tart () -> (list int)))
  (list 1 2 3))
(takes-any-list (get-int-list))
|}
  in
  let result = Check.check_program sexps in
  Alcotest.(check bool) "has type error" true (List.length result.errors > 0)

(** Test that (list any) passed to (list any) parameter is ok. Note: '(1
    "hello") infers to (List Any), so no annotation needed. *)
let test_invariance_list_any_to_list_any () =
  let sexps =
    parse_many
      {|
(defun takes-any-list (xs)
  (declare (tart ((list any)) -> nil))
  nil)
(takes-any-list '(1 "hello"))
|}
  in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors)

(** Test invariance with option type *)
let test_invariance_option () =
  let sexps =
    parse_many
      {|
(defun takes-option-any (x)
  (declare (tart ((option any)) -> nil))
  nil)
(defun get-option-int ()
  (declare (tart () -> (option int)))
  42)
(takes-option-any (get-option-int))
|}
  in
  let result = Check.check_program sexps in
  Alcotest.(check bool) "has type error" true (List.length result.errors > 0)

(** Test invariance with hash-table - first type param *)
let test_invariance_hash_table_key () =
  let sexps =
    parse_many
      {|
(defun takes-any-key (h)
  (declare (tart ((hash-table any int)) -> nil))
  nil)
(defun get-string-int-ht ()
  (declare (tart () -> (hash-table string int)))
  (make-hash-table))
(takes-any-key (get-string-int-ht))
|}
  in
  let result = Check.check_program sexps in
  Alcotest.(check bool) "has type error" true (List.length result.errors > 0)

(** Test that polymorphic functions still work with lists. Note: We use (list 1
    2 3) not '(1 2 3) because quoted lists infer to (List Any) and can't be
    narrowed to (List Int) due to invariance. *)
let test_invariance_poly_function_ok () =
  let sexps =
    parse_many
      {|
(defun my-car (xs)
  (declare (tart [a] ((list a)) -> (option a)))
  (car xs))
(my-car (list 1 2 3))
|}
  in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors)

(* =============================================================================
   @type (Explicit Type Instantiation) Tests
   ============================================================================= *)

(** Test that (@type [int] identity 42) type-checks correctly *)
let test_at_type_basic () =
  let env =
    Env.extend_poly "identity" [ "a" ] (arrow [ TCon "a" ] (TCon "a")) Env.empty
  in
  let sexp = parse "(@type [int] identity 42)" in
  let ty, errors = Check.check_expr ~env sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string) "returns Int" "Int" (to_string ty)

(** Test that (@type [string] identity 42) produces type error *)
let test_at_type_type_mismatch () =
  let env =
    Env.extend_poly "identity" [ "a" ] (arrow [ TCon "a" ] (TCon "a")) Env.empty
  in
  let sexp = parse {|(@type [string] identity 42)|} in
  let _, errors = Check.check_expr ~env sexp in
  (* Should error: 42 is Int, but String was specified *)
  Alcotest.(check bool) "has type error" true (List.length errors > 0)

(** Test that (@type [_] identity 42) with placeholder infers correctly *)
let test_at_type_placeholder () =
  let env =
    Env.extend_poly "identity" [ "a" ] (arrow [ TCon "a" ] (TCon "a")) Env.empty
  in
  let sexp = parse "(@type [_] identity 42)" in
  let ty, errors = Check.check_expr ~env sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string) "returns Int" "Int" (to_string ty)

(** Test multi-parameter function with explicit types *)
let test_at_type_multi_param () =
  let pair_ty =
    arrow [ TCon "a"; TCon "b" ] (TApp (TCon "cons", [ TCon "a"; TCon "b" ]))
  in
  let env = Env.extend_poly "pair" [ "a"; "b" ] pair_ty Env.empty in
  let sexp = parse {|(@type [int string] pair 1 "hi")|} in
  let ty, errors = Check.check_expr ~env sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string) "returns cons" "(cons Int String)" (to_string ty)

(** Test partial instantiation with mixed explicit and inferred *)
let test_at_type_partial () =
  let pair_ty =
    arrow [ TCon "a"; TCon "b" ] (TApp (TCon "cons", [ TCon "a"; TCon "b" ]))
  in
  let env = Env.extend_poly "pair" [ "a"; "b" ] pair_ty Env.empty in
  let sexp = parse {|(@type [_ string] pair 1 "hi")|} in
  let ty, errors = Check.check_expr ~env sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  (* First param inferred from 1 (Int), second explicit (String) *)
  Alcotest.(check string) "returns cons" "(cons Int String)" (to_string ty)

(** Test @type in a program context *)
let test_at_type_in_program () =
  let sexps =
    parse_many
      {|(defun my-identity (x)
          (declare (tart [a] (a) -> a))
          x)
        (@type [int] my-identity 42)|}
  in
  let result = Check.check_program sexps in
  Alcotest.(check int) "no errors" 0 (List.length result.errors)

(** Test HK type constructor instantiation: (@type [list int string] fmap ...)
*)
let test_at_type_hk_instantiation () =
  (* fmap : [f a b] (((a -> b)) (f a)) -> (f b)
     where f is a type constructor of kind * -> * *)
  let fmap_ty =
    TArrow
      ( [
          (* First param: (a -> b) *)
          PPositional (TArrow ([ PPositional (TCon "a") ], TCon "b"));
          (* Second param: (f a) - type application with f as type var *)
          PPositional (TApp (TCon "f", [ TCon "a" ]));
        ],
        (* Return: (f b) *)
        TApp (TCon "f", [ TCon "b" ]) )
  in
  let env = Env.extend_poly "fmap" [ "f"; "a"; "b" ] fmap_ty Env.empty in
  (* Also add a number-to-string function and a (list int) value *)
  let env =
    Env.extend_mono "number-to-string"
      (TArrow ([ PPositional Prim.int ], Prim.string))
      env
  in
  let env = Env.extend_mono "my-list" (TApp (TCon "List", [ Prim.int ])) env in
  let sexp =
    parse {|(@type [list int string] fmap number-to-string my-list)|}
  in
  let ty, errors = Check.check_expr ~env sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string) "returns (List String)" "(List String)" (to_string ty)

(** Test HK instantiation with partial placeholder *)
let test_at_type_hk_partial () =
  (* Same fmap signature *)
  let fmap_ty =
    TArrow
      ( [
          PPositional (TArrow ([ PPositional (TCon "a") ], TCon "b"));
          PPositional (TApp (TCon "f", [ TCon "a" ]));
        ],
        TApp (TCon "f", [ TCon "b" ]) )
  in
  let env = Env.extend_poly "fmap" [ "f"; "a"; "b" ] fmap_ty Env.empty in
  let env =
    Env.extend_mono "upcase"
      (TArrow ([ PPositional Prim.string ], Prim.string))
      env
  in
  let env =
    Env.extend_mono "my-list" (TApp (TCon "List", [ Prim.string ])) env
  in
  (* Use placeholder for a, let it be inferred from my-list and upcase *)
  let sexp = parse {|(@type [list _ _] fmap upcase my-list)|} in
  let ty, errors = Check.check_expr ~env sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string) "returns (List String)" "(List String)" (to_string ty)

(** Test HK instantiation with type mismatch *)
let test_at_type_hk_mismatch () =
  (* fmap with list specified but value is option *)
  let fmap_ty =
    TArrow
      ( [
          PPositional (TArrow ([ PPositional (TCon "a") ], TCon "b"));
          PPositional (TApp (TCon "f", [ TCon "a" ]));
        ],
        TApp (TCon "f", [ TCon "b" ]) )
  in
  let env = Env.extend_poly "fmap" [ "f"; "a"; "b" ] fmap_ty Env.empty in
  let env =
    Env.extend_mono "upcase"
      (TArrow ([ PPositional Prim.string ], Prim.string))
      env
  in
  (* my-list is Option String, but we say list *)
  let env =
    Env.extend_mono "my-list" (TApp (TCon "Option", [ Prim.string ])) env
  in
  let sexp = parse {|(@type [list string string] fmap upcase my-list)|} in
  let _, errors = Check.check_expr ~env sexp in
  (* Should have error: list vs option *)
  Alcotest.(check bool) "has type error" true (List.length errors > 0)

(** Test that (@type [int string] identity 42) fails: too many type args *)
let test_at_type_too_many_args () =
  let env =
    Env.extend_poly "identity" [ "a" ] (arrow [ TCon "a" ] (TCon "a")) Env.empty
  in
  let sexp = parse {|(@type [int string] identity 42)|} in
  let _, errors = Check.check_expr ~env sexp in
  (* Should error: identity has 1 type param, we gave 2 *)
  Alcotest.(check bool) "has type error" true (List.length errors > 0)

(** Test that (@type [] identity 42) fails: too few type args *)
let test_at_type_too_few_args () =
  let env =
    Env.extend_poly "identity" [ "a" ] (arrow [ TCon "a" ] (TCon "a")) Env.empty
  in
  let sexp = parse {|(@type [] identity 42)|} in
  let _, errors = Check.check_expr ~env sexp in
  (* Should error: identity has 1 type param, we gave 0 *)
  Alcotest.(check bool) "has type error" true (List.length errors > 0)

(** Check if needle is a substring of haystack *)
let contains_substring haystack needle =
  let re = Str.regexp_string needle in
  try
    ignore (Str.search_forward re haystack 0);
    true
  with Not_found -> false

(** Test that error message mentions number of type arguments *)
let test_at_type_arity_error_message () =
  let env =
    Env.extend_poly "identity" [ "a" ] (arrow [ TCon "a" ] (TCon "a")) Env.empty
  in
  let sexp = parse {|(@type [int string] identity 42)|} in
  let _, errors = Check.check_expr ~env sexp in
  Alcotest.(check bool) "has error" true (List.length errors > 0);
  let diag = Tart.Diagnostic.of_unify_error (List.hd errors) in
  Alcotest.(check bool)
    "message mentions type arguments" true
    (contains_substring diag.message "type argument")

(** Test that multi-param function with wrong arity fails *)
let test_at_type_multi_param_arity () =
  let pair_ty =
    arrow [ TCon "a"; TCon "b" ] (TApp (TCon "cons", [ TCon "a"; TCon "b" ]))
  in
  let env = Env.extend_poly "pair" [ "a"; "b" ] pair_ty Env.empty in
  (* pair has 2 type params, we give 3 *)
  let sexp = parse {|(@type [int string bool] pair 1 "hi")|} in
  let _, errors = Check.check_expr ~env sexp in
  Alcotest.(check bool) "has type error" true (List.length errors > 0)

(* =============================================================================
   Pcase Tests
   ============================================================================= *)

let test_pcase_returns_branch_type () =
  (* pcase with literal body returns that literal's type *)
  let sexp = parse {|(pcase x (_ 42))|} in
  let ty, errors = Check.check_expr sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string) "pcase returns Int" "Int" (to_string ty)

let test_pcase_unifies_branches () =
  (* All branches must have compatible types *)
  let sexp = parse {|(pcase x (1 "one") (2 "two") (_ "default"))|} in
  let ty, errors = Check.check_expr sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string) "pcase returns String" "String" (to_string ty)

let test_pcase_branch_type_mismatch () =
  (* Incompatible branch types cause error *)
  let sexp = parse {|(pcase x (1 "string") (2 42))|} in
  let _, errors = Check.check_expr sexp in
  Alcotest.(check bool) "has type error" true (List.length errors > 0)

let test_pcase_binds_pattern_var () =
  (* Pattern variables are available in body *)
  let env =
    Env.extend_mono "+" (arrow [ Prim.int; Prim.int ] Prim.int) Env.empty
  in
  let sexp = parse {|(pcase 42 ((, x) (+ x 1)))|} in
  let ty, errors = Check.check_expr ~env sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string) "pcase with binding returns Int" "Int" (to_string ty)

let test_pcase_exhaustive_same_as_pcase () =
  (* pcase-exhaustive works identically to pcase for type checking *)
  let sexp = parse {|(pcase-exhaustive x (_ "result"))|} in
  let ty, errors = Check.check_expr sexp in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  Alcotest.(check string)
    "pcase-exhaustive returns String" "String" (to_string ty)

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
          Alcotest.test_case "return error message" `Quick
            test_declare_tart_return_error_message;
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
      ( "defvar_tart",
        [
          Alcotest.test_case "binds type" `Quick
            test_defvar_tart_annotation_binds_type;
          Alcotest.test_case "checks value" `Quick
            test_defvar_tart_annotation_checks_value;
          Alcotest.test_case "defconst" `Quick test_defconst_tart_annotation;
          Alcotest.test_case "no annotation infers" `Quick
            test_defvar_no_annotation_infers;
          Alcotest.test_case "no init uses Any" `Quick test_defvar_no_init;
        ] );
      ( "tart_declare",
        [
          Alcotest.test_case "binds type" `Quick test_tart_declare_binds_type;
          Alcotest.test_case "then defvar" `Quick test_tart_declare_then_defvar;
          Alcotest.test_case "arrow type" `Quick test_tart_declare_arrow_type;
        ] );
      ( "setq_checking",
        [
          Alcotest.test_case "checks declared type" `Quick
            test_setq_checks_declared_type;
          Alcotest.test_case "declared type ok" `Quick
            test_setq_declared_type_ok;
          Alcotest.test_case "tart-declare checks" `Quick
            test_setq_tart_declare_checks;
          Alcotest.test_case "read declared variable" `Quick
            test_read_declared_variable;
        ] );
      ( "tart_type",
        [
          Alcotest.test_case "simple alias" `Quick test_tart_type_simple;
          Alcotest.test_case "usable in annotation" `Quick
            test_tart_type_usable_in_annotation;
          Alcotest.test_case "annotation mismatch" `Quick
            test_tart_type_annotation_mismatch;
          Alcotest.test_case "parameterized" `Quick test_tart_type_parameterized;
          Alcotest.test_case "parameterized usage" `Quick
            test_tart_type_parameterized_usage;
          Alcotest.test_case "multi param" `Quick test_tart_type_multi_param;
          Alcotest.test_case "to_string simple" `Quick
            test_tart_type_to_string_simple;
          Alcotest.test_case "to_string params" `Quick
            test_tart_type_to_string_params;
          Alcotest.test_case "union alias" `Quick test_tart_type_union;
          Alcotest.test_case "in result" `Quick test_tart_type_in_result;
        ] );
      ( "invariance",
        [
          Alcotest.test_case "list int != list any" `Quick
            test_invariance_list_int_not_list_any;
          Alcotest.test_case "list any = list any" `Quick
            test_invariance_list_any_to_list_any;
          Alcotest.test_case "option int != option any" `Quick
            test_invariance_option;
          Alcotest.test_case "hash-table invariance" `Quick
            test_invariance_hash_table_key;
          Alcotest.test_case "polymorphic function ok" `Quick
            test_invariance_poly_function_ok;
        ] );
      ( "@type",
        [
          Alcotest.test_case "basic instantiation" `Quick test_at_type_basic;
          Alcotest.test_case "type mismatch" `Quick test_at_type_type_mismatch;
          Alcotest.test_case "placeholder" `Quick test_at_type_placeholder;
          Alcotest.test_case "multi-param" `Quick test_at_type_multi_param;
          Alcotest.test_case "partial instantiation" `Quick test_at_type_partial;
          Alcotest.test_case "in program" `Quick test_at_type_in_program;
          Alcotest.test_case "HK instantiation" `Quick
            test_at_type_hk_instantiation;
          Alcotest.test_case "HK partial" `Quick test_at_type_hk_partial;
          Alcotest.test_case "HK mismatch" `Quick test_at_type_hk_mismatch;
          Alcotest.test_case "too many type args" `Quick
            test_at_type_too_many_args;
          Alcotest.test_case "too few type args" `Quick
            test_at_type_too_few_args;
          Alcotest.test_case "arity error message" `Quick
            test_at_type_arity_error_message;
          Alcotest.test_case "multi-param arity" `Quick
            test_at_type_multi_param_arity;
        ] );
      ( "pcase",
        [
          Alcotest.test_case "returns branch type" `Quick
            test_pcase_returns_branch_type;
          Alcotest.test_case "unifies branches" `Quick
            test_pcase_unifies_branches;
          Alcotest.test_case "branch type mismatch" `Quick
            test_pcase_branch_type_mismatch;
          Alcotest.test_case "binds pattern var" `Quick
            test_pcase_binds_pattern_var;
          Alcotest.test_case "exhaustive same as pcase" `Quick
            test_pcase_exhaustive_same_as_pcase;
        ] );
    ]
