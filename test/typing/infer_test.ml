(** Tests for constraint generation (type inference) *)

open Tart.Types
module Env = Tart.Type_env
module Infer = Tart.Infer

(** Parse a string to S-expression for testing *)
let parse str =
  match Tart.Read.parse_one ~filename:"<test>" str with
  | Ok sexp -> sexp
  | Error msg -> failwith ("parse error: " ^ msg)

(** Helper to run inference and return the type string *)
let infer_type ?(env = Env.empty) str =
  reset_tvar_counter ();
  let sexp = parse str in
  let result = Infer.infer env sexp in
  to_string result.ty

(** Helper to get the constraint count *)
let constraint_count ?(env = Env.empty) str =
  reset_tvar_counter ();
  let sexp = parse str in
  let result = Infer.infer env sexp in
  List.length result.constraints

(* =============================================================================
   Literal Tests
   ============================================================================= *)

let test_int_literal () =
  Alcotest.(check string) "int literal" "Int" (infer_type "42")

let test_negative_int () =
  Alcotest.(check string) "negative int" "Int" (infer_type "-17")

let test_float_literal () =
  Alcotest.(check string) "float literal" "Float" (infer_type "3.14")

let test_string_literal () =
  Alcotest.(check string) "string literal" "String" (infer_type "\"hello\"")

let test_empty_string () =
  Alcotest.(check string) "empty string" "String" (infer_type "\"\"")

let test_keyword_literal () =
  Alcotest.(check string) "keyword literal" "Keyword" (infer_type ":foo")

let test_char_literal () =
  (* Characters are integers in Elisp *)
  Alcotest.(check string) "char literal" "Int" (infer_type "?a")

let test_nil_literal () =
  (* nil is the empty list *)
  Alcotest.(check string) "nil" "Nil" (infer_type "()")

(* =============================================================================
   Variable Tests
   ============================================================================= *)

let test_bound_var () =
  let env = Env.extend_mono "x" Prim.int Env.empty in
  Alcotest.(check string) "bound variable" "Int" (infer_type ~env "x")

let test_unbound_var () =
  (* Unbound variables get fresh type variables *)
  let ty = infer_type "unknown" in
  Alcotest.(check bool) "fresh tvar" true (String.length ty > 0 && ty.[0] = '\'')

let test_poly_var_instantiation () =
  (* Polymorphic variable gets instantiated with fresh tvars *)
  let env =
    Env.extend_poly "id" [ "a" ] (arrow [ TCon "a" ] (TCon "a")) Env.empty
  in
  let ty = infer_type ~env "id" in
  (* Should be a function type with fresh type variables *)
  Alcotest.(check bool)
    "instantiated poly type" true
    (String.sub ty 0 4 = "(-> ")

(* =============================================================================
   Quote Tests
   ============================================================================= *)

let test_quoted_symbol () =
  Alcotest.(check string) "quoted symbol" "Symbol" (infer_type "'foo")

let test_quoted_list () =
  Alcotest.(check string) "quoted list" "(List (Or Truthy Nil))" (infer_type "'(1 2 3)")

let test_quoted_int () =
  Alcotest.(check string) "quoted int" "Int" (infer_type "'42")

let test_quoted_string () =
  Alcotest.(check string) "quoted string" "String" (infer_type "'\"hello\"")

(* =============================================================================
   Lambda Tests
   ============================================================================= *)

let test_lambda_no_params () =
  let ty = infer_type "(lambda () 42)" in
  Alcotest.(check string) "nullary lambda" "(-> () Int)" ty

let test_lambda_one_param () =
  let ty = infer_type "(lambda (x) x)" in
  (* Should be (-> ('a) 'a) with the same type variable *)
  Alcotest.(check bool) "identity lambda" true (String.sub ty 0 4 = "(-> ")

let test_lambda_two_params () =
  let ty = infer_type "(lambda (x y) y)" in
  Alcotest.(check bool) "two-param lambda" true (String.sub ty 0 4 = "(-> ")

let test_lambda_body_uses_param () =
  let env =
    Env.extend_mono "+" (arrow [ Prim.int; Prim.int ] Prim.int) Env.empty
  in
  let ty = infer_type ~env "(lambda (x) (+ x 1))" in
  (* Constraint: x must unify with Int due to + *)
  Alcotest.(check bool) "lambda with body" true (String.sub ty 0 4 = "(-> ")

let test_lambda_constraint_count () =
  (* Lambda itself generates no constraints, but application inside does *)
  let count = constraint_count "(lambda (x) x)" in
  Alcotest.(check int) "identity lambda constraints" 0 count

(* =============================================================================
   Application Tests
   ============================================================================= *)

let test_application_generates_constraint () =
  (* Applying unknown function generates constraints:
     1 for the function type, 1 per argument for better error messages *)
  let count = constraint_count "(f 1)" in
  Alcotest.(check int) "application constraint" 2 count

let test_application_result_type () =
  (* Result is a fresh type variable *)
  let ty = infer_type "(f 1)" in
  Alcotest.(check bool)
    "application result is tvar" true
    (String.length ty > 0 && ty.[0] = '\'')

let test_known_function_application () =
  (* Applying a known function generates 1 constraint for fn type + 1 per arg *)
  let env = Env.extend_mono "add1" (arrow [ Prim.int ] Prim.int) Env.empty in
  let result =
    reset_tvar_counter ();
    let sexp = parse "(add1 42)" in
    Infer.infer env sexp
  in
  Alcotest.(check int) "known fn constraint" 2 (List.length result.constraints)

let test_multi_arg_application () =
  (* 1 constraint for fn type + 3 constraints for 3 args *)
  let count = constraint_count "(f 1 2 3)" in
  Alcotest.(check int) "multi-arg constraint" 4 count

(* =============================================================================
   If Tests
   ============================================================================= *)

let test_if_with_else () =
  (* Both branches contribute constraints *)
  let count = constraint_count "(if t 1 2)" in
  (* Two constraints: result = then_type, result = else_type *)
  Alcotest.(check int) "if-else constraints" 2 count

let test_if_result_type () =
  let ty = infer_type "(if t 1 2)" in
  (* Result is a fresh tvar that will unify with Int *)
  Alcotest.(check bool)
    "if result is tvar" true
    (String.length ty > 0 && ty.[0] = '\'')

let test_if_without_else () =
  let count = constraint_count "(if t 1)" in
  (* One constraint: result = then_type *)
  Alcotest.(check int) "if-no-else constraints" 1 count

(* =============================================================================
   Let Tests
   ============================================================================= *)

let test_let_simple () =
  let ty = infer_type "(let ((x 42)) x)" in
  Alcotest.(check string) "simple let" "Int" ty

let test_let_multiple_bindings () =
  let ty = infer_type "(let ((x 1) (y 2)) y)" in
  Alcotest.(check string) "let with two bindings" "Int" ty

let test_let_binding_shadowing () =
  let env = Env.extend_mono "x" Prim.string Env.empty in
  let ty = infer_type ~env "(let ((x 42)) x)" in
  (* Inner x shadows outer x *)
  Alcotest.(check string) "let shadows outer" "Int" ty

let test_let_nil_binding () =
  let ty = infer_type "(let ((x)) x)" in
  Alcotest.(check string) "nil binding" "Nil" ty

let test_let_star_sequential () =
  let ty = infer_type "(let* ((x 1) (y x)) y)" in
  (* y sees x, should be Int *)
  Alcotest.(check string) "let* sequential" "Int" ty

(* =============================================================================
   Progn Tests
   ============================================================================= *)

let test_progn_empty () =
  let ty = infer_type "(progn)" in
  Alcotest.(check string) "empty progn" "Nil" ty

let test_progn_single () =
  let ty = infer_type "(progn 42)" in
  Alcotest.(check string) "single progn" "Int" ty

let test_progn_multiple () =
  let ty = infer_type "(progn 1 2 \"end\")" in
  Alcotest.(check string) "multiple progn" "String" ty

(* =============================================================================
   Boolean Expression Tests
   ============================================================================= *)

let test_and_empty () =
  let ty = infer_type "(and)" in
  Alcotest.(check string) "empty and" "T" ty

let test_or_empty () =
  let ty = infer_type "(or)" in
  Alcotest.(check string) "empty or" "Nil" ty

let test_not_returns_bool () =
  let ty = infer_type "(not 42)" in
  Alcotest.(check string) "not returns bool" "(Or T Nil)" ty

let test_and_last_type () =
  let ty = infer_type "(and 1 \"str\")" in
  Alcotest.(check string) "and returns last" "String" ty

let test_or_last_type () =
  let ty = infer_type "(or 1 \"str\")" in
  Alcotest.(check string) "or returns last" "String" ty

(* =============================================================================
   Vector Tests
   ============================================================================= *)

let test_empty_vector () =
  let ty = infer_type "#()" in
  (* Empty vector has fresh element type *)
  Alcotest.(check bool) "empty vector" true (String.sub ty 0 8 = "(Vector ")

let test_int_vector () =
  let ty = infer_type "#(1 2 3)" in
  Alcotest.(check string) "int vector" "(Vector Int)" ty

let test_string_vector () =
  let ty = infer_type "#(\"a\" \"b\")" in
  Alcotest.(check string) "string vector" "(Vector String)" ty

let test_mixed_vector_constraints () =
  (* Mixed elements generate unification constraints *)
  let count = constraint_count "#(1 2 3)" in
  (* Each subsequent element constrains to equal first *)
  Alcotest.(check int) "vector constraints" 2 count

(* =============================================================================
   Cond Tests
   ============================================================================= *)

let test_cond_single_clause () =
  let count = constraint_count "(cond (t 42))" in
  (* One constraint: result = clause_body *)
  Alcotest.(check int) "single cond constraint" 1 count

let test_cond_multiple_clauses () =
  let count = constraint_count "(cond (nil 1) (t 2))" in
  (* Two constraints: result = each clause body *)
  Alcotest.(check int) "multi cond constraints" 2 count

(* =============================================================================
   Pcase Tests
   ============================================================================= *)

let test_pcase_simple () =
  (* pcase returns a fresh type variable that branches unify with *)
  let ty = infer_type "(pcase 42 (1 \"one\") (2 \"two\"))" in
  (* Result is a type variable before constraint solving *)
  Alcotest.(check bool)
    "pcase result is tvar" true
    (String.length ty > 0 && ty.[0] = '\'')

let test_pcase_underscore () =
  (* Wildcard pattern _ matches anything, result is tvar *)
  let ty = infer_type "(pcase x (_ 42))" in
  Alcotest.(check bool)
    "pcase wildcard is tvar" true
    (String.length ty > 0 && ty.[0] = '\'')

let test_pcase_constraint_count () =
  (* One constraint per clause: body type = result type *)
  let count = constraint_count "(pcase x (1 42) (_ 0))" in
  Alcotest.(check int) "pcase constraint count" 2 count

let test_pcase_branches_unify () =
  (* All branches must have compatible types *)
  let count = constraint_count {|(pcase x (1 "one") (2 "two") (_ "other"))|} in
  (* Three constraints: one for each branch body = result *)
  Alcotest.(check int) "pcase branch constraints" 3 count

let test_pcase_exhaustive () =
  (* pcase-exhaustive works the same way *)
  let count = constraint_count "(pcase-exhaustive x (_ 42))" in
  Alcotest.(check int) "pcase-exhaustive constraint" 1 count

let test_pcase_with_env () =
  (* Bound variables are available in pcase body *)
  let env = Env.extend_mono "x" Prim.int Env.empty in
  let count = constraint_count ~env "(pcase x (1 x) (_ 0))" in
  (* Two constraints: one per branch *)
  Alcotest.(check int) "pcase with env" 2 count

(* =============================================================================
   Defun Tests
   ============================================================================= *)

let test_defun_returns_symbol () =
  (* As an expression, defun returns a symbol *)
  let ty = infer_type "(defun foo () 42)" in
  Alcotest.(check string) "defun returns symbol" "Symbol" ty

let test_defun_infer_simple () =
  (* infer_defun extracts the function type *)
  reset_tvar_counter ();
  let sexp = parse "(defun add1 (x) (+ x 1))" in
  let env =
    Env.extend_mono "+" (arrow [ Prim.int; Prim.int ] Prim.int) Env.empty
  in
  match Infer.infer_defun env sexp with
  | Some result ->
      Alcotest.(check string) "defun name" "add1" result.name;
      (* The parameter type should unify with Int due to + *)
      Alcotest.(check bool)
        "defun fn_type" true
        (match result.fn_type with TArrow (_, _) -> true | _ -> false)
  | None -> Alcotest.fail "expected defun result"

let test_defun_infer_identity () =
  (* Identity function should get polymorphic type *)
  reset_tvar_counter ();
  let sexp = parse "(defun id (x) x)" in
  match Infer.infer_defun Env.empty sexp with
  | Some result ->
      Alcotest.(check string) "defun name" "id" result.name;
      (* Should be generalized: (forall (a) (-> (a) a)) *)
      let ty_str = to_string result.fn_type in
      Alcotest.(check bool)
        "identity is forall" true
        (String.sub ty_str 0 7 = "(forall")
  | None -> Alcotest.fail "expected defun result"

let test_defun_not_defun () =
  (* Non-defun returns None *)
  let sexp = parse "(lambda (x) x)" in
  match Infer.infer_defun Env.empty sexp with
  | Some _ -> Alcotest.fail "expected None for lambda"
  | None -> ()

(* =============================================================================
   Declare Tart Tests
   ============================================================================= *)

let test_defun_declare_tart_monomorphic () =
  (* Defun with (declare (tart ...)) should use declared type *)
  reset_tvar_counter ();
  let sexp =
    parse
      {|(defun my-add (x y)
              (declare (tart (int int) -> int))
              (+ x y))|}
  in
  let env =
    Env.extend_mono "+" (arrow [ Prim.int; Prim.int ] Prim.int) Env.empty
  in
  match Infer.infer_defun env sexp with
  | Some result ->
      Alcotest.(check string) "defun name" "my-add" result.name;
      let ty_str = to_string result.fn_type in
      Alcotest.(check string) "declared type" "(-> (Int Int) Int)" ty_str
  | None -> Alcotest.fail "expected defun result"

let test_defun_declare_tart_polymorphic () =
  (* Defun with polymorphic declaration returns type with fresh TVars.
     The forall is added by check_form via generalize, not by infer_defun.
     Requires explicit [a] quantifier - implicit inference was removed. *)
  reset_tvar_counter ();
  let sexp =
    parse
      {|(defun my-id (x)
              (declare (tart [a] (a) -> a))
              x)|}
  in
  match Infer.infer_defun Env.empty sexp with
  | Some result ->
      Alcotest.(check string) "defun name" "my-id" result.name;
      let ty_str = to_string result.fn_type in
      (* The raw fn_type has fresh TVars; check_form will generalize to forall *)
      Alcotest.(check bool)
        "has arrow type" true
        (String.sub ty_str 0 4 = "(-> ")
  | None -> Alcotest.fail "expected defun result"

let test_defun_declare_tart_explicit_forall () =
  (* Defun with explicit [a b] quantifiers returns type with fresh TVars.
     check_form will generalize to proper forall. *)
  reset_tvar_counter ();
  let sexp =
    parse
      {|(defun my-const (x y)
              (declare (tart [a b] (a b) -> a))
              x)|}
  in
  match Infer.infer_defun Env.empty sexp with
  | Some result ->
      let ty_str = to_string result.fn_type in
      (* Raw type has TVars; check_form generalizes to forall *)
      Alcotest.(check bool)
        "has arrow type" true
        (String.sub ty_str 0 4 = "(-> ")
  | None -> Alcotest.fail "expected defun result"

let test_defun_declare_tart_body_mismatch () =
  (* Body type should generate constraint with declared return type *)
  reset_tvar_counter ();
  let sexp =
    parse
      {|(defun bad-fn (x)
              (declare (tart (int) -> string))
              x)|}
  in
  match Infer.infer_defun Env.empty sexp with
  | Some result ->
      (* Function should still have the declared type *)
      let ty_str = to_string result.fn_type in
      Alcotest.(check string) "declared type" "(-> (Int) String)" ty_str;
      (* But there should be a constraint that x = String,
         which will produce an error when solved *)
      Alcotest.(check bool)
        "has constraints" true
        (List.length result.defun_constraints > 0)
  | None -> Alcotest.fail "expected defun result"

let test_defun_no_declare_still_infers () =
  (* Without declaration, defun should still infer as before *)
  reset_tvar_counter ();
  let sexp = parse {|(defun foo (x)
              (+ x 1))|} in
  let env =
    Env.extend_mono "+" (arrow [ Prim.int; Prim.int ] Prim.int) Env.empty
  in
  match Infer.infer_defun env sexp with
  | Some result ->
      Alcotest.(check string) "defun name" "foo" result.name;
      let ty_str = to_string result.fn_type in
      (* Should infer Int -> Int due to + constraint *)
      Alcotest.(check string) "inferred type" "(-> (Int) Int)" ty_str
  | None -> Alcotest.fail "expected defun result"

let test_defun_declare_tart_with_body () =
  (* Declaration followed by actual body expressions *)
  reset_tvar_counter ();
  let sexp =
    parse
      {|(defun greet (name)
              (declare (tart (string) -> string))
              (concat "Hello, " name))|}
  in
  let env =
    Env.extend_mono "concat"
      (arrow [ Prim.string; Prim.string ] Prim.string)
      Env.empty
  in
  match Infer.infer_defun env sexp with
  | Some result ->
      let ty_str = to_string result.fn_type in
      Alcotest.(check string) "declared type" "(-> (String) String)" ty_str
  | None -> Alcotest.fail "expected defun result"

(* =============================================================================
   Tart Annotation Tests: (tart TYPE FORM)
   ============================================================================= *)

let test_tart_annotation_result_type () =
  (* (tart string "hello") should have type String *)
  let ty = infer_type {|(tart string "hello")|} in
  Alcotest.(check string) "tart annotation result" "String" ty

let test_tart_annotation_generates_constraint () =
  (* (tart int "wrong") generates a constraint: declared = inferred *)
  let count = constraint_count {|(tart int "wrong")|} in
  Alcotest.(check int) "tart annotation constraint" 1 count

let test_tart_annotation_uses_declared_type () =
  (* The result type should be the declared type, not the inferred type *)
  let ty = infer_type "(tart string 42)" in
  (* Result is String (declared), even though 42 is Int *)
  Alcotest.(check string) "uses declared type" "String" ty

let test_tart_annotation_nested () =
  (* Annotations can be nested in expressions.
     The (tart int 1) has type Int, so it can be used as argument to +.
     We check that the tart form itself has type Int and generates
     constraints that can be solved. *)
  let ty = infer_type "(let ((x (tart int 42))) x)" in
  Alcotest.(check string) "nested annotation" "Int" ty

let test_tart_annotation_list_type () =
  (* (tart (list int) '(1 2 3)) should have type (List Int) *)
  let ty = infer_type {|(tart (list int) '(1 2 3))|} in
  Alcotest.(check string) "list annotation" "(List Int)" ty

let test_tart_annotation_polymorphic () =
  (* Polymorphic annotations like (tart (a -> a) (lambda (x) x)) *)
  let ty = infer_type {|(tart ((a) -> a) (lambda (x) x))|} in
  (* Result should be an arrow type *)
  Alcotest.(check bool)
    "polymorphic annotation" true
    (String.sub ty 0 4 = "(-> ")

(* =============================================================================
   Explicit Type Instantiation Tests: (tart [T1 T2 ...] fn args...)
   ============================================================================= *)

let test_at_type_result_is_tvar () =
  (* (tart [int] identity 42) returns a fresh type variable before solving *)
  let env =
    Env.extend_poly "identity" [ "a" ] (arrow [ TCon "a" ] (TCon "a")) Env.empty
  in
  let ty = infer_type ~env "(tart [int] identity 42)" in
  (* Result is a type variable that will unify with Int after solving *)
  Alcotest.(check bool)
    "tart instantiation result is tvar" true
    (String.length ty > 0 && ty.[0] = '\'')

let test_at_type_generates_constraints () =
  (* (tart [int] identity 42) generates constraints for instantiation *)
  let env =
    Env.extend_poly "identity" [ "a" ] (arrow [ TCon "a" ] (TCon "a")) Env.empty
  in
  let count = constraint_count ~env "(tart [int] identity 42)" in
  (* 1 constraint for fn type, 1 for the argument *)
  Alcotest.(check bool) "tart instantiation generates constraints" true (count >= 2)

let test_at_type_with_placeholder () =
  (* (tart [_] identity 42) uses placeholder for inference *)
  let env =
    Env.extend_poly "identity" [ "a" ] (arrow [ TCon "a" ] (TCon "a")) Env.empty
  in
  let count = constraint_count ~env "(tart [_] identity 42)" in
  Alcotest.(check bool) "placeholder generates constraints" true (count >= 2)

let test_at_type_multi_arg_function () =
  (* (tart [int string] pair 1 "hi") instantiates multi-param function *)
  let pair_ty =
    arrow [ TCon "a"; TCon "b" ] (TApp (TCon "cons", [ TCon "a"; TCon "b" ]))
  in
  let env = Env.extend_poly "pair" [ "a"; "b" ] pair_ty Env.empty in
  let count = constraint_count ~env {|(tart [int string] pair 1 "hi")|} in
  Alcotest.(check bool) "multi-arg generates constraints" true (count >= 3)

let test_at_type_partial_instantiation () =
  (* (tart [_ string] pair 1 "hi") infers first type arg *)
  let pair_ty =
    arrow [ TCon "a"; TCon "b" ] (TApp (TCon "cons", [ TCon "a"; TCon "b" ]))
  in
  let env = Env.extend_poly "pair" [ "a"; "b" ] pair_ty Env.empty in
  let count = constraint_count ~env {|(tart [_ string] pair 1 "hi")|} in
  Alcotest.(check bool)
    "partial instantiation generates constraints" true (count >= 3)

let test_at_type_no_type_args () =
  (* (tart [] identity 42) with empty type args - uses all inference *)
  let env =
    Env.extend_poly "identity" [ "a" ] (arrow [ TCon "a" ] (TCon "a")) Env.empty
  in
  let count = constraint_count ~env "(tart [] identity 42)" in
  Alcotest.(check bool) "empty type args" true (count >= 1)

(* =============================================================================
   Constraint Content Tests
   ============================================================================= *)

let test_constraint_has_location () =
  let sexp = parse "(+ 1 2)" in
  let result = Infer.infer Env.empty sexp in
  match result.constraints with
  | c :: _ ->
      let loc = c.Tart.Constraint.loc in
      Alcotest.(check bool)
        "constraint has location" true
        (loc.start_pos.offset >= 0)
  | [] -> Alcotest.fail "expected constraint"

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "infer"
    [
      ( "literals",
        [
          Alcotest.test_case "int literal" `Quick test_int_literal;
          Alcotest.test_case "negative int" `Quick test_negative_int;
          Alcotest.test_case "float literal" `Quick test_float_literal;
          Alcotest.test_case "string literal" `Quick test_string_literal;
          Alcotest.test_case "empty string" `Quick test_empty_string;
          Alcotest.test_case "keyword literal" `Quick test_keyword_literal;
          Alcotest.test_case "char literal" `Quick test_char_literal;
          Alcotest.test_case "nil literal" `Quick test_nil_literal;
        ] );
      ( "variables",
        [
          Alcotest.test_case "bound variable" `Quick test_bound_var;
          Alcotest.test_case "unbound variable" `Quick test_unbound_var;
          Alcotest.test_case "poly instantiation" `Quick
            test_poly_var_instantiation;
        ] );
      ( "quote",
        [
          Alcotest.test_case "quoted symbol" `Quick test_quoted_symbol;
          Alcotest.test_case "quoted list" `Quick test_quoted_list;
          Alcotest.test_case "quoted int" `Quick test_quoted_int;
          Alcotest.test_case "quoted string" `Quick test_quoted_string;
        ] );
      ( "lambda",
        [
          Alcotest.test_case "nullary lambda" `Quick test_lambda_no_params;
          Alcotest.test_case "identity lambda" `Quick test_lambda_one_param;
          Alcotest.test_case "two-param lambda" `Quick test_lambda_two_params;
          Alcotest.test_case "lambda body uses param" `Quick
            test_lambda_body_uses_param;
          Alcotest.test_case "lambda constraint count" `Quick
            test_lambda_constraint_count;
        ] );
      ( "application",
        [
          Alcotest.test_case "generates constraint" `Quick
            test_application_generates_constraint;
          Alcotest.test_case "result is tvar" `Quick
            test_application_result_type;
          Alcotest.test_case "known function" `Quick
            test_known_function_application;
          Alcotest.test_case "multi-arg" `Quick test_multi_arg_application;
        ] );
      ( "if",
        [
          Alcotest.test_case "if with else" `Quick test_if_with_else;
          Alcotest.test_case "if result type" `Quick test_if_result_type;
          Alcotest.test_case "if without else" `Quick test_if_without_else;
        ] );
      ( "let",
        [
          Alcotest.test_case "simple let" `Quick test_let_simple;
          Alcotest.test_case "multiple bindings" `Quick
            test_let_multiple_bindings;
          Alcotest.test_case "shadowing" `Quick test_let_binding_shadowing;
          Alcotest.test_case "nil binding" `Quick test_let_nil_binding;
          Alcotest.test_case "let* sequential" `Quick test_let_star_sequential;
        ] );
      ( "progn",
        [
          Alcotest.test_case "empty progn" `Quick test_progn_empty;
          Alcotest.test_case "single progn" `Quick test_progn_single;
          Alcotest.test_case "multiple progn" `Quick test_progn_multiple;
        ] );
      ( "boolean",
        [
          Alcotest.test_case "empty and" `Quick test_and_empty;
          Alcotest.test_case "empty or" `Quick test_or_empty;
          Alcotest.test_case "not returns bool" `Quick test_not_returns_bool;
          Alcotest.test_case "and last type" `Quick test_and_last_type;
          Alcotest.test_case "or last type" `Quick test_or_last_type;
        ] );
      ( "vector",
        [
          Alcotest.test_case "empty vector" `Quick test_empty_vector;
          Alcotest.test_case "int vector" `Quick test_int_vector;
          Alcotest.test_case "string vector" `Quick test_string_vector;
          Alcotest.test_case "mixed constraints" `Quick
            test_mixed_vector_constraints;
        ] );
      ( "cond",
        [
          Alcotest.test_case "single clause" `Quick test_cond_single_clause;
          Alcotest.test_case "multiple clauses" `Quick
            test_cond_multiple_clauses;
        ] );
      ( "pcase",
        [
          Alcotest.test_case "simple" `Quick test_pcase_simple;
          Alcotest.test_case "underscore" `Quick test_pcase_underscore;
          Alcotest.test_case "constraint count" `Quick
            test_pcase_constraint_count;
          Alcotest.test_case "branches unify" `Quick test_pcase_branches_unify;
          Alcotest.test_case "exhaustive" `Quick test_pcase_exhaustive;
          Alcotest.test_case "with env" `Quick test_pcase_with_env;
        ] );
      ( "defun",
        [
          Alcotest.test_case "returns symbol" `Quick test_defun_returns_symbol;
          Alcotest.test_case "infer simple" `Quick test_defun_infer_simple;
          Alcotest.test_case "infer identity" `Quick test_defun_infer_identity;
          Alcotest.test_case "not defun" `Quick test_defun_not_defun;
        ] );
      ( "declare-tart",
        [
          Alcotest.test_case "monomorphic" `Quick
            test_defun_declare_tart_monomorphic;
          Alcotest.test_case "polymorphic" `Quick
            test_defun_declare_tart_polymorphic;
          Alcotest.test_case "explicit forall" `Quick
            test_defun_declare_tart_explicit_forall;
          Alcotest.test_case "body mismatch" `Quick
            test_defun_declare_tart_body_mismatch;
          Alcotest.test_case "no declare infers" `Quick
            test_defun_no_declare_still_infers;
          Alcotest.test_case "with body" `Quick
            test_defun_declare_tart_with_body;
        ] );
      ( "tart-annotation",
        [
          Alcotest.test_case "result type" `Quick
            test_tart_annotation_result_type;
          Alcotest.test_case "generates constraint" `Quick
            test_tart_annotation_generates_constraint;
          Alcotest.test_case "uses declared type" `Quick
            test_tart_annotation_uses_declared_type;
          Alcotest.test_case "nested" `Quick test_tart_annotation_nested;
          Alcotest.test_case "list type" `Quick test_tart_annotation_list_type;
          Alcotest.test_case "polymorphic" `Quick
            test_tart_annotation_polymorphic;
        ] );
      ( "tart_instantiation",
        [
          Alcotest.test_case "result is tvar" `Quick test_at_type_result_is_tvar;
          Alcotest.test_case "generates constraints" `Quick
            test_at_type_generates_constraints;
          Alcotest.test_case "with placeholder" `Quick
            test_at_type_with_placeholder;
          Alcotest.test_case "multi-arg function" `Quick
            test_at_type_multi_arg_function;
          Alcotest.test_case "partial instantiation" `Quick
            test_at_type_partial_instantiation;
          Alcotest.test_case "no type args" `Quick test_at_type_no_type_args;
        ] );
      ( "constraints",
        [
          Alcotest.test_case "has location" `Quick test_constraint_has_location;
        ] );
    ]
