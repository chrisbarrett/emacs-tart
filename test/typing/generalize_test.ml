(** Tests for levels-based generalization *)

open Tart.Types
module Env = Tart.Type_env
module Infer = Tart.Infer
module Unify = Tart.Unify
module G = Tart.Generalize

(** Parse a string to S-expression for testing *)
let parse str =
  match Tart.Read.parse_one ~filename:"<test>" str with
  | Ok sexp -> sexp
  | Error msg -> failwith ("parse error: " ^ msg)

(** Helper to reset state before each test *)
let setup () = reset_tvar_counter ()

(** Helper to infer, solve, and return the final type string *)
let infer_and_solve ?(env = Env.empty) str =
  setup ();
  let sexp = parse str in
  let result = Infer.infer env sexp in
  let _ = Unify.solve result.constraints in
  to_string result.ty

(* =============================================================================
   Syntactic Value Tests
   ============================================================================= *)

let test_lambda_is_value () =
  let sexp = parse "(lambda (x) x)" in
  Alcotest.(check bool) "lambda is value" true (G.is_syntactic_value sexp)

let test_literal_is_value () =
  let sexp = parse "42" in
  Alcotest.(check bool) "literal is value" true (G.is_syntactic_value sexp)

let test_quote_is_value () =
  let sexp = parse "'foo" in
  Alcotest.(check bool) "quote is value" true (G.is_syntactic_value sexp)

let test_symbol_is_value () =
  let sexp = parse "x" in
  Alcotest.(check bool) "symbol is value" true (G.is_syntactic_value sexp)

let test_application_not_value () =
  let sexp = parse "(f 1)" in
  Alcotest.(check bool)
    "application not value" false
    (G.is_syntactic_value sexp)

let test_if_not_value () =
  let sexp = parse "(if x 1 2)" in
  Alcotest.(check bool) "if not value" false (G.is_syntactic_value sexp)

let test_let_not_value () =
  let sexp = parse "(let ((x 1)) x)" in
  Alcotest.(check bool) "let not value" false (G.is_syntactic_value sexp)

(* =============================================================================
   Generalization Tests
   ============================================================================= *)

let test_generalize_tvar_at_higher_level () =
  (* Type variable at level 1 should be generalized when outer level is 0 *)
  setup ();
  let tv = fresh_tvar 1 in
  (* level 1 *)
  let scheme = G.generalize 0 tv in
  (* outer level 0 *)
  match scheme with
  | Env.Poly ([ _ ], _) -> ()
  | _ -> Alcotest.fail "expected Poly scheme"

let test_no_generalize_tvar_at_same_level () =
  (* Type variable at level 0 should not be generalized when outer level is 0 *)
  setup ();
  let tv = fresh_tvar 0 in
  (* level 0 *)
  let scheme = G.generalize 0 tv in
  (* outer level 0 *)
  match scheme with
  | Env.Mono _ -> ()
  | _ -> Alcotest.fail "expected Mono scheme"

let test_generalize_arrow_with_tvars () =
  (* Function (-> ('a) 'a) should generalize to (forall (a) (-> (a) a)) *)
  setup ();
  let tv = fresh_tvar 1 in
  (* level 1 *)
  let fn_type = TArrow ([ PPositional tv ], tv) in
  let scheme = G.generalize 0 fn_type in
  match scheme with
  | Env.Poly ([ name ], TArrow ([ PPositional (TCon a) ], TCon b))
    when a = name && b = name ->
      ()
  | _ -> Alcotest.fail "expected polymorphic identity type"

let test_generalize_nested_tvars () =
  (* Multiple different tvars should all be generalized *)
  setup ();
  let tv1 = fresh_tvar 1 in
  let tv2 = fresh_tvar 1 in
  let fn_type = TArrow ([ PPositional tv1; PPositional tv2 ], tv1) in
  let scheme = G.generalize 0 fn_type in
  match scheme with
  | Env.Poly (vars, _) when List.length vars = 2 -> ()
  | _ -> Alcotest.fail "expected two type variables"

(* =============================================================================
   Full Integration Tests (with let bindings)
   ============================================================================= *)

let test_let_identity_polymorphic () =
  (* The classic test: (let ((id (lambda (x) x))) (id 1) (id "s"))
     should type-check because id is polymorphic *)
  let env =
    Env.extend_mono "progn" (arrow [ Prim.any; Prim.any ] Prim.any) Env.empty
  in
  let _ =
    infer_and_solve ~env
      "(let ((id (lambda (x) x)))\n       (progn (id 1) (id \"s\")))"
  in
  (* If it doesn't fail, test passes *)
  ()

let test_let_identity_used_at_int () =
  (* (let ((id (lambda (x) x))) (id 1)) should infer result as Int *)
  let ty = infer_and_solve "(let ((id (lambda (x) x))) (id 1))" in
  Alcotest.(check string) "result is Int" "Int" ty

let test_let_identity_used_at_string () =
  (* (let ((id (lambda (x) x))) (id "s")) should infer result as String *)
  let ty = infer_and_solve "(let ((id (lambda (x) x))) (id \"s\"))" in
  Alcotest.(check string) "result is String" "String" ty

let test_let_const_polymorphic () =
  (* const function: (let ((const (lambda (x y) x))) ...) *)
  let ty =
    infer_and_solve "(let ((const (lambda (x y) x))) (const 1 \"ignore\"))"
  in
  Alcotest.(check string) "const returns Int" "Int" ty

let test_let_star_generalization () =
  (* let* should also generalize *)
  let ty = infer_and_solve "(let* ((id (lambda (x) x)) (n (id 1))) n)" in
  Alcotest.(check string) "let* generalizes" "Int" ty

(* =============================================================================
   Value Restriction Tests
   ============================================================================= *)

let test_value_restriction_application () =
  (* (let ((x (f y))) ...) - x should be monomorphic because f y is not a value *)
  setup ();
  let env =
    Env.of_list
      [
        ("f", Env.Poly ([ "a" ], TArrow ([ PPositional (TCon "a") ], TCon "a")));
        ("y", Env.Mono Prim.int);
      ]
  in
  let sexp = parse "(let ((x (f y))) x)" in
  let result = Infer.infer env sexp in
  let _ = Unify.solve result.constraints in
  (* x should be Int (monomorphic), not polymorphic *)
  let ty_str = to_string result.ty in
  Alcotest.(check string) "value restriction" "Int" ty_str

let test_value_restriction_reverse () =
  (* Spec R6 test: (let ((xs (reverse '()))) xs) has monomorphic type.
     reverse : forall a. (List a) -> (List a)
     Since (reverse '()) is a function application (not a syntactic value),
     xs should NOT be generalized - it stays at (List Any). *)
  setup ();
  let env =
    Env.of_list
      [
        ( "reverse",
          Env.Poly
            ( [ "a" ],
              TArrow ([ PPositional (list_of (TCon "a")) ], list_of (TCon "a"))
            ) );
      ]
  in
  let sexp = parse "(let ((xs (reverse '()))) xs)" in
  let result = Infer.infer env sexp in
  let _ = Unify.solve result.constraints in
  (* xs should be monomorphic (List Any), not polymorphic *)
  let ty_str = to_string result.ty in
  Alcotest.(check string)
    "reverse result monomorphic" "(List (Or Truthy Nil))" ty_str

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "generalize"
    [
      ( "syntactic values",
        [
          Alcotest.test_case "lambda is value" `Quick test_lambda_is_value;
          Alcotest.test_case "literal is value" `Quick test_literal_is_value;
          Alcotest.test_case "quote is value" `Quick test_quote_is_value;
          Alcotest.test_case "symbol is value" `Quick test_symbol_is_value;
          Alcotest.test_case "application not value" `Quick
            test_application_not_value;
          Alcotest.test_case "if not value" `Quick test_if_not_value;
          Alcotest.test_case "let not value" `Quick test_let_not_value;
        ] );
      ( "generalize",
        [
          Alcotest.test_case "tvar at higher level" `Quick
            test_generalize_tvar_at_higher_level;
          Alcotest.test_case "no generalize same level" `Quick
            test_no_generalize_tvar_at_same_level;
          Alcotest.test_case "arrow with tvars" `Quick
            test_generalize_arrow_with_tvars;
          Alcotest.test_case "nested tvars" `Quick test_generalize_nested_tvars;
        ] );
      ( "let polymorphism",
        [
          Alcotest.test_case "identity polymorphic" `Quick
            test_let_identity_polymorphic;
          Alcotest.test_case "identity at int" `Quick
            test_let_identity_used_at_int;
          Alcotest.test_case "identity at string" `Quick
            test_let_identity_used_at_string;
          Alcotest.test_case "const polymorphic" `Quick
            test_let_const_polymorphic;
          Alcotest.test_case "let* generalization" `Quick
            test_let_star_generalization;
        ] );
      ( "value restriction",
        [
          Alcotest.test_case "application not generalized" `Quick
            test_value_restriction_application;
          Alcotest.test_case "reverse monomorphic" `Quick
            test_value_restriction_reverse;
        ] );
    ]
