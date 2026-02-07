(** Tests for multi-clause function dispatch. *)

module Dispatch = Tart.Clause_dispatch
module Types = Tart.Types
module Prim = Types.Prim
module Env = Tart.Type_env
module Sexp = Tart.Sexp
module Loc = Tart.Location

let dummy = Loc.dummy_span
let to_string = Types.to_string

(* =============================================================================
   extract_arg_literal: keyword and quoted symbol extraction
   ============================================================================= *)

let test_extract_keyword () =
  let sexp = Sexp.Keyword ("name", dummy) in
  Alcotest.(check (option string))
    "keyword" (Some ":name")
    (Dispatch.extract_arg_literal sexp)

let test_extract_keyword_multi_word () =
  let sexp = Sexp.Keyword ("font-lock", dummy) in
  Alcotest.(check (option string))
    "hyphenated keyword" (Some ":font-lock")
    (Dispatch.extract_arg_literal sexp)

let test_extract_quoted_symbol () =
  let sexp =
    Sexp.List
      ([ Sexp.Symbol ("quote", dummy); Sexp.Symbol ("name", dummy) ], dummy)
  in
  Alcotest.(check (option string))
    "quoted symbol" (Some "name")
    (Dispatch.extract_arg_literal sexp)

let test_extract_plain_symbol () =
  let sexp = Sexp.Symbol ("x", dummy) in
  Alcotest.(check (option string))
    "plain symbol" None
    (Dispatch.extract_arg_literal sexp)

let test_extract_integer () =
  let sexp = Sexp.Int (42, dummy) in
  Alcotest.(check (option string))
    "integer" None
    (Dispatch.extract_arg_literal sexp)

let test_extract_string () =
  let sexp = Sexp.String ("hello", dummy) in
  Alcotest.(check (option string))
    "string" None
    (Dispatch.extract_arg_literal sexp)

let test_extract_list () =
  let sexp =
    Sexp.List ([ Sexp.Symbol ("foo", dummy); Sexp.Int (1, dummy) ], dummy)
  in
  Alcotest.(check (option string))
    "list expr" None
    (Dispatch.extract_arg_literal sexp)

(* =============================================================================
   substitute_tvar_names: type variable substitution
   ============================================================================= *)

let test_subst_tcon_match () =
  let subst = [ ("a", Prim.int) ] in
  let result = Dispatch.substitute_tvar_names subst (Types.TCon "a") in
  Alcotest.(check string) "TCon a → int" "int" (to_string result)

let test_subst_tcon_no_match () =
  let subst = [ ("a", Prim.int) ] in
  let result = Dispatch.substitute_tvar_names subst (Types.TCon "b") in
  Alcotest.(check string) "TCon b unchanged" "b" (to_string result)

let test_subst_empty () =
  let result = Dispatch.substitute_tvar_names [] Prim.string in
  Alcotest.(check string) "empty subst → identity" "string" (to_string result)

let test_subst_arrow () =
  let subst = [ ("a", Prim.int); ("b", Prim.string) ] in
  let ty =
    Types.TArrow ([ Types.PPositional (Types.TCon "a") ], Types.TCon "b")
  in
  let result = Dispatch.substitute_tvar_names subst ty in
  Alcotest.(check string)
    "arrow substituted" "(-> (int) string)" (to_string result)

let test_subst_union () =
  let subst = [ ("a", Prim.int) ] in
  let ty = Types.TUnion [ Types.TCon "a"; Prim.string ] in
  let result = Dispatch.substitute_tvar_names subst ty in
  Alcotest.(check string)
    "union substituted" "(Or int string)" (to_string result)

let test_subst_tapp () =
  let subst = [ ("a", Prim.int) ] in
  let ty = Types.TApp (Types.TCon "List", [ Types.TCon "a" ]) in
  let result = Dispatch.substitute_tvar_names subst ty in
  Alcotest.(check string) "tapp substituted" "(List int)" (to_string result)

let test_subst_forall_shadows () =
  (* Forall binding should shadow the substitution *)
  let subst = [ ("a", Prim.int) ] in
  let ty = Types.TForall ([ "a" ], Types.TCon "a") in
  let result = Dispatch.substitute_tvar_names subst ty in
  Alcotest.(check string) "forall shadows" "(forall (a) a)" (to_string result)

let test_subst_forall_no_shadow () =
  (* Forall binding for 'b' should not shadow 'a' *)
  let subst = [ ("a", Prim.int) ] in
  let ty = Types.TForall ([ "b" ], Types.TCon "a") in
  let result = Dispatch.substitute_tvar_names subst ty in
  Alcotest.(check string)
    "forall doesn't shadow other" "(forall (b) int)" (to_string result)

let test_subst_tuple () =
  let subst = [ ("a", Prim.int); ("b", Prim.string) ] in
  let ty = Types.TTuple [ Types.TCon "a"; Types.TCon "b" ] in
  let result = Dispatch.substitute_tvar_names subst ty in
  Alcotest.(check string)
    "tuple substituted" "(Tuple int string)" (to_string result)

let test_subst_row () =
  let subst = [ ("a", Prim.int) ] in
  let ty =
    Types.TRow { row_fields = [ ("x", Types.TCon "a") ]; row_var = None }
  in
  let result = Dispatch.substitute_tvar_names subst ty in
  match result with
  | Types.TRow { row_fields = [ ("x", t) ]; row_var = None } ->
      Alcotest.(check string) "row field substituted" "int" (to_string t)
  | _ -> Alcotest.fail "expected TRow"

let test_subst_row_var () =
  let subst = [ ("r", Prim.any) ] in
  let ty =
    Types.TRow
      { row_fields = [ ("x", Prim.int) ]; row_var = Some (Types.TCon "r") }
  in
  let result = Dispatch.substitute_tvar_names subst ty in
  match result with
  | Types.TRow { row_fields = [ ("x", _) ]; row_var = Some rv } ->
      Alcotest.(check string)
        "row var substituted" "(Or truthy nil)" (to_string rv)
  | _ -> Alcotest.fail "expected TRow with row_var"

let test_subst_literal_passthrough () =
  let subst = [ ("a", Prim.int) ] in
  let ty = Types.TLiteral (Types.LitInt 42, Prim.int) in
  let result = Dispatch.substitute_tvar_names subst ty in
  Alcotest.(check string) "literal unchanged" "42" (to_string result)

let test_subst_linked_tvar () =
  (* TVar with Link should follow the link *)
  let subst = [ ("a", Prim.string) ] in
  let ty = Types.TVar (ref (Types.Link (Types.TCon "a"))) in
  let result = Dispatch.substitute_tvar_names subst ty in
  Alcotest.(check string) "linked tvar resolved" "string" (to_string result)

let test_subst_unbound_tvar () =
  let subst = [ ("a", Prim.string) ] in
  let tv = Types.fresh_tvar 0 in
  let result = Dispatch.substitute_tvar_names subst tv in
  (* Unbound tvar with no matching name → unchanged *)
  Alcotest.(check bool) "unbound tvar unchanged" true (Types.equal result tv)

(* =============================================================================
   try_dispatch: single clause, no literals
   ============================================================================= *)

let make_clause ?(diagnostic = None) params ret : Env.loaded_clause =
  { Env.lc_params = params; lc_return = ret; lc_diagnostic = diagnostic }

let test_dispatch_single_clause_match () =
  let env = Env.empty in
  let clause = make_clause [ Types.PPositional Prim.int ] Prim.string in
  let result =
    Dispatch.try_dispatch env ~tvar_names:[] ~clauses:[ clause ]
      ~arg_types:[ Prim.int ] ~arg_literals:[ None ] ~loc:dummy
  in
  match result with
  | Some (ty, None) ->
      Alcotest.(check string) "returns string" "string" (to_string ty)
  | Some (_, Some _) -> Alcotest.fail "unexpected diagnostic"
  | None -> Alcotest.fail "expected match"

let test_dispatch_single_clause_no_match () =
  let env = Env.empty in
  let clause = make_clause [ Types.PPositional Prim.int ] Prim.string in
  let result =
    Dispatch.try_dispatch env ~tvar_names:[] ~clauses:[ clause ]
      ~arg_types:[ Prim.string ] ~arg_literals:[ None ] ~loc:dummy
  in
  Alcotest.(check bool) "no match" true (Option.is_none result)

let test_dispatch_empty_clauses () =
  let env = Env.empty in
  let result =
    Dispatch.try_dispatch env ~tvar_names:[] ~clauses:[] ~arg_types:[]
      ~arg_literals:[] ~loc:dummy
  in
  Alcotest.(check bool) "empty clauses → None" true (Option.is_none result)

(* =============================================================================
   try_dispatch: multi-clause, first match wins
   ============================================================================= *)

let test_dispatch_multi_first_wins () =
  let env = Env.empty in
  let clause1 = make_clause [ Types.PPositional Prim.int ] Prim.string in
  let clause2 = make_clause [ Types.PPositional Prim.any ] Prim.nil in
  let result =
    Dispatch.try_dispatch env ~tvar_names:[] ~clauses:[ clause1; clause2 ]
      ~arg_types:[ Prim.int ] ~arg_literals:[ None ] ~loc:dummy
  in
  match result with
  | Some (ty, None) ->
      Alcotest.(check string) "first clause wins" "string" (to_string ty)
  | _ -> Alcotest.fail "expected first clause match"

let test_dispatch_multi_fallback () =
  let env = Env.empty in
  let clause1 = make_clause [ Types.PPositional Prim.int ] Prim.string in
  let clause2 = make_clause [ Types.PPositional Prim.any ] Prim.nil in
  let result =
    Dispatch.try_dispatch env ~tvar_names:[] ~clauses:[ clause1; clause2 ]
      ~arg_types:[ Prim.string ] ~arg_literals:[ None ] ~loc:dummy
  in
  match result with
  | Some (ty, None) ->
      Alcotest.(check string) "falls to second clause" "nil" (to_string ty)
  | _ -> Alcotest.fail "expected second clause match"

(* =============================================================================
   try_dispatch: literal parameters
   ============================================================================= *)

let test_dispatch_literal_keyword_match () =
  let env = Env.empty in
  let clause_name =
    make_clause
      [ Types.PPositional Prim.any; Types.PLiteral ":name" ]
      Prim.string
  in
  let clause_fallback =
    make_clause
      [ Types.PPositional Prim.any; Types.PPositional Prim.keyword ]
      Prim.any
  in
  let result =
    Dispatch.try_dispatch env ~tvar_names:[]
      ~clauses:[ clause_name; clause_fallback ]
      ~arg_types:[ Prim.any; Prim.keyword ] ~arg_literals:[ None; Some ":name" ]
      ~loc:dummy
  in
  match result with
  | Some (ty, None) ->
      Alcotest.(check string) "literal :name matches" "string" (to_string ty)
  | _ -> Alcotest.fail "expected literal match"

let test_dispatch_literal_keyword_mismatch () =
  let env = Env.empty in
  let clause_name =
    make_clause
      [ Types.PPositional Prim.any; Types.PLiteral ":name" ]
      Prim.string
  in
  let clause_fallback =
    make_clause
      [ Types.PPositional Prim.any; Types.PPositional Prim.keyword ]
      Prim.int
  in
  let result =
    Dispatch.try_dispatch env ~tvar_names:[]
      ~clauses:[ clause_name; clause_fallback ]
      ~arg_types:[ Prim.any; Prim.keyword ] ~arg_literals:[ None; Some ":age" ]
      ~loc:dummy
  in
  match result with
  | Some (ty, None) ->
      Alcotest.(check string) ":age falls to fallback" "int" (to_string ty)
  | _ -> Alcotest.fail "expected fallback match"

let test_dispatch_literal_no_literal_arg () =
  let env = Env.empty in
  let clause_name = make_clause [ Types.PLiteral ":name" ] Prim.string in
  let clause_fallback =
    make_clause [ Types.PPositional Prim.keyword ] Prim.int
  in
  let result =
    Dispatch.try_dispatch env ~tvar_names:[]
      ~clauses:[ clause_name; clause_fallback ]
      ~arg_types:[ Prim.keyword ] ~arg_literals:[ None ] ~loc:dummy
  in
  match result with
  | Some (ty, None) ->
      Alcotest.(check string)
        "non-literal arg skips PLiteral" "int" (to_string ty)
  | _ -> Alcotest.fail "expected fallback match"

let test_dispatch_quoted_symbol_match () =
  let env = Env.empty in
  let clause_name = make_clause [ Types.PLiteral "name" ] Prim.string in
  let clause_fallback =
    make_clause [ Types.PPositional Prim.symbol ] Prim.any
  in
  let result =
    Dispatch.try_dispatch env ~tvar_names:[]
      ~clauses:[ clause_name; clause_fallback ]
      ~arg_types:[ Prim.symbol ] ~arg_literals:[ Some "name" ] ~loc:dummy
  in
  match result with
  | Some (ty, None) ->
      Alcotest.(check string) "quoted symbol matches" "string" (to_string ty)
  | _ -> Alcotest.fail "expected literal match"

(* =============================================================================
   try_dispatch: polymorphic clauses with tvar_names
   ============================================================================= *)

let test_dispatch_polymorphic_clause () =
  let env = Env.empty in
  (* A clause: (a) → a (identity-like) *)
  let clause =
    make_clause [ Types.PPositional (Types.TCon "a") ] (Types.TCon "a")
  in
  let result =
    Dispatch.try_dispatch env ~tvar_names:[ "a" ] ~clauses:[ clause ]
      ~arg_types:[ Prim.int ] ~arg_literals:[ None ] ~loc:dummy
  in
  match result with
  | Some (ty, None) ->
      Alcotest.(check string)
        "polymorphic clause returns int" "int" (to_string ty)
  | _ -> Alcotest.fail "expected polymorphic match"

let test_dispatch_polymorphic_two_params () =
  let env = Env.empty in
  (* Clause: (a, a) → a — both params must unify to same type *)
  let clause =
    make_clause
      [ Types.PPositional (Types.TCon "a"); Types.PPositional (Types.TCon "a") ]
      (Types.TCon "a")
  in
  (* int, int → should match *)
  let result =
    Dispatch.try_dispatch env ~tvar_names:[ "a" ] ~clauses:[ clause ]
      ~arg_types:[ Prim.int; Prim.int ] ~arg_literals:[ None; None ] ~loc:dummy
  in
  match result with
  | Some (ty, None) ->
      Alcotest.(check string) "both int → int" "int" (to_string ty)
  | _ -> Alcotest.fail "expected match"

let test_dispatch_polymorphic_mismatch () =
  let env = Env.empty in
  (* Clause: (a, a) → a — both params must unify to same type *)
  let clause =
    make_clause
      [ Types.PPositional (Types.TCon "a"); Types.PPositional (Types.TCon "a") ]
      (Types.TCon "a")
  in
  (* int, string → should NOT match (a can't be both) *)
  let result =
    Dispatch.try_dispatch env ~tvar_names:[ "a" ] ~clauses:[ clause ]
      ~arg_types:[ Prim.int; Prim.string ] ~arg_literals:[ None; None ]
      ~loc:dummy
  in
  Alcotest.(check bool) "mismatched types → None" true (Option.is_none result)

(* =============================================================================
   try_dispatch: diagnostics
   ============================================================================= *)

let test_dispatch_with_diagnostic () =
  let env = Env.empty in
  let diag : Env.loaded_diagnostic =
    { ld_severity = Env.DiagWarn; ld_message = "deprecated"; ld_args = [] }
  in
  let clause =
    make_clause ~diagnostic:(Some diag) [ Types.PPositional Prim.any ] Prim.any
  in
  let result =
    Dispatch.try_dispatch env ~tvar_names:[] ~clauses:[ clause ]
      ~arg_types:[ Prim.int ] ~arg_literals:[ None ] ~loc:dummy
  in
  match result with
  | Some (_, Some d) -> (
      Alcotest.(check string) "diagnostic message" "deprecated" d.rcd_message;
      match d.rcd_severity with
      | Env.DiagWarn -> ()
      | _ -> Alcotest.fail "expected warn severity")
  | Some (_, None) -> Alcotest.fail "expected diagnostic"
  | None -> Alcotest.fail "expected match"

let test_dispatch_diagnostic_format_args () =
  let env = Env.empty in
  let diag : Env.loaded_diagnostic =
    {
      ld_severity = Env.DiagNote;
      ld_message = "got %s instead of %s";
      ld_args = [ "a"; "b" ];
    }
  in
  (* Clause: (a, b) → any, with diagnostic referencing a and b *)
  let clause =
    make_clause ~diagnostic:(Some diag)
      [ Types.PPositional (Types.TCon "a"); Types.PPositional (Types.TCon "b") ]
      Prim.any
  in
  let result =
    Dispatch.try_dispatch env ~tvar_names:[ "a"; "b" ] ~clauses:[ clause ]
      ~arg_types:[ Prim.int; Prim.string ] ~arg_literals:[ None; None ]
      ~loc:dummy
  in
  match result with
  | Some (_, Some d) ->
      Alcotest.(check string)
        "format args resolved" "got int instead of string" d.rcd_message
  | Some (_, None) -> Alcotest.fail "expected diagnostic"
  | None -> Alcotest.fail "expected match"

let test_dispatch_no_diagnostic () =
  let env = Env.empty in
  let clause = make_clause [ Types.PPositional Prim.any ] Prim.any in
  let result =
    Dispatch.try_dispatch env ~tvar_names:[] ~clauses:[ clause ]
      ~arg_types:[ Prim.int ] ~arg_literals:[ None ] ~loc:dummy
  in
  match result with
  | Some (_, None) -> ()
  | Some (_, Some _) -> Alcotest.fail "unexpected diagnostic"
  | None -> Alcotest.fail "expected match"

let test_dispatch_diagnostic_first_clause_only () =
  let env = Env.empty in
  let diag : Env.loaded_diagnostic =
    { ld_severity = Env.DiagWarn; ld_message = "unsafe"; ld_args = [] }
  in
  let clause1 = make_clause [ Types.PPositional Prim.int ] Prim.string in
  let clause2 =
    make_clause ~diagnostic:(Some diag) [ Types.PPositional Prim.any ] Prim.any
  in
  (* int arg matches clause1 (no diagnostic), not clause2 *)
  let result =
    Dispatch.try_dispatch env ~tvar_names:[] ~clauses:[ clause1; clause2 ]
      ~arg_types:[ Prim.int ] ~arg_literals:[ None ] ~loc:dummy
  in
  match result with
  | Some (ty, None) ->
      Alcotest.(check string)
        "first clause, no diagnostic" "string" (to_string ty)
  | _ -> Alcotest.fail "expected first clause without diagnostic"

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "clause_dispatch"
    [
      ( "extract_arg_literal",
        [
          Alcotest.test_case "keyword" `Quick test_extract_keyword;
          Alcotest.test_case "hyphenated keyword" `Quick
            test_extract_keyword_multi_word;
          Alcotest.test_case "quoted symbol" `Quick test_extract_quoted_symbol;
          Alcotest.test_case "plain symbol" `Quick test_extract_plain_symbol;
          Alcotest.test_case "integer" `Quick test_extract_integer;
          Alcotest.test_case "string" `Quick test_extract_string;
          Alcotest.test_case "list" `Quick test_extract_list;
        ] );
      ( "substitute_tvar_names",
        [
          Alcotest.test_case "TCon match" `Quick test_subst_tcon_match;
          Alcotest.test_case "TCon no match" `Quick test_subst_tcon_no_match;
          Alcotest.test_case "empty subst" `Quick test_subst_empty;
          Alcotest.test_case "arrow" `Quick test_subst_arrow;
          Alcotest.test_case "union" `Quick test_subst_union;
          Alcotest.test_case "tapp" `Quick test_subst_tapp;
          Alcotest.test_case "forall shadows" `Quick test_subst_forall_shadows;
          Alcotest.test_case "forall no shadow" `Quick
            test_subst_forall_no_shadow;
          Alcotest.test_case "tuple" `Quick test_subst_tuple;
          Alcotest.test_case "row" `Quick test_subst_row;
          Alcotest.test_case "row var" `Quick test_subst_row_var;
          Alcotest.test_case "literal passthrough" `Quick
            test_subst_literal_passthrough;
          Alcotest.test_case "linked tvar" `Quick test_subst_linked_tvar;
          Alcotest.test_case "unbound tvar" `Quick test_subst_unbound_tvar;
        ] );
      ( "dispatch single clause",
        [
          Alcotest.test_case "match" `Quick test_dispatch_single_clause_match;
          Alcotest.test_case "no match" `Quick
            test_dispatch_single_clause_no_match;
          Alcotest.test_case "empty clauses" `Quick test_dispatch_empty_clauses;
        ] );
      ( "dispatch multi clause",
        [
          Alcotest.test_case "first wins" `Quick test_dispatch_multi_first_wins;
          Alcotest.test_case "fallback" `Quick test_dispatch_multi_fallback;
        ] );
      ( "dispatch literals",
        [
          Alcotest.test_case "keyword match" `Quick
            test_dispatch_literal_keyword_match;
          Alcotest.test_case "keyword mismatch" `Quick
            test_dispatch_literal_keyword_mismatch;
          Alcotest.test_case "no literal arg" `Quick
            test_dispatch_literal_no_literal_arg;
          Alcotest.test_case "quoted symbol" `Quick
            test_dispatch_quoted_symbol_match;
        ] );
      ( "dispatch polymorphic",
        [
          Alcotest.test_case "identity" `Quick test_dispatch_polymorphic_clause;
          Alcotest.test_case "two same params" `Quick
            test_dispatch_polymorphic_two_params;
          Alcotest.test_case "mismatch" `Quick
            test_dispatch_polymorphic_mismatch;
        ] );
      ( "dispatch diagnostics",
        [
          Alcotest.test_case "with diagnostic" `Quick
            test_dispatch_with_diagnostic;
          Alcotest.test_case "format args" `Quick
            test_dispatch_diagnostic_format_args;
          Alcotest.test_case "no diagnostic" `Quick test_dispatch_no_diagnostic;
          Alcotest.test_case "first clause skips diagnostic" `Quick
            test_dispatch_diagnostic_first_clause_only;
        ] );
    ]
