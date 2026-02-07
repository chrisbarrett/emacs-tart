(** Tests for feature guard recognition in Narrow.analyze_condition (Spec 49).
*)

module Sexp = Tart.Sexp
module Loc = Tart.Location
module Narrow = Tart.Narrow
module Env = Tart.Type_env
module Types = Tart.Types

let dummy = Loc.dummy_span

(** Helper: make a quoted symbol '(quote name) sexp *)
let quote name =
  Sexp.List ([ Sexp.Symbol ("quote", dummy); Sexp.Symbol (name, dummy) ], dummy)

(** Helper: make a function call (fn args...) sexp *)
let call fn args = Sexp.List (Sexp.Symbol (fn, dummy) :: args, dummy)

(** Helper: make a bare symbol sexp *)
let sym name = Sexp.Symbol (name, dummy)

(** Empty env (no predicates registered) *)
let empty_env = Env.empty

(** Env with a stringp predicate registered *)
let env_with_stringp =
  Env.extend_predicate "stringp"
    { param_index = 0; param_name = "x"; narrowed_type = Types.Prim.string }
    Env.empty

(** Helper: classify analysis result to a string for easy comparison *)
let analysis_to_string (a : Narrow.condition_analysis) : string =
  match a with
  | Narrow.NoPredicate -> "NoPredicate"
  | Narrow.Predicate p -> Printf.sprintf "Predicate(%s)" p.var_name
  | Narrow.Predicates ps ->
      Printf.sprintf "Predicates(%s)"
        (String.concat ", "
           (List.map (fun (p : Narrow.predicate_info) -> p.var_name) ps))
  | Narrow.Guard g -> (
      match g with
      | Narrow.FeatureGuard f -> Printf.sprintf "Guard(FeatureGuard %s)" f
      | Narrow.FboundGuard f -> Printf.sprintf "Guard(FboundGuard %s)" f
      | Narrow.BoundGuard v -> Printf.sprintf "Guard(BoundGuard %s)" v
      | Narrow.BoundTrueGuard v -> Printf.sprintf "Guard(BoundTrueGuard %s)" v)
  | Narrow.Guards gs ->
      Printf.sprintf "Guards(%s)"
        (String.concat ", "
           (List.map
              (fun (g : Narrow.guard_info) ->
                match g with
                | Narrow.FeatureGuard f -> Printf.sprintf "FeatureGuard %s" f
                | Narrow.FboundGuard f -> Printf.sprintf "FboundGuard %s" f
                | Narrow.BoundGuard v -> Printf.sprintf "BoundGuard %s" v
                | Narrow.BoundTrueGuard v ->
                    Printf.sprintf "BoundTrueGuard %s" v)
              gs))
  | Narrow.PredicatesAndGuards (ps, gs) ->
      Printf.sprintf "PredicatesAndGuards([%s], [%s])"
        (String.concat ", "
           (List.map (fun (p : Narrow.predicate_info) -> p.var_name) ps))
        (String.concat ", "
           (List.map
              (fun (g : Narrow.guard_info) ->
                match g with
                | Narrow.FeatureGuard f -> Printf.sprintf "FeatureGuard %s" f
                | Narrow.FboundGuard f -> Printf.sprintf "FboundGuard %s" f
                | Narrow.BoundGuard v -> Printf.sprintf "BoundGuard %s" v
                | Narrow.BoundTrueGuard v ->
                    Printf.sprintf "BoundTrueGuard %s" v)
              gs))

let check = Alcotest.(check string)

(* =============================================================================
   featurep guard
   ============================================================================= *)

let test_featurep_basic () =
  (* (featurep 'json) → Guard (FeatureGuard "json") *)
  let cond = call "featurep" [ quote "json" ] in
  let result = Narrow.analyze_condition cond empty_env in
  check "featurep 'json" "Guard(FeatureGuard json)" (analysis_to_string result)

let test_featurep_other_feature () =
  (* (featurep 'libxml) → Guard (FeatureGuard "libxml") *)
  let cond = call "featurep" [ quote "libxml" ] in
  let result = Narrow.analyze_condition cond empty_env in
  check "featurep 'libxml" "Guard(FeatureGuard libxml)"
    (analysis_to_string result)

let test_featurep_non_literal () =
  (* (featurep x) where x is a variable → NoPredicate *)
  let cond = call "featurep" [ sym "x" ] in
  let result = Narrow.analyze_condition cond empty_env in
  check "featurep variable" "NoPredicate" (analysis_to_string result)

let test_featurep_no_args () =
  (* (featurep) with no args → NoPredicate *)
  let cond = call "featurep" [] in
  let result = Narrow.analyze_condition cond empty_env in
  check "featurep no args" "NoPredicate" (analysis_to_string result)

let test_featurep_extra_args () =
  (* (featurep 'json 'extra) → NoPredicate (too many args) *)
  let cond = call "featurep" [ quote "json"; quote "extra" ] in
  let result = Narrow.analyze_condition cond empty_env in
  check "featurep extra args" "NoPredicate" (analysis_to_string result)

(* =============================================================================
   fboundp guard
   ============================================================================= *)

let test_fboundp_basic () =
  (* (fboundp 'json-parse-string) → Guard (FboundGuard "json-parse-string") *)
  let cond = call "fboundp" [ quote "json-parse-string" ] in
  let result = Narrow.analyze_condition cond empty_env in
  check "fboundp 'json-parse-string" "Guard(FboundGuard json-parse-string)"
    (analysis_to_string result)

let test_fboundp_non_literal () =
  (* (fboundp x) → NoPredicate *)
  let cond = call "fboundp" [ sym "x" ] in
  let result = Narrow.analyze_condition cond empty_env in
  check "fboundp variable" "NoPredicate" (analysis_to_string result)

(* =============================================================================
   boundp guard
   ============================================================================= *)

let test_boundp_basic () =
  (* (boundp 'json-null) → Guard (BoundGuard "json-null") *)
  let cond = call "boundp" [ quote "json-null" ] in
  let result = Narrow.analyze_condition cond empty_env in
  check "boundp 'json-null" "Guard(BoundGuard json-null)"
    (analysis_to_string result)

let test_boundp_non_literal () =
  (* (boundp x) → NoPredicate *)
  let cond = call "boundp" [ sym "x" ] in
  let result = Narrow.analyze_condition cond empty_env in
  check "boundp variable" "NoPredicate" (analysis_to_string result)

(* =============================================================================
   bound-and-true-p guard
   ============================================================================= *)

let test_bound_and_true_p_basic () =
  (* (bound-and-true-p my-var) → Guard (BoundTrueGuard "my-var") *)
  let cond = call "bound-and-true-p" [ sym "my-var" ] in
  let result = Narrow.analyze_condition cond empty_env in
  check "bound-and-true-p my-var" "Guard(BoundTrueGuard my-var)"
    (analysis_to_string result)

let test_bound_and_true_p_quoted () =
  (* (bound-and-true-p 'my-var) — bound-and-true-p takes bare symbol, not quoted *)
  let cond = call "bound-and-true-p" [ quote "my-var" ] in
  let result = Narrow.analyze_condition cond empty_env in
  check "bound-and-true-p quoted" "NoPredicate" (analysis_to_string result)

(* =============================================================================
   and: combined guards (R16)
   ============================================================================= *)

let test_and_two_guards () =
  (* (and (featurep 'json) (fboundp 'json-parse-buffer)) →
     Guards [FeatureGuard "json"; FboundGuard "json-parse-buffer"] *)
  let cond =
    call "and"
      [
        call "featurep" [ quote "json" ];
        call "fboundp" [ quote "json-parse-buffer" ];
      ]
  in
  let result = Narrow.analyze_condition cond empty_env in
  check "and two guards"
    "Guards(FeatureGuard json, FboundGuard json-parse-buffer)"
    (analysis_to_string result)

let test_and_single_guard () =
  (* (and (featurep 'json) (something-else)) → single guard *)
  let cond =
    call "and"
      [ call "featurep" [ quote "json" ]; call "something-else" [ sym "x" ] ]
  in
  let result = Narrow.analyze_condition cond empty_env in
  check "and single guard" "Guard(FeatureGuard json)"
    (analysis_to_string result)

(* =============================================================================
   and: mixed predicates and guards
   ============================================================================= *)

let test_and_mixed_predicate_and_guard () =
  (* (and (featurep 'json) (stringp x)) with stringp registered →
     PredicatesAndGuards *)
  let cond =
    call "and" [ call "featurep" [ quote "json" ]; call "stringp" [ sym "x" ] ]
  in
  let result = Narrow.analyze_condition cond env_with_stringp in
  check "and mixed pred+guard" "PredicatesAndGuards([x], [FeatureGuard json])"
    (analysis_to_string result)

let test_and_only_predicates () =
  (* (and (stringp x) (stringp y)) with both as predicates → Predicates *)
  let env =
    Env.extend_predicate "stringp"
      { param_index = 0; param_name = "x"; narrowed_type = Types.Prim.string }
      Env.empty
  in
  let cond =
    call "and" [ call "stringp" [ sym "x" ]; call "stringp" [ sym "y" ] ]
  in
  let result = Narrow.analyze_condition cond env in
  check "and only predicates" "Predicates(x, y)" (analysis_to_string result)

let test_and_no_matches () =
  (* (and (unknown-fn x) (another y)) → NoPredicate *)
  let cond =
    call "and" [ call "unknown-fn" [ sym "x" ]; call "another" [ sym "y" ] ]
  in
  let result = Narrow.analyze_condition cond empty_env in
  check "and no matches" "NoPredicate" (analysis_to_string result)

(* =============================================================================
   non-guard functions
   ============================================================================= *)

let test_unknown_function () =
  (* (my-fn 'something) → NoPredicate *)
  let cond = call "my-fn" [ quote "something" ] in
  let result = Narrow.analyze_condition cond empty_env in
  check "unknown function" "NoPredicate" (analysis_to_string result)

let test_bare_symbol () =
  (* x (bare symbol, no call) → NoPredicate *)
  let cond = sym "x" in
  let result = Narrow.analyze_condition cond empty_env in
  check "bare symbol" "NoPredicate" (analysis_to_string result)

let test_predicate_takes_priority () =
  (* If featurep were also a registered predicate (hypothetical), predicate
     wins because analyze_single_predicate is checked first *)
  let env =
    Env.extend_predicate "featurep"
      { param_index = 0; param_name = "x"; narrowed_type = Types.Prim.string }
      Env.empty
  in
  let cond = call "featurep" [ sym "x" ] in
  let result = Narrow.analyze_condition cond env in
  check "predicate priority" "Predicate(x)" (analysis_to_string result)

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "narrow guards"
    [
      ( "featurep",
        [
          Alcotest.test_case "basic" `Quick test_featurep_basic;
          Alcotest.test_case "other feature" `Quick test_featurep_other_feature;
          Alcotest.test_case "non-literal" `Quick test_featurep_non_literal;
          Alcotest.test_case "no args" `Quick test_featurep_no_args;
          Alcotest.test_case "extra args" `Quick test_featurep_extra_args;
        ] );
      ( "fboundp",
        [
          Alcotest.test_case "basic" `Quick test_fboundp_basic;
          Alcotest.test_case "non-literal" `Quick test_fboundp_non_literal;
        ] );
      ( "boundp",
        [
          Alcotest.test_case "basic" `Quick test_boundp_basic;
          Alcotest.test_case "non-literal" `Quick test_boundp_non_literal;
        ] );
      ( "bound-and-true-p",
        [
          Alcotest.test_case "basic" `Quick test_bound_and_true_p_basic;
          Alcotest.test_case "quoted arg" `Quick test_bound_and_true_p_quoted;
        ] );
      ( "and combined",
        [
          Alcotest.test_case "two guards" `Quick test_and_two_guards;
          Alcotest.test_case "single guard" `Quick test_and_single_guard;
        ] );
      ( "and mixed",
        [
          Alcotest.test_case "pred + guard" `Quick
            test_and_mixed_predicate_and_guard;
          Alcotest.test_case "only predicates" `Quick test_and_only_predicates;
          Alcotest.test_case "no matches" `Quick test_and_no_matches;
        ] );
      ( "non-guard",
        [
          Alcotest.test_case "unknown function" `Quick test_unknown_function;
          Alcotest.test_case "bare symbol" `Quick test_bare_symbol;
          Alcotest.test_case "predicate priority" `Quick
            test_predicate_takes_priority;
        ] );
    ]
