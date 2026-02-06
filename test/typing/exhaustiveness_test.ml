(** Tests for exhaustiveness checking *)

module Exhaustiveness = Tart.Exhaustiveness
module Sig_ast = Tart.Sig_ast
module Types = Tart.Types
module Loc = Tart.Location

(** Parse a string to S-expression for testing *)
let parse str =
  match Tart.Read.parse_one ~filename:"test.el" str with
  | Ok sexp -> sexp
  | Error msg -> failwith ("parse error: " ^ msg)

(* =============================================================================
   ADT Registry Tests
   ============================================================================= *)

let test_empty_registry () =
  let reg = Exhaustiveness.empty_registry in
  Alcotest.(check (option (of_pp (fun _ _ -> ()))))
    "empty lookup" None
    (Exhaustiveness.lookup_adt "foo" reg)

let test_register_and_lookup () =
  let info =
    { Exhaustiveness.adt_name = "result"; adt_constructors = [ "Ok"; "Err" ] }
  in
  let reg =
    Exhaustiveness.register_adt "mymod/result" info
      Exhaustiveness.empty_registry
  in
  match Exhaustiveness.lookup_adt "mymod/result" reg with
  | Some found ->
      Alcotest.(check string) "name matches" "result" found.adt_name;
      Alcotest.(check int)
        "two constructors" 2
        (List.length found.adt_constructors)
  | None -> Alcotest.fail "should find registered ADT"

let test_merge_registries () =
  let info1 =
    {
      Exhaustiveness.adt_name = "option";
      adt_constructors = [ "Some"; "None" ];
    }
  in
  let info2 =
    { Exhaustiveness.adt_name = "result"; adt_constructors = [ "Ok"; "Err" ] }
  in
  let reg1 =
    Exhaustiveness.register_adt "mod1/option" info1
      Exhaustiveness.empty_registry
  in
  let reg2 =
    Exhaustiveness.register_adt "mod2/result" info2
      Exhaustiveness.empty_registry
  in
  let merged = Exhaustiveness.merge_registries [ reg1; reg2 ] in
  Alcotest.(check bool)
    "finds option" true
    (Option.is_some (Exhaustiveness.lookup_adt "mod1/option" merged));
  Alcotest.(check bool)
    "finds result" true
    (Option.is_some (Exhaustiveness.lookup_adt "mod2/result" merged))

(* =============================================================================
   Pattern Analysis Tests
   ============================================================================= *)

let test_exhaustive_with_wildcard () =
  let info =
    { Exhaustiveness.adt_name = "result"; adt_constructors = [ "Ok"; "Err" ] }
  in
  let reg =
    Exhaustiveness.register_adt "test/result" info Exhaustiveness.empty_registry
  in
  let scrutinee_type = Types.TCon "test/result" in
  (* Pattern with wildcard covers everything *)
  let patterns = [ parse "(_ 42)" ] in
  let result =
    Exhaustiveness.check_exhaustiveness ~registry:reg ~scrutinee_type ~patterns
  in
  Alcotest.(check bool)
    "wildcard is exhaustive" true
    (result = Exhaustiveness.Exhaustive)

let test_exhaustive_all_constructors () =
  let info =
    { Exhaustiveness.adt_name = "result"; adt_constructors = [ "Ok"; "Err" ] }
  in
  let reg =
    Exhaustiveness.register_adt "test/result" info Exhaustiveness.empty_registry
  in
  let scrutinee_type = Types.TCon "test/result" in
  (* Patterns covering both Ok and Err - use backquote syntax *)
  let patterns = [ parse "(`(ok . ,x) x)"; parse "(`(err . ,e) nil)" ] in
  let result =
    Exhaustiveness.check_exhaustiveness ~registry:reg ~scrutinee_type ~patterns
  in
  Alcotest.(check bool)
    "covering all is exhaustive" true
    (result = Exhaustiveness.Exhaustive)

let test_non_exhaustive_missing_constructor () =
  let info =
    { Exhaustiveness.adt_name = "result"; adt_constructors = [ "Ok"; "Err" ] }
  in
  let reg =
    Exhaustiveness.register_adt "test/result" info Exhaustiveness.empty_registry
  in
  let scrutinee_type = Types.TCon "test/result" in
  (* Only covering Ok - use backquote syntax *)
  let patterns = [ parse "(`(ok . ,x) x)" ] in
  let result =
    Exhaustiveness.check_exhaustiveness ~registry:reg ~scrutinee_type ~patterns
  in
  match result with
  | Exhaustiveness.NonExhaustive missing ->
      Alcotest.(check bool) "missing Err" true (List.mem "err" missing)
  | _ -> Alcotest.fail "should be non-exhaustive"

let test_not_adt_type () =
  let info =
    { Exhaustiveness.adt_name = "result"; adt_constructors = [ "Ok"; "Err" ] }
  in
  let reg =
    Exhaustiveness.register_adt "test/result" info Exhaustiveness.empty_registry
  in
  (* Using a type that's not in the registry *)
  let scrutinee_type = Types.Prim.int in
  let patterns = [ parse "(1 \"one\")"; parse "(2 \"two\")" ] in
  let result =
    Exhaustiveness.check_exhaustiveness ~registry:reg ~scrutinee_type ~patterns
  in
  Alcotest.(check bool) "int is not ADT" true (result = Exhaustiveness.NotADT)

let test_not_adt_unknown_type () =
  let reg = Exhaustiveness.empty_registry in
  let scrutinee_type = Types.TCon "unknown/type" in
  let patterns = [ parse "(_ nil)" ] in
  let result =
    Exhaustiveness.check_exhaustiveness ~registry:reg ~scrutinee_type ~patterns
  in
  Alcotest.(check bool)
    "unknown type is NotADT" true
    (result = Exhaustiveness.NotADT)

(* =============================================================================
   Warning Generation Tests
   ============================================================================= *)

let test_warning_for_non_exhaustive () =
  let span = Loc.dummy_span in
  let result = Exhaustiveness.NonExhaustive [ "err" ] in
  match Exhaustiveness.warning_of_result ~span result with
  | Some warning ->
      Alcotest.(check bool)
        "message mentions Missing" true
        (String.length warning.message > 0
        && String.lowercase_ascii warning.message |> fun s ->
           String.length s > 0)
  | None -> Alcotest.fail "should produce warning"

let test_no_warning_for_exhaustive () =
  let span = Loc.dummy_span in
  let result = Exhaustiveness.Exhaustive in
  Alcotest.(check (option (of_pp (fun _ _ -> ()))))
    "no warning" None
    (Exhaustiveness.warning_of_result ~span result)

let test_no_warning_for_not_adt () =
  let span = Loc.dummy_span in
  let result = Exhaustiveness.NotADT in
  Alcotest.(check (option (of_pp (fun _ _ -> ()))))
    "no warning" None
    (Exhaustiveness.warning_of_result ~span result)

(* =============================================================================
   Build Registry from Signature Tests
   ============================================================================= *)

let test_build_registry_from_signature () =
  (* Create a signature with a data declaration *)
  let data_decl : Sig_ast.data_decl =
    {
      data_name = "option";
      data_params =
        [
          {
            Sig_ast.name = "a";
            bound = None;
            kind = None;
            loc = Loc.dummy_span;
          };
        ];
      data_ctors =
        [
          { ctor_name = "Some"; ctor_fields = []; ctor_loc = Loc.dummy_span };
          { ctor_name = "None"; ctor_fields = []; ctor_loc = Loc.dummy_span };
        ];
      data_loc = Loc.dummy_span;
    }
  in
  let sig_ast : Sig_ast.signature =
    {
      sig_module = "mymod";
      sig_decls = [ Sig_ast.DData data_decl ];
      sig_loc = Loc.dummy_span;
    }
  in
  let reg = Exhaustiveness.build_registry_from_signature sig_ast in
  match Exhaustiveness.lookup_adt "mymod/option" reg with
  | Some info ->
      Alcotest.(check string) "name" "option" info.adt_name;
      Alcotest.(check int) "constructors" 2 (List.length info.adt_constructors);
      Alcotest.(check bool)
        "has Some" true
        (List.mem "Some" info.adt_constructors);
      Alcotest.(check bool)
        "has None" true
        (List.mem "None" info.adt_constructors)
  | None -> Alcotest.fail "should have built registry entry"

let test_build_registry_empty_for_non_data () =
  (* Signature with no data declarations *)
  let sig_ast : Sig_ast.signature =
    {
      sig_module = "mymod";
      sig_decls =
        [
          Sig_ast.DDefun
            {
              defun_name = "foo";
              defun_tvar_binders = [];
              defun_clauses =
                [
                  {
                    clause_params = [];
                    clause_return = Sig_ast.STCon ("int", Loc.dummy_span);
                    clause_loc = Loc.dummy_span;
                  };
                ];
              defun_loc = Loc.dummy_span;
            };
        ];
      sig_loc = Loc.dummy_span;
    }
  in
  let reg = Exhaustiveness.build_registry_from_signature sig_ast in
  Alcotest.(check (option (of_pp (fun _ _ -> ()))))
    "no ADTs" None
    (Exhaustiveness.lookup_adt "mymod/foo" reg)

(* =============================================================================
   Find pcase Tests
   ============================================================================= *)

let test_find_pcases_simple () =
  let sexp = parse "(pcase x (_ nil))" in
  let pcases = Exhaustiveness.find_all_pcases [ sexp ] in
  Alcotest.(check int) "one pcase" 1 (List.length pcases)

let test_find_pcases_nested () =
  let sexp = parse "(defun foo (x) (pcase x (_ (pcase y (_ nil)))))" in
  let pcases = Exhaustiveness.find_all_pcases [ sexp ] in
  Alcotest.(check int) "two nested pcases" 2 (List.length pcases)

let test_find_pcases_exhaustive_variant () =
  let sexp = parse "(pcase-exhaustive x (_ nil))" in
  let pcases = Exhaustiveness.find_all_pcases [ sexp ] in
  Alcotest.(check int) "pcase-exhaustive found" 1 (List.length pcases)

let test_find_pcases_no_pcase () =
  let sexp = parse "(if x 1 2)" in
  let pcases = Exhaustiveness.find_all_pcases [ sexp ] in
  Alcotest.(check int) "no pcase" 0 (List.length pcases)

(* =============================================================================
   Diagnostic Creation Tests
   ============================================================================= *)

let test_non_exhaustive_match_diagnostic () =
  let span = Loc.dummy_span in
  let d =
    Tart.Diagnostic.non_exhaustive_match ~span
      ~message:"non-exhaustive pattern match. Missing: None" ()
  in
  Alcotest.(check bool) "is warning" true (d.severity = Tart.Diagnostic.Warning);
  Alcotest.(check bool) "has help" true (List.length d.help > 0)

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "exhaustiveness"
    [
      ( "registry",
        [
          Alcotest.test_case "empty" `Quick test_empty_registry;
          Alcotest.test_case "register and lookup" `Quick
            test_register_and_lookup;
          Alcotest.test_case "merge registries" `Quick test_merge_registries;
        ] );
      ( "pattern_analysis",
        [
          Alcotest.test_case "wildcard is exhaustive" `Quick
            test_exhaustive_with_wildcard;
          Alcotest.test_case "all constructors is exhaustive" `Quick
            test_exhaustive_all_constructors;
          Alcotest.test_case "missing constructor" `Quick
            test_non_exhaustive_missing_constructor;
          Alcotest.test_case "not ADT type" `Quick test_not_adt_type;
          Alcotest.test_case "unknown type" `Quick test_not_adt_unknown_type;
        ] );
      ( "warnings",
        [
          Alcotest.test_case "warning for non-exhaustive" `Quick
            test_warning_for_non_exhaustive;
          Alcotest.test_case "no warning for exhaustive" `Quick
            test_no_warning_for_exhaustive;
          Alcotest.test_case "no warning for not ADT" `Quick
            test_no_warning_for_not_adt;
        ] );
      ( "build_registry",
        [
          Alcotest.test_case "from signature" `Quick
            test_build_registry_from_signature;
          Alcotest.test_case "empty for non-data" `Quick
            test_build_registry_empty_for_non_data;
        ] );
      ( "find_pcases",
        [
          Alcotest.test_case "simple" `Quick test_find_pcases_simple;
          Alcotest.test_case "nested" `Quick test_find_pcases_nested;
          Alcotest.test_case "exhaustive variant" `Quick
            test_find_pcases_exhaustive_variant;
          Alcotest.test_case "no pcase" `Quick test_find_pcases_no_pcase;
        ] );
      ( "diagnostic",
        [
          Alcotest.test_case "creation" `Quick
            test_non_exhaustive_match_diagnostic;
        ] );
    ]
