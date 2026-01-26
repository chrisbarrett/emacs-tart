(** Tests for type error diagnostics *)

module Types = Tart.Types
module Loc = Tart.Location
module Diag = Tart.Diagnostic
module Check = Tart.Check
module Unify = Tart.Unify
module Constraint = Tart.Constraint

(** Parse a string to S-expression for testing *)
let parse str =
  match Tart.Read.parse_one ~filename:"test.el" str with
  | Ok sexp -> sexp
  | Error msg -> failwith ("parse error: " ^ msg)

(** Check if a pattern exists anywhere in a string *)
let contains_pattern pattern str =
  try
    let _ = Str.search_forward pattern str 0 in
    true
  with Not_found -> false

(* =============================================================================
   Diagnostic Creation Tests
   ============================================================================= *)

let test_type_mismatch_diagnostic () =
  let span = Loc.dummy_span in
  let d =
    Diag.type_mismatch ~span ~expected:Types.Prim.int ~actual:Types.Prim.string
      ()
  in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(check bool) "has expected" true (Option.is_some d.expected);
  Alcotest.(check bool) "has actual" true (Option.is_some d.actual);
  Alcotest.(check string)
    "expected is Int" "Int"
    (Types.to_string (Option.get d.expected));
  Alcotest.(check string)
    "actual is String" "String"
    (Types.to_string (Option.get d.actual))

let test_arity_mismatch_diagnostic () =
  let span = Loc.dummy_span in
  let d = Diag.arity_mismatch ~span ~expected:2 ~actual:3 () in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(check bool)
    "message contains arity info" true
    (String.length d.message > 0);
  Alcotest.(check bool)
    "message mentions 2" true
    (contains_pattern (Str.regexp "2") d.message);
  Alcotest.(check bool)
    "message mentions 3" true
    (contains_pattern (Str.regexp "3") d.message)

let test_occurs_check_diagnostic () =
  let span = Loc.dummy_span in
  let d =
    Diag.occurs_check ~span ~tvar_id:42 ~typ:(Types.list_of Types.Prim.int) ()
  in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(check bool)
    "message mentions infinite" true
    (contains_pattern (Str.regexp_case_fold "infinite") d.message)

(* =============================================================================
   Diagnostic Formatting Tests
   ============================================================================= *)

let test_format_span_same_line () =
  let pos = Loc.make_pos ~file:"test.el" ~line:10 ~col:5 ~offset:100 in
  let end_pos = Loc.make_pos ~file:"test.el" ~line:10 ~col:15 ~offset:110 in
  let span = Loc.make_span ~start_pos:pos ~end_pos in
  let formatted = Diag.format_span span in
  Alcotest.(check bool)
    "contains file" true
    (contains_pattern (Str.regexp "test\\.el") formatted);
  Alcotest.(check bool)
    "contains line" true
    (contains_pattern (Str.regexp "10") formatted)

let test_format_span_multiple_lines () =
  let pos = Loc.make_pos ~file:"test.el" ~line:10 ~col:5 ~offset:100 in
  let end_pos = Loc.make_pos ~file:"test.el" ~line:12 ~col:3 ~offset:150 in
  let span = Loc.make_span ~start_pos:pos ~end_pos in
  let formatted = Diag.format_span span in
  Alcotest.(check bool)
    "contains start line" true
    (contains_pattern (Str.regexp "10") formatted);
  Alcotest.(check bool)
    "contains end line" true
    (contains_pattern (Str.regexp "12") formatted)

let test_to_string_includes_location () =
  let pos = Loc.make_pos ~file:"test.el" ~line:5 ~col:3 ~offset:50 in
  let span = Loc.make_span ~start_pos:pos ~end_pos:pos in
  let d =
    Diag.type_mismatch ~span ~expected:Types.Prim.int ~actual:Types.Prim.string
      ()
  in
  let str = Diag.to_string d in
  Alcotest.(check bool)
    "contains file" true
    (contains_pattern (Str.regexp "test\\.el") str);
  Alcotest.(check bool)
    "contains line 5" true
    (contains_pattern (Str.regexp ":5:") str)

let test_to_string_includes_expected_actual () =
  let span = Loc.dummy_span in
  let d =
    Diag.type_mismatch ~span ~expected:Types.Prim.int ~actual:Types.Prim.string
      ()
  in
  let str = Diag.to_string d in
  Alcotest.(check bool)
    "contains 'expected'" true
    (contains_pattern (Str.regexp_case_fold "expected") str);
  Alcotest.(check bool)
    "contains 'found'" true
    (contains_pattern (Str.regexp_case_fold "found") str);
  Alcotest.(check bool)
    "contains Int" true
    (contains_pattern (Str.regexp "Int") str);
  Alcotest.(check bool)
    "contains String" true
    (contains_pattern (Str.regexp "String") str)

let test_to_string_includes_related_locations () =
  let pos1 = Loc.make_pos ~file:"test.el" ~line:5 ~col:0 ~offset:50 in
  let span1 = Loc.make_span ~start_pos:pos1 ~end_pos:pos1 in
  let pos2 = Loc.make_pos ~file:"test.el" ~line:10 ~col:0 ~offset:100 in
  let span2 = Loc.make_span ~start_pos:pos2 ~end_pos:pos2 in
  let related =
    [ { Diag.span = span2; message = "expected type from here" } ]
  in
  let d =
    Diag.type_mismatch ~span:span1 ~expected:Types.Prim.int
      ~actual:Types.Prim.string ~related ()
  in
  let str = Diag.to_string d in
  Alcotest.(check bool)
    "contains 'note'" true
    (contains_pattern (Str.regexp_case_fold "note") str);
  Alcotest.(check bool)
    "contains related message" true
    (contains_pattern (Str.regexp_case_fold "expected type from here") str);
  Alcotest.(check bool)
    "contains related line" true
    (contains_pattern (Str.regexp ":10:") str)

let test_to_string_compact () =
  let pos = Loc.make_pos ~file:"test.el" ~line:5 ~col:3 ~offset:50 in
  let span = Loc.make_span ~start_pos:pos ~end_pos:pos in
  let d =
    Diag.type_mismatch ~span ~expected:Types.Prim.int ~actual:Types.Prim.string
      ()
  in
  let str = Diag.to_string_compact d in
  (* Should be a single line *)
  Alcotest.(check bool) "single line" true (not (String.contains str '\n'));
  Alcotest.(check bool)
    "contains location" true
    (contains_pattern (Str.regexp "test\\.el:5:") str);
  Alcotest.(check bool)
    "contains expected" true
    (contains_pattern (Str.regexp_case_fold "expected") str)

(* =============================================================================
   Conversion from Unify.error Tests
   ============================================================================= *)

let test_of_unify_type_mismatch () =
  let span = Loc.dummy_span in
  let err =
    Unify.TypeMismatch
      (Types.Prim.int, Types.Prim.string, span, Constraint.NoContext)
  in
  let d = Diag.of_unify_error err in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(check string)
    "expected is Int" "Int"
    (Types.to_string (Option.get d.expected));
  Alcotest.(check string)
    "actual is String" "String"
    (Types.to_string (Option.get d.actual))

let test_of_unify_occurs_check () =
  let span = Loc.dummy_span in
  let err = Unify.OccursCheck (42, Types.list_of Types.Prim.int, span) in
  let d = Diag.of_unify_error err in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(check bool)
    "message mentions infinite" true
    (contains_pattern (Str.regexp_case_fold "infinite") d.message)

let test_of_unify_arity_mismatch () =
  let span = Loc.dummy_span in
  let err = Unify.ArityMismatch (2, 3, span) in
  let d = Diag.of_unify_error err in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(check bool)
    "message contains arguments" true
    (contains_pattern (Str.regexp_case_fold "arguments") d.message)

let test_of_unify_errors_list () =
  let span = Loc.dummy_span in
  let errors =
    [
      Unify.TypeMismatch
        (Types.Prim.int, Types.Prim.string, span, Constraint.NoContext);
      Unify.ArityMismatch (1, 2, span);
    ]
  in
  let diagnostics = Diag.of_unify_errors errors in
  Alcotest.(check int) "two diagnostics" 2 (List.length diagnostics);
  Alcotest.(check int) "two errors" 2 (Diag.count_errors diagnostics)

let test_type_mismatch_with_function_context () =
  let span = Loc.dummy_span in
  let fn_type = Types.arrow [ Types.Prim.string ] Types.Prim.string in
  let context =
    Constraint.FunctionArg { fn_name = "upcase"; fn_type; arg_index = 0 }
  in
  let err =
    Unify.TypeMismatch (Types.Prim.string, Types.Prim.int, span, context)
  in
  let d = Diag.of_unify_error err in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(check bool) "has related info" true (List.length d.related > 0);
  let related_msg = (List.hd d.related).message in
  Alcotest.(check bool)
    "related mentions function name" true
    (contains_pattern (Str.regexp "upcase") related_msg);
  Alcotest.(check bool)
    "related mentions expected type" true
    (contains_pattern (Str.regexp "String") related_msg)

let test_end_to_end_function_arg_error () =
  (* Type-check (upcase count) where count is Int - should get context *)
  let sexp = parse "(upcase count)" in
  let env =
    Tart.Type_env.extend_mono "upcase"
      (Types.arrow [ Types.Prim.string ] Types.Prim.string)
      (Tart.Type_env.extend_mono "count" Types.Prim.int Tart.Type_env.empty)
  in
  let _, errors = Check.check_expr ~env sexp in
  Alcotest.(check bool) "has error" true (List.length errors > 0);
  let diagnostics = Diag.of_unify_errors errors in
  (* Check first - find an error with context (some errors may not have it) *)
  let with_related =
    List.filter (fun d -> List.length d.Diag.related > 0) diagnostics
  in
  Alcotest.(check bool)
    "has diagnostic with related info" true
    (List.length with_related > 0);
  let d = List.hd with_related in
  let related_msg = (List.hd d.related).message in
  Alcotest.(check bool)
    "mentions upcase" true
    (contains_pattern (Str.regexp "upcase") related_msg)

(* =============================================================================
   Branch Mismatch Tests (R2)
   ============================================================================= *)

let test_branch_mismatch_diagnostic_creation () =
  let span1 = Loc.dummy_span in
  let span2 = Loc.dummy_span in
  let d =
    Diag.branch_mismatch ~span:span1 ~this_type:Types.Prim.int
      ~other_branch_span:span2 ~other_type:Types.Prim.string ~is_then:false ()
  in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(
    check
      (option
         (of_pp (fun fmt c ->
              Format.fprintf fmt "%s" (Diag.error_code_to_string c)))))
    "code is E0317" (Some Diag.E0317) d.code;
  Alcotest.(check bool)
    "message mentions incompatible" true
    (contains_pattern (Str.regexp_case_fold "incompatible") d.message)

let test_branch_mismatch_has_related_info () =
  let span1 = Loc.dummy_span in
  let span2 = Loc.dummy_span in
  let d =
    Diag.branch_mismatch ~span:span1 ~this_type:Types.Prim.int
      ~other_branch_span:span2 ~other_type:Types.Prim.string ~is_then:true ()
  in
  Alcotest.(check bool) "has related info" true (List.length d.related > 0);
  let related_msgs =
    List.map (fun (r : Diag.related_location) -> r.message) d.related
  in
  let combined = String.concat " " related_msgs in
  Alcotest.(check bool)
    "mentions other branch" true
    (contains_pattern (Str.regexp_case_fold "else branch") combined);
  Alcotest.(check bool)
    "mentions must have same type" true
    (contains_pattern (Str.regexp_case_fold "same type") combined)

let test_branch_mismatch_with_if_context () =
  let span = Loc.dummy_span in
  let other_span = Loc.dummy_span in
  let context =
    Constraint.IfBranch
      {
        is_then = false;
        other_branch_span = other_span;
        other_branch_type = Types.Prim.int;
      }
  in
  let err =
    Unify.TypeMismatch (Types.Prim.int, Types.Prim.string, span, context)
  in
  let d = Diag.of_unify_error err in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(
    check
      (option
         (of_pp (fun fmt c ->
              Format.fprintf fmt "%s" (Diag.error_code_to_string c)))))
    "code is E0317" (Some Diag.E0317) d.code

let test_end_to_end_if_branch_mismatch () =
  (* Type-check (if t n "negative") - should get branch mismatch error *)
  let sexp = parse "(if t 42 \"negative\")" in
  let _, errors = Check.check_expr sexp in
  Alcotest.(check bool) "has error" true (List.length errors > 0);
  let diagnostics = Diag.of_unify_errors errors in
  (* Find the E0317 error *)
  let branch_errors =
    List.filter (fun d -> d.Diag.code = Some Diag.E0317) diagnostics
  in
  Alcotest.(check bool)
    "has branch mismatch error" true
    (List.length branch_errors > 0);
  let d = List.hd branch_errors in
  let str = Diag.to_string d in
  Alcotest.(check bool)
    "mentions incompatible types" true
    (contains_pattern (Str.regexp_case_fold "incompatible") str)

let test_if_branch_mismatch_shows_both_types () =
  let sexp = parse "(if t 42 \"negative\")" in
  let _, errors = Check.check_expr sexp in
  let diagnostics = Diag.of_unify_errors errors in
  let branch_errors =
    List.filter (fun d -> d.Diag.code = Some Diag.E0317) diagnostics
  in
  let d = List.hd branch_errors in
  let str = Diag.to_string d in
  (* Should show both Int and String types *)
  Alcotest.(check bool)
    "shows Int" true
    (contains_pattern (Str.regexp "Int") str);
  Alcotest.(check bool)
    "shows String" true
    (contains_pattern (Str.regexp "String") str)

let test_branch_mismatch_help_for_int_string () =
  let span = Loc.dummy_span in
  let d =
    Diag.branch_mismatch ~span ~this_type:Types.Prim.int ~other_branch_span:span
      ~other_type:Types.Prim.string ~is_then:false ()
  in
  Alcotest.(check bool) "has help" true (List.length d.help > 0);
  let help_combined = String.concat " " d.help in
  Alcotest.(check bool)
    "suggests conversion" true
    (contains_pattern (Str.regexp_case_fold "number-to-string") help_combined)

(* =============================================================================
   Integration Tests with Real Type Checking
   ============================================================================= *)

let test_check_expr_error_diagnostic () =
  let sexp = parse "(+ 1 \"hello\")" in
  let _, errors = Check.check_expr sexp in
  Alcotest.(check bool) "has error" true (List.length errors > 0);
  let diagnostics = Diag.of_unify_errors errors in
  Alcotest.(check bool) "has diagnostic" true (List.length diagnostics > 0);
  let d = List.hd diagnostics in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  let str = Diag.to_string d in
  Alcotest.(check bool) "non-empty message" true (String.length str > 0)

let test_diagnostic_has_source_location () =
  let sexp = parse "(+ 1 \"hello\")" in
  let _, errors = Check.check_expr sexp in
  let diagnostics = Diag.of_unify_errors errors in
  let d = List.hd diagnostics in
  (* The span should be from our test file *)
  let span = Diag.span d in
  Alcotest.(check string) "file is test.el" "test.el" span.start_pos.file

let test_all_spans () =
  let pos1 = Loc.make_pos ~file:"test.el" ~line:5 ~col:0 ~offset:50 in
  let span1 = Loc.make_span ~start_pos:pos1 ~end_pos:pos1 in
  let pos2 = Loc.make_pos ~file:"test.el" ~line:10 ~col:0 ~offset:100 in
  let span2 = Loc.make_span ~start_pos:pos2 ~end_pos:pos2 in
  let related = [ { Diag.span = span2; message = "note" } ] in
  let d =
    Diag.type_mismatch ~span:span1 ~expected:Types.Prim.int
      ~actual:Types.Prim.string ~related ()
  in
  let spans = Diag.all_spans d in
  Alcotest.(check int) "two spans" 2 (List.length spans)

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "diagnostic"
    [
      ( "creation",
        [
          Alcotest.test_case "type mismatch" `Quick
            test_type_mismatch_diagnostic;
          Alcotest.test_case "arity mismatch" `Quick
            test_arity_mismatch_diagnostic;
          Alcotest.test_case "occurs check" `Quick test_occurs_check_diagnostic;
        ] );
      ( "formatting",
        [
          Alcotest.test_case "format span same line" `Quick
            test_format_span_same_line;
          Alcotest.test_case "format span multi line" `Quick
            test_format_span_multiple_lines;
          Alcotest.test_case "includes location" `Quick
            test_to_string_includes_location;
          Alcotest.test_case "includes expected/actual" `Quick
            test_to_string_includes_expected_actual;
          Alcotest.test_case "includes related" `Quick
            test_to_string_includes_related_locations;
          Alcotest.test_case "compact format" `Quick test_to_string_compact;
        ] );
      ( "conversion",
        [
          Alcotest.test_case "of type mismatch" `Quick
            test_of_unify_type_mismatch;
          Alcotest.test_case "of occurs check" `Quick test_of_unify_occurs_check;
          Alcotest.test_case "of arity mismatch" `Quick
            test_of_unify_arity_mismatch;
          Alcotest.test_case "of errors list" `Quick test_of_unify_errors_list;
          Alcotest.test_case "function context" `Quick
            test_type_mismatch_with_function_context;
        ] );
      ( "branch_mismatch",
        [
          Alcotest.test_case "diagnostic creation" `Quick
            test_branch_mismatch_diagnostic_creation;
          Alcotest.test_case "has related info" `Quick
            test_branch_mismatch_has_related_info;
          Alcotest.test_case "with if context" `Quick
            test_branch_mismatch_with_if_context;
          Alcotest.test_case "end-to-end if mismatch" `Quick
            test_end_to_end_if_branch_mismatch;
          Alcotest.test_case "shows both types" `Quick
            test_if_branch_mismatch_shows_both_types;
          Alcotest.test_case "help for int/string" `Quick
            test_branch_mismatch_help_for_int_string;
        ] );
      ( "integration",
        [
          Alcotest.test_case "check expr error" `Quick
            test_check_expr_error_diagnostic;
          Alcotest.test_case "source location" `Quick
            test_diagnostic_has_source_location;
          Alcotest.test_case "all spans" `Quick test_all_spans;
          Alcotest.test_case "function arg error context" `Quick
            test_end_to_end_function_arg_error;
        ] );
    ]
