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
  let err = Unify.ArityMismatch (2, 3, span, Constraint.NoContext) in
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
      Unify.ArityMismatch (1, 2, span, Constraint.NoContext);
    ]
  in
  let diagnostics = Diag.of_unify_errors errors in
  Alcotest.(check int) "two diagnostics" 2 (List.length diagnostics);
  Alcotest.(check int) "two errors" 2 (Diag.count_errors diagnostics)

let test_type_mismatch_with_function_context () =
  let span = Loc.dummy_span in
  let fn_type = Types.arrow [ Types.Prim.string ] Types.Prim.string in
  let context =
    Constraint.FunctionArg
      { fn_name = "upcase"; fn_type; arg_index = 0; arg_expr_source = None }
  in
  let err =
    Unify.TypeMismatch (Types.Prim.string, Types.Prim.int, span, context)
  in
  let d = Diag.of_unify_error err in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(check bool) "has related info" true (List.length d.Diag.related > 0);
  let related_msg = (List.hd d.Diag.related).message in
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
  let related_msg = (List.hd d.Diag.related).message in
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
  Alcotest.(check bool) "has related info" true (List.length d.Diag.related > 0);
  let related_msgs =
    List.map (fun (r : Diag.related_location) -> r.message) d.Diag.related
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
   Option/Nil Error Tests (R3)
   ============================================================================= *)

let test_option_nil_message () =
  (* When actual is Option String but expected is String, message should be "possible nil value" *)
  let span = Loc.dummy_span in
  let fn_type = Types.arrow [ Types.Prim.string ] Types.Prim.string in
  let context =
    Constraint.FunctionArg
      {
        fn_name = "upcase";
        fn_type;
        arg_index = 0;
        arg_expr_source = Some "get-name";
      }
  in
  let expected = Types.Prim.string in
  let actual = Types.option_of Types.Prim.string in
  let err = Unify.TypeMismatch (expected, actual, span, context) in
  let d = Diag.of_unify_error err in
  Alcotest.(check bool)
    "message is possible nil value" true
    (contains_pattern (Str.regexp_case_fold "possible nil") d.message)

let test_option_nil_may_return_note () =
  (* Should have a note like "`get-name` may return nil" *)
  let span = Loc.dummy_span in
  let fn_type = Types.arrow [ Types.Prim.string ] Types.Prim.string in
  let context =
    Constraint.FunctionArg
      {
        fn_name = "upcase";
        fn_type;
        arg_index = 0;
        arg_expr_source = Some "get-name";
      }
  in
  let expected = Types.Prim.string in
  let actual = Types.option_of Types.Prim.string in
  let err = Unify.TypeMismatch (expected, actual, span, context) in
  let d = Diag.of_unify_error err in
  let related_msgs =
    List.map (fun (r : Diag.related_location) -> r.message) d.Diag.related
  in
  let combined = String.concat " " related_msgs in
  Alcotest.(check bool)
    "has may return nil note" true
    (contains_pattern
       (Str.regexp_case_fold "get-name.*may return nil")
       combined)

let test_option_nil_help_suggestions () =
  (* Should have help suggestions for nil handling *)
  let span = Loc.dummy_span in
  let fn_type = Types.arrow [ Types.Prim.string ] Types.Prim.string in
  let context =
    Constraint.FunctionArg
      {
        fn_name = "upcase";
        fn_type;
        arg_index = 0;
        arg_expr_source = Some "get-name";
      }
  in
  let expected = Types.Prim.string in
  let actual = Types.option_of Types.Prim.string in
  let err = Unify.TypeMismatch (expected, actual, span, context) in
  let d = Diag.of_unify_error err in
  Alcotest.(check bool) "has help" true (List.length d.help > 0);
  let help_combined = String.concat " " d.help in
  (* Should suggest when-let or or *)
  Alcotest.(check bool)
    "suggests when-let" true
    (contains_pattern (Str.regexp_case_fold "when-let") help_combined);
  Alcotest.(check bool)
    "suggests or" true
    (contains_pattern (Str.regexp_case_fold "\\bor\\b") help_combined)

let test_option_nil_no_source_no_note () =
  (* When arg_expr_source is None, should not have "may return nil" note *)
  let span = Loc.dummy_span in
  let fn_type = Types.arrow [ Types.Prim.string ] Types.Prim.string in
  let context =
    Constraint.FunctionArg
      { fn_name = "upcase"; fn_type; arg_index = 0; arg_expr_source = None }
  in
  let expected = Types.Prim.string in
  let actual = Types.option_of Types.Prim.string in
  let err = Unify.TypeMismatch (expected, actual, span, context) in
  let d = Diag.of_unify_error err in
  let related_msgs =
    List.map (fun (r : Diag.related_location) -> r.message) d.Diag.related
  in
  let combined = String.concat " " related_msgs in
  (* Should NOT have "may return nil" note (only function arg note) *)
  Alcotest.(check bool)
    "no may return nil note" false
    (contains_pattern (Str.regexp_case_fold "may return nil") combined)

let test_end_to_end_option_nil_error () =
  (* Type-check (upcase (get-name x)) where get-name returns Option String *)
  let sexp = parse "(upcase (get-name x))" in
  let env =
    Tart.Type_env.extend_mono "upcase"
      (Types.arrow [ Types.Prim.string ] Types.Prim.string)
      (Tart.Type_env.extend_mono "get-name"
         (Types.arrow [ Types.Prim.any ] (Types.option_of Types.Prim.string))
         (Tart.Type_env.extend_mono "x" Types.Prim.any Tart.Type_env.empty))
  in
  let _, errors = Check.check_expr ~env sexp in
  Alcotest.(check bool) "has error" true (List.length errors > 0);
  let diagnostics = Diag.of_unify_errors errors in
  (* Find E0308 error with "possible nil value" message *)
  let nil_errors =
    List.filter
      (fun d ->
        contains_pattern (Str.regexp_case_fold "possible nil") d.Diag.message)
      diagnostics
  in
  Alcotest.(check bool) "has nil error" true (List.length nil_errors > 0);
  let d = List.hd nil_errors in
  let str = Diag.to_string d in
  (* Should mention the source function *)
  Alcotest.(check bool)
    "mentions get-name" true
    (contains_pattern (Str.regexp "get-name") str);
  (* Should suggest nil handling *)
  Alcotest.(check bool)
    "suggests fix" true
    (contains_pattern (Str.regexp_case_fold "when-let") str)

let test_option_nil_variable_source () =
  (* When the source is a variable (not a function call) *)
  let sexp = parse "(upcase maybe-name)" in
  let env =
    Tart.Type_env.extend_mono "upcase"
      (Types.arrow [ Types.Prim.string ] Types.Prim.string)
      (Tart.Type_env.extend_mono "maybe-name"
         (Types.option_of Types.Prim.string)
         Tart.Type_env.empty)
  in
  let _, errors = Check.check_expr ~env sexp in
  Alcotest.(check bool) "has error" true (List.length errors > 0);
  let diagnostics = Diag.of_unify_errors errors in
  let nil_errors =
    List.filter
      (fun d ->
        contains_pattern (Str.regexp_case_fold "possible nil") d.Diag.message)
      diagnostics
  in
  Alcotest.(check bool) "has nil error" true (List.length nil_errors > 0);
  let d = List.hd nil_errors in
  let str = Diag.to_string d in
  (* Should mention the variable name *)
  Alcotest.(check bool)
    "mentions maybe-name" true
    (contains_pattern (Str.regexp "maybe-name") str)

(* =============================================================================
   Undefined Variable Tests (R4)
   ============================================================================= *)

let test_undefined_variable_diagnostic () =
  let span = Loc.dummy_span in
  let d = Diag.undefined_variable ~span ~name:"foobar" ~candidates:[] () in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(
    check
      (option
         (of_pp (fun fmt c ->
              Format.fprintf fmt "%s" (Diag.error_code_to_string c)))))
    "code is E0425" (Some Diag.E0425) d.code;
  Alcotest.(check bool)
    "message mentions foobar" true
    (contains_pattern (Str.regexp "foobar") d.message)

let test_undefined_variable_with_suggestion () =
  let span = Loc.dummy_span in
  let d =
    Diag.undefined_variable ~span ~name:"foobar"
      ~candidates:[ "foobar1"; "foobaz"; "qux" ]
      ()
  in
  Alcotest.(check bool) "has help" true (List.length d.help > 0);
  let help_combined = String.concat " " d.help in
  Alcotest.(check bool)
    "suggests similar name" true
    (contains_pattern (Str.regexp "foobar1\\|foobaz") help_combined)

let test_undefined_variable_no_similar () =
  let span = Loc.dummy_span in
  let d =
    Diag.undefined_variable ~span ~name:"foobar"
      ~candidates:[ "completely"; "different"; "names" ]
      ()
  in
  Alcotest.(check bool) "no help when no similar" true (List.length d.help = 0)

let test_levenshtein_distance () =
  let module Lev = Tart.Levenshtein in
  (* Identical strings *)
  Alcotest.(check int) "identical" 0 (Lev.distance "hello" "hello");
  (* Empty strings *)
  Alcotest.(check int) "empty vs empty" 0 (Lev.distance "" "");
  Alcotest.(check int) "empty vs hello" 5 (Lev.distance "" "hello");
  Alcotest.(check int) "hello vs empty" 5 (Lev.distance "hello" "");
  (* Single character difference *)
  Alcotest.(check int) "substitution" 1 (Lev.distance "cat" "bat");
  Alcotest.(check int) "insertion" 1 (Lev.distance "cat" "cats");
  Alcotest.(check int) "deletion" 1 (Lev.distance "cats" "cat");
  (* Multiple differences *)
  Alcotest.(check int) "kitten vs sitting" 3 (Lev.distance "kitten" "sitting")

let test_find_similar_names () =
  let module Lev = Tart.Levenshtein in
  let candidates = [ "length"; "concat"; "car"; "cdr"; "cons"; "cadr" ] in
  (* Exact match should not be returned *)
  Alcotest.(check (list string))
    "no exact match" []
    (Lev.find_similar_names ~query:"length" ~candidates);
  (* Single typo should be found *)
  let result = Lev.find_similar_names ~query:"caar" ~candidates in
  Alcotest.(check bool) "finds car" true (List.mem "car" result);
  Alcotest.(check bool) "finds cadr" true (List.mem "cadr" result);
  (* Too different should not be found *)
  Alcotest.(check (list string))
    "nothing similar to xyz" []
    (Lev.find_similar_names ~query:"xyz" ~candidates)

let test_suggest_name () =
  let module Lev = Tart.Levenshtein in
  let candidates = [ "length"; "concat"; "car"; "cdr"; "cons" ] in
  Alcotest.(check (option string))
    "suggests car for caar" (Some "car")
    (Lev.suggest_name ~query:"caar" ~candidates);
  Alcotest.(check (option string))
    "nothing for xyz" None
    (Lev.suggest_name ~query:"xyz" ~candidates)

let test_end_to_end_undefined_variable () =
  (* Type-check (+ unknwon 1) where unknwon is not defined *)
  let sexp = parse "(+ unknwon 1)" in
  let result = Check.check_program [ sexp ] in
  Alcotest.(check bool) "has undefined" true (List.length result.undefineds > 0);
  let undef = List.hd result.undefineds in
  Alcotest.(check string) "name is unknwon" "unknwon" undef.name

let test_end_to_end_undefined_with_suggestion () =
  (* Type-check (+ lengtth xs) where lengtth is a typo for length *)
  let sexp = parse "(lengtth xs)" in
  let env =
    Tart.Type_env.extend_mono "length"
      (Types.arrow [ Types.Prim.any ] Types.Prim.int)
      (Tart.Type_env.extend_mono "xs" Types.Prim.any Tart.Type_env.empty)
  in
  let result = Check.check_program ~env [ sexp ] in
  Alcotest.(check bool) "has undefined" true (List.length result.undefineds > 0);
  let undef = List.hd result.undefineds in
  Alcotest.(check string) "name is lengtth" "lengtth" undef.name;
  (* Get candidates from environment *)
  let candidates = Tart.Type_env.names result.env in
  let d =
    Diag.undefined_variable ~span:undef.span ~name:undef.name ~candidates ()
  in
  Alcotest.(check bool) "has help" true (List.length d.help > 0);
  let help_combined = String.concat " " d.help in
  Alcotest.(check bool)
    "suggests length" true
    (contains_pattern (Str.regexp "length") help_combined)

(* =============================================================================
   Arity Mismatch Tests (R5)
   ============================================================================= *)

let test_arity_mismatch_with_context () =
  let span = Loc.dummy_span in
  let fn_type =
    Types.TArrow
      ( [ Types.PPositional Types.Prim.string; Types.PPositional Types.Prim.int ],
        Types.Prim.string )
  in
  let context =
    Constraint.FunctionArg
      { fn_name = "substring"; fn_type; arg_index = 0; arg_expr_source = None }
  in
  let err = Unify.ArityMismatch (2, 1, span, context) in
  let d = Diag.of_unify_error err in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(
    check
      (option
         (of_pp (fun fmt c ->
              Format.fprintf fmt "%s" (Diag.error_code_to_string c)))))
    "code is E0061" (Some Diag.E0061) d.code;
  Alcotest.(check bool) "has related info" true (List.length d.Diag.related > 0)

let test_arity_mismatch_shows_function_name () =
  let span = Loc.dummy_span in
  let fn_type =
    Types.TArrow
      ( [ Types.PPositional Types.Prim.string; Types.PPositional Types.Prim.int ],
        Types.Prim.string )
  in
  let context =
    Constraint.FunctionArg
      { fn_name = "substring"; fn_type; arg_index = 0; arg_expr_source = None }
  in
  let err = Unify.ArityMismatch (2, 1, span, context) in
  let d = Diag.of_unify_error err in
  let related_msg = (List.hd d.Diag.related).message in
  Alcotest.(check bool)
    "mentions function name" true
    (contains_pattern (Str.regexp "substring") related_msg)

let test_arity_mismatch_shows_signature () =
  let span = Loc.dummy_span in
  let fn_type =
    Types.TArrow
      ( [ Types.PPositional Types.Prim.string; Types.PPositional Types.Prim.int ],
        Types.Prim.string )
  in
  let context =
    Constraint.FunctionArg
      { fn_name = "substring"; fn_type; arg_index = 0; arg_expr_source = None }
  in
  let err = Unify.ArityMismatch (2, 1, span, context) in
  let d = Diag.of_unify_error err in
  let str = Diag.to_string d in
  (* Should show signature with types *)
  Alcotest.(check bool)
    "shows String type" true
    (contains_pattern (Str.regexp "String") str);
  Alcotest.(check bool)
    "shows Int type" true
    (contains_pattern (Str.regexp "Int") str)

let test_arity_mismatch_optional_range () =
  let span = Loc.dummy_span in
  (* Function with 2 required + 1 optional param *)
  let fn_type =
    Types.TArrow
      ( [
          Types.PPositional Types.Prim.string;
          Types.PPositional Types.Prim.int;
          Types.POptional Types.Prim.int;
        ],
        Types.Prim.string )
  in
  let context =
    Constraint.FunctionArg
      { fn_name = "substring"; fn_type; arg_index = 0; arg_expr_source = None }
  in
  let err = Unify.ArityMismatch (2, 1, span, context) in
  let d = Diag.of_unify_error err in
  Alcotest.(check bool)
    "message shows range" true
    (contains_pattern (Str.regexp "2-3 arguments") d.message)

let test_arity_mismatch_rest_args () =
  let span = Loc.dummy_span in
  (* Function with 1 required + rest params *)
  let fn_type =
    Types.TArrow
      ( [ Types.PPositional Types.Prim.string; Types.PRest Types.Prim.any ],
        Types.Prim.string )
  in
  let context =
    Constraint.FunctionArg
      { fn_name = "concat"; fn_type; arg_index = 0; arg_expr_source = None }
  in
  let err = Unify.ArityMismatch (1, 0, span, context) in
  let d = Diag.of_unify_error err in
  Alcotest.(check bool)
    "message shows rest indicator" true
    (contains_pattern (Str.regexp "1\\+ arguments") d.message)

let test_arity_mismatch_no_context () =
  let span = Loc.dummy_span in
  let err = Unify.ArityMismatch (2, 1, span, Constraint.NoContext) in
  let d = Diag.of_unify_error err in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(check bool) "no related info" true (List.length d.Diag.related = 0)

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
   Signature Mismatch Tests (R6)
   ============================================================================= *)

let test_signature_mismatch_diagnostic () =
  let impl_pos = Loc.make_pos ~file:"lib.el" ~line:10 ~col:0 ~offset:100 in
  let impl_span = Loc.make_span ~start_pos:impl_pos ~end_pos:impl_pos in
  let sig_pos = Loc.make_pos ~file:"lib.tart" ~line:5 ~col:0 ~offset:50 in
  let sig_span = Loc.make_span ~start_pos:sig_pos ~end_pos:sig_pos in
  let sig_type = Types.arrow [ Types.Prim.int ] Types.Prim.string in
  let impl_type = Types.arrow [ Types.Prim.int ] Types.Prim.int in
  let d =
    Diag.signature_mismatch ~name:"foo" ~impl_span ~impl_type ~sig_span
      ~sig_type ()
  in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(
    check
      (option
         (of_pp (fun fmt c ->
              Format.fprintf fmt "%s" (Diag.error_code_to_string c)))))
    "code is E0308" (Some Diag.E0308) d.code

let test_signature_mismatch_message () =
  let impl_span = Loc.dummy_span in
  let sig_span = Loc.dummy_span in
  let sig_type = Types.arrow [ Types.Prim.int ] Types.Prim.string in
  let impl_type = Types.arrow [ Types.Prim.int ] Types.Prim.int in
  let d =
    Diag.signature_mismatch ~name:"foo" ~impl_span ~impl_type ~sig_span
      ~sig_type ()
  in
  Alcotest.(check bool)
    "message mentions implementation" true
    (contains_pattern (Str.regexp_case_fold "implementation") d.message);
  Alcotest.(check bool)
    "message mentions function name" true
    (contains_pattern (Str.regexp "foo") d.message)

let test_signature_mismatch_has_related () =
  let impl_span = Loc.dummy_span in
  let sig_pos = Loc.make_pos ~file:"lib.tart" ~line:5 ~col:0 ~offset:50 in
  let sig_span = Loc.make_span ~start_pos:sig_pos ~end_pos:sig_pos in
  let sig_type = Types.arrow [ Types.Prim.int ] Types.Prim.string in
  let impl_type = Types.arrow [ Types.Prim.int ] Types.Prim.int in
  let d =
    Diag.signature_mismatch ~name:"foo" ~impl_span ~impl_type ~sig_span
      ~sig_type ()
  in
  Alcotest.(check bool) "has related info" true (List.length d.Diag.related > 0);
  let rel = List.hd d.Diag.related in
  Alcotest.(check bool)
    "related mentions signature declared" true
    (contains_pattern (Str.regexp_case_fold "signature declared") rel.message)

let test_signature_mismatch_shows_both_locations () =
  let impl_pos = Loc.make_pos ~file:"lib.el" ~line:10 ~col:0 ~offset:100 in
  let impl_span = Loc.make_span ~start_pos:impl_pos ~end_pos:impl_pos in
  let sig_pos = Loc.make_pos ~file:"lib.tart" ~line:5 ~col:0 ~offset:50 in
  let sig_span = Loc.make_span ~start_pos:sig_pos ~end_pos:sig_pos in
  let sig_type = Types.arrow [ Types.Prim.int ] Types.Prim.string in
  let impl_type = Types.arrow [ Types.Prim.int ] Types.Prim.int in
  let d =
    Diag.signature_mismatch ~name:"foo" ~impl_span ~impl_type ~sig_span
      ~sig_type ()
  in
  let str = Diag.to_string d in
  (* Primary location should be in lib.el *)
  Alcotest.(check bool)
    "shows lib.el" true
    (contains_pattern (Str.regexp "lib\\.el") str);
  (* Related location should be in lib.tart *)
  Alcotest.(check bool)
    "shows lib.tart" true
    (contains_pattern (Str.regexp "lib\\.tart") str)

let test_signature_mismatch_shows_types () =
  let impl_span = Loc.dummy_span in
  let sig_span = Loc.dummy_span in
  let sig_type = Types.arrow [ Types.Prim.int ] Types.Prim.string in
  let impl_type = Types.arrow [ Types.Prim.int ] Types.Prim.int in
  let d =
    Diag.signature_mismatch ~name:"foo" ~impl_span ~impl_type ~sig_span
      ~sig_type ()
  in
  let str = Diag.to_string d in
  Alcotest.(check bool)
    "shows expected String" true
    (contains_pattern (Str.regexp "String") str);
  Alcotest.(check bool)
    "shows expected and found" true
    (contains_pattern (Str.regexp "expected") str
    && contains_pattern (Str.regexp "found") str)

let test_signature_mismatch_spans () =
  let impl_pos = Loc.make_pos ~file:"lib.el" ~line:10 ~col:0 ~offset:100 in
  let impl_span = Loc.make_span ~start_pos:impl_pos ~end_pos:impl_pos in
  let sig_pos = Loc.make_pos ~file:"lib.tart" ~line:5 ~col:0 ~offset:50 in
  let sig_span = Loc.make_span ~start_pos:sig_pos ~end_pos:sig_pos in
  let sig_type = Types.arrow [ Types.Prim.int ] Types.Prim.string in
  let impl_type = Types.arrow [ Types.Prim.int ] Types.Prim.int in
  let d =
    Diag.signature_mismatch ~name:"foo" ~impl_span ~impl_type ~sig_span
      ~sig_type ()
  in
  (* Primary span should be implementation *)
  Alcotest.(check string) "primary span is impl" "lib.el" d.span.start_pos.file;
  (* all_spans should include both *)
  let spans = Diag.all_spans d in
  Alcotest.(check int) "two spans" 2 (List.length spans)

(* =============================================================================
   Kind Mismatch Tests (R6 from spec 17)
   ============================================================================= *)

module Kind = Tart.Kind
module Kind_infer = Tart.Kind_infer

let test_kind_mismatch_diagnostic_creation () =
  let span = Loc.dummy_span in
  let expected = Kind.(KStar @-> KStar) in
  let found = Kind.KStar in
  let d =
    Diag.kind_mismatch ~span ~expected ~found ~location:"type constructor f" ()
  in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(check string) "message is kind mismatch" "kind mismatch" d.message

let test_kind_mismatch_shows_expected_found () =
  let span = Loc.dummy_span in
  let expected = Kind.(KStar @-> KStar) in
  let found = Kind.KStar in
  let d =
    Diag.kind_mismatch ~span ~expected ~found ~location:"type application (f a)"
      ()
  in
  let str = Diag.to_string d in
  (* Verify expected and found kinds are shown *)
  Alcotest.(check bool)
    "shows expected kind" true
    (contains_pattern (Str.regexp "expected: \\* -> \\*") str);
  Alcotest.(check bool)
    "shows found kind" true
    (contains_pattern (Str.regexp "found: \\*") str)

let test_kind_mismatch_shows_location () =
  let span = Loc.dummy_span in
  let expected = Kind.(KStar @-> KStar) in
  let found = Kind.KStar in
  let d =
    Diag.kind_mismatch ~span ~expected ~found ~location:"type application (f a)"
      ()
  in
  let str = Diag.to_string d in
  Alcotest.(check bool)
    "shows location" true
    (contains_pattern (Str.regexp "in type application (f a)") str)

let test_kind_mismatch_has_error_code () =
  let span = Loc.dummy_span in
  let expected = Kind.(KStar @-> KStar) in
  let found = Kind.KStar in
  let d =
    Diag.kind_mismatch ~span ~expected ~found ~location:"type constructor f" ()
  in
  Alcotest.(check bool) "has error code" true (Option.is_some (Diag.code d));
  let code = Option.get (Diag.code d) in
  Alcotest.(check string)
    "code is E0509" "E0509"
    (Diag.error_code_to_string code)

let test_kind_mismatch_help_suggestions () =
  let span = Loc.dummy_span in
  (* When expected is arrow and found is star *)
  let d1 =
    Diag.kind_mismatch ~span
      ~expected:Kind.(KStar @-> KStar)
      ~found:Kind.KStar ~location:"f" ()
  in
  Alcotest.(check bool)
    "has help for arrow vs star" true
    (List.length (Diag.help d1) > 0);
  (* When expected is star and found is arrow *)
  let d2 =
    Diag.kind_mismatch ~span ~expected:Kind.KStar
      ~found:Kind.(KStar @-> KStar)
      ~location:"f" ()
  in
  Alcotest.(check bool)
    "has help for star vs arrow" true
    (List.length (Diag.help d2) > 0)

let test_of_kind_error () =
  let span = Loc.dummy_span in
  let err =
    Kind_infer.KindMismatch
      {
        expected = Kind.(KStar @-> KStar);
        found = Kind.KStar;
        location = "type constructor f";
      }
  in
  let d = Diag.of_kind_error span err in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(check string) "message is kind mismatch" "kind mismatch" d.message;
  let str = Diag.to_string d in
  Alcotest.(check bool)
    "shows expected" true
    (contains_pattern (Str.regexp "expected") str);
  Alcotest.(check bool)
    "shows found" true
    (contains_pattern (Str.regexp "found") str)

let test_kind_arity_mismatch_formatting () =
  let span = Loc.dummy_span in
  let err =
    Kind_infer.ArityMismatch { type_con = "list"; expected = 1; found = 2 }
  in
  let d = Diag.of_kind_error span err in
  Alcotest.(check bool) "is error" true (Diag.is_error d);
  Alcotest.(check bool)
    "mentions type constructor" true
    (contains_pattern (Str.regexp "list") d.message);
  Alcotest.(check bool)
    "mentions wrong number" true
    (contains_pattern (Str.regexp "wrong number") d.message);
  let str = Diag.to_string d in
  Alcotest.(check bool)
    "shows expected count" true
    (contains_pattern (Str.regexp "expected 1 argument") str);
  Alcotest.(check bool)
    "shows found count" true
    (contains_pattern (Str.regexp "found 2") str)

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
      ( "option_nil",
        [
          Alcotest.test_case "possible nil message" `Quick
            test_option_nil_message;
          Alcotest.test_case "may return nil note" `Quick
            test_option_nil_may_return_note;
          Alcotest.test_case "help suggestions" `Quick
            test_option_nil_help_suggestions;
          Alcotest.test_case "no source no note" `Quick
            test_option_nil_no_source_no_note;
          Alcotest.test_case "end-to-end function call" `Quick
            test_end_to_end_option_nil_error;
          Alcotest.test_case "variable source" `Quick
            test_option_nil_variable_source;
        ] );
      ( "undefined_variable",
        [
          Alcotest.test_case "diagnostic creation" `Quick
            test_undefined_variable_diagnostic;
          Alcotest.test_case "with suggestion" `Quick
            test_undefined_variable_with_suggestion;
          Alcotest.test_case "no similar names" `Quick
            test_undefined_variable_no_similar;
          Alcotest.test_case "levenshtein distance" `Quick
            test_levenshtein_distance;
          Alcotest.test_case "find similar names" `Quick test_find_similar_names;
          Alcotest.test_case "suggest name" `Quick test_suggest_name;
          Alcotest.test_case "end-to-end undefined" `Quick
            test_end_to_end_undefined_variable;
          Alcotest.test_case "end-to-end with suggestion" `Quick
            test_end_to_end_undefined_with_suggestion;
        ] );
      ( "arity_mismatch",
        [
          Alcotest.test_case "with context" `Quick
            test_arity_mismatch_with_context;
          Alcotest.test_case "shows function name" `Quick
            test_arity_mismatch_shows_function_name;
          Alcotest.test_case "shows signature" `Quick
            test_arity_mismatch_shows_signature;
          Alcotest.test_case "optional range" `Quick
            test_arity_mismatch_optional_range;
          Alcotest.test_case "rest args" `Quick test_arity_mismatch_rest_args;
          Alcotest.test_case "no context" `Quick test_arity_mismatch_no_context;
        ] );
      ( "signature_mismatch",
        [
          Alcotest.test_case "diagnostic creation" `Quick
            test_signature_mismatch_diagnostic;
          Alcotest.test_case "message content" `Quick
            test_signature_mismatch_message;
          Alcotest.test_case "has related info" `Quick
            test_signature_mismatch_has_related;
          Alcotest.test_case "shows both locations" `Quick
            test_signature_mismatch_shows_both_locations;
          Alcotest.test_case "shows types" `Quick
            test_signature_mismatch_shows_types;
          Alcotest.test_case "spans include both files" `Quick
            test_signature_mismatch_spans;
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
      ( "kind_mismatch",
        [
          Alcotest.test_case "diagnostic creation" `Quick
            test_kind_mismatch_diagnostic_creation;
          Alcotest.test_case "shows expected and found" `Quick
            test_kind_mismatch_shows_expected_found;
          Alcotest.test_case "shows location" `Quick
            test_kind_mismatch_shows_location;
          Alcotest.test_case "has error code" `Quick
            test_kind_mismatch_has_error_code;
          Alcotest.test_case "help for star vs arrow" `Quick
            test_kind_mismatch_help_suggestions;
          Alcotest.test_case "of_kind_error" `Quick test_of_kind_error;
          Alcotest.test_case "arity mismatch formatting" `Quick
            test_kind_arity_mismatch_formatting;
        ] );
    ]
