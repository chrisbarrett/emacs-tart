(** Unit tests for Code_action module. *)

open Lsp

(** {1 find_package_requires_version} *)

let test_find_version_basic () =
  let doc = ";; Package-Requires: ((emacs \"28.1\"))" in
  match Code_action.find_package_requires_version doc with
  | Some (line, col_start, col_end) ->
      Alcotest.(check int) "line" 0 line;
      let version = String.sub doc col_start (col_end - col_start) in
      Alcotest.(check string) "version" "28.1" version
  | None -> Alcotest.fail "expected Some"

let test_find_version_triple_semicolons () =
  let doc = ";;; Package-Requires: ((emacs \"29.1\"))" in
  match Code_action.find_package_requires_version doc with
  | Some (line, col_start, col_end) ->
      Alcotest.(check int) "line" 0 line;
      let version = String.sub doc col_start (col_end - col_start) in
      Alcotest.(check string) "version" "29.1" version
  | None -> Alcotest.fail "expected Some"

let test_find_version_with_other_deps () =
  let doc =
    ";; Package-Requires: ((emacs \"28.1\") (seq \"2.24\") (compat \"30\"))"
  in
  match Code_action.find_package_requires_version doc with
  | Some (_, col_start, col_end) ->
      let version = String.sub doc col_start (col_end - col_start) in
      Alcotest.(check string) "version" "28.1" version
  | None -> Alcotest.fail "expected Some"

let test_find_version_on_later_line () =
  let doc =
    ";;; test.el --- Test -*- lexical-binding: t -*-\n\
     ;; Package-Requires: ((emacs \"30.1\"))\n\
     ;;; Code:"
  in
  match Code_action.find_package_requires_version doc with
  | Some (line, _, _) -> Alcotest.(check int) "line" 1 line
  | None -> Alcotest.fail "expected Some"

let test_find_version_none_when_missing () =
  let doc = ";;; test.el --- Test\n;;; Code:\n(defun foo () nil)" in
  Alcotest.(check bool)
    "no version" true
    (Code_action.find_package_requires_version doc = None)

(** {1 generate_bump_version_action} *)

let test_bump_action_basic () =
  let doc =
    ";;; test.el --- Test -*- lexical-binding: t -*-\n\
     ;; Package-Requires: ((emacs \"28.1\"))\n\
     ;;; Code:"
  in
  let uri = "file:///tmp/test.el" in
  let diagnostic : Protocol.diagnostic =
    {
      range =
        {
          start = { line = 2; character = 0 };
          end_ = { line = 2; character = 10 };
        };
      severity = None;
      code = Some "E0900";
      message = "`json-parse-string` requires Emacs 29.1+";
      source = None;
      related_information = [];
    }
  in
  match
    Code_action.generate_bump_version_action ~uri ~doc_text:doc
      ~required_version:"29.1" ~diagnostic
  with
  | Some action -> (
      Alcotest.(check string)
        "title" "Bump minimum Emacs version to 29.1" action.ca_title;
      match action.ca_edit with
      | Some edit ->
          let doc_changes = edit.document_changes in
          Alcotest.(check int) "one doc change" 1 (List.length doc_changes);
          let doc_edit = List.hd doc_changes in
          Alcotest.(check string) "uri" uri doc_edit.tde_uri;
          let edits = doc_edit.edits in
          Alcotest.(check int) "one edit" 1 (List.length edits);
          let text_edit = List.hd edits in
          Alcotest.(check string) "new text" "29.1" text_edit.new_text;
          Alcotest.(check int) "edit line" 1 text_edit.te_range.start.line
      | None -> Alcotest.fail "expected edit")
  | None -> Alcotest.fail "expected Some action"

let test_bump_action_none_without_header () =
  let doc = "(defun foo () nil)" in
  let uri = "file:///tmp/test.el" in
  let diagnostic : Protocol.diagnostic =
    {
      range =
        {
          start = { line = 0; character = 0 };
          end_ = { line = 0; character = 10 };
        };
      severity = None;
      code = Some "E0900";
      message = "`foo` requires Emacs 29.1+";
      source = None;
      related_information = [];
    }
  in
  Alcotest.(check bool)
    "no action" true
    (Code_action.generate_bump_version_action ~uri ~doc_text:doc
       ~required_version:"29.1" ~diagnostic
    = None)

let test_bump_action_replaces_correct_range () =
  let doc = ";; Package-Requires: ((emacs \"28.1\") (seq \"2.24\"))" in
  let uri = "file:///tmp/test.el" in
  let diagnostic : Protocol.diagnostic =
    {
      range =
        {
          start = { line = 1; character = 0 };
          end_ = { line = 1; character = 10 };
        };
      severity = None;
      code = Some "E0900";
      message = "`fn` requires Emacs 31.0+";
      source = None;
      related_information = [];
    }
  in
  match
    Code_action.generate_bump_version_action ~uri ~doc_text:doc
      ~required_version:"31.0" ~diagnostic
  with
  | Some action -> (
      match action.ca_edit with
      | Some edit ->
          let text_edit = List.hd (List.hd edit.document_changes).edits in
          (* Verify the edit replaces only the version string *)
          let start_char = text_edit.te_range.start.character in
          let end_char = text_edit.te_range.end_.character in
          let old_text = String.sub doc start_char (end_char - start_char) in
          Alcotest.(check string) "replaces version" "28.1" old_text;
          Alcotest.(check string) "new version" "31.0" text_edit.new_text
      | None -> Alcotest.fail "expected edit")
  | None -> Alcotest.fail "expected Some action"

(** {1 extract_required_version / extract_removed_version /
    extract_function_name} *)

let test_extract_required_version () =
  Alcotest.(check (option string))
    "basic" (Some "29.1")
    (Code_action.extract_required_version
       "`json-parse-string` requires Emacs 29.1+")

let test_extract_required_version_none () =
  Alcotest.(check (option string))
    "no match" None
    (Code_action.extract_required_version "some other message")

let test_extract_removed_version () =
  Alcotest.(check (option string))
    "basic" (Some "30.1")
    (Code_action.extract_removed_version
       "`old-api` was removed after Emacs 30.1")

let test_extract_function_name () =
  Alcotest.(check (option string))
    "basic" (Some "json-parse-string")
    (Code_action.extract_function_name
       "`json-parse-string` requires Emacs 29.1+")

let test_extract_function_name_removed () =
  Alcotest.(check (option string))
    "removed" (Some "old-api")
    (Code_action.extract_function_name "`old-api` was removed after Emacs 30.1")

(** {1 Test runner} *)

let () =
  Alcotest.run "code_action"
    [
      ( "find-package-requires-version",
        [
          Alcotest.test_case "basic" `Quick test_find_version_basic;
          Alcotest.test_case "triple semicolons" `Quick
            test_find_version_triple_semicolons;
          Alcotest.test_case "with other deps" `Quick
            test_find_version_with_other_deps;
          Alcotest.test_case "on later line" `Quick
            test_find_version_on_later_line;
          Alcotest.test_case "none when missing" `Quick
            test_find_version_none_when_missing;
        ] );
      ( "bump-version-action",
        [
          Alcotest.test_case "basic" `Quick test_bump_action_basic;
          Alcotest.test_case "none without header" `Quick
            test_bump_action_none_without_header;
          Alcotest.test_case "replaces correct range" `Quick
            test_bump_action_replaces_correct_range;
        ] );
      ( "extract-version-helpers",
        [
          Alcotest.test_case "extract required version" `Quick
            test_extract_required_version;
          Alcotest.test_case "extract required version none" `Quick
            test_extract_required_version_none;
          Alcotest.test_case "extract removed version" `Quick
            test_extract_removed_version;
          Alcotest.test_case "extract function name" `Quick
            test_extract_function_name;
          Alcotest.test_case "extract function name removed" `Quick
            test_extract_function_name_removed;
        ] );
    ]
