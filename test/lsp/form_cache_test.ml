(** Tests for form-level caching (incremental type checking) *)

open Lsp

(** {1 Form ID Tests} *)

let test_form_id_defun () =
  let sexp =
    Syntax.Sexp.List
      ( [
          Syntax.Sexp.Symbol ("defun", Syntax.Location.dummy_span);
          Syntax.Sexp.Symbol ("foo", Syntax.Location.dummy_span);
          Syntax.Sexp.List ([], Syntax.Location.dummy_span);
          Syntax.Sexp.Symbol ("t", Syntax.Location.dummy_span);
        ],
        Syntax.Location.dummy_span )
  in
  let id = Form_cache.form_id_of_sexp sexp in
  Alcotest.(check (option string)) "defun name extracted" (Some "foo") id.name

let test_form_id_defvar () =
  let sexp =
    Syntax.Sexp.List
      ( [
          Syntax.Sexp.Symbol ("defvar", Syntax.Location.dummy_span);
          Syntax.Sexp.Symbol ("my-var", Syntax.Location.dummy_span);
          Syntax.Sexp.Int (42, Syntax.Location.dummy_span);
        ],
        Syntax.Location.dummy_span )
  in
  let id = Form_cache.form_id_of_sexp sexp in
  Alcotest.(check (option string))
    "defvar name extracted" (Some "my-var") id.name

let test_form_id_expression () =
  let sexp = Syntax.Sexp.Int (42, Syntax.Location.dummy_span) in
  let id = Form_cache.form_id_of_sexp sexp in
  Alcotest.(check (option string)) "expression has no name" None id.name

let test_form_id_hash_changes () =
  let sexp1 =
    Syntax.Sexp.List
      ( [
          Syntax.Sexp.Symbol ("defun", Syntax.Location.dummy_span);
          Syntax.Sexp.Symbol ("foo", Syntax.Location.dummy_span);
          Syntax.Sexp.List ([], Syntax.Location.dummy_span);
          Syntax.Sexp.Int (1, Syntax.Location.dummy_span);
        ],
        Syntax.Location.dummy_span )
  in
  let sexp2 =
    Syntax.Sexp.List
      ( [
          Syntax.Sexp.Symbol ("defun", Syntax.Location.dummy_span);
          Syntax.Sexp.Symbol ("foo", Syntax.Location.dummy_span);
          Syntax.Sexp.List ([], Syntax.Location.dummy_span);
          Syntax.Sexp.Int (2, Syntax.Location.dummy_span);
        ],
        Syntax.Location.dummy_span )
  in
  let id1 = Form_cache.form_id_of_sexp sexp1 in
  let id2 = Form_cache.form_id_of_sexp sexp2 in
  Alcotest.(check bool)
    "different content has different hash" true
    (id1.source_hash <> id2.source_hash)

(** {1 Cache Management Tests} *)

let test_cache_create () =
  let cache = Form_cache.create () in
  (* Should not raise *)
  Form_cache.remove_document cache "file:///nonexistent.el"

let test_cache_remove_document () =
  let cache = Form_cache.create () in
  let config = Typing.Module_check.default_config () in
  let sexps = [ Syntax.Sexp.Int (42, Syntax.Location.dummy_span) ] in
  (* First check - creates cache entry *)
  let _ =
    Form_cache.check_with_cache ~cache ~config ~filename:"/test.el"
      ~uri:"file:///test.el" ~sibling_sig_content:None sexps
  in
  (* Remove the document *)
  Form_cache.remove_document cache "file:///test.el";
  (* Second check should start fresh (no cached forms) *)
  let _, stats =
    Form_cache.check_with_cache ~cache ~config ~filename:"/test.el"
      ~uri:"file:///test.el" ~sibling_sig_content:None sexps
  in
  Alcotest.(check int) "no cached forms after remove" 0 stats.cached_forms

(** {1 Check With Cache Tests} *)

let test_check_stats_first_run () =
  let cache = Form_cache.create () in
  let config = Typing.Module_check.default_config () in
  let sexps =
    [
      Syntax.Sexp.List
        ( [
            Syntax.Sexp.Symbol ("defun", Syntax.Location.dummy_span);
            Syntax.Sexp.Symbol ("foo", Syntax.Location.dummy_span);
            Syntax.Sexp.List ([], Syntax.Location.dummy_span);
            Syntax.Sexp.Symbol ("t", Syntax.Location.dummy_span);
          ],
          Syntax.Location.dummy_span );
      Syntax.Sexp.List
        ( [
            Syntax.Sexp.Symbol ("defun", Syntax.Location.dummy_span);
            Syntax.Sexp.Symbol ("bar", Syntax.Location.dummy_span);
            Syntax.Sexp.List ([], Syntax.Location.dummy_span);
            Syntax.Sexp.Int (42, Syntax.Location.dummy_span);
          ],
          Syntax.Location.dummy_span );
    ]
  in
  let _, stats =
    Form_cache.check_with_cache ~cache ~config ~filename:"/test.el"
      ~uri:"file:///test.el" ~sibling_sig_content:None sexps
  in
  Alcotest.(check int) "total forms" 2 stats.total_forms;
  Alcotest.(check int) "cached forms (first run)" 0 stats.cached_forms;
  Alcotest.(check int) "checked forms" 2 stats.checked_forms

let test_check_stats_second_run () =
  let cache = Form_cache.create () in
  let config = Typing.Module_check.default_config () in
  let sexps =
    [
      Syntax.Sexp.List
        ( [
            Syntax.Sexp.Symbol ("defun", Syntax.Location.dummy_span);
            Syntax.Sexp.Symbol ("foo", Syntax.Location.dummy_span);
            Syntax.Sexp.List ([], Syntax.Location.dummy_span);
            Syntax.Sexp.Symbol ("t", Syntax.Location.dummy_span);
          ],
          Syntax.Location.dummy_span );
    ]
  in
  (* First run *)
  let _ =
    Form_cache.check_with_cache ~cache ~config ~filename:"/test.el"
      ~uri:"file:///test.el" ~sibling_sig_content:None sexps
  in
  (* Second run with same content *)
  let _, stats =
    Form_cache.check_with_cache ~cache ~config ~filename:"/test.el"
      ~uri:"file:///test.el" ~sibling_sig_content:None sexps
  in
  Alcotest.(check int) "total forms" 1 stats.total_forms;
  Alcotest.(check int) "cached forms (second run)" 1 stats.cached_forms;
  Alcotest.(check int) "checked forms" 0 stats.checked_forms

let test_check_stats_modified_form () =
  let cache = Form_cache.create () in
  let config = Typing.Module_check.default_config () in
  let sexps_v1 =
    [
      Syntax.Sexp.List
        ( [
            Syntax.Sexp.Symbol ("defun", Syntax.Location.dummy_span);
            Syntax.Sexp.Symbol ("foo", Syntax.Location.dummy_span);
            Syntax.Sexp.List ([], Syntax.Location.dummy_span);
            Syntax.Sexp.Int (1, Syntax.Location.dummy_span);
          ],
          Syntax.Location.dummy_span );
    ]
  in
  let sexps_v2 =
    [
      Syntax.Sexp.List
        ( [
            Syntax.Sexp.Symbol ("defun", Syntax.Location.dummy_span);
            Syntax.Sexp.Symbol ("foo", Syntax.Location.dummy_span);
            Syntax.Sexp.List ([], Syntax.Location.dummy_span);
            Syntax.Sexp.Int (2, Syntax.Location.dummy_span);
          ],
          Syntax.Location.dummy_span );
    ]
  in
  (* First run *)
  let _ =
    Form_cache.check_with_cache ~cache ~config ~filename:"/test.el"
      ~uri:"file:///test.el" ~sibling_sig_content:None sexps_v1
  in
  (* Second run with modified content *)
  let _, stats =
    Form_cache.check_with_cache ~cache ~config ~filename:"/test.el"
      ~uri:"file:///test.el" ~sibling_sig_content:None sexps_v2
  in
  Alcotest.(check int) "total forms" 1 stats.total_forms;
  Alcotest.(check int) "cached forms (modified)" 0 stats.cached_forms;
  Alcotest.(check int) "checked forms" 1 stats.checked_forms

let test_config_change_invalidates_cache () =
  let cache = Form_cache.create () in
  let config = Typing.Module_check.default_config () in
  let sexps =
    [
      Syntax.Sexp.List
        ( [
            Syntax.Sexp.Symbol ("defun", Syntax.Location.dummy_span);
            Syntax.Sexp.Symbol ("foo", Syntax.Location.dummy_span);
            Syntax.Sexp.List ([], Syntax.Location.dummy_span);
            Syntax.Sexp.Symbol ("t", Syntax.Location.dummy_span);
          ],
          Syntax.Location.dummy_span );
    ]
  in
  (* First run with no signature *)
  let _ =
    Form_cache.check_with_cache ~cache ~config ~filename:"/test.el"
      ~uri:"file:///test.el" ~sibling_sig_content:None sexps
  in
  (* Second run - signature content changed *)
  let _, stats =
    Form_cache.check_with_cache ~cache ~config ~filename:"/test.el"
      ~uri:"file:///test.el"
      ~sibling_sig_content:(Some "(defun foo () -> bool)") sexps
  in
  (* Cache should be invalidated due to config change *)
  Alcotest.(check int) "cached forms after config change" 0 stats.cached_forms

let test_check_returns_valid_result () =
  let cache = Form_cache.create () in
  let config = Typing.Module_check.default_config () in
  (* Use undefined-symbol to trigger an error without requiring c-core.
     Calling a symbol that doesn't exist produces an undefined variable error. *)
  let sexps =
    [
      Syntax.Sexp.List
        ( [
            Syntax.Sexp.Symbol
              ("undefined-function-xyz", Syntax.Location.dummy_span);
            Syntax.Sexp.Int (1, Syntax.Location.dummy_span);
          ],
          Syntax.Location.dummy_span );
    ]
  in
  let result, _ =
    Form_cache.check_with_cache ~cache ~config ~filename:"/test.el"
      ~uri:"file:///test.el" ~sibling_sig_content:None sexps
  in
  (* Should detect undefined function error (in undefined_errors, not type_errors) *)
  Alcotest.(check bool)
    "has undefined errors" true
    (result.undefined_errors <> [])

let () =
  Alcotest.run "form_cache"
    [
      ( "form-id",
        [
          Alcotest.test_case "defun" `Quick test_form_id_defun;
          Alcotest.test_case "defvar" `Quick test_form_id_defvar;
          Alcotest.test_case "expression" `Quick test_form_id_expression;
          Alcotest.test_case "hash changes" `Quick test_form_id_hash_changes;
        ] );
      ( "cache-management",
        [
          Alcotest.test_case "create" `Quick test_cache_create;
          Alcotest.test_case "remove document" `Quick test_cache_remove_document;
        ] );
      ( "check-with-cache",
        [
          Alcotest.test_case "first run" `Quick test_check_stats_first_run;
          Alcotest.test_case "second run" `Quick test_check_stats_second_run;
          Alcotest.test_case "modified form" `Quick
            test_check_stats_modified_form;
          Alcotest.test_case "config change invalidates" `Quick
            test_config_change_invalidates_cache;
          Alcotest.test_case "returns valid result" `Quick
            test_check_returns_valid_result;
        ] );
    ]
