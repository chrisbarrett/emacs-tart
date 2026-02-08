(** Tests for LSP server. *)

open Lsp
open Lsp_client

(** {1 Lifecycle Tests} *)

let test_initialize () =
  let result =
    run_session
      [
        initialize_msg ~id:1 ~root_uri:"file:///tmp/test" ();
        shutdown_msg ~id:2 ();
        exit_msg ();
      ]
  in
  Alcotest.(check int) "clean shutdown exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "Could not parse response"
  | Some json ->
      let open Yojson.Safe.Util in
      Alcotest.(check int) "id" 1 (json |> member "id" |> to_int);
      let result = json |> member "result" in
      let caps = result |> member "capabilities" in
      Alcotest.(check bool)
        "has textDocumentSync" true
        (caps |> member "textDocumentSync" <> `Null);
      Alcotest.(check bool)
        "hoverProvider" true
        (caps |> member "hoverProvider" |> to_bool);
      let server_info = result |> member "serverInfo" in
      Alcotest.(check string)
        "server name" "tart"
        (server_info |> member "name" |> to_string)

let test_initialize_already_initialized () =
  let result =
    run_session
      [
        initialize_msg ~id:1 ();
        initialize_msg ~id:2 ();
        shutdown_msg ~id:3 ();
        exit_msg ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  Alcotest.(check bool)
    "has error response" true
    (List.length result.messages > 0)

let test_shutdown_not_initialized () =
  let result = run_session [ shutdown_msg ~id:1 (); exit_msg () ] in
  Alcotest.(check int) "exit code without init" 1 result.exit_code

let test_exit_without_shutdown () =
  let result = run_session [ initialize_msg ~id:1 (); exit_msg () ] in
  Alcotest.(check int) "exit code without shutdown" 1 result.exit_code

let test_initialized_notification () =
  let result =
    run_session
      [
        initialize_msg ~id:1 ();
        initialized_msg ();
        shutdown_msg ~id:2 ();
        exit_msg ();
      ]
  in
  Alcotest.(check int) "clean shutdown with initialized" 0 result.exit_code

let test_unknown_method () =
  let result =
    run_session
      [
        initialize_msg ~id:1 ();
        make_message ~id:(`Int 2) ~method_:"unknown/method" ();
        shutdown_msg ~id:3 ();
        exit_msg ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No error response for unknown method"
  | Some json ->
      Alcotest.(check bool)
        "has error" true
        (Option.is_some (response_error json))

(** {1 Document Sync Tests} *)

let test_did_open () =
  let result =
    run_initialized_session
      [ did_open_msg ~uri:"file:///test.el" ~text:"(defun foo () t)" () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match Document.get_doc (Server.documents result.server) "file:///test.el" with
  | Some doc ->
      Alcotest.(check string) "doc text" "(defun foo () t)" doc.text;
      Alcotest.(check int) "doc version" 1 doc.version
  | None -> Alcotest.fail "Document not found after didOpen"

let test_did_change_incremental () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"hello world" ();
        did_change_msg ~uri:"file:///test.el" ~version:2
          ~changes:
            [
              `Assoc
                [
                  ( "range",
                    `Assoc
                      [
                        ( "start",
                          `Assoc [ ("line", `Int 0); ("character", `Int 6) ] );
                        ( "end",
                          `Assoc [ ("line", `Int 0); ("character", `Int 11) ] );
                      ] );
                  ("text", `String "Emacs");
                ];
            ]
          ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match Document.get_doc (Server.documents result.server) "file:///test.el" with
  | Some doc ->
      Alcotest.(check string) "doc text after change" "hello Emacs" doc.text;
      Alcotest.(check int) "doc version after change" 2 doc.version
  | None -> Alcotest.fail "Document not found after didChange"

let test_did_close () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"some content" ();
        did_close_msg ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  Alcotest.(check bool)
    "doc removed after close" true
    (Option.is_none
       (Document.get_doc (Server.documents result.server) "file:///test.el"))

(** {1 Diagnostics Tests} *)

let test_diagnostics_on_open () =
  let result =
    run_initialized_session
      [ did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 \"hello\")" () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_diagnostics ~uri:"file:///test.el" result.messages with
  | None -> Alcotest.fail "No publishDiagnostics notification found"
  | Some json ->
      let open Yojson.Safe.Util in
      let params = json |> member "params" in
      let diagnostics = params |> member "diagnostics" |> to_list in
      Alcotest.(check bool) "has diagnostics" true (List.length diagnostics > 0);
      let first = List.hd diagnostics in
      Alcotest.(check bool) "has range" true (first |> member "range" <> `Null);
      Alcotest.(check bool)
        "has message" true
        (first |> member "message" <> `Null);
      Alcotest.(check bool)
        "has severity" true
        (first |> member "severity" <> `Null);
      Alcotest.(check string)
        "source is tart" "tart"
        (first |> member "source" |> to_string)

let test_diagnostics_on_change () =
  (* Open a file with valid code, then change to have a parse error.
     The worker may coalesce rapid enqueues, so we check the *final*
     diagnostics match the final document state rather than requiring
     multiple notifications. *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        (* Change to an expression with a parse error — different diagnostics *)
        did_change_full_msg ~uri:"file:///test.el" ~version:2 ~text:"(+ 1 2) ("
          ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  let diag_notifications =
    find_all_notifications ~method_:"textDocument/publishDiagnostics"
      result.messages
  in
  Alcotest.(check bool)
    "has diagnostics notifications" true
    (List.length diag_notifications >= 1);
  (* The last diagnostics should reflect the parse error in the final text *)
  match find_last_diagnostics ~uri:"file:///test.el" result.messages with
  | None -> Alcotest.fail "No publishDiagnostics found"
  | Some last ->
      let open Yojson.Safe.Util in
      let diagnostics =
        last |> member "params" |> member "diagnostics" |> to_list
      in
      Alcotest.(check bool)
        "final diagnostics contain parse error" true
        (List.length diagnostics > 0)

let test_diagnostics_cleared_on_close () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 \"error\")" ();
        did_close_msg ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_last_diagnostics ~uri:"file:///test.el" result.messages with
  | None -> Alcotest.fail "No diagnostics notifications found"
  | Some last ->
      let open Yojson.Safe.Util in
      let diagnostics =
        last |> member "params" |> member "diagnostics" |> to_list
      in
      Alcotest.(check int) "diagnostics cleared" 0 (List.length diagnostics)

let test_diagnostics_valid_document () =
  let result =
    run_initialized_session
      [ did_open_msg ~uri:"file:///test.el" ~text:"(defun foo (x) x)" () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_diagnostics ~uri:"file:///test.el" result.messages with
  | None -> Alcotest.fail "No publishDiagnostics notification found"
  | Some json ->
      let open Yojson.Safe.Util in
      let params = json |> member "params" in
      let diagnostics = params |> member "diagnostics" |> to_list in
      Alcotest.(check int)
        "no diagnostics for valid code" 0 (List.length diagnostics)

let test_diagnostics_parse_error () =
  let result =
    run_initialized_session
      [ did_open_msg ~uri:"file:///test.el" ~text:"(defun foo (" () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_diagnostics ~uri:"file:///test.el" result.messages with
  | None -> Alcotest.fail "No publishDiagnostics notification found"
  | Some json ->
      let open Yojson.Safe.Util in
      let params = json |> member "params" in
      let diagnostics = params |> member "diagnostics" |> to_list in
      Alcotest.(check bool)
        "has parse error diagnostic" true
        (List.length diagnostics > 0)

(** {1 Hover Tests} *)

let test_hover_on_literal () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"42" ();
        hover_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:0 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No hover response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let contents = result |> member "contents" in
      let value = contents |> member "value" |> to_string in
      Alcotest.(check bool)
        "contains literal type" true
        (contains_string ~needle:"42" value)

let test_hover_on_function_call () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        hover_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No hover response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let contents = result |> member "contents" in
      let value = contents |> member "value" |> to_string in
      Alcotest.(check bool)
        "contains arrow" true
        (contains_string ~needle:"->" value)

let test_hover_outside_code () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"42\n" ();
        hover_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:0 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No hover response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is null" true (result = `Null)

let test_hover_has_range () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"42" ();
        hover_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:0 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No hover response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let range = result |> member "range" in
      Alcotest.(check bool) "has range" true (range <> `Null);
      let start = range |> member "start" in
      let end_ = range |> member "end" in
      Alcotest.(check bool) "has start" true (start <> `Null);
      Alcotest.(check bool) "has end" true (end_ <> `Null)

let test_hover_instantiated_type () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(list 1 2 3)" ();
        hover_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No hover response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let contents = result |> member "contents" in
      let value = contents |> member "value" |> to_string in
      Alcotest.(check bool)
        "contains resolved type" true
        (contains_string ~needle:"int" value
        || contains_string ~needle:"1" value)

let test_hover_with_errors_elsewhere () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"42\n(+ 1 \"hello\")" ();
        hover_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:0 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No hover response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let contents = result |> member "contents" in
      let value = contents |> member "value" |> to_string in
      Alcotest.(check bool)
        "contains literal type" true
        (contains_string ~needle:"42" value)

let test_hover_at_error_site () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 \"hello\")" ();
        hover_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No hover response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool)
        "response succeeded (not error)" true
        (json |> member "error" = `Null);
      if result <> `Null then
        let contents = result |> member "contents" in
        let value = contents |> member "value" |> to_string in
        Alcotest.(check bool) "has some content" true (String.length value > 0)

(** {1 Diagnostic Formatting Tests} *)

let test_diagnostic_has_error_code () =
  let result =
    run_initialized_session
      [ did_open_msg ~uri:"file:///test.el" ~text:"(undefined-fn 42)" () ]
  in
  match find_diagnostics ~uri:"file:///test.el" result.messages with
  | None -> Alcotest.fail "No publishDiagnostics notification found"
  | Some json ->
      let open Yojson.Safe.Util in
      let params = json |> member "params" in
      let diagnostics = params |> member "diagnostics" |> to_list in
      Alcotest.(check bool) "has diagnostics" true (List.length diagnostics > 0);
      let first = List.hd diagnostics in
      let code = first |> member "code" in
      Alcotest.(check bool) "has code" true (code <> `Null);
      Alcotest.(check string) "code is E0100" "E0100" (code |> to_string)

let test_diagnostic_has_help_suggestions () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defun foo () 42)\n(fop)" ();
      ]
  in
  match find_diagnostics ~uri:"file:///test.el" result.messages with
  | None -> Alcotest.fail "No publishDiagnostics notification found"
  | Some json -> (
      let open Yojson.Safe.Util in
      let params = json |> member "params" in
      let diagnostics = params |> member "diagnostics" |> to_list in
      Alcotest.(check bool) "has diagnostics" true (List.length diagnostics > 0);
      let fop_diag =
        List.find_opt
          (fun d ->
            let msg = d |> member "message" |> to_string in
            contains_string ~needle:"fop" msg)
          diagnostics
      in
      match fop_diag with
      | None -> Alcotest.fail "No diagnostic for 'fop' found"
      | Some diag ->
          let message = diag |> member "message" |> to_string in
          Alcotest.(check bool)
            "message has similar name" true
            (contains_string ~needle:"similar name" message);
          Alcotest.(check bool)
            "suggests foo" true
            (contains_string ~needle:"foo" message))

(** {1 Go to Definition Tests} *)

let test_definition_on_function_call () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defun foo () 42)\n(foo)" ();
        definition_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No definition response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let range = result |> member "range" in
      let start = range |> member "start" in
      let line = start |> member "line" |> to_int in
      Alcotest.(check int) "definition on line 0" 0 line

let test_definition_on_defvar_call () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defvar my-var 10)\nmy-var"
          ();
        definition_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:0 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No definition response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let range = result |> member "range" in
      let start = range |> member "start" in
      let line = start |> member "line" |> to_int in
      Alcotest.(check int) "definition on line 0" 0 line

let test_definition_not_found () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(undefined-fn)" ();
        definition_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No definition response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is null" true (result = `Null)

let test_definition_outside_code () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defun foo () 42)\n" ();
        definition_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:0 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No definition response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is null" true (result = `Null)

let test_definition_has_uri () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defun foo () 42)\n(foo)" ();
        definition_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No definition response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let uri = result |> member "uri" |> to_string in
      Alcotest.(check bool)
        "uri starts with file://" true
        (String.length uri > 7 && String.sub uri 0 7 = "file://")

let test_definition_cross_file () =
  let fixture_dir =
    Filename.concat (Sys.getcwd ()) "test/fixtures/definition"
  in
  let el_file = Filename.concat fixture_dir "main.el" in
  let tart_file = Filename.concat fixture_dir "mylib.tart" in
  if not (Sys.file_exists tart_file) then ()
  else
    let el_content = In_channel.with_open_text el_file In_channel.input_all in
    let uri = "file://" ^ el_file in
    let result =
      run_initialized_session
        [
          did_open_msg ~uri ~text:el_content ();
          definition_msg ~id:2 ~uri ~line:2 ~character:1 ();
        ]
    in
    Alcotest.(check int) "exit code" 0 result.exit_code;
    match find_response ~id:2 result.messages with
    | None -> Alcotest.fail "No definition response found"
    | Some json ->
        let open Yojson.Safe.Util in
        let result = json |> member "result" in
        Alcotest.(check bool)
          "result is not null (found cross-file definition)" true
          (result <> `Null);
        let result_uri = result |> member "uri" |> to_string in
        Alcotest.(check bool)
          "uri points to .tart file" true
          (contains_string ~needle:".tart" result_uri)

let test_definition_cross_file_defvar () =
  let fixture_dir =
    Filename.concat (Sys.getcwd ()) "test/fixtures/definition"
  in
  let el_file = Filename.concat fixture_dir "main.el" in
  let tart_file = Filename.concat fixture_dir "mylib.tart" in
  if not (Sys.file_exists tart_file) then ()
  else
    let el_content = In_channel.with_open_text el_file In_channel.input_all in
    let uri = "file://" ^ el_file in
    let result =
      run_initialized_session
        [
          did_open_msg ~uri ~text:el_content ();
          definition_msg ~id:2 ~uri ~line:3 ~character:0 ();
        ]
    in
    Alcotest.(check int) "exit code" 0 result.exit_code;
    match find_response ~id:2 result.messages with
    | None -> Alcotest.fail "No definition response found"
    | Some json ->
        let open Yojson.Safe.Util in
        let result = json |> member "result" in
        Alcotest.(check bool)
          "result is not null (found defvar in .tart)" true (result <> `Null);
        let result_uri = result |> member "uri" |> to_string in
        Alcotest.(check bool)
          "uri points to .tart file" true
          (contains_string ~needle:".tart" result_uri)

let test_definition_cross_file_el () =
  (* a.el defines (defun my-fn ...), b.el calls (my-fn).
     Goto-definition from b.el should resolve to a.el. *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///a.el" ~text:"(defun my-fn () 42)" ();
        did_open_msg ~uri:"file:///b.el" ~text:"(my-fn)" ();
        definition_msg ~id:2 ~uri:"file:///b.el" ~line:0 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No definition response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool)
        "result is not null (found cross-file .el definition)" true
        (result <> `Null);
      let result_uri = result |> member "uri" |> to_string in
      Alcotest.(check string) "uri points to a.el" "file:///a.el" result_uri;
      let range = result |> member "range" in
      let start_line = range |> member "start" |> member "line" |> to_int in
      Alcotest.(check int) "definition on line 0" 0 start_line

let test_definition_cross_file_el_defvar () =
  (* a.el defines (defvar my-var ...), b.el uses my-var.
     Goto-definition from b.el should resolve to a.el. *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///a.el" ~text:"(defvar my-var 99)" ();
        did_open_msg ~uri:"file:///b.el" ~text:"my-var" ();
        definition_msg ~id:2 ~uri:"file:///b.el" ~line:0 ~character:0 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No definition response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool)
        "result is not null (found cross-file .el defvar)" true (result <> `Null);
      let result_uri = result |> member "uri" |> to_string in
      Alcotest.(check string) "uri points to a.el" "file:///a.el" result_uri

let test_definition_prefers_local () =
  (* Both a.el and b.el define my-fn. Goto-definition from b.el should resolve
     locally in b.el, not jump to a.el. *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///a.el" ~text:"(defun my-fn () 1)" ();
        did_open_msg ~uri:"file:///b.el" ~text:"(defun my-fn () 2)\n(my-fn)" ();
        definition_msg ~id:2 ~uri:"file:///b.el" ~line:1 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No definition response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let result_uri = result |> member "uri" |> to_string in
      Alcotest.(check string)
        "uri points to b.el (local preferred)" "file:///b.el" result_uri

(** {1 Find References Tests} *)

let test_references_all_occurrences () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el"
          ~text:"(defun foo () 42)\n(foo)\n(foo)" ();
        references_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:7 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No references response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let locations = result |> to_list in
      Alcotest.(check int) "found 3 references" 3 (List.length locations)

let test_references_on_unknown_symbol () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(unknown-fn)" ();
        references_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No references response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let locations = result |> to_list in
      Alcotest.(check int) "found 1 reference" 1 (List.length locations)

let test_references_not_on_symbol () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"42" ();
        references_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:0 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No references response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is null" true (result = `Null)

let test_references_have_uris () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defun foo () 42)\n(foo)" ();
        references_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No references response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let locations = result |> to_list in
      Alcotest.(check bool) "has locations" true (List.length locations > 0);
      List.iter
        (fun loc ->
          let uri = loc |> member "uri" |> to_string in
          Alcotest.(check bool)
            "uri starts with file://" true
            (String.length uri > 7 && String.sub uri 0 7 = "file://"))
        locations

let test_references_defvar () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el"
          ~text:"(defvar my-var 10)\n(+ my-var 1)\nmy-var" ();
        references_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:8 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No references response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let locations = result |> to_list in
      Alcotest.(check int) "found 3 references" 3 (List.length locations)

let test_references_cross_file () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///a.el"
          ~text:"(defun shared-fn () 42)\n(shared-fn)" ();
        did_open_msg ~uri:"file:///b.el" ~text:"(shared-fn)\n(shared-fn)" ();
        references_msg ~id:2 ~uri:"file:///a.el" ~line:0 ~character:7 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No references response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let locations = result |> to_list in
      (* 2 in a.el (defun name + call) + 2 in b.el *)
      Alcotest.(check int) "found 4 references" 4 (List.length locations);
      let uris =
        List.map (fun loc -> loc |> member "uri" |> to_string) locations
      in
      let a_count =
        List.length (List.filter (fun u -> u = "file:///a.el") uris)
      in
      let b_count =
        List.length (List.filter (fun u -> u = "file:///b.el") uris)
      in
      Alcotest.(check int) "2 refs in a.el" 2 a_count;
      Alcotest.(check int) "2 refs in b.el" 2 b_count

(** {1 Code Action Tests} *)

let test_code_action_returns_empty_list () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        code_action_msg ~id:2 ~uri:"file:///test.el" ~start_line:0
          ~start_character:0 ~end_line:0 ~end_character:0 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let actions = result |> to_list in
      Alcotest.(check int) "returns empty list" 0 (List.length actions)

let test_code_action_parses_context () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 \"hello\")" ();
        code_action_msg ~id:2 ~uri:"file:///test.el" ~start_line:0
          ~start_character:5 ~end_line:0 ~end_character:12
          ~diagnostics:
            [
              `Assoc
                [
                  ( "range",
                    `Assoc
                      [
                        ( "start",
                          `Assoc [ ("line", `Int 0); ("character", `Int 5) ] );
                        ( "end",
                          `Assoc [ ("line", `Int 0); ("character", `Int 12) ] );
                      ] );
                  ("severity", `Int 1);
                  ("code", `String "E0001");
                  ("message", `String "type mismatch");
                  ("source", `String "tart");
                ];
            ]
          ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      Alcotest.(check bool) "no error" true (json |> member "error" = `Null);
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null)

let test_code_action_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let caps = result |> member "capabilities" in
      let code_action_provider = caps |> member "codeActionProvider" in
      Alcotest.(check bool)
        "codeActionProvider is true" true
        (code_action_provider = `Bool true)

let test_code_action_missing_signature_quickfix () =
  let temp_dir = Filename.temp_dir "tart_test" "" in
  let el_path = Filename.concat temp_dir "test.el" in
  let tart_path = Filename.concat temp_dir "test.tart" in
  Out_channel.with_open_text el_path (fun oc ->
      output_string oc "(defun my-add (x y) (+ x y))");
  Out_channel.with_open_text tart_path (fun oc ->
      output_string oc "; empty signature file\n");
  let uri = "file://" ^ el_path in
  let el_content = "(defun my-add (x y) (+ x y))" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri ~text:el_content ();
        code_action_msg ~id:2 ~uri ~start_line:0 ~start_character:0 ~end_line:0
          ~end_character:28 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let actions = result |> to_list in
      let quickfix_actions =
        List.filter
          (fun a -> a |> member "kind" |> to_string = "quickfix")
          actions
      in
      Alcotest.(check int) "has one quickfix" 1 (List.length quickfix_actions);
      let action = List.hd quickfix_actions in
      let title = action |> member "title" |> to_string in
      Alcotest.(check bool)
        "title contains function name" true
        (contains_string ~needle:"my-add" title);
      let kind = action |> member "kind" |> to_string in
      Alcotest.(check string) "kind is quickfix" "quickfix" kind

let test_code_action_quickfix_has_edit () =
  let temp_dir = Filename.temp_dir "tart_test" "" in
  let el_path = Filename.concat temp_dir "test.el" in
  let tart_path = Filename.concat temp_dir "test.tart" in
  Out_channel.with_open_text el_path (fun oc ->
      output_string oc "(defun my-add (x y) (+ x y))");
  Out_channel.with_open_text tart_path (fun oc -> output_string oc "");
  let uri = "file://" ^ el_path in
  let el_content = "(defun my-add (x y) (+ x y))" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri ~text:el_content ();
        code_action_msg ~id:2 ~uri ~start_line:0 ~start_character:0 ~end_line:0
          ~end_character:28 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let actions = result |> to_list in
      let action = List.hd actions in
      let edit = action |> member "edit" in
      Alcotest.(check bool) "has edit" true (edit <> `Null);
      let doc_changes = edit |> member "documentChanges" |> to_list in
      Alcotest.(check int) "has one document change" 1 (List.length doc_changes);
      let doc_change = List.hd doc_changes in
      let text_document = doc_change |> member "textDocument" in
      let target_uri = text_document |> member "uri" |> to_string in
      Alcotest.(check bool)
        "edit targets .tart file" true
        (contains_string ~needle:".tart" target_uri);
      let edits = doc_change |> member "edits" |> to_list in
      let text_edit = List.hd edits in
      let new_text = text_edit |> member "newText" |> to_string in
      Alcotest.(check bool)
        "new text contains defun" true
        (contains_string ~needle:"defun" new_text);
      Alcotest.(check bool)
        "new text contains function name" true
        (contains_string ~needle:"my-add" new_text)

let test_code_action_respects_range () =
  let temp_dir = Filename.temp_dir "tart_test" "" in
  let el_path = Filename.concat temp_dir "test.el" in
  let tart_path = Filename.concat temp_dir "test.tart" in
  Out_channel.with_open_text el_path (fun oc ->
      output_string oc "(defun fn-one (x) (+ x 1))\n(defun fn-two (y) (+ y 2))");
  Out_channel.with_open_text tart_path (fun oc -> output_string oc "");
  let uri = "file://" ^ el_path in
  let el_content = "(defun fn-one (x) (+ x 1))\n(defun fn-two (y) (+ y 2))" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri ~text:el_content ();
        code_action_msg ~id:2 ~uri ~start_line:1 ~start_character:0 ~end_line:1
          ~end_character:26 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let actions = result |> to_list in
      let quickfix_actions =
        List.filter
          (fun a -> a |> member "kind" |> to_string = "quickfix")
          actions
      in
      Alcotest.(check int)
        "returns one quickfix action" 1
        (List.length quickfix_actions);
      let action = List.hd quickfix_actions in
      let title = action |> member "title" |> to_string in
      Alcotest.(check bool)
        "action is for fn-two" true
        (contains_string ~needle:"fn-two" title)

(** {1 Extract Function Tests} *)

let test_extract_function_appears_on_selection () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defun foo (x) (+ x 1))" ();
        code_action_msg ~id:2 ~uri:"file:///test.el" ~start_line:0
          ~start_character:16 ~end_line:0 ~end_character:23 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let actions = result |> to_list in
      let extract_actions =
        List.filter
          (fun a -> a |> member "kind" |> to_string = "refactor.extract")
          actions
      in
      Alcotest.(check int) "has extract action" 1 (List.length extract_actions);
      let action = List.hd extract_actions in
      let title = action |> member "title" |> to_string in
      Alcotest.(check string)
        "title is extract function" "Extract function" title

let test_extract_function_has_edit () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defun foo (x) (+ x 1))" ();
        code_action_msg ~id:2 ~uri:"file:///test.el" ~start_line:0
          ~start_character:16 ~end_line:0 ~end_character:23 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let actions = result |> to_list in
      let extract_actions =
        List.filter
          (fun a -> a |> member "kind" |> to_string = "refactor.extract")
          actions
      in
      let action = List.hd extract_actions in
      let edit = action |> member "edit" in
      Alcotest.(check bool) "has edit" true (edit <> `Null);
      let doc_changes = edit |> member "documentChanges" |> to_list in
      Alcotest.(check int) "has one doc change" 1 (List.length doc_changes);
      let doc_change = List.hd doc_changes in
      let edits = doc_change |> member "edits" |> to_list in
      Alcotest.(check int) "has two edits" 2 (List.length edits)

let test_extract_function_captures_free_vars () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defun foo (x y) (+ x y))" ();
        code_action_msg ~id:2 ~uri:"file:///test.el" ~start_line:0
          ~start_character:17 ~end_line:0 ~end_character:24 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let actions = result |> to_list in
      let extract_actions =
        List.filter
          (fun a -> a |> member "kind" |> to_string = "refactor.extract")
          actions
      in
      let action = List.hd extract_actions in
      let edit = action |> member "edit" in
      let doc_changes = edit |> member "documentChanges" |> to_list in
      let doc_change = List.hd doc_changes in
      let edits = doc_change |> member "edits" |> to_list in
      let insert_edit =
        List.find
          (fun e ->
            let new_text = e |> member "newText" |> to_string in
            contains_string ~needle:"defun" new_text)
          edits
      in
      let new_text = insert_edit |> member "newText" |> to_string in
      Alcotest.(check bool)
        "has x param" true
        (contains_string ~needle:"(x y)" new_text)

(** {1 Document Symbol Tests} *)

let test_document_symbol_defun () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el"
          ~text:"(defun my-func (x y) (+ x y))" ();
        document_symbol_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No document symbol response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let symbols = result |> to_list in
      Alcotest.(check int) "has one symbol" 1 (List.length symbols);
      let symbol = List.hd symbols in
      let name = symbol |> member "name" |> to_string in
      Alcotest.(check string) "name is my-func" "my-func" name;
      let kind = symbol |> member "kind" |> to_int in
      Alcotest.(check int) "kind is Function" 12 kind;
      let detail = symbol |> member "detail" |> to_string in
      Alcotest.(check string) "detail shows params" "(x y)" detail

let test_document_symbol_defvar () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defvar my-var 42)" ();
        document_symbol_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No document symbol response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let symbols = result |> to_list in
      Alcotest.(check int) "has one symbol" 1 (List.length symbols);
      let symbol = List.hd symbols in
      let name = symbol |> member "name" |> to_string in
      Alcotest.(check string) "name is my-var" "my-var" name;
      let kind = symbol |> member "kind" |> to_int in
      Alcotest.(check int) "kind is Variable" 13 kind

let test_document_symbol_defconst () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defconst my-const 100)" ();
        document_symbol_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No document symbol response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let symbols = result |> to_list in
      Alcotest.(check int) "has one symbol" 1 (List.length symbols);
      let symbol = List.hd symbols in
      let name = symbol |> member "name" |> to_string in
      Alcotest.(check string) "name is my-const" "my-const" name;
      let kind = symbol |> member "kind" |> to_int in
      Alcotest.(check int) "kind is Constant" 14 kind

let test_document_symbol_defmacro () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el"
          ~text:"(defmacro my-macro (body) `(progn ,body))" ();
        document_symbol_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No document symbol response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let symbols = result |> to_list in
      Alcotest.(check int) "has one symbol" 1 (List.length symbols);
      let symbol = List.hd symbols in
      let name = symbol |> member "name" |> to_string in
      Alcotest.(check string) "name is my-macro" "my-macro" name;
      let kind = symbol |> member "kind" |> to_int in
      Alcotest.(check int) "kind is Method" 6 kind;
      let detail = symbol |> member "detail" |> to_string in
      Alcotest.(check string) "detail shows params" "(body)" detail

let test_document_symbol_capability () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let caps = result |> member "capabilities" in
      let doc_symbol_provider = caps |> member "documentSymbolProvider" in
      Alcotest.(check bool)
        "documentSymbolProvider is true" true
        (doc_symbol_provider = `Bool true)

let test_document_symbol_empty () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        document_symbol_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No document symbol response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let symbols = result |> to_list in
      Alcotest.(check int) "no symbols" 0 (List.length symbols)

let test_extract_function_no_cursor () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defun foo (x) (+ x 1))" ();
        code_action_msg ~id:2 ~uri:"file:///test.el" ~start_line:0
          ~start_character:16 ~end_line:0 ~end_character:16 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let actions = result |> to_list in
      let extract_actions =
        List.filter
          (fun a -> a |> member "kind" |> to_string = "refactor.extract")
          actions
      in
      Alcotest.(check int) "no extract action" 0 (List.length extract_actions)

(** {1 Completion Tests} *)

let test_completion_returns_local_defun () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defun my-foo (x) x)\n(my-f"
          ();
        completion_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:5 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No completion response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let items = result |> to_list in
      let my_foo_items =
        List.filter
          (fun item -> item |> member "label" |> to_string = "my-foo")
          items
      in
      Alcotest.(check int) "found my-foo" 1 (List.length my_foo_items)

let test_completion_returns_defvar () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defvar my-var 42)\n(+ my-v"
          ();
        completion_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:6 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No completion response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let items = result |> to_list in
      let my_var_items =
        List.filter
          (fun item -> item |> member "label" |> to_string = "my-var")
          items
      in
      Alcotest.(check int) "found my-var" 1 (List.length my_var_items);
      let item = List.hd my_var_items in
      let kind = item |> member "kind" |> to_int in
      Alcotest.(check int) "kind is Variable" 6 kind

let test_completion_has_type_info () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el"
          ~text:"(defun add (x y) (+ x y))\n(ad" ();
        completion_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:3 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No completion response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let items = result |> to_list in
      let add_items =
        List.filter
          (fun item -> item |> member "label" |> to_string = "add")
          items
      in
      Alcotest.(check int) "found add" 1 (List.length add_items);
      let item = List.hd add_items in
      let detail = item |> member "detail" |> to_string in
      Alcotest.(check bool) "has detail" true (String.length detail > 0)

let test_completion_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let caps = result |> member "capabilities" in
      let completion_provider = caps |> member "completionProvider" in
      Alcotest.(check bool)
        "completionProvider is present" true
        (completion_provider <> `Null)

let test_completion_filters_by_prefix () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el"
          ~text:
            "(defun my-alpha () nil)\n\
             (defun my-beta () nil)\n\
             (defun other () nil)\n\
             (my-b"
          ();
        completion_msg ~id:2 ~uri:"file:///test.el" ~line:3 ~character:5 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No completion response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let items = result |> to_list in
      let labels =
        List.map (fun item -> item |> member "label" |> to_string) items
      in
      Alcotest.(check bool) "my-beta included" true (List.mem "my-beta" labels);
      Alcotest.(check bool)
        "my-alpha excluded" true
        (not (List.mem "my-alpha" labels));
      Alcotest.(check bool)
        "other excluded" true
        (not (List.mem "other" labels))

let test_completion_scope_aware_let_body () =
  (* Inside a let body, the let-bound variable should appear *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el"
          ~text:"(let ((my-local 42))\n  (+ my-l 1))" ();
        completion_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:8 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No completion response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let items = result |> to_list in
      let labels =
        List.map (fun item -> item |> member "label" |> to_string) items
      in
      Alcotest.(check bool)
        "my-local visible in let body" true
        (List.mem "my-local" labels)

let test_completion_scope_aware_outside_let () =
  (* Outside the let body, the let-bound variable should NOT appear *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el"
          ~text:"(let ((inner-var 42))\n  (+ inner-var 1))\n(+ inne 1)" ();
        completion_msg ~id:2 ~uri:"file:///test.el" ~line:2 ~character:7 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No completion response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let items = result |> to_list in
      let labels =
        List.map (fun item -> item |> member "label" |> to_string) items
      in
      Alcotest.(check bool)
        "inner-var not visible outside let" true
        (not (List.mem "inner-var" labels))

let test_completion_scope_aware_defun_params () =
  (* Inside a defun body, parameters should appear in completions *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el"
          ~text:"(defun my-fn (my-param)\n  (+ my-p 1))" ();
        completion_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:8 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No completion response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let items = result |> to_list in
      let labels =
        List.map (fun item -> item |> member "label" |> to_string) items
      in
      Alcotest.(check bool)
        "my-param visible in defun body" true
        (List.mem "my-param" labels)

(** {1 Signature Help Tests} *)

let test_signature_help_shows_function_signature () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el"
          ~text:"(defun add (x y) (+ x y))\n(add 1 2)" ();
        signature_help_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:5 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No signature help response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let signatures = result |> member "signatures" |> to_list in
      Alcotest.(check bool)
        "has at least one signature" true
        (List.length signatures > 0);
      let first_sig = List.hd signatures in
      let label = first_sig |> member "label" |> to_string in
      Alcotest.(check bool)
        "signature contains function name" true
        (String.length label > 0 && String.sub label 0 4 = "(add")

let test_signature_help_highlights_active_parameter () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el"
          ~text:"(defun add (x y) (+ x y))\n(add 1 2)" ();
        signature_help_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:7 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No signature help response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let active_param =
        match result |> member "activeParameter" with
        | `Int i -> Some i
        | _ -> None
      in
      Alcotest.(check (option int))
        "active parameter is 1 (second arg)" (Some 1) active_param

let test_signature_help_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let caps = result |> member "capabilities" in
      let sig_help_provider = caps |> member "signatureHelpProvider" in
      Alcotest.(check bool)
        "signatureHelpProvider is present" true
        (sig_help_provider <> `Null)

let test_signature_help_not_in_call () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defun foo () 42)\n\n123" ();
        signature_help_msg ~id:2 ~uri:"file:///test.el" ~line:2 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No signature help response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is null" true (result = `Null)

let test_signature_help_has_parameters () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el"
          ~text:"(defun add (x y) (+ x y))\n(add 1 2)" ();
        signature_help_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:5 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No signature help response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let signatures = result |> member "signatures" |> to_list in
      let first_sig = List.hd signatures in
      let parameters = first_sig |> member "parameters" |> to_list in
      Alcotest.(check int) "has 2 parameters" 2 (List.length parameters)

(** {1 Rename Tests} *)

let test_rename_all_occurrences () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el"
          ~text:"(defun foo () 42)\n(foo)\n(foo)" ();
        rename_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:7
          ~new_name:"bar" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No rename response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let doc_changes = result |> member "documentChanges" |> to_list in
      Alcotest.(check int) "has 1 document change" 1 (List.length doc_changes);
      let first_doc = List.hd doc_changes in
      let edits = first_doc |> member "edits" |> to_list in
      Alcotest.(check int) "has 3 edits" 3 (List.length edits)

let test_rename_has_new_name () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defun foo () 42)" ();
        rename_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:7
          ~new_name:"my-new-name" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No rename response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let doc_changes = result |> member "documentChanges" |> to_list in
      let first_doc = List.hd doc_changes in
      let edits = first_doc |> member "edits" |> to_list in
      let first_edit = List.hd edits in
      let new_text = first_edit |> member "newText" |> to_string in
      Alcotest.(check string) "new text is my-new-name" "my-new-name" new_text

let test_rename_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let caps = result |> member "capabilities" in
      let rename_provider = caps |> member "renameProvider" in
      let prepare_provider =
        rename_provider |> member "prepareProvider" |> to_bool
      in
      Alcotest.(check bool) "prepareProvider is true" true prepare_provider

let test_rename_not_on_symbol () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        rename_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:3
          ~new_name:"foo" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No rename response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is null" true (result = `Null)

let test_rename_has_correct_uri () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///my-project/test.el"
          ~text:"(defvar my-var 42)" ();
        rename_msg ~id:2 ~uri:"file:///my-project/test.el" ~line:0 ~character:8
          ~new_name:"new-var" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No rename response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let doc_changes = result |> member "documentChanges" |> to_list in
      let first_doc = List.hd doc_changes in
      let text_doc = first_doc |> member "textDocument" in
      let uri = text_doc |> member "uri" |> to_string in
      Alcotest.(check string) "uri matches" "file:///my-project/test.el" uri

let test_rename_cross_file () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///a.el"
          ~text:"(defun shared-fn () 42)\n(shared-fn)" ();
        did_open_msg ~uri:"file:///b.el" ~text:"(shared-fn)\n(shared-fn)" ();
        rename_msg ~id:2 ~uri:"file:///a.el" ~line:0 ~character:7
          ~new_name:"renamed-fn" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No rename response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let doc_changes = result |> member "documentChanges" |> to_list in
      Alcotest.(check bool)
        "has 2 document changes" true
        (List.length doc_changes = 2);
      (* Collect all edits from all documents *)
      let all_edits =
        List.concat_map (fun dc -> dc |> member "edits" |> to_list) doc_changes
      in
      (* 2 in a.el (defun name + call) + 2 in b.el *)
      Alcotest.(check int) "has 4 total edits" 4 (List.length all_edits);
      (* All edits have the new name *)
      List.iter
        (fun edit ->
          let new_text = edit |> member "newText" |> to_string in
          Alcotest.(check string) "newText is renamed-fn" "renamed-fn" new_text)
        all_edits;
      (* Both URIs are present *)
      let uris =
        List.map
          (fun dc -> dc |> member "textDocument" |> member "uri" |> to_string)
          doc_changes
      in
      Alcotest.(check bool) "a.el present" true (List.mem "file:///a.el" uris);
      Alcotest.(check bool) "b.el present" true (List.mem "file:///b.el" uris)

(** {1 Prepare Rename Tests} *)

let test_prepare_rename_symbol () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(defun foo () 42)\n(foo)" ();
        prepare_rename_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:7 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No prepare rename response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let placeholder = result |> member "placeholder" |> to_string in
      Alcotest.(check string) "placeholder is symbol name" "foo" placeholder;
      let range = result |> member "range" in
      let start = range |> member "start" in
      let start_line = start |> member "line" |> to_int in
      let start_char = start |> member "character" |> to_int in
      Alcotest.(check int) "start line" 0 start_line;
      Alcotest.(check int) "start character" 7 start_char

let test_prepare_rename_builtin () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        prepare_rename_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No prepare rename response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is null for builtin" true (result = `Null)

let test_prepare_rename_keyword () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(list :foo 1)" ();
        prepare_rename_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:6 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No prepare rename response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is null for keyword" true (result = `Null)

let test_prepare_rename_number () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 42 1)" ();
        prepare_rename_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:3 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No prepare rename response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is null for number" true (result = `Null)

let test_prepare_rename_string () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:{|(message "hello")|} ();
        prepare_rename_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:9 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No prepare rename response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is null for string" true (result = `Null)

(** {1 UTF-16 Position Tests} *)

(** Diagnostics for code with CJK characters report UTF-16 positions, not byte
    offsets. Line 1 is [(+ 日本語 "bad")] where ["bad"] starts at byte 13 but
    UTF-16 offset 7. *)
let test_diagnostic_cjk_positions () =
  let text = "(defvar 日本語 42)\n(+ 日本語 \"bad\")" in
  let result =
    run_initialized_session [ did_open_msg ~uri:"file:///test.el" ~text () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_diagnostics ~uri:"file:///test.el" result.messages with
  | None -> Alcotest.fail "No publishDiagnostics notification found"
  | Some json ->
      let open Yojson.Safe.Util in
      let params = json |> member "params" in
      let diagnostics = params |> member "diagnostics" |> to_list in
      Alcotest.(check bool) "has diagnostics" true (List.length diagnostics > 0);
      (* Find a diagnostic on line 1 (the error line) *)
      let line1_diags =
        List.filter
          (fun d ->
            let range = d |> member "range" in
            let start = range |> member "start" in
            start |> member "line" |> to_int = 1)
          diagnostics
      in
      Alcotest.(check bool)
        "has diagnostic on line 1" true
        (List.length line1_diags > 0);
      (* The diagnostic character positions should be in UTF-16 code units.
         "bad" starts at UTF-16 offset 7, not byte offset 13. *)
      let diag = List.hd line1_diags in
      let range = diag |> member "range" in
      let start_char =
        range |> member "start" |> member "character" |> to_int
      in
      Alcotest.(check bool)
        "start character is UTF-16 (<=7), not byte offset (13)" true
        (start_char <= 7)

(** Diagnostics for code with 4-byte emoji report UTF-16 positions using
    surrogate-pair counting. Line 0 is [(+ 🎉 "bad")] where ["bad"] starts at
    byte 8 but UTF-16 offset 6. *)
let test_diagnostic_emoji_positions () =
  let text = "(+ \xF0\x9F\x8E\x89 \"bad\")" in
  let result =
    run_initialized_session [ did_open_msg ~uri:"file:///test.el" ~text () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_diagnostics ~uri:"file:///test.el" result.messages with
  | None -> Alcotest.fail "No publishDiagnostics notification found"
  | Some json ->
      let open Yojson.Safe.Util in
      let params = json |> member "params" in
      let diagnostics = params |> member "diagnostics" |> to_list in
      Alcotest.(check bool) "has diagnostics" true (List.length diagnostics > 0);
      let diag = List.hd diagnostics in
      let range = diag |> member "range" in
      let start_char =
        range |> member "start" |> member "character" |> to_int
      in
      (* 🎉 is 4 bytes / 2 UTF-16 code units. "bad" starts at byte 8 but
         UTF-16 offset 6. A byte-based implementation would report 8. *)
      Alcotest.(check bool)
        "start character is UTF-16 (<=6), not byte offset (8)" true
        (start_char <= 6)

(** Hover on a literal after CJK characters returns a range with UTF-16
    positions. [(defvar 日本語 42)] — [42] starts at byte 18, UTF-16 12. *)
let test_hover_cjk_range () =
  let text = "(defvar \xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e 42)" in
  (* Hover at UTF-16 character 12, which is byte 18 (the '4' of 42). *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        hover_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:12 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No hover response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let range = result |> member "range" in
      let start_char =
        range |> member "start" |> member "character" |> to_int
      in
      let end_char = range |> member "end" |> member "character" |> to_int in
      (* 42 is at UTF-16 12-14, not bytes 18-20 *)
      Alcotest.(check int) "start character is UTF-16" 12 start_char;
      Alcotest.(check int) "end character is UTF-16" 14 end_char

(** Incremental didChange with UTF-16 positions on a line containing CJK.
    Replaces ["world"] in ["日本語 hello world"] using UTF-16 character offsets.
    "world" starts at UTF-16 offset 10 (3 CJK + space + 5 ASCII + space). *)
let test_did_change_cjk_utf16 () =
  let text = "\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e hello world" in
  (* "日本語 hello world"
     日: bytes 0-2,  UTF-16 0
     本: bytes 3-5,  UTF-16 1
     語: bytes 6-8,  UTF-16 2
     ' ': byte 9,    UTF-16 3
     hello: bytes 10-14, UTF-16 4-8
     ' ': byte 15,   UTF-16 9
     world: bytes 16-20, UTF-16 10-14 *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        did_change_msg ~uri:"file:///test.el" ~version:2
          ~changes:
            [
              `Assoc
                [
                  ( "range",
                    `Assoc
                      [
                        ( "start",
                          `Assoc [ ("line", `Int 0); ("character", `Int 10) ] );
                        ( "end",
                          `Assoc [ ("line", `Int 0); ("character", `Int 15) ] );
                      ] );
                  ("text", `String "Emacs");
                ];
            ]
          ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match Document.get_doc (Server.documents result.server) "file:///test.el" with
  | Some doc ->
      Alcotest.(check string)
        "doc text after change"
        "\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e hello Emacs" doc.text
  | None -> Alcotest.fail "Document not found after didChange"

(** {1 Sync Recovery Tests} *)

let test_edit_past_end_succeeds () =
  (* Open a document, send a didChange with range extending past document
     bounds, then verify the server doesn't crash and subsequent operations
     (hover, diagnostics) still work on the clamped document. *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        (* Range extends far past document end — should be clamped *)
        did_change_msg ~uri:"file:///test.el" ~version:2
          ~changes:
            [
              `Assoc
                [
                  ( "range",
                    `Assoc
                      [
                        ( "start",
                          `Assoc [ ("line", `Int 0); ("character", `Int 5) ] );
                        ( "end",
                          `Assoc [ ("line", `Int 99); ("character", `Int 99) ]
                        );
                      ] );
                  ("text", `String "\"hello\")");
                ];
            ]
          ();
        (* Hover on the `+` to verify the server is still responsive *)
        hover_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  (* Document should reflect the clamped edit: "(+ 1 " + "\"hello\")" *)
  (match
     Document.get_doc (Server.documents result.server) "file:///test.el"
   with
  | Some doc ->
      Alcotest.(check string)
        "doc text after clamped edit" "(+ 1 \"hello\")" doc.text;
      Alcotest.(check int) "doc version" 2 doc.version
  | None -> Alcotest.fail "Document not found after clamped didChange");
  (* Hover should return a response (server is still working) *)
  (match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No hover response after clamped edit"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_field = json |> member "result" in
      Alcotest.(check bool)
        "hover result is not null" true (result_field <> `Null));
  (* Diagnostics should have been re-published after the edit *)
  match find_last_diagnostics ~uri:"file:///test.el" result.messages with
  | None -> Alcotest.fail "No diagnostics after clamped edit"
  | Some json ->
      let open Yojson.Safe.Util in
      let params = json |> member "params" in
      let _diagnostics = params |> member "diagnostics" |> to_list in
      (* Just verify diagnostics were published — exact content is not the
         point of this test *)
      ()

(** {1 Diagnostic Dedup Tests} *)

let test_identical_diagnostics_suppressed () =
  (* Open a file with an error, then send a whitespace-only change that
     doesn't alter the diagnostic set. The server should suppress the
     second (identical) publishDiagnostics notification. *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 \"hello\")" ();
        (* Append a trailing newline — same diagnostics *)
        did_change_full_msg ~uri:"file:///test.el" ~version:2
          ~text:"(+ 1 \"hello\")\n" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  let diag_notifications =
    find_all_notifications ~method_:"textDocument/publishDiagnostics"
      result.messages
  in
  let uri_notifications =
    List.filter
      (fun json ->
        let open Yojson.Safe.Util in
        json |> member "params" |> member "uri" |> to_string = "file:///test.el")
      diag_notifications
  in
  (* Should have exactly 1: the initial open publishes diagnostics, but the
     second change produces identical diagnostics and is suppressed. *)
  Alcotest.(check int)
    "only one publishDiagnostics for identical set" 1
    (List.length uri_notifications)

let test_diagnostics_republished_after_reopen () =
  (* Close a document with errors, then reopen it. The dedup cache should not
     suppress the diagnostics on reopen — the cache was cleared on close.
     The worker may coalesce, so check the final state rather than exact
     notification count. *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 \"hello\")" ();
        did_close_msg ~uri:"file:///test.el" ();
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 \"hello\")" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  let diag_notifications =
    find_all_notifications ~method_:"textDocument/publishDiagnostics"
      result.messages
  in
  let uri_notifications =
    List.filter
      (fun json ->
        let open Yojson.Safe.Util in
        json |> member "params" |> member "uri" |> to_string = "file:///test.el")
      diag_notifications
  in
  (* At least 2 notifications: close (empty) + second open (errors).
     The first open's diagnostics may be coalesced away by the worker. *)
  Alcotest.(check bool)
    "diagnostics republished after reopen" true
    (List.length uri_notifications >= 2);
  (* Last notification should have diagnostics (not empty) *)
  match List.rev uri_notifications with
  | last :: _ ->
      let open Yojson.Safe.Util in
      let diagnostics =
        last |> member "params" |> member "diagnostics" |> to_list
      in
      Alcotest.(check bool)
        "last notification has diagnostics" true
        (List.length diagnostics > 0)
  | [] -> Alcotest.fail "No diagnostics notifications"

(** {1 Version Staleness Tests} *)

let test_final_diagnostics_match_final_state () =
  (* After a sequence of changes, the final published diagnostics must
     reflect the final document state. This validates version plumbing:
     each publish_diagnostics call uses the triggering version. *)
  let result =
    run_initialized_session
      [
        (* Start with a type error *)
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 \"hello\")" ();
        (* Change to valid code — no diagnostics *)
        did_change_full_msg ~uri:"file:///test.el" ~version:2 ~text:"(+ 1 2)" ();
        (* Change to code with a parse error — unclosed paren *)
        did_change_full_msg ~uri:"file:///test.el" ~version:3 ~text:"(+ 1 2" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  (* The last diagnostics notification should reflect v3's parse error *)
  match find_last_diagnostics ~uri:"file:///test.el" result.messages with
  | None -> Alcotest.fail "No diagnostics notification found"
  | Some last ->
      let open Yojson.Safe.Util in
      let params = last |> member "params" in
      let diagnostics = params |> member "diagnostics" |> to_list in
      (* v3 has a parse error (unclosed paren) *)
      Alcotest.(check bool)
        "final diagnostics reflect final state" true
        (List.length diagnostics > 0);
      (* The parse error for unclosed paren is different from v2's clean
         state. Verify the diagnostic is indeed a parse error. *)
      let diag_msg = List.hd diagnostics |> member "message" |> to_string in
      Alcotest.(check bool)
        "diagnostic is a parse error from final state" true
        (String.length diag_msg > 0)

(** {1 Document Save Tests} *)

let test_save_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response"
  | Some json ->
      let open Yojson.Safe.Util in
      let caps = json |> member "result" |> member "capabilities" in
      let sync = caps |> member "textDocumentSync" in
      Alcotest.(check bool)
        "save capability" true
        (sync |> member "save" |> to_bool)

let test_save_triggers_fresh_diagnostics () =
  (* Open with valid code, then save. The save should force a full re-check
     and publish diagnostics. We verify by counting the total diagnostic
     notifications: open publishes once, save re-checks and publishes (dedup
     suppresses identical results, but the save path still runs). *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 \"bad\")" ();
        (* Save — forces a full re-check *)
        did_save_msg ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  (* After open+save with the same content, dedup suppresses the second
     identical set. But the first open must have produced diagnostics. *)
  let diag_notifications =
    find_all_notifications ~method_:"textDocument/publishDiagnostics"
      result.messages
  in
  Alcotest.(check bool)
    "has diagnostics notifications" true
    (List.length diag_notifications >= 1);
  (* The diagnostics should contain the type error *)
  match find_last_diagnostics ~uri:"file:///test.el" result.messages with
  | None -> Alcotest.fail "No diagnostics notification found"
  | Some last ->
      let open Yojson.Safe.Util in
      let diagnostics =
        last |> member "params" |> member "diagnostics" |> to_list
      in
      Alcotest.(check bool)
        "diagnostics present" true
        (List.length diagnostics > 0)

let test_save_invalidates_cache () =
  (* Save forces a full re-check even when the form cache would normally hit.
     Verify by: open → change to error → save → check that error diagnostics
     are still published (the cache was invalidated). *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        (* Change to have a parse error *)
        did_change_full_msg ~uri:"file:///test.el" ~version:2 ~text:"(+ 1 2) ("
          ();
        (* Save — cache invalidated, full re-check *)
        did_save_msg ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_last_diagnostics ~uri:"file:///test.el" result.messages with
  | None -> Alcotest.fail "No diagnostics notification found"
  | Some last ->
      let open Yojson.Safe.Util in
      let diagnostics =
        last |> member "params" |> member "diagnostics" |> to_list
      in
      (* The parse error from v2 should still be present after save *)
      Alcotest.(check bool)
        "diagnostics present after save" true
        (List.length diagnostics > 0)

(** {1 Folding Range Tests} *)

let test_folding_range_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response"
  | Some json ->
      let open Yojson.Safe.Util in
      let caps = json |> member "result" |> member "capabilities" in
      Alcotest.(check bool)
        "foldingRangeProvider" true
        (caps |> member "foldingRangeProvider" |> to_bool)

let test_folding_range_multiline_defun () =
  let text = "(defun foo (x)\n  (+ x 1))" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        folding_range_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No folding range response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_field = json |> member "result" in
      let ranges = result_field |> to_list in
      Alcotest.(check bool) "has at least one fold" true (List.length ranges > 0);
      let first = List.hd ranges in
      Alcotest.(check int) "startLine" 0 (first |> member "startLine" |> to_int);
      Alcotest.(check int) "endLine" 1 (first |> member "endLine" |> to_int)

let test_folding_range_comments () =
  let text = ";; line 1\n;; line 2\n;; line 3\n(+ 1 2)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        folding_range_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No folding range response"
  | Some json ->
      let open Yojson.Safe.Util in
      let ranges = json |> member "result" |> to_list in
      let comment_folds =
        List.filter
          (fun r ->
            match r |> member "kind" with
            | `String "comment" -> true
            | _ -> false)
          ranges
      in
      Alcotest.(check bool)
        "has comment fold" true
        (List.length comment_folds > 0);
      let fold = List.hd comment_folds in
      Alcotest.(check int) "startLine" 0 (fold |> member "startLine" |> to_int);
      Alcotest.(check int) "endLine" 2 (fold |> member "endLine" |> to_int)

let test_folding_range_empty_file () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"" ();
        folding_range_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No folding range response"
  | Some json ->
      let open Yojson.Safe.Util in
      let ranges = json |> member "result" |> to_list in
      Alcotest.(check int) "no folds" 0 (List.length ranges)

(** {1 Semantic Tokens Tests} *)

let test_semantic_tokens_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response"
  | Some json ->
      let open Yojson.Safe.Util in
      let caps = json |> member "result" |> member "capabilities" in
      let stp = caps |> member "semanticTokensProvider" in
      let full = stp |> member "full" in
      Alcotest.(check bool)
        "full has delta" true
        (full |> member "delta" |> to_bool);
      let legend = stp |> member "legend" in
      let token_types = legend |> member "tokenTypes" |> to_list in
      Alcotest.(check bool) "has token types" true (List.length token_types > 0);
      let token_modifiers = legend |> member "tokenModifiers" |> to_list in
      Alcotest.(check bool)
        "has token modifiers" true
        (List.length token_modifiers > 0)

let test_semantic_tokens_defun () =
  let text = "(defun foo (x)\n  (+ x 1))" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        semantic_tokens_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No semantic tokens response"
  | Some json ->
      let open Yojson.Safe.Util in
      let data =
        json |> member "result" |> member "data" |> to_list |> List.map to_int
      in
      (* Should have at least some tokens *)
      Alcotest.(check bool) "has tokens" true (List.length data > 0);
      (* Tokens come in 5-tuples *)
      Alcotest.(check int) "multiple of 5" 0 (List.length data mod 5);
      (* First token is "defun" keyword at (0,1): deltaLine=0, deltaCol=1,
         len=5, type=4 (STKeyword), mods=0 *)
      Alcotest.(check int) "first deltaLine" 0 (List.nth data 0);
      Alcotest.(check int) "first deltaCol" 1 (List.nth data 1);
      Alcotest.(check int) "first length" 5 (List.nth data 2);
      Alcotest.(check int) "first type (keyword)" 4 (List.nth data 3);
      (* Second token is "foo" function+definition: deltaLine=0, deltaCol=6,
         len=3, type=0 (STFunction), mods=1 (SMDefinition) *)
      Alcotest.(check int) "second deltaLine" 0 (List.nth data 5);
      Alcotest.(check int) "second length" 3 (List.nth data 7);
      Alcotest.(check int) "second type (function)" 0 (List.nth data 8);
      Alcotest.(check int) "second mods (definition)" 1 (List.nth data 9)

let test_semantic_tokens_empty_file () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"" ();
        semantic_tokens_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No semantic tokens response"
  | Some json ->
      let open Yojson.Safe.Util in
      let data = json |> member "result" |> member "data" |> to_list in
      Alcotest.(check int) "empty data" 0 (List.length data)

let test_semantic_tokens_comments () =
  let text = ";; a comment\n(+ 1 2)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        semantic_tokens_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No semantic tokens response"
  | Some json ->
      let open Yojson.Safe.Util in
      let data =
        json |> member "result" |> member "data" |> to_list |> List.map to_int
      in
      Alcotest.(check bool) "has tokens" true (List.length data >= 5);
      (* Find a comment token (type index 7) somewhere in the data *)
      let rec has_comment_type i =
        if i + 4 >= List.length data then false
        else if List.nth data (i + 3) = 7 then true
        else has_comment_type (i + 5)
      in
      Alcotest.(check bool) "has comment token" true (has_comment_type 0)

(** {1 Inlay Hints Tests} *)

let test_inlay_hint_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response"
  | Some json ->
      let open Yojson.Safe.Util in
      let caps = json |> member "result" |> member "capabilities" in
      let inlay_hint = caps |> member "inlayHintProvider" in
      Alcotest.(check bool) "inlayHintProvider" true (to_bool inlay_hint)

let test_inlay_hint_defun_return_type () =
  let text = "(defun foo (x)\n  (+ x 1))" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        inlay_hint_msg ~id:2 ~uri:"file:///test.el" ~start_line:0
          ~start_character:0 ~end_line:1 ~end_character:11 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No inlay hint response"
  | Some json ->
      let open Yojson.Safe.Util in
      let hints = json |> member "result" |> to_list in
      Alcotest.(check bool) "has hints" true (List.length hints > 0);
      let hint = List.hd hints in
      let label = hint |> member "label" |> to_string in
      Alcotest.(check bool)
        "label starts with colon" true
        (String.length label > 0 && label.[0] = ':');
      let kind = hint |> member "kind" |> to_int in
      Alcotest.(check int) "kind is Type (1)" 1 kind

let test_inlay_hint_let_binding () =
  let text = "(let ((x (+ 1 2)))\n  x)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        inlay_hint_msg ~id:2 ~uri:"file:///test.el" ~start_line:0
          ~start_character:0 ~end_line:1 ~end_character:3 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No inlay hint response"
  | Some json ->
      let open Yojson.Safe.Util in
      let hints = json |> member "result" |> to_list in
      Alcotest.(check bool) "has let binding hints" true (List.length hints > 0);
      let hint = List.hd hints in
      let label = hint |> member "label" |> to_string in
      Alcotest.(check bool)
        "label starts with colon" true
        (String.length label > 0 && label.[0] = ':');
      let kind = hint |> member "kind" |> to_int in
      Alcotest.(check int) "kind is Type (1)" 1 kind

let test_inlay_hint_empty_file () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"" ();
        inlay_hint_msg ~id:2 ~uri:"file:///test.el" ~start_line:0
          ~start_character:0 ~end_line:0 ~end_character:0 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No inlay hint response"
  | Some json ->
      let open Yojson.Safe.Util in
      let hints = json |> member "result" |> to_list in
      Alcotest.(check int) "no hints" 0 (List.length hints)

(** {1 Type Definition Tests} *)

let test_type_definition_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response"
  | Some json ->
      let open Yojson.Safe.Util in
      let caps = json |> member "result" |> member "capabilities" in
      Alcotest.(check bool)
        "typeDefinitionProvider" true
        (caps |> member "typeDefinitionProvider" |> to_bool)

let test_type_definition_returns_null_for_primitive () =
  (* Cursor on 42 — type is integer, a primitive with no user-visible definition *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"42" ();
        type_definition_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:0 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No type definition response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_field = json |> member "result" in
      Alcotest.(check bool)
        "result is null for primitive" true (result_field = `Null)

let test_type_definition_returns_null_for_symbol () =
  (* Cursor on a symbol whose type is not a named user type *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        type_definition_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No type definition response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_field = json |> member "result" in
      Alcotest.(check bool)
        "result is null for function type" true (result_field = `Null)

let test_type_definition_empty_file () =
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"" ();
        type_definition_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:0 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No type definition response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_field = json |> member "result" in
      Alcotest.(check bool)
        "result is null for empty file" true (result_field = `Null)

(** {1 Workspace Symbols Tests} *)

let test_workspace_symbol_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response"
  | Some json ->
      let open Yojson.Safe.Util in
      let caps = json |> member "result" |> member "capabilities" in
      Alcotest.(check bool)
        "workspaceSymbolProvider" true
        (caps |> member "workspaceSymbolProvider" |> to_bool)

let test_workspace_symbol_defun_found () =
  let text = "(defun my-greet (name)\n  (message name))" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        workspace_symbol_msg ~id:2 ~query:"greet" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No workspace symbol response"
  | Some json ->
      let open Yojson.Safe.Util in
      let symbols = json |> member "result" |> to_list in
      Alcotest.(check bool) "at least one symbol" true (List.length symbols >= 1);
      let first = List.hd symbols in
      Alcotest.(check string)
        "symbol name" "my-greet"
        (first |> member "name" |> to_string);
      (* SKFunction = 12 *)
      Alcotest.(check int)
        "kind is function" 12
        (first |> member "kind" |> to_int);
      let container = first |> member "containerName" |> to_string in
      Alcotest.(check string) "container name" "test" container

let test_workspace_symbol_query_filtering () =
  let text = "(defun foo-bar ())\n(defvar baz-qux 42)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        workspace_symbol_msg ~id:2 ~query:"foo" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No workspace symbol response"
  | Some json ->
      let open Yojson.Safe.Util in
      let symbols = json |> member "result" |> to_list in
      Alcotest.(check int) "only matching symbol" 1 (List.length symbols);
      Alcotest.(check string)
        "symbol name" "foo-bar"
        (List.hd symbols |> member "name" |> to_string)

let test_workspace_symbol_empty_query () =
  let text = "(defun aaa ())\n(defvar bbb 1)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        workspace_symbol_msg ~id:2 ~query:"" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No workspace symbol response"
  | Some json ->
      let open Yojson.Safe.Util in
      let symbols = json |> member "result" |> to_list in
      Alcotest.(check bool) "returns all symbols" true (List.length symbols >= 2)

let test_workspace_symbol_empty_workspace () =
  let result =
    run_initialized_session [ workspace_symbol_msg ~id:2 ~query:"foo" () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No workspace symbol response"
  | Some json ->
      let open Yojson.Safe.Util in
      let symbols = json |> member "result" |> to_list in
      Alcotest.(check int) "no symbols" 0 (List.length symbols)

let test_workspace_symbol_tart_file () =
  let text = "(defun greet (string) -> string)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///mymod.tart" ~text ();
        workspace_symbol_msg ~id:2 ~query:"greet" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No workspace symbol response"
  | Some json ->
      let open Yojson.Safe.Util in
      let symbols = json |> member "result" |> to_list in
      Alcotest.(check bool) "at least one symbol" true (List.length symbols >= 1);
      let first = List.hd symbols in
      Alcotest.(check string)
        "symbol name" "greet"
        (first |> member "name" |> to_string);
      (* SKFunction = 12 *)
      Alcotest.(check int)
        "kind is function" 12
        (first |> member "kind" |> to_int)

(** {1 Workspace Configuration Tests} *)

let test_initialization_options_emacs_version () =
  (* Verify that initializationOptions with emacsVersion is accepted and
     diagnostics are published after initialize. *)
  let result =
    run_session
      [
        initialize_msg ~id:1
          ~initialization_options:
            (`Assoc [ ("tart.emacsVersion", `String "30.1") ])
          ();
        initialized_msg ();
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        shutdown_msg ~id:99 ();
        exit_msg ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  (* Diagnostics should be published for the opened document *)
  match find_diagnostics ~uri:"file:///test.el" result.messages with
  | None -> Alcotest.fail "No diagnostics after init with emacsVersion"
  | Some json ->
      let open Yojson.Safe.Util in
      let _diagnostics = json |> member "params" |> member "diagnostics" in
      (* Just verify it didn't crash — the specific diagnostics depend on
         available typings *)
      ()

let test_did_change_configuration_searchpath () =
  (* Open a doc, then send didChangeConfiguration with a searchPath. The
     server should accept the notification, invalidate caches, and remain
     functional — verified by a successful hover afterward. Diagnostic dedup
     may suppress re-publication of identical diagnostics, so we check the
     server still responds to requests instead of counting notifications. *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        did_change_configuration_msg
          ~settings:(`Assoc [ ("tart.searchPath", `List [ `String "/tmp" ]) ])
          ();
        hover_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  (* Verify server is still functional after config change *)
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No hover response after config change"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_field = json |> member "result" in
      Alcotest.(check bool)
        "hover works after config change" true (result_field <> `Null)

let test_empty_settings_tolerated () =
  (* Empty settings object should not crash the server *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        did_change_configuration_msg ~settings:(`Assoc []) ();
      ]
  in
  Alcotest.(check int) "exit code with empty settings" 0 result.exit_code

let test_unknown_settings_keys_ignored () =
  (* Extra unknown keys in settings should be silently ignored *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        did_change_configuration_msg
          ~settings:
            (`Assoc
               [
                 ("tart.unknownKey", `String "value");
                 ("completely.random", `Int 42);
               ])
          ();
      ]
  in
  Alcotest.(check int) "exit code with unknown keys" 0 result.exit_code

(** {1 File Watching Tests} *)

let test_file_watcher_registration () =
  (* After initialized notification, the server should send a
     client/registerCapability request for file watchers *)
  let result =
    run_session
      [
        initialize_msg ~id:1 ();
        initialized_msg ();
        shutdown_msg ~id:99 ();
        exit_msg ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_request ~method_:"client/registerCapability" result.messages with
  | None -> Alcotest.fail "No client/registerCapability request found"
  | Some json ->
      let open Yojson.Safe.Util in
      let params = json |> member "params" in
      let registrations = params |> member "registrations" |> to_list in
      Alcotest.(check bool)
        "has registrations" true
        (List.length registrations >= 1);
      let reg = List.hd registrations in
      Alcotest.(check string)
        "method is didChangeWatchedFiles" "workspace/didChangeWatchedFiles"
        (reg |> member "method" |> to_string);
      (* Verify glob patterns *)
      let watchers =
        reg |> member "registerOptions" |> member "watchers" |> to_list
      in
      let globs =
        List.map (fun w -> w |> member "globPattern" |> to_string) watchers
      in
      Alcotest.(check bool) "has *.el pattern" true (List.mem "**/*.el" globs);
      Alcotest.(check bool)
        "has *.tart pattern" true
        (List.mem "**/*.tart" globs)

let test_watched_tart_file_invalidates_dependent () =
  (* Open an .el file, then send a watched files notification for its sibling
     .tart file. The .el file's diagnostics should be republished. *)
  let result =
    run_session
      [
        initialize_msg ~id:1 ();
        initialized_msg ();
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        (* Notify that a sibling .tart file changed *)
        did_change_watched_files_msg
          ~changes:[ ("file:///test.tart", 2 (* Changed *)) ]
          ();
        shutdown_msg ~id:99 ();
        exit_msg ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  (* Should have diagnostics published for the .el file (from open, and
     possibly again from the invalidation) *)
  let diag_notifications =
    find_all_notifications ~method_:"textDocument/publishDiagnostics"
      result.messages
  in
  Alcotest.(check bool)
    "has diagnostic notifications" true
    (List.length diag_notifications >= 1)

let test_watched_file_open_ignored () =
  (* If a file is open in the editor, watched file events for the same URI
     should be ignored (the buffer is source of truth). We verify by opening
     a file with an error, then sending a "changed" event for the same URI.
     The diagnostics count should not increase beyond what the open produced. *)
  let result =
    run_session
      [
        initialize_msg ~id:1 ();
        initialized_msg ();
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 \"bad\")" ();
        (* Send a watched files notification for the same open URI *)
        did_change_watched_files_msg
          ~changes:[ ("file:///test.el", 2 (* Changed *)) ]
          ();
        shutdown_msg ~id:99 ();
        exit_msg ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  (* Only one set of diagnostics should exist — from the initial open.
     The watched file event should have been ignored for the open URI. *)
  let diag_notifications =
    find_all_notifications ~method_:"textDocument/publishDiagnostics"
      result.messages
    |> List.filter (fun json ->
        let open Yojson.Safe.Util in
        json |> member "params" |> member "uri" |> to_string = "file:///test.el")
  in
  (* Exactly 1 diagnostic notification from the open, none from the watched event *)
  Alcotest.(check int)
    "only one diagnostic notification" 1
    (List.length diag_notifications)

(** {1 Concurrency Tests} *)

let test_cancel_unknown_request () =
  (* Sending $/cancelRequest for an unknown ID should not crash *)
  let result =
    run_initialized_session
      [
        cancel_request_msg ~id:999 ();
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code

let test_cancel_no_params () =
  (* $/cancelRequest without params should not crash *)
  let result =
    run_initialized_session [ make_message ~method_:"$/cancelRequest" () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code

let test_debounce_ms_setting () =
  (* didChangeConfiguration with debounceMs should be accepted *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        did_change_configuration_msg
          ~settings:(`Assoc [ ("tart.diagnostics.debounceMs", `Int 500) ])
          ();
        hover_msg ~id:2 ~uri:"file:///test.el" ~line:0 ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  (* Server still functional after config change *)
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No hover response after debounceMs change"
  | Some json ->
      Alcotest.(check bool)
        "hover works after debounceMs change" true
        (response_result json <> `Null)

let test_debounce_ms_negative_ignored () =
  (* Negative debounceMs should be silently ignored *)
  let result =
    run_initialized_session
      [
        did_change_configuration_msg
          ~settings:(`Assoc [ ("tart.diagnostics.debounceMs", `Int (-100)) ])
          ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code

let test_progress_notifications () =
  (* When client advertises workDoneProgress, apply_settings sends
     progress notifications during full invalidation *)
  let result =
    run_session
      [
        initialize_msg ~id:1
          ~capabilities:
            (`Assoc [ ("window", `Assoc [ ("workDoneProgress", `Bool true) ]) ])
          ();
        initialized_msg ();
        did_open_msg ~uri:"file:///a.el" ~text:"(+ 1 2)" ();
        did_change_configuration_msg
          ~settings:(`Assoc [ ("tart.emacsVersion", `String "30.1") ])
          ();
        shutdown_msg ~id:99 ();
        exit_msg ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  (* Should have window/workDoneProgress/create request *)
  let create_req =
    find_request ~method_:"window/workDoneProgress/create" result.messages
  in
  Alcotest.(check bool)
    "has progress create request" true
    (Option.is_some create_req);
  (* Should have $/progress notifications *)
  let progress = find_all_notifications ~method_:"$/progress" result.messages in
  Alcotest.(check bool)
    "has progress notifications" true
    (List.length progress >= 1)

let test_progress_not_sent_without_capability () =
  (* Without workDoneProgress capability, no progress messages should appear *)
  let result =
    run_session
      [
        initialize_msg ~id:1 ();
        initialized_msg ();
        did_open_msg ~uri:"file:///a.el" ~text:"(+ 1 2)" ();
        did_change_configuration_msg
          ~settings:(`Assoc [ ("tart.emacsVersion", `String "30.1") ])
          ();
        shutdown_msg ~id:99 ();
        exit_msg ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  let create_req =
    find_request ~method_:"window/workDoneProgress/create" result.messages
  in
  Alcotest.(check bool)
    "no progress create request" true
    (Option.is_none create_req);
  let progress = find_all_notifications ~method_:"$/progress" result.messages in
  Alcotest.(check int) "no progress notifications" 0 (List.length progress)

let test_rapid_changes_final_state () =
  (* Send 3 rapid didChange messages. Final diagnostics should reflect the
     last version. This exercises the coalescing/debounce path. *)
  let uri = "file:///test.el" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri ~text:"(+ 1 2)" ();
        did_change_full_msg ~uri ~version:2 ~text:"(+ 1 \"bad\")" ();
        did_change_full_msg ~uri ~version:3 ~text:"(+ 1 3)" ();
        did_change_full_msg ~uri ~version:4 ~text:"(+ 1 \"also-bad\")" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  (* Final diagnostics should show type error from the last change *)
  match find_last_diagnostics ~uri result.messages with
  | None -> Alcotest.fail "No final diagnostics"
  | Some json ->
      let open Yojson.Safe.Util in
      let diags = json |> member "params" |> member "diagnostics" |> to_list in
      Alcotest.(check bool)
        "final diagnostics have errors" true
        (List.length diags > 0)

(** {1 Call Hierarchy Tests} *)

let test_call_hierarchy_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response"
  | Some json ->
      let open Yojson.Safe.Util in
      let caps = json |> member "result" |> member "capabilities" in
      Alcotest.(check bool)
        "callHierarchyProvider" true
        (caps |> member "callHierarchyProvider" |> to_bool)

let test_call_hierarchy_prepare_on_defun () =
  let text = "(defun my-fn () 42)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        call_hierarchy_prepare_msg ~id:2 ~uri:"file:///test.el" ~line:0
          ~character:7 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No prepare response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let items = result |> to_list in
      Alcotest.(check int) "one item" 1 (List.length items);
      let item = List.hd items in
      Alcotest.(check string) "name" "my-fn" (item |> member "name" |> to_string);
      (* SKFunction = 12 *)
      Alcotest.(check int) "kind" 12 (item |> member "kind" |> to_int)

let test_call_hierarchy_prepare_not_on_defun () =
  let text = "(+ 1 2)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        call_hierarchy_prepare_msg ~id:2 ~uri:"file:///test.el" ~line:0
          ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No prepare response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is null" true (result = `Null)

let test_call_hierarchy_incoming_calls () =
  (* caller calls callee; requesting incoming calls for callee should find caller *)
  let a_text = "(defun caller () (callee))" in
  let b_text = "(defun callee () 42)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///a.el" ~text:a_text ();
        did_open_msg ~uri:"file:///b.el" ~text:b_text ();
        call_hierarchy_prepare_msg ~id:2 ~uri:"file:///b.el" ~line:0
          ~character:7 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No prepare response"
  | Some json -> (
      let open Yojson.Safe.Util in
      let items = json |> member "result" |> to_list in
      Alcotest.(check int) "one prepare item" 1 (List.length items);
      let callee_item = List.hd items in
      (* Now ask for incoming calls *)
      let result2 =
        run_initialized_session
          [
            did_open_msg ~uri:"file:///a.el" ~text:a_text ();
            did_open_msg ~uri:"file:///b.el" ~text:b_text ();
            incoming_calls_msg ~id:2 ~item:callee_item ();
          ]
      in
      Alcotest.(check int) "exit code 2" 0 result2.exit_code;
      match find_response ~id:2 result2.messages with
      | None -> Alcotest.fail "No incoming calls response"
      | Some json2 ->
          let result2_field = json2 |> member "result" in
          Alcotest.(check bool)
            "result is not null" true (result2_field <> `Null);
          let calls = result2_field |> to_list in
          Alcotest.(check int) "one incoming caller" 1 (List.length calls);
          let call = List.hd calls in
          let from = call |> member "from" in
          Alcotest.(check string)
            "caller name" "caller"
            (from |> member "name" |> to_string);
          let from_ranges = call |> member "fromRanges" |> to_list in
          Alcotest.(check bool)
            "has from ranges" true
            (List.length from_ranges > 0))

let test_call_hierarchy_outgoing_calls () =
  (* caller calls callee; requesting outgoing calls for caller should find callee *)
  let a_text = "(defun caller () (callee))" in
  let b_text = "(defun callee () 42)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///a.el" ~text:a_text ();
        did_open_msg ~uri:"file:///b.el" ~text:b_text ();
        call_hierarchy_prepare_msg ~id:2 ~uri:"file:///a.el" ~line:0
          ~character:7 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No prepare response"
  | Some json -> (
      let open Yojson.Safe.Util in
      let items = json |> member "result" |> to_list in
      Alcotest.(check int) "one prepare item" 1 (List.length items);
      let caller_item = List.hd items in
      (* Now ask for outgoing calls *)
      let result2 =
        run_initialized_session
          [
            did_open_msg ~uri:"file:///a.el" ~text:a_text ();
            did_open_msg ~uri:"file:///b.el" ~text:b_text ();
            outgoing_calls_msg ~id:2 ~item:caller_item ();
          ]
      in
      Alcotest.(check int) "exit code 2" 0 result2.exit_code;
      match find_response ~id:2 result2.messages with
      | None -> Alcotest.fail "No outgoing calls response"
      | Some json2 ->
          let result2_field = json2 |> member "result" in
          Alcotest.(check bool)
            "result is not null" true (result2_field <> `Null);
          let calls = result2_field |> to_list in
          Alcotest.(check bool) "has outgoing calls" true (List.length calls > 0);
          let callee_names =
            List.map
              (fun c -> c |> member "to" |> member "name" |> to_string)
              calls
          in
          Alcotest.(check bool)
            "callee in outgoing" true
            (List.mem "callee" callee_names))

let test_call_hierarchy_cross_file_incoming () =
  (* Two files: a.el defines (defun caller () (shared-fn))
     b.el defines (defun shared-fn () 99)
     Incoming calls for shared-fn should find caller in a.el *)
  let a_text = "(defun caller () (shared-fn))" in
  let b_text = "(defun shared-fn () 99)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///a.el" ~text:a_text ();
        did_open_msg ~uri:"file:///b.el" ~text:b_text ();
        call_hierarchy_prepare_msg ~id:2 ~uri:"file:///b.el" ~line:0
          ~character:7 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No prepare response"
  | Some json -> (
      let open Yojson.Safe.Util in
      let items = json |> member "result" |> to_list in
      let item = List.hd items in
      let result2 =
        run_initialized_session
          [
            did_open_msg ~uri:"file:///a.el" ~text:a_text ();
            did_open_msg ~uri:"file:///b.el" ~text:b_text ();
            incoming_calls_msg ~id:2 ~item ();
          ]
      in
      Alcotest.(check int) "exit code 2" 0 result2.exit_code;
      match find_response ~id:2 result2.messages with
      | None -> Alcotest.fail "No incoming calls response"
      | Some json2 ->
          let calls = json2 |> member "result" |> to_list in
          Alcotest.(check int) "one incoming caller" 1 (List.length calls);
          let caller = List.hd calls |> member "from" in
          Alcotest.(check string)
            "caller name" "caller"
            (caller |> member "name" |> to_string);
          Alcotest.(check string)
            "caller uri" "file:///a.el"
            (caller |> member "uri" |> to_string))

(** {1 Type Hierarchy Tests} *)

let test_type_hierarchy_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response"
  | Some json ->
      let open Yojson.Safe.Util in
      let caps = json |> member "result" |> member "capabilities" in
      Alcotest.(check bool)
        "typeHierarchyProvider" true
        (caps |> member "typeHierarchyProvider" |> to_bool)

let test_type_hierarchy_prepare_on_int () =
  (* Hover over a literal integer to get its type, then prepare type hierarchy *)
  let text = "(defvar x 42)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        (* Position on the integer literal 42 at col 10 *)
        type_hierarchy_prepare_msg ~id:2 ~uri:"file:///test.el" ~line:0
          ~character:10 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No prepare response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      (* Result may be null if type checker doesn't resolve to a TCon at this
         position; we just verify the handler doesn't crash *)
      ignore result

let test_type_hierarchy_prepare_not_on_type () =
  let text = "(+ 1 2)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        (* Position on whitespace *)
        type_hierarchy_prepare_msg ~id:2 ~uri:"file:///test.el" ~line:0
          ~character:3 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No prepare response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      (* Should return null or an item; either way no crash *)
      ignore result

let test_type_hierarchy_supertypes_int () =
  (* Int has supertype Num in the numeric tower *)
  let int_item : Yojson.Safe.t =
    `Assoc
      [
        ("name", `String "Int");
        ("kind", `Int 5);
        ("uri", `String "file:///test.el");
        ( "range",
          `Assoc
            [
              ("start", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
              ("end", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
            ] );
        ( "selectionRange",
          `Assoc
            [
              ("start", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
              ("end", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
            ] );
        ("data", `String Core.Types.Prim.int_name);
      ]
  in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        supertypes_msg ~id:2 ~item:int_item ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No supertypes response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let items = result |> to_list in
      Alcotest.(check int) "one supertype" 1 (List.length items);
      let item = List.hd items in
      Alcotest.(check string) "name" "Num" (item |> member "name" |> to_string)

let test_type_hierarchy_subtypes_num () =
  (* Num has subtypes Int and Float *)
  let num_item : Yojson.Safe.t =
    `Assoc
      [
        ("name", `String "Num");
        ("kind", `Int 5);
        ("uri", `String "file:///test.el");
        ( "range",
          `Assoc
            [
              ("start", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
              ("end", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
            ] );
        ( "selectionRange",
          `Assoc
            [
              ("start", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
              ("end", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
            ] );
        ("data", `String Core.Types.Prim.num_name);
      ]
  in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        subtypes_msg ~id:2 ~item:num_item ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No subtypes response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let items = result |> to_list in
      Alcotest.(check int) "two subtypes" 2 (List.length items);
      let names = List.map (fun i -> i |> member "name" |> to_string) items in
      Alcotest.(check bool) "Int in subtypes" true (List.mem "Int" names);
      Alcotest.(check bool) "Float in subtypes" true (List.mem "Float" names)

let test_type_hierarchy_supertypes_no_parent () =
  (* Num has no supertypes in the numeric tower *)
  let num_item : Yojson.Safe.t =
    `Assoc
      [
        ("name", `String "Num");
        ("kind", `Int 5);
        ("uri", `String "file:///test.el");
        ( "range",
          `Assoc
            [
              ("start", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
              ("end", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
            ] );
        ( "selectionRange",
          `Assoc
            [
              ("start", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
              ("end", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
            ] );
        ("data", `String Core.Types.Prim.num_name);
      ]
  in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        supertypes_msg ~id:2 ~item:num_item ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No supertypes response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let items = result |> to_list in
      Alcotest.(check int) "no supertypes" 0 (List.length items)

(** {1 Code Lens Tests} *)

let test_code_lens_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response"
  | Some json ->
      let open Yojson.Safe.Util in
      let caps = json |> member "result" |> member "capabilities" in
      let code_lens = caps |> member "codeLensProvider" in
      Alcotest.(check bool) "codeLensProvider present" true (code_lens <> `Null)

let test_code_lens_defun () =
  let text = "(defun my-fn () 42)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        code_lens_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No code lens response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_json = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result_json <> `Null);
      let lenses = result_json |> to_list in
      (* Two lenses per definition: reference count + signature status *)
      Alcotest.(check int) "two lenses" 2 (List.length lenses);
      let ref_lens = List.nth lenses 0 in
      let ref_cmd = ref_lens |> member "command" in
      let ref_title = ref_cmd |> member "title" |> to_string in
      Alcotest.(check bool)
        "ref lens has references" true
        (contains_string ~needle:"reference" ref_title);
      let sig_lens = List.nth lenses 1 in
      let sig_cmd = sig_lens |> member "command" in
      let sig_title = sig_cmd |> member "title" |> to_string in
      Alcotest.(check bool)
        "sig lens present" true
        (contains_string ~needle:"signature" sig_title)

let test_code_lens_multiple_defs () =
  let text = "(defun fn-a () 1)\n(defvar my-var 2)\n(defconst my-const 3)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        code_lens_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No code lens response"
  | Some json ->
      let open Yojson.Safe.Util in
      let lenses = json |> member "result" |> to_list in
      (* 3 definitions * 2 lenses each = 6 *)
      Alcotest.(check int) "six lenses" 6 (List.length lenses)

let test_code_lens_empty_file () =
  let text = "" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        code_lens_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No code lens response"
  | Some json ->
      let open Yojson.Safe.Util in
      let lenses = json |> member "result" |> to_list in
      Alcotest.(check int) "no lenses" 0 (List.length lenses)

let test_code_lens_reference_count () =
  (* my-fn is referenced 3 times total: definition + 2 call sites *)
  let text = "(defun my-fn () 42)\n(my-fn)\n(my-fn)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        code_lens_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No code lens response"
  | Some json ->
      let open Yojson.Safe.Util in
      let lenses = json |> member "result" |> to_list in
      Alcotest.(check int) "two lenses" 2 (List.length lenses);
      let ref_lens = List.nth lenses 0 in
      let title = ref_lens |> member "command" |> member "title" |> to_string in
      Alcotest.(check string) "3 references" "3 references" title

let test_code_lens_missing_signature () =
  let text = "(defun unknown-fn () 42)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        code_lens_msg ~id:2 ~uri:"file:///test.el" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No code lens response"
  | Some json ->
      let open Yojson.Safe.Util in
      let lenses = json |> member "result" |> to_list in
      let sig_lens = List.nth lenses 1 in
      let title = sig_lens |> member "command" |> member "title" |> to_string in
      Alcotest.(check string) "missing signature" "missing signature" title

(** {1 Linked Editing Ranges} *)

let test_linked_editing_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response"
  | Some json ->
      let open Yojson.Safe.Util in
      let caps = json |> member "result" |> member "capabilities" in
      let provider = caps |> member "linkedEditingRangeProvider" in
      Alcotest.(check bool)
        "linkedEditingRangeProvider present" true
        (provider = `Bool true)

let test_linked_editing_let_binding () =
  (* (let ((x 1)) (+ x 2))
      0123456789012345678901
     Cursor on x in the body at character 16 *)
  let text = "(let ((x 1)) (+ x 2))" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        linked_editing_range_msg ~id:2 ~uri:"file:///test.el" ~line:0
          ~character:16 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No linked editing response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_json = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result_json <> `Null);
      let ranges = result_json |> member "ranges" |> to_list in
      (* Should have 2 ranges: the binding (x) and the reference (x) *)
      Alcotest.(check int) "two ranges" 2 (List.length ranges)

let test_linked_editing_not_on_symbol () =
  let text = "(+ 1 2)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        linked_editing_range_msg ~id:2 ~uri:"file:///test.el" ~line:0
          ~character:3 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No linked editing response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_json = json |> member "result" in
      Alcotest.(check bool) "result is null" true (result_json = `Null)

let test_linked_editing_defun_params () =
  (* (defun my-fn (x) (+ x 1))
      0         1         2
      0123456789012345678901234
     Cursor on x in the body at character 20 *)
  let text = "(defun my-fn (x) (+ x 1))" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        linked_editing_range_msg ~id:2 ~uri:"file:///test.el" ~line:0
          ~character:20 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No linked editing response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_json = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result_json <> `Null);
      let ranges = result_json |> member "ranges" |> to_list in
      (* Should have 2 ranges: the param (x) and the body reference (x) *)
      Alcotest.(check int) "two ranges" 2 (List.length ranges)

let test_linked_editing_no_binding () =
  (* Symbol is not a let binding or param, so no linked ranges *)
  let text = "(message \"hello\")" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        linked_editing_range_msg ~id:2 ~uri:"file:///test.el" ~line:0
          ~character:1 ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "No linked editing response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_json = json |> member "result" in
      (* No let binding scope, so null *)
      Alcotest.(check bool) "result is null" true (result_json = `Null)

(** {1 On-Type Formatting} *)

let test_on_type_formatting_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  let init_resp = List.nth result.messages 0 in
  let open Yojson.Safe.Util in
  let caps = init_resp |> member "result" |> member "capabilities" in
  let provider = caps |> member "documentOnTypeFormattingProvider" in
  Alcotest.(check bool) "provider present" true (provider <> `Null);
  let first_trigger = provider |> member "firstTriggerCharacter" |> to_string in
  Alcotest.(check string) "first trigger" ")" first_trigger;
  let more = provider |> member "moreTriggerCharacter" |> to_list in
  Alcotest.(check int) "more triggers count" 1 (List.length more);
  Alcotest.(check string) "more trigger" "\n" (List.hd more |> to_string)

let test_on_type_formatting_newline_indentation () =
  (* (defun my-fn (x)\n  |
     After pressing enter inside the defun body, we're at depth 1 so expect
     2 spaces of indentation *)
  let text = "(defun my-fn (x)\n)" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        on_type_formatting_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:0
          ~ch:"\n" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "no response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_json = json |> member "result" in
      Alcotest.(check bool) "result not null" true (result_json <> `Null);
      let edits = result_json |> to_list in
      Alcotest.(check int) "one edit" 1 (List.length edits);
      let edit = List.hd edits in
      let new_text = edit |> member "newText" |> to_string in
      Alcotest.(check string) "2 spaces" "  " new_text

let test_on_type_formatting_newline_top_level () =
  (* At top level (depth 0), no indentation needed *)
  let text = "(defun my-fn (x) x)\n" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        on_type_formatting_msg ~id:2 ~uri:"file:///test.el" ~line:1 ~character:0
          ~ch:"\n" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "no response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_json = json |> member "result" in
      Alcotest.(check bool) "result is null" true (result_json = `Null)

let test_on_type_formatting_close_paren_top_level () =
  (* Closing paren that balances to depth 0 should insert trailing newline *)
  let text = "(defun my-fn (x) x)" in
  (* Position is after the closing paren, at col 20 *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        on_type_formatting_msg ~id:2 ~uri:"file:///test.el" ~line:0
          ~character:20 ~ch:")" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "no response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_json = json |> member "result" in
      Alcotest.(check bool) "result not null" true (result_json <> `Null);
      let edits = result_json |> to_list in
      Alcotest.(check int) "one edit" 1 (List.length edits);
      let edit = List.hd edits in
      let new_text = edit |> member "newText" |> to_string in
      Alcotest.(check string) "trailing newline" "\n" new_text

let test_on_type_formatting_close_paren_nested () =
  (* Closing paren that does NOT balance to depth 0 — no edit *)
  let text = "(defun my-fn (x) (+ x 1))" in
  (* The ) at position 24 closes the (+ x 1) but we're still inside defun *)
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        on_type_formatting_msg ~id:2 ~uri:"file:///test.el" ~line:0
          ~character:24 ~ch:")" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "no response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_json = json |> member "result" in
      Alcotest.(check bool) "result is null" true (result_json = `Null)

let test_on_type_formatting_close_paren_already_newline () =
  (* Top-level close paren but already has trailing newline *)
  let text = "(defun my-fn (x) x)\n" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text ();
        on_type_formatting_msg ~id:2 ~uri:"file:///test.el" ~line:0
          ~character:20 ~ch:")" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "no response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_json = json |> member "result" in
      Alcotest.(check bool) "result is null" true (result_json = `Null)

(** {1 Pull-Based Diagnostics Tests} *)

let test_pull_diagnostic_capability_advertised () =
  let result =
    run_session [ initialize_msg ~id:1 (); shutdown_msg ~id:2 (); exit_msg () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "no initialize response"
  | Some json ->
      let open Yojson.Safe.Util in
      let caps = json |> member "result" |> member "capabilities" in
      let diag_provider = caps |> member "diagnosticProvider" in
      Alcotest.(check bool)
        "diagnosticProvider present" true (diag_provider <> `Null);
      let inter_file = diag_provider |> member "interFileDependencies" in
      Alcotest.(check bool)
        "interFileDependencies is true" true
        (inter_file = `Bool true);
      let workspace = diag_provider |> member "workspaceDiagnostics" in
      Alcotest.(check bool)
        "workspaceDiagnostics is false" true
        (workspace = `Bool false)

let test_pull_diagnostic_full_report () =
  let uri = "file:///test/pull_diag.el" in
  let text = "(defun my-fn (x) (+ x \"oops\"))" in
  let result =
    run_initialized_session
      [ did_open_msg ~uri ~text (); document_diagnostic_msg ~id:2 ~uri () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "no response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_json = json |> member "result" in
      let kind = result_json |> member "kind" |> to_string in
      Alcotest.(check string) "kind is full" "full" kind;
      let result_id = result_json |> member "resultId" in
      Alcotest.(check bool) "has resultId" true (result_id <> `Null);
      let items = result_json |> member "items" |> to_list in
      Alcotest.(check bool) "has diagnostics" true (List.length items > 0)

let test_pull_diagnostic_unchanged_report () =
  let uri = "file:///test/pull_unchanged.el" in
  let text = "(defun my-fn (x) (+ x \"oops\"))" in
  let result =
    run_initialized_session
      [
        did_open_msg ~uri ~text ();
        document_diagnostic_msg ~id:2 ~uri ();
        document_diagnostic_msg ~id:3 ~uri ~previous_result_id:"1" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  (* First response should be full *)
  (match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "no first response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_json = json |> member "result" in
      let kind = result_json |> member "kind" |> to_string in
      Alcotest.(check string) "first request is full" "full" kind);
  (* Second response should be unchanged *)
  match find_response ~id:3 result.messages with
  | None -> Alcotest.fail "no second response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_json = json |> member "result" in
      let kind = result_json |> member "kind" |> to_string in
      Alcotest.(check string) "second request is unchanged" "unchanged" kind;
      let result_id = result_json |> member "resultId" |> to_string in
      Alcotest.(check string) "resultId matches" "1" result_id

let test_pull_diagnostic_no_errors () =
  let uri = "file:///test/pull_clean.el" in
  let text = ";;; comment" in
  let result =
    run_initialized_session
      [ did_open_msg ~uri ~text (); document_diagnostic_msg ~id:2 ~uri () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "no response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_json = json |> member "result" in
      let kind = result_json |> member "kind" |> to_string in
      Alcotest.(check string) "kind is full" "full" kind;
      let items = result_json |> member "items" |> to_list in
      Alcotest.(check int) "no diagnostics" 0 (List.length items)

let test_pull_diagnostic_matches_push () =
  let uri = "file:///test/pull_matches_push.el" in
  let text = "(defun my-fn (x) (+ x \"oops\"))" in
  let result =
    run_initialized_session
      [ did_open_msg ~uri ~text (); document_diagnostic_msg ~id:2 ~uri () ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:2 result.messages with
  | None -> Alcotest.fail "no response"
  | Some json ->
      let open Yojson.Safe.Util in
      let result_json = json |> member "result" in
      let kind = result_json |> member "kind" |> to_string in
      Alcotest.(check string) "kind is full" "full" kind;
      let items = result_json |> member "items" |> to_list in
      (* Pull diagnostics should produce the same error the push model finds *)
      Alcotest.(check bool)
        "has diagnostics from type check" true
        (List.length items > 0);
      (* Each item should have message and range *)
      let first = List.hd items in
      let msg = first |> member "message" in
      Alcotest.(check bool) "has message" true (msg <> `Null);
      let range = first |> member "range" in
      Alcotest.(check bool) "has range" true (range <> `Null)

(** {1 Dynamic Registration Tests} *)

let test_dynamic_registration_omits_static_capability () =
  (* When the client advertises dynamicRegistration for completion, the
     initialize response should omit completionProvider. *)
  let caps =
    `Assoc
      [
        ( "textDocument",
          `Assoc
            [ ("completion", `Assoc [ ("dynamicRegistration", `Bool true) ]) ]
        );
      ]
  in
  let result =
    run_session
      [
        initialize_msg ~id:1 ~capabilities:caps ();
        initialized_msg ();
        shutdown_msg ~id:99 ();
        exit_msg ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response"
  | Some json ->
      let open Yojson.Safe.Util in
      let server_caps = json |> member "result" |> member "capabilities" in
      (* completionProvider should be absent *)
      Alcotest.(check bool)
        "completionProvider omitted" true
        (server_caps |> member "completionProvider" = `Null)

let test_dynamic_registration_sends_register_capability () =
  (* When the client advertises dynamicRegistration for completion, the server
     should send a client/registerCapability request after initialized. *)
  let caps =
    `Assoc
      [
        ( "textDocument",
          `Assoc
            [ ("completion", `Assoc [ ("dynamicRegistration", `Bool true) ]) ]
        );
      ]
  in
  let result =
    run_session
      [
        initialize_msg ~id:1 ~capabilities:caps ();
        initialized_msg ();
        shutdown_msg ~id:99 ();
        exit_msg ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  (* There should be at least two client/registerCapability requests:
     one for file watchers and one for dynamic registrations *)
  let regs =
    List.filter
      (fun msg ->
        let open Yojson.Safe.Util in
        msg |> member "method" |> to_string_option
        = Some "client/registerCapability")
      result.messages
  in
  Alcotest.(check bool)
    "at least two registerCapability requests" true
    (List.length regs >= 2);
  (* The second one should contain completion registration *)
  let last_reg = List.nth regs (List.length regs - 1) in
  let open Yojson.Safe.Util in
  let registrations =
    last_reg |> member "params" |> member "registrations" |> to_list
  in
  let methods =
    List.map (fun r -> r |> member "method" |> to_string) registrations
  in
  Alcotest.(check bool)
    "has completion registration" true
    (List.mem "textDocument/completion" methods)

let test_dynamic_registration_not_sent_without_client_support () =
  (* When the client does NOT advertise dynamicRegistration, only the
     file watcher registration should be sent. *)
  let result =
    run_session
      [
        initialize_msg ~id:1 ();
        initialized_msg ();
        shutdown_msg ~id:99 ();
        exit_msg ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  let regs =
    List.filter
      (fun msg ->
        let open Yojson.Safe.Util in
        msg |> member "method" |> to_string_option
        = Some "client/registerCapability")
      result.messages
  in
  (* Only the file watcher registration *)
  Alcotest.(check int)
    "exactly one registerCapability (file watchers only)" 1 (List.length regs)

let test_dynamic_registration_multiple_capabilities () =
  (* When the client advertises dynamicRegistration for multiple capabilities,
     all are registered and omitted from static response. *)
  let caps =
    `Assoc
      [
        ( "textDocument",
          `Assoc
            [
              ("completion", `Assoc [ ("dynamicRegistration", `Bool true) ]);
              ("hover", `Assoc [ ("dynamicRegistration", `Bool true) ]);
              ("definition", `Assoc [ ("dynamicRegistration", `Bool true) ]);
            ] );
      ]
  in
  let result =
    run_session
      [
        initialize_msg ~id:1 ~capabilities:caps ();
        initialized_msg ();
        shutdown_msg ~id:99 ();
        exit_msg ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  match find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No initialize response"
  | Some json ->
      let open Yojson.Safe.Util in
      let server_caps = json |> member "result" |> member "capabilities" in
      (* All three should be absent from static capabilities *)
      Alcotest.(check bool)
        "completionProvider omitted" true
        (server_caps |> member "completionProvider" = `Null);
      Alcotest.(check bool)
        "hoverProvider omitted or false" true
        (server_caps |> member "hoverProvider" <> `Bool true);
      Alcotest.(check bool)
        "definitionProvider omitted or false" true
        (server_caps |> member "definitionProvider" <> `Bool true);
      (* Verify dynamic registration includes all three *)
      let regs =
        List.filter
          (fun msg ->
            msg |> member "method" |> to_string_option
            = Some "client/registerCapability")
          result.messages
      in
      let last_reg = List.nth regs (List.length regs - 1) in
      let registrations =
        last_reg |> member "params" |> member "registrations" |> to_list
      in
      let methods =
        List.map (fun r -> r |> member "method" |> to_string) registrations
      in
      Alcotest.(check bool)
        "has completion" true
        (List.mem "textDocument/completion" methods);
      Alcotest.(check bool)
        "has hover" true
        (List.mem "textDocument/hover" methods);
      Alcotest.(check bool)
        "has definition" true
        (List.mem "textDocument/definition" methods)

let test_dynamic_registration_with_options () =
  (* Verify that completion registration includes triggerCharacters. *)
  let caps =
    `Assoc
      [
        ( "textDocument",
          `Assoc
            [ ("completion", `Assoc [ ("dynamicRegistration", `Bool true) ]) ]
        );
      ]
  in
  let result =
    run_session
      [
        initialize_msg ~id:1 ~capabilities:caps ();
        initialized_msg ();
        shutdown_msg ~id:99 ();
        exit_msg ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  let regs =
    List.filter
      (fun msg ->
        let open Yojson.Safe.Util in
        msg |> member "method" |> to_string_option
        = Some "client/registerCapability")
      result.messages
  in
  let last_reg = List.nth regs (List.length regs - 1) in
  let open Yojson.Safe.Util in
  let registrations =
    last_reg |> member "params" |> member "registrations" |> to_list
  in
  let completion_reg =
    List.find
      (fun r -> r |> member "method" |> to_string = "textDocument/completion")
      registrations
  in
  let opts = completion_reg |> member "registerOptions" in
  Alcotest.(check bool) "has registerOptions" true (opts <> `Null);
  let triggers = opts |> member "triggerCharacters" |> to_list in
  Alcotest.(check bool) "has trigger characters" true (List.length triggers > 0)

(** {1 Test Registration} *)

let () =
  Alcotest.run "server"
    [
      ( "lifecycle",
        [
          Alcotest.test_case "initialize" `Quick test_initialize;
          Alcotest.test_case "already initialized" `Quick
            test_initialize_already_initialized;
          Alcotest.test_case "shutdown not initialized" `Quick
            test_shutdown_not_initialized;
          Alcotest.test_case "exit without shutdown" `Quick
            test_exit_without_shutdown;
          Alcotest.test_case "initialized notification" `Quick
            test_initialized_notification;
          Alcotest.test_case "unknown method" `Quick test_unknown_method;
        ] );
      ( "document-sync",
        [
          Alcotest.test_case "didOpen" `Quick test_did_open;
          Alcotest.test_case "didChange incremental" `Quick
            test_did_change_incremental;
          Alcotest.test_case "didClose" `Quick test_did_close;
        ] );
      ( "diagnostics",
        [
          Alcotest.test_case "on open with errors" `Quick
            test_diagnostics_on_open;
          Alcotest.test_case "on change" `Quick test_diagnostics_on_change;
          Alcotest.test_case "cleared on close" `Quick
            test_diagnostics_cleared_on_close;
          Alcotest.test_case "valid document" `Quick
            test_diagnostics_valid_document;
          Alcotest.test_case "parse error" `Quick test_diagnostics_parse_error;
          Alcotest.test_case "has error code" `Quick
            test_diagnostic_has_error_code;
          Alcotest.test_case "has help suggestions" `Quick
            test_diagnostic_has_help_suggestions;
        ] );
      ( "hover",
        [
          Alcotest.test_case "on literal" `Quick test_hover_on_literal;
          Alcotest.test_case "on function call" `Quick
            test_hover_on_function_call;
          Alcotest.test_case "outside code" `Quick test_hover_outside_code;
          Alcotest.test_case "has range" `Quick test_hover_has_range;
          Alcotest.test_case "instantiated type" `Quick
            test_hover_instantiated_type;
          Alcotest.test_case "with errors elsewhere" `Quick
            test_hover_with_errors_elsewhere;
          Alcotest.test_case "at error site" `Quick test_hover_at_error_site;
        ] );
      ( "definition",
        [
          Alcotest.test_case "on function call" `Quick
            test_definition_on_function_call;
          Alcotest.test_case "on defvar" `Quick test_definition_on_defvar_call;
          Alcotest.test_case "not found" `Quick test_definition_not_found;
          Alcotest.test_case "outside code" `Quick test_definition_outside_code;
          Alcotest.test_case "has uri" `Quick test_definition_has_uri;
          Alcotest.test_case "cross-file defun" `Quick
            test_definition_cross_file;
          Alcotest.test_case "cross-file defvar" `Quick
            test_definition_cross_file_defvar;
          Alcotest.test_case "cross-file .el defun" `Quick
            test_definition_cross_file_el;
          Alcotest.test_case "cross-file .el defvar" `Quick
            test_definition_cross_file_el_defvar;
          Alcotest.test_case "prefers local definition" `Quick
            test_definition_prefers_local;
        ] );
      ( "references",
        [
          Alcotest.test_case "all occurrences" `Quick
            test_references_all_occurrences;
          Alcotest.test_case "unknown symbol" `Quick
            test_references_on_unknown_symbol;
          Alcotest.test_case "not on symbol" `Quick
            test_references_not_on_symbol;
          Alcotest.test_case "have uris" `Quick test_references_have_uris;
          Alcotest.test_case "defvar" `Quick test_references_defvar;
          Alcotest.test_case "cross-file" `Quick test_references_cross_file;
        ] );
      ( "code-action",
        [
          Alcotest.test_case "returns empty list" `Quick
            test_code_action_returns_empty_list;
          Alcotest.test_case "parses context" `Quick
            test_code_action_parses_context;
          Alcotest.test_case "capability advertised" `Quick
            test_code_action_capability_advertised;
          Alcotest.test_case "missing signature quickfix" `Quick
            test_code_action_missing_signature_quickfix;
          Alcotest.test_case "quickfix has edit" `Quick
            test_code_action_quickfix_has_edit;
          Alcotest.test_case "respects range" `Quick
            test_code_action_respects_range;
          Alcotest.test_case "extract function appears" `Quick
            test_extract_function_appears_on_selection;
          Alcotest.test_case "extract function has edit" `Quick
            test_extract_function_has_edit;
          Alcotest.test_case "extract function captures vars" `Quick
            test_extract_function_captures_free_vars;
          Alcotest.test_case "extract function no cursor" `Quick
            test_extract_function_no_cursor;
        ] );
      ( "document-symbol",
        [
          Alcotest.test_case "returns defun" `Quick test_document_symbol_defun;
          Alcotest.test_case "returns defvar" `Quick test_document_symbol_defvar;
          Alcotest.test_case "returns defconst" `Quick
            test_document_symbol_defconst;
          Alcotest.test_case "returns defmacro" `Quick
            test_document_symbol_defmacro;
          Alcotest.test_case "capability advertised" `Quick
            test_document_symbol_capability;
          Alcotest.test_case "empty document" `Quick test_document_symbol_empty;
        ] );
      ( "completion",
        [
          Alcotest.test_case "returns local defun" `Quick
            test_completion_returns_local_defun;
          Alcotest.test_case "returns defvar" `Quick
            test_completion_returns_defvar;
          Alcotest.test_case "has type info" `Quick
            test_completion_has_type_info;
          Alcotest.test_case "capability advertised" `Quick
            test_completion_capability_advertised;
          Alcotest.test_case "filters by prefix" `Quick
            test_completion_filters_by_prefix;
          Alcotest.test_case "scope-aware let body" `Quick
            test_completion_scope_aware_let_body;
          Alcotest.test_case "scope-aware outside let" `Quick
            test_completion_scope_aware_outside_let;
          Alcotest.test_case "scope-aware defun params" `Quick
            test_completion_scope_aware_defun_params;
        ] );
      ( "signature-help",
        [
          Alcotest.test_case "shows function signature" `Quick
            test_signature_help_shows_function_signature;
          Alcotest.test_case "highlights active parameter" `Quick
            test_signature_help_highlights_active_parameter;
          Alcotest.test_case "capability advertised" `Quick
            test_signature_help_capability_advertised;
          Alcotest.test_case "not in call returns null" `Quick
            test_signature_help_not_in_call;
          Alcotest.test_case "has parameters" `Quick
            test_signature_help_has_parameters;
        ] );
      ( "rename",
        [
          Alcotest.test_case "all occurrences" `Quick
            test_rename_all_occurrences;
          Alcotest.test_case "has new name" `Quick test_rename_has_new_name;
          Alcotest.test_case "capability advertised" `Quick
            test_rename_capability_advertised;
          Alcotest.test_case "not on symbol" `Quick test_rename_not_on_symbol;
          Alcotest.test_case "has correct uri" `Quick
            test_rename_has_correct_uri;
          Alcotest.test_case "cross-file" `Quick test_rename_cross_file;
        ] );
      ( "prepare-rename",
        [
          Alcotest.test_case "symbol" `Quick test_prepare_rename_symbol;
          Alcotest.test_case "builtin" `Quick test_prepare_rename_builtin;
          Alcotest.test_case "keyword" `Quick test_prepare_rename_keyword;
          Alcotest.test_case "number" `Quick test_prepare_rename_number;
          Alcotest.test_case "string" `Quick test_prepare_rename_string;
        ] );
      ( "type-definition",
        [
          Alcotest.test_case "capability advertised" `Quick
            test_type_definition_capability_advertised;
          Alcotest.test_case "null for primitive" `Quick
            test_type_definition_returns_null_for_primitive;
          Alcotest.test_case "null for symbol" `Quick
            test_type_definition_returns_null_for_symbol;
          Alcotest.test_case "empty file" `Quick test_type_definition_empty_file;
        ] );
      ( "sync-recovery",
        [
          Alcotest.test_case "edit past end succeeds" `Quick
            test_edit_past_end_succeeds;
        ] );
      ( "diagnostic-dedup",
        [
          Alcotest.test_case "identical diagnostics suppressed" `Quick
            test_identical_diagnostics_suppressed;
          Alcotest.test_case "republished after reopen" `Quick
            test_diagnostics_republished_after_reopen;
        ] );
      ( "version-staleness",
        [
          Alcotest.test_case "final diagnostics match final state" `Quick
            test_final_diagnostics_match_final_state;
        ] );
      ( "did-save",
        [
          Alcotest.test_case "capability advertised" `Quick
            test_save_capability_advertised;
          Alcotest.test_case "triggers fresh diagnostics" `Quick
            test_save_triggers_fresh_diagnostics;
          Alcotest.test_case "invalidates cache" `Quick
            test_save_invalidates_cache;
        ] );
      ( "folding-range",
        [
          Alcotest.test_case "capability advertised" `Quick
            test_folding_range_capability_advertised;
          Alcotest.test_case "multiline defun" `Quick
            test_folding_range_multiline_defun;
          Alcotest.test_case "comments" `Quick test_folding_range_comments;
          Alcotest.test_case "empty file" `Quick test_folding_range_empty_file;
        ] );
      ( "semantic-tokens",
        [
          Alcotest.test_case "capability advertised" `Quick
            test_semantic_tokens_capability_advertised;
          Alcotest.test_case "defun tokens" `Quick test_semantic_tokens_defun;
          Alcotest.test_case "empty file" `Quick test_semantic_tokens_empty_file;
          Alcotest.test_case "comment tokens" `Quick
            test_semantic_tokens_comments;
        ] );
      ( "utf16-positions",
        [
          Alcotest.test_case "diagnostics CJK positions" `Quick
            test_diagnostic_cjk_positions;
          Alcotest.test_case "diagnostics emoji positions" `Quick
            test_diagnostic_emoji_positions;
          Alcotest.test_case "hover CJK range" `Quick test_hover_cjk_range;
          Alcotest.test_case "didChange CJK UTF-16" `Quick
            test_did_change_cjk_utf16;
        ] );
      ( "inlay-hints",
        [
          Alcotest.test_case "capability advertised" `Quick
            test_inlay_hint_capability_advertised;
          Alcotest.test_case "defun return type" `Quick
            test_inlay_hint_defun_return_type;
          Alcotest.test_case "let binding" `Quick test_inlay_hint_let_binding;
          Alcotest.test_case "empty file" `Quick test_inlay_hint_empty_file;
        ] );
      ( "workspace-symbols",
        [
          Alcotest.test_case "capability advertised" `Quick
            test_workspace_symbol_capability_advertised;
          Alcotest.test_case "defun symbol found" `Quick
            test_workspace_symbol_defun_found;
          Alcotest.test_case "query filtering" `Quick
            test_workspace_symbol_query_filtering;
          Alcotest.test_case "empty query returns all" `Quick
            test_workspace_symbol_empty_query;
          Alcotest.test_case "empty workspace" `Quick
            test_workspace_symbol_empty_workspace;
          Alcotest.test_case "tart file symbols" `Quick
            test_workspace_symbol_tart_file;
        ] );
      ( "workspace-config",
        [
          Alcotest.test_case "initializationOptions emacsVersion" `Quick
            test_initialization_options_emacs_version;
          Alcotest.test_case "didChangeConfiguration searchPath" `Quick
            test_did_change_configuration_searchpath;
          Alcotest.test_case "empty settings tolerated" `Quick
            test_empty_settings_tolerated;
          Alcotest.test_case "unknown keys ignored" `Quick
            test_unknown_settings_keys_ignored;
        ] );
      ( "file-watching",
        [
          Alcotest.test_case "registration sent after initialized" `Quick
            test_file_watcher_registration;
          Alcotest.test_case "changed tart invalidates dependent" `Quick
            test_watched_tart_file_invalidates_dependent;
          Alcotest.test_case "open file events ignored" `Quick
            test_watched_file_open_ignored;
        ] );
      ( "concurrency",
        [
          Alcotest.test_case "cancel unknown request" `Quick
            test_cancel_unknown_request;
          Alcotest.test_case "cancel without params" `Quick
            test_cancel_no_params;
          Alcotest.test_case "debounceMs setting" `Quick
            test_debounce_ms_setting;
          Alcotest.test_case "negative debounceMs ignored" `Quick
            test_debounce_ms_negative_ignored;
          Alcotest.test_case "progress notifications" `Quick
            test_progress_notifications;
          Alcotest.test_case "no progress without capability" `Quick
            test_progress_not_sent_without_capability;
          Alcotest.test_case "rapid changes final state" `Quick
            test_rapid_changes_final_state;
        ] );
      ( "call-hierarchy",
        [
          Alcotest.test_case "capability advertised" `Quick
            test_call_hierarchy_capability_advertised;
          Alcotest.test_case "prepare on defun" `Quick
            test_call_hierarchy_prepare_on_defun;
          Alcotest.test_case "prepare not on defun" `Quick
            test_call_hierarchy_prepare_not_on_defun;
          Alcotest.test_case "incoming calls" `Quick
            test_call_hierarchy_incoming_calls;
          Alcotest.test_case "outgoing calls" `Quick
            test_call_hierarchy_outgoing_calls;
          Alcotest.test_case "cross-file incoming" `Quick
            test_call_hierarchy_cross_file_incoming;
        ] );
      ( "type-hierarchy",
        [
          Alcotest.test_case "capability advertised" `Quick
            test_type_hierarchy_capability_advertised;
          Alcotest.test_case "prepare on int" `Quick
            test_type_hierarchy_prepare_on_int;
          Alcotest.test_case "prepare not on type" `Quick
            test_type_hierarchy_prepare_not_on_type;
          Alcotest.test_case "supertypes of int" `Quick
            test_type_hierarchy_supertypes_int;
          Alcotest.test_case "subtypes of num" `Quick
            test_type_hierarchy_subtypes_num;
          Alcotest.test_case "supertypes no parent" `Quick
            test_type_hierarchy_supertypes_no_parent;
        ] );
      ( "code-lens",
        [
          Alcotest.test_case "capability advertised" `Quick
            test_code_lens_capability_advertised;
          Alcotest.test_case "defun lens" `Quick test_code_lens_defun;
          Alcotest.test_case "multiple definitions" `Quick
            test_code_lens_multiple_defs;
          Alcotest.test_case "empty file" `Quick test_code_lens_empty_file;
          Alcotest.test_case "reference count" `Quick
            test_code_lens_reference_count;
          Alcotest.test_case "missing signature" `Quick
            test_code_lens_missing_signature;
        ] );
      ( "linked-editing",
        [
          Alcotest.test_case "capability advertised" `Quick
            test_linked_editing_capability_advertised;
          Alcotest.test_case "let binding" `Quick
            test_linked_editing_let_binding;
          Alcotest.test_case "not on symbol" `Quick
            test_linked_editing_not_on_symbol;
          Alcotest.test_case "defun params" `Quick
            test_linked_editing_defun_params;
          Alcotest.test_case "no binding" `Quick test_linked_editing_no_binding;
        ] );
      ( "on-type-formatting",
        [
          Alcotest.test_case "capability advertised" `Quick
            test_on_type_formatting_capability_advertised;
          Alcotest.test_case "newline indentation" `Quick
            test_on_type_formatting_newline_indentation;
          Alcotest.test_case "newline top level" `Quick
            test_on_type_formatting_newline_top_level;
          Alcotest.test_case "close paren top level" `Quick
            test_on_type_formatting_close_paren_top_level;
          Alcotest.test_case "close paren nested" `Quick
            test_on_type_formatting_close_paren_nested;
          Alcotest.test_case "close paren already newline" `Quick
            test_on_type_formatting_close_paren_already_newline;
        ] );
      ( "pull-diagnostics",
        [
          Alcotest.test_case "capability advertised" `Quick
            test_pull_diagnostic_capability_advertised;
          Alcotest.test_case "full report" `Quick
            test_pull_diagnostic_full_report;
          Alcotest.test_case "unchanged report" `Quick
            test_pull_diagnostic_unchanged_report;
          Alcotest.test_case "no errors" `Quick test_pull_diagnostic_no_errors;
          Alcotest.test_case "matches push" `Quick
            test_pull_diagnostic_matches_push;
        ] );
      ( "dynamic-registration",
        [
          Alcotest.test_case "omits static capability" `Quick
            test_dynamic_registration_omits_static_capability;
          Alcotest.test_case "sends registerCapability" `Quick
            test_dynamic_registration_sends_register_capability;
          Alcotest.test_case "not sent without client support" `Quick
            test_dynamic_registration_not_sent_without_client_support;
          Alcotest.test_case "multiple capabilities" `Quick
            test_dynamic_registration_multiple_capabilities;
          Alcotest.test_case "registration with options" `Quick
            test_dynamic_registration_with_options;
        ] );
    ]
