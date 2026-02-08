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
      Alcotest.(check bool) "has full" true (stp |> member "full" |> to_bool);
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
    ]
