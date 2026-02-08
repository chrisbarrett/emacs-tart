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
  let result =
    run_initialized_session
      [
        did_open_msg ~uri:"file:///test.el" ~text:"(+ 1 2)" ();
        did_change_full_msg ~uri:"file:///test.el" ~version:2
          ~text:"(+ 1 \"bad\")" ();
      ]
  in
  Alcotest.(check int) "exit code" 0 result.exit_code;
  let diag_notifications =
    find_all_notifications ~method_:"textDocument/publishDiagnostics"
      result.messages
  in
  Alcotest.(check bool)
    "has multiple diagnostics notifications" true
    (List.length diag_notifications >= 2)

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
        (String.length value > 0
        &&
          try
            let _ = Str.search_forward (Str.regexp_string "42") value 0 in
            true
          with Not_found -> false)

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
        (try
           let _ = Str.search_forward (Str.regexp_string "->") value 0 in
           true
         with Not_found -> false)

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
        (let has_int =
           try
             ignore (Str.search_forward (Str.regexp_string "int") value 0);
             true
           with Not_found -> false
         in
         let has_literal =
           try
             ignore (Str.search_forward (Str.regexp_string "1") value 0);
             true
           with Not_found -> false
         in
         has_int || has_literal)

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
        (try
           let _ = Str.search_forward (Str.regexp_string "42") value 0 in
           true
         with Not_found -> false)

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
            String.length msg > 0
            &&
              try
                let _ = Str.search_forward (Str.regexp_string "fop") msg 0 in
                true
              with Not_found -> false)
          diagnostics
      in
      match fop_diag with
      | None -> Alcotest.fail "No diagnostic for 'fop' found"
      | Some diag ->
          let message = diag |> member "message" |> to_string in
          let has_similar_name =
            try
              let _ =
                Str.search_forward (Str.regexp_string "similar name") message 0
              in
              true
            with Not_found -> false
          in
          Alcotest.(check bool) "message has similar name" true has_similar_name;
          let has_foo =
            try
              let _ = Str.search_forward (Str.regexp_string "foo") message 0 in
              true
            with Not_found -> false
          in
          Alcotest.(check bool) "suggests foo" true has_foo)

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
          (try
             let _ =
               Str.search_forward (Str.regexp_string ".tart") result_uri 0
             in
             true
           with Not_found -> false)

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
          (try
             let _ =
               Str.search_forward (Str.regexp_string ".tart") result_uri 0
             in
             true
           with Not_found -> false)

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
        (try
           let _ = Str.search_forward (Str.regexp_string "my-add") title 0 in
           true
         with Not_found -> false);
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
        (try
           let _ =
             Str.search_forward (Str.regexp_string ".tart") target_uri 0
           in
           true
         with Not_found -> false);
      let edits = doc_change |> member "edits" |> to_list in
      let text_edit = List.hd edits in
      let new_text = text_edit |> member "newText" |> to_string in
      Alcotest.(check bool)
        "new text contains defun" true
        (try
           let _ = Str.search_forward (Str.regexp_string "defun") new_text 0 in
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "new text contains function name" true
        (try
           let _ = Str.search_forward (Str.regexp_string "my-add") new_text 0 in
           true
         with Not_found -> false)

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
        (try
           let _ = Str.search_forward (Str.regexp_string "fn-two") title 0 in
           true
         with Not_found -> false)

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
            try
              let _ =
                Str.search_forward (Str.regexp_string "defun") new_text 0
              in
              true
            with Not_found -> false)
          edits
      in
      let new_text = insert_edit |> member "newText" |> to_string in
      Alcotest.(check bool)
        "has x param" true
        (try
           let _ = Str.search_forward (Str.regexp_string "(x y)") new_text 0 in
           true
         with Not_found -> false)

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
      Alcotest.(check bool)
        "renameProvider is true" true
        (rename_provider = `Bool true)

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
    ]
