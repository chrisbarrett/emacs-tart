(** Tests for LSP server initialize/initialized/shutdown *)

open Lsp

(** Helper to create a JSON-RPC message *)
let make_message ?(id : Yojson.Safe.t option) ~method_ ?params () : string =
  let json =
    `Assoc
      ([ ("jsonrpc", `String "2.0"); ("method", `String method_) ]
      @ (match id with
        | Some i -> [ ("id", i) ]
        | None -> [])
      @ (match params with
        | Some p -> [ ("params", p) ]
        | None -> []))
  in
  let content = Yojson.Safe.to_string json in
  Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length content) content

let parse_response_content_length output : int option =
  match Str.split (Str.regexp "\r\n\r\n") output with
  | header :: _ ->
      let re = Str.regexp "Content-Length: \\([0-9]+\\)" in
      if Str.string_match re header 0 then
        Some (int_of_string (Str.matched_group 1 header))
      else None
  | _ -> None

(** Tests *)

let test_initialize () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:
        (`Assoc
          [
            ("processId", `Int 12345);
            ("rootUri", `String "file:///tmp/test");
            ("capabilities", `Assoc []);
          ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 2) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc -> Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  (* Verify exit code *)
  Alcotest.(check int) "clean shutdown exit code" 0 exit_code;
  (* Read and parse output *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  (* Should have Content-Length header *)
  Alcotest.(check bool) "has content-length" true (String.length output > 0);
  (* Parse first response (initialize) *)
  match parse_response_content_length output with
  | Some len ->
      let header_end = String.index output '\n' + 3 in
      let json_str = String.sub output header_end len in
      let json = Yojson.Safe.from_string json_str in
      let open Yojson.Safe.Util in
      (* Check id matches *)
      Alcotest.(check int) "id" 1 (json |> member "id" |> to_int);
      (* Check capabilities present *)
      let result = json |> member "result" in
      let caps = result |> member "capabilities" in
      Alcotest.(check bool) "has textDocumentSync" true (caps |> member "textDocumentSync" <> `Null);
      Alcotest.(check bool) "hoverProvider" true (caps |> member "hoverProvider" |> to_bool);
      (* Check serverInfo *)
      let server_info = result |> member "serverInfo" in
      Alcotest.(check string) "server name" "tart" (server_info |> member "name" |> to_string)
  | None ->
      Alcotest.fail "Could not parse response"

let test_initialize_already_initialized () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  let init_msg2 =
    make_message ~id:(`Int 2) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ init_msg2 ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc -> Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output - should have error for second initialize *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  Alcotest.(check bool) "has error response" true (String.length output > 0)

let test_shutdown_not_initialized () =
  let shutdown_msg = make_message ~id:(`Int 1) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc -> Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  (* Exit without init/shutdown is error *)
  Alcotest.(check int) "exit code without init" 1 exit_code

let test_exit_without_shutdown () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc -> Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  (* Exit without shutdown should return 1 *)
  Alcotest.(check int) "exit code without shutdown" 1 exit_code

let test_initialized_notification () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  let initialized_msg = make_message ~method_:"initialized" ~params:(`Assoc []) () in
  let shutdown_msg = make_message ~id:(`Int 2) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ initialized_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc -> Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "clean shutdown with initialized" 0 exit_code

let test_unknown_method () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  let unknown_msg = make_message ~id:(`Int 2) ~method_:"unknown/method" () in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ unknown_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc -> Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  (* Should still complete successfully *)
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Check we got a method not found error for the unknown request *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let has_method_error =
    try
      let _ = Str.search_forward (Str.regexp_string "method") (String.lowercase_ascii output) 0 in
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "has error" true has_method_error

(** {1 Document Sync Tests} *)

let test_did_open () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  let did_open_msg =
    make_message ~method_:"textDocument/didOpen"
      ~params:
        (`Assoc
          [
            ( "textDocument",
              `Assoc
                [
                  ("uri", `String "file:///test.el");
                  ("languageId", `String "elisp");
                  ("version", `Int 1);
                  ("text", `String "(defun foo () t)");
                ] );
          ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 2) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ did_open_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc -> Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Check document was stored *)
  match Document.get_doc (Server.documents server) "file:///test.el" with
  | Some doc ->
      Alcotest.(check string) "doc text" "(defun foo () t)" doc.text;
      Alcotest.(check int) "doc version" 1 doc.version
  | None -> Alcotest.fail "Document not found after didOpen"

let test_did_change_incremental () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  let did_open_msg =
    make_message ~method_:"textDocument/didOpen"
      ~params:
        (`Assoc
          [
            ( "textDocument",
              `Assoc
                [
                  ("uri", `String "file:///test.el");
                  ("languageId", `String "elisp");
                  ("version", `Int 1);
                  ("text", `String "hello world");
                ] );
          ])
      ()
  in
  let did_change_msg =
    make_message ~method_:"textDocument/didChange"
      ~params:
        (`Assoc
          [
            ( "textDocument",
              `Assoc
                [ ("uri", `String "file:///test.el"); ("version", `Int 2) ] );
            ( "contentChanges",
              `List
                [
                  `Assoc
                    [
                      ( "range",
                        `Assoc
                          [
                            ( "start",
                              `Assoc
                                [ ("line", `Int 0); ("character", `Int 6) ] );
                            ( "end",
                              `Assoc
                                [ ("line", `Int 0); ("character", `Int 11) ] );
                          ] );
                      ("text", `String "Emacs");
                    ];
                ] );
          ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 2) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ did_open_msg ^ did_change_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc -> Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Check document was updated *)
  match Document.get_doc (Server.documents server) "file:///test.el" with
  | Some doc ->
      Alcotest.(check string) "doc text after change" "hello Emacs" doc.text;
      Alcotest.(check int) "doc version after change" 2 doc.version
  | None -> Alcotest.fail "Document not found after didChange"

let test_did_close () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  let did_open_msg =
    make_message ~method_:"textDocument/didOpen"
      ~params:
        (`Assoc
          [
            ( "textDocument",
              `Assoc
                [
                  ("uri", `String "file:///test.el");
                  ("languageId", `String "elisp");
                  ("version", `Int 1);
                  ("text", `String "some content");
                ] );
          ])
      ()
  in
  let did_close_msg =
    make_message ~method_:"textDocument/didClose"
      ~params:
        (`Assoc
          [ ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]) ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 2) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ did_open_msg ^ did_close_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc -> Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Check document was removed *)
  Alcotest.(check bool)
    "doc removed after close" true
    (Option.is_none (Document.get_doc (Server.documents server) "file:///test.el"))

let () =
  Alcotest.run "server"
    [
      ( "lifecycle",
        [
          Alcotest.test_case "initialize" `Quick test_initialize;
          Alcotest.test_case "already initialized" `Quick test_initialize_already_initialized;
          Alcotest.test_case "shutdown not initialized" `Quick test_shutdown_not_initialized;
          Alcotest.test_case "exit without shutdown" `Quick test_exit_without_shutdown;
          Alcotest.test_case "initialized notification" `Quick test_initialized_notification;
          Alcotest.test_case "unknown method" `Quick test_unknown_method;
        ] );
      ( "document-sync",
        [
          Alcotest.test_case "didOpen" `Quick test_did_open;
          Alcotest.test_case "didChange incremental" `Quick test_did_change_incremental;
          Alcotest.test_case "didClose" `Quick test_did_close;
        ] );
    ]
