(** Tests for LSP server initialize/initialized/shutdown *)

open Lsp

(** Helper to create a JSON-RPC message *)
let make_message ?(id : Yojson.Safe.t option) ~method_ ?params () : string =
  let json =
    `Assoc
      ([ ("jsonrpc", `String "2.0"); ("method", `String method_) ]
      @ (match id with Some i -> [ ("id", i) ] | None -> [])
      @ match params with Some p -> [ ("params", p) ] | None -> [])
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
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
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
      Alcotest.(check bool)
        "has textDocumentSync" true
        (caps |> member "textDocumentSync" <> `Null);
      Alcotest.(check bool)
        "hoverProvider" true
        (caps |> member "hoverProvider" |> to_bool);
      (* Check serverInfo *)
      let server_info = result |> member "serverInfo" in
      Alcotest.(check string)
        "server name" "tart"
        (server_info |> member "name" |> to_string)
  | None -> Alcotest.fail "Could not parse response"

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
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
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
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
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
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
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
  let initialized_msg =
    make_message ~method_:"initialized" ~params:(`Assoc []) ()
  in
  let shutdown_msg = make_message ~id:(`Int 2) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ initialized_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
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
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
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
      let _ =
        Str.search_forward
          (Str.regexp_string "method")
          (String.lowercase_ascii output)
          0
      in
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
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
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
  let input =
    init_msg ^ did_open_msg ^ did_change_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
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
  let input =
    init_msg ^ did_open_msg ^ did_close_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
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
    (Option.is_none
       (Document.get_doc (Server.documents server) "file:///test.el"))

(** {1 Diagnostics Tests} *)

(** Helper to parse all JSON-RPC messages from output *)
let parse_messages (output : string) : Yojson.Safe.t list =
  let rec parse_all offset acc =
    if offset >= String.length output then List.rev acc
    else
      (* Find Content-Length header *)
      let header_end =
        try String.index_from output offset '\r'
        with Not_found -> String.length output
      in
      if header_end >= String.length output then List.rev acc
      else
        let header = String.sub output offset (header_end - offset) in
        let re = Str.regexp "Content-Length: \\([0-9]+\\)" in
        if Str.string_match re header 0 then
          let content_length = int_of_string (Str.matched_group 1 header) in
          (* Skip past \r\n\r\n to content *)
          let content_start = header_end + 4 in
          if content_start + content_length <= String.length output then
            let content = String.sub output content_start content_length in
            let json = Yojson.Safe.from_string content in
            parse_all (content_start + content_length) (json :: acc)
          else List.rev acc
        else List.rev acc
  in
  parse_all 0 []

(** Helper to find a publishDiagnostics notification in messages *)
let find_publish_diagnostics (messages : Yojson.Safe.t list) (uri : string) :
    Yojson.Safe.t option =
  let open Yojson.Safe.Util in
  List.find_opt
    (fun json ->
      match json |> member "method" with
      | `String "textDocument/publishDiagnostics" ->
          let params = json |> member "params" in
          params |> member "uri" |> to_string = uri
      | _ -> false)
    messages

let test_diagnostics_on_open () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Open a document with a type error: adding string to int *)
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
                   ("text", `String "(+ 1 \"hello\")");
                 ] );
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 2) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ did_open_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find diagnostics notification *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_publish_diagnostics messages "file:///test.el" with
  | None -> Alcotest.fail "No publishDiagnostics notification found"
  | Some json ->
      let open Yojson.Safe.Util in
      let params = json |> member "params" in
      let diagnostics = params |> member "diagnostics" |> to_list in
      Alcotest.(check bool) "has diagnostics" true (List.length diagnostics > 0);
      (* Check the first diagnostic has expected fields *)
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
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Open a valid document first *)
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
                   ("text", `String "(+ 1 2)");
                 ] );
           ])
      ()
  in
  (* Change it to have an error *)
  let did_change_msg =
    make_message ~method_:"textDocument/didChange"
      ~params:
        (`Assoc
           [
             ( "textDocument",
               `Assoc
                 [ ("uri", `String "file:///test.el"); ("version", `Int 2) ] );
             ( "contentChanges",
               `List [ `Assoc [ ("text", `String "(+ 1 \"bad\")") ] ] );
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 2) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ did_change_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and look for diagnostics *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  (* We should have at least 2 diagnostics notifications (one for open, one for change) *)
  let diag_notifications =
    List.filter
      (fun json ->
        let open Yojson.Safe.Util in
        json |> member "method" = `String "textDocument/publishDiagnostics")
      messages
  in
  Alcotest.(check bool)
    "has multiple diagnostics notifications" true
    (List.length diag_notifications >= 2)

let test_diagnostics_cleared_on_close () =
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
                   ("text", `String "(+ 1 \"error\")");
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
  let input =
    init_msg ^ did_open_msg ^ did_close_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find the last diagnostics for this file (should be empty) *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  let diag_notifications =
    List.filter
      (fun json ->
        let open Yojson.Safe.Util in
        match json |> member "method" with
        | `String "textDocument/publishDiagnostics" ->
            json |> member "params" |> member "uri" |> to_string
            = "file:///test.el"
        | _ -> false)
      messages
  in
  (* The last notification should have empty diagnostics *)
  match List.rev diag_notifications with
  | [] -> Alcotest.fail "No diagnostics notifications found"
  | last :: _ ->
      let open Yojson.Safe.Util in
      let diagnostics =
        last |> member "params" |> member "diagnostics" |> to_list
      in
      Alcotest.(check int) "diagnostics cleared" 0 (List.length diagnostics)

let test_diagnostics_valid_document () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Open a valid document - should have no diagnostics *)
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
                   ("text", `String "(+ 1 2)");
                 ] );
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 2) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ did_open_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find diagnostics notification *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_publish_diagnostics messages "file:///test.el" with
  | None -> Alcotest.fail "No publishDiagnostics notification found"
  | Some json ->
      let open Yojson.Safe.Util in
      let params = json |> member "params" in
      let diagnostics = params |> member "diagnostics" |> to_list in
      Alcotest.(check int)
        "no diagnostics for valid code" 0 (List.length diagnostics)

let test_diagnostics_parse_error () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Open a document with a parse error: unclosed paren *)
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
                   ("text", `String "(defun foo (");
                 ] );
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 2) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ did_open_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find diagnostics notification *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_publish_diagnostics messages "file:///test.el" with
  | None -> Alcotest.fail "No publishDiagnostics notification found"
  | Some json ->
      let open Yojson.Safe.Util in
      let params = json |> member "params" in
      let diagnostics = params |> member "diagnostics" |> to_list in
      Alcotest.(check bool)
        "has parse error diagnostic" true
        (List.length diagnostics > 0)

(** {1 Hover Tests} *)

(** Helper to find a response with a given id *)
let find_response (messages : Yojson.Safe.t list) (id : int) :
    Yojson.Safe.t option =
  let open Yojson.Safe.Util in
  List.find_opt
    (fun json -> match json |> member "id" with `Int i -> i = id | _ -> false)
    messages

let test_hover_on_literal () =
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
                   ("text", `String "42");
                 ] );
           ])
      ()
  in
  (* Hover at position (0, 0) - should get int type *)
  let hover_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/hover"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ did_open_msg ^ hover_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find hover response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No hover response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let contents = result |> member "contents" in
      let value = contents |> member "value" |> to_string in
      (* Should contain "Int" type *)
      Alcotest.(check bool)
        "contains Int type" true
        (String.length value > 0
        &&
        try
          let _ = Str.search_forward (Str.regexp_string "Int") value 0 in
          true
        with Not_found -> false)

let test_hover_on_function_call () =
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
                   ("text", `String "(+ 1 2)");
                 ] );
           ])
      ()
  in
  (* Hover at position (0, 1) - on the + symbol *)
  let hover_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/hover"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 0); ("character", `Int 1) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ did_open_msg ^ hover_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find hover response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No hover response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let contents = result |> member "contents" in
      let value = contents |> member "value" |> to_string in
      (* Should have a function type with arrows *)
      Alcotest.(check bool)
        "contains arrow" true
        (try
           let _ = Str.search_forward (Str.regexp_string "->") value 0 in
           true
         with Not_found -> false)

let test_hover_outside_code () =
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
                   ("text", `String "42\n");
                 ] );
           ])
      ()
  in
  (* Hover at position (1, 0) - empty line, outside any code *)
  let hover_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/hover"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 1); ("character", `Int 0) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ did_open_msg ^ hover_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find hover response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No hover response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      (* Should return null when not hovering over code *)
      Alcotest.(check bool) "result is null" true (result = `Null)

let test_hover_has_range () =
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
                   (* Use a simpler literal for testing range *)
                   ("text", `String "42");
                 ] );
           ])
      ()
  in
  (* Hover on int literal at position 0 *)
  let hover_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/hover"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ did_open_msg ^ hover_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find hover response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No hover response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      (* Check that range is included *)
      let range = result |> member "range" in
      Alcotest.(check bool) "has range" true (range <> `Null);
      (* Check range has start and end *)
      let start = range |> member "start" in
      let end_ = range |> member "end" in
      Alcotest.(check bool) "has start" true (start <> `Null);
      Alcotest.(check bool) "has end" true (end_ <> `Null)

let test_hover_instantiated_type () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* cons is polymorphic: (forall (a) (-> (a (List a)) (List a)))
     When applied to 1 and nil, 'a should be instantiated to Int
     Result: (-> (Int (List Int)) (List Int)) *)
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
                   ("text", `String "(cons 1 nil)");
                 ] );
           ])
      ()
  in
  (* Hover at position (0, 1) - on the 'cons' symbol *)
  let hover_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/hover"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 0); ("character", `Int 1) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ did_open_msg ^ hover_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find hover response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No hover response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let contents = result |> member "contents" in
      let value = contents |> member "value" |> to_string in
      (* Should have Int in the type, not a type variable like '_0 *)
      Alcotest.(check bool)
        "contains Int (instantiated)" true
        (try
           let _ = Str.search_forward (Str.regexp_string "Int") value 0 in
           true
         with Not_found -> false);
      (* Should NOT have unresolved type variables like '_0 *)
      let has_tvar =
        try
          let _ = Str.search_forward (Str.regexp "'_[0-9]+") value 0 in
          true
        with Not_found -> false
      in
      Alcotest.(check bool) "no unresolved tvars" false has_tvar

(** Test hover still works on valid code when document has type errors elsewhere
*)
let test_hover_with_errors_elsewhere () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Document with a type error on line 2, but valid code on line 1 *)
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
                   (* Line 0: valid code (42)
                     Line 1: type error (+ 1 "hello") *)
                   ("text", `String "42\n(+ 1 \"hello\")");
                 ] );
           ])
      ()
  in
  (* Hover at position (0, 0) - on the valid literal 42 *)
  let hover_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/hover"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ did_open_msg ^ hover_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find hover response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No hover response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      (* Should still get a type for the valid code *)
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let contents = result |> member "contents" in
      let value = contents |> member "value" |> to_string in
      (* Should contain "Int" type for the literal 42 *)
      Alcotest.(check bool)
        "contains Int type" true
        (try
           let _ = Str.search_forward (Str.regexp_string "Int") value 0 in
           true
         with Not_found -> false)

(** Test hover on code at the error site itself still provides info *)
let test_hover_at_error_site () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Document with a type error: (+ 1 "hello") *)
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
                   ("text", `String "(+ 1 \"hello\")");
                 ] );
           ])
      ()
  in
  (* Hover at position (0, 1) - on the + function *)
  let hover_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/hover"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 0); ("character", `Int 1) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ did_open_msg ^ hover_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find hover response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No hover response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      (* Should still get some response - either type or null, but not an error *)
      Alcotest.(check bool)
        "response succeeded (not error)" true
        (json |> member "error" = `Null);
      (* If we get a result, it should have some type info *)
      if result <> `Null then
        let contents = result |> member "contents" in
        let value = contents |> member "value" |> to_string in
        (* Should have some content (best-effort type) *)
        Alcotest.(check bool) "has some content" true (String.length value > 0)

(** {1 Diagnostic Formatting Tests} *)

(** Test that diagnostics include error codes in LSP format *)
let test_diagnostic_has_error_code () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Type error: adding string to int *)
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
                   ("text", `String "(+ 1 \"hello\")");
                 ] );
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 2) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ did_open_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let _ = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_publish_diagnostics messages "file:///test.el" with
  | None -> Alcotest.fail "No publishDiagnostics notification found"
  | Some json ->
      let open Yojson.Safe.Util in
      let params = json |> member "params" in
      let diagnostics = params |> member "diagnostics" |> to_list in
      Alcotest.(check bool) "has diagnostics" true (List.length diagnostics > 0);
      let first = List.hd diagnostics in
      (* Check error code is present *)
      let code = first |> member "code" in
      Alcotest.(check bool) "has code" true (code <> `Null);
      Alcotest.(check string) "code is E0308" "E0308" (code |> to_string)

(** Test that diagnostics include help suggestions in message *)
let test_diagnostic_has_help_suggestions () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Type error: passing int to string function *)
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
                   ("text", `String "(upcase 42)");
                 ] );
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 2) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ did_open_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let _ = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_publish_diagnostics messages "file:///test.el" with
  | None -> Alcotest.fail "No publishDiagnostics notification found"
  | Some json ->
      let open Yojson.Safe.Util in
      let params = json |> member "params" in
      let diagnostics = params |> member "diagnostics" |> to_list in
      Alcotest.(check bool) "has diagnostics" true (List.length diagnostics > 0);
      let first = List.hd diagnostics in
      let message = first |> member "message" |> to_string in
      (* Check message includes help suggestion *)
      let has_help =
        try
          let _ = Str.search_forward (Str.regexp_string "help:") message 0 in
          true
        with Not_found -> false
      in
      Alcotest.(check bool) "message has help" true has_help;
      (* Check specific suggestion for int->string conversion *)
      let has_number_to_string =
        try
          let _ =
            Str.search_forward (Str.regexp_string "number-to-string") message 0
          in
          true
        with Not_found -> false
      in
      Alcotest.(check bool)
        "suggests number-to-string" true has_number_to_string

(** {1 Go to Definition Tests} *)

let test_definition_on_function_call () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Document with a defun and a call to it *)
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
                   ("text", `String "(defun foo () 42)\n(foo)");
                 ] );
           ])
      ()
  in
  (* Definition request at position (1, 1) - on the 'foo' call *)
  let definition_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/definition"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 1); ("character", `Int 1) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ definition_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find definition response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No definition response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      (* Check that we got a location with line 0 (the defun line) *)
      let range = result |> member "range" in
      let start = range |> member "start" in
      let line = start |> member "line" |> to_int in
      Alcotest.(check int) "definition on line 0" 0 line

let test_definition_on_defvar_call () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Document with a defvar and a reference to it *)
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
                   ("text", `String "(defvar my-var 10)\nmy-var");
                 ] );
           ])
      ()
  in
  (* Definition request at position (1, 0) - on 'my-var' reference *)
  let definition_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/definition"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 1); ("character", `Int 0) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ definition_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find definition response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No definition response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      (* Check that we got a location with line 0 (the defvar line) *)
      let range = result |> member "range" in
      let start = range |> member "start" in
      let line = start |> member "line" |> to_int in
      Alcotest.(check int) "definition on line 0" 0 line

let test_definition_not_found () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Document with a reference to an undefined function *)
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
                   ("text", `String "(undefined-fn)");
                 ] );
           ])
      ()
  in
  (* Definition request at position (0, 1) - on 'undefined-fn' *)
  let definition_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/definition"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 0); ("character", `Int 1) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ definition_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find definition response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No definition response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      (* Should return null when definition not found *)
      Alcotest.(check bool) "result is null" true (result = `Null)

let test_definition_outside_code () =
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
                   ("text", `String "(defun foo () 42)\n");
                 ] );
           ])
      ()
  in
  (* Definition request at position (1, 0) - empty line *)
  let definition_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/definition"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 1); ("character", `Int 0) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ definition_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find definition response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No definition response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      (* Should return null when not on code *)
      Alcotest.(check bool) "result is null" true (result = `Null)

let test_definition_has_uri () =
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
                   ("text", `String "(defun foo () 42)\n(foo)");
                 ] );
           ])
      ()
  in
  let definition_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/definition"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 1); ("character", `Int 1) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ definition_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No definition response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      (* Check that the location includes a URI *)
      let uri = result |> member "uri" |> to_string in
      Alcotest.(check bool)
        "uri starts with file://" true
        (String.length uri > 7 && String.sub uri 0 7 = "file://")

(** Test that definition lookup finds declarations in sibling .tart files (R14)
*)
let test_definition_cross_file () =
  (* Get the path to our test fixture *)
  let fixture_dir =
    Filename.concat (Sys.getcwd ()) "test/fixtures/definition"
  in
  let el_file = Filename.concat fixture_dir "main.el" in
  let tart_file = Filename.concat fixture_dir "mylib.tart" in
  (* Skip if fixtures don't exist (e.g., running from different directory) *)
  if not (Sys.file_exists tart_file) then ()
  else
    let el_content = In_channel.with_open_text el_file In_channel.input_all in
    let uri = "file://" ^ el_file in
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
                     ("uri", `String uri);
                     ("languageId", `String "elisp");
                     ("version", `Int 1);
                     ("text", `String el_content);
                   ] );
             ])
        ()
    in
    (* Definition request at position (2, 1) - on 'mylib-greet' call *)
    let definition_msg =
      make_message ~id:(`Int 2) ~method_:"textDocument/definition"
        ~params:
          (`Assoc
             [
               ("textDocument", `Assoc [ ("uri", `String uri) ]);
               ("position", `Assoc [ ("line", `Int 2); ("character", `Int 1) ]);
             ])
        ()
    in
    let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
    let exit_msg = make_message ~method_:"exit" () in
    let input =
      init_msg ^ did_open_msg ^ definition_msg ^ shutdown_msg ^ exit_msg
    in
    let in_file = Filename.temp_file "lsp_in" ".json" in
    Out_channel.with_open_bin in_file (fun oc ->
        Out_channel.output_string oc input);
    let ic = In_channel.open_bin in_file in
    let out_file = Filename.temp_file "lsp_out" ".json" in
    let oc = Out_channel.open_bin out_file in
    let server = Server.create ~log_level:Quiet ~ic ~oc () in
    let exit_code = Server.run server in
    In_channel.close ic;
    Out_channel.close oc;
    Alcotest.(check int) "exit code" 0 exit_code;
    let output = In_channel.with_open_bin out_file In_channel.input_all in
    let messages = parse_messages output in
    match find_response messages 2 with
    | None -> Alcotest.fail "No definition response found"
    | Some json ->
        let open Yojson.Safe.Util in
        let result = json |> member "result" in
        Alcotest.(check bool)
          "result is not null (found cross-file definition)" true
          (result <> `Null);
        (* Check that the URI points to the .tart file *)
        let result_uri = result |> member "uri" |> to_string in
        Alcotest.(check bool)
          "uri points to .tart file" true
          (try
             let _ =
               Str.search_forward (Str.regexp_string ".tart") result_uri 0
             in
             true
           with Not_found -> false)

(** Test that definition lookup works for defvar in sibling .tart (R14) *)
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
                     ("uri", `String uri);
                     ("languageId", `String "elisp");
                     ("version", `Int 1);
                     ("text", `String el_content);
                   ] );
             ])
        ()
    in
    (* Definition request at position (3, 0) - on 'mylib-version' reference *)
    let definition_msg =
      make_message ~id:(`Int 2) ~method_:"textDocument/definition"
        ~params:
          (`Assoc
             [
               ("textDocument", `Assoc [ ("uri", `String uri) ]);
               ("position", `Assoc [ ("line", `Int 3); ("character", `Int 0) ]);
             ])
        ()
    in
    let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
    let exit_msg = make_message ~method_:"exit" () in
    let input =
      init_msg ^ did_open_msg ^ definition_msg ^ shutdown_msg ^ exit_msg
    in
    let in_file = Filename.temp_file "lsp_in" ".json" in
    Out_channel.with_open_bin in_file (fun oc ->
        Out_channel.output_string oc input);
    let ic = In_channel.open_bin in_file in
    let out_file = Filename.temp_file "lsp_out" ".json" in
    let oc = Out_channel.open_bin out_file in
    let server = Server.create ~log_level:Quiet ~ic ~oc () in
    let exit_code = Server.run server in
    In_channel.close ic;
    Out_channel.close oc;
    Alcotest.(check int) "exit code" 0 exit_code;
    let output = In_channel.with_open_bin out_file In_channel.input_all in
    let messages = parse_messages output in
    match find_response messages 2 with
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

(** Test that find references returns all symbol occurrences *)
let test_references_all_occurrences () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Document with a defun and multiple calls to foo *)
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
                   ("text", `String "(defun foo () 42)\n(foo)\n(foo)");
                 ] );
           ])
      ()
  in
  (* References request at position (0, 7) - on 'foo' in defun *)
  let references_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/references"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 0); ("character", `Int 7) ]);
             ("context", `Assoc [ ("includeDeclaration", `Bool true) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ references_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find references response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No references response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      (* Should return a list of 3 locations (defun + 2 calls) *)
      let locations = result |> to_list in
      Alcotest.(check int) "found 3 references" 3 (List.length locations)

(** Test that find references returns empty list for unknown symbol *)
let test_references_on_unknown_symbol () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Document with a reference to an unknown symbol *)
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
                   ("text", `String "(unknown-fn)");
                 ] );
           ])
      ()
  in
  (* References request at position (0, 1) - on 'unknown-fn' *)
  let references_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/references"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 0); ("character", `Int 1) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ references_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find references response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No references response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      (* Should return exactly 1 reference (the call itself) *)
      let locations = result |> to_list in
      Alcotest.(check int) "found 1 reference" 1 (List.length locations)

(** Test that find references returns null when not on a symbol *)
let test_references_not_on_symbol () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Document with a literal number *)
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
                   ("text", `String "42");
                 ] );
           ])
      ()
  in
  (* References request at position (0, 0) - on the number *)
  let references_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/references"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ references_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find references response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No references response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      (* Should return null when not on a symbol *)
      Alcotest.(check bool) "result is null" true (result = `Null)

(** Test that references include location URIs *)
let test_references_have_uris () =
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
                   ("text", `String "(defun foo () 42)\n(foo)");
                 ] );
           ])
      ()
  in
  let references_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/references"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 1); ("character", `Int 1) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ references_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No references response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let locations = result |> to_list in
      Alcotest.(check bool) "has locations" true (List.length locations > 0);
      (* Check that each location has a URI *)
      List.iter
        (fun loc ->
          let uri = loc |> member "uri" |> to_string in
          Alcotest.(check bool)
            "uri starts with file://" true
            (String.length uri > 7 && String.sub uri 0 7 = "file://"))
        locations

(** Test references on defvar with multiple uses *)
let test_references_defvar () =
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
                   ("text", `String "(defvar my-var 10)\n(+ my-var 1)\nmy-var");
                 ] );
           ])
      ()
  in
  (* References request on my-var in defvar *)
  let references_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/references"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 0); ("character", `Int 8) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ references_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No references response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      (* Should find 3 references: defvar + (+ my-var 1) + my-var *)
      let locations = result |> to_list in
      Alcotest.(check int) "found 3 references" 3 (List.length locations)

(** {1 Code Action Tests} *)

(** Test that code action request returns an empty list (framework ready) *)
let test_code_action_returns_empty_list () =
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
                   ("text", `String "(+ 1 2)");
                 ] );
           ])
      ()
  in
  (* Code action request with cursor position (no selection) - should return empty *)
  let code_action_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/codeAction"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ( "range",
               `Assoc
                 [
                   ("start", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
                   ("end", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
                 ] );
             ("context", `Assoc [ ("diagnostics", `List []) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ code_action_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find code action response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      (* Should return an empty list (not null) *)
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let actions = result |> to_list in
      Alcotest.(check int) "returns empty list" 0 (List.length actions)

(** Test that code action request parses diagnostics context *)
let test_code_action_parses_context () =
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
                   ("text", `String "(+ 1 \"hello\")");
                 ] );
           ])
      ()
  in
  (* Code action request with a diagnostic in context *)
  let code_action_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/codeAction"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ( "range",
               `Assoc
                 [
                   ("start", `Assoc [ ("line", `Int 0); ("character", `Int 5) ]);
                   ("end", `Assoc [ ("line", `Int 0); ("character", `Int 12) ]);
                 ] );
             ( "context",
               `Assoc
                 [
                   ( "diagnostics",
                     `List
                       [
                         `Assoc
                           [
                             ( "range",
                               `Assoc
                                 [
                                   ( "start",
                                     `Assoc
                                       [
                                         ("line", `Int 0); ("character", `Int 5);
                                       ] );
                                   ( "end",
                                     `Assoc
                                       [
                                         ("line", `Int 0); ("character", `Int 12);
                                       ] );
                                 ] );
                             ("severity", `Int 1);
                             ("code", `String "E0308");
                             ("message", `String "type mismatch");
                             ("source", `String "tart");
                           ];
                       ] );
                 ] );
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ code_action_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find code action response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      (* Should not have an error - context parsed successfully *)
      Alcotest.(check bool) "no error" true (json |> member "error" = `Null);
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null)

(** Test that codeActionProvider capability is advertised *)
let test_code_action_capability_advertised () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 2) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find initialize response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 1 with
  | None -> Alcotest.fail "No initialize response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let caps = result |> member "capabilities" in
      (* Check codeActionProvider is true *)
      let code_action_provider = caps |> member "codeActionProvider" in
      Alcotest.(check bool)
        "codeActionProvider is true" true
        (code_action_provider = `Bool true)

(** Test that code action returns quickfix for missing signature.

    Creates temp files with a .el file defining a function and an empty .tart
    file, then verifies that a quickfix to add the signature is generated. *)
let test_code_action_missing_signature_quickfix () =
  (* Create temp directory with .el and .tart files *)
  let temp_dir = Filename.temp_dir "tart_test" "" in
  let el_path = Filename.concat temp_dir "test.el" in
  let tart_path = Filename.concat temp_dir "test.tart" in

  (* Write .el file with a function *)
  Out_channel.with_open_text el_path (fun oc ->
      output_string oc "(defun my-add (x y) (+ x y))");

  (* Write empty .tart file (no declaration for my-add) *)
  Out_channel.with_open_text tart_path (fun oc ->
      output_string oc "; empty signature file\n");

  let uri = "file://" ^ el_path in
  let el_content = "(defun my-add (x y) (+ x y))" in

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
                   ("uri", `String uri);
                   ("languageId", `String "elisp");
                   ("version", `Int 1);
                   ("text", `String el_content);
                 ] );
           ])
      ()
  in
  (* Code action request on line 0 where the defun is *)
  let code_action_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/codeAction"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String uri) ]);
             ( "range",
               `Assoc
                 [
                   ("start", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
                   ("end", `Assoc [ ("line", `Int 0); ("character", `Int 28) ]);
                 ] );
             ("context", `Assoc [ ("diagnostics", `List []) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ code_action_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  (* Parse output and find code action response *)
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let actions = result |> to_list in
      (* Should have at least one quickfix action *)
      let quickfix_actions =
        List.filter
          (fun a -> a |> member "kind" |> to_string = "quickfix")
          actions
      in
      Alcotest.(check int) "has one quickfix" 1 (List.length quickfix_actions);
      let action = List.hd quickfix_actions in
      (* Check the action title *)
      let title = action |> member "title" |> to_string in
      Alcotest.(check bool)
        "title contains function name" true
        (try
           let _ = Str.search_forward (Str.regexp_string "my-add") title 0 in
           true
         with Not_found -> false);
      (* Check action kind is quickfix *)
      let kind = action |> member "kind" |> to_string in
      Alcotest.(check string) "kind is quickfix" "quickfix" kind

(** Test that the quickfix contains a valid workspace edit *)
let test_code_action_quickfix_has_edit () =
  let temp_dir = Filename.temp_dir "tart_test" "" in
  let el_path = Filename.concat temp_dir "test.el" in
  let tart_path = Filename.concat temp_dir "test.tart" in

  Out_channel.with_open_text el_path (fun oc ->
      output_string oc "(defun my-add (x y) (+ x y))");
  Out_channel.with_open_text tart_path (fun oc -> output_string oc "");

  let uri = "file://" ^ el_path in
  let el_content = "(defun my-add (x y) (+ x y))" in

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
                   ("uri", `String uri);
                   ("languageId", `String "elisp");
                   ("version", `Int 1);
                   ("text", `String el_content);
                 ] );
           ])
      ()
  in
  let code_action_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/codeAction"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String uri) ]);
             ( "range",
               `Assoc
                 [
                   ("start", `Assoc [ ("line", `Int 0); ("character", `Int 0) ]);
                   ("end", `Assoc [ ("line", `Int 0); ("character", `Int 28) ]);
                 ] );
             ("context", `Assoc [ ("diagnostics", `List []) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ code_action_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let actions = result |> to_list in
      let action = List.hd actions in
      (* Check that edit is present *)
      let edit = action |> member "edit" in
      Alcotest.(check bool) "has edit" true (edit <> `Null);
      (* Check edit has documentChanges *)
      let doc_changes = edit |> member "documentChanges" |> to_list in
      Alcotest.(check int) "has one document change" 1 (List.length doc_changes);
      (* Check the URI points to .tart file *)
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
      (* Check the edit contains defun *)
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

(** Test that code action respects range - only returns actions for overlapping
    warnings *)
let test_code_action_respects_range () =
  let temp_dir = Filename.temp_dir "tart_test" "" in
  let el_path = Filename.concat temp_dir "test.el" in
  let tart_path = Filename.concat temp_dir "test.tart" in

  (* Two functions: one on line 0, one on line 1 *)
  Out_channel.with_open_text el_path (fun oc ->
      output_string oc "(defun fn-one (x) (+ x 1))\n(defun fn-two (y) (+ y 2))");
  Out_channel.with_open_text tart_path (fun oc -> output_string oc "");

  let uri = "file://" ^ el_path in
  let el_content = "(defun fn-one (x) (+ x 1))\n(defun fn-two (y) (+ y 2))" in

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
                   ("uri", `String uri);
                   ("languageId", `String "elisp");
                   ("version", `Int 1);
                   ("text", `String el_content);
                 ] );
           ])
      ()
  in
  (* Request code action only on line 1 (second function) *)
  let code_action_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/codeAction"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String uri) ]);
             ( "range",
               `Assoc
                 [
                   ("start", `Assoc [ ("line", `Int 1); ("character", `Int 0) ]);
                   ("end", `Assoc [ ("line", `Int 1); ("character", `Int 26) ]);
                 ] );
             ("context", `Assoc [ ("diagnostics", `List []) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ code_action_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let actions = result |> to_list in
      (* Filter to quickfix actions (not extract function) *)
      let quickfix_actions =
        List.filter
          (fun a -> a |> member "kind" |> to_string = "quickfix")
          actions
      in
      (* Should only return quickfix action for fn-two, not fn-one *)
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

(** Test that extract function action appears when selecting code *)
let test_extract_function_appears_on_selection () =
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
                   ("text", `String "(defun foo (x) (+ x 1))");
                 ] );
           ])
      ()
  in
  (* Select the (+ x 1) expression: chars 16-23 on line 0 *)
  let code_action_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/codeAction"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ( "range",
               `Assoc
                 [
                   ("start", `Assoc [ ("line", `Int 0); ("character", `Int 16) ]);
                   ("end", `Assoc [ ("line", `Int 0); ("character", `Int 23) ]);
                 ] );
             ("context", `Assoc [ ("diagnostics", `List []) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ code_action_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let actions = result |> to_list in
      (* Should have extract function action *)
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

(** Test that extract function generates correct workspace edit *)
let test_extract_function_has_edit () =
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
                   ("text", `String "(defun foo (x) (+ x 1))");
                 ] );
           ])
      ()
  in
  let code_action_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/codeAction"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ( "range",
               `Assoc
                 [
                   ("start", `Assoc [ ("line", `Int 0); ("character", `Int 16) ]);
                   ("end", `Assoc [ ("line", `Int 0); ("character", `Int 23) ]);
                 ] );
             ("context", `Assoc [ ("diagnostics", `List []) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ code_action_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
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
      (* Check that edit exists *)
      let edit = action |> member "edit" in
      Alcotest.(check bool) "has edit" true (edit <> `Null);
      (* Check document changes *)
      let doc_changes = edit |> member "documentChanges" |> to_list in
      Alcotest.(check int) "has one doc change" 1 (List.length doc_changes);
      let doc_change = List.hd doc_changes in
      (* Check there are edits (insert defun + replace selection) *)
      let edits = doc_change |> member "edits" |> to_list in
      Alcotest.(check int) "has two edits" 2 (List.length edits)

(** Test that extract function captures free variables as parameters *)
let test_extract_function_captures_free_vars () =
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
                   ("text", `String "(defun foo (x y) (+ x y))");
                 ] );
           ])
      ()
  in
  (* Select (+ x y) - position 17 is the opening paren, 24 is closing paren *)
  let code_action_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/codeAction"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ( "range",
               `Assoc
                 [
                   ("start", `Assoc [ ("line", `Int 0); ("character", `Int 17) ]);
                   ("end", `Assoc [ ("line", `Int 0); ("character", `Int 24) ]);
                 ] );
             ("context", `Assoc [ ("diagnostics", `List []) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ code_action_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
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
      (* Find the insert edit (contains defun) *)
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
      (* Check the defun has both x and y as parameters *)
      Alcotest.(check bool)
        "has x param" true
        (try
           let _ = Str.search_forward (Str.regexp_string "(x y)") new_text 0 in
           true
         with Not_found -> false)

(** {1 Document Symbol Tests} *)

(** Test that document symbol returns defun definitions *)
let test_document_symbol_defun () =
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
                   ("text", `String "(defun my-func (x y) (+ x y))");
                 ] );
           ])
      ()
  in
  let doc_symbol_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/documentSymbol"
      ~params:
        (`Assoc
           [ ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]) ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ doc_symbol_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
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
      (* kind 12 = Function *)
      let kind = symbol |> member "kind" |> to_int in
      Alcotest.(check int) "kind is Function" 12 kind;
      (* Check detail includes params *)
      let detail = symbol |> member "detail" |> to_string in
      Alcotest.(check string) "detail shows params" "(x y)" detail

(** Test that document symbol returns defvar definitions *)
let test_document_symbol_defvar () =
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
                   ("text", `String "(defvar my-var 42)");
                 ] );
           ])
      ()
  in
  let doc_symbol_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/documentSymbol"
      ~params:
        (`Assoc
           [ ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]) ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ doc_symbol_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No document symbol response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let symbols = result |> to_list in
      Alcotest.(check int) "has one symbol" 1 (List.length symbols);
      let symbol = List.hd symbols in
      let name = symbol |> member "name" |> to_string in
      Alcotest.(check string) "name is my-var" "my-var" name;
      (* kind 13 = Variable *)
      let kind = symbol |> member "kind" |> to_int in
      Alcotest.(check int) "kind is Variable" 13 kind

(** Test that document symbol returns defconst definitions *)
let test_document_symbol_defconst () =
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
                   ("text", `String "(defconst my-const 100)");
                 ] );
           ])
      ()
  in
  let doc_symbol_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/documentSymbol"
      ~params:
        (`Assoc
           [ ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]) ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ doc_symbol_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No document symbol response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let symbols = result |> to_list in
      Alcotest.(check int) "has one symbol" 1 (List.length symbols);
      let symbol = List.hd symbols in
      let name = symbol |> member "name" |> to_string in
      Alcotest.(check string) "name is my-const" "my-const" name;
      (* kind 14 = Constant *)
      let kind = symbol |> member "kind" |> to_int in
      Alcotest.(check int) "kind is Constant" 14 kind

(** Test that document symbol returns defmacro definitions *)
let test_document_symbol_defmacro () =
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
                   ("text", `String "(defmacro my-macro (body) `(progn ,body))");
                 ] );
           ])
      ()
  in
  let doc_symbol_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/documentSymbol"
      ~params:
        (`Assoc
           [ ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]) ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ doc_symbol_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No document symbol response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let symbols = result |> to_list in
      Alcotest.(check int) "has one symbol" 1 (List.length symbols);
      let symbol = List.hd symbols in
      let name = symbol |> member "name" |> to_string in
      Alcotest.(check string) "name is my-macro" "my-macro" name;
      (* kind 6 = Method (used for macros) *)
      let kind = symbol |> member "kind" |> to_int in
      Alcotest.(check int) "kind is Method" 6 kind;
      let detail = symbol |> member "detail" |> to_string in
      Alcotest.(check string) "detail shows params" "(body)" detail

(** Test that document symbol capability is advertised *)
let test_document_symbol_capability () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 2) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 1 with
  | None -> Alcotest.fail "No initialize response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let caps = result |> member "capabilities" in
      let doc_symbol_provider = caps |> member "documentSymbolProvider" in
      Alcotest.(check bool)
        "documentSymbolProvider is true" true
        (doc_symbol_provider = `Bool true)

(** Test that document symbol returns empty list for document with no defs *)
let test_document_symbol_empty () =
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
                   ("text", `String "(+ 1 2)");
                 ] );
           ])
      ()
  in
  let doc_symbol_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/documentSymbol"
      ~params:
        (`Assoc
           [ ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]) ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ doc_symbol_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No document symbol response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let symbols = result |> to_list in
      Alcotest.(check int) "no symbols" 0 (List.length symbols)

(** Test that extract function does not appear without selection *)
let test_extract_function_no_cursor () =
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
                   ("text", `String "(defun foo (x) (+ x 1))");
                 ] );
           ])
      ()
  in
  (* Cursor position only, no selection *)
  let code_action_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/codeAction"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ( "range",
               `Assoc
                 [
                   ("start", `Assoc [ ("line", `Int 0); ("character", `Int 16) ]);
                   ("end", `Assoc [ ("line", `Int 0); ("character", `Int 16) ]);
                 ] );
             ("context", `Assoc [ ("diagnostics", `List []) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ code_action_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No code action response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let actions = result |> to_list in
      (* Should NOT have extract function action (no selection) *)
      let extract_actions =
        List.filter
          (fun a -> a |> member "kind" |> to_string = "refactor.extract")
          actions
      in
      Alcotest.(check int) "no extract action" 0 (List.length extract_actions)

(** {1 Completion Tests} *)

(** Test that completion returns local defun *)
let test_completion_returns_local_defun () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Document with a defun and a partial call *)
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
                   ("text", `String "(defun my-foo (x) x)\n(my-f");
                 ] );
           ])
      ()
  in
  (* Completion request at position (1, 5) - after "my-f" *)
  let completion_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/completion"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 1); ("character", `Int 5) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ completion_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No completion response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      Alcotest.(check bool) "result is not null" true (result <> `Null);
      let items = result |> to_list in
      (* Should find my-foo *)
      let my_foo_items =
        List.filter
          (fun item -> item |> member "label" |> to_string = "my-foo")
          items
      in
      Alcotest.(check int) "found my-foo" 1 (List.length my_foo_items)

(** Test that completion returns defvar *)
let test_completion_returns_defvar () =
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
                   ("text", `String "(defvar my-var 42)\n(+ my-v");
                 ] );
           ])
      ()
  in
  let completion_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/completion"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 1); ("character", `Int 6) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ completion_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
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
      (* Check it has variable kind *)
      let item = List.hd my_var_items in
      let kind = item |> member "kind" |> to_int in
      Alcotest.(check int) "kind is Variable" 6 kind

(** Test that completion includes type information in detail *)
let test_completion_has_type_info () =
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
                   ("text", `String "(defun add (x y) (+ x y))\n(ad");
                 ] );
           ])
      ()
  in
  let completion_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/completion"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 1); ("character", `Int 3) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ completion_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
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
      (* Check it has a detail with params *)
      let item = List.hd add_items in
      let detail = item |> member "detail" |> to_string in
      Alcotest.(check bool) "has detail" true (String.length detail > 0)

(** Test that completion capability is advertised *)
let test_completion_capability_advertised () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 2) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input = init_msg ^ shutdown_msg ^ exit_msg in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 1 with
  | None -> Alcotest.fail "No initialize response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let caps = result |> member "capabilities" in
      let completion_provider = caps |> member "completionProvider" in
      Alcotest.(check bool)
        "completionProvider is present" true
        (completion_provider <> `Null)

(** Test that completion filters by prefix *)
let test_completion_filters_by_prefix () =
  let init_msg =
    make_message ~id:(`Int 1) ~method_:"initialize"
      ~params:(`Assoc [ ("processId", `Null); ("capabilities", `Assoc []) ])
      ()
  in
  (* Define multiple functions, request completion for "my-b" prefix *)
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
                   ( "text",
                     `String
                       "(defun my-alpha () nil)\n\
                        (defun my-beta () nil)\n\
                        (defun other () nil)\n\
                        (my-b" );
                 ] );
           ])
      ()
  in
  let completion_msg =
    make_message ~id:(`Int 2) ~method_:"textDocument/completion"
      ~params:
        (`Assoc
           [
             ("textDocument", `Assoc [ ("uri", `String "file:///test.el") ]);
             ("position", `Assoc [ ("line", `Int 3); ("character", `Int 5) ]);
           ])
      ()
  in
  let shutdown_msg = make_message ~id:(`Int 3) ~method_:"shutdown" () in
  let exit_msg = make_message ~method_:"exit" () in
  let input =
    init_msg ^ did_open_msg ^ completion_msg ^ shutdown_msg ^ exit_msg
  in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~log_level:Quiet ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  Alcotest.(check int) "exit code" 0 exit_code;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  match find_response messages 2 with
  | None -> Alcotest.fail "No completion response found"
  | Some json ->
      let open Yojson.Safe.Util in
      let result = json |> member "result" in
      let items = result |> to_list in
      (* Should only include my-beta, not my-alpha or other *)
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
    ]
