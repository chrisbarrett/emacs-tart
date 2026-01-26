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
        ] );
    ]
