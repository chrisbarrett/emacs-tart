(** Smoke tests for the Lsp_client test helper library. *)

let test_make_message () =
  let msg = Lsp_client.make_message ~method_:"test" () in
  Alcotest.(check bool)
    "has Content-Length header" true
    (String.length msg > 0 && String.sub msg 0 15 = "Content-Length:");
  let msg_with_id =
    Lsp_client.make_message ~id:(`Int 1) ~method_:"test"
      ~params:(`Assoc [ ("key", `String "value") ])
      ()
  in
  Alcotest.(check bool)
    "message with id has content" true
    (String.length msg_with_id > String.length msg)

let test_message_builders () =
  let init = Lsp_client.initialize_msg () in
  Alcotest.(check bool) "initialize non-empty" true (String.length init > 0);
  let init_uri = Lsp_client.initialize_msg ~root_uri:"file:///tmp" () in
  Alcotest.(check bool)
    "initialize with uri" true
    (String.length init_uri > String.length init);
  let sd = Lsp_client.shutdown_msg () in
  Alcotest.(check bool) "shutdown non-empty" true (String.length sd > 0);
  let ex = Lsp_client.exit_msg () in
  Alcotest.(check bool) "exit non-empty" true (String.length ex > 0);
  let open_ = Lsp_client.did_open_msg ~uri:"file:///test.el" ~text:"hello" () in
  Alcotest.(check bool) "didOpen non-empty" true (String.length open_ > 0);
  let close = Lsp_client.did_close_msg ~uri:"file:///test.el" () in
  Alcotest.(check bool) "didClose non-empty" true (String.length close > 0)

let test_parse_messages () =
  (* Build a raw output with two messages *)
  let json1 = Yojson.Safe.to_string (`Assoc [ ("id", `Int 1) ]) in
  let json2 = Yojson.Safe.to_string (`Assoc [ ("id", `Int 2) ]) in
  let raw =
    Printf.sprintf "Content-Length: %d\r\n\r\n%sContent-Length: %d\r\n\r\n%s"
      (String.length json1) json1 (String.length json2) json2
  in
  let msgs = Lsp_client.parse_messages raw in
  Alcotest.(check int) "parsed 2 messages" 2 (List.length msgs)

let test_run_session () =
  let result =
    Lsp_client.run_session
      [
        Lsp_client.initialize_msg ();
        Lsp_client.shutdown_msg ();
        Lsp_client.exit_msg ();
      ]
  in
  Alcotest.(check int) "exit code 0" 0 result.exit_code;
  Alcotest.(check bool) "has responses" true (List.length result.messages > 0)

let test_run_initialized_session () =
  let result = Lsp_client.run_initialized_session [] in
  Alcotest.(check int) "exit code 0" 0 result.exit_code;
  let init_resp = Lsp_client.find_response ~id:1 result.messages in
  Alcotest.(check bool) "has init response" true (Option.is_some init_resp)

let test_find_response () =
  let result = Lsp_client.run_initialized_session [] in
  let found = Lsp_client.find_response ~id:1 result.messages in
  Alcotest.(check bool) "found id 1" true (Option.is_some found);
  let not_found = Lsp_client.find_response ~id:9999 result.messages in
  Alcotest.(check bool) "not found id 9999" true (Option.is_none not_found)

let test_response_result () =
  let result = Lsp_client.run_initialized_session [] in
  match Lsp_client.find_response ~id:1 result.messages with
  | None -> Alcotest.fail "No init response"
  | Some resp ->
      let r = Lsp_client.response_result resp in
      Alcotest.(check bool) "result is not null" true (r <> `Null);
      let err = Lsp_client.response_error resp in
      Alcotest.(check bool) "no error" true (Option.is_none err)

let test_find_diagnostics () =
  let uri = "file:///test.el" in
  let result =
    Lsp_client.run_initialized_session
      [ Lsp_client.did_open_msg ~uri ~text:"(+ 1 \"bad\")" () ]
  in
  let diag = Lsp_client.find_diagnostics ~uri result.messages in
  Alcotest.(check bool) "found diagnostics" true (Option.is_some diag)

let () =
  Alcotest.run "lsp_client"
    [
      ( "message-construction",
        [
          Alcotest.test_case "make_message" `Quick test_make_message;
          Alcotest.test_case "builders" `Quick test_message_builders;
          Alcotest.test_case "parse_messages" `Quick test_parse_messages;
        ] );
      ( "session",
        [
          Alcotest.test_case "run_session" `Quick test_run_session;
          Alcotest.test_case "run_initialized_session" `Quick
            test_run_initialized_session;
        ] );
      ( "queries",
        [
          Alcotest.test_case "find_response" `Quick test_find_response;
          Alcotest.test_case "response_result" `Quick test_response_result;
          Alcotest.test_case "find_diagnostics" `Quick test_find_diagnostics;
        ] );
    ]
