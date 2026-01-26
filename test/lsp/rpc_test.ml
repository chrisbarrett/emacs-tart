(** Tests for JSON-RPC protocol implementation *)

open Alcotest
module Rpc = Tart.Rpc

(** Helper to check if a string contains a substring *)
let contains ~substring s =
  try
    let _ = Str.search_forward (Str.regexp_string substring) s 0 in
    true
  with Not_found -> false

(** Helper to create an in_channel from a string *)
let ic_of_string s =
  let filename = Filename.temp_file "rpc_test" ".txt" in
  let oc = open_out filename in
  output_string oc s;
  close_out oc;
  open_in filename

(** Helper to capture output to a string *)
let capture_output f =
  let filename = Filename.temp_file "rpc_test" ".txt" in
  let oc = open_out filename in
  f oc;
  close_out oc;
  let ic = open_in filename in
  let content = In_channel.input_all ic in
  close_in ic;
  content

(** Test read_message with a valid request *)
let test_read_request () =
  let json = {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}|} in
  let content =
    Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length json) json
  in
  let ic = ic_of_string content in
  match Rpc.read_message ic with
  | Ok msg ->
      check (option (of_pp Yojson.Safe.pp)) "id" (Some (`Int 1)) msg.id;
      check string "method" "initialize" msg.method_;
      check
        (option (of_pp Yojson.Safe.pp))
        "params"
        (Some (`Assoc []))
        msg.params
  | Error e -> fail (Rpc.read_error_to_string e)

(** Test read_message with a notification (no id) *)
let test_read_notification () =
  let json = {|{"jsonrpc":"2.0","method":"initialized","params":{}}|} in
  let content =
    Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length json) json
  in
  let ic = ic_of_string content in
  match Rpc.read_message ic with
  | Ok msg ->
      check (option (of_pp Yojson.Safe.pp)) "id" None msg.id;
      check string "method" "initialized" msg.method_
  | Error e -> fail (Rpc.read_error_to_string e)

(** Test read_message with no params *)
let test_read_no_params () =
  let json = {|{"jsonrpc":"2.0","id":1,"method":"shutdown"}|} in
  let content =
    Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length json) json
  in
  let ic = ic_of_string content in
  match Rpc.read_message ic with
  | Ok msg -> check (option (of_pp Yojson.Safe.pp)) "params" None msg.params
  | Error e -> fail (Rpc.read_error_to_string e)

(** Test read_message with missing Content-Length *)
let test_read_missing_header () =
  let content = "\r\n{}" in
  let ic = ic_of_string content in
  match Rpc.read_message ic with
  | Ok _ -> fail "Should have failed"
  | Error (Rpc.InvalidHeader _) -> ()
  | Error e ->
      fail (Printf.sprintf "Wrong error: %s" (Rpc.read_error_to_string e))

(** Test read_message with invalid JSON *)
let test_read_invalid_json () =
  let content = "Content-Length: 3\r\n\r\n{x}" in
  let ic = ic_of_string content in
  match Rpc.read_message ic with
  | Ok _ -> fail "Should have failed"
  | Error (Rpc.InvalidJson _) -> ()
  | Error e ->
      fail (Printf.sprintf "Wrong error: %s" (Rpc.read_error_to_string e))

(** Test read_message with missing method *)
let test_read_missing_method () =
  let json = {|{"jsonrpc":"2.0","id":1}|} in
  let content =
    Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length json) json
  in
  let ic = ic_of_string content in
  match Rpc.read_message ic with
  | Ok _ -> fail "Should have failed"
  | Error (Rpc.InvalidMessage _) -> ()
  | Error e ->
      fail (Printf.sprintf "Wrong error: %s" (Rpc.read_error_to_string e))

(** Test write_response with success *)
let test_write_success () =
  let resp = Rpc.success_response ~id:(`Int 1) ~result:(`String "ok") in
  let output = capture_output (fun oc -> Rpc.write_response oc resp) in
  check bool "has header" true (String.length output > 0);
  check bool "has content-length" true
    (String.starts_with ~prefix:"Content-Length:" output);
  check bool "has result" true
    (String.ends_with ~suffix:{|"result":"ok"}|} output)

(** Test write_response with error *)
let test_write_error () =
  let resp =
    Rpc.error_response ~id:(`Int 1) ~code:Rpc.method_not_found
      ~message:"Method not found" ()
  in
  let output = capture_output (fun oc -> Rpc.write_response oc resp) in
  check bool "has error" true (contains ~substring:{|"error"|} output);
  check bool "has code" true (contains ~substring:"-32601" output)

(** Test write_notification *)
let test_write_notification () =
  let output =
    capture_output (fun oc ->
        Rpc.write_notification oc ~method_:"textDocument/publishDiagnostics"
          ~params:(`Assoc [ ("uri", `String "file:///test.el") ]))
  in
  check bool "has method" true
    (contains ~substring:{|"method":"textDocument/publishDiagnostics"|} output);
  check bool "has params" true (contains ~substring:"uri" output);
  check bool "no id" false (contains ~substring:{|"id"|} output)

(** Test error codes *)
let test_error_codes () =
  check int "parse_error" (-32700) Rpc.parse_error;
  check int "invalid_request" (-32600) Rpc.invalid_request;
  check int "method_not_found" (-32601) Rpc.method_not_found;
  check int "invalid_params" (-32602) Rpc.invalid_params;
  check int "internal_error" (-32603) Rpc.internal_error;
  check int "server_not_initialized" (-32002) Rpc.server_not_initialized

(** Test message_to_string *)
let test_message_to_string () =
  let msg = { Rpc.id = Some (`Int 1); method_ = "test"; params = None } in
  let s = Rpc.message_to_string msg in
  check bool "has id" true (contains ~substring:"id=1" s);
  check bool "has method" true (contains ~substring:"method=test" s)

(** Test response_to_string *)
let test_response_to_string () =
  let resp = Rpc.success_response ~id:(`Int 1) ~result:(`String "ok") in
  let s = Rpc.response_to_string resp in
  check bool "has id" true (contains ~substring:"id=1" s);
  check bool "has result" true (contains ~substring:"result" s)

(** Test reading multiple messages *)
let test_read_multiple () =
  let msg1 = {|{"jsonrpc":"2.0","id":1,"method":"m1"}|} in
  let msg2 = {|{"jsonrpc":"2.0","id":2,"method":"m2"}|} in
  let content =
    Printf.sprintf "Content-Length: %d\r\n\r\n%sContent-Length: %d\r\n\r\n%s"
      (String.length msg1) msg1 (String.length msg2) msg2
  in
  let ic = ic_of_string content in
  match Rpc.read_message ic with
  | Ok msg1_parsed -> (
      check string "first method" "m1" msg1_parsed.method_;
      match Rpc.read_message ic with
      | Ok msg2_parsed -> check string "second method" "m2" msg2_parsed.method_
      | Error e -> fail (Rpc.read_error_to_string e))
  | Error e -> fail (Rpc.read_error_to_string e)

(** Test EOF handling *)
let test_read_eof () =
  let ic = ic_of_string "" in
  match Rpc.read_message ic with
  | Ok _ -> fail "Should have failed with Eof"
  | Error Rpc.Eof -> ()
  | Error e ->
      fail (Printf.sprintf "Wrong error: %s" (Rpc.read_error_to_string e))

let () =
  run "rpc"
    [
      ( "read",
        [
          test_case "request" `Quick test_read_request;
          test_case "notification" `Quick test_read_notification;
          test_case "no params" `Quick test_read_no_params;
          test_case "missing header" `Quick test_read_missing_header;
          test_case "invalid json" `Quick test_read_invalid_json;
          test_case "missing method" `Quick test_read_missing_method;
          test_case "multiple" `Quick test_read_multiple;
          test_case "eof" `Quick test_read_eof;
        ] );
      ( "write",
        [
          test_case "success" `Quick test_write_success;
          test_case "error" `Quick test_write_error;
          test_case "notification" `Quick test_write_notification;
        ] );
      ( "helpers",
        [
          test_case "error codes" `Quick test_error_codes;
          test_case "message to string" `Quick test_message_to_string;
          test_case "response to string" `Quick test_response_to_string;
        ] );
    ]
