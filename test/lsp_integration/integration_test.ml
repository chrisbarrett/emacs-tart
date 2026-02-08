(** Subprocess integration tests for the [tart lsp] binary.

    These tests spawn the actual binary and communicate via stdio pipes,
    exercising CLI argument parsing, process lifecycle, and pipe semantics. *)

open Lsp_client

let tart_bin =
  match Sys.getenv_opt "TART_BIN" with
  | Some bin -> bin
  | None -> failwith "TART_BIN not set. Run via 'dune test'."

(** {1 Protocol Lifecycle Tests} *)

let test_initialize_handshake () =
  let client = Subprocess_client.start ~tart_bin in
  Subprocess_client.send client (initialize_msg ~id:1 ());
  let resp = Subprocess_client.recv client in
  let open Yojson.Safe.Util in
  Alcotest.(check int) "response id" 1 (resp |> member "id" |> to_int);
  let result = resp |> member "result" in
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
    (server_info |> member "name" |> to_string);
  (* Clean shutdown *)
  Subprocess_client.send client (shutdown_msg ~id:99 ());
  let _shutdown_resp = Subprocess_client.recv client in
  Subprocess_client.send client (exit_msg ());
  let exit_code = Subprocess_client.shutdown client in
  Alcotest.(check int) "clean exit" 0 exit_code

let test_shutdown_exit () =
  let client = Subprocess_client.start ~tart_bin in
  Subprocess_client.send client (initialize_msg ~id:1 ());
  let _init = Subprocess_client.recv client in
  Subprocess_client.send client (shutdown_msg ~id:99 ());
  let shutdown_resp = Subprocess_client.recv client in
  let open Yojson.Safe.Util in
  Alcotest.(check int) "shutdown id" 99 (shutdown_resp |> member "id" |> to_int);
  Alcotest.(check bool)
    "shutdown result is null" true
    (shutdown_resp |> member "result" = `Null);
  Subprocess_client.send client (exit_msg ());
  let exit_code = Subprocess_client.shutdown client in
  Alcotest.(check int) "clean exit after shutdown" 0 exit_code

let test_exit_without_shutdown () =
  let client = Subprocess_client.start ~tart_bin in
  Subprocess_client.send client (initialize_msg ~id:1 ());
  let _init = Subprocess_client.recv client in
  Subprocess_client.send client (exit_msg ());
  let exit_code = Subprocess_client.shutdown client in
  Alcotest.(check int) "exit code 1 without shutdown" 1 exit_code

let test_unknown_method () =
  let client = Subprocess_client.start ~tart_bin in
  Subprocess_client.send client (initialize_msg ~id:1 ());
  let _init = Subprocess_client.recv client in
  Subprocess_client.send client
    (make_message ~id:(`Int 2) ~method_:"unknown/method" ());
  let resp = Subprocess_client.recv client in
  let open Yojson.Safe.Util in
  (match response_error resp with
  | Some err ->
      Alcotest.(check int)
        "error code -32601" (-32601)
        (err |> member "code" |> to_int)
  | None -> Alcotest.fail "Expected error response for unknown method");
  Subprocess_client.send client (shutdown_msg ~id:99 ());
  let _shutdown = Subprocess_client.recv client in
  Subprocess_client.send client (exit_msg ());
  let exit_code = Subprocess_client.shutdown client in
  Alcotest.(check int) "clean exit" 0 exit_code

(** {1 Document Sync Tests} *)

let test_did_open_publishes_diagnostics () =
  let client = Subprocess_client.start ~tart_bin in
  Subprocess_client.send client (initialize_msg ~id:1 ());
  let _init = Subprocess_client.recv client in
  Subprocess_client.send client (initialized_msg ());
  Subprocess_client.send client
    (did_open_msg ~uri:"file:///test.el" ~text:"(defun foo () t)" ());
  let msgs = Subprocess_client.recv_all client ~timeout_ms:5000 in
  let diag = find_diagnostics ~uri:"file:///test.el" msgs in
  Alcotest.(check bool) "received diagnostics" true (Option.is_some diag);
  (match diag with
  | Some json ->
      let open Yojson.Safe.Util in
      let diags = json |> member "params" |> member "diagnostics" |> to_list in
      Alcotest.(check bool)
        "no errors for valid code" true
        (List.length diags = 0)
  | None -> ());
  Subprocess_client.send client (shutdown_msg ~id:99 ());
  let _shutdown = Subprocess_client.recv client in
  Subprocess_client.send client (exit_msg ());
  let _exit_code = Subprocess_client.shutdown client in
  ()

let test_did_open_with_error () =
  let client = Subprocess_client.start ~tart_bin in
  Subprocess_client.send client (initialize_msg ~id:1 ());
  let _init = Subprocess_client.recv client in
  Subprocess_client.send client (initialized_msg ());
  (* An unclosed paren should produce a parse error *)
  Subprocess_client.send client
    (did_open_msg ~uri:"file:///bad.el" ~text:"(defun foo ()" ());
  let msgs = Subprocess_client.recv_all client ~timeout_ms:5000 in
  let diag = find_diagnostics ~uri:"file:///bad.el" msgs in
  Alcotest.(check bool)
    "received diagnostics for bad file" true (Option.is_some diag);
  (match diag with
  | Some json ->
      let open Yojson.Safe.Util in
      let diags = json |> member "params" |> member "diagnostics" |> to_list in
      Alcotest.(check bool)
        "has at least one diagnostic" true
        (List.length diags > 0)
  | None -> ());
  Subprocess_client.send client (shutdown_msg ~id:99 ());
  let _shutdown = Subprocess_client.recv client in
  Subprocess_client.send client (exit_msg ());
  let _exit_code = Subprocess_client.shutdown client in
  ()

let test_did_change_incremental () =
  let client = Subprocess_client.start ~tart_bin in
  Subprocess_client.send client (initialize_msg ~id:1 ());
  let _init = Subprocess_client.recv client in
  Subprocess_client.send client (initialized_msg ());
  Subprocess_client.send client
    (did_open_msg ~uri:"file:///change.el" ~text:"(defun foo () t)" ());
  (* Consume initial diagnostics *)
  let _initial = Subprocess_client.recv_all client ~timeout_ms:3000 in
  (* Send a full-document change *)
  Subprocess_client.send client
    (did_change_full_msg ~uri:"file:///change.el" ~version:2
       ~text:"(defun bar () nil)" ());
  let msgs = Subprocess_client.recv_all client ~timeout_ms:3000 in
  let diag = find_diagnostics ~uri:"file:///change.el" msgs in
  Alcotest.(check bool)
    "received diagnostics after change" true (Option.is_some diag);
  Subprocess_client.send client (shutdown_msg ~id:99 ());
  let _shutdown = Subprocess_client.recv client in
  Subprocess_client.send client (exit_msg ());
  let _exit_code = Subprocess_client.shutdown client in
  ()

let test_did_close_clears_diagnostics () =
  let client = Subprocess_client.start ~tart_bin in
  Subprocess_client.send client (initialize_msg ~id:1 ());
  let _init = Subprocess_client.recv client in
  Subprocess_client.send client (initialized_msg ());
  Subprocess_client.send client
    (did_open_msg ~uri:"file:///close.el" ~text:"(defun foo () t)" ());
  let _initial = Subprocess_client.recv_all client ~timeout_ms:3000 in
  Subprocess_client.send client (did_close_msg ~uri:"file:///close.el" ());
  let msgs = Subprocess_client.recv_all client ~timeout_ms:3000 in
  (* After close, server should publish empty diagnostics *)
  let diag = find_diagnostics ~uri:"file:///close.el" msgs in
  (match diag with
  | Some json ->
      let open Yojson.Safe.Util in
      let diags = json |> member "params" |> member "diagnostics" |> to_list in
      Alcotest.(check int) "diagnostics cleared" 0 (List.length diags)
  | None ->
      (* It's also acceptable if no diagnostics notification is sent after close,
         since the document is gone. *)
      ());
  Subprocess_client.send client (shutdown_msg ~id:99 ());
  let _shutdown = Subprocess_client.recv client in
  Subprocess_client.send client (exit_msg ());
  let _exit_code = Subprocess_client.shutdown client in
  ()

(** {1 Edge Case Tests} *)

let test_empty_file () =
  let client = Subprocess_client.start ~tart_bin in
  Subprocess_client.send client (initialize_msg ~id:1 ());
  let _init = Subprocess_client.recv client in
  Subprocess_client.send client (initialized_msg ());
  Subprocess_client.send client
    (did_open_msg ~uri:"file:///empty.el" ~text:"" ());
  let msgs = Subprocess_client.recv_all client ~timeout_ms:3000 in
  let diag = find_diagnostics ~uri:"file:///empty.el" msgs in
  Alcotest.(check bool)
    "received diagnostics for empty file" true (Option.is_some diag);
  (match diag with
  | Some json ->
      let open Yojson.Safe.Util in
      let diags = json |> member "params" |> member "diagnostics" |> to_list in
      Alcotest.(check int) "no errors for empty file" 0 (List.length diags)
  | None -> ());
  Subprocess_client.send client (shutdown_msg ~id:99 ());
  let _shutdown = Subprocess_client.recv client in
  Subprocess_client.send client (exit_msg ());
  let _exit_code = Subprocess_client.shutdown client in
  ()

let test_comment_only_file () =
  let client = Subprocess_client.start ~tart_bin in
  Subprocess_client.send client (initialize_msg ~id:1 ());
  let _init = Subprocess_client.recv client in
  Subprocess_client.send client (initialized_msg ());
  Subprocess_client.send client
    (did_open_msg ~uri:"file:///comments.el"
       ~text:";; This is a comment\n;; Another comment\n" ());
  let msgs = Subprocess_client.recv_all client ~timeout_ms:3000 in
  let diag = find_diagnostics ~uri:"file:///comments.el" msgs in
  Alcotest.(check bool)
    "received diagnostics for comment-only file" true (Option.is_some diag);
  (match diag with
  | Some json ->
      let open Yojson.Safe.Util in
      let diags = json |> member "params" |> member "diagnostics" |> to_list in
      Alcotest.(check int)
        "no errors for comment-only file" 0 (List.length diags)
  | None -> ());
  Subprocess_client.send client (shutdown_msg ~id:99 ());
  let _shutdown = Subprocess_client.recv client in
  Subprocess_client.send client (exit_msg ());
  let _exit_code = Subprocess_client.shutdown client in
  ()

(** {1 Stdin EOF Test} *)

let test_stdin_eof () =
  let client = Subprocess_client.start ~tart_bin in
  Subprocess_client.send client (initialize_msg ~id:1 ());
  let _init = Subprocess_client.recv client in
  (* Close stdin without sending shutdown/exit *)
  let exit_code = Subprocess_client.shutdown client in
  (* Server should exit with non-zero when stdin closes unexpectedly *)
  Alcotest.(check bool) "non-zero exit on stdin EOF" true (exit_code <> 0)

(** {1 Test Runner} *)

let () =
  Alcotest.run "LSP Integration"
    [
      ( "lifecycle",
        [
          Alcotest.test_case "initialize handshake" `Slow
            test_initialize_handshake;
          Alcotest.test_case "shutdown/exit" `Slow test_shutdown_exit;
          Alcotest.test_case "exit without shutdown" `Slow
            test_exit_without_shutdown;
          Alcotest.test_case "unknown method → -32601" `Slow test_unknown_method;
          Alcotest.test_case "stdin EOF" `Slow test_stdin_eof;
        ] );
      ( "document sync",
        [
          Alcotest.test_case "didOpen publishes diagnostics" `Slow
            test_did_open_publishes_diagnostics;
          Alcotest.test_case "didOpen with error" `Slow test_did_open_with_error;
          Alcotest.test_case "didChange incremental" `Slow
            test_did_change_incremental;
          Alcotest.test_case "didClose clears diagnostics" `Slow
            test_did_close_clears_diagnostics;
        ] );
      ( "edge cases",
        [
          Alcotest.test_case "empty file" `Slow test_empty_file;
          Alcotest.test_case "comment-only file" `Slow test_comment_only_file;
        ] );
    ]
