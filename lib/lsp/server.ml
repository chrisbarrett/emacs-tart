(** LSP Server state and main loop.

    Manages server lifecycle: uninitialized -> initialized -> shutdown. *)

(** Server state *)
type state =
  | Uninitialized
  | Initialized of { root_uri : string option }
  | ShuttingDown

type t = {
  ic : In_channel.t;
  oc : Out_channel.t;
  mutable state : state;
  mutable log_level : log_level;
  documents : Document.t;
}

and log_level =
  | Quiet
  | Normal
  | Debug

(** Create a new server on the given channels *)
let create ?(log_level = Normal) ~ic ~oc () : t =
  { ic; oc; state = Uninitialized; log_level; documents = Document.create () }

(** Get the server's current state *)
let state (server : t) : state = server.state

(** Get the server's document store (for testing) *)
let documents (server : t) : Document.t = server.documents

(** Log a message to stderr *)
let log (server : t) (level : log_level) (msg : string) : unit =
  let should_log =
    match (server.log_level, level) with
    | Quiet, _ -> false
    | Normal, Quiet -> false
    | Normal, _ -> true
    | Debug, _ -> true
  in
  if should_log then prerr_endline ("[tart-lsp] " ^ msg)

(** Log a debug message *)
let debug (server : t) (msg : string) : unit = log server Debug msg

(** Log an info message *)
let info (server : t) (msg : string) : unit = log server Normal msg

(** Get server capabilities *)
let capabilities () : Protocol.server_capabilities =
  {
    text_document_sync =
      Some { open_close = true; change = Protocol.Incremental };
    hover_provider = true;
  }

(** Handle initialize request *)
let handle_initialize (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  match server.state with
  | Initialized _ | ShuttingDown ->
      Error
        {
          Rpc.code = Rpc.invalid_request;
          message = "Server already initialized";
          data = None;
        }
  | Uninitialized ->
      let init_params =
        match params with
        | Some json -> Protocol.parse_initialize_params json
        | None ->
            {
              Protocol.process_id = None;
              root_uri = None;
              capabilities = { text_document = None };
            }
      in
      info server
        (Printf.sprintf "Initializing (root: %s)"
           (Option.value init_params.root_uri ~default:"<none>"));
      server.state <- Initialized { root_uri = init_params.root_uri };
      let result : Protocol.initialize_result =
        { capabilities = capabilities () }
      in
      let server_info : Protocol.server_info =
        { name = "tart"; version = Some "0.1.0" }
      in
      Ok (Protocol.initialize_response_to_json ~result ~server_info)

(** Handle initialized notification *)
let handle_initialized (server : t) : unit =
  info server "Client confirmed initialization"

(** Handle shutdown request *)
let handle_shutdown (server : t) : (Yojson.Safe.t, Rpc.response_error) result =
  match server.state with
  | Uninitialized ->
      Error
        {
          Rpc.code = Rpc.server_not_initialized;
          message = "Server not initialized";
          data = None;
        }
  | ShuttingDown -> Ok `Null
  | Initialized _ ->
      info server "Shutting down";
      server.state <- ShuttingDown;
      Ok `Null

(** Handle exit notification *)
let handle_exit (server : t) : [ `Exit of int ] =
  match server.state with
  | ShuttingDown ->
      info server "Exiting (clean shutdown)";
      `Exit 0
  | _ ->
      info server "Exiting (no shutdown request)";
      `Exit 1

(** Handle textDocument/didOpen notification *)
let handle_did_open (server : t) (params : Yojson.Safe.t option) : unit =
  match params with
  | None -> debug server "didOpen missing params"
  | Some json ->
      let open Yojson.Safe.Util in
      let text_document = json |> member "textDocument" in
      let uri, version, text =
        Document.text_document_item_of_json text_document
      in
      Document.open_doc server.documents ~uri ~version ~text;
      debug server (Printf.sprintf "Opened document: %s (version %d)" uri version)

(** Handle textDocument/didChange notification *)
let handle_did_change (server : t) (params : Yojson.Safe.t option) : unit =
  match params with
  | None -> debug server "didChange missing params"
  | Some json ->
      let open Yojson.Safe.Util in
      let text_document = json |> member "textDocument" in
      let uri, version =
        Document.versioned_text_document_identifier_of_json text_document
      in
      let changes =
        json |> member "contentChanges" |> to_list
        |> List.map Document.content_change_of_json
      in
      (match Document.apply_changes server.documents ~uri ~version changes with
      | Ok () ->
          debug server
            (Printf.sprintf "Changed document: %s (version %d)" uri version)
      | Error e ->
          info server
            (Printf.sprintf "Error applying changes to %s: %s" uri e))

(** Handle textDocument/didClose notification *)
let handle_did_close (server : t) (params : Yojson.Safe.t option) : unit =
  match params with
  | None -> debug server "didClose missing params"
  | Some json ->
      let open Yojson.Safe.Util in
      let text_document = json |> member "textDocument" in
      let uri = Document.text_document_identifier_of_json text_document in
      Document.close_doc server.documents ~uri;
      debug server (Printf.sprintf "Closed document: %s" uri)

(** Dispatch a request to its handler *)
let dispatch_request (server : t) (msg : Rpc.message) :
    (Yojson.Safe.t, Rpc.response_error) result =
  debug server (Printf.sprintf "Request: %s" msg.method_);
  match msg.method_ with
  | "initialize" -> handle_initialize server msg.params
  | "shutdown" -> handle_shutdown server
  | _ ->
      Error
        {
          Rpc.code = Rpc.method_not_found;
          message = Printf.sprintf "Unknown method: %s" msg.method_;
          data = None;
        }

(** Dispatch a notification to its handler *)
let dispatch_notification (server : t) (msg : Rpc.message) :
    [ `Continue | `Exit of int ] =
  debug server (Printf.sprintf "Notification: %s" msg.method_);
  match msg.method_ with
  | "initialized" ->
      handle_initialized server;
      `Continue
  | "exit" -> (handle_exit server :> [ `Continue | `Exit of int ])
  | "textDocument/didOpen" ->
      handle_did_open server msg.params;
      `Continue
  | "textDocument/didChange" ->
      handle_did_change server msg.params;
      `Continue
  | "textDocument/didClose" ->
      handle_did_close server msg.params;
      `Continue
  | "$/cancelRequest" ->
      (* Ignore cancellation for now *)
      `Continue
  | _ ->
      (* Ignore unknown notifications per LSP spec *)
      debug server (Printf.sprintf "Ignoring unknown notification: %s" msg.method_);
      `Continue

(** Process a single message and optionally send a response *)
let process_message (server : t) (msg : Rpc.message) : [ `Continue | `Exit of int ]
    =
  match msg.id with
  | Some id ->
      (* Request - must send response *)
      let response =
        match dispatch_request server msg with
        | Ok result -> Rpc.success_response ~id ~result
        | Error err -> { Rpc.id; result = None; error = Some err }
      in
      debug server (Printf.sprintf "Response: %s" (Rpc.response_to_string response));
      Rpc.write_response server.oc response;
      `Continue
  | None ->
      (* Notification - no response *)
      dispatch_notification server msg

(** Main server loop *)
let run (server : t) : int =
  info server "Starting LSP server";
  let rec loop () =
    match Rpc.read_message server.ic with
    | Error Rpc.Eof ->
        info server "Client disconnected";
        (* Exit with code 1 if we didn't get proper shutdown *)
        (match server.state with
        | ShuttingDown -> 0
        | _ -> 1)
    | Error err ->
        info server (Printf.sprintf "Read error: %s" (Rpc.read_error_to_string err));
        1
    | Ok msg -> (
        debug server (Printf.sprintf "Received: %s" (Rpc.message_to_string msg));
        match process_message server msg with
        | `Continue -> loop ()
        | `Exit code -> code)
  in
  loop ()
