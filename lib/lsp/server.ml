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

(** Extract filename from a file:// URI.
    Returns the path portion, or the raw URI if not a file:// URI. *)
let filename_of_uri (uri : string) : string =
  if String.length uri > 7 && String.sub uri 0 7 = "file://" then
    String.sub uri 7 (String.length uri - 7)
  else
    uri

(** Convert a source location span to an LSP range.
    Note: Loc.span has 1-based lines and 0-based columns.
    LSP uses 0-based lines and 0-based characters (UTF-16 code units, but
    we approximate with bytes). *)
let range_of_span (span : Syntax.Location.span) : Protocol.range =
  {
    Protocol.start = {
      line = span.start_pos.line - 1;  (* Convert to 0-based *)
      character = span.start_pos.col;
    };
    end_ = {
      line = span.end_pos.line - 1;
      character = span.end_pos.col;
    };
  }

(** Convert a Typing.Diagnostic.t to an LSP diagnostic *)
let lsp_diagnostic_of_diagnostic (d : Typing.Diagnostic.t) : Protocol.diagnostic =
  let severity =
    match d.severity with
    | Typing.Diagnostic.Error -> Protocol.Error
    | Typing.Diagnostic.Warning -> Protocol.Warning
    | Typing.Diagnostic.Hint -> Protocol.Hint
  in
  {
    Protocol.range = range_of_span d.span;
    severity = Some severity;
    message = d.message;
    source = Some "tart";
  }

(** Convert a parse error to an LSP diagnostic *)
let lsp_diagnostic_of_parse_error (err : Syntax.Read.parse_error) : Protocol.diagnostic =
  {
    Protocol.range = range_of_span err.span;
    severity = Some Protocol.Error;
    message = err.message;
    source = Some "tart";
  }

(** Type-check a document and return LSP diagnostics.
    Returns parse errors if parsing fails, otherwise type errors. *)
let check_document (uri : string) (text : string) : Protocol.diagnostic list =
  let filename = filename_of_uri uri in
  let parse_result = Syntax.Read.parse_string ~filename text in
  (* Collect parse errors *)
  let parse_diagnostics = List.map lsp_diagnostic_of_parse_error parse_result.errors in
  (* If we have sexps, type-check them *)
  let type_diagnostics =
    if parse_result.sexps = [] then []
    else
      let check_result = Typing.Check.check_program parse_result.sexps in
      let typing_diagnostics = Typing.Diagnostic.of_unify_errors check_result.errors in
      List.map lsp_diagnostic_of_diagnostic typing_diagnostics
  in
  parse_diagnostics @ type_diagnostics

(** Publish diagnostics for a document *)
let publish_diagnostics (server : t) (uri : string) (version : int option) : unit =
  match Document.get_doc server.documents uri with
  | None -> ()
  | Some doc ->
      let diagnostics = check_document uri doc.text in
      let params : Protocol.publish_diagnostics_params =
        { uri; version; diagnostics }
      in
      debug server (Printf.sprintf "Publishing %d diagnostics for %s"
        (List.length diagnostics) uri);
      Rpc.write_notification server.oc
        ~method_:"textDocument/publishDiagnostics"
        ~params:(Protocol.publish_diagnostics_params_to_json params)

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
      debug server (Printf.sprintf "Opened document: %s (version %d)" uri version);
      (* Publish diagnostics for the newly opened document *)
      publish_diagnostics server uri (Some version)

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
            (Printf.sprintf "Changed document: %s (version %d)" uri version);
          (* Publish diagnostics for the changed document *)
          publish_diagnostics server uri (Some version)
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
      debug server (Printf.sprintf "Closed document: %s" uri);
      (* Clear diagnostics for the closed document *)
      let params : Protocol.publish_diagnostics_params =
        { uri; version = None; diagnostics = [] }
      in
      Rpc.write_notification server.oc
        ~method_:"textDocument/publishDiagnostics"
        ~params:(Protocol.publish_diagnostics_params_to_json params)

(** Get the type at a specific S-expression within a document.

    This re-type-checks the document and infers the type for the target sexp. *)
let type_at_sexp (env : Typing.Check.check_result) (target : Syntax.Sexp.t) : Core.Types.typ option =
  (* For now, use the simple approach: infer the type of the target expression directly *)
  let open Core.Types in
  reset_tvar_counter ();
  let result = Typing.Infer.infer env.Typing.Check.env target in
  let _ = Typing.Unify.solve result.Typing.Infer.constraints in
  Some (repr result.Typing.Infer.ty)

(** Handle textDocument/hover request *)
let handle_hover (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  match params with
  | None ->
      Error
        {
          Rpc.code = Rpc.invalid_params;
          message = "Missing hover params";
          data = None;
        }
  | Some json -> (
      let hover_params = Protocol.parse_hover_params json in
      let uri = hover_params.text_document in
      let line = hover_params.position.line in
      let col = hover_params.position.character in
      debug server
        (Printf.sprintf "Hover request at %s:%d:%d" uri line col);
      match Document.get_doc server.documents uri with
      | None ->
          debug server (Printf.sprintf "Document not found: %s" uri);
          Ok `Null
      | Some doc -> (
          let filename = filename_of_uri uri in
          let parse_result = Syntax.Read.parse_string ~filename doc.text in
          if parse_result.sexps = [] then (
            debug server "No S-expressions parsed";
            Ok `Null)
          else
            match
              Syntax.Sexp.find_at_position_in_forms ~line ~col
                parse_result.sexps
            with
            | None ->
                debug server "No S-expression at position";
                Ok `Null
            | Some target_sexp -> (
                debug server
                  (Printf.sprintf "Found sexp: %s"
                     (Syntax.Sexp.to_string target_sexp));
                (* Type-check the document to get the environment *)
                let check_result =
                  Typing.Check.check_program parse_result.sexps
                in
                match type_at_sexp check_result target_sexp with
                | None ->
                    debug server "Could not infer type";
                    Ok `Null
                | Some ty ->
                    let type_str = Core.Types.to_string ty in
                    debug server (Printf.sprintf "Type: %s" type_str);
                    let hover : Protocol.hover =
                      {
                        contents =
                          {
                            kind = Protocol.Markdown;
                            value =
                              Printf.sprintf "```elisp\n%s\n```" type_str;
                          };
                        range = Some (range_of_span (Syntax.Sexp.span_of target_sexp));
                      }
                    in
                    Ok (Protocol.hover_to_json hover))))

(** Dispatch a request to its handler *)
let dispatch_request (server : t) (msg : Rpc.message) :
    (Yojson.Safe.t, Rpc.response_error) result =
  debug server (Printf.sprintf "Request: %s" msg.method_);
  match msg.method_ with
  | "initialize" -> handle_initialize server msg.params
  | "shutdown" -> handle_shutdown server
  | "textDocument/hover" -> handle_hover server msg.params
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
