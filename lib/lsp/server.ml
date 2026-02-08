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
  mutable position_encoding : Protocol.position_encoding;
  mutable next_request_id : int;
      (** Monotonic counter for server-initiated request IDs *)
  documents : Document.t;
  form_cache : Form_cache.t;
  dependency_graph : Graph.Dependency_graph.t;
  signature_tracker : Signature_tracker.t;
  mutable module_config : Typing.Module_check.config;
      (** Module check config, rebuilt when settings change *)
  mutable emacs_version : Sig.Emacs_version.version option;
      (** Detected or overridden Emacs version *)
  last_diagnostics : (string, Protocol.diagnostic list) Hashtbl.t;
      (** Last published diagnostics per URI, for deduplication *)
  worker : Worker.t;  (** Background domain for async type checking *)
  mutable debounce_ms : int;
      (** Debounce delay in milliseconds before enqueuing type checks.
          Configurable via [tart.diagnostics.debounceMs]. Default 200. *)
  pending_checks : (string, float) Hashtbl.t;
      (** Pending debounced checks: URI → earliest-allowed-enqueue timestamp *)
  pending_requests : (string, bool Atomic.t) Hashtbl.t;
      (** In-flight request cancellation flags, keyed by JSON-serialised ID. Set
          to [true] by [$/cancelRequest]; checked by long-running handlers. *)
  mutable cancel_flag : bool Atomic.t;
      (** Cancellation flag for the currently-executing request. Set before
          dispatching each request; handlers call {!check_cancelled} to poll. *)
  mutable supports_work_done_progress : bool;
      (** Whether the client supports [window/workDoneProgress]. Parsed from
          [capabilities.window.workDoneProgress] during initialization. *)
  semantic_tokens_cache : Semantic_tokens.cache;
      (** Cache of previous semantic token arrays per URI, used for delta
          computation in [textDocument/semanticTokens/full/delta]. *)
}

exception Cancelled

(** Raise {!Cancelled} if the current request has been cancelled.

    Intended for periodic checks inside long-running request handlers so that
    [$/cancelRequest] can abort in-flight work. Reads the server's [cancel_flag]
    which is set before each request dispatch. *)
let check_cancelled (server : t) : unit =
  if Atomic.get server.cancel_flag then raise Cancelled

module Log = Tart_log.Log

(** Locate stdlib and typings root directories relative to the executable. *)
let discover_paths () : string option * string option =
  let exe_dir = Filename.dirname Sys.executable_name in
  let stdlib_candidates =
    [
      Filename.concat exe_dir "stdlib";
      Filename.concat (Filename.dirname exe_dir) "stdlib";
      Filename.concat exe_dir "../share/tart/stdlib";
    ]
  in
  let stdlib_dir = List.find_opt Sys.file_exists stdlib_candidates in
  let typings_candidates =
    [
      Filename.concat exe_dir "typings/emacs";
      Filename.concat (Filename.dirname exe_dir) "typings/emacs";
      Filename.concat exe_dir "../share/tart/typings/emacs";
      "typings/emacs";
    ]
  in
  let typings_root = List.find_opt Sys.file_exists typings_candidates in
  (stdlib_dir, typings_root)

(** Build module config from discovered paths and optional overrides.

    @param emacs_version Target Emacs version for versioned typings
    @param extra_search_dirs
      Additional directories to prepend to the search path *)
let build_config ?(emacs_version : Sig.Emacs_version.version option)
    ?(extra_search_dirs : string list = []) ~(stdlib_dir : string option)
    ~(typings_root : string option) () : Typing.Module_check.config =
  let config = Typing.Module_check.default_config () in
  let config =
    match stdlib_dir with
    | Some dir -> Typing.Module_check.with_stdlib dir config
    | None -> config
  in
  let search_path = Typing.Module_check.search_path config in
  let search_path =
    match typings_root with
    | Some root -> Sig.Search_path.with_typings_root root search_path
    | None -> search_path
  in
  let search_path =
    match emacs_version with
    | Some v -> Sig.Search_path.with_emacs_version v search_path
    | None -> search_path
  in
  let search_path =
    List.fold_right Sig.Search_path.prepend_dir extra_search_dirs search_path
  in
  Typing.Module_check.with_search_path search_path config

(** Detect Emacs version and build module config.

    Detects Emacs version once at server startup and builds the module config
    with appropriate versioned typings. *)
let detect_emacs_and_build_config () :
    Typing.Module_check.config * Sig.Emacs_version.version option =
  let version_result = Sig.Emacs_version.detect () in
  let emacs_version =
    match version_result with
    | Sig.Emacs_version.Detected v -> Some v
    | Sig.Emacs_version.NotFound | Sig.Emacs_version.ParseError _ -> None
  in
  let stdlib_dir, typings_root = discover_paths () in
  let config = build_config ?emacs_version ~stdlib_dir ~typings_root () in
  (config, emacs_version)

(** Create a new server on the given channels *)
let create ~ic ~oc ?(debounce_ms = 200) () : t =
  let module_config, emacs_version = detect_emacs_and_build_config () in
  {
    ic;
    oc;
    state = Uninitialized;
    position_encoding = Protocol.UTF16;
    next_request_id = 1;
    documents = Document.create ();
    form_cache = Form_cache.create ();
    dependency_graph = Graph.Dependency_graph.create ();
    signature_tracker = Signature_tracker.create ();
    module_config;
    emacs_version;
    last_diagnostics = Hashtbl.create 16;
    worker = Worker.create ();
    debounce_ms;
    pending_checks = Hashtbl.create 16;
    pending_requests = Hashtbl.create 8;
    cancel_flag = Atomic.make false;
    supports_work_done_progress = false;
    semantic_tokens_cache = Semantic_tokens.create_cache ();
  }

(** Get the server's current state *)
let state (server : t) : state = server.state

(** Get the server's document store (for testing) *)
let documents (server : t) : Document.t = server.documents

(** Get the server's dependency graph (for testing) *)
let dependency_graph (server : t) : Graph.Dependency_graph.t =
  server.dependency_graph

(** Get the server's signature tracker (for testing) *)
let signature_tracker (server : t) : Signature_tracker.t =
  server.signature_tracker

(** Get the detected Emacs version (for testing) *)
let emacs_version (server : t) : Sig.Emacs_version.version option =
  server.emacs_version

(** Get server capabilities *)
let capabilities () : Protocol.server_capabilities =
  {
    text_document_sync =
      Some { open_close = true; change = Protocol.Incremental; save = true };
    hover_provider = true;
    definition_provider = true;
    references_provider = true;
    code_action_provider = true;
    document_symbol_provider = true;
    completion_provider = true;
    signature_help_provider = true;
    rename_provider = Some { prepare_provider = true };
    folding_range_provider = true;
    semantic_tokens_provider = true;
    inlay_hint_provider = true;
    type_definition_provider = true;
    workspace_symbol_provider = true;
    call_hierarchy_provider = true;
    type_hierarchy_provider = true;
    code_lens_provider = true;
  }

(** Require non-None params, returning an invalid-params error otherwise. *)
let require_params (label : string) (params : Yojson.Safe.t option)
    (f : Yojson.Safe.t -> (Yojson.Safe.t, Rpc.response_error) result) :
    (Yojson.Safe.t, Rpc.response_error) result =
  match params with
  | None ->
      Error
        {
          Rpc.code = Rpc.invalid_params;
          message = "Missing " ^ label ^ " params";
          data = None;
        }
  | Some json -> f json

(** Look up a document by URI, returning [not_found] when absent. *)
let with_document (server : t) ~(uri : string) ~(not_found : Yojson.Safe.t)
    (f : Document.doc -> (Yojson.Safe.t, Rpc.response_error) result) :
    (Yojson.Safe.t, Rpc.response_error) result =
  match Document.get_doc server.documents uri with
  | None ->
      Log.debug "Document not found: %s" uri;
      Ok not_found
  | Some doc -> f doc

(** Parse a document and find the S-expression at the given cursor position,
    returning [not_found] when the document is empty or no sexp is at the
    cursor. *)
let with_sexp_at_cursor ~(doc : Document.doc) ~(uri : string) ~(line : int)
    ~(col : int) ~(not_found : Yojson.Safe.t)
    (f :
      Syntax.Read.parse_result ->
      Syntax.Sexp.position_context ->
      (Yojson.Safe.t, Rpc.response_error) result) :
    (Yojson.Safe.t, Rpc.response_error) result =
  let filename = Uri.to_filename uri in
  let parse_result = Syntax.Read.parse_string ~filename doc.text in
  if parse_result.sexps = [] then (
    Log.debug "No S-expressions parsed";
    Ok not_found)
  else
    match
      Syntax.Sexp.find_with_context_in_forms ~line ~col parse_result.sexps
    with
    | None ->
        Log.debug "No S-expression at position";
        Ok not_found
    | Some ctx -> f parse_result ctx

(** Read a file's contents, returning [""] on any error. *)
let read_file_safe (path : string) : string =
  try
    let ic = In_channel.open_text path in
    let content = In_channel.input_all ic in
    In_channel.close ic;
    content
  with _ -> ""

let range_of_span = Span_conv.range_of_span
let location_of_span = Span_conv.location_of_span

(** Publish pre-computed diagnostics for a document.

    Performs version staleness check and deduplication, then writes the
    notification to the output channel. Called from {!handle_worker_results}
    when the background domain delivers results. *)
let publish_diagnostics (server : t) (result : Worker.work_result) : unit =
  let uri = result.wr_uri in
  let version = Some result.wr_version in
  match Document.get_doc server.documents uri with
  | None -> ()
  | Some doc -> (
      (* Version staleness check: if the checked version doesn't match the
         document's current version, discard stale results. *)
      match version with
      | Some v when v <> doc.version ->
          Log.debug
            "Discarding stale diagnostics for %s (checked v%d, current v%d)" uri
            v doc.version
      | _ ->
          let all_diagnostics = result.wr_diagnostics in
          (* Log cache statistics at debug level *)
          (match result.wr_stats with
          | Some s ->
              Log.debug "Type check: %d forms total, %d cached, %d re-checked"
                s.total_forms s.cached_forms s.checked_forms
          | None -> ());
          (* Suppress identical diagnostics *)
          let dominated =
            match Hashtbl.find_opt server.last_diagnostics uri with
            | Some prev -> Protocol.diagnostics_equal prev all_diagnostics
            | None -> false
          in
          if dominated then
            Log.debug "Suppressing identical diagnostics for %s" uri
          else (
            Hashtbl.replace server.last_diagnostics uri all_diagnostics;
            let params : Protocol.publish_diagnostics_params =
              { uri; version; diagnostics = all_diagnostics }
            in
            Log.debug "Publishing %d diagnostics for %s"
              (List.length all_diagnostics)
              uri;
            Rpc.write_notification server.oc
              ~method_:"textDocument/publishDiagnostics"
              ~params:(Protocol.publish_diagnostics_params_to_json params)))

(** Process completed results from the background worker and publish
    diagnostics. Called from the main loop when the worker signal pipe is
    readable. *)
let handle_worker_results (server : t) : unit =
  let results = Worker.poll_results server.worker in
  List.iter (publish_diagnostics server) results

(** Immediately enqueue a diagnostic check for the given URI, bypassing the
    debounce timer. Used by [flush_pending_checks] once the deadline expires. *)
let enqueue_check (server : t) (uri : string) : unit =
  match Document.get_doc server.documents uri with
  | None -> ()
  | Some doc ->
      let item : Worker.work_item =
        {
          uri;
          text = doc.text;
          version = doc.version;
          config = server.module_config;
          cache = server.form_cache;
          sig_tracker = server.signature_tracker;
          dependency_graph = server.dependency_graph;
          is_tart = Signature_tracker.is_tart_file uri;
        }
      in
      Worker.enqueue server.worker item

(** Force-enqueue all pending checks regardless of deadline. Called at shutdown
    so debounced items are not lost. *)
let force_flush_pending_checks (server : t) : unit =
  let uris =
    Hashtbl.fold (fun uri _deadline acc -> uri :: acc) server.pending_checks []
  in
  Hashtbl.clear server.pending_checks;
  List.iter (enqueue_check server) uris

(** Block until the worker has no more pending or in-flight items, publishing
    results as they arrive. Called before shutdown to ensure all enqueued
    diagnostics are delivered. *)
let drain_worker (server : t) : unit =
  force_flush_pending_checks server;
  let worker_fd = Worker.signal_fd server.worker in
  while Worker.pending_count server.worker > 0 do
    (* Wait for the signal pipe with a short timeout to avoid deadlock *)
    let readable, _, _ = Unix.select [ worker_fd ] [] [] 0.1 in
    if readable <> [] then handle_worker_results server
  done;
  (* One final poll to pick up any results posted between the last
     pending_count check and now *)
  handle_worker_results server

(** Generate a unique progress token. *)
let next_progress_token (server : t) : string =
  let id = server.next_request_id in
  server.next_request_id <- id + 1;
  Printf.sprintf "tart-progress-%d" id

(** Send progress begin notification (no-op if client doesn't support it). *)
let send_progress_begin (server : t) ~(token : string) ~(title : string)
    ?(message : string option) () : unit =
  if server.supports_work_done_progress then (
    (* Create the progress token via window/workDoneProgress/create *)
    let create_id = server.next_request_id in
    server.next_request_id <- create_id + 1;
    Rpc.write_request server.oc ~id:create_id
      ~method_:"window/workDoneProgress/create"
      ~params:(Protocol.work_done_progress_create_json ~token);
    (* Send begin notification *)
    Rpc.write_notification server.oc ~method_:"$/progress"
      ~params:(Protocol.progress_begin_json ~token ~title ?message ()))

(** Send progress report notification (no-op if client doesn't support it). *)
let send_progress_report (server : t) ~(token : string)
    ?(message : string option) ?(percentage : int option) () : unit =
  if server.supports_work_done_progress then
    Rpc.write_notification server.oc ~method_:"$/progress"
      ~params:(Protocol.progress_report_json ~token ?message ?percentage ())

(** Send progress end notification (no-op if client doesn't support it). *)
let send_progress_end (server : t) ~(token : string) ?(message : string option)
    () : unit =
  if server.supports_work_done_progress then
    Rpc.write_notification server.oc ~method_:"$/progress"
      ~params:(Protocol.progress_end_json ~token ?message ())

(** Schedule a diagnostic check for the given URI.

    Records a debounce deadline; the check is enqueued by [flush_pending_checks]
    once the deadline expires. Each new call for the same URI resets the
    deadline so that rapid keystrokes produce only one check after the pause. *)
let schedule_check (server : t) (uri : string) : unit =
  let delay = Float.of_int server.debounce_ms /. 1000.0 in
  let deadline = Unix.gettimeofday () +. delay in
  Hashtbl.replace server.pending_checks uri deadline

(** Enqueue any pending checks whose debounce deadline has expired. Returns the
    time until the next deadline (in seconds), or [None] when no checks are
    pending. *)
let flush_pending_checks (server : t) : float option =
  let now = Unix.gettimeofday () in
  let ready = ref [] in
  let soonest = ref infinity in
  Hashtbl.iter
    (fun uri deadline ->
      if deadline <= now then ready := uri :: !ready
      else soonest := Float.min !soonest deadline)
    server.pending_checks;
  List.iter
    (fun uri ->
      Hashtbl.remove server.pending_checks uri;
      enqueue_check server uri)
    !ready;
  if Hashtbl.length server.pending_checks = 0 then None
  else Some (Float.max 0.001 (!soonest -. Unix.gettimeofday ()))

(** Apply user settings, rebuilding the module config and rechecking all open
    documents.

    Called from [initializationOptions] and [workspace/didChangeConfiguration].
*)
let apply_settings (server : t) (settings : Protocol.tart_settings) : unit =
  let new_version =
    match settings.ts_emacs_version with
    | Some s -> (
        match Sig.Emacs_version.parse_version s with
        | Some v ->
            Log.info "Settings: emacs version overridden to %s"
              (Sig.Emacs_version.version_to_string v);
            Some v
        | None ->
            Log.info "Settings: ignoring unparseable emacsVersion %S" s;
            server.emacs_version)
    | None -> server.emacs_version
  in
  (match settings.ts_debounce_ms with
  | Some ms ->
      Log.info "Settings: diagnostic debounce set to %d ms" ms;
      server.debounce_ms <- ms
  | None -> ());
  let extra_search_dirs = settings.ts_search_path in
  if extra_search_dirs <> [] then
    Log.info "Settings: prepending %d search path dirs"
      (List.length extra_search_dirs);
  let stdlib_dir, typings_root = discover_paths () in
  let new_config =
    build_config ?emacs_version:new_version ~extra_search_dirs ~stdlib_dir
      ~typings_root ()
  in
  server.module_config <- new_config;
  server.emacs_version <- new_version;
  (* Invalidate all caches and re-publish diagnostics for every open document *)
  Form_cache.invalidate_all server.form_cache;
  let open_uris = Document.list_uris server.documents in
  let n = List.length open_uris in
  let token =
    if n > 0 then (
      let tok = next_progress_token server in
      send_progress_begin server ~token:tok ~title:"Checking"
        ~message:(Printf.sprintf "Rechecking %d files" n)
        ();
      Some tok)
    else None
  in
  List.iteri
    (fun i uri ->
      (match token with
      | Some tok ->
          let pct = (i + 1) * 100 / n in
          let basename = Filename.basename (Uri.to_filename uri) in
          send_progress_report server ~token:tok ~message:basename
            ~percentage:pct ()
      | None -> ());
      schedule_check server uri)
    open_uris;
  match token with
  | Some tok -> send_progress_end server ~token:tok ()
  | None -> ()

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
              capabilities =
                { text_document = None; general = None; window = None };
              initialization_options = None;
            }
      in
      Log.info "Initializing (root: %s)"
        (Option.value init_params.root_uri ~default:"<none>");
      (* Log detected Emacs version at debug level (R7 of Spec 24) *)
      (match server.emacs_version with
      | Some v ->
          Log.debug "Detected Emacs version: %s"
            (Sig.Emacs_version.version_to_string v)
      | None -> Log.debug "Emacs version not detected, using latest typings");
      (* Parse work-done progress support *)
      (match init_params.capabilities.window with
      | Some w -> server.supports_work_done_progress <- w.work_done_progress
      | None -> ());
      (* Negotiate position encoding *)
      let encoding =
        Protocol.negotiate_position_encoding init_params.capabilities
      in
      server.position_encoding <- encoding;
      Log.debug "Position encoding: %s"
        (Protocol.position_encoding_to_string encoding);
      server.state <- Initialized { root_uri = init_params.root_uri };
      (* Apply initializationOptions if provided *)
      (match init_params.initialization_options with
      | Some json ->
          let settings = Protocol.parse_tart_settings json in
          apply_settings server settings
      | None -> ());
      let result : Protocol.initialize_result =
        { capabilities = capabilities (); position_encoding = encoding }
      in
      let server_info : Protocol.server_info =
        { name = "tart"; version = Some "0.1.0" }
      in
      Ok (Protocol.initialize_response_to_json ~result ~server_info)

(** Handle initialized notification.

    Registers dynamic file watchers for [.el] and [.tart] files via
    [client/registerCapability]. The client may ignore this if it doesn't
    support dynamic registration. *)
let handle_initialized (server : t) : unit =
  Log.info "Client confirmed initialization";
  (* Register file watchers for .el and .tart files *)
  let id = server.next_request_id in
  server.next_request_id <- id + 1;
  let params = Protocol.register_file_watchers_json ~id:(string_of_int id) in
  Rpc.write_request server.oc ~id ~method_:"client/registerCapability" ~params;
  Log.debug "Registered file watchers (request id %d)" id

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
      Log.info "Shutting down";
      server.state <- ShuttingDown;
      Ok `Null

(** Handle exit notification *)
let handle_exit (server : t) : [ `Exit of int ] =
  match server.state with
  | ShuttingDown ->
      Log.info "Exiting (clean shutdown)";
      `Exit 0
  | _ ->
      Log.info "Exiting (no shutdown request)";
      `Exit 1

(** Invalidate caches and re-publish diagnostics for all open documents that
    depend on the module identified by [uri]. *)
let invalidate_dependents (server : t) ~uri : unit =
  let module_id = Graph_tracker.module_id_of_uri uri in
  let open_uris = Document.list_uris server.documents in
  let dependent_uris =
    Graph_tracker.dependent_uris server.dependency_graph ~module_id ~open_uris
  in
  let n = List.length dependent_uris in
  if n > 0 then (
    Log.debug "Invalidating %d dependents of %s" n module_id;
    let token =
      if n >= 3 then (
        let tok = next_progress_token server in
        send_progress_begin server ~token:tok ~title:"Checking dependents"
          ~message:(Printf.sprintf "%d files" n)
          ();
        Some tok)
      else None
    in
    List.iteri
      (fun i dep_uri ->
        (match token with
        | Some tok ->
            let pct = (i + 1) * 100 / n in
            let basename = Filename.basename (Uri.to_filename dep_uri) in
            send_progress_report server ~token:tok ~message:basename
              ~percentage:pct ()
        | None -> ());
        Form_cache.invalidate_document server.form_cache dep_uri;
        schedule_check server dep_uri)
      dependent_uris;
    match token with
    | Some tok -> send_progress_end server ~token:tok ()
    | None -> ())

(** Handle textDocument/didOpen notification *)
let handle_did_open (server : t) (params : Yojson.Safe.t option) : unit =
  match params with
  | None -> Log.debug "didOpen missing params"
  | Some json ->
      let open Yojson.Safe.Util in
      let text_document = json |> member "textDocument" in
      let uri, version, text =
        Document.text_document_item_of_json text_document
      in
      Document.open_doc server.documents ~uri ~version ~text;
      (* Update dependency graph *)
      Graph_tracker.update_document server.dependency_graph ~uri ~text;
      (* Track .tart files in signature tracker *)
      if Signature_tracker.is_tart_file uri then
        Signature_tracker.set server.signature_tracker ~uri ~text;
      Log.debug "Opened document: %s (version %d)" uri version;
      (* Publish diagnostics for the newly opened document *)
      schedule_check server uri

(** Handle textDocument/didChange notification *)
let handle_did_change (server : t) (params : Yojson.Safe.t option) : unit =
  match params with
  | None -> Log.debug "didChange missing params"
  | Some json -> (
      let open Yojson.Safe.Util in
      let text_document = json |> member "textDocument" in
      let uri, version =
        Document.versioned_text_document_identifier_of_json text_document
      in
      let changes =
        json |> member "contentChanges" |> to_list
        |> List.map Document.content_change_of_json
      in
      match Document.apply_changes server.documents ~uri ~version changes with
      | Ok { warning } ->
          (match warning with Some msg -> Log.info "%s" msg | None -> ());
          Log.debug "Changed document: %s (version %d)" uri version;
          (* Update dependency graph with new document content *)
          (match Document.get_doc server.documents uri with
          | Some doc ->
              Graph_tracker.update_document server.dependency_graph ~uri
                ~text:doc.text;
              (* Update signature tracker for .tart files *)
              if Signature_tracker.is_tart_file uri then
                Signature_tracker.set server.signature_tracker ~uri
                  ~text:doc.text
          | None -> ());
          (* Publish diagnostics for the changed document *)
          schedule_check server uri;
          invalidate_dependents server ~uri
      | Error e -> Log.info "Error applying changes to %s: %s" uri e)

(** Handle textDocument/didSave notification.

    Forces a full re-check by invalidating the form cache for the saved
    document, then re-publishes diagnostics and cascades to dependents. *)
let handle_did_save (server : t) (params : Yojson.Safe.t option) : unit =
  match params with
  | None -> Log.debug "didSave missing params"
  | Some json ->
      let open Yojson.Safe.Util in
      let text_document = json |> member "textDocument" in
      let uri = Document.text_document_identifier_of_json text_document in
      Log.debug "Saved document: %s" uri;
      (* Invalidate form cache to force a full re-check *)
      Form_cache.invalidate_document server.form_cache uri;
      (* Re-publish diagnostics with a fresh check *)
      schedule_check server uri;
      (* Cascade to dependents *)
      invalidate_dependents server ~uri

(** Handle workspace/didChangeWatchedFiles notification.

    For each file event, skips URIs that are currently open (buffer is source of
    truth). For changed/created/deleted [.tart] files, invalidates dependents so
    the next type-check re-reads from disk. *)
let handle_did_change_watched_files (server : t) (params : Yojson.Safe.t option)
    : unit =
  match params with
  | None -> Log.debug "didChangeWatchedFiles missing params"
  | Some json ->
      let dcwf = Protocol.parse_did_change_watched_files_params json in
      List.iter
        (fun (event : Protocol.file_event) ->
          let uri = event.fe_uri in
          (* Skip open documents — the editor buffer is source of truth *)
          match Document.get_doc server.documents uri with
          | Some _ ->
              Log.debug "Ignoring watched file event for open URI: %s" uri
          | None -> (
              match event.fe_type with
              | Protocol.Changed | Protocol.Created ->
                  if Signature_tracker.is_tart_file uri then (
                    Log.debug "Watched .tart file changed: %s" uri;
                    invalidate_dependents server ~uri)
                  else Log.debug "Ignoring watched .el file change: %s" uri
              | Protocol.Deleted ->
                  Log.debug "Watched file deleted: %s" uri;
                  invalidate_dependents server ~uri))
        dcwf.dcwf_changes

(** Handle workspace/didChangeConfiguration notification.

    Parses the settings payload and applies it, rebuilding the module config and
    rechecking all open documents. *)
let handle_did_change_configuration (server : t) (params : Yojson.Safe.t option)
    : unit =
  match params with
  | None -> Log.debug "didChangeConfiguration missing params"
  | Some json ->
      let dcc = Protocol.parse_did_change_configuration_params json in
      let settings = Protocol.parse_tart_settings dcc.dcc_settings in
      Log.info "Configuration changed";
      apply_settings server settings

(** Handle textDocument/didClose notification *)
let handle_did_close (server : t) (params : Yojson.Safe.t option) : unit =
  match params with
  | None -> Log.debug "didClose missing params"
  | Some json ->
      let open Yojson.Safe.Util in
      let text_document = json |> member "textDocument" in
      let uri = Document.text_document_identifier_of_json text_document in
      (* If it's a .tart file, remove from signature tracker and re-check dependents *)
      let is_tart = Signature_tracker.is_tart_file uri in
      if is_tart then (
        Signature_tracker.remove server.signature_tracker uri;
        invalidate_dependents server ~uri);
      Document.close_doc server.documents ~uri;
      (* Also clear the form cache for this document *)
      Form_cache.remove_document server.form_cache uri;
      (* Clear semantic tokens cache for this document *)
      Semantic_tokens.invalidate server.semantic_tokens_cache uri;
      (* Keep graph entry on close - file still exists on disk (R4) *)
      Graph_tracker.close_document server.dependency_graph ~uri;
      Log.debug "Closed document: %s" uri;
      (* Clear diagnostics for the closed document *)
      Hashtbl.remove server.last_diagnostics uri;
      let params : Protocol.publish_diagnostics_params =
        { uri; version = None; diagnostics = [] }
      in
      Rpc.write_notification server.oc
        ~method_:"textDocument/publishDiagnostics"
        ~params:(Protocol.publish_diagnostics_params_to_json params)

(** Get the type at a specific S-expression within a document.

    This infers the type for the target sexp. If the target is in function
    position of an application, the enclosing application is inferred first to
    resolve instantiated type variables.

    Handles errors gracefully:
    - If unification fails, returns the best-effort type (before solving)
    - If type inference itself fails, returns None
    - Never raises exceptions *)
let type_at_sexp (env : Typing.Check.check_result)
    (ctx : Syntax.Sexp.position_context) : Core.Types.typ option =
  let open Core.Types in
  reset_tvar_counter ();

  (* Helper to safely infer and solve, returning best-effort type on error.
     Even if unification fails, we return the inferred type so the user
     gets some information. *)
  let safe_infer_and_solve infer_fn =
    try
      let result = infer_fn () in
      (* Try to solve constraints - if it fails, we still have the inferred type *)
      (match Typing.Unify.solve result.Typing.Infer.constraints with
      | Ok () -> ()
      | Error _ -> ());
      Some (repr result.Typing.Infer.ty)
    with _ ->
      (* On any exception during inference, return None *)
      None
  in

  (* Determine what to infer based on context *)
  match ctx.enclosing_application with
  | Some (Syntax.Sexp.List (fn :: args, _span))
    when Syntax.Sexp.equal fn ctx.target || true ->
      (* Target is in function position of an application.
         We need to infer the whole application so type variables are resolved.
         Then extract the function type. *)
      safe_infer_and_solve (fun () ->
          let fn_result = Typing.Infer.infer env.Typing.Check.env fn in
          let arg_results =
            List.map (Typing.Infer.infer env.Typing.Check.env) args
          in

          (* Build the application constraint *)
          let result_ty = fresh_tvar 0 in
          let arg_types =
            List.map (fun r -> PPositional r.Typing.Infer.ty) arg_results
          in
          let expected_fn_type = TArrow (arg_types, result_ty) in
          let fn_constraint =
            Typing.Constraint.equal fn_result.Typing.Infer.ty expected_fn_type
              _span
          in

          (* Combine all constraints *)
          let all_constraints =
            List.fold_left
              (fun acc r ->
                Typing.Constraint.combine acc r.Typing.Infer.constraints)
              (Typing.Constraint.add fn_constraint
                 fn_result.Typing.Infer.constraints)
              arg_results
          in

          (* Return combined result - solve happens in safe_infer_and_solve *)
          {
            Typing.Infer.ty = fn_result.Typing.Infer.ty;
            constraints = all_constraints;
            undefineds = fn_result.Typing.Infer.undefineds;
            clause_diagnostics = fn_result.Typing.Infer.clause_diagnostics;
          })
  | _ ->
      (* Not in an application context, just infer the target directly *)
      safe_infer_and_solve (fun () ->
          Typing.Infer.infer env.Typing.Check.env ctx.target)

(** Handle textDocument/hover request *)
let handle_hover (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "hover" params @@ fun json ->
  let hover_params = Protocol.parse_hover_params json in
  let uri = hover_params.text_document in
  let line = hover_params.position.line in
  Log.debug "Hover request at %s:%d:%d" uri line hover_params.position.character;
  with_document server ~uri ~not_found:`Null @@ fun doc ->
  let col =
    Document.utf16_col_to_byte ~text:doc.text ~line
      ~col:hover_params.position.character
  in
  with_sexp_at_cursor ~doc ~uri ~line ~col ~not_found:`Null
  @@ fun parse_result ctx ->
  Log.debug "Found sexp: %s (in application: %b)"
    (Syntax.Sexp.to_string ctx.target)
    (Option.is_some ctx.enclosing_application);
  check_cancelled server;
  (* Type-check the document to get the environment *)
  let check_result = Typing.Check.check_program parse_result.sexps in
  match type_at_sexp check_result ctx with
  | None ->
      Log.debug "Could not infer type";
      Ok `Null
  | Some ty ->
      let type_str = Core.Types.to_string ty in
      Log.debug "Type: %s" type_str;
      let hover : Protocol.hover =
        {
          contents =
            {
              kind = Protocol.Markdown;
              value = Printf.sprintf "```elisp\n%s\n```" type_str;
            };
          range =
            Some (range_of_span ~text:doc.text (Syntax.Sexp.span_of ctx.target));
        }
      in
      Ok (Protocol.hover_to_json hover)

(** Extract definition locations (defun, defvar, defconst) from parsed sexps.

    Returns a list of (name, span) pairs for all definitions in the document.
    The span points to the beginning of the definition form. *)
let extract_definitions (sexps : Syntax.Sexp.t list) :
    (string * Syntax.Location.span) list =
  let open Syntax.Sexp in
  List.filter_map
    (fun sexp ->
      match sexp with
      | List (Symbol ("defun", _) :: Symbol (name, _) :: _, span) ->
          Some (name, span)
      | List
          ( (Symbol ("defvar", _) | Symbol ("defconst", _))
            :: Symbol (name, _)
            :: _,
            span ) ->
          Some (name, span)
      | _ -> None)
    sexps

(** Find definition in signature file declarations.

    Returns the location span if the name is declared in the signature. *)
let find_definition_in_signature (name : string)
    (sig_ast : Sig.Sig_ast.signature) : Syntax.Location.span option =
  List.find_map
    (fun decl ->
      match decl with
      | Sig.Sig_ast.DDefun d when d.defun_name = name -> Some d.defun_loc
      | Sig.Sig_ast.DDefvar d when d.defvar_name = name -> Some d.defvar_loc
      | _ -> None)
    sig_ast.sig_decls

(** Find a type definition in signature file declarations.

    Returns the location span if a type with the given name is declared in the
    signature (DType, DData, or DImportStruct). *)
let find_type_definition_in_signature (name : string)
    (sig_ast : Sig.Sig_ast.signature) : Syntax.Location.span option =
  List.find_map
    (fun decl ->
      match decl with
      | Sig.Sig_ast.DType d when d.type_name = name -> Some d.type_loc
      | Sig.Sig_ast.DData d when d.data_name = name -> Some d.data_loc
      | Sig.Sig_ast.DImportStruct d when d.struct_name = name ->
          Some d.struct_loc
      | _ -> None)
    sig_ast.sig_decls

(** Look up a type definition by its TCon name in loaded signatures.

    Given a TCon name like "mymod/person", splits on "/", resolves the module
    via the search path, and looks for a matching DType, DData, or
    DImportStruct. Returns None for intrinsic or prelude types. *)
let find_type_definition_in_signatures ~(config : Typing.Module_check.config)
    (tcon_name : string) : Syntax.Location.span option =
  (* Skip intrinsics *)
  if Core.Types.is_intrinsic_name tcon_name then None (* Skip prelude types *)
  else if String.length tcon_name > 8 && String.sub tcon_name 0 8 = "prelude."
  then None
  else
    (* Split on "/" to get module_name and type_name *)
    match String.index_opt tcon_name '/' with
    | None -> None
    | Some idx -> (
        let module_name = String.sub tcon_name 0 idx in
        let type_name =
          String.sub tcon_name (idx + 1) (String.length tcon_name - idx - 1)
        in
        (* Find the module's .tart file *)
        let search_path = Typing.Module_check.search_path config in
        match Sig.Search_path.find_signature search_path module_name with
        | Some sig_path -> (
            match Sig.Search_path.parse_signature_file sig_path with
            | Some sig_ast ->
                find_type_definition_in_signature type_name sig_ast
            | None -> None)
        | None -> None)

(** Search for a definition by prefix-based module lookup.

    Tries each prefix of the symbol name (e.g. ["foo-bar"; "foo"] for
    "foo-bar-baz") against the search path, returning the first match. *)
let find_definition_by_prefix ~(config : Typing.Module_check.config)
    (name : string) : Syntax.Location.span option =
  let prefixes = Typing.Module_check.extract_module_prefixes name in
  let search_path = Typing.Module_check.search_path config in
  let rec try_prefixes = function
    | [] -> None
    | prefix :: rest -> (
        match Sig.Search_path.find_signature search_path prefix with
        | Some sig_path -> (
            match Sig.Search_path.parse_signature_file sig_path with
            | Some sig_ast -> (
                match find_definition_in_signature name sig_ast with
                | Some span -> Some span
                | None -> try_prefixes rest)
            | None -> try_prefixes rest)
        | None -> try_prefixes rest)
  in
  try_prefixes prefixes

(** Look up a symbol definition in loaded signatures.

    Searches the sibling .tart file and any signatures from the search path.
    Returns the location if found. *)
let find_definition_in_signatures ~(config : Typing.Module_check.config)
    ~(filename : string) (name : string) : Syntax.Location.span option =
  let dir = Filename.dirname filename in
  let basename = Filename.basename filename in
  let module_name =
    if Filename.check_suffix basename ".el" then
      Filename.chop_suffix basename ".el"
    else basename
  in
  (* First try the sibling .tart file *)
  let tart_path = Filename.concat dir (module_name ^ ".tart") in
  match Sig.Search_path.parse_signature_file tart_path with
  | Some sig_ast -> (
      match find_definition_in_signature name sig_ast with
      | Some span -> Some span
      | None -> find_definition_by_prefix ~config name)
  | None -> find_definition_by_prefix ~config name

(** Extract the symbol name from an S-expression, if it is a symbol. *)
let symbol_name_of_sexp (sexp : Syntax.Sexp.t) : string option =
  match sexp with Syntax.Sexp.Symbol (name, _) -> Some name | _ -> None

(** Find all references to a symbol in the parsed sexps.

    Walks the entire S-expression tree and collects all occurrences of the given
    symbol name. Returns a list of spans where the symbol appears. *)
let find_references (name : string) (sexps : Syntax.Sexp.t list) :
    Syntax.Location.span list =
  let open Syntax.Sexp in
  let rec collect_sexp acc sexp =
    match sexp with
    | Symbol (n, span) when n = name -> span :: acc
    | Symbol _ -> acc
    | Int _ | Float _ | String _ | Char _ | Keyword _ | Error _ -> acc
    | List (elems, _) -> List.fold_left collect_sexp acc elems
    | Vector (elems, _) -> List.fold_left collect_sexp acc elems
    | Curly (elems, _) -> List.fold_left collect_sexp acc elems
    | Cons (car, cdr, _) -> collect_sexp (collect_sexp acc car) cdr
  in
  List.fold_left collect_sexp [] sexps |> List.rev

(** Find a definition in another open [.el] document.

    Searches all open documents (excluding [origin_uri] and [.tart] files) for a
    top-level [defun], [defvar], or [defconst] matching [name]. Returns the
    first match as an LSP location. *)
let find_definition_in_workspace (server : t) ~(origin_uri : string)
    (name : string) : Protocol.location option =
  let uris = Document.list_uris server.documents in
  let other_uris =
    List.filter
      (fun u -> u <> origin_uri && not (Signature_tracker.is_tart_file u))
      uris
  in
  List.find_map
    (fun uri ->
      match Document.get_doc server.documents uri with
      | None -> None
      | Some doc -> (
          let filename = Uri.to_filename uri in
          let parse_result = Syntax.Read.parse_string ~filename doc.text in
          let defs = extract_definitions parse_result.sexps in
          match List.assoc_opt name defs with
          | Some span -> Some (location_of_span ~text:doc.text span)
          | None -> None))
    other_uris

(** Handle textDocument/definition request *)
let handle_definition (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "definition" params @@ fun json ->
  let def_params = Protocol.parse_definition_params json in
  let uri = def_params.def_text_document in
  let line = def_params.def_position.line in
  Log.debug "Definition request at %s:%d:%d" uri line
    def_params.def_position.character;
  let def_null = Protocol.definition_result_to_json Protocol.DefNull in
  with_document server ~uri ~not_found:def_null @@ fun doc ->
  let col =
    Document.utf16_col_to_byte ~text:doc.text ~line
      ~col:def_params.def_position.character
  in
  with_sexp_at_cursor ~doc ~uri ~line ~col ~not_found:def_null
  @@ fun parse_result ctx ->
  let filename = Uri.to_filename uri in
  (* Extract symbol name from the target sexp *)
  match symbol_name_of_sexp ctx.target with
  | None ->
      Log.debug "Target is not a symbol";
      Ok (Protocol.definition_result_to_json Protocol.DefNull)
  | Some name -> (
      Log.debug "Looking for definition of: %s" name;
      (* Look up definition in the current document first *)
      let definitions = extract_definitions parse_result.sexps in
      match List.assoc_opt name definitions with
      | Some span ->
          Log.debug "Found local definition at %s:%d:%d" span.start_pos.file
            span.start_pos.line span.start_pos.col;
          let loc = location_of_span ~text:doc.text span in
          Ok (Protocol.definition_result_to_json (Protocol.DefLocation loc))
      | None -> (
          (* Not found locally - try signature lookup (R14) *)
          Log.debug "Not found locally, trying signature lookup for: %s" name;
          match
            find_definition_in_signatures ~config:server.module_config ~filename
              name
          with
          | Some span ->
              Log.debug "Found definition in signature at %s:%d:%d"
                span.start_pos.file span.start_pos.line span.start_pos.col;
              let target_text = read_file_safe span.start_pos.file in
              let loc = location_of_span ~text:target_text span in
              Ok (Protocol.definition_result_to_json (Protocol.DefLocation loc))
          | None -> (
              (* Not found in signatures - try other open .el files *)
              Log.debug
                "Not found in signatures, trying other open .el files for: %s"
                name;
              match
                find_definition_in_workspace server ~origin_uri:uri name
              with
              | Some loc ->
                  Log.debug "Found definition in another .el file";
                  Ok
                    (Protocol.definition_result_to_json
                       (Protocol.DefLocation loc))
              | None ->
                  Log.debug "No definition found for: %s" name;
                  Ok (Protocol.definition_result_to_json Protocol.DefNull))))

(** Handle textDocument/typeDefinition request.

    Infers the type at the cursor. If it resolves to a named type (TCon) defined
    in a .tart file, returns that type's declaration location. Returns null for
    primitive, prelude, and non-named types. *)
let handle_type_definition (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "type definition" params @@ fun json ->
  let def_params = Protocol.parse_type_definition_params json in
  let uri = def_params.def_text_document in
  let line = def_params.def_position.line in
  Log.debug "Type definition request at %s:%d:%d" uri line
    def_params.def_position.character;
  let def_null = Protocol.definition_result_to_json Protocol.DefNull in
  with_document server ~uri ~not_found:def_null @@ fun doc ->
  let col =
    Document.utf16_col_to_byte ~text:doc.text ~line
      ~col:def_params.def_position.character
  in
  with_sexp_at_cursor ~doc ~uri ~line ~col ~not_found:def_null
  @@ fun parse_result ctx ->
  (* Type-check the document to get the environment *)
  let check_result = Typing.Check.check_program parse_result.sexps in
  match type_at_sexp check_result ctx with
  | None ->
      Log.debug "Could not infer type";
      Ok (Protocol.definition_result_to_json Protocol.DefNull)
  | Some ty -> (
      (* Extract TCon name from the inferred type *)
      let extract_tcon_name (t : Core.Types.typ) : string option =
        match Core.Types.repr t with
        | Core.Types.TCon name -> Some name
        | Core.Types.TApp (Core.Types.TCon name, _) -> Some name
        | _ -> None
      in
      match extract_tcon_name ty with
      | None ->
          Log.debug "Type is not a named type: %s" (Core.Types.to_string ty);
          Ok (Protocol.definition_result_to_json Protocol.DefNull)
      | Some tcon_name -> (
          Log.debug "Looking for type definition of: %s" tcon_name;
          match
            find_type_definition_in_signatures ~config:server.module_config
              tcon_name
          with
          | Some span ->
              Log.debug "Found type definition at %s:%d:%d" span.start_pos.file
                span.start_pos.line span.start_pos.col;
              let target_text = read_file_safe span.start_pos.file in
              let loc = location_of_span ~text:target_text span in
              Ok (Protocol.definition_result_to_json (Protocol.DefLocation loc))
          | None ->
              Log.debug "No type definition found for: %s" tcon_name;
              Ok (Protocol.definition_result_to_json Protocol.DefNull)))

(** Find all references to a symbol across all open documents.

    Iterates every open document, parses its sexps, and collects all occurrences
    of [name]. Returns a flat list of LSP locations spanning multiple files. The
    [origin_uri] document appears first in results. *)
let find_references_in_workspace (server : t) ~(origin_uri : string)
    (name : string) : Protocol.location list =
  let uris = Document.list_uris server.documents in
  (* Put origin document first so its results come first *)
  let sorted_uris =
    let origin, rest = List.partition (fun u -> u = origin_uri) uris in
    origin @ rest
  in
  List.concat_map
    (fun uri ->
      match Document.get_doc server.documents uri with
      | None -> []
      | Some doc ->
          if Signature_tracker.is_tart_file uri then []
          else
            let filename = Uri.to_filename uri in
            let parse_result = Syntax.Read.parse_string ~filename doc.text in
            let ref_spans = find_references name parse_result.sexps in
            List.map (location_of_span ~text:doc.text) ref_spans)
    sorted_uris

(** Handle textDocument/references request *)
let handle_references (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "references" params @@ fun json ->
  let ref_params = Protocol.parse_references_params json in
  let uri = ref_params.ref_text_document in
  let line = ref_params.ref_position.line in
  Log.debug "References request at %s:%d:%d" uri line
    ref_params.ref_position.character;
  let ref_null = Protocol.references_result_to_json None in
  with_document server ~uri ~not_found:ref_null @@ fun doc ->
  let col =
    Document.utf16_col_to_byte ~text:doc.text ~line
      ~col:ref_params.ref_position.character
  in
  with_sexp_at_cursor ~doc ~uri ~line ~col ~not_found:ref_null
  @@ fun _parse_result ctx ->
  (* Extract symbol name from the target sexp *)
  match symbol_name_of_sexp ctx.target with
  | None ->
      Log.debug "Target is not a symbol";
      Ok (Protocol.references_result_to_json None)
  | Some name ->
      Log.debug "Finding references to: %s" name;
      (* Find all references across all open documents *)
      let locations =
        find_references_in_workspace server ~origin_uri:uri name
      in
      Log.debug "Found %d references across workspace" (List.length locations);
      Ok (Protocol.references_result_to_json (Some locations))

(** Handle textDocument/codeAction request.

    Delegates to {!Code_action} module for quickfixes and refactorings. *)
let handle_code_action (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "code action" params @@ fun json ->
  let ca_params = Protocol.parse_code_action_params json in
  let uri = ca_params.ca_text_document in
  let range = ca_params.ca_range in
  let context = ca_params.ca_context in
  Log.debug "Code action request at %s:%d:%d-%d:%d" uri range.start.line
    range.start.character range.end_.line range.end_.character;
  Log.debug "Context has %d diagnostics" (List.length context.cac_diagnostics);

  with_document server ~uri
    ~not_found:(Protocol.code_action_result_to_json (Some []))
  @@ fun doc ->
  let to_byte line col = Document.utf16_col_to_byte ~text:doc.text ~line ~col in
  let range : Protocol.range =
    {
      start =
        {
          line = range.start.line;
          character = to_byte range.start.line range.start.character;
        };
      end_ =
        {
          line = range.end_.line;
          character = to_byte range.end_.line range.end_.character;
        };
    }
  in
  Code_action.handle
    ~range_of_span:(range_of_span ~text:doc.text)
    ~config:server.module_config ~uri ~doc_text:doc.text ~range ~context

(** {1 Document Symbols} *)

(** Extract a symbol from a definition form.

    For defun, extracts function with type from environment if available. For
    defvar/defconst, extracts variable/constant with its span. *)
let rec extract_symbol_from_def ~(text : string) (sexp : Syntax.Sexp.t) :
    Protocol.document_symbol option =
  let open Syntax.Sexp in
  match sexp with
  | List (Symbol ("defun", _) :: Symbol (name, name_span) :: args :: body, span)
    ->
      (* Get the selection range from the name symbol *)
      let selection_range = range_of_span ~text name_span in
      (* Extract parameter names for detail *)
      let param_detail =
        match args with
        | List (params, _) ->
            let param_strs =
              List.filter_map
                (function
                  | Symbol (p, _) when not (String.length p > 0 && p.[0] = '&')
                    ->
                      Some p
                  | _ -> None)
                params
            in
            if param_strs = [] then "()"
            else "(" ^ String.concat " " param_strs ^ ")"
        | _ -> "()"
      in
      (* Look for nested defuns in the body *)
      let children =
        List.filter_map
          (extract_symbol_from_def ~text)
          (List.concat_map (function List (elems, _) -> elems | _ -> []) body)
      in
      Some
        {
          Protocol.ds_name = name;
          ds_detail = Some param_detail;
          ds_kind = Protocol.SKFunction;
          ds_range = range_of_span ~text span;
          ds_selection_range = selection_range;
          ds_children = children;
        }
  | List (Symbol ("defvar", _) :: Symbol (name, name_span) :: _, span) ->
      let selection_range = range_of_span ~text name_span in
      Some
        {
          Protocol.ds_name = name;
          ds_detail = None;
          ds_kind = Protocol.SKVariable;
          ds_range = range_of_span ~text span;
          ds_selection_range = selection_range;
          ds_children = [];
        }
  | List (Symbol ("defconst", _) :: Symbol (name, name_span) :: _, span) ->
      let selection_range = range_of_span ~text name_span in
      Some
        {
          Protocol.ds_name = name;
          ds_detail = None;
          ds_kind = Protocol.SKConstant;
          ds_range = range_of_span ~text span;
          ds_selection_range = selection_range;
          ds_children = [];
        }
  | List (Symbol ("defmacro", _) :: Symbol (name, name_span) :: args :: _, span)
    ->
      let selection_range = range_of_span ~text name_span in
      let param_detail =
        match args with
        | List (params, _) ->
            let param_strs =
              List.filter_map
                (function
                  | Symbol (p, _) when not (String.length p > 0 && p.[0] = '&')
                    ->
                      Some p
                  | _ -> None)
                params
            in
            if param_strs = [] then "()"
            else "(" ^ String.concat " " param_strs ^ ")"
        | _ -> "()"
      in
      Some
        {
          Protocol.ds_name = name;
          ds_detail = Some param_detail;
          ds_kind = Protocol.SKMethod;
          (* Use Method for macros to distinguish from functions *)
          ds_range = range_of_span ~text span;
          ds_selection_range = selection_range;
          ds_children = [];
        }
  | _ -> None

(** Handle textDocument/documentSymbol request.

    Returns a list of symbols (functions, variables, constants, macros) defined
    in the document, with their types as details when available. *)
let handle_document_symbol (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "document symbol" params @@ fun json ->
  let dsp_params = Protocol.parse_document_symbol_params json in
  let uri = dsp_params.dsp_text_document in
  Log.debug "Document symbol request for %s" uri;
  with_document server ~uri
    ~not_found:(Protocol.document_symbol_result_to_json None)
  @@ fun doc ->
  let filename = Uri.to_filename uri in
  let parse_result = Syntax.Read.parse_string ~filename doc.text in
  if parse_result.sexps = [] then (
    Log.debug "No S-expressions parsed";
    Ok (Protocol.document_symbol_result_to_json (Some [])))
  else
    let symbols =
      List.filter_map
        (extract_symbol_from_def ~text:doc.text)
        parse_result.sexps
    in
    Log.debug "Found %d symbols" (List.length symbols);
    Ok (Protocol.document_symbol_result_to_json (Some symbols))

(** Handle textDocument/completion request.

    Delegates to {!Completion} module. *)
let handle_completion (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "completion" params @@ fun json ->
  let cp_params = Protocol.parse_completion_params json in
  let uri = cp_params.cp_text_document in
  let line = cp_params.cp_position.line in
  Log.debug "Completion request at %s:%d:%d" uri line
    cp_params.cp_position.character;
  with_document server ~uri ~not_found:(Protocol.completion_result_to_json None)
  @@ fun doc ->
  let col =
    Document.utf16_col_to_byte ~text:doc.text ~line
      ~col:cp_params.cp_position.character
  in
  Completion.handle ~config:server.module_config ~uri ~doc_text:doc.text ~line
    ~col

(** Handle textDocument/signatureHelp request.

    Delegates to {!Signature_help} module. *)
let handle_signature_help (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "signature help" params @@ fun json ->
  let sh_params = Protocol.parse_signature_help_params json in
  let uri = sh_params.shp_text_document in
  let line = sh_params.shp_position.line in
  Log.debug "Signature help request at %s:%d:%d" uri line
    sh_params.shp_position.character;
  with_document server ~uri
    ~not_found:(Protocol.signature_help_result_to_json None)
  @@ fun doc ->
  let col =
    Document.utf16_col_to_byte ~text:doc.text ~line
      ~col:sh_params.shp_position.character
  in
  Signature_help.handle ~config:server.module_config ~uri ~doc_text:doc.text
    ~line ~col

(** Handle textDocument/foldingRange request.

    Delegates to {!Folding} module. *)
let handle_folding_range (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "folding range" params @@ fun json ->
  let frp = Protocol.parse_folding_range_params json in
  let uri = frp.frp_text_document in
  Log.debug "Folding range request for %s" uri;
  with_document server ~uri
    ~not_found:(Protocol.folding_range_result_to_json None)
  @@ fun doc -> Folding.handle ~uri ~doc_text:doc.text

(** Handle textDocument/semanticTokens/full request.

    Delegates to {!Semantic_tokens} module. Caches the result for subsequent
    delta requests. *)
let handle_semantic_tokens (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "semantic tokens" params @@ fun json ->
  let stp = Protocol.parse_semantic_tokens_params json in
  let uri = stp.stp_text_document in
  Log.debug "Semantic tokens request for %s" uri;
  with_document server ~uri
    ~not_found:(Protocol.semantic_tokens_result_to_json None)
  @@ fun doc ->
  Semantic_tokens.handle ~uri ~doc_text:doc.text
    ~cache:server.semantic_tokens_cache

(** Handle textDocument/semanticTokens/full/delta request.

    Computes a delta from the previously cached token array, or falls back to a
    full response when the cache misses. *)
let handle_semantic_tokens_delta (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "semantic tokens delta" params @@ fun json ->
  let p = Protocol.parse_semantic_tokens_delta_params json in
  let uri = p.stdp_text_document in
  Log.debug "Semantic tokens delta request for %s (prev %s)" uri
    p.stdp_previous_result_id;
  with_document server ~uri
    ~not_found:(Protocol.semantic_tokens_delta_response_to_json None)
  @@ fun doc ->
  Semantic_tokens.handle_delta ~uri ~doc_text:doc.text
    ~previous_result_id:p.stdp_previous_result_id
    ~cache:server.semantic_tokens_cache

(** Handle textDocument/inlayHint request.

    Returns inline type annotations for [defun] return types and [let]/[let*]
    bindings within the requested range. *)
let handle_inlay_hints (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "inlay hint" params @@ fun json ->
  let ihp = Protocol.parse_inlay_hint_params json in
  let uri = ihp.ihp_text_document in
  Log.debug "Inlay hint request for %s" uri;
  with_document server ~uri ~not_found:(Protocol.inlay_hint_result_to_json None)
  @@ fun doc -> Inlay_hints.handle ~uri ~doc_text:doc.text ~range:ihp.ihp_range

(** {1 Rename} *)

(** Handle textDocument/prepareRename request.

    Validates whether the symbol at the cursor can be renamed. Returns the
    symbol's range and name as placeholder for renameable user-defined symbols,
    or null for builtins, keywords, literals, and comments. *)
let handle_prepare_rename (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "prepare rename" params @@ fun json ->
  let prp = Protocol.parse_prepare_rename_params json in
  let uri = prp.prp_text_document in
  let line = prp.prp_position.line in
  Log.debug "Prepare rename request at %s:%d:%d" uri line
    prp.prp_position.character;
  with_document server ~uri ~not_found:`Null @@ fun doc ->
  let col =
    Document.utf16_col_to_byte ~text:doc.text ~line
      ~col:prp.prp_position.character
  in
  with_sexp_at_cursor ~doc ~uri ~line ~col ~not_found:`Null
  @@ fun _parse_result ctx ->
  match ctx.target with
  | Syntax.Sexp.Symbol (name, span)
    when not (Code_action.is_builtin_symbol name) ->
      Log.debug "Prepare rename: symbol '%s' is renameable" name;
      let result : Protocol.prepare_rename_result =
        {
          prr_range = range_of_span ~text:doc.text span;
          prr_placeholder = name;
        }
      in
      Ok (Protocol.prepare_rename_result_to_json (Some result))
  | _ ->
      Log.debug "Prepare rename: target is not a renameable symbol";
      Ok (Protocol.prepare_rename_result_to_json None)

(** Handle textDocument/rename request.

    Renames all occurrences of a symbol across all open documents. Uses
    {!find_references_in_workspace} to locate occurrences workspace-wide and
    generates text edits grouped by document URI. *)
let handle_rename (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "rename" params @@ fun json ->
  let rename_params = Protocol.parse_rename_params json in
  let uri = rename_params.rp_text_document in
  let line = rename_params.rp_position.line in
  let new_name = rename_params.rp_new_name in
  Log.debug "Rename request at %s:%d:%d -> '%s'" uri line
    rename_params.rp_position.character new_name;
  let rename_null = Protocol.rename_result_to_json None in
  with_document server ~uri ~not_found:rename_null @@ fun doc ->
  let col =
    Document.utf16_col_to_byte ~text:doc.text ~line
      ~col:rename_params.rp_position.character
  in
  with_sexp_at_cursor ~doc ~uri ~line ~col ~not_found:rename_null
  @@ fun _parse_result ctx ->
  (* Extract symbol name from the target sexp *)
  match symbol_name_of_sexp ctx.target with
  | None ->
      Log.debug "Target is not a symbol";
      Ok (Protocol.rename_result_to_json None)
  | Some name ->
      Log.debug "Renaming '%s' to '%s'" name new_name;
      (* Find all references across all open documents *)
      let locations =
        find_references_in_workspace server ~origin_uri:uri name
      in
      Log.debug "Found %d occurrences to rename" (List.length locations);
      if locations = [] then Ok (Protocol.rename_result_to_json None)
      else
        (* Group locations by URI *)
        let by_uri = Hashtbl.create 16 in
        List.iter
          (fun (loc : Protocol.location) ->
            let existing =
              match Hashtbl.find_opt by_uri loc.uri with
              | Some edits -> edits
              | None -> []
            in
            let edit : Protocol.text_edit =
              { te_range = loc.range; new_text = new_name }
            in
            Hashtbl.replace by_uri loc.uri (edit :: existing))
          locations;
        (* Build text_document_edit for each document *)
        let document_changes =
          Hashtbl.fold
            (fun doc_uri edits acc ->
              let doc_edit : Protocol.text_document_edit =
                {
                  tde_uri = doc_uri;
                  tde_version = None;
                  edits = List.rev edits;
                }
              in
              doc_edit :: acc)
            by_uri []
        in
        let workspace_edit : Protocol.workspace_edit = { document_changes } in
        Ok (Protocol.rename_result_to_json (Some workspace_edit))

(** Handle workspace/symbol request.

    Returns symbols matching the query across all open documents. Delegates to
    {!Workspace_symbols}. *)
let handle_workspace_symbol (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "workspace symbol" params @@ fun json ->
  let ws_params = Protocol.parse_workspace_symbol_params json in
  Log.debug "Workspace symbol request: query=%S" ws_params.ws_query;
  Workspace_symbols.handle ~documents:server.documents ~query:ws_params.ws_query

(** {1 Call Hierarchy} *)

(** Handle textDocument/prepareCallHierarchy request.

    Returns a [CallHierarchyItem] for the function at the cursor, or null if the
    cursor is not on a function name. *)
let handle_call_hierarchy_prepare (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "prepare call hierarchy" params @@ fun json ->
  let p = Protocol.parse_call_hierarchy_prepare_params json in
  let uri = p.chpp_text_document in
  let line = p.chpp_position.line in
  Log.debug "Prepare call hierarchy at %s:%d:%d" uri line
    p.chpp_position.character;
  let null = Protocol.call_hierarchy_prepare_result_to_json None in
  with_document server ~uri ~not_found:null @@ fun doc ->
  let col =
    Document.utf16_col_to_byte ~text:doc.text ~line
      ~col:p.chpp_position.character
  in
  with_sexp_at_cursor ~doc ~uri ~line ~col ~not_found:null
  @@ fun parse_result _ctx ->
  (* Find the defun at the cursor position *)
  let defuns =
    Call_hierarchy.extract_defun_infos ~uri ~text:doc.text parse_result.sexps
  in
  let matching =
    List.find_opt
      (fun (d : Call_hierarchy.defun_info) ->
        let sr = d.selection_range in
        (line > sr.start.line
        || (line = sr.start.line && col >= sr.start.character))
        && (line < sr.end_.line
           || (line = sr.end_.line && col <= sr.end_.character)))
      defuns
  in
  match matching with
  | None ->
      Log.debug "No function definition at cursor";
      Ok null
  | Some d ->
      Log.debug "Found function: %s" d.name;
      let item : Protocol.call_hierarchy_item =
        {
          chi_name = d.name;
          chi_kind = Protocol.SKFunction;
          chi_uri = uri;
          chi_range = d.range;
          chi_selection_range = d.selection_range;
          chi_data = None;
        }
      in
      Ok (Protocol.call_hierarchy_prepare_result_to_json (Some [ item ]))

(** Handle callHierarchy/incomingCalls request.

    Finds all callers of the target function across all open [.el] documents. *)
let handle_incoming_calls (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "incoming calls" params @@ fun json ->
  let item = Protocol.parse_incoming_calls_params json in
  let target_name = item.chi_name in
  Log.debug "Incoming calls for: %s" target_name;
  let uris = Document.list_uris server.documents in
  let calls =
    List.concat_map
      (fun uri ->
        match Document.get_doc server.documents uri with
        | None -> []
        | Some doc ->
            if Signature_tracker.is_tart_file uri then []
            else
              let filename = Uri.to_filename uri in
              let parse_result = Syntax.Read.parse_string ~filename doc.text in
              let defuns =
                Call_hierarchy.extract_defun_infos ~uri ~text:doc.text
                  parse_result.sexps
              in
              List.filter_map
                (fun (d : Call_hierarchy.defun_info) ->
                  let call_spans =
                    Call_hierarchy.find_call_sites ~callee:target_name d.body
                  in
                  if call_spans = [] then None
                  else
                    let from_ranges =
                      List.map (range_of_span ~text:doc.text) call_spans
                    in
                    Some
                      {
                        Protocol.chic_from =
                          {
                            chi_name = d.name;
                            chi_kind = Protocol.SKFunction;
                            chi_uri = uri;
                            chi_range = d.range;
                            chi_selection_range = d.selection_range;
                            chi_data = None;
                          };
                        chic_from_ranges = from_ranges;
                      })
                defuns)
      uris
  in
  Log.debug "Found %d incoming callers" (List.length calls);
  Ok (Protocol.call_hierarchy_incoming_calls_result_to_json (Some calls))

(** Handle callHierarchy/outgoingCalls request.

    Finds all functions called from the body of the target function. *)
let handle_outgoing_calls (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "outgoing calls" params @@ fun json ->
  let item = Protocol.parse_outgoing_calls_params json in
  let target_uri = item.chi_uri in
  let target_name = item.chi_name in
  Log.debug "Outgoing calls for: %s in %s" target_name target_uri;
  let null = Protocol.call_hierarchy_outgoing_calls_result_to_json (Some []) in
  with_document server ~uri:target_uri ~not_found:null @@ fun doc ->
  let filename = Uri.to_filename target_uri in
  let parse_result = Syntax.Read.parse_string ~filename doc.text in
  let defuns =
    Call_hierarchy.extract_defun_infos ~uri:target_uri ~text:doc.text
      parse_result.sexps
  in
  match
    List.find_opt
      (fun (d : Call_hierarchy.defun_info) -> d.name = target_name)
      defuns
  with
  | None ->
      Log.debug "Function %s not found in %s" target_name target_uri;
      Ok null
  | Some defun ->
      let callees = Call_hierarchy.find_callees_in_body defun.body in
      (* Group call sites by callee name *)
      let by_name = Hashtbl.create 16 in
      List.iter
        (fun (name, span) ->
          let existing =
            match Hashtbl.find_opt by_name name with
            | Some spans -> spans
            | None -> []
          in
          Hashtbl.replace by_name name (span :: existing))
        callees;
      (* Build outgoing call items. Try to resolve each callee to a defun_info
         across all open documents for accurate range info. *)
      let all_uris = Document.list_uris server.documents in
      let calls =
        Hashtbl.fold
          (fun callee_name spans acc ->
            let from_ranges =
              List.map (range_of_span ~text:doc.text) (List.rev spans)
            in
            (* Try to resolve callee definition *)
            let callee_item =
              List.find_map
                (fun u ->
                  match Document.get_doc server.documents u with
                  | None -> None
                  | Some d ->
                      if Signature_tracker.is_tart_file u then None
                      else
                        let fn = Uri.to_filename u in
                        let pr = Syntax.Read.parse_string ~filename:fn d.text in
                        let defs =
                          Call_hierarchy.extract_defun_infos ~uri:u ~text:d.text
                            pr.sexps
                        in
                        List.find_opt
                          (fun (di : Call_hierarchy.defun_info) ->
                            di.name = callee_name)
                          defs)
                all_uris
            in
            let to_item : Protocol.call_hierarchy_item =
              match callee_item with
              | Some ci ->
                  {
                    chi_name = ci.name;
                    chi_kind = Protocol.SKFunction;
                    chi_uri = ci.uri;
                    chi_range = ci.range;
                    chi_selection_range = ci.selection_range;
                    chi_data = None;
                  }
              | None ->
                  (* Callee not found as a defun — use a synthetic item with
                     the first call site as the range *)
                  let first_range = List.hd from_ranges in
                  {
                    chi_name = callee_name;
                    chi_kind = Protocol.SKFunction;
                    chi_uri = target_uri;
                    chi_range = first_range;
                    chi_selection_range = first_range;
                    chi_data = None;
                  }
            in
            { Protocol.choc_to = to_item; choc_from_ranges = from_ranges }
            :: acc)
          by_name []
      in
      Log.debug "Found %d outgoing callees" (List.length calls);
      Ok (Protocol.call_hierarchy_outgoing_calls_result_to_json (Some calls))

(** {1 Type Hierarchy} *)

(** A zero-width range at position (0, 0), used for synthetic type hierarchy
    items that represent built-in types without source locations. *)
let zero_range : Protocol.range =
  { start = { line = 0; character = 0 }; end_ = { line = 0; character = 0 } }

(** Handle typeHierarchy/prepare request.

    Infers the type at the cursor. If it resolves to a named type (TCon),
    returns a [TypeHierarchyItem]. Returns null for non-named types. *)
let handle_type_hierarchy_prepare (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "prepare type hierarchy" params @@ fun json ->
  let p = Protocol.parse_type_hierarchy_prepare_params json in
  let uri = p.thpp_text_document in
  let line = p.thpp_position.line in
  Log.debug "Prepare type hierarchy at %s:%d:%d" uri line
    p.thpp_position.character;
  let null = Protocol.type_hierarchy_prepare_result_to_json None in
  with_document server ~uri ~not_found:null @@ fun doc ->
  let col =
    Document.utf16_col_to_byte ~text:doc.text ~line
      ~col:p.thpp_position.character
  in
  with_sexp_at_cursor ~doc ~uri ~line ~col ~not_found:null
  @@ fun parse_result ctx ->
  (* Type-check to get the inferred type *)
  let check_result = Typing.Check.check_program parse_result.sexps in
  match type_at_sexp check_result ctx with
  | None ->
      Log.debug "Could not infer type";
      Ok null
  | Some ty -> (
      let extract_tcon_name (t : Core.Types.typ) : string option =
        match Core.Types.repr t with
        | Core.Types.TCon name -> Some name
        | Core.Types.TApp (Core.Types.TCon name, _) -> Some name
        | _ -> None
      in
      match extract_tcon_name ty with
      | None ->
          Log.debug "Type is not a named type: %s" (Core.Types.to_string ty);
          Ok null
      | Some tcon_name ->
          Log.debug "Found type: %s" tcon_name;
          let display_name = Core.Types.intrinsic_base_name tcon_name in
          (* Try to find source location *)
          let item_range, item_sel_range, item_uri =
            match
              find_type_definition_in_signatures ~config:server.module_config
                tcon_name
            with
            | Some span ->
                let target_text = read_file_safe span.start_pos.file in
                let r = range_of_span ~text:target_text span in
                let file_uri = "file://" ^ span.start_pos.file in
                (r, r, file_uri)
            | None -> (zero_range, zero_range, uri)
          in
          let item : Protocol.type_hierarchy_item =
            {
              thi_name = display_name;
              thi_kind = Protocol.SKClass;
              thi_uri = item_uri;
              thi_range = item_range;
              thi_selection_range = item_sel_range;
              thi_data = Some (`String tcon_name);
            }
          in
          Ok (Protocol.type_hierarchy_prepare_result_to_json (Some [ item ])))

(** Build a type hierarchy item for a type info, using its type definition
    location if available. *)
let type_info_to_item ~(config : Typing.Module_check.config)
    ~(fallback_uri : string) (info : Type_hierarchy.type_info) :
    Protocol.type_hierarchy_item =
  let item_range, item_sel_range, item_uri =
    match find_type_definition_in_signatures ~config info.name with
    | Some span ->
        let target_text = read_file_safe span.start_pos.file in
        let r = range_of_span ~text:target_text span in
        let file_uri = "file://" ^ span.start_pos.file in
        (r, r, file_uri)
    | None -> (zero_range, zero_range, fallback_uri)
  in
  {
    Protocol.thi_name = info.display_name;
    thi_kind = Protocol.SKClass;
    thi_uri = item_uri;
    thi_range = item_range;
    thi_selection_range = item_sel_range;
    thi_data = Some (`String info.name);
  }

(** Handle typeHierarchy/supertypes request.

    Returns the supertypes of the given type. For Int and Float, returns Num. *)
let handle_supertypes (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "supertypes" params @@ fun json ->
  let item = Protocol.parse_supertypes_params json in
  let tcon_name =
    match item.thi_data with Some (`String name) -> name | _ -> item.thi_name
  in
  Log.debug "Supertypes for: %s" tcon_name;
  let supertypes = Type_hierarchy.supertypes_of tcon_name in
  let items =
    List.map
      (type_info_to_item ~config:server.module_config ~fallback_uri:item.thi_uri)
      supertypes
  in
  Log.debug "Found %d supertypes" (List.length items);
  Ok (Protocol.type_hierarchy_supertypes_result_to_json (Some items))

(** Handle typeHierarchy/subtypes request.

    Returns the subtypes of the given type. For Num, returns Int and Float. *)
let handle_subtypes (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "subtypes" params @@ fun json ->
  let item = Protocol.parse_subtypes_params json in
  let tcon_name =
    match item.thi_data with Some (`String name) -> name | _ -> item.thi_name
  in
  Log.debug "Subtypes for: %s" tcon_name;
  let subtypes = Type_hierarchy.subtypes_of tcon_name in
  let items =
    List.map
      (type_info_to_item ~config:server.module_config ~fallback_uri:item.thi_uri)
      subtypes
  in
  Log.debug "Found %d subtypes" (List.length items);
  Ok (Protocol.type_hierarchy_subtypes_result_to_json (Some items))

(** {1 Code Lens} *)

(** Handle textDocument/codeLens request.

    Returns code lenses for all top-level definitions showing reference counts
    and signature availability. *)
let handle_code_lens (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  require_params "code lens" params @@ fun json ->
  let p = Protocol.parse_code_lens_params json in
  let uri = p.clp_text_document in
  Log.debug "Code lens request for %s" uri;
  let null = Protocol.code_lens_result_to_json (Some []) in
  with_document server ~uri ~not_found:null @@ fun doc ->
  let filename = Uri.to_filename uri in
  let parse_result = Syntax.Read.parse_string ~filename doc.text in
  let defs = Code_lens.extract_definitions ~text:doc.text parse_result.sexps in
  let lenses =
    List.concat_map
      (fun (def : Code_lens.definition_info) ->
        (* Reference count lens *)
        let ref_count =
          let locations =
            find_references_in_workspace server ~origin_uri:uri def.name
          in
          List.length locations
        in
        let ref_title =
          if ref_count = 1 then "1 reference"
          else Printf.sprintf "%d references" ref_count
        in
        let ref_lens : Protocol.code_lens =
          {
            cl_range = def.range;
            cl_command =
              Some
                {
                  cmd_title = ref_title;
                  cmd_command = "tart.showReferences";
                  cmd_arguments =
                    [
                      `String uri;
                      `Assoc
                        [
                          ("line", `Int def.selection_range.start.line);
                          ("character", `Int def.selection_range.start.character);
                        ];
                    ];
                };
          }
        in
        (* Signature status lens *)
        let has_signature =
          find_definition_in_signatures ~config:server.module_config ~filename
            def.name
          |> Option.is_some
        in
        let sig_title =
          if has_signature then "signature \xE2\x9C\x93"
          else "missing signature"
        in
        let sig_lens : Protocol.code_lens =
          {
            cl_range = def.range;
            cl_command =
              Some
                { cmd_title = sig_title; cmd_command = ""; cmd_arguments = [] };
          }
        in
        [ ref_lens; sig_lens ])
      defs
  in
  Log.debug "Returning %d code lenses" (List.length lenses);
  Ok (Protocol.code_lens_result_to_json (Some lenses))

(** Dispatch a request to its handler *)
let dispatch_request (server : t) (msg : Rpc.message) :
    (Yojson.Safe.t, Rpc.response_error) result =
  Log.debug "Request: %s" msg.method_;
  match msg.method_ with
  | "initialize" -> handle_initialize server msg.params
  | "shutdown" -> handle_shutdown server
  | "textDocument/hover" -> handle_hover server msg.params
  | "textDocument/definition" -> handle_definition server msg.params
  | "textDocument/references" -> handle_references server msg.params
  | "textDocument/codeAction" -> handle_code_action server msg.params
  | "textDocument/documentSymbol" -> handle_document_symbol server msg.params
  | "textDocument/completion" -> handle_completion server msg.params
  | "textDocument/signatureHelp" -> handle_signature_help server msg.params
  | "textDocument/prepareRename" -> handle_prepare_rename server msg.params
  | "textDocument/rename" -> handle_rename server msg.params
  | "textDocument/foldingRange" -> handle_folding_range server msg.params
  | "textDocument/semanticTokens/full" ->
      handle_semantic_tokens server msg.params
  | "textDocument/semanticTokens/full/delta" ->
      handle_semantic_tokens_delta server msg.params
  | "textDocument/inlayHint" -> handle_inlay_hints server msg.params
  | "textDocument/typeDefinition" -> handle_type_definition server msg.params
  | "workspace/symbol" -> handle_workspace_symbol server msg.params
  | "textDocument/prepareCallHierarchy" ->
      handle_call_hierarchy_prepare server msg.params
  | "callHierarchy/incomingCalls" -> handle_incoming_calls server msg.params
  | "callHierarchy/outgoingCalls" -> handle_outgoing_calls server msg.params
  | "textDocument/prepareTypeHierarchy" ->
      handle_type_hierarchy_prepare server msg.params
  | "typeHierarchy/supertypes" -> handle_supertypes server msg.params
  | "typeHierarchy/subtypes" -> handle_subtypes server msg.params
  | "textDocument/codeLens" -> handle_code_lens server msg.params
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
  Log.debug "Notification: %s" msg.method_;
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
  | "textDocument/didSave" ->
      handle_did_save server msg.params;
      `Continue
  | "workspace/didChangeWatchedFiles" ->
      handle_did_change_watched_files server msg.params;
      `Continue
  | "workspace/didChangeConfiguration" ->
      handle_did_change_configuration server msg.params;
      `Continue
  | "$/cancelRequest" ->
      (match msg.params with
      | Some json -> (
          let open Yojson.Safe.Util in
          let id = json |> member "id" in
          let key = Yojson.Safe.to_string id in
          match Hashtbl.find_opt server.pending_requests key with
          | Some flag ->
              Atomic.set flag true;
              Log.debug "Cancelled request %s" key
          | None -> Log.debug "Cancel request for unknown id %s" key)
      | None -> Log.debug "$/cancelRequest missing params");
      `Continue
  | _ ->
      (* Ignore unknown notifications per LSP spec *)
      Log.debug "Ignoring unknown notification: %s" msg.method_;
      `Continue

(** Process a single message and optionally send a response *)
let process_message (server : t) (msg : Rpc.message) :
    [ `Continue | `Exit of int ] =
  match msg.id with
  | Some id ->
      (* Request - register cancellation flag *)
      let key = Yojson.Safe.to_string id in
      let cancel_flag = Atomic.make false in
      server.cancel_flag <- cancel_flag;
      Hashtbl.replace server.pending_requests key cancel_flag;
      let response =
        (* Check if already cancelled before dispatching *)
        if Atomic.get cancel_flag then
          {
            Rpc.id;
            result = None;
            error =
              Some
                {
                  Rpc.code = Rpc.request_cancelled;
                  message = "Request cancelled";
                  data = None;
                };
          }
        else
          try
            match dispatch_request server msg with
            | Ok result -> Rpc.success_response ~id ~result
            | Error err -> { Rpc.id; result = None; error = Some err }
          with Cancelled ->
            {
              Rpc.id;
              result = None;
              error =
                Some
                  {
                    Rpc.code = Rpc.request_cancelled;
                    message = "Request cancelled";
                    data = None;
                  };
            }
      in
      Hashtbl.remove server.pending_requests key;
      Log.debug "Response: %s" (Rpc.response_to_string response);
      Rpc.write_response server.oc response;
      `Continue
  | None ->
      (* Notification - no response *)
      dispatch_notification server msg

(** Main server loop.

    Reads messages from stdin with a background reader thread while the main
    thread multiplexes between stdin messages and worker results using
    [Unix.select].

    Architecture:
    - A reader thread pulls JSON-RPC messages from the In_channel and pushes
      them through a pipe as serialised JSON.
    - The main loop uses [Unix.select] on the reader pipe + worker signal pipe,
      avoiding the In_channel buffering vs. select deadlock. *)
let run (server : t) : int =
  Log.info "Starting LSP server";
  let worker_fd = Worker.signal_fd server.worker in
  (* We use the simple approach: read stdin synchronously, but between
     messages always poll for worker results. The trade-off is that
     worker results are delivered only when the next stdin message
     arrives, or at shutdown. To keep responsiveness, we also poll on
     just the worker pipe with a short timeout when waiting for stdin. *)

  (* Use a mutex-protected queue to pass messages from a reader thread
     and a pipe to signal the main select loop. *)
  let msg_pipe_r, msg_pipe_w = Unix.pipe () in
  let msg_queue : (Rpc.message, Rpc.read_error) result Queue.t =
    Queue.create ()
  in
  let msg_mutex = Mutex.create () in

  (* Reader thread: pulls messages from In_channel and pushes them *)
  let _reader_thread =
    Thread.create
      (fun () ->
        let running = ref true in
        while !running do
          let msg = Rpc.read_message server.ic in
          Mutex.lock msg_mutex;
          Queue.push msg msg_queue;
          Mutex.unlock msg_mutex;
          (* Wake the main select loop *)
          let _n = Unix.write msg_pipe_w (Bytes.of_string "m") 0 1 in
          match msg with Error _ -> running := false | Ok _ -> ()
        done)
      ()
  in

  (* Drain signal bytes from message pipe *)
  let drain_msg_pipe () =
    let buf = Bytes.create 64 in
    (try
       Unix.set_nonblock msg_pipe_r;
       while Unix.read msg_pipe_r buf 0 64 > 0 do
         ()
       done
     with
     | Unix.Unix_error (Unix.EAGAIN, _, _)
     | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
     ->
       ());
    Unix.clear_nonblock msg_pipe_r
  in

  let rec loop () =
    (* Flush any expired debounced checks and compute timeout for select *)
    let timeout =
      match flush_pending_checks server with Some t -> t | None -> -1.0
    in
    let readable, _, _ = Unix.select [ msg_pipe_r; worker_fd ] [] [] timeout in
    (* Process worker results *)
    if List.mem worker_fd readable then handle_worker_results server;
    (* Process stdin messages *)
    if List.mem msg_pipe_r readable then (
      drain_msg_pipe ();
      Mutex.lock msg_mutex;
      let msgs = Queue.fold (fun acc m -> m :: acc) [] msg_queue in
      Queue.clear msg_queue;
      Mutex.unlock msg_mutex;
      process_queued_msgs (List.rev msgs))
    else loop ()
  and process_queued_msgs = function
    | [] -> loop ()
    | msg :: rest -> (
        match msg with
        | Error Rpc.Eof -> (
            Log.info "Client disconnected";
            match server.state with ShuttingDown -> finish 0 | _ -> finish 1)
        | Error err ->
            Log.info "Read error: %s" (Rpc.read_error_to_string err);
            finish 1
        | Ok msg -> (
            Log.debug "Received: %s" (Rpc.message_to_string msg);
            match process_message server msg with
            | `Continue -> process_queued_msgs rest
            | `Exit code -> finish code))
  and finish code =
    (* Drain any remaining worker results before shutdown so all pending
       diagnostics are published. *)
    drain_worker server;
    Worker.shutdown server.worker;
    Unix.close msg_pipe_r;
    Unix.close msg_pipe_w;
    code
  in
  loop ()
