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
  form_cache : Form_cache.t;
  dependency_graph : Graph.Dependency_graph.t;
  signature_tracker : Signature_tracker.t;
  module_config : Typing.Module_check.config;
      (** Module check config, computed once at server creation *)
  emacs_version : Sig.Emacs_version.version option;
      (** Detected Emacs version, if any *)
}

and log_level = Quiet | Normal | Debug

(** Detect Emacs version and build module config.

    Detects Emacs version once at server startup and builds the module config
    with appropriate versioned typings. *)
let detect_emacs_and_build_config () :
    Typing.Module_check.config * Sig.Emacs_version.version option =
  (* Detect Emacs version *)
  let version_result = Sig.Emacs_version.detect () in
  let emacs_version =
    match version_result with
    | Sig.Emacs_version.Detected v -> Some v
    | Sig.Emacs_version.NotFound | Sig.Emacs_version.ParseError _ -> None
  in

  (* Try to find stdlib/typings relative to the executable *)
  let exe_dir = Filename.dirname Sys.executable_name in
  let stdlib_candidates =
    [
      Filename.concat exe_dir "stdlib";
      Filename.concat (Filename.dirname exe_dir) "stdlib";
      Filename.concat exe_dir "../share/tart/stdlib";
    ]
  in
  let stdlib_dir = List.find_opt Sys.file_exists stdlib_candidates in

  (* Find typings root for versioned c-core.
     Search relative to executable, parent of executable, share/tart,
     and current working directory (for development/testing). *)
  let typings_candidates =
    [
      Filename.concat exe_dir "typings/emacs";
      Filename.concat (Filename.dirname exe_dir) "typings/emacs";
      Filename.concat exe_dir "../share/tart/typings/emacs";
      "typings/emacs";
      (* Current working directory *)
    ]
  in
  let typings_root = List.find_opt Sys.file_exists typings_candidates in

  (* Build config with all components *)
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
  (* Rebuild config with updated search path *)
  let config = Typing.Module_check.with_search_path search_path config in
  (config, emacs_version)

(** Create a new server on the given channels *)
let create ?(log_level = Normal) ~ic ~oc () : t =
  let module_config, emacs_version = detect_emacs_and_build_config () in
  {
    ic;
    oc;
    state = Uninitialized;
    log_level;
    documents = Document.create ();
    form_cache = Form_cache.create ();
    dependency_graph = Graph.Dependency_graph.create ();
    signature_tracker = Signature_tracker.create ();
    module_config;
    emacs_version;
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
    definition_provider = true;
    references_provider = true;
    code_action_provider = true;
    document_symbol_provider = true;
    completion_provider = true;
    signature_help_provider = true;
    rename_provider = true;
  }

(** Extract filename from a file:// URI. Returns the path portion, or the raw
    URI if not a file:// URI. *)
let filename_of_uri (uri : string) : string =
  if String.length uri > 7 && String.sub uri 0 7 = "file://" then
    String.sub uri 7 (String.length uri - 7)
  else uri

(** Convert a source location span to an LSP range. Note: Loc.span has 1-based
    lines and 0-based columns. LSP uses 0-based lines and 0-based characters
    (UTF-16 code units, but we approximate with bytes). *)
let range_of_span (span : Syntax.Location.span) : Protocol.range =
  {
    Protocol.start =
      {
        line = span.start_pos.line - 1;
        (* Convert to 0-based *)
        character = span.start_pos.col;
      };
    end_ = { line = span.end_pos.line - 1; character = span.end_pos.col };
  }

(** Convert a source location span to an LSP location.

    Creates a location with a file:// URI from the span's file path. *)
let location_of_span (span : Syntax.Location.span) : Protocol.location =
  let uri = "file://" ^ span.start_pos.file in
  { Protocol.uri; range = range_of_span span }

(** Convert a related location to LSP DiagnosticRelatedInformation.

    Skips locations with dummy spans (used for notes without source locations).
*)
let related_info_of_related_location (rel : Typing.Diagnostic.related_location)
    : Protocol.diagnostic_related_information option =
  (* Skip dummy spans - these are notes without specific locations *)
  if rel.span.start_pos.file = "<generated>" then None
  else
    Some
      { Protocol.location = location_of_span rel.span; message = rel.message }

(** Convert a Typing.Diagnostic.t to an LSP diagnostic.

    Maps the rich diagnostic structure to LSP format:
    - Primary span → diagnostic range
    - Error code → diagnostic code string
    - Related locations → relatedInformation (skipping dummy spans)
    - Help suggestions → appended to message with "help: " prefix *)
let lsp_diagnostic_of_diagnostic (d : Typing.Diagnostic.t) : Protocol.diagnostic
    =
  let severity =
    match d.severity with
    | Typing.Diagnostic.Error -> Protocol.Error
    | Typing.Diagnostic.Warning -> Protocol.Warning
    | Typing.Diagnostic.Hint -> Protocol.Hint
  in
  (* Build the message with help suggestions appended *)
  let message =
    match d.help with
    | [] -> d.message
    | helps ->
        let help_lines =
          List.map (fun h -> "help: " ^ h) helps |> String.concat "\n"
        in
        d.message ^ "\n\n" ^ help_lines
  in
  (* Convert error code to string *)
  let code =
    match d.code with
    | Some c -> Some (Typing.Diagnostic.error_code_to_string c)
    | None -> None
  in
  (* Convert related locations to LSP format, filtering out dummy spans *)
  let related_information =
    List.filter_map related_info_of_related_location d.related
  in
  {
    Protocol.range = range_of_span d.span;
    severity = Some severity;
    code;
    message;
    source = Some "tart";
    related_information;
  }

(** Convert a parse error to an LSP diagnostic *)
let lsp_diagnostic_of_parse_error (err : Syntax.Read.parse_error) :
    Protocol.diagnostic =
  {
    Protocol.range = range_of_span err.span;
    severity = Some Protocol.Error;
    code = None;
    message = err.message;
    source = Some "tart";
    related_information = [];
  }

(** Convert a signature error to an LSP diagnostic *)
let lsp_diagnostic_of_sig_error (err : Sig.Search_path.sig_error) :
    Protocol.diagnostic =
  let message =
    match err.kind with
    | Sig.Search_path.LexerError msg -> msg
    | Sig.Search_path.ParseError msg -> msg
    | Sig.Search_path.ValidationError msg -> msg
    | Sig.Search_path.IOError msg -> msg
  in
  {
    Protocol.range = range_of_span err.span;
    severity = Some Protocol.Error;
    code = None;
    message;
    source = Some "tart";
    related_information = [];
  }

(** Check a .tart signature file and return LSP diagnostics.

    Validates the signature file for:
    - Lexer errors (invalid characters)
    - Parse errors (invalid syntax)
    - Validation errors (unbound type variables, invalid types) *)
let check_tart_document (text : string) (uri : string) :
    Protocol.diagnostic list =
  let filename = filename_of_uri uri in
  let parse_result = Syntax.Read.parse_string ~filename text in
  (* Check for lexer errors first *)
  if parse_result.errors <> [] then
    List.map lsp_diagnostic_of_parse_error parse_result.errors
  else
    (* Parse the signature *)
    let basename = Filename.basename filename in
    let module_name =
      if Filename.check_suffix basename ".tart" then
        Filename.chop_suffix basename ".tart"
      else basename
    in
    match Sig.Sig_parser.parse_signature ~module_name parse_result.sexps with
    | Error errors ->
        List.map
          (fun (e : Sig.Sig_parser.parse_error) ->
            lsp_diagnostic_of_sig_error
              { path = filename; kind = ParseError e.message; span = e.span })
          errors
    | Ok sig_file ->
        (* Validate the signature *)
        let validation_errors =
          Sig.Sig_loader.validate_signature_all sig_file
        in
        List.map
          (fun (e : Sig.Sig_loader.load_error) ->
            lsp_diagnostic_of_sig_error
              {
                path = filename;
                kind = ValidationError e.message;
                span = e.span;
              })
          validation_errors

(** Try to read the content of a sibling .tart file for cache invalidation.

    Checks the signature tracker first (for open buffers), then falls back to
    disk. Returns None if the file doesn't exist in either place. *)
let read_sibling_sig_content ~(sig_tracker : Signature_tracker.t)
    (filename : string) : string option =
  let dir = Filename.dirname filename in
  let basename = Filename.basename filename in
  let module_name =
    if Filename.check_suffix basename ".el" then
      Filename.chop_suffix basename ".el"
    else basename
  in
  let tart_path = Filename.concat dir (module_name ^ ".tart") in
  (* Check signature tracker first (R3: prefer buffer over disk) *)
  match Signature_tracker.get_by_path sig_tracker tart_path with
  | Some content -> Some content
  | None ->
      (* Fall back to disk *)
      if Sys.file_exists tart_path then
        try
          let ic = In_channel.open_text tart_path in
          let content = In_channel.input_all ic in
          In_channel.close ic;
          Some content
        with _ -> None
      else None

(** Type-check a document and return LSP diagnostics. Returns parse errors if
    parsing fails, otherwise type errors.

    Uses module-aware type checking with form-level caching to:
    - Load sibling `.tart` files for signature verification
    - Load required modules from the search path
    - Verify implementations match signatures
    - Cache unchanged forms for incremental updates *)
let check_document ~(config : Typing.Module_check.config)
    ~(cache : Form_cache.t) ~(sig_tracker : Signature_tracker.t) (uri : string)
    (text : string) : Protocol.diagnostic list * Form_cache.check_stats option =
  let filename = filename_of_uri uri in
  let parse_result = Syntax.Read.parse_string ~filename text in
  (* Collect parse errors *)
  let parse_diagnostics =
    List.map lsp_diagnostic_of_parse_error parse_result.errors
  in
  (* If we have sexps, type-check them *)
  let type_diagnostics, stats =
    if parse_result.sexps = [] then ([], None)
    else
      let sibling_sig_content =
        read_sibling_sig_content ~sig_tracker filename
      in
      let check_result, stats =
        Form_cache.check_with_cache ~cache ~config ~filename ~uri
          ~sibling_sig_content parse_result.sexps
      in
      let typing_diagnostics =
        Typing.Module_check.diagnostics_of_result check_result
      in
      (List.map lsp_diagnostic_of_diagnostic typing_diagnostics, Some stats)
  in
  (parse_diagnostics @ type_diagnostics, stats)

(** Publish diagnostics for a document *)
let publish_diagnostics (server : t) (uri : string) (version : int option) :
    unit =
  match Document.get_doc server.documents uri with
  | None -> ()
  | Some doc ->
      (* Check file type and use appropriate checker *)
      let is_tart = Signature_tracker.is_tart_file uri in
      let diagnostics, stats =
        if is_tart then
          (* For .tart files, check signature syntax and validation *)
          (check_tart_document doc.text uri, None)
        else
          (* For .el files, do full type checking *)
          check_document ~config:server.module_config ~cache:server.form_cache
            ~sig_tracker:server.signature_tracker uri doc.text
      in
      (* Check for dependency cycles (only for .el files) *)
      let cycle_diagnostics =
        if is_tart then []
        else Graph_tracker.check_cycles_for_module server.dependency_graph ~uri
      in
      let all_diagnostics = diagnostics @ cycle_diagnostics in
      let params : Protocol.publish_diagnostics_params =
        { uri; version; diagnostics = all_diagnostics }
      in
      (* Log cache statistics at debug level *)
      (match stats with
      | Some s ->
          debug server
            (Printf.sprintf
               "Type check: %d forms total, %d cached, %d re-checked"
               s.total_forms s.cached_forms s.checked_forms)
      | None -> ());
      debug server
        (Printf.sprintf "Publishing %d diagnostics for %s"
           (List.length all_diagnostics)
           uri);
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
      (* Log detected Emacs version at debug level (R7 of Spec 24) *)
      (match server.emacs_version with
      | Some v ->
          debug server
            (Printf.sprintf "Detected Emacs version: %s"
               (Sig.Emacs_version.version_to_string v))
      | None -> debug server "Emacs version not detected, using latest typings");
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
      (* Update dependency graph *)
      Graph_tracker.update_document server.dependency_graph ~uri ~text;
      (* Track .tart files in signature tracker *)
      if Signature_tracker.is_tart_file uri then
        Signature_tracker.set server.signature_tracker ~uri ~text;
      debug server
        (Printf.sprintf "Opened document: %s (version %d)" uri version);
      (* Publish diagnostics for the newly opened document *)
      publish_diagnostics server uri (Some version)

(** Handle textDocument/didChange notification *)
let handle_did_change (server : t) (params : Yojson.Safe.t option) : unit =
  match params with
  | None -> debug server "didChange missing params"
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
      | Ok () ->
          debug server
            (Printf.sprintf "Changed document: %s (version %d)" uri version);
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
          publish_diagnostics server uri (Some version);
          (* Invalidation cascade: invalidate caches and re-check dependents *)
          let module_id = Graph_tracker.module_id_of_uri uri in
          let open_uris = Document.list_uris server.documents in
          let dependent_uris =
            Graph_tracker.dependent_uris server.dependency_graph ~module_id
              ~open_uris
          in
          if dependent_uris <> [] then (
            debug server
              (Printf.sprintf "Invalidating %d dependents of %s"
                 (List.length dependent_uris)
                 module_id);
            List.iter
              (fun dep_uri ->
                (* Invalidate the cache for this dependent *)
                Form_cache.invalidate_document server.form_cache dep_uri;
                (* Re-publish diagnostics *)
                match Document.get_doc server.documents dep_uri with
                | Some dep_doc ->
                    publish_diagnostics server dep_uri (Some dep_doc.version)
                | None -> ())
              dependent_uris)
      | Error e ->
          info server (Printf.sprintf "Error applying changes to %s: %s" uri e))

(** Handle textDocument/didClose notification *)
let handle_did_close (server : t) (params : Yojson.Safe.t option) : unit =
  match params with
  | None -> debug server "didClose missing params"
  | Some json ->
      let open Yojson.Safe.Util in
      let text_document = json |> member "textDocument" in
      let uri = Document.text_document_identifier_of_json text_document in
      (* If it's a .tart file, remove from signature tracker and re-check dependents *)
      let is_tart = Signature_tracker.is_tart_file uri in
      if is_tart then (
        Signature_tracker.remove server.signature_tracker uri;
        (* Re-check dependents since they'll now read from disk *)
        let module_id = Graph_tracker.module_id_of_uri uri in
        let open_uris = Document.list_uris server.documents in
        let dependent_uris =
          Graph_tracker.dependent_uris server.dependency_graph ~module_id
            ~open_uris
        in
        List.iter
          (fun dep_uri ->
            Form_cache.invalidate_document server.form_cache dep_uri;
            match Document.get_doc server.documents dep_uri with
            | Some dep_doc ->
                publish_diagnostics server dep_uri (Some dep_doc.version)
            | None -> ())
          dependent_uris);
      Document.close_doc server.documents ~uri;
      (* Also clear the form cache for this document *)
      Form_cache.remove_document server.form_cache uri;
      (* Keep graph entry on close - file still exists on disk (R4) *)
      Graph_tracker.close_document server.dependency_graph ~uri;
      debug server (Printf.sprintf "Closed document: %s" uri);
      (* Clear diagnostics for the closed document *)
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
          })
  | _ ->
      (* Not in an application context, just infer the target directly *)
      safe_infer_and_solve (fun () ->
          Typing.Infer.infer env.Typing.Check.env ctx.target)

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
      debug server (Printf.sprintf "Hover request at %s:%d:%d" uri line col);
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
              Syntax.Sexp.find_with_context_in_forms ~line ~col
                parse_result.sexps
            with
            | None ->
                debug server "No S-expression at position";
                Ok `Null
            | Some ctx -> (
                debug server
                  (Printf.sprintf "Found sexp: %s (in application: %b)"
                     (Syntax.Sexp.to_string ctx.target)
                     (Option.is_some ctx.enclosing_application));
                (* Type-check the document to get the environment *)
                let check_result =
                  Typing.Check.check_program parse_result.sexps
                in
                match type_at_sexp check_result ctx with
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
                            value = Printf.sprintf "```elisp\n%s\n```" type_str;
                          };
                        range =
                          Some (range_of_span (Syntax.Sexp.span_of ctx.target));
                      }
                    in
                    Ok (Protocol.hover_to_json hover))))

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
      | None ->
          (* Not in sibling, try signatures from requires and autoloads.
             For now, we try prefix-based lookup similar to autoloads. *)
          let prefixes = Typing.Module_check.extract_module_prefixes name in
          let rec try_prefixes = function
            | [] -> None
            | prefix :: rest -> (
                match
                  Sig.Search_path.find_signature
                    (Typing.Module_check.search_path config)
                    prefix
                with
                | Some sig_path -> (
                    match Sig.Search_path.parse_signature_file sig_path with
                    | Some sig_ast -> (
                        match find_definition_in_signature name sig_ast with
                        | Some span -> Some span
                        | None -> try_prefixes rest)
                    | None -> try_prefixes rest)
                | None -> try_prefixes rest)
          in
          try_prefixes prefixes)
  | None ->
      (* No sibling .tart, try prefix-based lookup *)
      let prefixes = Typing.Module_check.extract_module_prefixes name in
      let rec try_prefixes = function
        | [] -> None
        | prefix :: rest -> (
            match
              Sig.Search_path.find_signature
                (Typing.Module_check.search_path config)
                prefix
            with
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
    | Cons (car, cdr, _) -> collect_sexp (collect_sexp acc car) cdr
  in
  List.fold_left collect_sexp [] sexps |> List.rev

(** Handle textDocument/definition request *)
let handle_definition (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  match params with
  | None ->
      Error
        {
          Rpc.code = Rpc.invalid_params;
          message = "Missing definition params";
          data = None;
        }
  | Some json -> (
      let def_params = Protocol.parse_definition_params json in
      let uri = def_params.def_text_document in
      let line = def_params.def_position.line in
      let col = def_params.def_position.character in
      debug server
        (Printf.sprintf "Definition request at %s:%d:%d" uri line col);
      match Document.get_doc server.documents uri with
      | None ->
          debug server (Printf.sprintf "Document not found: %s" uri);
          Ok (Protocol.definition_result_to_json Protocol.DefNull)
      | Some doc -> (
          let filename = filename_of_uri uri in
          let parse_result = Syntax.Read.parse_string ~filename doc.text in
          if parse_result.sexps = [] then (
            debug server "No S-expressions parsed";
            Ok (Protocol.definition_result_to_json Protocol.DefNull))
          else
            match
              Syntax.Sexp.find_with_context_in_forms ~line ~col
                parse_result.sexps
            with
            | None ->
                debug server "No S-expression at position";
                Ok (Protocol.definition_result_to_json Protocol.DefNull)
            | Some ctx -> (
                (* Extract symbol name from the target sexp *)
                match symbol_name_of_sexp ctx.target with
                | None ->
                    debug server "Target is not a symbol";
                    Ok (Protocol.definition_result_to_json Protocol.DefNull)
                | Some name -> (
                    debug server
                      (Printf.sprintf "Looking for definition of: %s" name);
                    (* Look up definition in the current document first *)
                    let definitions = extract_definitions parse_result.sexps in
                    match List.assoc_opt name definitions with
                    | Some span ->
                        debug server
                          (Printf.sprintf "Found local definition at %s:%d:%d"
                             span.start_pos.file span.start_pos.line
                             span.start_pos.col);
                        let loc = location_of_span span in
                        Ok
                          (Protocol.definition_result_to_json
                             (Protocol.DefLocation loc))
                    | None -> (
                        (* Not found locally - try signature lookup (R14) *)
                        debug server
                          (Printf.sprintf
                             "Not found locally, trying signature lookup for: \
                              %s"
                             name);
                        match
                          find_definition_in_signatures
                            ~config:server.module_config ~filename name
                        with
                        | Some span ->
                            debug server
                              (Printf.sprintf
                                 "Found definition in signature at %s:%d:%d"
                                 span.start_pos.file span.start_pos.line
                                 span.start_pos.col);
                            let loc = location_of_span span in
                            Ok
                              (Protocol.definition_result_to_json
                                 (Protocol.DefLocation loc))
                        | None ->
                            debug server
                              (Printf.sprintf "No definition found for: %s" name);
                            Ok
                              (Protocol.definition_result_to_json
                                 Protocol.DefNull))))))

(** Handle textDocument/references request *)
let handle_references (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  match params with
  | None ->
      Error
        {
          Rpc.code = Rpc.invalid_params;
          message = "Missing references params";
          data = None;
        }
  | Some json -> (
      let ref_params = Protocol.parse_references_params json in
      let uri = ref_params.ref_text_document in
      let line = ref_params.ref_position.line in
      let col = ref_params.ref_position.character in
      debug server
        (Printf.sprintf "References request at %s:%d:%d" uri line col);
      match Document.get_doc server.documents uri with
      | None ->
          debug server (Printf.sprintf "Document not found: %s" uri);
          Ok (Protocol.references_result_to_json None)
      | Some doc -> (
          let filename = filename_of_uri uri in
          let parse_result = Syntax.Read.parse_string ~filename doc.text in
          if parse_result.sexps = [] then (
            debug server "No S-expressions parsed";
            Ok (Protocol.references_result_to_json None))
          else
            match
              Syntax.Sexp.find_with_context_in_forms ~line ~col
                parse_result.sexps
            with
            | None ->
                debug server "No S-expression at position";
                Ok (Protocol.references_result_to_json None)
            | Some ctx -> (
                (* Extract symbol name from the target sexp *)
                match symbol_name_of_sexp ctx.target with
                | None ->
                    debug server "Target is not a symbol";
                    Ok (Protocol.references_result_to_json None)
                | Some name ->
                    debug server
                      (Printf.sprintf "Finding references to: %s" name);
                    (* Find all references in the document *)
                    let ref_spans = find_references name parse_result.sexps in
                    debug server
                      (Printf.sprintf "Found %d references"
                         (List.length ref_spans));
                    (* Convert spans to locations *)
                    let locations = List.map location_of_span ref_spans in
                    Ok (Protocol.references_result_to_json (Some locations)))))

(** {1 Type to Signature Format Conversion} *)

(** Convert a type to signature file format string.

    Signature format uses `(params) -> return` for functions, not `(-> (params)
    return)`. Type variables are rendered without the underscore prefix. *)
let rec type_to_sig_string (ty : Core.Types.typ) : string =
  match Core.Types.repr ty with
  | Core.Types.TVar tv -> (
      match !tv with
      | Core.Types.Unbound (id, _) -> Printf.sprintf "'t%d" id
      | Core.Types.Link _ -> failwith "repr should have followed link")
  | Core.Types.TCon name -> String.lowercase_ascii name
  | Core.Types.TApp (con, args) ->
      let con_str = type_to_sig_string con in
      Printf.sprintf "(%s %s)" con_str
        (String.concat " " (List.map type_to_sig_string args))
  | Core.Types.TArrow (params, ret) ->
      Printf.sprintf "((%s) -> %s)"
        (String.concat " " (List.map param_to_sig_string params))
        (type_to_sig_string ret)
  | Core.Types.TForall (vars, body) ->
      (* Don't render the forall wrapper - the type variables will be inferred *)
      type_to_sig_string_with_vars vars body
  | Core.Types.TUnion types ->
      Printf.sprintf "(%s)"
        (String.concat " | " (List.map type_to_sig_string types))
  | Core.Types.TTuple types ->
      Printf.sprintf "(tuple %s)"
        (String.concat " " (List.map type_to_sig_string types))

and param_to_sig_string = function
  | Core.Types.PPositional ty -> type_to_sig_string ty
  | Core.Types.POptional ty ->
      Printf.sprintf "&optional %s" (type_to_sig_string ty)
  | Core.Types.PRest ty -> Printf.sprintf "&rest %s" (type_to_sig_string ty)
  | Core.Types.PKey (name, ty) ->
      Printf.sprintf "&key :%s %s" name (type_to_sig_string ty)

(** Format a polymorphic type with explicit type variable binders *)
and type_to_sig_string_with_vars (vars : string list) (body : Core.Types.typ) :
    string =
  let body_str =
    match Core.Types.repr body with
    | Core.Types.TArrow (params, ret) ->
        Printf.sprintf "(%s) -> %s"
          (String.concat " " (List.map param_to_sig_string params))
          (type_to_sig_string ret)
    | _ -> type_to_sig_string body
  in
  if vars = [] then body_str
  else Printf.sprintf "[%s] %s" (String.concat " " vars) body_str

(** Generate a defun signature declaration string.

    Given a function name and its type, generates: `(defun name (params) ->
    return)` or `(defun name [vars] (params) -> return)` for polymorphic
    functions *)
let generate_defun_signature (name : string) (ty : Core.Types.typ) : string =
  match Core.Types.repr ty with
  | Core.Types.TForall (vars, Core.Types.TArrow (params, ret)) ->
      Printf.sprintf "(defun %s [%s] (%s) -> %s)" name (String.concat " " vars)
        (String.concat " " (List.map param_to_sig_string params))
        (type_to_sig_string ret)
  | Core.Types.TArrow (params, ret) ->
      Printf.sprintf "(defun %s (%s) -> %s)" name
        (String.concat " " (List.map param_to_sig_string params))
        (type_to_sig_string ret)
  | _ ->
      (* Not a function type - shouldn't happen for defuns but handle gracefully *)
      Printf.sprintf "(defvar %s %s)" name (type_to_sig_string ty)

(** {1 Extract Function Refactoring} *)

(** Elisp special forms that introduce variable bindings *)
let binding_forms =
  [
    "let";
    "let*";
    "letrec";
    "if-let";
    "when-let";
    "if-let*";
    "when-let*";
    "pcase-let";
    "pcase-let*";
    "cl-destructuring-bind";
    "lambda";
    "defun";
    "defsubst";
    "defmacro";
    "cl-defun";
    "cl-defsubst";
    "cl-defmacro";
  ]

(** Check if a symbol is a binding form *)
let is_binding_form (name : string) : bool = List.mem name binding_forms

(** Check if a symbol is a builtin that shouldn't be extracted as a parameter *)
let is_builtin_symbol (name : string) : bool =
  (* Common elisp builtins that should not become parameters *)
  let builtins =
    [
      (* Control flow *)
      "if";
      "when";
      "unless";
      "cond";
      "and";
      "or";
      "not";
      "progn";
      "prog1";
      "prog2";
      "while";
      "dolist";
      "dotimes";
      (* Binding forms *)
      "let";
      "let*";
      "lambda";
      "defun";
      "defvar";
      "defconst";
      "setq";
      "setf";
      (* List operations *)
      "car";
      "cdr";
      "cons";
      "list";
      "append";
      "nth";
      "nthcdr";
      "length";
      "mapcar";
      "mapc";
      "member";
      "assoc";
      "assq";
      (* Arithmetic *)
      "+";
      "-";
      "*";
      "/";
      "mod";
      "1+";
      "1-";
      "max";
      "min";
      "abs";
      (* Comparison *)
      "<";
      ">";
      "<=";
      ">=";
      "=";
      "/=";
      "eq";
      "eql";
      "equal";
      "string=";
      (* String operations *)
      "concat";
      "substring";
      "string-to-number";
      "number-to-string";
      "format";
      "message";
      (* Type predicates *)
      "null";
      "consp";
      "listp";
      "stringp";
      "numberp";
      "integerp";
      "symbolp";
      "functionp";
      (* Quoting *)
      "quote";
      "function";
      "backquote";
      (* Special *)
      "t";
      "nil";
      "error";
      "signal";
      "funcall";
      "apply";
    ]
  in
  List.mem name builtins || is_binding_form name

(** Extract bound variables from a binding clause.

    For `let`/`let*`, the clause is either `(var value)` or just `var`. *)
let extract_bound_from_clause (clause : Syntax.Sexp.t) : string list =
  match clause with
  | Syntax.Sexp.List (Syntax.Sexp.Symbol (name, _) :: _, _) -> [ name ]
  | Syntax.Sexp.Symbol (name, _) -> [ name ]
  | _ -> []

(** Extract bound variables from a let-style binding list.

    Given `((x 1) (y 2) z)`, returns `["x"; "y"; "z"]`. *)
let extract_bindings_from_let (bindings : Syntax.Sexp.t) : string list =
  match bindings with
  | Syntax.Sexp.List (clauses, _) ->
      List.concat_map extract_bound_from_clause clauses
  | _ -> []

(** Extract parameter names from a lambda/defun argument list.

    Given `(x y &optional z)`, returns `["x"; "y"; "z"]`. *)
let extract_params (args : Syntax.Sexp.t) : string list =
  let rec collect = function
    | [] -> []
    | Syntax.Sexp.Symbol ("&optional", _) :: rest -> collect rest
    | Syntax.Sexp.Symbol ("&rest", _) :: rest -> collect rest
    | Syntax.Sexp.Symbol ("&key", _) :: rest -> collect rest
    | Syntax.Sexp.Symbol (name, _) :: rest -> name :: collect rest
    | _ :: rest -> collect rest
  in
  match args with Syntax.Sexp.List (elems, _) -> collect elems | _ -> []

(** Collect all symbol references in an S-expression.

    Returns the set of symbol names that appear in the expression, excluding:
    - Symbols in function position of special forms
    - Symbols bound by let/lambda/etc. within the expression *)
let collect_symbol_refs (sexp : Syntax.Sexp.t) : string list =
  let rec collect ~(bound : string list) (expr : Syntax.Sexp.t) : string list =
    match expr with
    | Syntax.Sexp.Symbol (name, _) ->
        if List.mem name bound || is_builtin_symbol name then [] else [ name ]
    | Syntax.Sexp.List
        (Syntax.Sexp.Symbol (("let" | "let*"), _) :: bindings :: body, _) ->
        (* Collect refs from binding values (before they're bound) *)
        let binding_refs = collect_from_let_bindings ~bound bindings in
        (* Then collect from body with new bindings in scope *)
        let new_bound = extract_bindings_from_let bindings @ bound in
        let body_refs = List.concat_map (collect ~bound:new_bound) body in
        binding_refs @ body_refs
    | Syntax.Sexp.List
        ( Syntax.Sexp.Symbol (("lambda" | "defun" | "defsubst" | "defmacro"), _)
          :: args :: body,
          _ ) ->
        (* For lambda/defun, first arg is params (skip if symbol for defun name) *)
        let params, actual_body =
          match args with
          | Syntax.Sexp.Symbol _ ->
              (* defun name - skip it, next is args *)
              ( (match body with [] -> [] | h :: _ -> extract_params h),
                match body with [] | [ _ ] -> [] | _ :: t -> t )
          | _ -> (extract_params args, body)
        in
        let new_bound = params @ bound in
        List.concat_map (collect ~bound:new_bound) actual_body
    | Syntax.Sexp.List (Syntax.Sexp.Symbol (form, _) :: _, _)
      when is_binding_form form ->
        (* Other binding forms - conservatively just collect from all children *)
        collect_from_children ~bound expr
    | Syntax.Sexp.List (head :: args, _) ->
        (* Regular function call - collect from head (unless it's a symbol being called)
           and all arguments *)
        let head_refs =
          match head with
          | Syntax.Sexp.Symbol _ ->
              [] (* Don't treat function name as free var *)
          | _ -> collect ~bound head
        in
        head_refs @ List.concat_map (collect ~bound) args
    | Syntax.Sexp.List ([], _) -> []
    | Syntax.Sexp.Vector (elems, _) -> List.concat_map (collect ~bound) elems
    | Syntax.Sexp.Cons (car, cdr, _) -> collect ~bound car @ collect ~bound cdr
    | Syntax.Sexp.Int _ | Syntax.Sexp.Float _ | Syntax.Sexp.String _
    | Syntax.Sexp.Keyword _ | Syntax.Sexp.Char _ | Syntax.Sexp.Error _ ->
        []
  and collect_from_let_bindings ~(bound : string list)
      (bindings : Syntax.Sexp.t) : string list =
    match bindings with
    | Syntax.Sexp.List (clauses, _) ->
        List.concat_map
          (fun clause ->
            match clause with
            | Syntax.Sexp.List (_ :: value :: _, _) -> collect ~bound value
            | _ -> [])
          clauses
    | _ -> []
  and collect_from_children ~(bound : string list) (expr : Syntax.Sexp.t) :
      string list =
    match expr with
    | Syntax.Sexp.List (elems, _) -> List.concat_map (collect ~bound) elems
    | Syntax.Sexp.Vector (elems, _) -> List.concat_map (collect ~bound) elems
    | Syntax.Sexp.Cons (car, cdr, _) -> collect ~bound car @ collect ~bound cdr
    | _ -> []
  in
  collect ~bound:[] sexp |> List.sort_uniq String.compare

(** Find the S-expression that best matches a given LSP range.

    The range is 0-based (LSP convention). Returns the first sexp whose span
    contains the start position of the range, provided the selection is
    non-trivial (not just a cursor position). *)
let find_sexp_at_range (range : Protocol.range) (sexps : Syntax.Sexp.t list) :
    Syntax.Sexp.t option =
  (* Only offer extract if there's a real selection (not just a cursor) *)
  if
    range.start.line = range.end_.line
    && range.start.character = range.end_.character
  then None
  else
    (* Find sexp at start of range *)
    Syntax.Sexp.find_at_position_in_forms ~line:range.start.line
      ~col:range.start.character sexps

(** Generate "Extract function" refactoring action.

    Creates a workspace edit that: 1. Inserts a new defun before the current
    top-level form 2. Replaces the selection with a call to the new function *)
let generate_extract_function_action ~(uri : string) ~(doc_text : string)
    ~(sexp : Syntax.Sexp.t) : Protocol.code_action option =
  let free_vars = collect_symbol_refs sexp in
  let fn_name = "extracted-fn" in
  let span = Syntax.Sexp.span_of sexp in

  (* Generate the new function definition *)
  let params_str =
    if free_vars = [] then "()" else "(" ^ String.concat " " free_vars ^ ")"
  in
  let body_str = Syntax.Sexp.to_string sexp in
  let new_defun =
    Printf.sprintf "(defun %s %s\n  %s)\n\n" fn_name params_str body_str
  in

  (* Generate the replacement call *)
  let call_str =
    if free_vars = [] then Printf.sprintf "(%s)" fn_name
    else Printf.sprintf "(%s %s)" fn_name (String.concat " " free_vars)
  in

  (* Find position to insert new defun - at start of line containing the selection *)
  let insert_line = span.start_pos.line - 1 in
  (* Convert to 0-based *)
  let insert_pos : Protocol.position = { line = insert_line; character = 0 } in

  (* Convert span to LSP range for replacement *)
  let replace_range = range_of_span span in

  (* Create edits:
     1. Insert new defun at start of the line
     2. Replace selected expression with function call *)
  let insert_edit : Protocol.text_edit =
    {
      te_range = { start = insert_pos; end_ = insert_pos };
      new_text = new_defun;
    }
  in
  let replace_edit : Protocol.text_edit =
    { te_range = replace_range; new_text = call_str }
  in

  (* Both edits target the same document *)
  let _ = doc_text in
  let doc_edit : Protocol.text_document_edit =
    { tde_uri = uri; tde_version = None; edits = [ insert_edit; replace_edit ] }
  in
  let workspace_edit : Protocol.workspace_edit =
    { document_changes = [ doc_edit ] }
  in

  Some
    {
      Protocol.ca_title = "Extract function";
      ca_kind = Some Protocol.RefactorExtract;
      ca_diagnostics = [];
      ca_is_preferred = false;
      ca_edit = Some workspace_edit;
    }

(** {1 Code Action Generation} *)

(** Get the sibling .tart file path for an .el file *)
let get_tart_path (el_filename : string) : string =
  let dir = Filename.dirname el_filename in
  let basename = Filename.basename el_filename in
  let module_name =
    if Filename.check_suffix basename ".el" then
      Filename.chop_suffix basename ".el"
    else basename
  in
  Filename.concat dir (module_name ^ ".tart")

(** Read the content of a .tart file if it exists, or return empty string *)
let read_tart_file (tart_path : string) : string option =
  if Sys.file_exists tart_path then
    try
      let ic = In_channel.open_text tart_path in
      let content = In_channel.input_all ic in
      In_channel.close ic;
      Some content
    with _ -> None
  else None

(** Generate "Add type annotation" quickfix for a missing signature warning.

    Creates a workspace edit that appends the function signature to the .tart
    file. *)
let generate_add_signature_action ~(name : string) ~(ty : Core.Types.typ)
    ~(tart_path : string) ~(tart_content : string option) :
    Protocol.code_action option =
  let signature_str = generate_defun_signature name ty in
  let tart_uri = "file://" ^ tart_path in

  (* Calculate the position to insert - append at end of file *)
  let insert_pos, new_text =
    match tart_content with
    | Some content ->
        let lines = String.split_on_char '\n' content in
        let line_count = List.length lines in
        (* Insert on a new line at the end *)
        let last_line_len =
          match List.rev lines with [] -> 0 | last :: _ -> String.length last
        in
        ( { Protocol.line = line_count - 1; character = last_line_len },
          "\n" ^ signature_str )
    | None ->
        (* File doesn't exist - create with just the signature *)
        ({ Protocol.line = 0; character = 0 }, signature_str ^ "\n")
  in

  let edit : Protocol.text_edit =
    { te_range = { start = insert_pos; end_ = insert_pos }; new_text }
  in
  let doc_edit : Protocol.text_document_edit =
    { tde_uri = tart_uri; tde_version = None; edits = [ edit ] }
  in
  let workspace_edit : Protocol.workspace_edit =
    { document_changes = [ doc_edit ] }
  in

  Some
    {
      Protocol.ca_title =
        Printf.sprintf "Add signature for `%s` to .tart file" name;
      ca_kind = Some Protocol.QuickFix;
      ca_diagnostics = [];
      ca_is_preferred = true;
      ca_edit = Some workspace_edit;
    }

(** Handle textDocument/codeAction request.

    Returns code actions available for the given range and context. Generates
    quickfixes for:
    - Missing signature warnings: offers to add the function signature to the
      .tart file *)
let handle_code_action (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  match params with
  | None ->
      Error
        {
          Rpc.code = Rpc.invalid_params;
          message = "Missing code action params";
          data = None;
        }
  | Some json -> (
      let ca_params = Protocol.parse_code_action_params json in
      let uri = ca_params.ca_text_document in
      let range = ca_params.ca_range in
      let context = ca_params.ca_context in
      debug server
        (Printf.sprintf "Code action request at %s:%d:%d-%d:%d" uri
           range.start.line range.start.character range.end_.line
           range.end_.character);
      debug server
        (Printf.sprintf "Context has %d diagnostics"
           (List.length context.cac_diagnostics));

      match Document.get_doc server.documents uri with
      | None ->
          debug server (Printf.sprintf "Document not found: %s" uri);
          Ok (Protocol.code_action_result_to_json (Some []))
      | Some doc ->
          let filename = filename_of_uri uri in
          let parse_result = Syntax.Read.parse_string ~filename doc.text in

          if parse_result.sexps = [] then
            Ok (Protocol.code_action_result_to_json (Some []))
          else
            (* Type-check to get missing signature warnings *)
            let check_result =
              Typing.Module_check.check_module ~config:server.module_config
                ~filename parse_result.sexps
            in

            (* Generate code actions for missing signatures *)
            let tart_path = get_tart_path filename in
            let tart_content = read_tart_file tart_path in

            let missing_sig_actions =
              List.filter_map
                (fun (warn : Typing.Module_check.missing_signature_warning) ->
                  (* Check if this warning's range overlaps with the request range *)
                  let warn_range = range_of_span warn.span in
                  let overlaps =
                    warn_range.start.line <= range.end_.line
                    && warn_range.end_.line >= range.start.line
                  in
                  if overlaps then (
                    debug server
                      (Printf.sprintf "Found missing signature for: %s"
                         warn.name);
                    (* Look up the inferred type from the environment *)
                    match
                      Core.Type_env.lookup warn.name check_result.final_env
                    with
                    | Some scheme ->
                        let ty =
                          match scheme with
                          | Core.Type_env.Mono t -> t
                          | Core.Type_env.Poly (vars, t) ->
                              Core.Types.TForall (vars, t)
                        in
                        generate_add_signature_action ~name:warn.name ~ty
                          ~tart_path ~tart_content
                    | None ->
                        debug server
                          (Printf.sprintf "No type found for: %s" warn.name);
                        None)
                  else None)
                check_result.missing_signature_warnings
            in

            (* Generate "Extract function" refactoring if selection covers a sexp *)
            let extract_actions =
              match find_sexp_at_range range parse_result.sexps with
              | Some sexp -> (
                  debug server
                    (Printf.sprintf "Found sexp for extraction: %s"
                       (Syntax.Sexp.to_string sexp));
                  match
                    generate_extract_function_action ~uri ~doc_text:doc.text
                      ~sexp
                  with
                  | Some action -> [ action ]
                  | None -> [])
              | None -> []
            in

            let all_actions = missing_sig_actions @ extract_actions in
            debug server
              (Printf.sprintf "Generated %d code actions"
                 (List.length all_actions));
            Ok (Protocol.code_action_result_to_json (Some all_actions)))

(** {1 Document Symbols} *)

(** Extract a symbol from a definition form.

    For defun, extracts function with type from environment if available. For
    defvar/defconst, extracts variable/constant with its span. *)
let rec extract_symbol_from_def (sexp : Syntax.Sexp.t) :
    Protocol.document_symbol option =
  let open Syntax.Sexp in
  match sexp with
  | List (Symbol ("defun", _) :: Symbol (name, name_span) :: args :: body, span)
    ->
      (* Get the selection range from the name symbol *)
      let selection_range =
        {
          Protocol.start =
            {
              line = name_span.start_pos.line - 1;
              character = name_span.start_pos.col;
            };
          end_ =
            {
              line = name_span.end_pos.line - 1;
              character = name_span.end_pos.col;
            };
        }
      in
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
        List.filter_map extract_symbol_from_def
          (List.concat_map (function List (elems, _) -> elems | _ -> []) body)
      in
      Some
        {
          Protocol.ds_name = name;
          ds_detail = Some param_detail;
          ds_kind = Protocol.SKFunction;
          ds_range = range_of_span span;
          ds_selection_range = selection_range;
          ds_children = children;
        }
  | List (Symbol ("defvar", _) :: Symbol (name, name_span) :: _, span) ->
      let selection_range =
        {
          Protocol.start =
            {
              line = name_span.start_pos.line - 1;
              character = name_span.start_pos.col;
            };
          end_ =
            {
              line = name_span.end_pos.line - 1;
              character = name_span.end_pos.col;
            };
        }
      in
      Some
        {
          Protocol.ds_name = name;
          ds_detail = None;
          ds_kind = Protocol.SKVariable;
          ds_range = range_of_span span;
          ds_selection_range = selection_range;
          ds_children = [];
        }
  | List (Symbol ("defconst", _) :: Symbol (name, name_span) :: _, span) ->
      let selection_range =
        {
          Protocol.start =
            {
              line = name_span.start_pos.line - 1;
              character = name_span.start_pos.col;
            };
          end_ =
            {
              line = name_span.end_pos.line - 1;
              character = name_span.end_pos.col;
            };
        }
      in
      Some
        {
          Protocol.ds_name = name;
          ds_detail = None;
          ds_kind = Protocol.SKConstant;
          ds_range = range_of_span span;
          ds_selection_range = selection_range;
          ds_children = [];
        }
  | List (Symbol ("defmacro", _) :: Symbol (name, name_span) :: args :: _, span)
    ->
      let selection_range =
        {
          Protocol.start =
            {
              line = name_span.start_pos.line - 1;
              character = name_span.start_pos.col;
            };
          end_ =
            {
              line = name_span.end_pos.line - 1;
              character = name_span.end_pos.col;
            };
        }
      in
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
          ds_range = range_of_span span;
          ds_selection_range = selection_range;
          ds_children = [];
        }
  | _ -> None

(** Handle textDocument/documentSymbol request.

    Returns a list of symbols (functions, variables, constants, macros) defined
    in the document, with their types as details when available. *)
let handle_document_symbol (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  match params with
  | None ->
      Error
        {
          Rpc.code = Rpc.invalid_params;
          message = "Missing document symbol params";
          data = None;
        }
  | Some json -> (
      let dsp_params = Protocol.parse_document_symbol_params json in
      let uri = dsp_params.dsp_text_document in
      debug server (Printf.sprintf "Document symbol request for %s" uri);
      match Document.get_doc server.documents uri with
      | None ->
          debug server (Printf.sprintf "Document not found: %s" uri);
          Ok (Protocol.document_symbol_result_to_json None)
      | Some doc ->
          let filename = filename_of_uri uri in
          let parse_result = Syntax.Read.parse_string ~filename doc.text in
          if parse_result.sexps = [] then (
            debug server "No S-expressions parsed";
            Ok (Protocol.document_symbol_result_to_json (Some [])))
          else
            let symbols =
              List.filter_map extract_symbol_from_def parse_result.sexps
            in
            debug server
              (Printf.sprintf "Found %d symbols" (List.length symbols));
            Ok (Protocol.document_symbol_result_to_json (Some symbols)))

(** {1 Completion} *)

(** Extract the word prefix at the given position.

    Scans backwards from the position to find the start of the current symbol.
    Returns the prefix string and its start column. *)
let extract_prefix_at_position (text : string) (line : int) (col : int) :
    string * int =
  (* Find the line in the text *)
  let lines = String.split_on_char '\n' text in
  if line >= List.length lines then ("", col)
  else
    let line_text = List.nth lines line in
    if col > String.length line_text then ("", col)
    else
      (* Scan backwards to find word start *)
      let rec find_start i =
        if i < 0 then 0
        else
          let c = line_text.[i] in
          (* Symbol characters in elisp: alphanumeric, hyphen, underscore, etc. *)
          if
            (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
            || (c >= '0' && c <= '9')
            || c = '-' || c = '_' || c = '+' || c = '*' || c = '/' || c = '<'
            || c = '>' || c = '=' || c = '!' || c = '?' || c = '%'
          then find_start (i - 1)
          else i + 1
      in
      let start_col = find_start (col - 1) in
      let prefix = String.sub line_text start_col (col - start_col) in
      (prefix, start_col)

(** Collect completion candidates from definitions in the document.

    Extracts function and variable names from defun/defvar/defconst forms. *)
let collect_local_completions (sexps : Syntax.Sexp.t list) :
    Protocol.completion_item list =
  let open Syntax.Sexp in
  List.filter_map
    (fun sexp ->
      match sexp with
      | List (Symbol ("defun", _) :: Symbol (name, _) :: args :: _, _) ->
          let param_detail =
            match args with
            | List (params, _) ->
                let param_strs =
                  List.filter_map
                    (function
                      | Symbol (p, _)
                        when not (String.length p > 0 && p.[0] = '&') ->
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
              Protocol.ci_label = name;
              ci_kind = Some Protocol.CIKFunction;
              ci_detail = Some param_detail;
              ci_documentation = None;
              ci_insert_text = None;
            }
      | List
          ( (Symbol ("defvar", _) | Symbol ("defconst", _))
            :: Symbol (name, _)
            :: _,
            _ ) ->
          let kind =
            match sexp with
            | List (Symbol ("defconst", _) :: _, _) -> Protocol.CIKConstant
            | _ -> Protocol.CIKVariable
          in
          Some
            {
              Protocol.ci_label = name;
              ci_kind = Some kind;
              ci_detail = None;
              ci_documentation = None;
              ci_insert_text = None;
            }
      | _ -> None)
    sexps

(** Collect completion candidates from a type environment.

    Creates completion items for all bound names with their types. *)
let collect_env_completions (env : Core.Type_env.t) :
    Protocol.completion_item list =
  List.filter_map
    (fun (name, scheme) ->
      let ty =
        match scheme with
        | Core.Type_env.Mono t -> t
        | Core.Type_env.Poly (_, t) -> t
      in
      let kind =
        match ty with
        | Core.Types.TArrow _ -> Protocol.CIKFunction
        | _ -> Protocol.CIKVariable
      in
      let detail = Core.Types.to_string ty in
      Some
        {
          Protocol.ci_label = name;
          ci_kind = Some kind;
          ci_detail = Some detail;
          ci_documentation = None;
          ci_insert_text = None;
        })
    env.Core.Type_env.bindings

(** Filter completions by prefix match (case-insensitive) *)
let filter_by_prefix (prefix : string) (items : Protocol.completion_item list) :
    Protocol.completion_item list =
  if prefix = "" then items
  else
    let prefix_lower = String.lowercase_ascii prefix in
    List.filter
      (fun item ->
        let label_lower = String.lowercase_ascii item.Protocol.ci_label in
        String.length label_lower >= String.length prefix_lower
        && String.sub label_lower 0 (String.length prefix_lower) = prefix_lower)
      items

(** Deduplicate completion items by label, preferring items with types *)
let deduplicate_completions (items : Protocol.completion_item list) :
    Protocol.completion_item list =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun item ->
      match Hashtbl.find_opt tbl item.Protocol.ci_label with
      | None -> Hashtbl.add tbl item.Protocol.ci_label item
      | Some existing ->
          (* Prefer items with type information *)
          if existing.ci_detail = None && item.ci_detail <> None then
            Hashtbl.replace tbl item.Protocol.ci_label item)
    items;
  Hashtbl.fold (fun _ item acc -> item :: acc) tbl []
  |> List.sort (fun a b ->
         String.compare a.Protocol.ci_label b.Protocol.ci_label)

(** Handle textDocument/completion request.

    Returns completion items for the current position, including:
    - Local definitions (defun, defvar, defconst)
    - Functions from loaded signatures (stdlib, requires) *)
let handle_completion (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  match params with
  | None ->
      Error
        {
          Rpc.code = Rpc.invalid_params;
          message = "Missing completion params";
          data = None;
        }
  | Some json -> (
      let cp_params = Protocol.parse_completion_params json in
      let uri = cp_params.cp_text_document in
      let line = cp_params.cp_position.line in
      let col = cp_params.cp_position.character in
      debug server
        (Printf.sprintf "Completion request at %s:%d:%d" uri line col);
      match Document.get_doc server.documents uri with
      | None ->
          debug server (Printf.sprintf "Document not found: %s" uri);
          Ok (Protocol.completion_result_to_json None)
      | Some doc ->
          let filename = filename_of_uri uri in
          let prefix, _ = extract_prefix_at_position doc.text line col in
          debug server (Printf.sprintf "Completion prefix: '%s'" prefix);

          let parse_result = Syntax.Read.parse_string ~filename doc.text in

          (* Collect local definitions *)
          let local_items = collect_local_completions parse_result.sexps in
          debug server
            (Printf.sprintf "Found %d local completions"
               (List.length local_items));

          (* Type-check to get environment with signatures *)
          let check_result =
            Typing.Module_check.check_module ~config:server.module_config
              ~filename parse_result.sexps
          in

          (* Collect items from loaded signatures *)
          let sig_items =
            match check_result.signature_env with
            | Some env -> collect_env_completions env
            | None -> []
          in
          debug server
            (Printf.sprintf "Found %d signature completions"
               (List.length sig_items));

          (* Collect items from final environment (includes builtins) *)
          let env_items = collect_env_completions check_result.final_env in
          debug server
            (Printf.sprintf "Found %d env completions" (List.length env_items));

          (* Combine, deduplicate, and filter *)
          let all_items = local_items @ sig_items @ env_items in
          let filtered = filter_by_prefix prefix all_items in
          let deduped = deduplicate_completions filtered in
          debug server
            (Printf.sprintf "Returning %d completions" (List.length deduped));

          Ok (Protocol.completion_result_to_json (Some deduped)))

(** {1 Signature Help} *)

(** Find the enclosing function call and the argument position at the cursor.

    Given a position, finds the innermost (fn arg1 arg2 ...) list containing
    that position and returns the function name and the 0-based argument index
    where the cursor is located.

    The position is 0-based (LSP convention). Line numbers in spans are 1-based,
    so we adjust when comparing. *)
let find_call_context ~(line : int) ~(col : int) (sexps : Syntax.Sexp.t list) :
    (string * int) option =
  (* Find all list forms that contain the position, keeping track of depth *)
  let rec find_in_sexp (sexp : Syntax.Sexp.t) : (string * int) option =
    let span = Syntax.Sexp.span_of sexp in
    (* LSP position is 0-based; span line is 1-based *)
    if not (Syntax.Location.contains_position span ~line ~col) then None
    else
      match sexp with
      | Syntax.Sexp.List (Syntax.Sexp.Symbol (fn_name, _) :: args, _) ->
          (* This is a function call - check if cursor is in args *)
          (* First, try to find a more nested call in the args *)
          let nested_result = List.find_map find_in_sexp args in
          if Option.is_some nested_result then nested_result
          else
            (* Cursor is in this call's args - find which argument position *)
            let arg_index = ref 0 in
            let found = ref false in
            List.iteri
              (fun i arg ->
                if not !found then
                  let arg_span = Syntax.Sexp.span_of arg in
                  if Syntax.Location.contains_position arg_span ~line ~col then (
                    arg_index := i;
                    found := true)
                  else if
                    (* Check if position is between args (whitespace) *)
                    (* If the cursor is after this arg but before the next, it's for the next arg *)
                    arg_span.end_pos.line - 1 < line
                    || arg_span.end_pos.line - 1 = line
                       && arg_span.end_pos.col <= col
                  then arg_index := i + 1)
              args;
            Some (fn_name, !arg_index)
      | Syntax.Sexp.List (elems, _) ->
          (* Non-function-call list, search inside *)
          List.find_map find_in_sexp elems
      | Syntax.Sexp.Vector (elems, _) -> List.find_map find_in_sexp elems
      | Syntax.Sexp.Cons (car, cdr, _) -> (
          match find_in_sexp car with
          | Some _ as r -> r
          | None -> find_in_sexp cdr)
      | _ -> None
  in
  List.find_map find_in_sexp sexps

(** Convert a type parameter to a signature help parameter label.

    Returns the label string for displaying the parameter. *)
let param_to_label (param : Core.Types.param) : string =
  match param with
  | Core.Types.PPositional ty -> Core.Types.to_string ty
  | Core.Types.POptional ty ->
      Printf.sprintf "&optional %s" (Core.Types.to_string ty)
  | Core.Types.PRest ty -> Printf.sprintf "&rest %s" (Core.Types.to_string ty)
  | Core.Types.PKey (name, ty) ->
      Printf.sprintf ":%s %s" name (Core.Types.to_string ty)

(** Generate signature help for a function type.

    Creates signature information from a function name and its type,
    highlighting the active parameter. *)
let signature_of_function_type (fn_name : string) (ty : Core.Types.typ)
    (active_param : int) : Protocol.signature_help option =
  (* Unwrap TForall to get the arrow type *)
  let inner_ty =
    match Core.Types.repr ty with
    | Core.Types.TForall (_, body) -> Core.Types.repr body
    | other -> other
  in
  match inner_ty with
  | Core.Types.TArrow (params, ret) ->
      (* Build parameter list *)
      let parameters =
        List.map
          (fun param : Protocol.parameter_information ->
            { pi_label = param_to_label param; pi_documentation = None })
          params
      in
      (* Build the full signature label: fn_name :: (params) -> return *)
      let params_str = String.concat " " (List.map param_to_label params) in
      let ret_str = Core.Types.to_string ret in
      let label = Printf.sprintf "(%s %s) → %s" fn_name params_str ret_str in
      (* Clamp active parameter to valid range *)
      let active =
        if active_param >= 0 && active_param < List.length params then
          Some active_param
        else if List.length params > 0 then
          (* Check if last param is rest - if so, keep highlighting it *)
          match List.rev params with
          | Core.Types.PRest _ :: _ -> Some (List.length params - 1)
          | _ -> None
        else None
      in
      Some
        {
          Protocol.sh_signatures =
            [
              {
                Protocol.si_label = label;
                si_documentation = None;
                si_parameters = parameters;
                si_active_parameter = active;
              };
            ];
          sh_active_signature = Some 0;
          sh_active_parameter = active;
        }
  | _ -> None

(** Handle textDocument/signatureHelp request.

    Returns signature help when the cursor is inside a function call, showing
    the function's signature with the current parameter highlighted. *)
let handle_signature_help (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  match params with
  | None ->
      Error
        {
          Rpc.code = Rpc.invalid_params;
          message = "Missing signature help params";
          data = None;
        }
  | Some json -> (
      let sh_params = Protocol.parse_signature_help_params json in
      let uri = sh_params.shp_text_document in
      let line = sh_params.shp_position.line in
      let col = sh_params.shp_position.character in
      debug server
        (Printf.sprintf "Signature help request at %s:%d:%d" uri line col);
      match Document.get_doc server.documents uri with
      | None ->
          debug server (Printf.sprintf "Document not found: %s" uri);
          Ok (Protocol.signature_help_result_to_json None)
      | Some doc -> (
          let filename = filename_of_uri uri in
          let parse_result = Syntax.Read.parse_string ~filename doc.text in
          if parse_result.sexps = [] then (
            debug server "No S-expressions parsed";
            Ok (Protocol.signature_help_result_to_json None))
          else
            (* Find the enclosing function call and argument position *)
            match find_call_context ~line ~col parse_result.sexps with
            | None ->
                debug server "No function call at position";
                Ok (Protocol.signature_help_result_to_json None)
            | Some (fn_name, arg_index) -> (
                debug server
                  (Printf.sprintf "Found call to '%s' at arg position %d"
                     fn_name arg_index);
                (* Type-check to get the environment with function types *)
                let check_result =
                  Typing.Module_check.check_module ~config:server.module_config
                    ~filename parse_result.sexps
                in
                (* Look up the function's type in the environment *)
                match Core.Type_env.lookup fn_name check_result.final_env with
                | None ->
                    debug server
                      (Printf.sprintf "Function '%s' not found in environment"
                         fn_name);
                    Ok (Protocol.signature_help_result_to_json None)
                | Some scheme ->
                    let ty =
                      match scheme with
                      | Core.Type_env.Mono t -> t
                      | Core.Type_env.Poly (vars, t) ->
                          Core.Types.TForall (vars, t)
                    in
                    debug server
                      (Printf.sprintf "Function type: %s"
                         (Core.Types.to_string ty));
                    let result =
                      signature_of_function_type fn_name ty arg_index
                    in
                    Ok (Protocol.signature_help_result_to_json result))))

(** {1 Rename} *)

(** Handle textDocument/rename request.

    Renames all occurrences of a symbol within the document. Uses
    find_references to locate all occurrences and generates text edits to
    replace each one with the new name. *)
let handle_rename (server : t) (params : Yojson.Safe.t option) :
    (Yojson.Safe.t, Rpc.response_error) result =
  match params with
  | None ->
      Error
        {
          Rpc.code = Rpc.invalid_params;
          message = "Missing rename params";
          data = None;
        }
  | Some json -> (
      let rename_params = Protocol.parse_rename_params json in
      let uri = rename_params.rp_text_document in
      let line = rename_params.rp_position.line in
      let col = rename_params.rp_position.character in
      let new_name = rename_params.rp_new_name in
      debug server
        (Printf.sprintf "Rename request at %s:%d:%d -> '%s'" uri line col
           new_name);
      match Document.get_doc server.documents uri with
      | None ->
          debug server (Printf.sprintf "Document not found: %s" uri);
          Ok (Protocol.rename_result_to_json None)
      | Some doc -> (
          let filename = filename_of_uri uri in
          let parse_result = Syntax.Read.parse_string ~filename doc.text in
          if parse_result.sexps = [] then (
            debug server "No S-expressions parsed";
            Ok (Protocol.rename_result_to_json None))
          else
            match
              Syntax.Sexp.find_with_context_in_forms ~line ~col
                parse_result.sexps
            with
            | None ->
                debug server "No S-expression at position";
                Ok (Protocol.rename_result_to_json None)
            | Some ctx -> (
                (* Extract symbol name from the target sexp *)
                match symbol_name_of_sexp ctx.target with
                | None ->
                    debug server "Target is not a symbol";
                    Ok (Protocol.rename_result_to_json None)
                | Some name ->
                    debug server
                      (Printf.sprintf "Renaming '%s' to '%s'" name new_name);
                    (* Find all references in the document *)
                    let ref_spans = find_references name parse_result.sexps in
                    debug server
                      (Printf.sprintf "Found %d occurrences to rename"
                         (List.length ref_spans));
                    if ref_spans = [] then
                      Ok (Protocol.rename_result_to_json None)
                    else
                      (* Generate text edits for each occurrence *)
                      let edits =
                        List.map
                          (fun span : Protocol.text_edit ->
                            {
                              te_range = range_of_span span;
                              new_text = new_name;
                            })
                          ref_spans
                      in
                      let doc_edit : Protocol.text_document_edit =
                        { tde_uri = uri; tde_version = None; edits }
                      in
                      let workspace_edit : Protocol.workspace_edit =
                        { document_changes = [ doc_edit ] }
                      in
                      Ok (Protocol.rename_result_to_json (Some workspace_edit)))
          ))

(** Dispatch a request to its handler *)
let dispatch_request (server : t) (msg : Rpc.message) :
    (Yojson.Safe.t, Rpc.response_error) result =
  debug server (Printf.sprintf "Request: %s" msg.method_);
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
  | "textDocument/rename" -> handle_rename server msg.params
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
      debug server
        (Printf.sprintf "Ignoring unknown notification: %s" msg.method_);
      `Continue

(** Process a single message and optionally send a response *)
let process_message (server : t) (msg : Rpc.message) :
    [ `Continue | `Exit of int ] =
  match msg.id with
  | Some id ->
      (* Request - must send response *)
      let response =
        match dispatch_request server msg with
        | Ok result -> Rpc.success_response ~id ~result
        | Error err -> { Rpc.id; result = None; error = Some err }
      in
      debug server
        (Printf.sprintf "Response: %s" (Rpc.response_to_string response));
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
    | Error Rpc.Eof -> (
        info server "Client disconnected";
        (* Exit with code 1 if we didn't get proper shutdown *)
        match server.state with
        | ShuttingDown -> 0
        | _ -> 1)
    | Error err ->
        info server
          (Printf.sprintf "Read error: %s" (Rpc.read_error_to_string err));
        1
    | Ok msg -> (
        debug server (Printf.sprintf "Received: %s" (Rpc.message_to_string msg));
        match process_message server msg with
        | `Continue -> loop ()
        | `Exit code -> code)
  in
  loop ()
