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
}

and log_level = Quiet | Normal | Debug

(** Create a new server on the given channels *)
let create ?(log_level = Normal) ~ic ~oc () : t =
  {
    ic;
    oc;
    state = Uninitialized;
    log_level;
    documents = Document.create ();
    form_cache = Form_cache.create ();
  }

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
    definition_provider = true;
    references_provider = true;
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

(** Get the default module check configuration.

    Uses the stdlib directory relative to the executable location. *)
let default_module_config () : Typing.Module_check.config =
  (* Try to find stdlib relative to the executable *)
  let exe_dir = Filename.dirname Sys.executable_name in
  let stdlib_candidates =
    [
      Filename.concat exe_dir "stdlib";
      Filename.concat (Filename.dirname exe_dir) "stdlib";
      Filename.concat exe_dir "../share/tart/stdlib";
    ]
  in
  let stdlib_dir = List.find_opt Sys.file_exists stdlib_candidates in
  let config = Typing.Module_check.default_config () in
  match stdlib_dir with
  | Some dir -> Typing.Module_check.with_stdlib dir config
  | None -> config

(** Try to read the content of a sibling .tart file for cache invalidation.
    Returns None if the file doesn't exist. *)
let read_sibling_sig_content (filename : string) : string option =
  let dir = Filename.dirname filename in
  let basename = Filename.basename filename in
  let module_name =
    if Filename.check_suffix basename ".el" then
      Filename.chop_suffix basename ".el"
    else basename
  in
  let tart_path = Filename.concat dir (module_name ^ ".tart") in
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
let check_document ~(cache : Form_cache.t) (uri : string) (text : string) :
    Protocol.diagnostic list * Form_cache.check_stats option =
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
      let config = default_module_config () in
      let sibling_sig_content = read_sibling_sig_content filename in
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
      let diagnostics, stats =
        check_document ~cache:server.form_cache uri doc.text
      in
      let params : Protocol.publish_diagnostics_params =
        { uri; version; diagnostics }
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
          (* Publish diagnostics for the changed document *)
          publish_diagnostics server uri (Some version)
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
      Document.close_doc server.documents ~uri;
      (* Also clear the form cache for this document *)
      Form_cache.remove_document server.form_cache uri;
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
let find_definition_in_signatures ~(filename : string) (name : string) :
    Syntax.Location.span option =
  let config = default_module_config () in
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
                        match find_definition_in_signatures ~filename name with
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
