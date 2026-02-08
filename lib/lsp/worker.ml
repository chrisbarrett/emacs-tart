(** Background domain for asynchronous type checking.

    A single OCaml 5 domain processes type-check work items from a shared queue.
    Results are posted to a result queue and a byte is written to a Unix pipe to
    wake the main loop's [Unix.select]. *)

module Log = Tart_log.Log

(** {1 Types} *)

type work_item = {
  uri : string;
  text : string;
  version : int;
  config : Typing.Module_check.config;
  cache : Form_cache.t;
  sig_tracker : Signature_tracker.t;
  dependency_graph : Graph.Dependency_graph.t;
  is_tart : bool;
}

type work_result = {
  wr_uri : string;
  wr_version : int;
  wr_diagnostics : Protocol.diagnostic list;
  wr_stats : Form_cache.check_stats option;
}

type shared = {
  work_queue : (string, work_item) Hashtbl.t;
  work_mutex : Mutex.t;
  work_condition : Condition.t;
  result_queue : work_result Queue.t;
  result_mutex : Mutex.t;
  signal_write : Unix.file_descr;
  stop : bool Atomic.t;
  in_flight : int Atomic.t;
      (** Number of items taken from the queue but not yet posted as results.
          Used by [pending_count] to include items currently being processed. *)
}
(** Shared mutable state accessed by both domains. *)

(** {1 Internal state} *)

type t = {
  shared : shared;
  signal_read : Unix.file_descr;
  domain : unit Domain.t;
}

(** {1 Diagnostic checking (runs on background domain)} *)

(** Check a .tart signature file and return LSP diagnostics. *)
let check_tart_document (text : string) (uri : string) :
    Protocol.diagnostic list =
  let filename = Uri.to_filename uri in
  let parse_result = Syntax.Read.parse_string ~filename text in
  if parse_result.errors <> [] then
    List.map
      (fun (err : Syntax.Read.parse_error) ->
        {
          Protocol.range = Span_conv.range_of_span ~text err.span;
          severity = Some Protocol.Error;
          code = None;
          message = err.message;
          source = Some "tart";
          related_information = [];
        })
      parse_result.errors
  else
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
            {
              Protocol.range = Span_conv.range_of_span ~text e.span;
              severity = Some Protocol.Error;
              code = None;
              message = e.message;
              source = Some "tart";
              related_information = [];
            })
          errors
    | Ok sig_file ->
        let validation_errors =
          Sig.Sig_loader.validate_signature_all sig_file
        in
        List.map
          (fun (e : Sig.Sig_loader.load_error) ->
            {
              Protocol.range = Span_conv.range_of_span ~text e.span;
              severity = Some Protocol.Error;
              code = None;
              message = e.message;
              source = Some "tart";
              related_information = [];
            })
          validation_errors

(** Read the content of a sibling .tart file for cache invalidation. *)
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
  match Signature_tracker.get_by_path sig_tracker tart_path with
  | Some content -> Some content
  | None ->
      if Sys.file_exists tart_path then
        try
          let ic = In_channel.open_text tart_path in
          let content = In_channel.input_all ic in
          In_channel.close ic;
          Some content
        with _ -> None
      else None

(** Convert a [Typing.Diagnostic.related_location] to LSP format. *)
let related_info_of_related_location ~(text : string)
    (rel : Typing.Diagnostic.related_location) :
    Protocol.diagnostic_related_information option =
  if rel.span.start_pos.file = "<generated>" then None
  else
    Some
      {
        Protocol.location = Span_conv.location_of_span ~text rel.span;
        message = rel.message;
      }

(** Convert a [Typing.Diagnostic.t] to an LSP diagnostic. *)
let lsp_diagnostic_of_diagnostic ~(text : string) (d : Typing.Diagnostic.t) :
    Protocol.diagnostic =
  let severity =
    match d.severity with
    | Typing.Diagnostic.Error -> Protocol.Error
    | Typing.Diagnostic.Warning -> Protocol.Warning
    | Typing.Diagnostic.Hint -> Protocol.Hint
  in
  let message =
    match d.help with
    | [] -> d.message
    | helps ->
        let help_lines =
          List.map (fun h -> "help: " ^ h) helps |> String.concat "\n"
        in
        d.message ^ "\n\n" ^ help_lines
  in
  let code =
    match d.code with
    | Some c -> Some (Typing.Diagnostic.error_code_to_string c)
    | None -> None
  in
  let related_information =
    List.filter_map (related_info_of_related_location ~text) d.related
  in
  {
    Protocol.range = Span_conv.range_of_span ~text d.span;
    severity = Some severity;
    code;
    message;
    source = Some "tart";
    related_information;
  }

(** Convert a parse error to an LSP diagnostic. *)
let lsp_diagnostic_of_parse_error ~(text : string)
    (err : Syntax.Read.parse_error) : Protocol.diagnostic =
  {
    Protocol.range = Span_conv.range_of_span ~text err.span;
    severity = Some Protocol.Error;
    code = None;
    message = err.message;
    source = Some "tart";
    related_information = [];
  }

(** Type-check a .el document and return LSP diagnostics. *)
let check_el_document ~(config : Typing.Module_check.config)
    ~(cache : Form_cache.t) ~(sig_tracker : Signature_tracker.t) (uri : string)
    (text : string) : Protocol.diagnostic list * Form_cache.check_stats option =
  let filename = Uri.to_filename uri in
  let config =
    match Sig.Package_header.parse_package_requires text with
    | Some v ->
        let core_v : Core.Type_env.emacs_version =
          { major = v.major; minor = v.minor }
        in
        Typing.Module_check.with_declared_version core_v config
    | None -> config
  in
  let parse_result = Syntax.Read.parse_string ~filename text in
  let parse_diagnostics =
    List.map (lsp_diagnostic_of_parse_error ~text) parse_result.errors
  in
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
      ( List.map (lsp_diagnostic_of_diagnostic ~text) typing_diagnostics,
        Some stats )
  in
  (parse_diagnostics @ type_diagnostics, stats)

(** Process a single work item. *)
let process_item (item : work_item) : work_result =
  let diagnostics, stats =
    if item.is_tart then (check_tart_document item.text item.uri, None)
    else
      check_el_document ~config:item.config ~cache:item.cache
        ~sig_tracker:item.sig_tracker item.uri item.text
  in
  let cycle_diagnostics =
    if item.is_tart then []
    else
      Graph_tracker.check_cycles_for_module item.dependency_graph ~uri:item.uri
  in
  {
    wr_uri = item.uri;
    wr_version = item.version;
    wr_diagnostics = diagnostics @ cycle_diagnostics;
    wr_stats = stats;
  }

(** Write a single byte to wake the main loop. *)
let wake (s : shared) : unit =
  let _n = Unix.write s.signal_write (Bytes.of_string "w") 0 1 in
  ()

(** Drain the wake pipe so [Unix.select] won't fire again until new results. *)
let drain_signal (fd : Unix.file_descr) : unit =
  let buf = Bytes.create 64 in
  (try
     Unix.set_nonblock fd;
     while Unix.read fd buf 0 64 > 0 do
       ()
     done
   with
   | Unix.Unix_error (Unix.EAGAIN, _, _)
   | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
   ->
     ());
  Unix.clear_nonblock fd

(** Background domain entry point. *)
let worker_loop (s : shared) : unit =
  while not (Atomic.get s.stop) do
    Mutex.lock s.work_mutex;
    while Hashtbl.length s.work_queue = 0 && not (Atomic.get s.stop) do
      Condition.wait s.work_condition s.work_mutex
    done;
    let item =
      if Atomic.get s.stop then None
      else
        let item_opt =
          Hashtbl.fold (fun _uri item _acc -> Some item) s.work_queue None
        in
        (match item_opt with
        | Some item ->
            Hashtbl.remove s.work_queue item.uri;
            Atomic.incr s.in_flight
        | None -> ());
        item_opt
    in
    Mutex.unlock s.work_mutex;
    match item with
    | None -> ()
    | Some item ->
        let result =
          try process_item item
          with exn ->
            Log.info "Worker exception for %s: %s" item.uri
              (Printexc.to_string exn);
            {
              wr_uri = item.uri;
              wr_version = item.version;
              wr_diagnostics = [];
              wr_stats = None;
            }
        in
        Mutex.lock s.result_mutex;
        Queue.push result s.result_queue;
        Mutex.unlock s.result_mutex;
        Atomic.decr s.in_flight;
        wake s
  done

(** {1 Public API} *)

let create () : t =
  let signal_read, signal_write = Unix.pipe () in
  let shared =
    {
      work_queue = Hashtbl.create 16;
      work_mutex = Mutex.create ();
      work_condition = Condition.create ();
      result_queue = Queue.create ();
      result_mutex = Mutex.create ();
      signal_write;
      stop = Atomic.make false;
      in_flight = Atomic.make 0;
    }
  in
  let domain = Domain.spawn (fun () -> worker_loop shared) in
  { shared; signal_read; domain }

let shutdown (t : t) : unit =
  Atomic.set t.shared.stop true;
  Mutex.lock t.shared.work_mutex;
  Condition.signal t.shared.work_condition;
  Mutex.unlock t.shared.work_mutex;
  Domain.join t.domain;
  Unix.close t.signal_read;
  Unix.close t.shared.signal_write

let enqueue (t : t) (item : work_item) : unit =
  Mutex.lock t.shared.work_mutex;
  Hashtbl.replace t.shared.work_queue item.uri item;
  Condition.signal t.shared.work_condition;
  Mutex.unlock t.shared.work_mutex

let poll_results (t : t) : work_result list =
  drain_signal t.signal_read;
  Mutex.lock t.shared.result_mutex;
  let results = Queue.fold (fun acc r -> r :: acc) [] t.shared.result_queue in
  Queue.clear t.shared.result_queue;
  Mutex.unlock t.shared.result_mutex;
  List.rev results

let signal_fd (t : t) : Unix.file_descr = t.signal_read

let pending_count (t : t) : int =
  Mutex.lock t.shared.work_mutex;
  let queued = Hashtbl.length t.shared.work_queue in
  Mutex.unlock t.shared.work_mutex;
  Mutex.lock t.shared.result_mutex;
  let undelivered = Queue.length t.shared.result_queue in
  Mutex.unlock t.shared.result_mutex;
  queued + Atomic.get t.shared.in_flight + undelivered
