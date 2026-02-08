(** Background domain for asynchronous type checking.

    Runs a single background domain that processes type-check work items. The
    main domain enqueues work via {!enqueue} and polls for results via
    {!poll_results}. A Unix pipe signals when results are available so the
    caller can use [Unix.select] to multiplex stdin and result readiness.

    Work items coalesce by URI: enqueueing a check for a URI that already has a
    pending entry replaces the old entry. *)

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
(** A unit of work to be processed by the background domain. *)

type work_result = {
  wr_uri : string;
  wr_version : int;
  wr_diagnostics : Protocol.diagnostic list;
  wr_stats : Form_cache.check_stats option;
}
(** The result of a completed type check. *)

(** {1 Worker Lifecycle} *)

type t
(** A background worker. *)

val create : unit -> t
(** Spawn a background domain and return the worker handle. *)

val shutdown : t -> unit
(** Signal the worker to stop and join its domain. *)

(** {1 Enqueue / Poll} *)

val enqueue : t -> work_item -> unit
(** Add or replace a work item for the given URI. Writes a wake-up byte to the
    signal pipe. *)

val poll_results : t -> work_result list
(** Drain all completed results (non-blocking). Returns an empty list when no
    results are available. *)

val signal_fd : t -> Unix.file_descr
(** The read end of the signal pipe. Becomes readable when new results have been
    posted. Use with [Unix.select] to avoid busy-waiting. *)

val pending_count : t -> int
(** Number of items still in the work queue (not yet picked up by the background
    domain). Does not count the item currently being processed. *)
