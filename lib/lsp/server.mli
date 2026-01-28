(** LSP Server state and main loop.

    Manages server lifecycle: uninitialized -> initialized -> shutdown. *)

(** Server state *)
type state =
  | Uninitialized
  | Initialized of { root_uri : string option }
  | ShuttingDown

(** Log level *)
type log_level = Quiet | Normal | Debug

type t
(** Server instance *)

val create :
  ?log_level:log_level -> ic:In_channel.t -> oc:Out_channel.t -> unit -> t
(** Create a new server on the given channels.

    @param log_level Controls logging verbosity (default: Normal)
    @param ic Input channel (typically stdin)
    @param oc Output channel (typically stdout) *)

val state : t -> state
(** Get the server's current state *)

val documents : t -> Document.t
(** Get the server's document store (for testing) *)

val dependency_graph : t -> Graph.Dependency_graph.t
(** Get the server's dependency graph (for testing) *)

val run : t -> int
(** Run the server's main loop.

    Reads messages, dispatches to handlers, sends responses. Returns exit code
    (0 for clean shutdown, 1 for error). *)

val debug : t -> string -> unit
(** Log a message at debug level *)

val info : t -> string -> unit
(** Log a message at info level *)
