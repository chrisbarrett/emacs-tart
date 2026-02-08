(** Subprocess-based LSP client for integration testing.

    Spawns the [tart lsp] binary as a child process and communicates over stdio
    pipes, exercising the real binary, CLI argument parsing, and process
    lifecycle. *)

type t
(** A running tart LSP subprocess. *)

val start : tart_bin:string -> t
(** [start ~tart_bin] launches [tart_bin lsp] as a child process. *)

val send : t -> string -> unit
(** [send t msg] writes [msg] to the subprocess's stdin. *)

val recv : t -> Yojson.Safe.t
(** [recv t] reads a single Content-Length-delimited JSON-RPC message from the
    subprocess's stdout. Blocks until a complete message is available. *)

val recv_all : t -> timeout_ms:int -> Yojson.Safe.t list
(** [recv_all t ~timeout_ms] reads all available messages, waiting up to
    [timeout_ms] milliseconds for each message. Returns messages collected so
    far when the timeout expires. *)

val shutdown : t -> int
(** [shutdown t] closes the subprocess's stdin, waits for it to exit, and
    returns the exit code. *)
