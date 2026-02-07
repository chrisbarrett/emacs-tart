(** Structured logging with verbosity levels and switchable output format.

    Log output is gated by a global level set once at CLI startup. Suppressed
    calls are zero-cost: format strings and arguments are never evaluated when
    the level is below threshold (via [Format.ifprintf]).

    All output goes to stderr. *)

type level =
  | Quiet  (** Suppress all log output *)
  | Normal  (** Info and errors *)
  | Verbose  (** + detailed operations *)
  | Debug  (** + trace-level internals *)

type format =
  | Text  (** Plain text with level prefix *)
  | Json  (** One JSON object per line *)

val set_level : level -> unit
(** Set the global log level. Call once at CLI startup. *)

val set_format : format -> unit
(** Set the global output format. Call once at CLI startup. *)

val level : unit -> level
(** Return the current log level. *)

val info : ('a, Format.formatter, unit) Stdlib.format -> 'a
(** Log at info level (visible at Normal and above). *)

val verbose : ('a, Format.formatter, unit) Stdlib.format -> 'a
(** Log at verbose level (visible at Verbose and above). *)

val debug : ('a, Format.formatter, unit) Stdlib.format -> 'a
(** Log at debug level (visible at Debug only). *)

val debug_with_ctx : (string * string) list -> string -> unit
(** [debug_with_ctx ctx msg] logs [msg] at debug level with structured context.

    In text format, context key-value pairs are appended. In JSON format,
    context becomes the ["ctx"] field. *)

val flush : unit -> unit
(** Flush stderr. *)
