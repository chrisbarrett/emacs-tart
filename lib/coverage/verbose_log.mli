(** Verbose logging utility for coverage commands.

    Provides conditional logging to stderr with a [verbose] prefix.
    Used by coverage and emacs-coverage commands for debugging output.

    See Spec 30 for requirements. *)

val verbose_log : bool -> ('a, out_channel, unit) format -> 'a
(** [verbose_log verbose fmt ...] logs a formatted message to stderr when
    [verbose] is true. Output is prefixed with [[verbose]].

    When [verbose] is false, arguments are discarded and nothing is printed. *)

val always_log : ('a, out_channel, unit) format -> 'a
(** [always_log fmt ...] logs unconditionally to stderr with [[verbose]] prefix.
    Useful for errors or warnings that should always appear. *)

val flush : unit -> unit
(** Flush stderr to ensure all verbose output is written. *)
