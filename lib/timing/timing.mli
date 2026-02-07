(** Wall-clock timing with auto-scaled human-readable formatting.

    Use {!start} to capture a timestamp, then {!elapsed_s} or {!elapsed_ms} to
    measure the interval. {!format_duration} auto-scales the display. *)

type t
(** An opaque start-time marker. *)

val start : unit -> t
(** Capture the current wall-clock time. *)

val elapsed_s : t -> float
(** Seconds elapsed since [start]. *)

val elapsed_ms : t -> float
(** Milliseconds elapsed since [start]. *)

val format_duration : float -> string
(** [format_duration secs] returns a human-readable string. Auto-scales to
    ["1.23s"], ["45.0ms"], or ["123\u{03BC}s"]. *)
