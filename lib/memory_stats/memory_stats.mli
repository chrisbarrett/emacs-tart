(** GC and memory statistics with human-readable formatting.

    {!snapshot} captures a point-in-time GC state. {!diff} computes allocation
    and collection deltas between two snapshots. {!format_summary} reports
    overall heap state. *)

type snapshot
(** Opaque GC state captured at a point in time. *)

type delta
(** Difference between two snapshots. *)

val snapshot : unit -> snapshot
(** Capture current GC statistics. *)

val diff : before:snapshot -> after:snapshot -> delta
(** Compute allocation and collection deltas. *)

val format_delta : delta -> string
(** Format delta as e.g. ["1.2MB alloc, 3 minor GC"]. *)

val format_summary : unit -> string
(** Format current heap state: heap size, GC counts, total allocation. *)

val format_bytes : float -> string
(** [format_bytes n] auto-scales to ["1.2MB"], ["45.3KB"], or ["123B"]. *)
