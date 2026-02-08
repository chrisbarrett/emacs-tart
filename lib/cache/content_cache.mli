(** Content-addressable file cache.

    XDG-compliant cache using content-based keys for incremental type-checking
    results. All operations are best-effort: cache failures never propagate as
    exceptions. *)

val cache_dir : unit -> string
(** Cache root directory. Uses [$XDG_CACHE_HOME/tart/] when set, otherwise
    [~/.cache/tart/]. *)

val binary_path : unit -> string
(** Path to the currently running executable, resolved through symlinks. *)

val compute_key : binary:string -> input:string -> deps:string list -> string
(** [compute_key ~binary ~input ~deps] hashes the contents of [binary], [input],
    and all [deps] files, returning a 32-char hex string. [deps] are sorted
    before hashing for deterministic keys regardless of enumeration order.
    Returns empty string if [binary] or [input] is unreadable; unreadable deps
    are skipped. *)

val store : key:string -> data:string -> unit
(** [store ~key ~data] writes [data] to the cache under [key]. Creates
    directories on demand. Uses atomic write (temp + rename). Silently does
    nothing on I/O failure. *)

val retrieve : key:string -> string option
(** [retrieve ~key] returns [Some data] if [key] is cached and the stored JSON
    envelope is valid, [None] otherwise. Never raises. *)

val evict_older_than : days:int -> unit
(** [evict_older_than ~days] deletes cache entries with mtime older than [days]
    days. Removes empty prefix directories. Best-effort: individual file
    failures are logged and skipped. *)

val maybe_evict : unit -> unit
(** Run eviction if it hasn't been done recently (within 1 hour). Uses a
    [.last-eviction] marker file. Never raises. *)
