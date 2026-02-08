(** URI handling for file:// URIs.

    Converts between file:// URIs used by the LSP protocol and filesystem paths.
*)

val to_filename : string -> string
(** Extract filename from a file:// URI.

    Returns the path portion, or the raw URI if not a file:// URI. *)

val of_filename : string -> string
(** Construct a file:// URI from a filesystem path. *)
