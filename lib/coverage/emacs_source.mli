(** Emacs source directory discovery.

    Discovers the Emacs C source directory for scanning DEFUNs and DEFVARs.
    Supports auto-detection via running Emacs, or explicit path override.

    See Spec 29, R1-R3 for requirements. *)

(** {1 Types} *)

(** Result of source directory discovery. *)
type discovery_result =
  | Found of { source_dir : string; version : string }
      (** Source directory found with Emacs version *)
  | NotFound of string  (** Emacs not installed or source not available *)
  | InvalidPath of string  (** Specified path is invalid *)

(** {1 Path Validation} *)

val is_valid_emacs_source : string -> bool
(** Check if a directory looks like valid Emacs source.

    A valid source directory either:
    - Contains src/*.c files (traditional layout)
    - Contains *.c files directly (e.g., find-function-C-source-directory) *)

(** {1 Discovery} *)

val detect : unit -> discovery_result
(** Detect Emacs source directory automatically.

    Uses [find-function-C-source-directory] from a running Emacs to find the
    source location. Returns [NotFound] with helpful message if:
    - Emacs is not installed
    - Source is not available (e.g., package manager install without source) *)

val from_path : string -> discovery_result
(** Use an explicit source path.

    Validates that the path contains src/*.c files. *)

val discover : explicit_path:string option -> discovery_result
(** Discover Emacs source, using explicit path if provided.

    @param explicit_path If [Some path], uses that path; otherwise auto-detects
*)

(** {1 Error Formatting} *)

val format_error : discovery_result -> string
(** Format a discovery error as a user-friendly message. *)
