(** Emacs source directory discovery and version resolution.

    Discovers the Emacs C source directory for scanning DEFUNs and DEFVARs.
    Supports auto-detection via running Emacs, or explicit path override.
    Resolves version specifiers to concrete Emacs refs.

    See Spec 29, R1-R3 for requirements; Spec 99 for version resolution. *)

(** {1 Types} *)

(** Result of source directory discovery. *)
type discovery_result =
  | Found of { source_dir : string; version : string }
      (** Source directory found with Emacs version *)
  | NotFound of string  (** Emacs not installed or source not available *)
  | InvalidPath of string  (** Specified path is invalid *)

(** A resolved version specifier. *)
type resolved_version =
  | Release of { tag : string; version : Sig.Emacs_version.version }
      (** A release tag like [emacs-29.1] *)
  | Dev  (** Development HEAD ([main] branch) *)
  | Commit of string  (** An arbitrary git commit SHA *)

(** Errors from version resolution. *)
type resolution_error =
  | No_matching_tag of string  (** No tag matches the specifier *)
  | Git_error of string  (** Git operation failed *)

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

(** {1 Version Resolution} *)

val is_sha : string -> bool
(** Check if a string looks like a git SHA (7-40 hex characters). *)

val is_dev_identifier : string -> bool
(** Check if a string is a development identifier ([dev], [devel], [git]). *)

val parse_emacs_tag : string -> Sig.Emacs_version.version option
(** Parse a tag like [emacs-29.1] into a version. Returns [None] if the tag does
    not match the expected format. *)

val list_remote_tags :
  ?url:string ->
  ?fetch_tags:(unit -> (string, resolution_error) result) ->
  unit ->
  (string list, resolution_error) result
(** Fetch release tags from the Emacs git repository.

    Returns a list of tag names like [emacs-29.1]. Filters out dereferenced tags
    and pre-release tags.

    @param url Override the repository URL (default: git.savannah.gnu.org).
    @param fetch_tags Inject raw output for testing. *)

val sort_tags_descending :
  string list -> (string * Sig.Emacs_version.version) list
(** Sort parsed tags by version, most recent first. *)

val resolve_version :
  ?fetch_tags:(unit -> (string, resolution_error) result) ->
  string ->
  (resolved_version, resolution_error) result
(** Resolve a version specifier to a concrete Emacs ref.

    Supports:
    - Semver shorthand: ["29"] → latest 29.x release tag. ["29.1"] → exact tag.
      ["29.1.2"] → exact tag.
    - ["latest"] → most recent stable release tag.
    - ["dev"], ["devel"], ["git"] → development HEAD.
    - 7+ hex chars → literal git commit SHA.

    On failure, returns [No_matching_tag] with usage and recent tags.

    @param fetch_tags Inject raw tag output for testing. *)

val format_resolution_error : resolution_error -> string
(** Format a resolution error as a user-friendly message with usage hints. *)

val resolved_version_to_string : resolved_version -> string
(** Convert a resolved version to a display string. *)
