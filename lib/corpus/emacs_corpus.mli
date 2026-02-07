(** Emacs source corpus management.

    Manages a shallow clone of the Emacs source repository in the XDG cache
    directory. Used for testing the type checker against real Elisp files.

    The corpus is stored at [$XDG_CACHE_HOME/tart/emacs-src/] (or
    [~/.cache/tart/emacs-src/] if unset). Only a single version is checked out
    at a time; switching versions uses shallow fetch + checkout. *)

(** {1 Error Types} *)

type corpus_error =
  | Clone_failed of string
  | Fetch_failed of string
  | Checkout_failed of string
  | No_emacs
  | Invalid_ref of string

val corpus_error_to_string : corpus_error -> string
(** Human-readable description of the error *)

(** {1 Path Resolution} *)

val corpus_dir : unit -> string
(** Absolute path to the corpus directory. Respects [$XDG_CACHE_HOME]. *)

(** {1 Git Operations} *)

val run_git : ?cwd:string -> string list -> (string, corpus_error) result
(** Run a git command with the given arguments. Returns stdout on success,
    [Clone_failed] or [Fetch_failed] on failure depending on the command. *)

val ensure_clone :
  ?url:string -> tag:string -> unit -> (unit, corpus_error) result
(** Clone the Emacs repository if not already present. Uses [--depth 1] for a
    shallow clone. No-ops if the corpus directory already exists. *)

val checkout : string -> (unit, corpus_error) result
(** Fetch and checkout a git ref (tag, branch, or SHA). Uses shallow fetch for
    tags (e.g. [emacs-31.1]). *)

(** {1 Version Detection} *)

val detect_tag : unit -> (string, corpus_error) result
(** Auto-detect the system Emacs version and return the corresponding tag (e.g.
    [emacs-31.1]). Returns [No_emacs] if Emacs is not found. *)

val version_to_tag : Sig.Emacs_version.version -> string
(** Convert a parsed version to a git tag like [emacs-31.1]. *)

(** {1 File Listing} *)

val list_el_files : unit -> string list
(** List all [.el] files in the corpus directory, as absolute paths. Returns
    empty list if corpus is not cloned. *)

(** {1 Cache Management} *)

val clean : unit -> int
(** Remove the corpus directory. Returns approximate bytes freed. Returns 0 if
    the directory does not exist. *)

val is_cloned : unit -> bool
(** Whether the corpus directory exists *)
