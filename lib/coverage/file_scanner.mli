(** Scan directories for Emacs Lisp files.

    This module implements directory scanning for the coverage command,
    supporting recursive directory traversal, explicit paths, and exclude
    patterns.

    @see Spec 28, R1, R2, R17 for requirements. *)

(** {1 Types} *)

type scan_config = {
  exclude_patterns : string list;
      (** Glob patterns for files to exclude (e.g., "*-test.el"). *)
}
(** Configuration for file scanning. *)

val default_config : scan_config
(** Default configuration with no exclusions. *)

(** {1 Pattern Matching} *)

val matches_exclude : patterns:string list -> string -> bool
(** Check if a filename matches any exclude pattern. *)

(** {1 File Discovery} *)

val is_elisp_file : string -> bool
(** Check if a file is an Emacs Lisp source file. *)

val scan_directory : config:scan_config -> string -> string list
(** List all .el files in a directory recursively. *)

val scan_path : config:scan_config -> string -> string list
(** Scan a single path (file or directory).

    If path is a file, returns it if it's a .el file. If path is a directory,
    scans it recursively. *)

val scan_paths : config:scan_config -> string list -> string list
(** Scan multiple paths.

    If no paths are given, scans the current directory. Results are sorted and
    deduplicated. *)
