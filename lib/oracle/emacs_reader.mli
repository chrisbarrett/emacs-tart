(** Emacs subprocess invocation for oracle comparison.

    Runs Emacs in batch mode to read and print Elisp forms using
    [prin1-to-string], providing a gold-standard reference for parser testing.
*)

(** {1 Types} *)

type emacs_error =
  | Read_error of { input : string; message : string }
  | Emacs_not_found
  | Emacs_failed of { exit_code : int; stderr : string }
  | Timeout of { timeout_ms : int }

(** {1 Emacs Discovery} *)

val find_emacs : unit -> string option
(** Search PATH for an [emacs] executable. Returns [Some path] if found. *)

(** {1 Low-level Invocation} *)

val run_batch :
  ?timeout_ms:int -> string -> (string * string, emacs_error) result
(** [run_batch ?timeout_ms expr] runs [emacs --batch --quick --eval expr] and
    returns [(stdout, stderr)]. Default timeout: 5000ms. *)

(** {1 Reading} *)

val read_string : ?timeout_ms:int -> string -> (string, emacs_error) result
(** [read_string ?timeout_ms input] reads a single Elisp form from [input] using
    Emacs, returning the [prin1-to-string] output. *)

val read_file : ?timeout_ms:int -> string -> (string list, emacs_error) result
(** [read_file ?timeout_ms path] reads all top-level forms from [path] using
    Emacs, returning a list of [prin1-to-string] outputs. *)
