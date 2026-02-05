(** C source scanner for Emacs DEFUNs, DEFVARs, and DEFSYMs.

    Parses Emacs C source files to extract Lisp-accessible definitions. Supports
    DEFUN (functions), DEFVAR_* (variables), and DEFSYM (symbols).

    See Spec 29, R4-R6 for requirements. *)

(** {1 Types} *)

(** Kind of C definition. *)
type def_kind =
  | Defun  (** Function defined via DEFUN macro *)
  | Defvar  (** Variable defined via DEFVAR_LISP, DEFVAR_INT, or DEFVAR_BOOL *)
  | Defsym  (** Symbol defined via DEFSYM macro *)

type c_definition = {
  name : string;  (** Lisp name (e.g., "car", "load-path") *)
  kind : def_kind;  (** Kind of definition *)
  file : string;  (** Source file containing the definition *)
  line : int;  (** Line number in source file *)
}
(** A definition extracted from C source. *)

(** {1 Scanning} *)

val scan_file : string -> c_definition list
(** Scan a single C file for definitions.

    Parses DEFUN, DEFVAR_*, and DEFSYM macro invocations. Returns all
    definitions found in the file. *)

val scan_dir : string -> c_definition list
(** Scan all C files in a directory.

    Scans all [*.c] files in the given directory (non-recursive). Returns
    definitions from all files. *)

(** {1 Filtering} *)

val is_private : string -> bool
(** Check if a name is private (contains "--"). *)

val partition_public_private :
  c_definition list -> c_definition list * c_definition list
(** Partition definitions into (public, private). *)
