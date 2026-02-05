(** Extract definitions from Emacs Lisp files.

    This module parses `.el` files and extracts all public definitions
    including:
    - Functions (defun, defsubst, cl-defun, defmacro, cl-defmacro)
    - Variables (defvar, defcustom, defconst, defvar-local)
    - Structs (cl-defstruct with generated accessors)
    - Classes (defclass with slots)
    - Faces (defface)

    See Spec 28 for full requirements. *)

(** {1 Types} *)

type def_kind =
  | Function  (** defun, defsubst, cl-defun *)
  | Macro  (** defmacro, cl-defmacro *)
  | Variable  (** defvar, defcustom, defconst, defvar-local *)
  | Face  (** defface *)
  | Struct  (** cl-defstruct type/constructor *)
  | StructAccessor  (** cl-defstruct field accessor *)
  | StructPredicate  (** cl-defstruct -p predicate *)
  | StructCopier  (** cl-defstruct copy- function *)
  | Class  (** defclass type *)
  | ClassAccessor  (** defclass slot accessor *)
  | ClassPredicate  (** defclass -p predicate *)

type definition = {
  name : string;
  kind : def_kind;
  span : Syntax.Location.span;
  is_private : bool;  (** Contains -- in name *)
}
(** A definition extracted from source code. *)

type extraction_result = { filename : string; definitions : definition list }
(** Result of extracting definitions from a file. *)

(** {1 Private Identifier Detection} *)

val is_private_name : string -> bool
(** Check if a name is private (contains --). *)

(** {1 Extraction} *)

val extract_definitions : Syntax.Sexp.t list -> definition list
(** Extract all definitions from a list of S-expressions. *)

val extract_from_file : string -> extraction_result option
(** Extract definitions from a file.

    Returns None if the file doesn't exist or can't be parsed. *)

(** {1 Utilities} *)

val kind_to_string : def_kind -> string
(** Pretty-print a definition kind. *)

val count_public : definition list -> int
(** Count public definitions. *)

val count_private : definition list -> int
(** Count private definitions. *)
