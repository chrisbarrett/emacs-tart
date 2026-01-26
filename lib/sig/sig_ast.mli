(** Signature file AST.

    This module defines the abstract syntax tree for `.tart` signature files.
    Signature files declare types for Elisp modules without executable code.

    Example signature file:
    {[
      (open 'seq)

      (type int-list (list int))
      (type buffer)  ; opaque type

      (defun my-add (int int) -> int)
      (defun identity [a] (a) -> a)

      (defvar my-default string)
    ]} *)

open Syntax.Location

(** {1 Type Variable Binders} *)

type tvar_binder = {
  name : string;
  bound : sig_type option;  (** Upper bound, if any *)
  loc : span;
}
(** A type variable binder, optionally with an upper bound.

    Examples:
    - [a] - unbounded type variable
    - [(a : truthy)] - bounded type variable *)

(** {1 Type Expressions} *)

(** Type expressions in signature files.

    These are surface syntax types that get resolved to [Core.Types.typ] during
    loading. Unlike [Types.typ], these preserve source locations and don't use
    mutable references. *)
and sig_type =
  | STVar of string * span  (** Type variable reference (e.g., [a]) *)
  | STCon of string * span  (** Type constant (e.g., [int], [string], [nil]) *)
  | STApp of string * sig_type list * span
      (** Type application (e.g., [(list int)], [(option string)]) *)
  | STArrow of sig_param list * sig_type * span
      (** Function type (e.g., [(int int) -> int]) *)
  | STForall of tvar_binder list * sig_type * span
      (** Polymorphic type with explicit quantifiers (e.g., [[a] (a) -> a]) *)
  | STUnion of sig_type list * span  (** Union type (e.g., [(int | string)]) *)
  | STTuple of sig_type list * span
      (** Tuple type (e.g., [(tuple int string bool)]) *)

(** Function parameter in signature types *)
and sig_param =
  | SPPositional of sig_type  (** Required positional parameter *)
  | SPOptional of sig_type  (** Optional parameter (&optional) *)
  | SPRest of sig_type  (** Rest parameter (&rest) *)
  | SPKey of string * sig_type  (** Keyword parameter (&key :name type) *)

(** {1 Declarations} *)

(** A single declaration in a signature file *)
type decl =
  | DOpen of string * span
      (** [(open 'module)] - import types for use (not re-exported) *)
  | DInclude of string * span
      (** [(include 'module)] - inline and re-export declarations *)
  | DDefun of defun_decl  (** [(defun name ...)] - function signature *)
  | DDefvar of defvar_decl  (** [(defvar name type)] - variable declaration *)
  | DType of type_decl  (** [(type name ...)] - type alias or opaque type *)
  | DImportStruct of import_struct_decl
      (** [(import-struct name ...)] - struct import *)

and defun_decl = {
  defun_name : string;
  defun_tvar_binders : tvar_binder list;
      (** Type variables with optional bounds *)
  defun_params : sig_param list;  (** Parameter types *)
  defun_return : sig_type;  (** Return type *)
  defun_loc : span;
}
(** Function signature declaration.

    Examples:
    - [(defun add (int int) -> int)] - monomorphic
    - [(defun identity [a] (a) -> a)] - polymorphic
    - [(defun map [a b] (((a -> b)) (list a)) -> (list b))] - higher-order *)

and defvar_decl = {
  defvar_name : string;
  defvar_type : sig_type;
  defvar_loc : span;
}
(** Variable declaration.

    Examples:
    - [(defvar my-default string)] - simple type
    - [(defvar my-handler ((string) -> nil))] - function value type *)

and type_decl = {
  type_name : string;
  type_params : tvar_binder list;  (** Type parameters with optional bounds *)
  type_body : sig_type option;  (** None for opaque types *)
  type_loc : span;
}
(** Type declaration (alias or opaque).

    Examples:
    - [(type int-list (list int))] - alias
    - [(type result [a e] ((ok a) | (err e)))] - parameterized alias
    - [(type buffer)] - opaque (no definition)
    - [(type tagged [a])] - opaque with phantom parameter *)

and import_struct_decl = {
  struct_name : string;
  struct_slots : (string * sig_type) list;  (** Slot name and type *)
  struct_loc : span;
}
(** Struct import declaration.

    Example:
    - [(import-struct person :slots ((name string) (age int)))]

    Generates: type person, make-person, person-p, and accessors. *)

(** {1 Signature File} *)

type signature = {
  sig_module : string;  (** Module name derived from filename *)
  sig_decls : decl list;  (** Declarations in source order *)
  sig_loc : span;  (** Span of entire file *)
}
(** A complete signature file.

    The module name is derived from the filename (e.g., [foo.tart] -> [foo]).
    Contains a list of declarations in source order. *)

(** {1 Location Accessors} *)

val sig_type_loc : sig_type -> span
(** Get the source location of a type expression *)

val decl_loc : decl -> span
(** Get the source location of a declaration *)

(** {1 Constructors} *)

val make_tvar_binder : name:string -> loc:span -> tvar_binder
(** Create a simple unbounded type variable binder *)

val make_bounded_tvar_binder :
  name:string -> bound:sig_type -> loc:span -> tvar_binder
(** Create a bounded type variable binder *)

val st_con : string -> span -> sig_type
(** Create a type constant *)

val st_var : string -> span -> sig_type
(** Create a type variable reference *)

val st_app : string -> sig_type list -> span -> sig_type
(** Create a type application *)

val st_arrow : sig_param list -> sig_type -> span -> sig_type
(** Create an arrow type *)

val st_forall : tvar_binder list -> sig_type -> span -> sig_type
(** Create a forall type *)

val st_union : sig_type list -> span -> sig_type
(** Create a union type *)

val st_tuple : sig_type list -> span -> sig_type
(** Create a tuple type *)
