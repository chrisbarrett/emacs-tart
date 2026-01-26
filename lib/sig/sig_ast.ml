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

(** {1 Kinds} *)

(** Kind expressions in signature files.

    These are surface syntax kinds that mirror the internal kind representation.
    Used for explicit kind annotations on type variable binders. *)
type sig_kind =
  | SKStar  (** Concrete type kind [*] *)
  | SKArrow of sig_kind * sig_kind  (** Type constructor kind [* -> *] *)

(** {1 Type Variable Binders} *)

type tvar_binder = {
  name : string;
  bound : sig_type option;  (** Upper bound, if any *)
  kind : sig_kind option;  (** Explicit kind annotation, if any *)
  loc : span;
}
(** A type variable binder, optionally with an upper bound or kind annotation.

    Examples:
    - [a] - unbounded type variable
    - [(a : truthy)] - bounded type variable
    - [(f : (* -> *))] - kind-annotated type variable *)

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
  | DData of data_decl  (** [(data name ...)] - algebraic data type *)

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

and ctor_decl = {
  ctor_name : string;  (** Constructor name (e.g., "Ok", "Err") *)
  ctor_fields : sig_type list;  (** Field types *)
  ctor_loc : span;
}
(** ADT constructor declaration.

    Examples:
    - [(Ok a)] - single-field constructor
    - [(Point2D int int)] - multi-field constructor
    - [(None)] - nullary constructor *)

and data_decl = {
  data_name : string;  (** Type name (e.g., "result") *)
  data_params : tvar_binder list;  (** Type parameters (e.g., [a e]) *)
  data_ctors : ctor_decl list;  (** Constructor declarations *)
  data_loc : span;
}
(** Algebraic data type declaration.

    Example:
    - [(data result [a e] (Ok a) (Err e))]

    Generates: constructor functions, predicates, and accessors. *)

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

(** Get the source location of a type expression *)
let sig_type_loc = function
  | STVar (_, loc) -> loc
  | STCon (_, loc) -> loc
  | STApp (_, _, loc) -> loc
  | STArrow (_, _, loc) -> loc
  | STForall (_, _, loc) -> loc
  | STUnion (_, loc) -> loc
  | STTuple (_, loc) -> loc

(** Get the source location of a declaration *)
let decl_loc = function
  | DOpen (_, loc) -> loc
  | DInclude (_, loc) -> loc
  | DDefun d -> d.defun_loc
  | DDefvar d -> d.defvar_loc
  | DType d -> d.type_loc
  | DImportStruct d -> d.struct_loc
  | DData d -> d.data_loc

(** {1 Constructors} *)

(** Create a simple unbounded type variable binder *)
let make_tvar_binder ~name ~loc : tvar_binder =
  { name; bound = None; kind = None; loc }

(** Create a bounded type variable binder *)
let make_bounded_tvar_binder ~name ~bound ~loc : tvar_binder =
  { name; bound = Some bound; kind = None; loc }

(** Create a kind-annotated type variable binder *)
let make_kinded_tvar_binder ~name ~kind ~loc : tvar_binder =
  { name; bound = None; kind = Some kind; loc }

(** Create a type constant *)
let st_con name loc = STCon (name, loc)

(** Create a type variable reference *)
let st_var name loc = STVar (name, loc)

(** Create a type application *)
let st_app name args loc = STApp (name, args, loc)

(** Create an arrow type *)
let st_arrow params ret loc = STArrow (params, ret, loc)

(** Create a forall type *)
let st_forall binders body loc = STForall (binders, body, loc)

(** Create a union type *)
let st_union types loc = STUnion (types, loc)

(** Create a tuple type *)
let st_tuple types loc = STTuple (types, loc)
