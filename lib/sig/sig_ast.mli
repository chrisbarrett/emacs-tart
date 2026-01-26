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
  | DTypeScope of type_scope_decl
      (** [(type-scope [vars] ...)] - scoped type variable declarations *)
  | DClass of class_decl  (** [(class (Name a) ...)] - type class definition *)
  | DInstance of instance_decl
      (** [(instance (Class type) ...)] - type class instance *)

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

and type_scope_decl = {
  scope_tvar_binders : tvar_binder list;
      (** Type variables shared across declarations in this scope *)
  scope_decls : decl list;  (** Declarations within the scope *)
  scope_loc : span;
}
(** Type scope declaration for sharing type variables across signatures.

    Example:
    - [(type-scope [a] (defun iter-next ((iter a)) -> (a | nil)) (defun
       iter-peek ((iter a)) -> (a | nil)))]

    The type variable [a] is shared across all declarations in the scope. *)

and class_method = {
  method_name : string;  (** Method name (e.g., "eq", "compare") *)
  method_tvar_binders : tvar_binder list;
      (** Method-local type variables (for HKT methods like fmap) *)
  method_params : sig_param list;  (** Parameter types *)
  method_return : sig_type;  (** Return type *)
  method_loc : span;
}
(** Type class method declaration.

    Examples:
    - [(eq (a a) -> bool)] - simple method using class type variable
    - [(fmap [a b] (((a) -> b) (f a)) -> (f b))] - HKT method with local vars *)

and class_decl = {
  class_name : string;  (** Class name (e.g., "Eq", "Functor") *)
  class_tvar_binder : tvar_binder;
      (** The single type parameter for the class *)
  class_superclasses : (string * sig_type) list;
      (** Superclass constraints: (ClassName, type-arg) pairs *)
  class_methods : class_method list;  (** Method signatures *)
  class_loc : span;
}
(** Type class definition.

    Examples:
    - [(class (Eq a) (eq (a a) -> bool))] - simple class
    - [(class (Ord a) (Eq a) (compare (a a) -> ordering))] - with superclass
    - [(class (Functor (f : (* -> *))) (fmap [a b] ...))] - HKT class *)

and method_impl = {
  impl_method : string;  (** Class method name (e.g., "eq") *)
  impl_fn : string;  (** Implementation function name (e.g., "int-eq") *)
  impl_loc : span;
}
(** Method implementation mapping in an instance declaration.

    Example: [(eq . int-eq)] maps the [eq] method to the [int-eq] function. *)

and instance_decl = {
  inst_class : string;  (** Class name (e.g., "Eq", "Functor") *)
  inst_type : sig_type;  (** Type being instantiated (e.g., int, (list a)) *)
  inst_tvar_binders : tvar_binder list;
      (** Type variables for parameterized instances *)
  inst_constraints : (string * sig_type) list;
      (** Required constraints for parameterized instances: (ClassName,
          type-arg) *)
  inst_methods : method_impl list;  (** Method implementations *)
  inst_loc : span;
}
(** Type class instance declaration.

    Examples:
    - [(instance (Eq int) (eq . =))] - simple instance
    - [(instance [a] (Eq a) => (Eq (list a)) (eq . list-eq))] - parameterized
    - [(instance (Functor list) (fmap . mapcar))] - HKT instance *)

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

val make_kinded_tvar_binder :
  name:string -> kind:sig_kind -> loc:span -> tvar_binder
(** Create a kind-annotated type variable binder *)

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
