(** Type representation for the Tart type system.

    This module defines the core type representation used for type inference and
    checking. It uses mutable type variables with union-find for efficient
    unification.

    Based on levels-based Hindley-Milner inference with union-find for
    near-linear generalization performance. *)

(** {1 Type Variables} *)

type tvar_id = int
(** Unique identifier for type variables. *)

(** Type variable state - uses union-find representation.

    - [Unbound (id, level)] is an unresolved type variable. The level tracks the
      scope depth for let-generalization.
    - [Link ty] indicates this variable has been unified with [ty]. *)
type tvar = Unbound of tvar_id * int  (** id, level *) | Link of typ

(** {1 Types} *)

(** Type representation.

    - [TVar] - Mutable type variable for union-find unification
    - [TCon] - Type constant (Int, String, Nil, T, etc.)
    - [TApp] - Type application (List a, Option a, etc.). The first element is
      the type constructor (TCon for concrete types, TVar for higher-kinded)
    - [TArrow] - Function type with grouped parameters
    - [TForall] - Universally quantified type
    - [TUnion] - Union types (Or a b)
    - [TTuple] - Fixed-length heterogeneous tuples *)
and typ =
  | TVar of tvar ref
  | TCon of string
  | TApp of typ * typ list
  | TArrow of param list * typ
  | TForall of string list * typ
  | TUnion of typ list
  | TTuple of typ list

(** Function parameter kinds.

    Elisp functions can have positional, optional, rest, and keyword arguments.
*)
and param =
  | PPositional of typ
  | POptional of typ  (** Type should be (Option a) *)
  | PRest of typ  (** Type is the element type; expands to (List a) *)
  | PKey of string * typ  (** :keyword name and type *)

(** {1 Comparison} *)

val equal : typ -> typ -> bool
(** Structural equality for types (after following links). Type variables are
    equal only if they have the same identity. *)

val equal_tvar_id : tvar_id -> tvar_id -> bool

(** {1 Pretty-printing} *)

val show_tvar_id : tvar_id -> string
val pp_tvar_id : Format.formatter -> tvar_id -> unit

val to_string : typ -> string
(** Pretty-print a type to string. *)

val param_to_string : param -> string
(** Pretty-print a function parameter to string. *)

(** {1 Type variable operations} *)

val reset_tvar_counter : unit -> unit
(** Reset the type variable counter (for testing). *)

val fresh_tvar : int -> typ
(** [fresh_tvar level] creates a fresh unbound type variable at the given level.
*)

val tvar_id : tvar ref -> tvar_id option
(** Get the ID of an unbound type variable, or [None] if linked. *)

val tvar_level : tvar ref -> int option
(** Get the level of an unbound type variable, or [None] if linked. *)

val repr : typ -> typ
(** [repr ty] follows links to find the representative type. Performs path
    compression for amortized near-constant lookup. *)

val is_tvar : typ -> bool
(** Check if a type is a type variable (after following links). *)

(** {1 Primitive types} *)

(** Primitive type constants. *)
module Prim : sig
  val int : typ
  val float : typ
  val num : typ
  val string : typ
  val symbol : typ
  val keyword : typ
  val nil : typ
  val t : typ
  val truthy : typ
  val bool : typ
  val any : typ
  val never : typ
end

(** {1 Type constructors} *)

val list_of : typ -> typ
(** [list_of elem] creates [(List elem)]. *)

val vector_of : typ -> typ
(** [vector_of elem] creates [(Vector elem)]. *)

val option_of : typ -> typ
(** [option_of elem] creates [(Option elem)]. Note: Does not validate the truthy
    constraint. *)

val pair_of : typ -> typ -> typ
(** [pair_of a b] creates [(Pair a b)]. *)

val hash_table_of : typ -> typ -> typ
(** [hash_table_of k v] creates [(HashTable k v)]. *)

val arrow : typ list -> typ -> typ
(** [arrow params ret] creates a function type with positional parameters. *)

val forall : string list -> typ -> typ
(** [forall vars ty] creates a polymorphic type. *)

(** {1 Truthiness} *)

(** Errors that can occur during type construction/validation. *)
type validation_error =
  | NonTruthyOptionArg of typ
      (** Option argument must be truthy - contains the offending type. *)

val is_truthy : typ -> bool
(** Check if a type is definitely truthy (cannot be nil).

    A type is truthy if it's a concrete non-nil primitive, a type application
    other than Option, a function, tuple, or forall with truthy body.

    A type is NOT truthy if it's Nil, Any, Prim.bool (T | Nil), a union
    containing Nil, an unresolved type variable, or an Option type. *)

val validate_option_arg : typ -> (unit, validation_error) result
(** Validate that a type is suitable as an Option argument. Returns [Ok ()] if
    truthy, or [Error] with the problematic type. *)

val option_of_checked : typ -> (typ, validation_error) result
(** Create an Option type with validation. Returns [Error] if the argument type
    is not truthy. *)

val validation_error_to_string : validation_error -> string
(** Format a validation error as a string. *)
