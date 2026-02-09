(** Type representation for the Tart type system.

    This module defines the core type representation used for type inference and
    checking. It uses mutable type variables with union-find for efficient
    unification.

    Based on levels-based Hindley-Milner inference with union-find for
    near-linear generalization performance. *)

(** {1 Type Variables} *)

type tvar_id = int
(** Unique identifier for type variables. *)

(** {1 Literal Values} *)

(** Literal value representation for literal types.

    Literal types carry their precise value and widen to their base type when
    context demands it during unification. *)
type literal_value =
  | LitInt of int
  | LitFloat of float
  | LitString of string
  | LitSymbol of string  (** quoted symbol *)
  | LitKeyword of string  (** :keyword *)

(** {1 Type Variables and Types} *)

(** Type variable state - uses union-find representation.

    - [Unbound (id, level)] is an unresolved type variable. The level tracks the
      scope depth for let-generalization.
    - [Link ty] indicates this variable has been unified with [ty]. *)
type tvar = Unbound of tvar_id * int  (** id, level *) | Link of typ

(** Type representation.

    - [TVar] - Mutable type variable for union-find unification
    - [TCon] - Type constant (Int, String, Nil, T, etc.)
    - [TApp] - Type application (List a, Option a, etc.). The first element is
      the type constructor (TCon for concrete types, TVar for higher-kinded)
    - [TArrow] - Function type with grouped parameters
    - [TForall] - Universally quantified type
    - [TUnion] - Union types (Or a b)
    - [TTuple] - Fixed-length heterogeneous tuples
    - [TRow] - Row type for record-style map types (alist, plist, hash-table)
    - [TLiteral] - Literal type carrying a precise value and its base type *)
and typ =
  | TVar of tvar ref
  | TCon of string
  | TApp of typ * typ list
  | TArrow of param list * typ
  | TForall of string list * typ
  | TUnion of typ list
  | TTuple of typ list
  | TRow of row
  | TLiteral of literal_value * typ

and row = {
  row_fields : (string * typ) list;  (** Named fields in declaration order *)
  row_var : typ option;  (** Optional row variable (TVar) for open rows *)
}
(** Row type representation for record-style typing of
    alists/plists/hash-tables.

    A row is a collection of field name-type pairs, optionally with a row
    variable for polymorphism (open rows).

    - [{name string age int}] - closed row, exactly these fields
    - [{name string & r}] - open row, at least name field, r captures the rest
*)

(** Function parameter kinds.

    Elisp functions can have positional, optional, rest, and keyword arguments.
*)
and param =
  | PPositional of typ
  | POptional of typ  (** Type should be (Option a) *)
  | PRest of typ  (** Type is the element type; expands to (List a) *)
  | PKey of string * typ  (** :keyword name and type *)
  | PLiteral of string
      (** Literal value parameter for clause matching. Keywords [:name] become
          [PLiteral ":name"], quoted symbols ['foo] become [PLiteral "foo"].
          Matches only when the call-site argument is the same literal. *)

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

(** {1 Type Variable Bounds} *)

val set_tvar_bound : tvar_id -> typ -> unit
(** [set_tvar_bound id bound] records an upper bound on the type variable with
    the given ID. Used for bounded quantification: when a tvar is unified with a
    union from a rest parameter, the union becomes an upper bound rather than an
    equality link. *)

val get_tvar_bound : tvar_id -> typ option
(** [get_tvar_bound id] returns the upper bound on the type variable, if any. *)

val remove_tvar_bound : tvar_id -> unit
(** [remove_tvar_bound id] removes the upper bound from the type variable, if
    any. Used when a bound check succeeds and the bound is no longer needed. *)

val clear_tvar_bounds : unit -> unit
(** [clear_tvar_bounds ()] removes all tvar bounds. Called by
    [reset_tvar_counter]. *)

(** {1 Type variable operations} *)

val reset_tvar_counter : unit -> unit
(** Reset the type variable counter and clear all tvar bounds (for testing). *)

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

(** {1 Intrinsic Types} *)

val intrinsic_prefix : string
(** The prefix used for built-in intrinsic type names ("%tart-intrinsic%"). *)

val intrinsic : string -> string
(** [intrinsic name] creates an intrinsic type name with the prefix. *)

val is_intrinsic_name : string -> bool
(** [is_intrinsic_name name] returns true if [name] is an intrinsic type name.
*)

val intrinsic_base_name : string -> string
(** [intrinsic_base_name name] extracts the base name from an intrinsic (e.g.,
    "%tart-intrinsic%Int" -> "Int"). Returns [name] unchanged if not an
    intrinsic. *)

(** {1 Primitive types} *)

(** Primitive type constants using intrinsic names. *)
module Prim : sig
  val int_name : string
  (** Intrinsic type names for comparison in unification *)

  val float_name : string
  val num_name : string
  val never_name : string

  val int : typ
  (** Type constructors *)

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
(** [option_of elem] creates [(elem | Nil)]. Note: Does not validate the truthy
    constraint. *)

val pair_of : typ -> typ -> typ
(** [pair_of a b] creates [(Pair a b)]. *)

val hash_table_of : typ -> typ -> typ
(** [hash_table_of k v] creates [(HashTable k v)]. *)

val plist_of : typ -> typ -> typ
(** [plist_of k v] creates [(Plist k v)]. *)

val map_of : typ -> typ
(** [map_of row] creates [(Map row)]. Used as a supertype of alist, plist, and
    hash-table. *)

val is_any : typ -> bool
(** Check if a type is the "any" type (truthy | nil). Used in unification to
    maintain top-type semantics. *)

val is_option : typ -> typ option
(** Check if a type is an option type (a | nil) and return the inner type.
    Returns [Some inner] if the type is [(inner | nil)], [None] otherwise. Used
    in diagnostics to detect nullable types. *)

val is_union : typ -> bool
(** Check if a type is a union type. *)

val arrow : typ list -> typ -> typ
(** [arrow params ret] creates a function type with positional parameters. *)

val forall : string list -> typ -> typ
(** [forall vars ty] creates a polymorphic type. *)

(** {1 Row Types} *)

val closed_row : (string * typ) list -> typ
(** [closed_row fields] creates a closed row type with the given fields.
    Example: [closed_row [("name", string); ("age", int)]] creates
    [{name string age int}]. *)

val open_row : (string * typ) list -> typ -> typ
(** [open_row fields var] creates an open row type with a row variable. Example:
    [open_row [("name", string)] (fresh_tvar 0)] creates [{name string & r}]. *)

val row_lookup : row -> string -> typ option
(** [row_lookup row field] looks up a field in a row, returning its type if
    present. *)

val row_has_field : row -> string -> bool
(** [row_has_field row field] returns true if the row has the given field. *)

val is_row : typ -> bool
(** Check if a type is a row type. *)

val is_open_row : typ -> bool
(** Check if a type is an open row (has a row variable). *)

(** {1 Truthiness} *)

(** Errors that can occur during type construction/validation. *)
type validation_error =
  | NonTruthyOptionArg of typ
      (** Option argument must be truthy - contains the offending type. *)

val is_truthy : typ -> bool
(** Check if a type is definitely truthy (cannot be nil).

    A type is truthy if it's a concrete non-nil primitive, a type application
    other than Option, a function, tuple, or forall with truthy body.

    A type is NOT truthy if it's Nil, a union containing Nil (including
    Prim.any, Prim.bool, option_of), or an unresolved type variable. *)

val validate_option_arg : typ -> (unit, validation_error) result
(** Validate that a type is suitable as an Option argument. Returns [Ok ()] if
    truthy, or [Error] with the problematic type. *)

val option_of_checked : typ -> (typ, validation_error) result
(** Create an Option type with validation. Returns [Error] if the argument type
    is not truthy. *)

val validation_error_to_string : validation_error -> string
(** Format a validation error as a string. *)

(** {1 Type Subtraction} *)

val literal_value_equal : literal_value -> literal_value -> bool
(** Structural equality for literal values. *)

val literal_base_type : literal_value -> typ
(** [literal_base_type v] returns the base Prim type for a literal value. E.g.,
    [LitInt _] → [Prim.int], [LitString _] → [Prim.string]. *)

val is_never : typ -> bool
(** [is_never ty] returns true if [ty] is the [never] bottom type (after
    following links). *)

val normalize_union : typ list -> typ
(** [normalize_union members] filters [never] members from a union and collapses
    the result: empty → [Prim.never], singleton → the single type, otherwise
    [TUnion filtered]. *)

val subtract_type : typ -> typ -> typ
(** [subtract_type minuend subtrahend] returns [minuend - subtrahend].

    For unions, removes members that appear in the subtrahend:
    - [(int | string) - int => string]
    - [(string | int | (list any)) - (string | (list any) | (vector any)) =>
       int]

    When the subtrahend is itself a union, each member of the minuend is removed
    if it equals any member of the subtrahend. For non-union minuends, returns
    [Prim.never] when matched, or [minuend] unchanged.

    Uses structural equality after following type variable links. *)
