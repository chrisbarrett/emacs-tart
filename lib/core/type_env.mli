(** Type environment for tracking variable bindings during type inference.

    The type environment maps variable names to their type schemes. It also
    tracks the current level for let-generalization. *)

(** {1 Type Schemes} *)

(** A type scheme is a possibly-polymorphic type.

    - [Mono ty] is a monomorphic type (no quantified variables).
    - [Poly (vars, ty)] is a polymorphic type with bound type variables.

    Type schemes are created during let-generalization when the RHS is a
    syntactic value (lambda, literal, variable, constructor application). *)
type scheme = Mono of Types.typ | Poly of string list * Types.typ

(** {1 Environment} *)

type t = {
  bindings : (string * scheme) list;
  level : int;  (** Current scope level for generalization *)
}
(** Type environment: maps names to type schemes. *)

(** {1 Creation} *)

val empty : t
(** Empty environment at level 0. *)

val of_list : (string * scheme) list -> t
(** Create an environment with initial bindings. *)

(** {1 Level management} *)

val current_level : t -> int
(** Get the current level. *)

val enter_level : t -> t
(** Enter a new scope (increment level). *)

val exit_level : t -> t
(** Exit a scope (decrement level). *)

(** {1 Lookup} *)

val lookup : string -> t -> scheme option
(** Look up a name in the environment. *)

(** {1 Extension} *)

val extend : string -> scheme -> t -> t
(** Extend the environment with a new binding. *)

val extend_mono : string -> Types.typ -> t -> t
(** Extend with a monomorphic binding. *)

val extend_monos : (string * Types.typ) list -> t -> t
(** Extend with multiple monomorphic bindings. *)

val extend_poly : string -> string list -> Types.typ -> t -> t
(** Extend with a polymorphic binding. *)

(** {1 Instantiation} *)

val instantiate : scheme -> t -> Types.typ
(** [instantiate scheme env] instantiates a type scheme at the current level.

    For monomorphic types, returns the type as-is. For polymorphic types,
    replaces bound variables with fresh type variables. *)

(** {1 Pretty-printing} *)

val scheme_to_string : scheme -> string
(** Convert a scheme to string for debugging. *)

val to_string : t -> string
(** Pretty-print the environment for debugging. *)
