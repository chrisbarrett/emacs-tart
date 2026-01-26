(** Unification with union-find for type inference.

    This module solves equality constraints by unifying types using the
    union-find data structure. Type variables are mutable refs that can either
    be Unbound or Link to another type.

    The occurs check prevents infinite types like [a = List a]. *)

(** {1 Errors} *)

(** Unification errors. *)
type error =
  | TypeMismatch of Core.Types.typ * Core.Types.typ * Syntax.Location.span
      (** Two concrete types that cannot unify. *)
  | OccursCheck of Core.Types.tvar_id * Core.Types.typ * Syntax.Location.span
      (** Type variable occurs in the type it's being unified with. *)
  | ArityMismatch of int * int * Syntax.Location.span
      (** Function arities don't match: expected, actual. *)

val error_to_string : error -> string
(** Format an error for display. *)

val error_location : error -> Syntax.Location.span
(** Get the source location from an error. *)

(** {1 Unification} *)

type 'a result = ('a, error) Result.t
(** Result type for unification operations. *)

val unify :
  Core.Types.typ -> Core.Types.typ -> Syntax.Location.span -> unit result
(** [unify t1 t2 loc] unifies two types.

    This is the core unification algorithm. It follows links and handles each
    type constructor case. Type variables are linked to the other type when
    unified.

    The occurs check prevents infinite types. *)

(** {1 Constraint solving} *)

val solve : Constraint.set -> unit result
(** [solve constraints] solves a set of constraints. Returns [Ok ()] if all
    constraints can be satisfied, or the first error encountered. *)

val solve_all : Constraint.set -> error list
(** [solve_all constraints] solves constraints and returns all errors. Useful
    for reporting multiple type errors at once. *)
