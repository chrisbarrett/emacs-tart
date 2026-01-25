(** Constraint representation for type inference.

    This module defines equality constraints generated during type inference.
    Constraints are collected and then solved by unification. *)

(** {1 Constraints} *)

(** An equality constraint: two types that must unify. *)
type t = {
  lhs : Core.Types.typ;  (** Left-hand side type *)
  rhs : Core.Types.typ;  (** Right-hand side type *)
  loc : Syntax.Location.span;  (** Source location for error reporting *)
}

(** A set of constraints to be solved. *)
type set = t list

(** {1 Construction} *)

val equal : Core.Types.typ -> Core.Types.typ -> Syntax.Location.span -> t
(** [equal lhs rhs loc] creates an equality constraint. *)

val empty : set
(** Empty constraint set. *)

val add : t -> set -> set
(** Add a constraint to a set. *)

val combine : set -> set -> set
(** Combine two constraint sets. *)

(** {1 Pretty-printing} *)

val to_string : t -> string
(** Pretty-print a constraint for debugging. *)

val set_to_string : set -> string
(** Pretty-print a constraint set. *)
