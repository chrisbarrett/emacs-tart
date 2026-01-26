(** Constraint representation for type inference.

    This module defines equality constraints generated during type inference.
    Constraints are collected and then solved by unification. *)

open Core.Types

type t = {
  lhs : typ;  (** Left-hand side type *)
  rhs : typ;  (** Right-hand side type *)
  loc : Syntax.Location.span;  (** Source location for error reporting *)
}
(** An equality constraint: two types that must unify. *)

type set = t list
(** A set of constraints to be solved *)

(** Create an equality constraint *)
let equal lhs rhs loc = { lhs; rhs; loc }

(** Empty constraint set *)
let empty : set = []

(** Add a constraint to a set *)
let add c cs = c :: cs

(** Combine two constraint sets *)
let combine cs1 cs2 = cs1 @ cs2

(** Pretty-print a constraint for debugging *)
let to_string c = Printf.sprintf "%s = %s" (to_string c.lhs) (to_string c.rhs)

(** Pretty-print a constraint set *)
let set_to_string cs = String.concat "\n" (List.map to_string cs)
