(** Constraint representation for type inference.

    This module defines equality constraints generated during type inference.
    Constraints are collected and then solved by unification. *)

(** {1 Types} *)

(** Context about where a constraint originated.

    Used to provide better error messages with function names and signatures. *)
type context =
  | NoContext
  | FunctionArg of {
      fn_name : string;  (** Name of the function being called *)
      fn_type : Core.Types.typ;  (** Full function type *)
      arg_index : int;  (** Which argument (0-indexed) *)
    }
  | IfBranch of {
      is_then : bool;  (** True for then branch, false for else branch *)
      other_branch_span : Syntax.Location.span;  (** Span of the other branch *)
      other_branch_type : Core.Types.typ;  (** Type of the other branch *)
    }

type t = {
  lhs : Core.Types.typ;  (** Left-hand side type *)
  rhs : Core.Types.typ;  (** Right-hand side type *)
  loc : Syntax.Location.span;  (** Source location for error reporting *)
  context : context;  (** Optional context for better errors *)
}
(** An equality constraint: two types that must unify. *)

type set = t list
(** A set of constraints to be solved. *)

(** {1 Construction} *)

val equal :
  ?context:context ->
  Core.Types.typ ->
  Core.Types.typ ->
  Syntax.Location.span ->
  t
(** [equal ?context lhs rhs loc] creates an equality constraint with optional
    context for better error messages. *)

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
