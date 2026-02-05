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
      arg_expr_source : string option;
          (** Name of function/variable that produced the argument value *)
    }
  | IfBranch of {
      is_then : bool;  (** True for then branch, false for else branch *)
      other_branch_span : Syntax.Location.span;  (** Span of the other branch *)
      other_branch_type : Core.Types.typ;  (** Type of the other branch *)
    }
  | TartAnnotation of { declared_type : Core.Types.typ }
      (** Type annotation via (tart TYPE FORM) *)
  | DeclaredReturn of {
      fn_name : string;  (** Name of the function *)
      declared_type : Core.Types.typ;  (** Declared return type *)
    }  (** Function body vs declared return type *)
  | ExplicitInstantiation of {
      type_args : Core.Types.typ list;
          (** Explicit type arguments from @type *)
      arg_index : int;
          (** Which type argument caused the mismatch (0-indexed) *)
    }  (** Explicit type instantiation via (@type [T1 T2] fn args) *)
  | TypeArgArity of {
      fn_name : string;  (** Name of the polymorphic function *)
      expected : int;  (** Number of type parameters *)
      actual : int;  (** Number of type arguments provided *)
    }  (** Wrong number of type arguments in @type *)
  | EqDisjointness of {
      fn_name : string;  (** "eq" or "eql" *)
      arg1_type : Core.Types.typ;  (** Type of first argument *)
      arg2_type : Core.Types.typ;  (** Type of second argument *)
    }  (** eq/eql called with provably disjoint types (Spec 11 R14) *)

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
