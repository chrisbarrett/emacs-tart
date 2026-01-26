(** Constraint representation for type inference.

    This module defines equality constraints generated during type inference.
    Constraints are collected and then solved by unification. *)

open Core.Types

(** Context about where a constraint originated.

    Used to provide better error messages with function names and signatures. *)
type context =
  | NoContext
  | FunctionArg of {
      fn_name : string;  (** Name of the function being called *)
      fn_type : typ;  (** Full function type *)
      arg_index : int;  (** Which argument (0-indexed) *)
      arg_expr_source : string option;
          (** Name of function/variable that produced the argument value *)
    }
  | IfBranch of {
      is_then : bool;  (** True for then branch, false for else branch *)
      other_branch_span : Syntax.Location.span;  (** Span of the other branch *)
      other_branch_type : typ;  (** Type of the other branch *)
    }
  | TartAnnotation of { declared_type : typ }
      (** Type annotation via (tart TYPE FORM) *)
  | DeclaredReturn of {
      fn_name : string;  (** Name of the function *)
      declared_type : typ;  (** Declared return type *)
    }  (** Function body vs declared return type *)
  | ExplicitInstantiation of {
      type_args : typ list;  (** Explicit type arguments from @type *)
      arg_index : int;
          (** Which type argument caused the mismatch (0-indexed) *)
    }  (** Explicit type instantiation via (@type [T1 T2] fn args) *)
  | TypeArgArity of {
      fn_name : string;  (** Name of the polymorphic function *)
      expected : int;  (** Number of type parameters *)
      actual : int;  (** Number of type arguments provided *)
    }  (** Wrong number of type arguments in @type *)

type t = {
  lhs : typ;  (** Left-hand side type *)
  rhs : typ;  (** Right-hand side type *)
  loc : Syntax.Location.span;  (** Source location for error reporting *)
  context : context;  (** Optional context for better errors *)
}
(** An equality constraint: two types that must unify. *)

type set = t list
(** A set of constraints to be solved *)

(** Create an equality constraint *)
let equal ?(context = NoContext) lhs rhs loc = { lhs; rhs; loc; context }

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
