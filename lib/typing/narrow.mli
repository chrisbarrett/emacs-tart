(** Type narrowing for predicate-based occurrence typing.

    This module implements type narrowing as specified in Spec 52 (Type
    Predicates). When a type predicate like [stringp] is called in a condition,
    the variable's type is narrowed in the then-branch and subtracted in the
    else-branch. *)

open Core.Types

type predicate_info = {
  var_name : string;  (** Name of the variable being tested *)
  narrowed_type : typ;  (** Type to narrow to when predicate is true *)
}
(** Predicate info extracted from a condition *)

(** Result of analyzing a condition for narrowing *)
type condition_analysis =
  | Predicate of predicate_info  (** A predicate call on a variable *)
  | NoPredicate  (** No narrowing applicable *)

val narrow_type : typ -> typ -> typ
(** [narrow_type original target] intersects [original] with [target]. Returns
    the narrowed type. *)

val subtract_type : typ -> typ -> typ
(** [subtract_type original subtracted] returns [original - subtracted]. For
    unions, removes matching members. For non-union types, returns [TUnion []]
    (empty/never type) when equal. Delegates to {!Core.Types.subtract_type}. *)
