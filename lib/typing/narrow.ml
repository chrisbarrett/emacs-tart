(** Type narrowing for predicate-based occurrence typing.

    This module implements type narrowing as specified in Spec 52 (Type
    Predicates). When a type predicate like [stringp] is called in a condition,
    the variable's type is narrowed in the then-branch and subtracted in the
    else-branch.

    Example:
    {[
      (if (stringp x)
          (upcase x)    ; x : string
        (other x))      ; x : (any - string)
    ]}

    Narrowing is inline-only: storing a predicate result in a variable does not
    enable narrowing (R12). *)

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

(** Narrow a type by intersecting with a target type.

    For now, we use a simple approach: if the target is more specific than the
    original, return the target; otherwise return the original.

    Future: Implement proper type intersection. *)
let narrow_type (original : typ) (target : typ) : typ =
  (* Simple approach: just use the target type.
     This works when the predicate's narrowed type is a subtype of the original.
     A proper implementation would compute the intersection. *)
  ignore original;
  target

(** Subtract a type from another type. Delegates to [Core.Types.subtract_type].
*)
let subtract_type = Core.Types.subtract_type
