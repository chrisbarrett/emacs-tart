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
  | Predicates of predicate_info list  (** Multiple predicates from [and] *)
  | NoPredicate  (** No narrowing applicable *)

val narrow_type : typ -> typ -> typ
(** [narrow_type original target] intersects [original] with [target].

    For unions, filters members to those that overlap with [target]:
    - [(string | int | nil) ∩ string] → [string]
    - [(string | nil) ∩ truthy] → [string]
    - [any ∩ T] → [T]

    For non-union types, returns [original] when it overlaps with [target]. *)

val analyze_condition : Syntax.Sexp.t -> Core.Type_env.t -> condition_analysis
(** [analyze_condition condition env] examines a condition for predicate calls.

    Detects the pattern [(predicate_fn arg)] where [predicate_fn] has a
    registered predicate in [env] and [arg] is a plain symbol (variable
    reference). Returns [Predicate { var_name; narrowed_type }] when a predicate
    call is found, or [NoPredicate] otherwise.

    Also handles [(and pred1 pred2 ...)] conditions by returning [Predicates]
    with all detected predicate calls (Spec 52 R4).

    Per R12 (inline-only restriction), only direct calls are recognized. Stored
    results do not enable narrowing. *)
