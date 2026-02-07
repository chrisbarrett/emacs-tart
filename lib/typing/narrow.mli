(** Type narrowing for predicate-based occurrence typing and feature guards.

    This module implements type narrowing as specified in Spec 52 (Type
    Predicates) and feature guard recognition as specified in Spec 49 (Feature
    Guards).

    Predicate narrowing: when a type predicate like [stringp] is called in a
    condition, the variable's type is narrowed in the then-branch and subtracted
    in the else-branch.

    Feature guards: when a guard like [(featurep 'json)] is detected in a
    condition, the then-branch gets names from the corresponding module loaded
    into the environment. *)

open Core.Types

type predicate_info = {
  var_name : string;  (** Name of the variable being tested *)
  narrowed_type : typ;  (** Type to narrow to when predicate is true *)
}
(** Predicate info extracted from a condition *)

(** What kind of feature guard was detected in a condition *)
type guard_info =
  | FeatureGuard of string
      (** [(featurep 'X)] — load all names from [X.tart] *)
  | FboundGuard of string  (** [(fboundp 'f)] — make function [f] available *)
  | BoundGuard of string  (** [(boundp 'v)] — make variable [v] available *)
  | BoundTrueGuard of string
      (** [(bound-and-true-p v)] — variable [v] bound and non-nil *)

(** Result of analyzing a condition for narrowing *)
type condition_analysis =
  | Predicate of predicate_info  (** A predicate call on a variable *)
  | Predicates of predicate_info list  (** Multiple predicates from [and] *)
  | Guard of guard_info  (** A single feature guard (Spec 49) *)
  | Guards of guard_info list  (** Multiple guards from [and] (Spec 49 R16) *)
  | PredicatesAndGuards of predicate_info list * guard_info list
      (** Mixed predicates and guards from [and] *)
  | NoPredicate  (** No narrowing applicable *)

val narrow_type : typ -> typ -> typ
(** [narrow_type original target] intersects [original] with [target].

    For unions, filters members to those that overlap with [target]:
    - [(string | int | nil) ∩ string] → [string]
    - [(string | int | (list any)) ∩ ((list any) | (vector any) | string)] →
      [(string | (list any))]
    - [(string | nil) ∩ truthy] → [string]
    - [any ∩ T] → [T]

    For non-union types, returns [original] when it overlaps with [target], or
    [TUnion []] (empty) when disjoint. *)

val detect_hard_require : Syntax.Sexp.t -> string option
(** [detect_hard_require sexp] returns [Some name] when [sexp] is a hard require
    form like [(require 'X)] that unconditionally loads a feature.

    Returns [None] for soft requires [(require 'X nil t)] or non-require forms.
    Used by [infer_progn] to thread the extended environment to subsequent forms
    (Spec 49 R5). *)

val analyze_condition : Syntax.Sexp.t -> Core.Type_env.t -> condition_analysis
(** [analyze_condition condition env] examines a condition for predicate calls
    and feature guards.

    Detects predicate patterns [(predicate_fn arg)] where [predicate_fn] has a
    registered predicate in [env] and [arg] is a plain symbol. Also detects
    guard patterns: [(featurep 'X)], [(fboundp 'f)], [(boundp 'v)],
    [(bound-and-true-p v)].

    Handles [(and ...)] conditions containing any mix of predicates and guards
    (Spec 52 R4, Spec 49 R16).

    Per inline-only restriction, only direct calls are recognized. Stored
    results do not enable narrowing (Spec 52 R12, Spec 49 R17). *)
