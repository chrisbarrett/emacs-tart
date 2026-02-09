(** Unification with union-find for type inference.

    This module solves equality constraints by unifying types using the
    union-find data structure. Type variables are mutable refs that can either
    be Unbound or Link to another type.

    The occurs check prevents infinite types like [a = List a]. *)

(** {1 Unification Context} *)

(** Controls context-sensitive unification rules.

    During normal constraint solving, subsumption rules (e.g. plist↔list
    widening) are applied freely. During clause matching, these
    cross-constructor rules are suppressed so that clause dispatch treats
    structurally distinct types (e.g. [plist] vs [list]) as non-matching. *)
type unify_context =
  | Constraint_solving  (** Normal constraint solving (default). *)
  | Clause_matching
      (** Clause dispatch: suppress cross-constructor subsumption. *)

(** {1 Errors} *)

(** Unification errors. *)
type error =
  | TypeMismatch of
      Core.Types.typ
      * Core.Types.typ
      * Syntax.Location.span
      * Constraint.context
      (** Two concrete types that cannot unify, with optional context. *)
  | OccursCheck of Core.Types.tvar_id * Core.Types.typ * Syntax.Location.span
      (** Type variable occurs in the type it's being unified with. *)
  | ArityMismatch of int * int * Syntax.Location.span * Constraint.context
      (** Function arities don't match: expected, actual, with optional context.
      *)

val error_to_string : error -> string
(** Format an error for display. *)

val error_location : error -> Syntax.Location.span
(** Get the source location from an error. *)

val error_context : error -> Constraint.context
(** Get the context from an error. *)

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

(** {1 Speculative Unification} *)

val try_unify :
  Core.Types.typ -> Core.Types.typ -> Syntax.Location.span -> unit result
(** [try_unify t1 t2 loc] attempts unification speculatively. If unification
    succeeds, the side-effects are committed (tvars remain linked). If it fails,
    all tvar mutations are rolled back and the types remain unchanged.

    Used by clause dispatch to try each clause without permanently modifying
    type state on failure. *)

val try_unify_params :
  ?context:unify_context ->
  Core.Types.param list ->
  Core.Types.param list ->
  Syntax.Location.span ->
  unit result
(** [try_unify_params ~context ps1 ps2 loc] attempts param list unification
    speculatively with rollback on failure.

    When [context] is [Clause_matching], cross-constructor subsumption rules
    (e.g. plist↔list widening) are suppressed so that clause dispatch treats
    structurally distinct types as non-matching. Defaults to
    [Constraint_solving]. *)

val try_unify_all_to_element :
  Core.Types.typ list -> Core.Types.typ -> Syntax.Location.span -> bool
(** [try_unify_all_to_element types elem_var loc] attempts to unify all [types]
    with [elem_var] speculatively. Snapshots all tvars before starting, then
    tries each unification sequentially. Returns [true] if all succeed
    (mutations committed) or [false] if any fails (all mutations rolled back).

    Used by the [list] intrinsic (Spec 84) to test whether argument types are
    homogeneous without leaking partial unification on failure. *)

(** {1 Disjointness} *)

val types_disjoint : Core.Types.typ -> Core.Types.typ -> bool
(** [types_disjoint t1 t2] returns [true] when [t1] and [t2] are provably
    disjoint (their intersection is empty).

    Respects numeric subtyping: [int] and [num] are NOT disjoint. Type variables
    are conservatively non-disjoint.

    Used by eq/eql disjointness checking (Spec 11 R14). *)
