(** Constraint-based type inference.

    This module generates type constraints for Elisp expressions. Constraints
    are equality constraints [τ₁ = τ₂] that are later solved by unification.

    Expressions handled:
    - Literals: produce base types (no constraints)
    - Variables: instantiate from environment (no constraints)
    - Lambda: introduce fresh type vars for params
    - Application: generate [τ_fun = (τ_args...) -> τ_result]
    - Quote: quoted expressions have type based on their structure
    - If: branches must unify; result is their common type
    - Let: generate constraints for bindings with generalization
    - Progn: result is type of last expression *)

(** {1 Types} *)

type undefined_var = { name : string; span : Syntax.Location.span }
(** An undefined variable reference. *)

type resolved_clause_diagnostic = {
  rcd_severity : Core.Type_env.diagnostic_severity;
  rcd_message : string;  (** Fully resolved message (no remaining %s) *)
  rcd_span : Syntax.Location.span;  (** Call-site span *)
}
(** A clause diagnostic resolved at a call site.

    When multi-clause dispatch selects a clause with a diagnostic annotation,
    the format string's [%s] placeholders are resolved against the actual types
    inferred at the call site. *)

type result = {
  ty : Core.Types.typ;
  constraints : Constraint.set;
  undefineds : undefined_var list;
  clause_diagnostics : resolved_clause_diagnostic list;
}
(** Result of inference: the inferred type, constraints, undefined vars, and any
    clause diagnostics emitted during multi-clause dispatch. *)

type defun_result = {
  name : string;
  fn_type : Core.Types.typ;
  defun_constraints : Constraint.set;
  defun_undefineds : undefined_var list;
  defun_clause_diagnostics : resolved_clause_diagnostic list;
}
(** Result of inferring a top-level definition. *)

(** {1 Inference} *)

val infer : Core.Type_env.t -> Syntax.Sexp.t -> result
(** [infer env sexp] infers the type of an S-expression.

    This generates constraints but does not solve them. Call {!Unify.solve} on
    the constraints to unify type variables. *)

val infer_defun : Core.Type_env.t -> Syntax.Sexp.t -> defun_result option
(** [infer_defun env sexp] infers the type of a defun and returns the binding
    info.

    Returns [Some] with the function name and type if [sexp] is a defun form, or
    [None] otherwise. The returned type is already generalized. *)

(** {1 Helpers} *)

val pure : Core.Types.typ -> result
(** Create a result with no constraints and no undefined vars. *)

val with_constraint : Core.Types.typ -> Constraint.t -> result
(** Create a result with a single constraint. *)

val combine_results : result list -> Constraint.set
(** Combine results, merging constraints. *)

val combine_undefineds : result list -> undefined_var list
(** Combine undefined variables from results. *)
