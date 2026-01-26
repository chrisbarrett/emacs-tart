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

type result = { ty : Core.Types.typ; constraints : Constraint.set }
(** Result of inference: the inferred type and generated constraints. *)

type defun_result = {
  name : string;
  fn_type : Core.Types.typ;
  defun_constraints : Constraint.set;
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
(** Create a result with no constraints. *)

val with_constraint : Core.Types.typ -> Constraint.t -> result
(** Create a result with a single constraint. *)

val combine_results : result list -> Constraint.set
(** Combine results, merging constraints. *)
