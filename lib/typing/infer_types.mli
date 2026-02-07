(** Shared types for the inference engine.

    These types are used by {!Infer} and extracted inference modules like
    {!Pcase_infer} to avoid circular dependencies. *)

type undefined_var = { name : string; span : Syntax.Location.span }
(** An undefined variable reference. *)

type resolved_clause_diagnostic = Clause_dispatch.resolved_diagnostic = {
  rcd_severity : Core.Type_env.diagnostic_severity;
  rcd_message : string;
  rcd_span : Syntax.Location.span;
}
(** A clause diagnostic resolved at a call site.

    Re-exported from {!Clause_dispatch} for backward compatibility. *)

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

(** {1 Constructors} *)

val pure : Core.Types.typ -> result
(** Create a result with no constraints and no undefined vars. *)

val with_constraint : Core.Types.typ -> Constraint.t -> result
(** Create a result with a single constraint. *)

val with_undefined : Core.Types.typ -> string -> Syntax.Location.span -> result
(** Create a result with an undefined variable error. *)

val combine_results : result list -> Constraint.set
(** Combine results, merging constraints. *)

val combine_undefineds : result list -> undefined_var list
(** Combine undefined variables from results. *)

val combine_clause_diagnostics : result list -> resolved_clause_diagnostic list
(** Combine clause diagnostics from results. *)
