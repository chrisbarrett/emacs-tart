(** Shared types for the inference engine. *)

module C = Constraint
module Loc = Syntax.Location

type undefined_var = { name : string; span : Loc.span }
(** An undefined variable reference *)

type resolved_clause_diagnostic = Clause_dispatch.resolved_diagnostic = {
  rcd_severity : Core.Type_env.diagnostic_severity;
  rcd_message : string;
  rcd_span : Loc.span;
}
(** A clause diagnostic resolved at a call site.

    Re-exported from {!Clause_dispatch} for backward compatibility. *)

type result = {
  ty : Core.Types.typ;
  constraints : C.set;
  undefineds : undefined_var list;
  clause_diagnostics : resolved_clause_diagnostic list;
}
(** Result of inference: the inferred type, constraints, undefined vars, and any
    clause diagnostics emitted during multi-clause dispatch. *)

type defun_result = {
  name : string;
  fn_type : Core.Types.typ;
  defun_constraints : C.set;
  defun_undefineds : undefined_var list;
  defun_clause_diagnostics : resolved_clause_diagnostic list;
}
(** Result of inferring a top-level definition *)

(** Create a result with no constraints and no undefined vars *)
let pure ty =
  { ty; constraints = C.empty; undefineds = []; clause_diagnostics = [] }

(** Create a result with a single constraint *)
let with_constraint ty c =
  {
    ty;
    constraints = C.add c C.empty;
    undefineds = [];
    clause_diagnostics = [];
  }

(** Create a result with an undefined variable error *)
let with_undefined ty name span =
  {
    ty;
    constraints = C.empty;
    undefineds = [ { name; span } ];
    clause_diagnostics = [];
  }

(** Combine results, merging constraints *)
let combine_results results =
  List.fold_left (fun acc r -> C.combine acc r.constraints) C.empty results

(** Combine undefined variables from results *)
let combine_undefineds results = List.concat_map (fun r -> r.undefineds) results

(** Combine clause diagnostics from results *)
let combine_clause_diagnostics results =
  List.concat_map (fun r -> r.clause_diagnostics) results
