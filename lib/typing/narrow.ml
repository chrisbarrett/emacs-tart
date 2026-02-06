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
module Env = Core.Type_env

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

    Computes the intersection of [original] and [target]:
    - [any ∩ T] → [T]
    - [T ∩ T] → [T]
    - [(A | B | C) ∩ T] → members of the union that overlap with [T]
    - [T ∩ truthy] → [T] when [T] is truthy (filters out nil)
    - Fallback → [target] (conservative but sound) *)
let narrow_type (original : typ) (target : typ) : typ =
  let original = repr original in
  let target = repr target in
  (* any ∩ T = T *)
  if is_any original then target (* T ∩ T = T *)
  else if equal original target then original
  else
    match original with
    | TUnion members -> (
        (* Filter union members to those that overlap with target *)
        let overlapping =
          List.filter (fun m -> not (Unify.types_disjoint m target)) members
        in
        match overlapping with
        | [] -> TUnion []
        | [ single ] -> single
        | _ -> TUnion overlapping)
    | _ ->
        (* Non-union original: if not disjoint with target, keep original;
           otherwise return target as conservative fallback *)
        if not (Unify.types_disjoint original target) then original else target

(** Analyze a condition expression for predicate calls.

    Detects the pattern [(predicate_fn arg)] where [predicate_fn] is registered
    in the type environment's predicates and [arg] is a plain symbol (variable
    reference). Returns the variable name and narrowed type if found.

    Per R12, only inline calls are recognized. Stored results like
    [(let ((result (stringp x))) (when result ...))] do not narrow. *)
let analyze_condition (condition : Syntax.Sexp.t) (env : Env.t) :
    condition_analysis =
  match condition with
  | Syntax.Sexp.List (Syntax.Sexp.Symbol (fn_name, _) :: args, _) -> (
      match Env.lookup_predicate fn_name env with
      | Some pred_info -> (
          (* Get the argument at the predicate's param_index *)
          let arg = List.nth_opt args pred_info.param_index in
          match arg with
          | Some (Syntax.Sexp.Symbol (var_name, _)) ->
              Predicate { var_name; narrowed_type = pred_info.narrowed_type }
          | _ -> NoPredicate)
      | None -> NoPredicate)
  | _ -> NoPredicate
