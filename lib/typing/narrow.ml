(** Type narrowing for predicate-based occurrence typing and feature guards.

    This module implements type narrowing as specified in Spec 52 (Type
    Predicates) and feature guard recognition as specified in Spec 49 (Feature
    Guards).

    Predicate narrowing: when a type predicate like [stringp] is called in a
    condition, the variable's type is narrowed in the then-branch and subtracted
    in the else-branch.

    Feature guards: when a guard like [(featurep 'json)] is detected in a
    condition, the then-branch gets names from the corresponding module loaded
    into the environment.

    Both are inline-only: storing a result in a variable does not enable
    narrowing or guard unlocking (Spec 52 R12, Spec 49 R17). *)

open Core.Types
module Env = Core.Type_env

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

(** Narrow a type by intersecting with a target type.

    Computes the intersection of [original] and [target]:
    - [any ∩ T] → [T]
    - [T ∩ T] → [T]
    - [(A | B | C) ∩ T] → members of the union that overlap with [T]
    - [T ∩ truthy] → [T] when [T] is truthy (filters out nil)
    - [T ∩ U] → [T] when [T] overlaps with [U], [TUnion []] when disjoint *)
let narrow_type (original : typ) (target : typ) : typ =
  let original = repr original in
  let target = repr target in
  (* any ∩ T = T *)
  if is_any original then target (* T ∩ T = T *)
  else if equal original target then original
  else
    match original with
    | TUnion members ->
        (* Filter union members to those that overlap with target *)
        let overlapping =
          List.filter (fun m -> not (Unify.types_disjoint m target)) members
        in
        normalize_union overlapping
    | _ ->
        (* Non-union original: if not disjoint with target, keep original;
           otherwise empty (the type cannot satisfy the predicate) *)
        if not (Unify.types_disjoint original target) then original
        else Prim.never

(** Try to extract a single predicate from a call expression. *)
let analyze_single_predicate (fn_name : string) (args : Syntax.Sexp.t list)
    (env : Env.t) : predicate_info option =
  match Env.lookup_predicate fn_name env with
  | Some pred_info -> (
      let arg = List.nth_opt args pred_info.param_index in
      match arg with
      | Some (Syntax.Sexp.Symbol (var_name, _)) ->
          Some { var_name; narrowed_type = pred_info.narrowed_type }
      | _ -> None)
  | None -> None

(** Try to extract a feature guard from a call expression.

    Recognizes: [(featurep 'X)], [(fboundp 'f)], [(boundp 'v)],
    [(bound-and-true-p v)], [(require 'X nil t)] (soft require). The first three
    require a quoted symbol argument; [bound-and-true-p] takes a bare symbol
    (it's a macro).

    Soft require [(require 'X nil t)] is treated as a [FeatureGuard] when used
    as a condition: the third argument [t] (noerror) means "return nil if not
    found" (Spec 49 R6). *)
let analyze_single_guard (fn_name : string) (args : Syntax.Sexp.t list) :
    guard_info option =
  match (fn_name, args) with
  (* (featurep 'X) — quoted symbol *)
  | ( "featurep",
      [
        Syntax.Sexp.List
          ( [ Syntax.Sexp.Symbol ("quote", _); Syntax.Sexp.Symbol (feature, _) ],
            _ );
      ] ) ->
      Some (FeatureGuard feature)
  (* (fboundp 'f) — quoted symbol *)
  | ( "fboundp",
      [
        Syntax.Sexp.List
          ([ Syntax.Sexp.Symbol ("quote", _); Syntax.Sexp.Symbol (fname, _) ], _);
      ] ) ->
      Some (FboundGuard fname)
  (* (boundp 'v) — quoted symbol *)
  | ( "boundp",
      [
        Syntax.Sexp.List
          ([ Syntax.Sexp.Symbol ("quote", _); Syntax.Sexp.Symbol (vname, _) ], _);
      ] ) ->
      Some (BoundGuard vname)
  (* (bound-and-true-p v) — bare symbol (macro) *)
  | "bound-and-true-p", [ Syntax.Sexp.Symbol (vname, _) ] ->
      Some (BoundTrueGuard vname)
  (* (require 'X nil t) — soft require, acts as FeatureGuard when used
     as condition. The noerror flag (3rd arg = t) makes it return nil
     on failure instead of signaling. *)
  | ( "require",
      [
        Syntax.Sexp.List
          ( [ Syntax.Sexp.Symbol ("quote", _); Syntax.Sexp.Symbol (feature, _) ],
            _ );
        Syntax.Sexp.Symbol ("nil", _);
        Syntax.Sexp.Symbol ("t", _);
      ] ) ->
      Some (FeatureGuard feature)
  | _ -> None

(** Detect a hard require form: [(require 'X)] with no noerror flag.

    Returns [Some feature_name] for hard requires, [None] for soft requires or
    non-require forms. A hard require unconditionally loads the feature, so it
    extends the environment for all subsequent forms (Spec 49 R5). *)
let detect_hard_require (sexp : Syntax.Sexp.t) : string option =
  match sexp with
  (* (require 'X) — no optional args *)
  | Syntax.Sexp.List
      ( [
          Syntax.Sexp.Symbol ("require", _);
          Syntax.Sexp.List
            ( [ Syntax.Sexp.Symbol ("quote", _); Syntax.Sexp.Symbol (name, _) ],
              _ );
        ],
        _ ) ->
      Some name
  (* (require 'X filename) — no noerror flag, still hard *)
  | Syntax.Sexp.List
      ( [
          Syntax.Sexp.Symbol ("require", _);
          Syntax.Sexp.List
            ( [ Syntax.Sexp.Symbol ("quote", _); Syntax.Sexp.Symbol (name, _) ],
              _ );
          _;
        ],
        _ ) ->
      Some name
  (* (require 'X filename nil) — explicit nil noerror, still hard *)
  | Syntax.Sexp.List
      ( [
          Syntax.Sexp.Symbol ("require", _);
          Syntax.Sexp.List
            ( [ Syntax.Sexp.Symbol ("quote", _); Syntax.Sexp.Symbol (name, _) ],
              _ );
          _;
          Syntax.Sexp.Symbol ("nil", _);
        ],
        _ ) ->
      Some name
  | _ -> None

(** Analyze a single sexp as either a predicate or a guard.

    Returns [(`Pred p)] or [(`Guard g)] or [None]. *)
let analyze_single (fn_name : string) (args : Syntax.Sexp.t list) (env : Env.t)
    : [ `Pred of predicate_info | `Guard of guard_info ] option =
  match analyze_single_predicate fn_name args env with
  | Some p -> Some (`Pred p)
  | None -> (
      match analyze_single_guard fn_name args with
      | Some g -> Some (`Guard g)
      | None -> None)

(** Build a condition_analysis from collected predicates and guards. *)
let build_analysis (preds : predicate_info list) (guards : guard_info list) :
    condition_analysis =
  match (preds, guards) with
  | [], [] -> NoPredicate
  | [ p ], [] -> Predicate p
  | ps, [] -> Predicates ps
  | [], [ g ] -> Guard g
  | [], gs -> Guards gs
  | ps, gs -> PredicatesAndGuards (ps, gs)

(** Analyze a condition expression for predicate calls and feature guards.

    Detects predicate patterns [(predicate_fn arg)] and guard patterns
    [(featurep 'X)], [(fboundp 'f)], [(boundp 'v)], [(bound-and-true-p v)].

    Handles [(and pred1 guard1 ...)] by collecting all predicates and guards
    (Spec 52 R4, Spec 49 R16).

    Per inline-only restriction, only direct calls are recognized. Stored
    results do not enable narrowing or guard unlocking. *)
let analyze_condition (condition : Syntax.Sexp.t) (env : Env.t) :
    condition_analysis =
  match condition with
  | Syntax.Sexp.List (Syntax.Sexp.Symbol ("and", _) :: and_args, _) ->
      (* Collect all predicate calls and guard patterns from and arguments *)
      let preds = ref [] in
      let guards = ref [] in
      List.iter
        (fun arg ->
          match arg with
          | Syntax.Sexp.List (Syntax.Sexp.Symbol (fn_name, _) :: args, _) -> (
              match analyze_single fn_name args env with
              | Some (`Pred p) -> preds := p :: !preds
              | Some (`Guard g) -> guards := g :: !guards
              | None -> ())
          | _ -> ())
        and_args;
      build_analysis (List.rev !preds) (List.rev !guards)
  | Syntax.Sexp.List (Syntax.Sexp.Symbol (fn_name, _) :: args, _) -> (
      match analyze_single fn_name args env with
      | Some (`Pred p) -> Predicate p
      | Some (`Guard g) -> Guard g
      | None -> NoPredicate)
  | _ -> NoPredicate
