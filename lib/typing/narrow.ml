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

(** Extract the return type from a type scheme, if it is a function type.

    Handles [TArrow], [TForall(_, TArrow ...)], and multi-clause functions
    (where all clauses must agree on returning [never]). *)
let fn_return_type_from_scheme (scheme : Env.scheme) : typ option =
  match scheme with
  | Mono (TArrow (_, ret)) -> Some (repr ret)
  | Poly { ps_body = TArrow (_, ret); _ } -> Some (repr ret)
  | _ -> None

(** Check whether a function call expression returns [never].

    Looks up the function name in the env and checks if its declared return type
    is [never]. Also checks multi-clause functions: returns [true] only if all
    clauses return [never]. *)
let call_returns_never (fn_name : string) (env : Env.t) : bool =
  (* Check multi-clause functions first *)
  match Env.lookup_fn_clauses fn_name env with
  | Some clauses ->
      clauses <> []
      && List.for_all
           (fun (c : Env.loaded_clause) -> is_never c.lc_return)
           clauses
  | None -> (
      match Env.lookup_fn fn_name env with
      | Some scheme -> (
          match fn_return_type_from_scheme scheme with
          | Some ret -> is_never ret
          | None -> false)
      | None -> false)

(** Analyze an [or] expression for the never-exit narrowing pattern (Spec 52
    R5).

    Detects forms like [(or (stringp x) (error "Expected string"))] where a
    predicate branch precedes a branch that returns [never]. When this pattern
    is found, the predicate's narrowing applies to subsequent code because if
    execution continues past the [or], the predicate must have been truthy.

    When multiple predicate branches test the same variable, the narrowing is
    the union of their narrowed types. When predicates test different variables,
    no narrowing is applied (we cannot determine which predicate was truthy).

    Returns the predicate narrowings that should be applied after the [or]
    expression. *)
let analyze_or_exit (sexp : Syntax.Sexp.t) (env : Env.t) : predicate_info list =
  match sexp with
  | Syntax.Sexp.List (Syntax.Sexp.Symbol ("or", _) :: or_args, _) -> (
      (* Check if any branch is a call to a function returning never.
         If so, collect predicates from all preceding branches. *)
      let has_never_branch =
        List.exists
          (fun arg ->
            match arg with
            | Syntax.Sexp.List (Syntax.Sexp.Symbol (fn_name, _) :: _, _) ->
                call_returns_never fn_name env
            | _ -> false)
          or_args
      in
      if not has_never_branch then []
      else
        (* Collect predicate narrowings from non-never branches *)
        let preds =
          List.filter_map
            (fun arg ->
              match arg with
              | Syntax.Sexp.List (Syntax.Sexp.Symbol (fn_name, _) :: args, _) ->
                  if call_returns_never fn_name env then None
                  else analyze_single_predicate fn_name args env
              | _ -> None)
            or_args
        in
        (* Group by variable name. Only narrow when all predicates test the
           same variable — with multiple variables we cannot determine which
           predicate was truthy. *)
        match preds with
        | [] -> []
        | [ _ ] -> preds
        | _ ->
            let all_same_var =
              let first = (List.hd preds).var_name in
              List.for_all (fun p -> p.var_name = first) preds
            in
            if all_same_var then
              (* Merge narrowings into a single union predicate *)
              let var_name = (List.hd preds).var_name in
              let narrowed_type =
                TUnion (List.map (fun p -> p.narrowed_type) preds)
              in
              [ { var_name; narrowed_type } ]
            else [])
  | _ -> []
