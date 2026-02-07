(** Multi-clause function dispatch.

    When a function has multiple signature clauses (from .tart files), this
    module tries each clause top-to-bottom via speculative unification. The
    first clause whose parameters match the call-site argument types wins,
    producing a more precise return type than the merged union type. *)

open Core.Types
module Env = Core.Type_env
module Loc = Syntax.Location

type resolved_diagnostic = {
  rcd_severity : Env.diagnostic_severity;
  rcd_message : string;  (** Fully resolved message (no remaining %s) *)
  rcd_span : Loc.span;  (** Call-site span *)
}
(** A clause diagnostic resolved at a call site.

    When multi-clause dispatch selects a clause with a diagnostic annotation,
    the format string's [%s] placeholders are resolved against the actual types
    inferred at the call site. *)

(** Extract the literal value of a call-site argument for clause matching.

    Returns [Some literal] for keywords ([:name]) and quoted symbols (['name]),
    [None] for all other expression forms. *)
let extract_arg_literal (arg : Syntax.Sexp.t) : string option =
  let open Syntax.Sexp in
  match arg with
  | Keyword (name, _) -> Some (":" ^ name)
  | List ([ Symbol ("quote", _); Symbol (name, _) ], _) -> Some name
  | _ -> None

(** Substitute named type variables with fresh types.

    Walks a type, replacing [TCon name] with the substitution for [name] when
    present. Used to instantiate clause type parameters with fresh type
    variables. *)
let rec substitute_tvar_names (subst : (string * typ) list) (ty : typ) : typ =
  match ty with
  | TCon name -> (
      match List.assoc_opt name subst with Some tv -> tv | None -> ty)
  | TVar { contents = Link t } -> substitute_tvar_names subst t
  | TVar _ -> ty
  | TArrow (params, ret) ->
      let params' =
        List.map
          (function
            | PPositional t -> PPositional (substitute_tvar_names subst t)
            | POptional t -> POptional (substitute_tvar_names subst t)
            | PRest t -> PRest (substitute_tvar_names subst t)
            | PKey (n, t) -> PKey (n, substitute_tvar_names subst t)
            | PLiteral _ as p -> p)
          params
      in
      TArrow (params', substitute_tvar_names subst ret)
  | TApp (con, args) ->
      TApp
        ( substitute_tvar_names subst con,
          List.map (substitute_tvar_names subst) args )
  | TForall (vars, body) ->
      (* Remove substituted vars from the substitution to avoid capture *)
      let subst' = List.filter (fun (n, _) -> not (List.mem n vars)) subst in
      TForall (vars, substitute_tvar_names subst' body)
  | TUnion types -> TUnion (List.map (substitute_tvar_names subst) types)
  | TTuple types -> TTuple (List.map (substitute_tvar_names subst) types)
  | TRow { row_fields; row_var } ->
      TRow
        {
          row_fields =
            List.map
              (fun (n, t) -> (n, substitute_tvar_names subst t))
              row_fields;
          row_var = Option.map (substitute_tvar_names subst) row_var;
        }
  | TLiteral _ -> ty

(** Instantiate a loaded clause with fresh type variables.

    The clause's param and return types contain [TCon] names for quantified type
    variables (from the enclosing [defun]). This creates fresh tvars for each
    name and substitutes them, producing types ready for unification.

    Returns [(params, return_type, substitution)] where [substitution] maps type
    variable names to fresh tvars. The substitution is needed to resolve format
    string arguments in clause diagnostics after matching. *)
let instantiate_clause (level : int) (tvar_names : string list)
    (clause : Env.loaded_clause) : param list * typ * (string * typ) list =
  let subst = List.map (fun v -> (v, fresh_tvar level)) tvar_names in
  let params =
    List.map
      (fun p ->
        match p with
        | PPositional t -> PPositional (substitute_tvar_names subst t)
        | POptional t -> POptional (substitute_tvar_names subst t)
        | PRest t -> PRest (substitute_tvar_names subst t)
        | PKey (n, t) -> PKey (n, substitute_tvar_names subst t)
        | PLiteral _ -> p)
      clause.lc_params
  in
  let ret = substitute_tvar_names subst clause.lc_return in
  (params, ret, subst)

(** Try to match a single clause against argument types via speculative
    unification. Returns [Some return_type] if the clause matches, [None] if
    unification fails (all tvar mutations are rolled back on failure).

    [arg_literals] provides the literal values of call-site arguments (extracted
    from the AST), used to match against [PLiteral] clause parameters. *)
let try_clause_match (arg_types : typ list) (arg_literals : string option list)
    (clause_params : param list) (clause_ret : typ) (loc : Loc.span) :
    typ option =
  (* Check literal parameters first. Walk clause params and arg info together;
     for PLiteral params, verify the call-site arg is the matching literal.
     Collect non-literal params and their corresponding arg types for
     unification. *)
  let rec check_literals c_params a_types a_lits typed_params typed_args =
    match (c_params, a_types, a_lits) with
    | PLiteral expected :: c_rest, _ :: a_types_rest, Some actual :: a_lits_rest
      ->
        if expected = actual then
          check_literals c_rest a_types_rest a_lits_rest typed_params typed_args
        else None (* Literal mismatch — clause does not match *)
    | PLiteral _ :: _, _ :: _, None :: _ ->
        None (* Clause expects literal but arg is not a literal *)
    | PLiteral _ :: _, _, [] -> None (* Not enough args for literal param *)
    | param :: c_rest, ty :: a_types_rest, _ :: a_lits_rest ->
        check_literals c_rest a_types_rest a_lits_rest (param :: typed_params)
          (ty :: typed_args)
    | param :: c_rest, ty :: a_types_rest, [] ->
        (* Fewer literals than args — remaining args have no literal info *)
        check_literals c_rest a_types_rest [] (param :: typed_params)
          (ty :: typed_args)
    | _, _, _ ->
        (* Remaining clause params (optional/rest) or exhausted args *)
        let clause_typed = List.rev typed_params @ c_params in
        let arg_typed =
          List.rev typed_args @ a_types |> List.map (fun t -> PPositional t)
        in
        Some (clause_typed, arg_typed)
  in
  match check_literals clause_params arg_types arg_literals [] [] with
  | None -> None (* Literal check failed *)
  | Some (clause_typed, arg_typed) -> (
      match Unify.try_unify_params clause_typed arg_typed loc with
      | Ok () -> Some clause_ret
      | Error _ -> None)

(** Resolve a clause diagnostic's format string by substituting [%s]
    placeholders with the stringified types of referenced type variables.

    The [subst] maps type variable names to the fresh tvars created during
    clause instantiation. After successful unification, those tvars have been
    unified with actual types, so [repr] resolves them. *)
let resolve_diagnostic (subst : (string * typ) list)
    (diag : Env.loaded_diagnostic) (span : Loc.span) : resolved_diagnostic =
  (* Resolve %s placeholders by walking the message character-by-character *)
  let args_resolved =
    List.map
      (fun name ->
        match List.assoc_opt name subst with
        | Some tv -> to_string (repr tv)
        | None -> name)
      diag.ld_args
  in
  let message =
    let buf = Buffer.create (String.length diag.ld_message) in
    let remaining_args = ref args_resolved in
    let i = ref 0 in
    let len = String.length diag.ld_message in
    while !i < len do
      if
        !i + 1 < len
        && diag.ld_message.[!i] = '%'
        && diag.ld_message.[!i + 1] = 's'
      then (
        (match !remaining_args with
        | arg :: rest ->
            Buffer.add_string buf arg;
            remaining_args := rest
        | [] -> Buffer.add_string buf "%s");
        i := !i + 2)
      else (
        Buffer.add_char buf diag.ld_message.[!i];
        i := !i + 1)
    done;
    Buffer.contents buf
  in
  { rcd_severity = diag.ld_severity; rcd_message = message; rcd_span = span }

(** Try clause-by-clause dispatch for a multi-clause function.

    Tries each clause top-to-bottom. For each clause: 1. Instantiate with fresh
    type variables 2. Attempt speculative unification of args with clause params
    3. If all succeed → return clause's return type 4. If any fail → rollback
    and try next clause

    [arg_literals] provides literal values from call-site argument expressions
    for matching against [PLiteral] clause parameters.

    Returns [Some (return_type, diagnostic_opt)] if a clause matched, [None] if
    no clause matched (caller should fall back to union-type constraint path).
    When the matching clause has a diagnostic annotation, the diagnostic is
    resolved with the actual types from unification. *)
let try_dispatch (env : Env.t) ~(tvar_names : string list)
    ~(clauses : Env.loaded_clause list) ~(arg_types : typ list)
    ~(arg_literals : string option list) ~(loc : Loc.span) :
    (typ * resolved_diagnostic option) option =
  let level = Env.current_level env in
  let rec try_clauses = function
    | [] -> None
    | clause :: rest -> (
        let clause_params, clause_ret, subst =
          instantiate_clause level tvar_names clause
        in
        match
          try_clause_match arg_types arg_literals clause_params clause_ret loc
        with
        | Some ret_ty ->
            let diag_opt =
              Option.map
                (fun d -> resolve_diagnostic subst d loc)
                clause.lc_diagnostic
            in
            Some (ret_ty, diag_opt)
        | None -> try_clauses rest)
  in
  try_clauses clauses
