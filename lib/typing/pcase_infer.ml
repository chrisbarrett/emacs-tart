(** Pcase pattern matching and destructuring inference.

    Extracted from {!Infer} to reduce module size. The [infer_pcase] and
    [infer_pcase_let] entry points receive [~infer] and [~infer_progn] as
    parameters to break the mutual recursion with the main inference engine. *)

open Core.Types
module Env = Core.Type_env
module C = Constraint
module Loc = Syntax.Location

type pattern_binding = { pb_name : string; pb_type : typ }
(** A binding extracted from a pcase pattern: name and its type *)

(** Extract bindings from a pcase pattern.

    Supports backquote ADT patterns, underscore wildcards, unquote bindings,
    and/or combinators, let, app, pred, guard, and map patterns. *)
let rec extract_pattern_bindings (env : Env.t) (_scrutinee_ty : typ)
    (pattern : Syntax.Sexp.t) : pattern_binding list =
  let open Syntax.Sexp in
  let level = Env.current_level env in
  match pattern with
  (* Backquote pattern: `(tag . ,value) or `(tag ,v1 ,v2) *)
  | List ([ Symbol ("\\`", _); quoted_pat ], _) ->
      extract_backquote_bindings env level quoted_pat
  (* Underscore wildcard *)
  | Symbol ("_", _) -> []
  (* Literal patterns - no bindings *)
  | Int _ | Float _ | String _ | Keyword _ -> []
  (* Quoted symbol - no bindings *)
  | List ([ Symbol ("quote", _); Symbol (_, _) ], _) -> []
  (* Comma (unquote) at top level: ,var captures the whole value *)
  | List ([ Symbol (",", _); Symbol (name, _) ], _) ->
      [ { pb_name = name; pb_type = fresh_tvar level } ]
  (* pred pattern: (pred PREDICATE) - no bindings captured *)
  | List ([ Symbol ("pred", _); _ ], _) -> []
  (* guard pattern: (guard EXPR) - no bindings *)
  | List ([ Symbol ("guard", _); _ ], _) -> []
  (* and pattern: (and PAT1 PAT2 ...) - combine all bindings *)
  | List (Symbol ("and", _) :: pats, _) ->
      List.concat_map (extract_pattern_bindings env _scrutinee_ty) pats
  (* or pattern: (or PAT1 PAT2 ...) - use first pattern's bindings *)
  | List (Symbol ("or", _) :: pat :: _, _) ->
      extract_pattern_bindings env _scrutinee_ty pat
  (* let pattern: (let VAR EXPR) - binds var *)
  | List ([ Symbol ("let", _); Symbol (name, _); _ ], _) ->
      [ { pb_name = name; pb_type = fresh_tvar level } ]
  (* app pattern: (app FN PAT) - bindings from inner pattern *)
  | List ([ Symbol ("app", _); _; inner_pat ], _) ->
      extract_pattern_bindings env _scrutinee_ty inner_pat
  (* map pattern: (map :field1 :field2 ...) or (map (:field1 var1) ...) *)
  | List (Symbol ("map", _) :: fields, _) ->
      extract_map_pattern_bindings level fields
  (* Other patterns - no bindings *)
  | _ -> []

(** Extract bindings from a backquoted pattern (inside backquote).

    Handles:
    - [(TAG . ,var)] - cons cell ADT pattern
    - [(TAG ,v1 ,v2 ...)] - list ADT pattern (multi-field)
    - [[TAG ,v1 ,v2 ...]] - vector ADT pattern
    - Nested structures *)
and extract_backquote_bindings (env : Env.t) (level : int)
    (pattern : Syntax.Sexp.t) : pattern_binding list =
  let open Syntax.Sexp in
  match pattern with
  (* Cons pattern: (tag . ,value) - ADT single-field pattern *)
  | Cons (Symbol (_tag, _), List ([ Symbol (",", _); Symbol (name, _) ], _), _)
    ->
      (* For now, give the binding a fresh type variable.
         In the future, we could look up the ADT definition and use the field type. *)
      [ { pb_name = name; pb_type = fresh_tvar level } ]
  (* Cons pattern with nested expr: (tag . ,expr) where expr is not a simple symbol *)
  | Cons (Symbol (_, _), List ([ Symbol (",", _); _ ], _), _) -> []
  (* List pattern with tag: (tag ,v1 ,v2 ...) *)
  | List (Symbol (_tag, _) :: rest, _) ->
      List.concat_map (extract_unquote_binding level) rest
  (* Vector pattern: [tag ,v1 ,v2 ...] *)
  | Vector (Symbol (_tag, _) :: rest, _) ->
      List.concat_map (extract_unquote_binding level) rest
  (* Nested cons: (a . (b . c)) *)
  | Cons (car, cdr, _) ->
      extract_backquote_bindings env level car
      @ extract_backquote_bindings env level cdr
  (* Plain cons pattern without symbol *)
  | List (elems, _) ->
      List.concat_map (extract_backquote_bindings env level) elems
  (* Unquoted variable at this level *)
  | _ -> extract_unquote_binding level pattern

(** Extract binding from an unquote expression: ,var *)
and extract_unquote_binding (level : int) (sexp : Syntax.Sexp.t) :
    pattern_binding list =
  let open Syntax.Sexp in
  match sexp with
  | List ([ Symbol (",", _); Symbol (name, _) ], _) ->
      [ { pb_name = name; pb_type = fresh_tvar level } ]
  | _ -> []

(** Extract bindings from a map pattern: [(map :field1 :field2 ...)] or
    [(map (:field1 var1) (:field2 var2) ...)].

    Each field specifier is either:
    - [:keyword] — bind a variable named after the keyword (without colon)
    - [(:keyword var)] — bind the named variable to the field's value

    Returns bindings with fresh type variables for each field. *)
and extract_map_pattern_bindings (level : int) (fields : Syntax.Sexp.t list) :
    pattern_binding list =
  let open Syntax.Sexp in
  List.filter_map
    (fun field ->
      match field with
      (* :keyword — bind variable with keyword name *)
      | Keyword (name, _) -> Some { pb_name = name; pb_type = fresh_tvar level }
      (* (:keyword var) — bind named variable *)
      | List ([ Keyword (_, _); Symbol (var_name, _) ], _) ->
          Some { pb_name = var_name; pb_type = fresh_tvar level }
      | _ -> None)
    fields

(** Generate row type constraints from a map pattern.

    Given a map pattern [(map :f1 :f2 ...)] and the scrutinee type, generates a
    constraint that the scrutinee is a map-like type with an open row containing
    at least the specified fields. Each field's type variable corresponds to the
    binding extracted by {!extract_map_pattern_bindings}.

    The constraint is: [scrutinee_ty = (map {f1 α1 f2 α2 & r})] where αi are the
    type variables from the pattern bindings. *)
let extract_map_pattern_constraints (env : Env.t)
    (bindings : pattern_binding list) (fields : Syntax.Sexp.t list)
    (scrutinee_ty : typ) (span : Loc.span) : C.set =
  let open Syntax.Sexp in
  (* Build field list from pattern: pair each keyword name with its binding type *)
  let row_fields =
    List.filter_map
      (fun field ->
        match field with
        | Keyword (name, _) ->
            (* Find the binding for this keyword name *)
            let binding = List.find_opt (fun b -> b.pb_name = name) bindings in
            Option.map (fun b -> (name, b.pb_type)) binding
        | List ([ Keyword (key_name, _); Symbol (var_name, _) ], _) ->
            (* Find the binding for the variable name *)
            let binding =
              List.find_opt (fun b -> b.pb_name = var_name) bindings
            in
            Option.map (fun b -> (key_name, b.pb_type)) binding
        | _ -> None)
      fields
  in
  (* Create an open row with fresh row variable for extensibility *)
  let row_var = fresh_tvar (Env.current_level env) in
  let row_ty = open_row row_fields row_var in
  (* Constrain scrutinee to (map {fields & r}) *)
  let map_ty = map_of row_ty in
  C.add (C.equal scrutinee_ty map_ty span) C.empty

(** Infer the type of a pcase-let expression.

    [(pcase-let ((PAT1 EXPR1) (PAT2 EXPR2) ...) BODY...)]

    Each binding destructures EXPR via PAT. Map patterns generate row type
    constraints on the expression. The body runs with all bindings in scope. *)
let infer_pcase_let ~(infer : Env.t -> Syntax.Sexp.t -> Infer_types.result)
    ~(infer_progn :
       Env.t -> Syntax.Sexp.t list -> Loc.span -> Infer_types.result)
    (env : Env.t) (bindings : Syntax.Sexp.t list) (body : Syntax.Sexp.t list)
    (span : Loc.span) : Infer_types.result =
  let open Syntax.Sexp in
  (* Process each (PATTERN EXPR) binding *)
  let rec process_bindings bindings env all_constraints all_undefineds =
    match bindings with
    | [] -> (env, all_constraints, all_undefineds)
    | List ([ pattern; expr ], binding_span) :: rest ->
        (* Infer the expression's type *)
        let expr_result = infer env expr in

        (* Extract pattern bindings *)
        let pat_bindings =
          extract_pattern_bindings env expr_result.Infer_types.ty pattern
        in

        (* Generate map pattern constraints if applicable *)
        let pat_constraints =
          match pattern with
          | List (Symbol ("map", _) :: fields, _) ->
              extract_map_pattern_constraints env pat_bindings fields
                expr_result.Infer_types.ty binding_span
          | _ -> C.empty
        in

        (* Extend environment with pattern bindings *)
        let new_env =
          List.fold_left
            (fun e { pb_name; pb_type } -> Env.extend_mono pb_name pb_type e)
            env pat_bindings
        in

        let constraints =
          C.combine expr_result.Infer_types.constraints
            (C.combine pat_constraints all_constraints)
        in
        let undefineds = all_undefineds @ expr_result.Infer_types.undefineds in

        process_bindings rest new_env constraints undefineds
    | _ :: rest ->
        (* Malformed binding - skip *)
        process_bindings rest env all_constraints all_undefineds
  in

  let body_env, binding_constraints, binding_undefineds =
    process_bindings bindings env C.empty []
  in

  (* Infer the body *)
  let body_result = infer_progn body_env body span in

  {
    ty = body_result.ty;
    constraints = C.combine binding_constraints body_result.constraints;
    undefineds = binding_undefineds @ body_result.undefineds;
    clause_diagnostics = body_result.clause_diagnostics;
  }

(** Infer the type of a pcase expression.

    (pcase EXPR (PAT1 BODY1...) (PAT2 BODY2...) ...)

    Each clause has a pattern and a body. Pattern-bound variables are available
    in the body with narrowed types. All branch bodies must have the same type
    (the result type of the pcase). *)
let infer_pcase ~(infer : Env.t -> Syntax.Sexp.t -> Infer_types.result)
    ~(infer_progn :
       Env.t -> Syntax.Sexp.t list -> Loc.span -> Infer_types.result)
    (env : Env.t) (expr : Syntax.Sexp.t) (clauses : Syntax.Sexp.t list)
    (_span : Loc.span) : Infer_types.result =
  let open Syntax.Sexp in
  (* Infer the type of the scrutinee *)
  let expr_result = infer env expr in
  let scrutinee_ty = expr_result.Infer_types.ty in

  (* Result type is a fresh variable that all branches unify with *)
  let result_ty = fresh_tvar (Env.current_level env) in

  (* Process each clause *)
  let rec process_clauses clauses constraints undefineds =
    match clauses with
    | [] -> (constraints, undefineds)
    | List (pattern :: body, clause_span) :: rest ->
        (* Extract pattern bindings with types *)
        let bindings = extract_pattern_bindings env scrutinee_ty pattern in

        (* Generate map pattern constraints if applicable *)
        let pat_constraints =
          match pattern with
          | List (Symbol ("map", _) :: fields, _) ->
              extract_map_pattern_constraints env bindings fields scrutinee_ty
                clause_span
          | _ -> C.empty
        in

        (* Extend environment with pattern bindings *)
        let body_env =
          List.fold_left
            (fun e { pb_name; pb_type } -> Env.extend_mono pb_name pb_type e)
            env bindings
        in

        (* Infer body type *)
        let body_result = infer_progn body_env body clause_span in

        (* Add constraint: body type = result type *)
        let body_constraint =
          C.equal result_ty body_result.Infer_types.ty clause_span
        in

        let all_constraints =
          C.combine pat_constraints
            (C.combine body_result.Infer_types.constraints
               (C.add body_constraint constraints))
        in
        let all_undefineds = undefineds @ body_result.Infer_types.undefineds in

        process_clauses rest all_constraints all_undefineds
    | _ :: rest ->
        (* Malformed clause - skip it *)
        process_clauses rest constraints undefineds
  in

  let clause_constraints, clause_undefineds =
    process_clauses clauses C.empty []
  in

  (* Combine all constraints *)
  let all_constraints = C.combine expr_result.constraints clause_constraints in
  let all_undefineds = expr_result.undefineds @ clause_undefineds in

  {
    ty = result_ty;
    constraints = all_constraints;
    undefineds = all_undefineds;
    clause_diagnostics = [];
  }
