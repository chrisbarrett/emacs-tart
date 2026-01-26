(** Exhaustiveness checking for pcase pattern matching.

    This module checks whether pcase patterns exhaustively cover all
    constructors of an ADT type. Non-exhaustive matches generate warnings.

    Patterns are checked by extracting constructor tags from backquote patterns:
    - [`(ok . ,x)] matches the 'ok constructor
    - [`(err . ,e)] matches the 'err constructor
    - [_] matches everything (wildcard)
    - Literal patterns and quoted symbols are not ADT matches *)

module Types = Core.Types
module Loc = Syntax.Location

(** {1 ADT Registry}

    To check exhaustiveness, we need to know what constructors an ADT has. This
    information comes from loaded signature files. The registry maps ADT type
    constructor names to their list of constructor names. *)

type adt_info = {
  adt_name : string;  (** Name of the ADT type *)
  adt_constructors : string list;  (** Names of all constructors *)
}
(** Information about an ADT needed for exhaustiveness checking *)

type adt_registry = (string * adt_info) list
(** Registry mapping type constructor names to ADT info *)

(** Empty registry *)
let empty_registry : adt_registry = []

(** Register an ADT in the registry *)
let register_adt (type_con : string) (info : adt_info) (reg : adt_registry) :
    adt_registry =
  (type_con, info) :: reg

(** Look up an ADT by its type constructor name *)
let lookup_adt (type_con : string) (reg : adt_registry) : adt_info option =
  List.assoc_opt type_con reg

(** {1 Pattern Analysis}

    Extract constructor tags from pcase patterns to determine which constructors
    are covered. *)

(** Extract the tag from a backquote pattern.

    Handles:
    - [`(tag . ,x)] -> Some "tag"
    - [`(tag ,x ,y)] -> Some "tag"
    - [[tag ,x ,y]] -> Some "tag" (vector pattern)

    Returns None for patterns that don't match ADT constructors. *)
let extract_pattern_tag (pattern : Syntax.Sexp.t) : string option =
  let open Syntax.Sexp in
  match pattern with
  (* Backquote pattern: `(tag . ,value) or `(tag ,v1 ,v2)
     Parser produces: (backquote ...) *)
  | List ([ Symbol ("backquote", _); quoted_pat ], _) -> (
      match quoted_pat with
      (* Cons pattern: (tag . ,value) *)
      | Cons (Symbol (tag, _), _, _) -> Some tag
      (* List pattern: (tag ,v1 ,v2 ...) *)
      | List (Symbol (tag, _) :: _, _) -> Some tag
      (* Vector pattern: [tag ,v1 ,v2 ...] *)
      | Vector (Symbol (tag, _) :: _, _) -> Some tag
      | _ -> None)
  (* Wildcard matches all constructors *)
  | Symbol ("_", _) -> None
  (* Quote pattern: 'tag *)
  | List ([ Symbol ("quote", _); Symbol (tag, _) ], _) -> Some tag
  | _ -> None

(** Check if a pattern is a wildcard that matches everything.

    Wildcards include:
    - [_] explicit wildcard
    - [(or _ ...)] with a wildcard branch
    - Variables that capture the whole value *)
let rec is_wildcard_pattern (pattern : Syntax.Sexp.t) : bool =
  let open Syntax.Sexp in
  match pattern with
  | Symbol ("_", _) -> true
  | Symbol ("t", _) -> true (* t is truthy, often used as catch-all *)
  (* or pattern with wildcard is a wildcard *)
  | List (Symbol ("or", _) :: pats, _) -> List.exists is_wildcard_pattern pats
  (* Quoted nil is sometimes used as a pattern *)
  | List ([ Symbol ("quote", _); Symbol ("nil", _) ], _) -> false
  (* Plain comma capture matches anything *)
  | List ([ Symbol (",", _); _ ], _) -> true
  | _ -> false

(** Extract all constructor tags from a list of patterns.

    Returns the set of covered constructors and whether there's a wildcard. *)
let analyze_patterns (patterns : Syntax.Sexp.t list) :
    string list * bool (* has_wildcard *) =
  let tags =
    List.filter_map
      (fun pat ->
        match pat with
        | Syntax.Sexp.List (pattern :: _body, _) -> extract_pattern_tag pattern
        | _ -> None)
      patterns
  in
  let has_wildcard =
    List.exists
      (fun pat ->
        match pat with
        | Syntax.Sexp.List (pattern :: _body, _) -> is_wildcard_pattern pattern
        | _ -> false)
      patterns
  in
  (* Deduplicate and lowercase tags to match ADT constructor naming *)
  let unique_tags =
    List.sort_uniq String.compare (List.map String.lowercase_ascii tags)
  in
  (unique_tags, has_wildcard)

(** {1 Exhaustiveness Check Result} *)

type exhaustiveness_result =
  | Exhaustive  (** All constructors are covered *)
  | NonExhaustive of string list  (** List of missing constructor names *)
  | NotADT  (** Scrutinee is not a known ADT type *)

(** Check if patterns exhaustively cover an ADT type.

    Returns [Exhaustive] if all constructors are covered, [NonExhaustive] with
    the list of missing constructors otherwise, or [NotADT] if the scrutinee
    type is not a registered ADT. *)
let check_exhaustiveness ~(registry : adt_registry)
    ~(scrutinee_type : Types.typ) ~(patterns : Syntax.Sexp.t list) :
    exhaustiveness_result =
  (* Get the type constructor name from the scrutinee type *)
  let type_con =
    match Types.repr scrutinee_type with
    | Types.TCon name -> Some name
    | Types.TApp (name, _) -> Some name
    | Types.TVar _ -> None (* Type variable - can't determine ADT *)
    | _ -> None
  in
  match type_con with
  | None -> NotADT
  | Some name -> (
      match lookup_adt name registry with
      | None -> NotADT (* Not a known ADT *)
      | Some adt_info ->
          let covered_tags, has_wildcard = analyze_patterns patterns in
          if has_wildcard then Exhaustive
          else
            (* Check which constructors are missing *)
            let all_ctors =
              List.map String.lowercase_ascii adt_info.adt_constructors
            in
            let missing =
              List.filter
                (fun ctor -> not (List.mem ctor covered_tags))
                all_ctors
            in
            if missing = [] then Exhaustive else NonExhaustive missing)

(** {1 Warning Generation} *)

type warning = { span : Loc.span; message : string }
(** A warning about non-exhaustive patterns *)

(** Generate a warning for non-exhaustive pattern match.

    The warning message includes the names of missing constructors. *)
let warning_of_result ~(span : Loc.span) (result : exhaustiveness_result) :
    warning option =
  match result with
  | Exhaustive -> None
  | NotADT -> None
  | NonExhaustive missing ->
      let missing_str =
        match missing with
        | [ name ] -> name
        | names -> String.concat ", " names
      in
      Some
        {
          span;
          message =
            Printf.sprintf "non-exhaustive pattern match. Missing: %s"
              missing_str;
        }

(** {1 Building Registry from Signatures} *)

module Sig_ast = Sig.Sig_ast

(** Build ADT registry from a signature AST.

    Extracts all data declarations and registers them with their
    module-qualified type constructor name (e.g., "mymod/result"). *)
let build_registry_from_signature (sig_ast : Sig_ast.signature) : adt_registry =
  let module_name = sig_ast.sig_module in
  List.fold_left
    (fun reg decl ->
      match decl with
      | Sig_ast.DData d ->
          let type_con = module_name ^ "/" ^ d.data_name in
          let constructors =
            List.map (fun (c : Sig_ast.ctor_decl) -> c.ctor_name) d.data_ctors
          in
          let info =
            { adt_name = d.data_name; adt_constructors = constructors }
          in
          register_adt type_con info reg
      | _ -> reg)
    empty_registry sig_ast.sig_decls

(** Merge multiple registries *)
let merge_registries (regs : adt_registry list) : adt_registry =
  List.concat regs

(** {1 Finding pcase Expressions} *)

type pcase_info = {
  pcase_span : Loc.span;  (** Location of the pcase expression *)
  pcase_scrutinee : Syntax.Sexp.t;  (** The expression being matched *)
  pcase_clauses : Syntax.Sexp.t list;  (** The pattern clauses *)
}
(** Information about a pcase expression *)

(** Recursively find all pcase expressions in an S-expression *)
let rec find_pcases (sexp : Syntax.Sexp.t) : pcase_info list =
  let open Syntax.Sexp in
  match sexp with
  | List (Symbol ("pcase", _) :: scrutinee :: clauses, span) ->
      let nested = List.concat_map find_pcases (scrutinee :: clauses) in
      {
        pcase_span = span;
        pcase_scrutinee = scrutinee;
        pcase_clauses = clauses;
      }
      :: nested
  | List (Symbol ("pcase-exhaustive", _) :: scrutinee :: clauses, span) ->
      (* pcase-exhaustive should be checked too - with stricter expectations *)
      let nested = List.concat_map find_pcases (scrutinee :: clauses) in
      {
        pcase_span = span;
        pcase_scrutinee = scrutinee;
        pcase_clauses = clauses;
      }
      :: nested
  | List (elems, _) -> List.concat_map find_pcases elems
  | Vector (elems, _) -> List.concat_map find_pcases elems
  | Cons (car, cdr, _) -> find_pcases car @ find_pcases cdr
  | _ -> []

(** Find all pcase expressions in a list of top-level forms *)
let find_all_pcases (sexps : Syntax.Sexp.t list) : pcase_info list =
  List.concat_map find_pcases sexps

(** {1 Type Inference for pcase Scrutinees}

    To check exhaustiveness, we need to know the type of each pcase scrutinee.
    We re-infer types for scrutinee expressions in the final environment. *)

(** Infer the type of a scrutinee expression in the given environment *)
let infer_scrutinee_type (env : Core.Type_env.t) (scrutinee : Syntax.Sexp.t) :
    Types.typ option =
  Types.reset_tvar_counter ();
  let result = Infer.infer env scrutinee in
  (* Solve constraints to get the resolved type *)
  let errors = Unify.solve_all result.Infer.constraints in
  if errors = [] then Some (Types.repr result.Infer.ty) else None

(** {1 Full Exhaustiveness Check} *)

(** Check exhaustiveness for all pcase expressions in a program.

    Returns a list of warnings for non-exhaustive matches.

    @param registry ADT registry with constructor information
    @param env Type environment for inferring scrutinee types
    @param sexps The program's S-expressions *)
let check_all_pcases ~(registry : adt_registry) ~(env : Core.Type_env.t)
    (sexps : Syntax.Sexp.t list) : warning list =
  let pcases = find_all_pcases sexps in
  List.filter_map
    (fun pcase ->
      match infer_scrutinee_type env pcase.pcase_scrutinee with
      | None -> None (* Can't determine type - skip *)
      | Some scrutinee_type ->
          let result =
            check_exhaustiveness ~registry ~scrutinee_type
              ~patterns:pcase.pcase_clauses
          in
          warning_of_result ~span:pcase.pcase_span result)
    pcases
