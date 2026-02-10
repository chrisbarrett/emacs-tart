(** Signature loader and validator.

    This module converts signature AST to the type environment, validating type
    variable scoping and resolving type references.

    Validation is implemented in {!Sig_validation} and re-exported here for
    backward compatibility. Type conversion is implemented in {!Sig_convert} and
    re-exported here for backward compatibility. *)

open Sig_ast
module Loc = Syntax.Location
module Types = Core.Types
module Type_env = Core.Type_env

(** {1 Load Errors} *)

type load_error = Sig_validation.validation_error = {
  message : string;
  span : Loc.span;
}
(** Error during signature loading *)

type 'a result = ('a, load_error) Result.t
(** Result type for loading *)

(** Create an error *)
let error message span : 'a result = Error { message; span }

(** {1 Re-exported Validation API} *)

type tvar_context = Sig_validation.tvar_context

let empty_context = Sig_validation.empty_context
let with_tvars = Sig_validation.with_tvars
let with_type = Sig_validation.with_type
let validate_type = Sig_validation.validate_type
let validate_decl = Sig_validation.validate_decl
let build_context = Sig_validation.build_context
let validate_signature = Sig_validation.validate_signature

let validate_signature_all ?prelude_type_names sig_file =
  Sig_validation.validate_signature_all ?prelude_type_names sig_file

(** {1 Re-exported Type Conversion API} *)

type alias_param = Sig_convert.alias_param = {
  ap_name : string;
  ap_bound : sig_type option;
}

type type_alias = Sig_convert.type_alias = {
  alias_params : alias_param list;
  alias_body : sig_type;
}

type alias_context = Sig_convert.alias_context

let empty_aliases = Sig_convert.empty_aliases
let lookup_alias = Sig_convert.lookup_alias
let add_alias = Sig_convert.add_alias
let alias_names = Sig_convert.alias_names
let binder_to_alias_param = Sig_convert.binder_to_alias_param
let build_alias_context = Sig_convert.build_alias_context

type opaque_type = Sig_convert.opaque_type = {
  opaque_params : string list;
  opaque_con : string;
}

type opaque_context = Sig_convert.opaque_context

let empty_opaques = Sig_convert.empty_opaques
let lookup_opaque = Sig_convert.lookup_opaque
let add_opaque = Sig_convert.add_opaque
let opaque_con_name = Sig_convert.opaque_con_name
let build_opaque_context = Sig_convert.build_opaque_context

type type_context = Sig_convert.type_context = {
  tc_aliases : alias_context;
  tc_opaques : opaque_context;
}

let empty_type_context = Sig_convert.empty_type_context
let satisfies_bound = Sig_convert.satisfies_bound
let sig_type_to_typ_with_ctx = Sig_convert.sig_type_to_typ_with_ctx
let sig_param_to_param_with_ctx = Sig_convert.sig_param_to_param_with_ctx
let sig_type_to_typ_with_aliases = Sig_convert.sig_type_to_typ_with_aliases
let sig_type_to_typ = Sig_convert.sig_type_to_typ
let sig_param_to_param = Sig_convert.sig_param_to_param

(** {1 Multi-Clause Type Computation}

    For multi-clause defuns, compute the overall function type by taking the
    union of param types at each position and the union of return types. Also
    derive predicate information from the clause structure. *)

(** Check if a sig_type is a truthy return type (t or truthy). *)
let is_truthy_return (ty : sig_type) : bool =
  match ty with
  | STCon ("t", _) | STCon ("truthy", _) -> true
  | STVar ("t", _) | STVar ("truthy", _) -> true
  | _ -> false

(** Check if a sig_type is a falsy return type (nil). *)
let is_falsy_return (ty : sig_type) : bool =
  match ty with STCon ("nil", _) | STVar ("nil", _) -> true | _ -> false

(** Check if a sig_param uses an STInfer (wildcard) type. *)
let is_infer_param (p : sig_param) : bool =
  match p with
  | SPPositional (_, STInfer (_, _)) -> true
  | SPOptional (_, STInfer (_, _)) -> true
  | SPRest (STInfer (_, _)) -> true
  | SPKey (_, STInfer (_, _)) -> true
  | _ -> false

(** Build a union type from a list of types, deduplicating and flattening. *)
let union_of_types (types : Types.typ list) : Types.typ =
  (* Flatten nested unions and deduplicate *)
  let flat =
    List.concat_map
      (fun t ->
        match Types.repr t with Types.TUnion members -> members | _ -> [ t ])
      types
  in
  (* If both truthy and nil are present, the union is any (top type) *)
  let has_truthy =
    List.exists (fun t -> Types.equal t Types.Prim.truthy) flat
  in
  let has_nil = List.exists (fun t -> Types.equal t Types.Prim.nil) flat in
  if has_truthy && has_nil then Types.Prim.any
  else
    (* Deduplicate *)
    let unique =
      List.fold_left
        (fun acc t ->
          if List.exists (fun u -> Types.equal t u) acc then acc
          else acc @ [ t ])
        [] flat
    in
    match unique with [ single ] -> single | _ -> Types.TUnion unique

(** Build a union param from params at the same position across clauses. *)
let union_of_params (params : Types.param list) : Types.param =
  match params with
  | [] -> Types.PPositional Types.Prim.any
  | first :: _ -> (
      let extract_type = function
        | Types.PPositional t
        | Types.POptional t
        | Types.PRest t
        | Types.PKey (_, t) ->
            t
        | Types.PLiteral v ->
            (* Literal params contribute the type of the literal value *)
            if String.length v > 0 && v.[0] = ':' then Types.Prim.keyword
            else Types.Prim.symbol
      in
      let types = List.map extract_type params in
      let union_ty = union_of_types types in
      (* Preserve the param kind from the first non-literal occurrence,
         falling back to PPositional for all-literal positions *)
      let first_typed =
        List.find_opt (function Types.PLiteral _ -> false | _ -> true) params
      in
      let kind = match first_typed with Some p -> p | None -> first in
      match kind with
      | Types.PPositional _ | Types.PLiteral _ -> Types.PPositional union_ty
      | Types.POptional _ -> Types.POptional union_ty
      | Types.PRest _ -> Types.PRest union_ty
      | Types.PKey (name, _) -> Types.PKey (name, union_ty))

(** Compute the overall function type from multi-clause defun.

    For each parameter position i, the overall type is the union of all clause
    param types at position i. The overall return type is the union of all
    clause return types. *)
let compute_defun_type ?(scope_tvars : (string * Types.typ) list = [])
    (ctx : type_context) (tvar_names : string list)
    (clauses : defun_clause list) : Types.typ =
  match clauses with
  | [] -> Types.TArrow ([], Types.Prim.nil)
  | [ single ] ->
      (* Single clause: direct conversion *)
      let params =
        List.map
          (sig_param_to_param_with_ctx ~scope_tvars ctx tvar_names)
          single.clause_params
      in
      let ret =
        sig_type_to_typ_with_ctx ~scope_tvars ctx tvar_names
          single.clause_return
      in
      Types.TArrow (params, ret)
  | _ ->
      (* Multi-clause: union of param types at each position, union of returns *)
      let max_params =
        List.fold_left
          (fun acc c -> max acc (List.length c.clause_params))
          0 clauses
      in
      (* Convert all clauses to core types *)
      let converted_clauses =
        List.map
          (fun c ->
            let params =
              List.map
                (sig_param_to_param_with_ctx ~scope_tvars ctx tvar_names)
                c.clause_params
            in
            let ret =
              sig_type_to_typ_with_ctx ~scope_tvars ctx tvar_names
                c.clause_return
            in
            (params, ret))
          clauses
      in
      (* Build union params *)
      let union_params =
        List.init max_params (fun i ->
            let params_at_i =
              List.filter_map
                (fun (params, _) -> List.nth_opt params i)
                converted_clauses
            in
            union_of_params params_at_i)
      in
      (* Build union return *)
      let returns = List.map snd converted_clauses in
      let union_ret = union_of_types returns in
      Types.TArrow (union_params, union_ret)

(** Convert an AST diagnostic severity to the loaded (core) severity. *)
let convert_diagnostic_severity :
    diagnostic_severity -> Type_env.diagnostic_severity = function
  | DiagError -> Type_env.DiagError
  | DiagWarn -> Type_env.DiagWarn
  | DiagNote -> Type_env.DiagNote

(** Convert an AST clause diagnostic to a loaded diagnostic. *)
let convert_clause_diagnostic (d : clause_diagnostic) :
    Type_env.loaded_diagnostic =
  {
    Type_env.ld_severity = convert_diagnostic_severity d.diag_severity;
    ld_message = d.diag_message;
    ld_args = d.diag_args;
  }

(** Convert defun clauses to loaded_clause list for overload resolution.

    Each clause's parameters and return type are converted to core types,
    preserving clause structure for call-site dispatch (Spec 56). Clause
    diagnostics are carried through for emission at call sites (Spec 57).
    Returns [Some clauses] for multi-clause defuns or single-clause defuns with
    a diagnostic annotation, [None] otherwise. *)
let compute_defun_clauses ?(scope_tvars : (string * Types.typ) list = [])
    (ctx : type_context) (tvar_names : string list)
    (clauses : defun_clause list) : Type_env.loaded_clause list option =
  let has_diagnostic =
    List.exists (fun c -> Option.is_some c.clause_diagnostic) clauses
  in
  match clauses with
  | [] -> None
  | [ _ ] when not has_diagnostic -> None
  | _ ->
      Some
        (List.map
           (fun c ->
             let lc_params =
               List.map
                 (sig_param_to_param_with_ctx ~scope_tvars ctx tvar_names)
                 c.clause_params
             in
             let lc_return =
               sig_type_to_typ_with_ctx ~scope_tvars ctx tvar_names
                 c.clause_return
             in
             let lc_diagnostic =
               Option.map convert_clause_diagnostic c.clause_diagnostic
             in
             { Type_env.lc_params; lc_return; lc_diagnostic })
           clauses)

(** Derive predicate information from multi-clause structure.

    Analyzes clause structure to determine if this function acts as a type
    predicate: 1. Partition clauses into truthy-returning (t/truthy) and
    falsy-returning (nil) 2. If not a clean partition → None 3. Truthy clauses
    have concrete param\[0\] → narrowed = union(truthy params) 4. Truthy clauses
    have wildcard param\[0\] → narrowed = any - union(falsy params) (inverted
    predicate, e.g., atom)

    Returns predicate info for parameter 0 if derivable, None otherwise. *)
let derive_predicate_info ?(scope_tvars : (string * Types.typ) list = [])
    (ctx : type_context) (tvar_names : string list)
    (clauses : defun_clause list) : Type_env.predicate_info option =
  if List.length clauses < 2 then None
  else
    (* Partition into truthy and falsy clauses *)
    let truthy_clauses =
      List.filter (fun c -> is_truthy_return c.clause_return) clauses
    in
    let falsy_clauses =
      List.filter (fun c -> is_falsy_return c.clause_return) clauses
    in
    (* Must be a clean partition: every clause is either truthy or falsy *)
    if
      List.length truthy_clauses + List.length falsy_clauses
      <> List.length clauses
    then None
    else if truthy_clauses = [] || falsy_clauses = [] then None
    else
      (* All clauses must have at least one parameter *)
      let all_have_params =
        List.for_all (fun c -> List.length c.clause_params > 0) clauses
      in
      if not all_have_params then None
      else
        (* Check if truthy clauses have concrete (non-wildcard) first params *)
        let truthy_first_params =
          List.map (fun c -> List.hd c.clause_params) truthy_clauses
        in
        let truthy_are_concrete =
          List.for_all (fun p -> not (is_infer_param p)) truthy_first_params
        in
        if truthy_are_concrete then
          (* Direct predicate: narrowed = union of truthy first param types *)
          let truthy_types =
            List.map
              (fun c ->
                let first_param = List.hd c.clause_params in
                let core_param =
                  sig_param_to_param_with_ctx ~scope_tvars ctx tvar_names
                    first_param
                in
                match core_param with
                | Types.PPositional t
                | Types.POptional t
                | Types.PRest t
                | Types.PKey (_, t) ->
                    t
                | Types.PLiteral v ->
                    if String.length v > 0 && v.[0] = ':' then
                      Types.Prim.keyword
                    else Types.Prim.symbol)
              truthy_clauses
          in
          let narrowed = union_of_types truthy_types in
          Some
            {
              Type_env.param_index = 0;
              param_name = "x";
              narrowed_type = narrowed;
            }
        else
          (* Check if falsy clauses have concrete first params (inverted
             predicate like `atom`) *)
          let falsy_first_params =
            List.map (fun c -> List.hd c.clause_params) falsy_clauses
          in
          let falsy_are_concrete =
            List.for_all (fun p -> not (is_infer_param p)) falsy_first_params
          in
          if falsy_are_concrete then
            (* Inverted predicate: narrowed = any - union(falsy first params) *)
            let falsy_types =
              List.map
                (fun c ->
                  let first_param = List.hd c.clause_params in
                  let core_param =
                    sig_param_to_param_with_ctx ~scope_tvars ctx tvar_names
                      first_param
                  in
                  match core_param with
                  | Types.PPositional t
                  | Types.POptional t
                  | Types.PRest t
                  | Types.PKey (_, t) ->
                      t
                  | Types.PLiteral v ->
                      if String.length v > 0 && v.[0] = ':' then
                        Types.Prim.keyword
                      else Types.Prim.symbol)
                falsy_clauses
            in
            let falsy_union = union_of_types falsy_types in
            let narrowed = Types.subtract_type Types.Prim.any falsy_union in
            Some
              {
                Type_env.param_index = 0;
                param_name = "x";
                narrowed_type = narrowed;
              }
          else (* Neither pattern matches - no predicate *)
            None

(** {1 Declaration Loading} *)

(** Convert a defun declaration to a type scheme with full type context. Returns
    a Poly scheme if the function has type parameters, otherwise a Mono scheme
    with an arrow type.

    Uses compute_defun_type to handle both single-clause and multi-clause
    defuns. *)
let load_defun_with_ctx (ctx : type_context) (d : defun_decl) : Type_env.scheme
    =
  let tvar_names = List.map (fun b -> b.name) d.defun_tvar_binders in
  let arrow = compute_defun_type ctx tvar_names d.defun_clauses in
  if tvar_names = [] then Type_env.Mono arrow
  else Type_env.Poly { ps_vars = tvar_names; ps_bounds = []; ps_body = arrow }

(** Convert a defun declaration inside a forall block to a type scheme. The
    scope type variables are added to the function's quantifier list. Uses the
    pre-created TVars from the scope for consistency across declarations. *)
let load_defun_with_scope (ctx : type_context)
    (scope_tvars : (string * Types.typ) list) (d : defun_decl) : Type_env.scheme
    =
  (* Get function's own type variables *)
  let fn_tvar_names = List.map (fun b -> b.name) d.defun_tvar_binders in
  (* Scope type variable names *)
  let scope_tvar_names = List.map fst scope_tvars in
  (* Combined tvar names: scope vars first, then function's own vars *)
  let all_tvar_names = scope_tvar_names @ fn_tvar_names in
  let arrow =
    compute_defun_type ~scope_tvars ctx fn_tvar_names d.defun_clauses
  in
  if all_tvar_names = [] then Type_env.Mono arrow
  else
    Type_env.Poly { ps_vars = all_tvar_names; ps_bounds = []; ps_body = arrow }

(** Convert a defvar declaration to a type scheme with full type context. The
    type may be polymorphic if it contains a forall. *)
let load_defvar_with_ctx (ctx : type_context) (d : defvar_decl) :
    Type_env.scheme =
  match d.defvar_type with
  | STForall (binders, body, _) ->
      (* Polymorphic variable - extract quantifiers *)
      let tvar_names = List.map (fun b -> b.name) binders in
      let body_type = sig_type_to_typ_with_ctx ctx tvar_names body in
      Type_env.Poly
        { ps_vars = tvar_names; ps_bounds = []; ps_body = body_type }
  | _ ->
      (* Monomorphic variable *)
      let ty = sig_type_to_typ_with_ctx ctx [] d.defvar_type in
      Type_env.Mono ty

(** Convert a defun declaration to a type scheme (without alias/opaque
    expansion). This is kept for backwards compatibility but users should prefer
    load_defun_with_ctx with the prelude context for proper intrinsic type name
    resolution. *)
let load_defun (d : defun_decl) : Type_env.scheme =
  load_defun_with_ctx empty_type_context d

(** Convert a defvar declaration to a type scheme (without alias/opaque
    expansion). This is kept for backwards compatibility but users should prefer
    load_defvar_with_ctx with the prelude context for proper intrinsic type name
    resolution. *)
let load_defvar (d : defvar_decl) : Type_env.scheme =
  load_defvar_with_ctx empty_type_context d

(** {1 Module Resolution}

    Module resolution is delegated to a callback function, allowing different
    resolution strategies (search paths, bundled stdlib, etc.). *)

type module_resolver = string -> signature option
(** Module resolution callback type. Takes a module name and returns the parsed
    signature if found. The result contains both the signature AST and whether
    it should be treated as an error if not found (for required modules). *)

(** A dummy resolver that finds nothing. Used as default. *)
let no_resolver : module_resolver = fun _ -> None

(** {1 Loaded Module State}

    State accumulated during signature loading, tracking:
    - Type context (aliases and opaques) from this and opened modules
    - Value environment (functions and variables)
    - Exports (which names are exported by this module) *)

type load_state = {
  ls_type_ctx : type_context;  (** Types available for use in signatures *)
  ls_env : Type_env.t;  (** Values exported by this module *)
  ls_resolver : module_resolver;  (** How to find other modules *)
  ls_has_el_file : string -> bool;
      (** Check if a module has a corresponding .el file. Used to enforce R19:
          auxiliary .tart files (no .el) can be included but not opened. *)
  ls_loaded : string list;  (** Modules already loaded (cycle detection) *)
  ls_scope_tvars : (string * Types.typ) list;
      (** Type variables from enclosing forall blocks *)
  ls_imported_types : string list;
      (** Type names imported from prelude/open/include (for shadowing check) *)
  ls_imported_values : string list;
      (** Value names imported from include (for shadowing check) *)
  ls_source_version : Type_env.emacs_version option;
      (** When loading from versioned typings, the minimum Emacs version implied
          by the typings directory path (Spec 50) *)
}
(** State accumulated during loading *)

(** Default el_file checker: assumes all modules have .el files (permissive) *)
let default_has_el_file : string -> bool = fun _ -> true

(** Create initial load state.

    @param type_ctx
      Initial type context (prelude or empty). When provided, prelude type
      aliases are available for use in the signature being loaded.
    @param has_el_file
      Callback to check if a module has a corresponding .el file. Defaults to
      allowing all modules.
    @param imported_types
      Initial set of imported type names (e.g., from prelude). *)
let init_load_state ~type_ctx ~resolver ?(has_el_file = default_has_el_file)
    ~base_env ~module_name ?(imported_types = []) ?(imported_values = [])
    ?source_version () =
  {
    ls_type_ctx = type_ctx;
    ls_env = base_env;
    ls_resolver = resolver;
    ls_has_el_file = has_el_file;
    ls_loaded = [ module_name ];
    (* Mark self as loaded to prevent self-import *)
    ls_scope_tvars = [];
    ls_imported_types = imported_types;
    ls_imported_values = imported_values;
    ls_source_version = source_version;
  }

(** Add a type alias to the load state *)
let add_alias_to_state name alias state =
  let aliases = add_alias name alias state.ls_type_ctx.tc_aliases in
  { state with ls_type_ctx = { state.ls_type_ctx with tc_aliases = aliases } }

(** Add an opaque type to the load state *)
let add_opaque_to_state name opaque state =
  let opaques = add_opaque name opaque state.ls_type_ctx.tc_opaques in
  { state with ls_type_ctx = { state.ls_type_ctx with tc_opaques = opaques } }

(** Add a value binding to the load state (variable namespace) *)
let add_value_to_state name scheme state =
  { state with ls_env = Type_env.extend name scheme state.ls_env }

(** Add a function binding to the load state (function namespace) *)
let add_fn_to_state name scheme state =
  { state with ls_env = Type_env.extend_fn name scheme state.ls_env }

(** Add loaded clauses to the load state for overload resolution *)
let add_fn_clauses_to_state name clauses state =
  { state with ls_env = Type_env.extend_fn_clauses name clauses state.ls_env }

(** Add a predicate to the load state *)
let add_predicate_to_state name info state =
  { state with ls_env = Type_env.extend_predicate name info state.ls_env }

(** Add version range for a function from source_version (Spec 50) *)
let add_fn_version_to_state name state =
  match state.ls_source_version with
  | Some v ->
      let range : Type_env.version_range =
        { min_version = Some v; max_version = None }
      in
      { state with ls_env = Type_env.extend_fn_version name range state.ls_env }
  | None -> state

(** Add version range for a variable from source_version (Spec 50) *)
let add_var_version_to_state name state =
  match state.ls_source_version with
  | Some v ->
      let range : Type_env.version_range =
        { min_version = Some v; max_version = None }
      in
      {
        state with
        ls_env = Type_env.extend_var_version name range state.ls_env;
      }
  | None -> state

(** {1 Shadowing Checks}

    Names imported via prelude, open, or include cannot be redefined in the
    current signature file. This prevents accidental shadowing of standard types
    like `list` or `option`. *)

(** Check if a type name would shadow an imported binding *)
let is_imported_type name state = List.mem name state.ls_imported_types

(** Check if a value name would shadow an imported binding *)
let is_imported_value name state = List.mem name state.ls_imported_values

(** Add a type name to the imported set *)
let mark_type_imported name state =
  { state with ls_imported_types = name :: state.ls_imported_types }

(** Add a value name to the imported set *)
let mark_value_imported name state =
  { state with ls_imported_values = name :: state.ls_imported_values }

(** {1 Import-Struct Processing}

    import-struct generates types and functions for cl-defstruct imports:
    - Type: The struct type (opaque)
    - Constructor: make-<name> taking slot values, returning struct
    - Predicate: <name>-p taking any, returning bool
    - Accessors: <name>-<slot> for each slot, taking struct, returning slot type
*)

(** Load an import-struct declaration. Generates the struct type, constructor,
    predicate, and accessors. *)
let load_import_struct (module_name : string) (d : import_struct_decl)
    (state : load_state) : load_state =
  let struct_name = d.struct_name in

  (* 1. Add the struct type as an opaque type *)
  let opaque =
    { opaque_params = []; opaque_con = opaque_con_name module_name struct_name }
  in
  let state = add_opaque_to_state struct_name opaque state in

  (* The struct type for use in function signatures *)
  let struct_typ = Types.TCon opaque.opaque_con in

  (* 2. Add constructor: make-<name> *)
  let constructor_name = "make-" ^ struct_name in
  let slot_params =
    List.map
      (fun (_, slot_type) ->
        Types.PPositional
          (sig_type_to_typ_with_ctx state.ls_type_ctx [] slot_type))
      d.struct_slots
  in
  let constructor_scheme =
    Type_env.Mono (Types.TArrow (slot_params, struct_typ))
  in
  let state = add_value_to_state constructor_name constructor_scheme state in

  (* 3. Add predicate: <name>-p *)
  let predicate_name = struct_name ^ "-p" in
  let predicate_scheme =
    Type_env.Mono
      (Types.TArrow ([ Types.PPositional Types.Prim.any ], Types.Prim.bool))
  in
  let state = add_value_to_state predicate_name predicate_scheme state in

  (* 4. Add accessors: <name>-<slot> for each slot *)
  let state =
    List.fold_left
      (fun state (slot_name, slot_type) ->
        let accessor_name = struct_name ^ "-" ^ slot_name in
        let slot_typ =
          sig_type_to_typ_with_ctx state.ls_type_ctx [] slot_type
        in
        let accessor_scheme =
          Type_env.Mono
            (Types.TArrow ([ Types.PPositional struct_typ ], slot_typ))
        in
        add_value_to_state accessor_name accessor_scheme state)
      state d.struct_slots
  in
  state

(** {1 Data (ADT) Processing}

    data declarations generate types and functions for ADT definitions:
    - Type: The ADT type (opaque with type parameters)
    - Constructors: One function per constructor variant

    Runtime representation (per spec 11):
    - Nullary: 'tag symbol
    - Single-field: (cons 'tag value)
    - Multi-field: [tag field1 field2 ...] *)

(** Load a data declaration. Generates the ADT type, constructor functions, and
    predicate functions. *)
let load_data (module_name : string) (d : data_decl) (state : load_state) :
    load_state =
  let data_name = d.data_name in
  let type_params = List.map (fun b -> b.name) d.data_params in

  (* 1. Add the data type as an opaque type (with type parameters) *)
  let opaque =
    {
      opaque_params = type_params;
      opaque_con = opaque_con_name module_name data_name;
    }
  in
  let state = add_opaque_to_state data_name opaque state in

  (* 2. Add constructor functions and predicates for each variant *)
  List.fold_left
    (fun state (ctor : ctor_decl) ->
      let ctor_name = ctor.ctor_name in
      (* Build parameter list for constructor function:
         each field becomes a positional parameter.
         Type params are in scope as TCon names. *)
      let field_params =
        List.map
          (fun field_ty ->
            let typ =
              sig_type_to_typ_with_ctx state.ls_type_ctx type_params field_ty
            in
            Types.PPositional typ)
          ctor.ctor_fields
      in
      (* Return type is the ADT type (may be parameterized) *)
      let return_typ =
        if type_params = [] then Types.TCon opaque.opaque_con
        else
          Types.TApp
            ( Types.TCon opaque.opaque_con,
              List.map (fun p -> Types.TCon p) type_params )
      in
      let ctor_typ = Types.TArrow (field_params, return_typ) in
      (* If polymorphic, wrap in forall (constructors have no constraints) *)
      let ctor_scheme =
        if type_params = [] then Type_env.Mono ctor_typ
        else
          Type_env.Poly
            { ps_vars = type_params; ps_bounds = []; ps_body = ctor_typ }
      in
      let state = add_value_to_state ctor_name ctor_scheme state in

      (* Add predicate function: <adt-name>-<ctor-lowercase>-p
         Predicate takes any value and returns bool *)
      let pred_name =
        data_name ^ "-" ^ String.lowercase_ascii ctor_name ^ "-p"
      in
      let pred_scheme =
        Type_env.Mono
          (Types.TArrow ([ Types.PPositional Types.Prim.any ], Types.Prim.bool))
      in
      let state = add_value_to_state pred_name pred_scheme state in

      (* Add accessor functions for fields.
         - Nullary constructors: no accessors
         - Single-field: <adt>-<ctor-lowercase>-value
         - Multi-field: <adt>-<ctor-lowercase>-1, <adt>-<ctor-lowercase>-2, ...

         Accessors take the ADT type and return the field type.
         For polymorphic ADTs, accessors are also polymorphic. *)
      let ctor_lower = String.lowercase_ascii ctor_name in
      let num_fields = List.length ctor.ctor_fields in
      let state =
        if num_fields = 0 then
          (* Nullary constructor - no accessor *)
          state
        else if num_fields = 1 then
          (* Single-field constructor: <adt>-<ctor>-value *)
          let accessor_name = data_name ^ "-" ^ ctor_lower ^ "-value" in
          let field_ty = List.hd ctor.ctor_fields in
          let field_typ =
            sig_type_to_typ_with_ctx state.ls_type_ctx type_params field_ty
          in
          (* Input type is the ADT type *)
          let input_typ =
            if type_params = [] then Types.TCon opaque.opaque_con
            else
              Types.TApp
                ( Types.TCon opaque.opaque_con,
                  List.map (fun p -> Types.TCon p) type_params )
          in
          let accessor_typ =
            Types.TArrow ([ Types.PPositional input_typ ], field_typ)
          in
          let accessor_scheme =
            if type_params = [] then Type_env.Mono accessor_typ
            else
              Type_env.Poly
                {
                  ps_vars = type_params;
                  ps_bounds = [];
                  ps_body = accessor_typ;
                }
          in
          add_value_to_state accessor_name accessor_scheme state
        else
          (* Multi-field constructor: <adt>-<ctor>-1, <adt>-<ctor>-2, ... *)
          let input_typ =
            if type_params = [] then Types.TCon opaque.opaque_con
            else
              Types.TApp
                ( Types.TCon opaque.opaque_con,
                  List.map (fun p -> Types.TCon p) type_params )
          in
          List.fold_left
            (fun (state, idx) field_ty ->
              let accessor_name =
                data_name ^ "-" ^ ctor_lower ^ "-" ^ string_of_int idx
              in
              let field_typ =
                sig_type_to_typ_with_ctx state.ls_type_ctx type_params field_ty
              in
              let accessor_typ =
                Types.TArrow ([ Types.PPositional input_typ ], field_typ)
              in
              let accessor_scheme =
                if type_params = [] then Type_env.Mono accessor_typ
                else
                  Type_env.Poly
                    {
                      ps_vars = type_params;
                      ps_bounds = [];
                      ps_body = accessor_typ;
                    }
              in
              (add_value_to_state accessor_name accessor_scheme state, idx + 1))
            (state, 1) ctor.ctor_fields
          |> fst
      in
      state)
    state d.data_ctors

(** {1 Defstruct Processing}

    defstruct generates types and functions for cl-defstruct declarations:
    - Type: The struct type as opaque (record tag)
    - Constructor: make-<name> taking field values, returning (record name)
    - Predicate: <name>-p multi-clause ((record name) -> t) ((_ ) -> nil)
    - Accessors: <name>-<field> for each field, taking (record name), returning
      field type *)

(** Resolve parent struct fields for (:include parent). Looks up the parent
    struct in the load state's type environment by checking for an existing
    defstruct-generated constructor. Returns parent fields by examining
    registered struct_decls from previous loads.

    For now, parent field resolution is done by looking for a previously loaded
    DDefstruct with matching name in the decl history. Since the loader
    processes declarations sequentially, the parent must already be loaded. *)
let resolve_parent_fields (_parent_name : string) (_state : load_state) :
    struct_field list =
  (* Parent field resolution would require tracking loaded struct_decls.
     For this iteration, inheritance via (:include) requires the parent fields
     to be explicitly repeated. A future enhancement could store struct_decls
     in load_state. *)
  []

(** Load a defstruct declaration. Generates the struct type (using record),
    constructor, predicate, and accessor functions. *)
let load_defstruct (module_name : string) (d : struct_decl) (state : load_state)
    : load_state =
  let struct_name = d.sd_name in
  let loc = d.sd_loc in

  (* Resolve parent fields if (:include parent) is present *)
  let parent_fields =
    match d.sd_include with
    | Some parent_name -> resolve_parent_fields parent_name state
    | None -> []
  in
  let all_fields = parent_fields @ d.sd_fields in

  (* The struct type is (record name) — use the prelude's record opaque type.
     We represent it as TApp(TCon "record", [TLiteral (LSymbol name)]) via
     the sig_type route. But since record is already declared in prelude,
     we build the core type directly. *)
  let struct_tag_lit =
    Types.TLiteral (Types.LitSymbol struct_name, Types.Prim.symbol)
  in
  let record_typ =
    match lookup_opaque "record" state.ls_type_ctx.tc_opaques with
    | Some opaque ->
        Types.TApp (Types.TCon opaque.opaque_con, [ struct_tag_lit ])
    | None ->
        (* Fallback: if record type not available, use a generated opaque *)
        let opaque_con = opaque_con_name module_name struct_name in
        Types.TCon opaque_con
  in

  (* 1. Add constructor: make-<name> *)
  let constructor_name = "make-" ^ struct_name in
  let ctor_params =
    if d.sd_keyword_ctor then
      (* Keyword constructor: &key :field1 type1 :field2 type2 ... *)
      List.map
        (fun (field : struct_field) ->
          let field_typ =
            sig_type_to_typ_with_ctx state.ls_type_ctx [] field.sf_type
          in
          Types.PKey (field.sf_name, field_typ))
        all_fields
    else
      (* Positional constructor: check if nullable fields should be &optional *)
      let rec build_params required optional = function
        | [] ->
            let req_params =
              List.rev_map (fun t -> Types.PPositional t) required
            in
            let opt_params =
              List.rev_map (fun t -> Types.POptional t) optional
            in
            req_params @ opt_params
        | (field : struct_field) :: rest ->
            let field_typ =
              sig_type_to_typ_with_ctx state.ls_type_ctx [] field.sf_type
            in
            (* If we've already seen optional params, remaining must be optional too *)
            if optional <> [] then
              build_params required (field_typ :: optional) rest
            else
              (* Check if field type includes nil — if so, make optional *)
              let is_nullable =
                match Types.repr field_typ with
                | Types.TUnion members ->
                    List.exists (fun t -> Types.equal t Types.Prim.nil) members
                | t -> Types.equal t Types.Prim.nil
              in
              if is_nullable then
                build_params required (field_typ :: optional) rest
              else build_params (field_typ :: required) optional rest
      in
      build_params [] [] all_fields
  in
  let constructor_scheme =
    Type_env.Mono (Types.TArrow (ctor_params, record_typ))
  in
  let state = add_fn_to_state constructor_name constructor_scheme state in
  let state = add_fn_version_to_state constructor_name state in

  (* 2. Add predicate: <name>-p as multi-clause *)
  let predicate_name = struct_name ^ "-p" in
  let pred_clause_truthy : Type_env.loaded_clause =
    {
      lc_params = [ Types.PPositional record_typ ];
      lc_return = Types.Prim.t;
      lc_diagnostic = None;
    }
  in
  let pred_clause_falsy : Type_env.loaded_clause =
    {
      lc_params = [ Types.PPositional Types.Prim.any ];
      lc_return = Types.Prim.nil;
      lc_diagnostic = None;
    }
  in
  let predicate_scheme =
    Type_env.Mono
      (Types.TArrow ([ Types.PPositional Types.Prim.any ], Types.Prim.bool))
  in
  let state = add_fn_to_state predicate_name predicate_scheme state in
  let state = add_fn_version_to_state predicate_name state in
  let state =
    add_fn_clauses_to_state predicate_name
      [ pred_clause_truthy; pred_clause_falsy ]
      state
  in
  (* Register predicate info for type narrowing *)
  let pred_info : Type_env.predicate_info =
    { param_index = 0; param_name = "x"; narrowed_type = record_typ }
  in
  let state = add_predicate_to_state predicate_name pred_info state in

  (* 3. Add accessors: <name>-<field> for each field *)
  let state =
    List.fold_left
      (fun state (field : struct_field) ->
        let accessor_name = struct_name ^ "-" ^ field.sf_name in
        let field_typ =
          sig_type_to_typ_with_ctx state.ls_type_ctx [] field.sf_type
        in
        let accessor_scheme =
          Type_env.Mono
            (Types.TArrow ([ Types.PPositional record_typ ], field_typ))
        in
        let state = add_fn_to_state accessor_name accessor_scheme state in
        add_fn_version_to_state accessor_name state)
      state all_fields
  in

  (* 4. Synthesize a defstruct-name defun clause for use with type-of:
     We generate a defvar for the name to make the type name visible *)
  let _ = loc in
  state

(** {1 Open and Include Processing}

    - open: Import types for use in signatures (not re-exported to value env)
    - include: Inline all declarations (types available AND values re-exported)
*)

(** Result bind operator for loading (polymorphic, not monomorphized by let rec)
*)
let ( let* ) = Result.bind

(** Check that a type name doesn't shadow an imported binding. Returns an error
    with the declaration's span if shadowing is detected. *)
let check_type_not_shadowing name span state =
  if is_imported_type name state then
    error (Printf.sprintf "cannot redefine imported binding '%s'" name) span
  else Ok ()

(** Check that a value name doesn't shadow an imported binding. Returns an error
    with the declaration's span if shadowing is detected. *)
let check_value_not_shadowing name span state =
  if is_imported_value name state then
    error (Printf.sprintf "cannot redefine imported binding '%s'" name) span
  else Ok ()

(** Process an 'open' directive. Imports types from another module for use in
    type expressions. Types are NOT re-exported; they are only available for
    signature writing.

    R12: "seq is available for use in type expressions" "seq is NOT re-exported
    from my-collection"

    R19: Auxiliary .tart files (no corresponding .el) cannot be opened; they
    must be included instead. *)
let process_open (module_name : string) (span : Loc.span) (state : load_state) :
    load_state result =
  if List.mem module_name state.ls_loaded then
    (* Already loaded - skip to prevent cycles *)
    Ok state
  else
    (* R19: Check if the module has a corresponding .el file *)
    let* () =
      if state.ls_has_el_file module_name then Ok ()
      else
        error
          (Printf.sprintf
             "cannot open auxiliary module '%s' (no .el file); use include"
             module_name)
          span
    in
    match state.ls_resolver module_name with
    | None ->
        (* Module not found - for now, silently skip.
           Later we can add proper error reporting. *)
        Ok state
    | Some opened_sig ->
        (* Mark as loaded *)
        let state = { state with ls_loaded = module_name :: state.ls_loaded } in
        (* Import type aliases from opened module *)
        let aliases = build_alias_context opened_sig in
        let state =
          List.fold_left
            (fun s (name, alias) ->
              let s = add_alias_to_state name alias s in
              mark_type_imported name s)
            state aliases
        in
        (* Import opaque types from opened module *)
        let opaques = build_opaque_context opened_sig.sig_module opened_sig in
        let state =
          List.fold_left
            (fun s (name, opaque) ->
              let s = add_opaque_to_state name opaque s in
              mark_type_imported name s)
            state opaques
        in
        (* Note: we do NOT add values to ls_env - open only imports types *)
        Ok state

(** Process an 'include' directive. Imports all declarations from another module
    and re-exports them. Both types AND values become part of this module's
    interface.

    R13: "all declarations from seq.tart are part of my-extended-seq's
    interface" "types from seq are available for use in signatures" *)
let rec process_include (module_name : string) (state : load_state) :
    load_state result =
  if List.mem module_name state.ls_loaded then
    (* Already loaded - skip to prevent cycles *)
    Ok state
  else
    match state.ls_resolver module_name with
    | None ->
        (* Module not found - for now, silently skip *)
        Ok state
    | Some included_sig ->
        (* Mark as loaded *)
        let state = { state with ls_loaded = module_name :: state.ls_loaded } in
        (* Process the included signature's declarations recursively.
           Mark as from_include so names are tracked as imported. *)
        load_decls_into_state ~from_include:true included_sig state

(** Load declarations from a signature into the load state. Handles all
    declaration types including open/include recursively. Forall inference is
    applied per-declaration using the accumulated type context, so types from
    includes/opens are known.

    @param from_include
      If true, declarations come from an include and should be marked as
      imported (preventing shadowing by later declarations in the including
      file). If false, declarations are from the current file and should be
      checked against the imported set. *)
and load_decls_into_state ?(from_include = false) (sig_file : signature)
    (state : load_state) : load_state result =
  List.fold_left
    (fun state_r decl ->
      match state_r with
      | Error _ as e -> e
      | Ok state -> (
          match decl with
          | DOpen (name, span) -> process_open name span state
          | DInclude (name, _) -> process_include name state
          | DType d ->
              (* Process each binding in the group *)
              let* state =
                List.fold_left
                  (fun state_r (b : type_binding) ->
                    let* state = state_r in
                    (* Check shadowing if from current file *)
                    let* () =
                      if not from_include then
                        check_type_not_shadowing b.tb_name b.tb_loc state
                      else Ok ()
                    in
                    (* Add to type context *)
                    let state =
                      match b.tb_body with
                      | Some body ->
                          let alias =
                            {
                              alias_params =
                                List.map binder_to_alias_param b.tb_params;
                              alias_body = body;
                            }
                          in
                          add_alias_to_state b.tb_name alias state
                      | None ->
                          let opaque =
                            {
                              opaque_params =
                                List.map (fun p -> p.name) b.tb_params;
                              opaque_con =
                                opaque_con_name sig_file.sig_module b.tb_name;
                            }
                          in
                          add_opaque_to_state b.tb_name opaque state
                    in
                    (* Mark as imported if from include *)
                    Ok
                      (if from_include then mark_type_imported b.tb_name state
                       else state))
                  (Ok state) d.type_bindings
              in
              Ok state
          | DDefun d ->
              (* Check shadowing if from current file *)
              let* () =
                if not from_include then
                  check_value_not_shadowing d.defun_name d.defun_loc state
                else Ok ()
              in
              let tvar_names =
                List.map (fun b -> b.name) d.defun_tvar_binders
              in
              let scheme = load_defun_with_ctx state.ls_type_ctx d in
              (* Add to function namespace for Lisp-2 semantics *)
              let state = add_fn_to_state d.defun_name scheme state in
              (* Track version from typings path (Spec 50) *)
              let state = add_fn_version_to_state d.defun_name state in
              (* Preserve clause structure for overload resolution *)
              let state =
                match
                  compute_defun_clauses state.ls_type_ctx tvar_names
                    d.defun_clauses
                with
                | Some clauses ->
                    add_fn_clauses_to_state d.defun_name clauses state
                | None -> state
              in
              (* Derive and register predicate info from clause structure *)
              let state =
                match
                  derive_predicate_info state.ls_type_ctx tvar_names
                    d.defun_clauses
                with
                | Some pred_info ->
                    add_predicate_to_state d.defun_name pred_info state
                | None -> state
              in
              (* Mark as imported if from include *)
              Ok
                (if from_include then mark_value_imported d.defun_name state
                 else state)
          | DDefvar d ->
              (* Check shadowing if from current file *)
              let* () =
                if not from_include then
                  check_value_not_shadowing d.defvar_name d.defvar_loc state
                else Ok ()
              in
              let scheme = load_defvar_with_ctx state.ls_type_ctx d in
              let state = add_value_to_state d.defvar_name scheme state in
              (* Track version from typings path (Spec 50) *)
              let state = add_var_version_to_state d.defvar_name state in
              (* Mark as imported if from include *)
              Ok
                (if from_include then mark_value_imported d.defvar_name state
                 else state)
          | DImportStruct d ->
              (* import-struct generates type + multiple values *)
              let* () =
                if not from_include then
                  check_type_not_shadowing d.struct_name d.struct_loc state
                else Ok ()
              in
              let state = load_import_struct sig_file.sig_module d state in
              Ok
                (if from_include then
                   let state = mark_type_imported d.struct_name state in
                   (* Also mark generated functions as imported *)
                   let state =
                     mark_value_imported ("make-" ^ d.struct_name) state
                   in
                   let state =
                     mark_value_imported (d.struct_name ^ "-p") state
                   in
                   List.fold_left
                     (fun s (slot_name, _) ->
                       mark_value_imported (d.struct_name ^ "-" ^ slot_name) s)
                     state d.struct_slots
                 else state)
          | DData d ->
              (* data generates type + constructor functions *)
              let* () =
                if not from_include then
                  check_type_not_shadowing d.data_name d.data_loc state
                else Ok ()
              in
              let state = load_data sig_file.sig_module d state in
              Ok
                (if from_include then
                   let state = mark_type_imported d.data_name state in
                   (* Mark constructor functions and predicates as imported *)
                   List.fold_left
                     (fun s (ctor : ctor_decl) ->
                       let s = mark_value_imported ctor.ctor_name s in
                       mark_value_imported
                         (d.data_name ^ "-"
                         ^ String.lowercase_ascii ctor.ctor_name
                         ^ "-p")
                         s)
                     state d.data_ctors
                 else state)
          | DDefstruct d ->
              (* defstruct generates type + constructor, predicate, accessors *)
              let* () =
                if not from_include then
                  check_type_not_shadowing d.sd_name d.sd_loc state
                else Ok ()
              in
              let state = load_defstruct sig_file.sig_module d state in
              Ok
                (if from_include then
                   let state = mark_type_imported d.sd_name state in
                   let state =
                     mark_value_imported ("make-" ^ d.sd_name) state
                   in
                   let state = mark_value_imported (d.sd_name ^ "-p") state in
                   List.fold_left
                     (fun s (field : struct_field) ->
                       mark_value_imported (d.sd_name ^ "-" ^ field.sf_name) s)
                     state d.sd_fields
                 else state)
          | DForall d -> load_forall ~from_include sig_file d state
          | DLetType d ->
              (* Process each binding in the group *)
              let* state =
                List.fold_left
                  (fun state_r (b : type_binding) ->
                    let* state = state_r in
                    (* Shadowing check still runs *)
                    let* () =
                      if not from_include then
                        check_type_not_shadowing b.tb_name b.tb_loc state
                      else Ok ()
                    in
                    (* Same loading as DType: add alias or opaque to type context *)
                    let state =
                      match b.tb_body with
                      | Some body ->
                          let alias =
                            {
                              alias_params =
                                List.map binder_to_alias_param b.tb_params;
                              alias_body = body;
                            }
                          in
                          add_alias_to_state b.tb_name alias state
                      | None ->
                          let opaque =
                            {
                              opaque_params =
                                List.map (fun p -> p.name) b.tb_params;
                              opaque_con =
                                opaque_con_name sig_file.sig_module b.tb_name;
                            }
                          in
                          add_opaque_to_state b.tb_name opaque state
                    in
                    (* Not exported: no mark_type_imported, even from include *)
                    Ok state)
                  (Ok state) d.type_bindings
              in
              Ok state))
    (Ok state) sig_file.sig_decls

(** Load a forall block. Creates fresh type variables for the scope's binders
    and processes each inner declaration with those variables available in the
    type context. *)
and load_forall ?(from_include = false) (sig_file : signature)
    (scope : forall_decl) (state : load_state) : load_state result =
  (* Create fresh TVars for each scope binder.
     Level 0 is used for signature-level type variables. *)
  let scope_tvars =
    List.map
      (fun binder -> (binder.name, Types.fresh_tvar 0))
      scope.forall_tvar_binders
  in
  (* Extend state with scope type variables *)
  let state_with_scope =
    { state with ls_scope_tvars = scope_tvars @ state.ls_scope_tvars }
  in
  (* Process inner declarations with the scope context *)
  List.fold_left
    (fun st_r decl ->
      match st_r with
      | Error _ as e -> e
      | Ok st -> load_scoped_decl ~from_include sig_file scope_tvars decl st)
    (Ok state_with_scope) scope.forall_decls

(** Load a declaration inside a forall block. Adds scope type variables to the
    defun's quantifier list. *)
and load_scoped_decl ?(from_include = false) (sig_file : signature)
    (scope_tvars : (string * Types.typ) list) (decl : decl) (state : load_state)
    : load_state result =
  match decl with
  | DOpen (name, span) -> process_open name span state
  | DInclude (name, _) -> process_include name state
  | DType d ->
      (* Process each binding in the group *)
      let* state =
        List.fold_left
          (fun state_r (b : type_binding) ->
            let* state = state_r in
            (* Check shadowing if from current file *)
            let* () =
              if not from_include then
                check_type_not_shadowing b.tb_name b.tb_loc state
              else Ok ()
            in
            (* Type declarations in scopes work the same way *)
            let state =
              match b.tb_body with
              | Some body ->
                  let alias =
                    {
                      alias_params = List.map binder_to_alias_param b.tb_params;
                      alias_body = body;
                    }
                  in
                  add_alias_to_state b.tb_name alias state
              | None ->
                  let opaque =
                    {
                      opaque_params = List.map (fun p -> p.name) b.tb_params;
                      opaque_con = opaque_con_name sig_file.sig_module b.tb_name;
                    }
                  in
                  add_opaque_to_state b.tb_name opaque state
            in
            Ok
              (if from_include then mark_type_imported b.tb_name state
               else state))
          (Ok state) d.type_bindings
      in
      Ok state
  | DDefun d ->
      (* Check shadowing if from current file *)
      let* () =
        if not from_include then
          check_value_not_shadowing d.defun_name d.defun_loc state
        else Ok ()
      in
      let fn_tvar_names = List.map (fun b -> b.name) d.defun_tvar_binders in
      (* For defuns in scope, combine scope tvars with function's own tvars *)
      let scheme = load_defun_with_scope state.ls_type_ctx scope_tvars d in
      (* Add to function namespace for Lisp-2 semantics *)
      let state = add_fn_to_state d.defun_name scheme state in
      (* Track version from typings path (Spec 50) *)
      let state = add_fn_version_to_state d.defun_name state in
      (* Preserve clause structure for overload resolution *)
      let state =
        match
          compute_defun_clauses ~scope_tvars state.ls_type_ctx fn_tvar_names
            d.defun_clauses
        with
        | Some clauses -> add_fn_clauses_to_state d.defun_name clauses state
        | None -> state
      in
      (* Derive and register predicate info from clause structure *)
      let state =
        match
          derive_predicate_info ~scope_tvars state.ls_type_ctx fn_tvar_names
            d.defun_clauses
        with
        | Some pred_info -> add_predicate_to_state d.defun_name pred_info state
        | None -> state
      in
      Ok
        (if from_include then mark_value_imported d.defun_name state else state)
  | DDefvar d ->
      (* Check shadowing if from current file *)
      let* () =
        if not from_include then
          check_value_not_shadowing d.defvar_name d.defvar_loc state
        else Ok ()
      in
      let scheme = load_defvar_with_ctx state.ls_type_ctx d in
      let state = add_value_to_state d.defvar_name scheme state in
      (* Track version from typings path (Spec 50) *)
      let state = add_var_version_to_state d.defvar_name state in
      Ok
        (if from_include then mark_value_imported d.defvar_name state else state)
  | DImportStruct d ->
      let* () =
        if not from_include then
          check_type_not_shadowing d.struct_name d.struct_loc state
        else Ok ()
      in
      let state = load_import_struct sig_file.sig_module d state in
      Ok
        (if from_include then
           let state = mark_type_imported d.struct_name state in
           let state = mark_value_imported ("make-" ^ d.struct_name) state in
           let state = mark_value_imported (d.struct_name ^ "-p") state in
           List.fold_left
             (fun s (slot_name, _) ->
               mark_value_imported (d.struct_name ^ "-" ^ slot_name) s)
             state d.struct_slots
         else state)
  | DData d ->
      let* () =
        if not from_include then
          check_type_not_shadowing d.data_name d.data_loc state
        else Ok ()
      in
      let state = load_data sig_file.sig_module d state in
      Ok
        (if from_include then
           let state = mark_type_imported d.data_name state in
           List.fold_left
             (fun s (ctor : ctor_decl) ->
               let s = mark_value_imported ctor.ctor_name s in
               mark_value_imported
                 (d.data_name ^ "-"
                 ^ String.lowercase_ascii ctor.ctor_name
                 ^ "-p")
                 s)
             state d.data_ctors
         else state)
  | DDefstruct d ->
      let* () =
        if not from_include then
          check_type_not_shadowing d.sd_name d.sd_loc state
        else Ok ()
      in
      let state = load_defstruct sig_file.sig_module d state in
      Ok
        (if from_include then
           let state = mark_type_imported d.sd_name state in
           let state = mark_value_imported ("make-" ^ d.sd_name) state in
           let state = mark_value_imported (d.sd_name ^ "-p") state in
           List.fold_left
             (fun s (field : struct_field) ->
               mark_value_imported (d.sd_name ^ "-" ^ field.sf_name) s)
             state d.sd_fields
         else state)
  | DForall nested ->
      (* Handle nested scopes recursively *)
      load_forall ~from_include sig_file nested state
  | DLetType d ->
      (* Process each binding in the group *)
      let* state =
        List.fold_left
          (fun state_r (b : type_binding) ->
            let* state = state_r in
            (* Shadowing check still runs *)
            let* () =
              if not from_include then
                check_type_not_shadowing b.tb_name b.tb_loc state
              else Ok ()
            in
            (* Same loading as DType: add alias or opaque to type context *)
            let state =
              match b.tb_body with
              | Some body ->
                  let alias =
                    {
                      alias_params = List.map binder_to_alias_param b.tb_params;
                      alias_body = body;
                    }
                  in
                  add_alias_to_state b.tb_name alias state
              | None ->
                  let opaque =
                    {
                      opaque_params = List.map (fun p -> p.name) b.tb_params;
                      opaque_con = opaque_con_name sig_file.sig_module b.tb_name;
                    }
                  in
                  add_opaque_to_state b.tb_name opaque state
            in
            (* Not exported: no mark_type_imported, even from include *)
            Ok state)
          (Ok state) d.type_bindings
      in
      Ok state

(** {1 Signature Loading}

    Load an entire signature file into a type environment. *)

(** Load a validated signature into a type environment with module resolution.
    Adds all function and variable declarations to the environment. Type aliases
    are expanded and opaque types are resolved during loading. Open directives
    import types only; include directives re-export values. Returns the extended
    environment.

    @param resolver Function to resolve module names to signatures
    @param prelude_ctx Optional prelude type context for implicit prelude types
    @param prelude_type_names
      Optional list of prelude type names (for shadowing check)
    @param env Base type environment to extend
    @param sig_file The signature to load *)
let load_signature_with_resolver ?prelude_ctx ?(prelude_type_names = [])
    ?has_el_file ?source_version ~(resolver : module_resolver)
    (env : Type_env.t) (sig_file : signature) :
    (Type_env.t, load_error) Result.t =
  let type_ctx =
    match prelude_ctx with Some ctx -> ctx | None -> empty_type_context
  in
  let state =
    init_load_state ~type_ctx ~resolver ?has_el_file ?source_version
      ~base_env:env ~module_name:sig_file.sig_module
      ~imported_types:prelude_type_names ()
  in
  let* final_state = load_decls_into_state sig_file state in
  Ok final_state.ls_env

(** Load a validated signature into a type environment. This is the simple
    interface without module resolution. Applies forall inference
    per-declaration during loading. Open and include directives will be ignored
    (no resolver provided). Adds all function and variable declarations to the
    environment. Type aliases are expanded and opaque types are resolved during
    loading. Returns the extended environment. *)
let load_signature (env : Type_env.t) (sig_file : signature) :
    (Type_env.t, load_error) Result.t =
  load_signature_with_resolver ~resolver:no_resolver env sig_file
