(** Signature loader and validator.

    This module converts signature AST to the type environment, validating type
    variable scoping and resolving type references.

    Key validations:
    - Type variables must be explicitly bound in quantifiers
    - Bounded quantifiers must reference valid types
    - Referenced types must be in scope (from opens or type decls) *)

open Sig_ast
module Loc = Syntax.Location
module Types = Core.Types
module Type_env = Core.Type_env

(** {1 Load Errors} *)

type load_error = { message : string; span : Loc.span }
(** Error during signature loading *)

type 'a result = ('a, load_error) Result.t
(** Result type for loading *)

(** Create an error *)
let error message span : 'a result = Error { message; span }

(** {1 Type Variable Context} *)

type tvar_context = {
  bound_vars : string list;  (** Type variables in scope *)
  defined_types : string list;  (** User-defined types in scope *)
}
(** Context for type variable resolution. Tracks which type variables are in
    scope. *)

(** Empty context *)
let empty_context = { bound_vars = []; defined_types = [] }

(** Add bound type variables to context *)
let with_tvars ctx vars = { ctx with bound_vars = vars @ ctx.bound_vars }

(** Add a defined type to context *)
let with_type ctx name = { ctx with defined_types = name :: ctx.defined_types }

(** Check if a name is a type variable in scope *)
let is_tvar ctx name = List.mem name ctx.bound_vars

(** Check if a name is a defined type in scope *)
let is_defined_type ctx name = List.mem name ctx.defined_types

(** {1 Primitive and Built-in Types} *)

(** Check if a name is a primitive type *)
let is_primitive name =
  match name with
  | "int" | "float" | "num" | "string" | "symbol" | "keyword" | "nil" | "t"
  | "bool" | "truthy" | "any" | "never" ->
      true
  | _ -> false

(** {1 Type Validation} *)

(** Validate a sig_type, checking that all type variables are in scope. Returns
    Ok () if valid, Error with the first unbound variable otherwise. *)
let rec validate_type (ctx : tvar_context) (ty : sig_type) : unit result =
  match ty with
  | STVar (name, span) ->
      if is_tvar ctx name then Ok ()
      else if is_primitive name then
        Ok () (* It's actually a primitive, parser misclassified it *)
      else if is_defined_type ctx name then Ok () (* It's a user-defined type *)
      else error (Printf.sprintf "Unbound type variable: %s" name) span
  | STCon (_, _) -> Ok ()
  | STApp (_name, args, _span) ->
      (* Type application: validate all arguments.
         We allow any constructor name since it could be:
         - A builtin (list, option, etc.)
         - A user-defined type
         - A variant constructor in a union type
         The constructor itself will be validated when the signature
         is actually loaded into the type environment. *)
      validate_types ctx args
  | STArrow (params, ret, _) ->
      let* () = validate_params ctx params in
      validate_type ctx ret
  | STForall (binders, body, _) ->
      (* Add binders to context *)
      let var_names = List.map (fun b -> b.name) binders in
      let inner_ctx = with_tvars ctx var_names in
      (* Validate bounds *)
      let* () = validate_binder_bounds ctx binders in
      (* Validate body with extended context *)
      validate_type inner_ctx body
  | STUnion (types, _) -> validate_types ctx types
  | STTuple (types, _) -> validate_types ctx types

and validate_types ctx types =
  List.fold_left
    (fun acc ty ->
      let* () = acc in
      validate_type ctx ty)
    (Ok ()) types

and validate_params ctx params =
  List.fold_left
    (fun acc param ->
      let* () = acc in
      match param with
      | SPPositional ty | SPOptional ty | SPRest ty -> validate_type ctx ty
      | SPKey (_, ty) -> validate_type ctx ty)
    (Ok ()) params

and validate_binder_bounds outer_ctx binders =
  List.fold_left
    (fun acc binder ->
      let* () = acc in
      match binder.bound with
      | None -> Ok ()
      | Some bound_ty -> validate_type outer_ctx bound_ty)
    (Ok ()) binders

(** Result bind operator *)
and ( let* ) = Result.bind

(** {1 Declaration Validation} *)

(** Validate a defun declaration *)
let validate_defun ctx (d : defun_decl) : unit result =
  (* Add type parameters to context *)
  let var_names = List.map (fun b -> b.name) d.defun_tvar_binders in
  let inner_ctx = with_tvars ctx var_names in
  (* Validate binder bounds in outer context *)
  let* () = validate_binder_bounds ctx d.defun_tvar_binders in
  (* Validate params and return type *)
  let* () = validate_params inner_ctx d.defun_params in
  validate_type inner_ctx d.defun_return

(** Validate a defvar declaration *)
let validate_defvar ctx (d : defvar_decl) : unit result =
  validate_type ctx d.defvar_type

(** Validate a type declaration *)
let validate_type_decl ctx (d : type_decl) : unit result =
  (* Add type parameters to context *)
  let var_names = List.map (fun b -> b.name) d.type_params in
  let inner_ctx = with_tvars ctx var_names in
  (* Validate binder bounds in outer context *)
  let* () = validate_binder_bounds ctx d.type_params in
  (* Validate body if present *)
  match d.type_body with
  | None -> Ok () (* Opaque type, no body to validate *)
  | Some body -> validate_type inner_ctx body

(** Validate an import-struct declaration *)
let validate_import_struct ctx (d : import_struct_decl) : unit result =
  List.fold_left
    (fun acc (_, ty) ->
      let* () = acc in
      validate_type ctx ty)
    (Ok ()) d.struct_slots

(** Validate a single declaration *)
let validate_decl ctx (decl : decl) : unit result =
  match decl with
  | DOpen (_, _) -> Ok () (* Opens are handled separately *)
  | DInclude (_, _) -> Ok () (* Includes are handled separately *)
  | DDefun d -> validate_defun ctx d
  | DDefvar d -> validate_defvar ctx d
  | DType d -> validate_type_decl ctx d
  | DImportStruct d -> validate_import_struct ctx d

(** {1 Signature Validation} *)

(** Build context from declarations. Adds all type declarations to the context
    so they can be referenced. *)
let build_context (sig_file : signature) : tvar_context =
  List.fold_left
    (fun ctx decl ->
      match decl with
      | DType d -> with_type ctx d.type_name
      | DImportStruct d -> with_type ctx d.struct_name
      | _ -> ctx)
    empty_context sig_file.sig_decls

(** Validate an entire signature file. Returns Ok () if all declarations are
    valid, or the first error. *)
let validate_signature (sig_file : signature) : unit result =
  let ctx = build_context sig_file in
  List.fold_left
    (fun acc decl ->
      let* () = acc in
      validate_decl ctx decl)
    (Ok ()) sig_file.sig_decls

(** Validate a signature and collect all errors (not just the first). *)
let validate_signature_all (sig_file : signature) : load_error list =
  let ctx = build_context sig_file in
  List.filter_map
    (fun decl ->
      match validate_decl ctx decl with Ok () -> None | Error e -> Some e)
    sig_file.sig_decls

(** {1 Type Alias Context}

    Type aliases map names to their definitions for expansion during loading. *)

type type_alias = {
  alias_params : string list;  (** Type parameters (e.g., [a e] in result) *)
  alias_body : sig_type;  (** The definition body *)
}
(** A type alias definition with optional parameters *)

type alias_context = (string * type_alias) list
(** Context for type alias expansion *)

(** Empty alias context *)
let empty_aliases : alias_context = []

(** Look up a type alias *)
let lookup_alias (name : string) (ctx : alias_context) : type_alias option =
  List.assoc_opt name ctx

(** Add a type alias to the context *)
let add_alias (name : string) (alias : type_alias) (ctx : alias_context) :
    alias_context =
  (name, alias) :: ctx

(** Get all alias names from the context *)
let alias_names (ctx : alias_context) : string list = List.map fst ctx

(** Build alias context from signature declarations. Only includes type
    declarations with bodies (aliases, not opaque types). *)
let build_alias_context (sig_file : signature) : alias_context =
  List.fold_left
    (fun ctx decl ->
      match decl with
      | DType d -> (
          match d.type_body with
          | Some body ->
              let alias =
                {
                  alias_params = List.map (fun b -> b.name) d.type_params;
                  alias_body = body;
                }
              in
              add_alias d.type_name alias ctx
          | None ->
              (* Opaque type - not an alias *)
              ctx)
      | _ -> ctx)
    empty_aliases sig_file.sig_decls

(** {1 Opaque Type Context}

    Opaque types are abstract types with no exposed structure. Each opaque type
    generates a unique type constructor that cannot unify with any other type
    except itself.

    For a module "mymod" with opaque type "buffer", we generate the type
    constructor "mymod/buffer" to ensure uniqueness.

    Opaque types may have phantom type parameters, e.g., [(type tagged [a])]
    creates a type that can be instantiated as [(tagged int)] or
    [(tagged string)], which are distinct. *)

type opaque_type = {
  opaque_params : string list;
      (** Phantom type parameters (e.g., [a] in tagged) *)
  opaque_con : string;  (** The generated type constructor name *)
}
(** An opaque type declaration with its parameters *)

type opaque_context = (string * opaque_type) list
(** Context for opaque type lookup *)

(** Empty opaque context *)
let empty_opaques : opaque_context = []

(** Look up an opaque type *)
let lookup_opaque (name : string) (ctx : opaque_context) : opaque_type option =
  List.assoc_opt name ctx

(** Add an opaque type to the context *)
let add_opaque (name : string) (opaque : opaque_type) (ctx : opaque_context) :
    opaque_context =
  (name, opaque) :: ctx

(** Generate a unique type constructor name for an opaque type. Format:
    module_name/type_name *)
let opaque_con_name (module_name : string) (type_name : string) : string =
  module_name ^ "/" ^ type_name

(** Build opaque context from signature declarations. Only includes type
    declarations without bodies (opaque types). *)
let build_opaque_context (module_name : string) (sig_file : signature) :
    opaque_context =
  List.fold_left
    (fun ctx decl ->
      match decl with
      | DType d -> (
          match d.type_body with
          | None ->
              (* Opaque type - generate unique constructor *)
              let opaque =
                {
                  opaque_params = List.map (fun b -> b.name) d.type_params;
                  opaque_con = opaque_con_name module_name d.type_name;
                }
              in
              add_opaque d.type_name opaque ctx
          | Some _ ->
              (* Type alias - not opaque *)
              ctx)
      | _ -> ctx)
    empty_opaques sig_file.sig_decls

(** {1 Type Conversion}

    These functions convert signature AST types to the core type representation.
    They assume the signature has been validated (type variables are in scope).
*)

(** Canonicalize a type constructor name to match core types convention.
    Signature files use lowercase; core types use capitalized names. *)
let canonicalize_type_name (name : string) : string =
  match name with
  | "list" -> "List"
  | "vector" -> "Vector"
  | "option" -> "Option"
  | "pair" -> "Pair"
  | "hash-table" -> "HashTable"
  | "tuple" -> "Tuple"
  | "ok" -> "ok" (* Variant tags stay as-is *)
  | "err" -> "err" (* Variant tags stay as-is *)
  | _ -> name

(** Convert a signature type name to a primitive type or TCon *)
let sig_name_to_prim (name : string) : Types.typ =
  match name with
  | "int" -> Types.Prim.int
  | "float" -> Types.Prim.float
  | "num" -> Types.Prim.num
  | "string" -> Types.Prim.string
  | "symbol" -> Types.Prim.symbol
  | "keyword" -> Types.Prim.keyword
  | "nil" -> Types.Prim.nil
  | "t" -> Types.Prim.t
  | "bool" -> Types.Prim.bool
  | "truthy" -> Types.Prim.truthy
  | "any" -> Types.Prim.any
  | "never" -> Types.Prim.never
  | _ -> Types.TCon (canonicalize_type_name name)

(** Substitute type variables in a sig_type with other sig_types. Used for
    expanding parameterized type aliases. *)
let rec substitute_sig_type (subst : (string * sig_type) list) (ty : sig_type) :
    sig_type =
  let loc = sig_type_loc ty in
  match ty with
  | STVar (name, _) -> (
      match List.assoc_opt name subst with
      | Some replacement -> replacement
      | None -> ty)
  | STCon (name, _) -> (
      (* Type constants might also need substitution if they're type params *)
      match List.assoc_opt name subst with
      | Some replacement -> replacement
      | None -> ty)
  | STApp (name, args, _) ->
      let args' = List.map (substitute_sig_type subst) args in
      STApp (name, args', loc)
  | STArrow (params, ret, _) ->
      let params' = List.map (substitute_sig_param subst) params in
      let ret' = substitute_sig_type subst ret in
      STArrow (params', ret', loc)
  | STForall (binders, body, _) ->
      (* Remove bound variables from substitution to avoid capture *)
      let bound_names = List.map (fun b -> b.name) binders in
      let subst' =
        List.filter (fun (n, _) -> not (List.mem n bound_names)) subst
      in
      let body' = substitute_sig_type subst' body in
      STForall (binders, body', loc)
  | STUnion (types, _) ->
      let types' = List.map (substitute_sig_type subst) types in
      STUnion (types', loc)
  | STTuple (types, _) ->
      let types' = List.map (substitute_sig_type subst) types in
      STTuple (types', loc)

and substitute_sig_param subst = function
  | SPPositional ty -> SPPositional (substitute_sig_type subst ty)
  | SPOptional ty -> SPOptional (substitute_sig_type subst ty)
  | SPRest ty -> SPRest (substitute_sig_type subst ty)
  | SPKey (name, ty) -> SPKey (name, substitute_sig_type subst ty)

(** {1 Type Context}

    Combined context for type resolution during signature loading. Contains both
    aliases and opaque types. *)

type type_context = { tc_aliases : alias_context; tc_opaques : opaque_context }

let empty_type_context =
  { tc_aliases = empty_aliases; tc_opaques = empty_opaques }

(** Convert a signature type to a core type. [ctx] is the type context
    containing aliases and opaques. [tvar_names] is the list of bound type
    variable names in scope. *)
let rec sig_type_to_typ_with_ctx (ctx : type_context) (tvar_names : string list)
    (ty : sig_type) : Types.typ =
  match ty with
  | STVar (name, _) -> (
      if
        (* Type variable - could be a type variable, alias, or opaque.
         Check for alias/opaque first if not in tvar_names scope. *)
        List.mem name tvar_names
      then
        (* Definitely a bound type variable - keep as TCon for later substitution *)
        Types.TCon name
      else
        match lookup_alias name ctx.tc_aliases with
        | Some alias when alias.alias_params = [] ->
            (* Non-parameterized alias - expand it *)
            sig_type_to_typ_with_ctx ctx tvar_names alias.alias_body
        | _ -> (
            (* Check for opaque type *)
            match lookup_opaque name ctx.tc_opaques with
            | Some opaque when opaque.opaque_params = [] ->
                (* Non-parameterized opaque - use the unique constructor *)
                Types.TCon opaque.opaque_con
            | _ ->
                (* Not an alias/opaque or has parameters - treat as type variable/constructor *)
                Types.TCon name))
  | STCon (name, _) -> (
      (* Type constant - check for alias first, then opaque, then map primitives *)
      match lookup_alias name ctx.tc_aliases with
      | Some alias when alias.alias_params = [] ->
          (* Non-parameterized alias - expand it *)
          sig_type_to_typ_with_ctx ctx tvar_names alias.alias_body
      | _ -> (
          (* Check for opaque type *)
          match lookup_opaque name ctx.tc_opaques with
          | Some opaque when opaque.opaque_params = [] ->
              (* Non-parameterized opaque - use the unique constructor *)
              Types.TCon opaque.opaque_con
          | _ ->
              (* Not an alias/opaque or has parameters (needs args) *)
              sig_name_to_prim name))
  | STApp (name, args, _) -> (
      (* Type application - check for alias expansion first *)
      match lookup_alias name ctx.tc_aliases with
      | Some alias when List.length alias.alias_params = List.length args ->
          (* Parameterized alias - substitute args into body and expand *)
          let subst = List.combine alias.alias_params args in
          let expanded = substitute_sig_type subst alias.alias_body in
          sig_type_to_typ_with_ctx ctx tvar_names expanded
      | _ -> (
          (* Check for opaque type with phantom parameters *)
          match lookup_opaque name ctx.tc_opaques with
          | Some opaque when List.length opaque.opaque_params = List.length args
            ->
              (* Parameterized opaque - create TApp with unique constructor and args *)
              let arg_types =
                List.map (sig_type_to_typ_with_ctx ctx tvar_names) args
              in
              Types.TApp (opaque.opaque_con, arg_types)
          | _ ->
              (* Not an alias/opaque or arity mismatch - treat as type application *)
              let arg_types =
                List.map (sig_type_to_typ_with_ctx ctx tvar_names) args
              in
              Types.TApp (canonicalize_type_name name, arg_types)))
  | STArrow (params, ret, _) ->
      (* Function type *)
      let param_types =
        List.map (sig_param_to_param_with_ctx ctx tvar_names) params
      in
      let ret_type = sig_type_to_typ_with_ctx ctx tvar_names ret in
      Types.TArrow (param_types, ret_type)
  | STForall (binders, body, _) ->
      (* Polymorphic type - add binders to scope *)
      let new_vars = List.map (fun b -> b.name) binders in
      let inner_names = new_vars @ tvar_names in
      let body_type = sig_type_to_typ_with_ctx ctx inner_names body in
      Types.TForall (new_vars, body_type)
  | STUnion (types, _) ->
      (* Union type *)
      let type_list =
        List.map (sig_type_to_typ_with_ctx ctx tvar_names) types
      in
      Types.TUnion type_list
  | STTuple (types, _) ->
      (* Tuple type *)
      let type_list =
        List.map (sig_type_to_typ_with_ctx ctx tvar_names) types
      in
      Types.TTuple type_list

(** Convert a signature parameter to a core parameter with type context *)
and sig_param_to_param_with_ctx (ctx : type_context) (tvar_names : string list)
    (p : sig_param) : Types.param =
  match p with
  | SPPositional ty ->
      Types.PPositional (sig_type_to_typ_with_ctx ctx tvar_names ty)
  | SPOptional ty ->
      Types.POptional (sig_type_to_typ_with_ctx ctx tvar_names ty)
  | SPRest ty -> Types.PRest (sig_type_to_typ_with_ctx ctx tvar_names ty)
  | SPKey (name, ty) ->
      Types.PKey (name, sig_type_to_typ_with_ctx ctx tvar_names ty)

(** Convert a signature type to a core type with alias expansion. [aliases] is
    the alias context for expansion. [tvar_names] is the list of bound type
    variable names in scope. *)
let sig_type_to_typ_with_aliases (aliases : alias_context)
    (tvar_names : string list) (ty : sig_type) : Types.typ =
  let ctx = { tc_aliases = aliases; tc_opaques = empty_opaques } in
  sig_type_to_typ_with_ctx ctx tvar_names ty

(** Convert a signature parameter to a core parameter with alias expansion *)
let sig_param_to_param_with_aliases (aliases : alias_context)
    (tvar_names : string list) (p : sig_param) : Types.param =
  let ctx = { tc_aliases = aliases; tc_opaques = empty_opaques } in
  sig_param_to_param_with_ctx ctx tvar_names p

(** Convert a signature type to a core type (without alias expansion).
    [tvar_names] is the list of bound type variable names in scope. *)
let sig_type_to_typ (tvar_names : string list) (ty : sig_type) : Types.typ =
  sig_type_to_typ_with_aliases empty_aliases tvar_names ty

(** Convert a signature parameter to a core parameter *)
let sig_param_to_param (tvar_names : string list) (p : sig_param) : Types.param
    =
  sig_param_to_param_with_aliases empty_aliases tvar_names p

(** {1 Declaration Loading}

    These functions convert signature declarations to type environment entries.
*)

(** Convert a defun declaration to a type scheme with full type context. Returns
    a Poly scheme if the function has type parameters, otherwise a Mono scheme
    with an arrow type. *)
let load_defun_with_ctx (ctx : type_context) (d : defun_decl) : Type_env.scheme
    =
  let tvar_names = List.map (fun b -> b.name) d.defun_tvar_binders in
  let params =
    List.map (sig_param_to_param_with_ctx ctx tvar_names) d.defun_params
  in
  let ret = sig_type_to_typ_with_ctx ctx tvar_names d.defun_return in
  let arrow = Types.TArrow (params, ret) in
  if tvar_names = [] then Type_env.Mono arrow
  else Type_env.Poly (tvar_names, arrow)

(** Convert a defvar declaration to a type scheme with full type context. The
    type may be polymorphic if it contains a forall. *)
let load_defvar_with_ctx (ctx : type_context) (d : defvar_decl) :
    Type_env.scheme =
  match d.defvar_type with
  | STForall (binders, body, _) ->
      (* Polymorphic variable - extract quantifiers *)
      let tvar_names = List.map (fun b -> b.name) binders in
      let body_type = sig_type_to_typ_with_ctx ctx tvar_names body in
      Type_env.Poly (tvar_names, body_type)
  | _ ->
      (* Monomorphic variable *)
      let ty = sig_type_to_typ_with_ctx ctx [] d.defvar_type in
      Type_env.Mono ty

(** Convert a defun declaration to a type scheme (without alias/opaque
    expansion). *)
let load_defun (d : defun_decl) : Type_env.scheme =
  load_defun_with_ctx empty_type_context d

(** Convert a defvar declaration to a type scheme (without alias/opaque
    expansion). *)
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
  ls_loaded : string list;  (** Modules already loaded (cycle detection) *)
}
(** State accumulated during loading *)

(** Create initial load state *)
let init_load_state ~resolver ~base_env ~module_name =
  {
    ls_type_ctx = empty_type_context;
    ls_env = base_env;
    ls_resolver = resolver;
    ls_loaded = [ module_name ];
    (* Mark self as loaded to prevent self-import *)
  }

(** Add a type alias to the load state *)
let add_alias_to_state name alias state =
  let aliases = add_alias name alias state.ls_type_ctx.tc_aliases in
  { state with ls_type_ctx = { state.ls_type_ctx with tc_aliases = aliases } }

(** Add an opaque type to the load state *)
let add_opaque_to_state name opaque state =
  let opaques = add_opaque name opaque state.ls_type_ctx.tc_opaques in
  { state with ls_type_ctx = { state.ls_type_ctx with tc_opaques = opaques } }

(** Add a value binding to the load state *)
let add_value_to_state name scheme state =
  { state with ls_env = Type_env.extend name scheme state.ls_env }

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

(** {1 Open and Include Processing}

    - open: Import types for use in signatures (not re-exported to value env)
    - include: Inline all declarations (types available AND values re-exported)
*)

(** Get the names of all known types from the current state. This is used to
    avoid inferring type constructors as type variables. *)
let get_known_types_from_state (state : load_state) : string list =
  let alias_names = List.map fst state.ls_type_ctx.tc_aliases in
  let opaque_names = List.map fst state.ls_type_ctx.tc_opaques in
  alias_names @ opaque_names

(** Process an 'open' directive. Imports types from another module for use in
    type expressions. Types are NOT re-exported; they are only available for
    signature writing.

    R12: "seq is available for use in type expressions" "seq is NOT re-exported
    from my-collection" *)
let process_open (module_name : string) (state : load_state) : load_state =
  if List.mem module_name state.ls_loaded then
    (* Already loaded - skip to prevent cycles *)
    state
  else
    match state.ls_resolver module_name with
    | None ->
        (* Module not found - for now, silently skip.
           Later we can add proper error reporting. *)
        state
    | Some opened_sig ->
        (* Mark as loaded *)
        let state = { state with ls_loaded = module_name :: state.ls_loaded } in
        (* Import type aliases from opened module *)
        let aliases = build_alias_context opened_sig in
        let state =
          List.fold_left
            (fun s (name, alias) -> add_alias_to_state name alias s)
            state aliases
        in
        (* Import opaque types from opened module *)
        let opaques = build_opaque_context opened_sig.sig_module opened_sig in
        let state =
          List.fold_left
            (fun s (name, opaque) -> add_opaque_to_state name opaque s)
            state opaques
        in
        (* Note: we do NOT add values to ls_env - open only imports types *)
        state

(** Process an 'include' directive. Imports all declarations from another module
    and re-exports them. Both types AND values become part of this module's
    interface.

    R13: "all declarations from seq.tart are part of my-extended-seq's
    interface" "types from seq are available for use in signatures" *)
let rec process_include (module_name : string) (state : load_state) : load_state
    =
  if List.mem module_name state.ls_loaded then
    (* Already loaded - skip to prevent cycles *)
    state
  else
    match state.ls_resolver module_name with
    | None ->
        (* Module not found - for now, silently skip *)
        state
    | Some included_sig ->
        (* Mark as loaded *)
        let state = { state with ls_loaded = module_name :: state.ls_loaded } in
        (* Process the included signature's declarations recursively *)
        load_decls_into_state included_sig state

(** Load declarations from a signature into the load state. Handles all
    declaration types including open/include recursively. Forall inference is
    applied per-declaration using the accumulated type context, so types from
    includes/opens are known. *)
and load_decls_into_state (sig_file : signature) (state : load_state) :
    load_state =
  List.fold_left
    (fun state decl ->
      match decl with
      | DOpen (name, _) -> process_open name state
      | DInclude (name, _) -> process_include name state
      | DType d -> (
          (* Apply inference to type declaration using current known types *)
          let known_types = get_known_types_from_state state in
          let d = Forall_infer.infer_type_decl ~known_types d in
          (* Add to type context *)
          match d.type_body with
          | Some body ->
              (* Type alias *)
              let alias =
                {
                  alias_params = List.map (fun b -> b.name) d.type_params;
                  alias_body = body;
                }
              in
              add_alias_to_state d.type_name alias state
          | None ->
              (* Opaque type - use the original module name for the constructor *)
              let opaque =
                {
                  opaque_params = List.map (fun b -> b.name) d.type_params;
                  opaque_con = opaque_con_name sig_file.sig_module d.type_name;
                }
              in
              add_opaque_to_state d.type_name opaque state)
      | DDefun d ->
          (* Apply inference to defun using current known types *)
          let known_types = get_known_types_from_state state in
          let d = Forall_infer.infer_defun ~known_types d in
          let scheme = load_defun_with_ctx state.ls_type_ctx d in
          add_value_to_state d.defun_name scheme state
      | DDefvar d ->
          let scheme = load_defvar_with_ctx state.ls_type_ctx d in
          add_value_to_state d.defvar_name scheme state
      | DImportStruct d -> load_import_struct sig_file.sig_module d state)
    state sig_file.sig_decls

(** {1 Signature Loading}

    Load an entire signature file into a type environment. *)

(** Load a validated signature into a type environment with module resolution.
    Applies forall inference per-declaration during loading, using accumulated
    type context so that types from includes/opens are recognized. Adds all
    function and variable declarations to the environment. Type aliases are
    expanded and opaque types are resolved during loading. Open directives
    import types only; include directives re-export values. Returns the extended
    environment.

    @param resolver Function to resolve module names to signatures
    @param env Base type environment to extend
    @param sig_file The signature to load *)
let load_signature_with_resolver ~(resolver : module_resolver)
    (env : Type_env.t) (sig_file : signature) : Type_env.t =
  let state =
    init_load_state ~resolver ~base_env:env ~module_name:sig_file.sig_module
  in
  let final_state = load_decls_into_state sig_file state in
  final_state.ls_env

(** Load a validated signature into a type environment. This is the simple
    interface without module resolution. Applies forall inference
    per-declaration during loading. Open and include directives will be ignored
    (no resolver provided). Adds all function and variable declarations to the
    environment. Type aliases are expanded and opaque types are resolved during
    loading. Returns the extended environment. *)
let load_signature (env : Type_env.t) (sig_file : signature) : Type_env.t =
  load_signature_with_resolver ~resolver:no_resolver env sig_file
