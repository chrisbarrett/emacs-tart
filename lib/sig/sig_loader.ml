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

(** Check if a name is a primitive type.

    This now only checks for intrinsic names (with %tart-intrinsic% prefix).
    User-facing names like "int", "string" etc. are handled by the prelude. *)
let is_primitive name =
  (* Intrinsic names are always recognized as primitives *)
  Types.is_intrinsic_name name

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
  | STSubtract (minuend, subtrahend, _) ->
      let* () = validate_type ctx minuend in
      validate_type ctx subtrahend
  | STRow (row, _) ->
      let field_types = List.map snd row.srow_fields in
      validate_types ctx field_types
  | STPredicate (_, narrowed_type, _) ->
      (* Validate the narrowed type *)
      validate_type ctx narrowed_type

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
      | SPPositional (_, ty) | SPOptional (_, ty) | SPRest ty ->
          validate_type ctx ty
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

(** Validate a constructor declaration *)
let validate_ctor ctx (ctor : ctor_decl) : unit result =
  List.fold_left
    (fun acc field_ty ->
      let* () = acc in
      validate_type ctx field_ty)
    (Ok ()) ctor.ctor_fields

(** Validate a data declaration *)
let validate_data ctx (d : data_decl) : unit result =
  (* Add type parameters to context *)
  let param_names = List.map (fun p -> p.name) d.data_params in
  let ctx = with_tvars ctx param_names in
  (* Validate each constructor *)
  List.fold_left
    (fun acc ctor ->
      let* () = acc in
      validate_ctor ctx ctor)
    (Ok ()) d.data_ctors

(** Validate a single declaration *)
let rec validate_decl ctx (decl : decl) : unit result =
  match decl with
  | DOpen (_, _) -> Ok () (* Opens are handled separately *)
  | DInclude (_, _) -> Ok () (* Includes are handled separately *)
  | DDefun d -> validate_defun ctx d
  | DDefvar d -> validate_defvar ctx d
  | DType d -> validate_type_decl ctx d
  | DImportStruct d -> validate_import_struct ctx d
  | DData d -> validate_data ctx d
  | DTypeScope d ->
      (* Add scope type variables to context, then validate inner decls *)
      let scope_var_names = List.map (fun b -> b.name) d.scope_tvar_binders in
      let scope_ctx = with_tvars ctx scope_var_names in
      List.fold_left
        (fun acc inner_decl ->
          let* () = acc in
          validate_decl scope_ctx inner_decl)
        (Ok ()) d.scope_decls

(** {1 Signature Validation} *)

(** Build context from declarations. Adds all type declarations to the context
    so they can be referenced. *)
let build_context (sig_file : signature) : tvar_context =
  let rec add_decl_types ctx decl =
    match decl with
    | DType d -> with_type ctx d.type_name
    | DImportStruct d -> with_type ctx d.struct_name
    | DData d -> with_type ctx d.data_name
    | DTypeScope d ->
        (* Recursively collect types from scope declarations *)
        List.fold_left add_decl_types ctx d.scope_decls
    | _ -> ctx
  in
  List.fold_left add_decl_types empty_context sig_file.sig_decls

(** Validate an entire signature file. Returns Ok () if all declarations are
    valid, or the first error.

    @param prelude_type_names
      Optional list of type names from the prelude that should be considered
      valid. This allows signatures to reference prelude types like buffer,
      window, etc. without declaring them locally. *)
let validate_signature ?(prelude_type_names = []) (sig_file : signature) :
    unit result =
  let base_ctx = build_context sig_file in
  let ctx =
    List.fold_left (fun c name -> with_type c name) base_ctx prelude_type_names
  in
  List.fold_left
    (fun acc decl ->
      let* () = acc in
      validate_decl ctx decl)
    (Ok ()) sig_file.sig_decls

(** Validate a signature and collect all errors (not just the first).

    @param prelude_type_names
      Optional list of type names from the prelude that should be considered
      valid. *)
let validate_signature_all ?(prelude_type_names = []) (sig_file : signature) :
    load_error list =
  let base_ctx = build_context sig_file in
  let ctx =
    List.fold_left (fun c name -> with_type c name) base_ctx prelude_type_names
  in
  List.filter_map
    (fun decl ->
      match validate_decl ctx decl with Ok () -> None | Error e -> Some e)
    sig_file.sig_decls

(** {1 Type Alias Context}

    Type aliases map names to their definitions for expansion during loading. *)

type alias_param = {
  ap_name : string;  (** Parameter name (e.g., "a") *)
  ap_bound : sig_type option;  (** Upper bound (e.g., truthy) *)
}
(** A type parameter with optional bound *)

type type_alias = {
  alias_params : alias_param list;  (** Type parameters with optional bounds *)
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

(** Convert a tvar_binder to an alias_param *)
let binder_to_alias_param (b : tvar_binder) : alias_param =
  { ap_name = b.name; ap_bound = b.bound }

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
                  alias_params = List.map binder_to_alias_param d.type_params;
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

(** Canonicalize a type constructor name.

    This handles: 1. Intrinsic names are passed through unchanged 2. Container
    type names map to their intrinsic equivalents 3. Other names pass through
    unchanged

    Note: The prelude handles user-facing names like "list" via aliases. This
    function is for backwards compatibility with any remaining hardcoded
    references. *)
let canonicalize_type_name (name : string) : string =
  if Types.is_intrinsic_name name then name
  else
    match name with
    (* Container types map to intrinsics *)
    | "List" -> Types.intrinsic "List"
    | "Vector" -> Types.intrinsic "Vector"
    | "Pair" -> Types.intrinsic "Pair"
    | "HashTable" -> Types.intrinsic "HashTable"
    (* Lowercase container names also map to intrinsics *)
    | "list" -> Types.intrinsic "List"
    | "vector" -> Types.intrinsic "Vector"
    | "cons" -> Types.intrinsic "Pair"
    | "hash-table" -> Types.intrinsic "HashTable"
    | "tuple" ->
        "Tuple" (* Tuple doesn't need intrinsic prefix - it's structural *)
    (* Variant tags stay as-is *)
    | "ok" -> "ok"
    | "err" -> "err"
    | _ -> name

(** Convert a signature type name to a type.

    Intrinsic names (with %tart-intrinsic% prefix) are passed through directly.
    Other names are canonicalized (e.g., "list" -> "List" for backwards
    compatibility with any hardcoded type names).

    Note: User-facing primitive names like "int", "string" are now handled by
    the prelude, not hardcoded here. This function is only called when a name is
    not found in the alias context. *)
let sig_name_to_prim (name : string) : Types.typ =
  if Types.is_intrinsic_name name then
    (* Intrinsic types pass through directly *)
    Types.TCon name
  else
    (* Non-intrinsic names get canonicalized *)
    Types.TCon (canonicalize_type_name name)

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
  | STSubtract (minuend, subtrahend, _) ->
      let minuend' = substitute_sig_type subst minuend in
      let subtrahend' = substitute_sig_type subst subtrahend in
      STSubtract (minuend', subtrahend', loc)
  | STRow (row, _) ->
      let fields' =
        List.map
          (fun (name, ty) -> (name, substitute_sig_type subst ty))
          row.srow_fields
      in
      STRow ({ row with srow_fields = fields' }, loc)
  | STPredicate (param_name, narrowed_type, _) ->
      let narrowed_type' = substitute_sig_type subst narrowed_type in
      STPredicate (param_name, narrowed_type', loc)

and substitute_sig_param subst = function
  | SPPositional (name, ty) -> SPPositional (name, substitute_sig_type subst ty)
  | SPOptional (name, ty) -> SPOptional (name, substitute_sig_type subst ty)
  | SPRest ty -> SPRest (substitute_sig_type subst ty)
  | SPKey (name, ty) -> SPKey (name, substitute_sig_type subst ty)

(** {1 Type Context}

    Combined context for type resolution during signature loading. Contains both
    aliases and opaque types. *)

type type_context = { tc_aliases : alias_context; tc_opaques : opaque_context }

let empty_type_context =
  { tc_aliases = empty_aliases; tc_opaques = empty_opaques }

(** Simple structural type equality for bound checking.

    Compares type constructors by name. Sufficient for checking whether a
    concrete type is a member of a union bound like eq-safe. *)
let rec bound_types_equal (t1 : Types.typ) (t2 : Types.typ) : bool =
  match (t1, t2) with
  | Types.TCon n1, Types.TCon n2 -> n1 = n2
  | Types.TApp (con1, args1), Types.TApp (con2, args2) ->
      bound_types_equal con1 con2
      && List.length args1 = List.length args2
      && List.for_all2 bound_types_equal args1 args2
  | Types.TUnion m1, Types.TUnion m2 ->
      List.length m1 = List.length m2 && List.for_all2 bound_types_equal m1 m2
  | _ -> false

(** Check if a type is a member of a union.

    A type [t] is a member of a union if it is structurally equal to one of the
    union's members. For example, [symbol] is a member of
    [(symbol | keyword | int | t | nil)]. *)
let is_union_member (arg_typ : Types.typ) (members : Types.typ list) : bool =
  let arg = Types.repr arg_typ in
  List.exists (fun member -> bound_types_equal arg (Types.repr member)) members

(** Check if a type satisfies a bound constraint.

    Supports:
    - truthy bound: the argument type must not contain nil
    - union bounds: the argument type must be a member of the union (e.g.,
      eq-safe = symbol | keyword | int | t | nil)

    This is a simplified subtype check for bound constraints. *)
let satisfies_bound (arg_typ : Types.typ) (bound_typ : Types.typ) : bool =
  let bound_typ = Types.repr bound_typ in
  match bound_typ with
  | Types.TCon name when name = Types.intrinsic "Truthy" ->
      (* Truthy bound: arg must not contain nil *)
      Types.is_truthy arg_typ
  | Types.TUnion members -> (
      (* Union bound: arg must be one of the union members.
         Also accept if arg itself is a union where all members satisfy the bound.
         E.g., (symbol | int) satisfies eq-safe since both are members. *)
      let arg = Types.repr arg_typ in
      match arg with
      | Types.TUnion arg_members ->
          List.for_all (fun m -> is_union_member m members) arg_members
      | _ -> is_union_member arg_typ members)
  | _ ->
      (* For other bounds, check structural equality *)
      bound_types_equal (Types.repr arg_typ) bound_typ

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
                (* Not an alias/opaque or has parameters - treat as type constant.
                   Use sig_name_to_prim to handle primitive names and canonicalization
                   (e.g., "list" -> "List" for HK type arguments). *)
                sig_name_to_prim name))
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
  | STApp (name, args, _span) -> (
      (* Type application - check for alias expansion first *)
      match lookup_alias name ctx.tc_aliases with
      | Some alias when List.length alias.alias_params = List.length args ->
          (* Parameterized alias - check bounds and substitute args into body *)
          (* First, check bounds for each argument *)
          List.iter2
            (fun param arg ->
              match param.ap_bound with
              | None -> () (* No bound - any type is acceptable *)
              | Some bound_sig_type ->
                  (* Convert the argument to a core type for checking *)
                  let arg_typ = sig_type_to_typ_with_ctx ctx tvar_names arg in
                  (* Convert the bound to a core type *)
                  let bound_typ =
                    sig_type_to_typ_with_ctx ctx tvar_names bound_sig_type
                  in
                  (* Check that arg satisfies the bound (arg <: bound).
                     For truthy bound, arg must be truthy. *)
                  if not (satisfies_bound arg_typ bound_typ) then
                    failwith
                      (Printf.sprintf
                         "Bound violation in %s: %s is not a subtype of %s" name
                         (Types.to_string arg_typ)
                         (Types.to_string bound_typ)))
            alias.alias_params args;
          (* Substitute args into body and expand *)
          let subst =
            List.combine (List.map (fun p -> p.ap_name) alias.alias_params) args
          in
          let expanded = substitute_sig_type subst alias.alias_body in
          sig_type_to_typ_with_ctx ctx tvar_names expanded
      | _ -> (
          (* Special case: map types with a single row argument (Design B).
             (alist {name string & r}) expands to (list (cons symbol {name string & r}))
             (plist {:name string & r}) expands to (list (keyword | {name string & r}))
             (hash-table {name string & r}) expands to (HashTable symbol {name string & r})
             This preserves field names for static key lookup while maintaining
             structural compatibility with homogeneous map types. *)
          match (name, args) with
          | "alist", [ single_arg ] -> (
              let arg_typ =
                sig_type_to_typ_with_ctx ctx tvar_names single_arg
              in
              match Types.repr arg_typ with
              | TRow _ ->
                  Types.list_of (Types.pair_of Types.Prim.symbol arg_typ)
              | _ ->
                  Types.TApp
                    (Types.TCon (canonicalize_type_name name), [ arg_typ ]))
          | "plist", [ single_arg ] -> (
              let arg_typ =
                sig_type_to_typ_with_ctx ctx tvar_names single_arg
              in
              match Types.repr arg_typ with
              | TRow _ -> Types.list_of (TUnion [ Types.Prim.keyword; arg_typ ])
              | _ ->
                  Types.TApp
                    (Types.TCon (canonicalize_type_name name), [ arg_typ ]))
          | "hash-table", [ single_arg ] -> (
              let arg_typ =
                sig_type_to_typ_with_ctx ctx tvar_names single_arg
              in
              match Types.repr arg_typ with
              | TRow _ -> Types.hash_table_of Types.Prim.symbol arg_typ
              | _ ->
                  Types.TApp
                    (Types.TCon (canonicalize_type_name name), [ arg_typ ]))
          | _ -> (
              (* Check for opaque type with phantom parameters *)
              match lookup_opaque name ctx.tc_opaques with
              | Some opaque
                when List.length opaque.opaque_params = List.length args ->
                  (* Parameterized opaque - create TApp with unique constructor and args *)
                  let arg_types =
                    List.map (sig_type_to_typ_with_ctx ctx tvar_names) args
                  in
                  Types.TApp (Types.TCon opaque.opaque_con, arg_types)
              | _ ->
                  (* Not an alias/opaque or arity mismatch - treat as type application.
                     If name is a type variable, keep it as TCon for later substitution
                     during instantiation. This enables higher-kinded types. *)
                  let arg_types =
                    List.map (sig_type_to_typ_with_ctx ctx tvar_names) args
                  in
                  Types.TApp
                    (Types.TCon (canonicalize_type_name name), arg_types))))
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
  | STSubtract (minuend, subtrahend, _) ->
      (* Type subtraction: remove subtrahend from minuend's union.
         E.g., ((int | string) - int) => string
         E.g., ((truthy | nil) - nil) => truthy *)
      let minuend_type = sig_type_to_typ_with_ctx ctx tvar_names minuend in
      let subtrahend_type =
        sig_type_to_typ_with_ctx ctx tvar_names subtrahend
      in
      subtract_type minuend_type subtrahend_type
  | STRow (row, _) ->
      (* Convert sig_row to core TRow *)
      let fields =
        List.map
          (fun (name, ty) -> (name, sig_type_to_typ_with_ctx ctx tvar_names ty))
          row.srow_fields
      in
      let row_var =
        match row.srow_var with
        | None -> None
        | Some var_name -> Some (Types.TCon var_name)
      in
      Types.TRow { row_fields = fields; row_var }
  | STPredicate (_, narrowed_type, _) ->
      (* For now, predicate types are just bool at the type level.
         The predicate information will be tracked separately in the type environment. *)
      let _ = sig_type_to_typ_with_ctx ctx tvar_names narrowed_type in
      Types.Prim.bool

(** Subtract a type from another type.

    For union types, removes all occurrences of the subtrahend from the union.
    For non-union types, returns the minuend unchanged if it's not equal to the
    subtrahend, otherwise returns an empty union (TUnion []).

    Examples:
    - (int | string) - int => string
    - (truthy | nil) - nil => truthy
    - (cons a (list a)) | nil) - nil => (cons a (list a)) *)
and subtract_type (minuend : Types.typ) (subtrahend : Types.typ) : Types.typ =
  let minuend = Types.repr minuend in
  let subtrahend = Types.repr subtrahend in
  match minuend with
  | Types.TUnion members -> (
      (* Filter out members that match the subtrahend *)
      let remaining =
        List.filter
          (fun m -> not (types_equal (Types.repr m) subtrahend))
          members
      in
      match remaining with
      | [] -> Types.TUnion [] (* Empty type - all members removed *)
      | [ single ] -> single (* Single type - unwrap union *)
      | _ -> Types.TUnion remaining)
  | _ ->
      (* Non-union type - return unchanged if not equal to subtrahend *)
      if types_equal minuend subtrahend then Types.TUnion [] (* Empty type *)
      else minuend

(** Check if two types are structurally equal (for subtraction purposes). This
    is a simple structural equality, not a full unification. *)
and types_equal (t1 : Types.typ) (t2 : Types.typ) : bool =
  match (t1, t2) with
  | Types.TCon n1, Types.TCon n2 -> n1 = n2
  | Types.TVar tv1, Types.TVar tv2 -> tv1 == tv2 (* Physical equality *)
  | Types.TApp (con1, args1), Types.TApp (con2, args2) ->
      types_equal con1 con2
      && List.length args1 = List.length args2
      && List.for_all2 types_equal args1 args2
  | Types.TArrow (params1, ret1), Types.TArrow (params2, ret2) ->
      List.length params1 = List.length params2
      && List.for_all2 params_equal params1 params2
      && types_equal ret1 ret2
  | Types.TForall (vars1, body1), Types.TForall (vars2, body2) ->
      vars1 = vars2 && types_equal body1 body2
  | Types.TUnion members1, Types.TUnion members2 ->
      List.length members1 = List.length members2
      && List.for_all2 types_equal members1 members2
  | Types.TTuple elems1, Types.TTuple elems2 ->
      List.length elems1 = List.length elems2
      && List.for_all2 types_equal elems1 elems2
  | _ -> false

and params_equal (p1 : Types.param) (p2 : Types.param) : bool =
  match (p1, p2) with
  | Types.PPositional t1, Types.PPositional t2 -> types_equal t1 t2
  | Types.POptional t1, Types.POptional t2 -> types_equal t1 t2
  | Types.PRest t1, Types.PRest t2 -> types_equal t1 t2
  | Types.PKey (n1, t1), Types.PKey (n2, t2) -> n1 = n2 && types_equal t1 t2
  | _ -> false

(** Convert a signature parameter to a core parameter with type context *)
and sig_param_to_param_with_ctx (ctx : type_context) (tvar_names : string list)
    (p : sig_param) : Types.param =
  match p with
  | SPPositional (_, ty) ->
      Types.PPositional (sig_type_to_typ_with_ctx ctx tvar_names ty)
  | SPOptional (_, ty) ->
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

(** {1 Scope Type Variable Conversion}

    For type-scope blocks, we need to use pre-created TVars for scope type
    variables so they are shared across all declarations in the scope. *)

(** Convert a signature type to a core type, using pre-created TVars for scope
    type variables. [scope_tvars] maps scope type var names to their shared
    TVars. [tvar_names] is the list of function-local type variable names. *)
let rec sig_type_to_typ_with_scope_ctx (ctx : type_context)
    (scope_tvars : (string * Types.typ) list) (tvar_names : string list)
    (ty : sig_type) : Types.typ =
  match ty with
  | STVar (name, _) -> (
      (* First check if it's a scope type variable *)
      match List.assoc_opt name scope_tvars with
      | Some tvar -> tvar (* Use the pre-created TVar *)
      | None ->
          if List.mem name tvar_names then
            (* Function-local type variable *)
            Types.TCon name
          else
            (* Fall through to alias/opaque/primitive handling *)
            sig_type_to_typ_with_ctx ctx
              (List.map fst scope_tvars @ tvar_names)
              ty)
  | STCon (name, _) -> (
      (* Check scope tvars first (in case someone uses a scope var as a type name) *)
      match List.assoc_opt name scope_tvars with
      | Some tvar -> tvar
      | None ->
          sig_type_to_typ_with_ctx ctx
            (List.map fst scope_tvars @ tvar_names)
            ty)
  | STApp (name, args, loc) -> (
      (* Check if the constructor is a scope tvar *)
      match List.assoc_opt name scope_tvars with
      | Some tvar ->
          (* HK scope variable - convert args and create TApp *)
          let arg_types =
            List.map
              (sig_type_to_typ_with_scope_ctx ctx scope_tvars tvar_names)
              args
          in
          Types.TApp (tvar, arg_types)
      | None ->
          (* Not a scope tvar - use regular conversion *)
          let combined_tvars = List.map fst scope_tvars @ tvar_names in
          sig_type_to_typ_with_ctx ctx combined_tvars (STApp (name, args, loc)))
  | STArrow (params, ret, _) ->
      let param_types =
        List.map
          (sig_param_to_param_with_scope_ctx ctx scope_tvars tvar_names)
          params
      in
      let ret_type =
        sig_type_to_typ_with_scope_ctx ctx scope_tvars tvar_names ret
      in
      Types.TArrow (param_types, ret_type)
  | STForall (binders, body, _) ->
      let new_vars = List.map (fun b -> b.name) binders in
      let inner_names = new_vars @ tvar_names in
      let body_type =
        sig_type_to_typ_with_scope_ctx ctx scope_tvars inner_names body
      in
      Types.TForall (new_vars, body_type)
  | STUnion (types, _) ->
      let type_list =
        List.map
          (sig_type_to_typ_with_scope_ctx ctx scope_tvars tvar_names)
          types
      in
      Types.TUnion type_list
  | STTuple (types, _) ->
      let type_list =
        List.map
          (sig_type_to_typ_with_scope_ctx ctx scope_tvars tvar_names)
          types
      in
      Types.TTuple type_list
  | STSubtract (minuend, subtrahend, _) ->
      let minuend_type =
        sig_type_to_typ_with_scope_ctx ctx scope_tvars tvar_names minuend
      in
      let subtrahend_type =
        sig_type_to_typ_with_scope_ctx ctx scope_tvars tvar_names subtrahend
      in
      subtract_type minuend_type subtrahend_type
  | STRow (row, _) ->
      (* Convert sig_row to core TRow with scope type variables *)
      let fields =
        List.map
          (fun (name, ty) ->
            (name, sig_type_to_typ_with_scope_ctx ctx scope_tvars tvar_names ty))
          row.srow_fields
      in
      let row_var =
        match row.srow_var with
        | None -> None
        | Some var_name -> (
            (* Check if the row variable is a scope type variable *)
            match List.assoc_opt var_name scope_tvars with
            | Some tvar -> Some tvar
            | None -> Some (Types.TCon var_name))
      in
      Types.TRow { row_fields = fields; row_var }
  | STPredicate (_, narrowed_type, _) ->
      (* For now, predicate types are just bool at the type level.
         The predicate information will be tracked separately in the type environment. *)
      let _ =
        sig_type_to_typ_with_scope_ctx ctx scope_tvars tvar_names narrowed_type
      in
      Types.Prim.bool

(** Convert a signature parameter to a core parameter with scope type variables
*)
and sig_param_to_param_with_scope_ctx (ctx : type_context)
    (scope_tvars : (string * Types.typ) list) (tvar_names : string list)
    (p : sig_param) : Types.param =
  match p with
  | SPPositional (_, ty) ->
      Types.PPositional
        (sig_type_to_typ_with_scope_ctx ctx scope_tvars tvar_names ty)
  | SPOptional (_, ty) ->
      Types.POptional
        (sig_type_to_typ_with_scope_ctx ctx scope_tvars tvar_names ty)
  | SPRest ty ->
      Types.PRest (sig_type_to_typ_with_scope_ctx ctx scope_tvars tvar_names ty)
  | SPKey (name, ty) ->
      Types.PKey
        (name, sig_type_to_typ_with_scope_ctx ctx scope_tvars tvar_names ty)

(** {1 Declaration Loading}

    These functions convert signature declarations to type environment entries.
*)

(** Extract predicate info from a defun declaration if it has a predicate return
    type.

    Returns [Some info] if the return type is [STPredicate], where info contains
    the parameter index that gets narrowed. Returns [None] otherwise. *)
let extract_predicate_info (ctx : type_context) (d : defun_decl) :
    Type_env.predicate_info option =
  match d.defun_return with
  | STPredicate (param_name, narrowed_sig_type, _) -> (
      (* Find the parameter index that matches the predicate's param_name *)
      let find_param_index () =
        let rec find_in_params idx = function
          | [] -> None
          | SPPositional (Some name, _) :: _ when name = param_name -> Some idx
          | SPOptional (Some name, _) :: _ when name = param_name -> Some idx
          | SPKey (name, _) :: _ when name = param_name -> Some idx
          | _ :: rest -> find_in_params (idx + 1) rest
        in
        find_in_params 0 d.defun_params
      in
      match find_param_index () with
      | None ->
          (* Parameter name not found - validation error would be reported
              elsewhere *)
          None
      | Some param_index ->
          let tvar_names = List.map (fun b -> b.name) d.defun_tvar_binders in
          let narrowed_type =
            sig_type_to_typ_with_ctx ctx tvar_names narrowed_sig_type
          in
          Some { Type_env.param_index; param_name; narrowed_type })
  | _ -> None

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

(** Convert a defun declaration inside a type-scope to a type scheme. The scope
    type variables are added to the function's quantifier list. Uses the
    pre-created TVars from the scope for consistency across declarations. *)
let load_defun_with_scope (ctx : type_context)
    (scope_tvars : (string * Types.typ) list) (d : defun_decl) : Type_env.scheme
    =
  (* Get function's own type variables *)
  let fn_tvar_names = List.map (fun b -> b.name) d.defun_tvar_binders in
  (* Scope type variable names that are actually used in this function
     (we include all scope vars in the quantifier for consistency) *)
  let scope_tvar_names = List.map fst scope_tvars in
  (* Combined tvar names: scope vars first, then function's own vars *)
  let all_tvar_names = scope_tvar_names @ fn_tvar_names in
  (* Build params and return type, recognizing scope tvars as type variables *)
  let params =
    List.map
      (sig_param_to_param_with_scope_ctx ctx scope_tvars fn_tvar_names)
      d.defun_params
  in
  let ret =
    sig_type_to_typ_with_scope_ctx ctx scope_tvars fn_tvar_names d.defun_return
  in
  let arrow = Types.TArrow (params, ret) in
  if all_tvar_names = [] then Type_env.Mono arrow
  else Type_env.Poly (all_tvar_names, arrow)

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
  ls_loaded : string list;  (** Modules already loaded (cycle detection) *)
  ls_scope_tvars : (string * Types.typ) list;
      (** Type variables from enclosing type-scope blocks *)
  ls_imported_types : string list;
      (** Type names imported from prelude/open/include (for shadowing check) *)
  ls_imported_values : string list;
      (** Value names imported from include (for shadowing check) *)
}
(** State accumulated during loading *)

(** Create initial load state.

    @param type_ctx
      Initial type context (prelude or empty). When provided, prelude type
      aliases are available for use in the signature being loaded.
    @param imported_types
      Initial set of imported type names (e.g., from prelude). *)
let init_load_state ~type_ctx ~resolver ~base_env ~module_name
    ?(imported_types = []) ?(imported_values = []) () =
  {
    ls_type_ctx = type_ctx;
    ls_env = base_env;
    ls_resolver = resolver;
    ls_loaded = [ module_name ];
    (* Mark self as loaded to prevent self-import *)
    ls_scope_tvars = [];
    ls_imported_types = imported_types;
    ls_imported_values = imported_values;
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
        else Type_env.Poly (type_params, ctor_typ)
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
            else Type_env.Poly (type_params, accessor_typ)
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
                else Type_env.Poly (type_params, accessor_typ)
              in
              (add_value_to_state accessor_name accessor_scheme state, idx + 1))
            (state, 1) ctor.ctor_fields
          |> fst
      in
      state)
    state d.data_ctors

(** {1 Open and Include Processing}

    - open: Import types for use in signatures (not re-exported to value env)
    - include: Inline all declarations (types available AND values re-exported)
*)

(** Check that a type name doesn't shadow an imported binding. Raises Failure if
    shadowing is detected. *)
let check_type_not_shadowing name state =
  if is_imported_type name state then
    failwith (Printf.sprintf "cannot redefine imported binding '%s'" name)

(** Check that a value name doesn't shadow an imported binding. Raises Failure
    if shadowing is detected. *)
let check_value_not_shadowing name state =
  if is_imported_value name state then
    failwith (Printf.sprintf "cannot redefine imported binding '%s'" name)

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
    (state : load_state) : load_state =
  List.fold_left
    (fun state decl ->
      match decl with
      | DOpen (name, _) -> process_open name state
      | DInclude (name, _) -> process_include name state
      | DType d ->
          (* Check shadowing if from current file *)
          if not from_include then check_type_not_shadowing d.type_name state;
          (* Add to type context *)
          let state =
            match d.type_body with
            | Some body ->
                (* Type alias *)
                let alias =
                  {
                    alias_params = List.map binder_to_alias_param d.type_params;
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
                add_opaque_to_state d.type_name opaque state
          in
          (* Mark as imported if from include *)
          if from_include then mark_type_imported d.type_name state else state
      | DDefun d ->
          (* Check shadowing if from current file *)
          if not from_include then check_value_not_shadowing d.defun_name state;
          let scheme = load_defun_with_ctx state.ls_type_ctx d in
          (* Add to function namespace for Lisp-2 semantics *)
          let state = add_fn_to_state d.defun_name scheme state in
          (* Register predicate info if this is a type predicate *)
          let state =
            match extract_predicate_info state.ls_type_ctx d with
            | Some pred_info ->
                {
                  state with
                  ls_env =
                    Type_env.extend_predicate d.defun_name pred_info
                      state.ls_env;
                }
            | None -> state
          in
          (* Mark as imported if from include *)
          if from_include then mark_value_imported d.defun_name state else state
      | DDefvar d ->
          (* Check shadowing if from current file *)
          if not from_include then check_value_not_shadowing d.defvar_name state;
          let scheme = load_defvar_with_ctx state.ls_type_ctx d in
          let state = add_value_to_state d.defvar_name scheme state in
          (* Mark as imported if from include *)
          if from_include then mark_value_imported d.defvar_name state
          else state
      | DImportStruct d ->
          (* import-struct generates type + multiple values *)
          if not from_include then check_type_not_shadowing d.struct_name state;
          let state = load_import_struct sig_file.sig_module d state in
          if from_include then
            let state = mark_type_imported d.struct_name state in
            (* Also mark generated functions as imported *)
            let state = mark_value_imported ("make-" ^ d.struct_name) state in
            let state = mark_value_imported (d.struct_name ^ "-p") state in
            List.fold_left
              (fun s (slot_name, _) ->
                mark_value_imported (d.struct_name ^ "-" ^ slot_name) s)
              state d.struct_slots
          else state
      | DData d ->
          (* data generates type + constructor functions *)
          if not from_include then check_type_not_shadowing d.data_name state;
          let state = load_data sig_file.sig_module d state in
          if from_include then
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
          else state
      | DTypeScope d -> load_type_scope ~from_include sig_file d state)
    state sig_file.sig_decls

(** Load a type-scope block. Creates fresh type variables for the scope's
    binders and processes each inner declaration with those variables available
    in the type context. *)
and load_type_scope ?(from_include = false) (sig_file : signature)
    (scope : type_scope_decl) (state : load_state) : load_state =
  (* Create fresh TVars for each scope binder.
     Level 0 is used for signature-level type variables. *)
  let scope_tvars =
    List.map
      (fun binder -> (binder.name, Types.fresh_tvar 0))
      scope.scope_tvar_binders
  in
  (* Extend state with scope type variables *)
  let state_with_scope =
    { state with ls_scope_tvars = scope_tvars @ state.ls_scope_tvars }
  in
  (* Process inner declarations with the scope context *)
  List.fold_left
    (fun st decl -> load_scoped_decl ~from_include sig_file scope_tvars decl st)
    state_with_scope scope.scope_decls

(** Load a declaration inside a type-scope. Adds scope type variables to the
    defun's quantifier list. *)
and load_scoped_decl ?(from_include = false) (sig_file : signature)
    (scope_tvars : (string * Types.typ) list) (decl : decl) (state : load_state)
    : load_state =
  match decl with
  | DOpen (name, _) -> process_open name state
  | DInclude (name, _) -> process_include name state
  | DType d ->
      (* Check shadowing if from current file *)
      if not from_include then check_type_not_shadowing d.type_name state;
      (* Type declarations in scopes work the same way *)
      let state =
        match d.type_body with
        | Some body ->
            let alias =
              {
                alias_params = List.map binder_to_alias_param d.type_params;
                alias_body = body;
              }
            in
            add_alias_to_state d.type_name alias state
        | None ->
            let opaque =
              {
                opaque_params = List.map (fun b -> b.name) d.type_params;
                opaque_con = opaque_con_name sig_file.sig_module d.type_name;
              }
            in
            add_opaque_to_state d.type_name opaque state
      in
      if from_include then mark_type_imported d.type_name state else state
  | DDefun d ->
      (* Check shadowing if from current file *)
      if not from_include then check_value_not_shadowing d.defun_name state;
      (* For defuns in scope, combine scope tvars with function's own tvars *)
      let scheme = load_defun_with_scope state.ls_type_ctx scope_tvars d in
      (* Add to function namespace for Lisp-2 semantics *)
      let state = add_fn_to_state d.defun_name scheme state in
      (* Register predicate info if this is a type predicate *)
      let state =
        match extract_predicate_info state.ls_type_ctx d with
        | Some pred_info ->
            {
              state with
              ls_env =
                Type_env.extend_predicate d.defun_name pred_info state.ls_env;
            }
        | None -> state
      in
      if from_include then mark_value_imported d.defun_name state else state
  | DDefvar d ->
      (* Check shadowing if from current file *)
      if not from_include then check_value_not_shadowing d.defvar_name state;
      let scheme = load_defvar_with_ctx state.ls_type_ctx d in
      let state = add_value_to_state d.defvar_name scheme state in
      if from_include then mark_value_imported d.defvar_name state else state
  | DImportStruct d ->
      if not from_include then check_type_not_shadowing d.struct_name state;
      let state = load_import_struct sig_file.sig_module d state in
      if from_include then
        let state = mark_type_imported d.struct_name state in
        let state = mark_value_imported ("make-" ^ d.struct_name) state in
        let state = mark_value_imported (d.struct_name ^ "-p") state in
        List.fold_left
          (fun s (slot_name, _) ->
            mark_value_imported (d.struct_name ^ "-" ^ slot_name) s)
          state d.struct_slots
      else state
  | DData d ->
      if not from_include then check_type_not_shadowing d.data_name state;
      let state = load_data sig_file.sig_module d state in
      if from_include then
        let state = mark_type_imported d.data_name state in
        List.fold_left
          (fun s (ctor : ctor_decl) ->
            let s = mark_value_imported ctor.ctor_name s in
            mark_value_imported
              (d.data_name ^ "-" ^ String.lowercase_ascii ctor.ctor_name ^ "-p")
              s)
          state d.data_ctors
      else state
  | DTypeScope nested ->
      (* Handle nested scopes recursively *)
      load_type_scope ~from_include sig_file nested state

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
    ~(resolver : module_resolver) (env : Type_env.t) (sig_file : signature) :
    Type_env.t =
  let type_ctx =
    match prelude_ctx with Some ctx -> ctx | None -> empty_type_context
  in
  let state =
    init_load_state ~type_ctx ~resolver ~base_env:env
      ~module_name:sig_file.sig_module ~imported_types:prelude_type_names ()
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
