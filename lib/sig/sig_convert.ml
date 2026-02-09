(** Signature-to-core type conversion.

    Converts signature AST types ({!Sig_ast.sig_type}) to core types
    ({!Core.Types.typ}). Handles alias expansion, opaque type resolution,
    intrinsic name canonicalization, and parameterized type substitution.

    The main entry point is {!sig_type_to_typ_with_ctx}, which resolves names
    using a {!type_context} containing aliases and opaques. Convenience wrappers
    provide simpler interfaces for common cases. *)

open Sig_ast
module Types = Core.Types

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
      | DType d ->
          List.fold_left
            (fun ctx (b : type_binding) ->
              match b.tb_body with
              | Some body ->
                  let alias =
                    {
                      alias_params = List.map binder_to_alias_param b.tb_params;
                      alias_body = body;
                    }
                  in
                  add_alias b.tb_name alias ctx
              | None -> ctx)
            ctx d.type_bindings
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
      | DType d ->
          List.fold_left
            (fun ctx (b : type_binding) ->
              match b.tb_body with
              | None ->
                  let opaque =
                    {
                      opaque_params = List.map (fun p -> p.name) b.tb_params;
                      opaque_con = opaque_con_name module_name b.tb_name;
                    }
                  in
                  add_opaque b.tb_name opaque ctx
              | Some _ -> ctx)
            ctx d.type_bindings
      | _ -> ctx)
    empty_opaques sig_file.sig_decls

(** {1 Type Context}

    Combined context for type resolution during signature loading. Contains both
    aliases and opaque types. *)

type type_context = { tc_aliases : alias_context; tc_opaques : opaque_context }

let empty_type_context =
  { tc_aliases = empty_aliases; tc_opaques = empty_opaques }

(** {1 Bound Checking} *)

(** Check if a type is a member of a union.

    A type [t] is a member of a union if it is structurally equal to one of the
    union's members. For example, [symbol] is a member of
    [(symbol | keyword | int | t | nil)]. *)
let is_union_member (arg_typ : Types.typ) (members : Types.typ list) : bool =
  List.exists (fun member -> Types.equal arg_typ member) members

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
      Types.equal arg_typ bound_typ

(** {1 Signature AST Substitution} *)

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
  | STInfer (_, _) ->
      (* Infer placeholders are not affected by substitution *)
      ty

and substitute_sig_param subst = function
  | SPPositional (name, ty) -> SPPositional (name, substitute_sig_type subst ty)
  | SPOptional (name, ty) -> SPOptional (name, substitute_sig_type subst ty)
  | SPRest ty -> SPRest (substitute_sig_type subst ty)
  | SPKey (name, ty) -> SPKey (name, substitute_sig_type subst ty)
  | SPLiteral _ as p -> p

(** {1 Name Canonicalization} *)

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
    | "Plist" -> Types.intrinsic "Plist"
    | "Map" -> Types.intrinsic "Map"
    (* Lowercase container names also map to intrinsics *)
    | "list" -> Types.intrinsic "List"
    | "vector" -> Types.intrinsic "Vector"
    | "cons" -> Types.intrinsic "Pair"
    | "hash-table" -> Types.intrinsic "HashTable"
    | "plist" -> Types.intrinsic "Plist"
    | "map" -> Types.intrinsic "Map"
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

(** {1 Type Conversion}

    These functions convert signature AST types to the core type representation.
    They assume the signature has been validated (type variables are in scope).
*)

(** Convert a signature type to a core type. [ctx] is the type context
    containing aliases and opaques. [tvar_names] is the list of bound type
    variable names in scope. [scope_tvars] maps scope type variable names to
    their pre-created TVars (shared across declarations in a forall block);
    defaults to empty. *)
let rec sig_type_to_typ_with_ctx ?(scope_tvars : (string * Types.typ) list = [])
    (ctx : type_context) (tvar_names : string list) (ty : sig_type) : Types.typ
    =
  let convert = sig_type_to_typ_with_ctx ~scope_tvars ctx in
  let convert_param = sig_param_to_param_with_ctx ~scope_tvars ctx in
  match ty with
  | STVar (name, _) -> (
      (* Check scope type variables first *)
      match List.assoc_opt name scope_tvars with
      | Some tvar -> tvar
      | None -> (
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
                convert tvar_names alias.alias_body
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
                    sig_name_to_prim name)))
  | STCon (name, _) -> (
      (* Check scope type variables first *)
      match List.assoc_opt name scope_tvars with
      | Some tvar -> tvar
      | None -> (
          (* Type constant - check for alias first, then opaque, then map primitives *)
          match lookup_alias name ctx.tc_aliases with
          | Some alias when alias.alias_params = [] ->
              (* Non-parameterized alias - expand it *)
              convert tvar_names alias.alias_body
          | _ -> (
              (* Check for opaque type *)
              match lookup_opaque name ctx.tc_opaques with
              | Some opaque when opaque.opaque_params = [] ->
                  (* Non-parameterized opaque - use the unique constructor *)
                  Types.TCon opaque.opaque_con
              | _ ->
                  (* Not an alias/opaque or has parameters (needs args) *)
                  sig_name_to_prim name)))
  | STApp (name, args, _span) -> (
      (* Check if constructor is a scope type variable (higher-kinded) *)
      match List.assoc_opt name scope_tvars with
      | Some tvar ->
          let arg_types = List.map (convert tvar_names) args in
          Types.TApp (tvar, arg_types)
      | None -> (
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
                      let arg_typ = convert tvar_names arg in
                      (* Convert the bound to a core type *)
                      let bound_typ = convert tvar_names bound_sig_type in
                      (* Check that arg satisfies the bound (arg <: bound).
                     For truthy bound, arg must be truthy. *)
                      if not (satisfies_bound arg_typ bound_typ) then
                        failwith
                          (Printf.sprintf
                             "Bound violation in %s: %s is not a subtype of %s"
                             name (Types.to_string arg_typ)
                             (Types.to_string bound_typ)))
                alias.alias_params args;
              (* Substitute args into body and expand *)
              let subst =
                List.combine
                  (List.map (fun p -> p.ap_name) alias.alias_params)
                  args
              in
              let expanded = substitute_sig_type subst alias.alias_body in
              convert tvar_names expanded
          | _ -> (
              (* Special case: map types with a single row argument (Design B).
             (alist {name string & r}) expands to (list (cons symbol {name string & r}))
             (plist {:name string & r}) expands to (list (keyword | {name string & r}))
             (hash-table {name string & r}) expands to (HashTable symbol {name string & r})
             (map {name string & r}) expands to (Map {name string & r})
             This preserves field names for static key lookup while maintaining
             structural compatibility with homogeneous map types. *)
              let expand_map_row name arg_typ =
                match name with
                | "alist" ->
                    Some
                      (Types.list_of (Types.pair_of Types.Prim.symbol arg_typ))
                | "plist" -> Some (Types.plist_of Types.Prim.keyword arg_typ)
                | "hash-table" ->
                    Some (Types.hash_table_of Types.Prim.symbol arg_typ)
                | "map" -> Some (Types.map_of arg_typ)
                | _ -> None
              in
              match (name, args) with
              | ("alist" | "plist" | "hash-table" | "map"), [ single_arg ] -> (
                  let arg_typ = convert tvar_names single_arg in
                  match (Types.repr arg_typ, expand_map_row name arg_typ) with
                  | TRow _, Some expanded -> expanded
                  | _ ->
                      Types.TApp
                        (Types.TCon (canonicalize_type_name name), [ arg_typ ]))
              | _ -> (
                  (* Check for opaque type with phantom parameters *)
                  match lookup_opaque name ctx.tc_opaques with
                  | Some opaque
                    when List.length opaque.opaque_params = List.length args ->
                      (* Parameterized opaque - create TApp with unique constructor and args *)
                      let arg_types = List.map (convert tvar_names) args in
                      Types.TApp (Types.TCon opaque.opaque_con, arg_types)
                  | _ ->
                      (* Not an alias/opaque or arity mismatch - treat as type application.
                     If name is a type variable, keep it as TCon for later substitution
                     during instantiation. This enables higher-kinded types. *)
                      let arg_types = List.map (convert tvar_names) args in
                      Types.TApp
                        (Types.TCon (canonicalize_type_name name), arg_types))))
      )
  | STArrow (params, ret, _) ->
      (* Function type *)
      let param_types = List.map (convert_param tvar_names) params in
      let ret_type = convert tvar_names ret in
      Types.TArrow (param_types, ret_type)
  | STForall (binders, body, _) ->
      (* Polymorphic type - add binders to scope *)
      let new_vars = List.map (fun b -> b.name) binders in
      let inner_names = new_vars @ tvar_names in
      let body_type = convert inner_names body in
      Types.TForall (new_vars, body_type)
  | STUnion (types, _) ->
      (* Union type *)
      let type_list = List.map (convert tvar_names) types in
      Types.TUnion type_list
  | STTuple (types, _) ->
      (* Tuple type *)
      let type_list = List.map (convert tvar_names) types in
      Types.TTuple type_list
  | STSubtract (minuend, subtrahend, _) ->
      (* Type subtraction: remove subtrahend from minuend's union.
         E.g., ((int | string) - int) => string
         E.g., ((truthy | nil) - nil) => truthy *)
      let minuend_type = convert tvar_names minuend in
      let subtrahend_type = convert tvar_names subtrahend in
      Types.subtract_type minuend_type subtrahend_type
  | STRow (row, _) ->
      (* Convert sig_row to core TRow *)
      let fields =
        List.map
          (fun (name, ty) -> (name, convert tvar_names ty))
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
  | STInfer (_, _) ->
      (* Infer placeholder represents "any type" in signatures *)
      Types.Prim.any

(** Convert a signature parameter to a core parameter with type context *)
and sig_param_to_param_with_ctx ?(scope_tvars : (string * Types.typ) list = [])
    (ctx : type_context) (tvar_names : string list) (p : sig_param) :
    Types.param =
  let convert = sig_type_to_typ_with_ctx ~scope_tvars ctx tvar_names in
  match p with
  | SPPositional (_, ty) -> Types.PPositional (convert ty)
  | SPOptional (_, ty) -> Types.POptional (convert ty)
  | SPRest ty -> Types.PRest (convert ty)
  | SPKey (name, ty) -> Types.PKey (name, convert ty)
  | SPLiteral (value, _) -> Types.PLiteral value

(** {1 Convenience Wrappers} *)

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
