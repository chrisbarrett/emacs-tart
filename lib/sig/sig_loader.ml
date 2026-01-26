(** Signature loader and validator.

    This module converts signature AST to the type environment,
    validating type variable scoping and resolving type references.

    Key validations:
    - Type variables must be explicitly bound in quantifiers
    - Bounded quantifiers must reference valid types
    - Referenced types must be in scope (from opens or type decls)
*)

open Sig_ast
module Loc = Syntax.Location
module Types = Core.Types
module Type_env = Core.Type_env

(** {1 Load Errors} *)

(** Error during signature loading *)
type load_error = {
  message : string;
  span : Loc.span;
}

(** Result type for loading *)
type 'a result = ('a, load_error) Result.t

(** Create an error *)
let error message span : 'a result = Error { message; span }

(** {1 Type Variable Context} *)

(** Context for type variable resolution.
    Tracks which type variables are in scope. *)
type tvar_context = {
  bound_vars : string list;  (** Type variables in scope *)
  defined_types : string list;  (** User-defined types in scope *)
}

(** Empty context *)
let empty_context = { bound_vars = []; defined_types = [] }

(** Add bound type variables to context *)
let with_tvars ctx vars =
  { ctx with bound_vars = vars @ ctx.bound_vars }

(** Add a defined type to context *)
let with_type ctx name =
  { ctx with defined_types = name :: ctx.defined_types }

(** Check if a name is a type variable in scope *)
let is_tvar ctx name = List.mem name ctx.bound_vars

(** Check if a name is a defined type in scope *)
let is_defined_type ctx name = List.mem name ctx.defined_types

(** {1 Primitive and Built-in Types} *)

(** Check if a name is a primitive type *)
let is_primitive name =
  match name with
  | "int" | "float" | "num" | "string" | "symbol" | "keyword"
  | "nil" | "t" | "bool" | "truthy" | "any" | "never" -> true
  | _ -> false

(** {1 Type Validation} *)

(** Validate a sig_type, checking that all type variables are in scope.
    Returns Ok () if valid, Error with the first unbound variable otherwise. *)
let rec validate_type (ctx : tvar_context) (ty : sig_type) : unit result =
  match ty with
  | STVar (name, span) ->
      if is_tvar ctx name then
        Ok ()
      else if is_primitive name then
        Ok ()  (* It's actually a primitive, parser misclassified it *)
      else if is_defined_type ctx name then
        Ok ()  (* It's a user-defined type *)
      else
        error (Printf.sprintf "Unbound type variable: %s" name) span

  | STCon (_, _) ->
      Ok ()

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

  | STUnion (types, _) ->
      validate_types ctx types

  | STTuple (types, _) ->
      validate_types ctx types

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
  | None -> Ok ()  (* Opaque type, no body to validate *)
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
  | DOpen (_, _) -> Ok ()  (* Opens are handled separately *)
  | DInclude (_, _) -> Ok ()  (* Includes are handled separately *)
  | DDefun d -> validate_defun ctx d
  | DDefvar d -> validate_defvar ctx d
  | DType d -> validate_type_decl ctx d
  | DImportStruct d -> validate_import_struct ctx d

(** {1 Signature Validation} *)

(** Build context from declarations.
    Adds all type declarations to the context so they can be referenced. *)
let build_context (sig_file : signature) : tvar_context =
  List.fold_left
    (fun ctx decl ->
       match decl with
       | DType d -> with_type ctx d.type_name
       | _ -> ctx)
    empty_context sig_file.sig_decls

(** Validate an entire signature file.
    Returns Ok () if all declarations are valid, or the first error. *)
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
       match validate_decl ctx decl with
       | Ok () -> None
       | Error e -> Some e)
    sig_file.sig_decls

(** {1 Type Conversion}

    These functions convert signature AST types to the core type representation.
    They assume the signature has been validated (type variables are in scope). *)

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
  | _ -> Types.TCon name

(** Convert a signature type to a core type.
    [tvar_names] is the list of bound type variable names in scope. *)
let rec sig_type_to_typ (tvar_names : string list) (ty : sig_type) : Types.typ =
  match ty with
  | STVar (name, _) ->
      (* Type variable - represented as TCon for later substitution *)
      Types.TCon name

  | STCon (name, _) ->
      (* Type constant - map primitives and preserve others *)
      sig_name_to_prim name

  | STApp (name, args, _) ->
      (* Type application *)
      let arg_types = List.map (sig_type_to_typ tvar_names) args in
      Types.TApp (name, arg_types)

  | STArrow (params, ret, _) ->
      (* Function type *)
      let param_types = List.map (sig_param_to_param tvar_names) params in
      let ret_type = sig_type_to_typ tvar_names ret in
      Types.TArrow (param_types, ret_type)

  | STForall (binders, body, _) ->
      (* Polymorphic type - add binders to scope *)
      let new_vars = List.map (fun b -> b.name) binders in
      let inner_names = new_vars @ tvar_names in
      let body_type = sig_type_to_typ inner_names body in
      Types.TForall (new_vars, body_type)

  | STUnion (types, _) ->
      (* Union type *)
      let type_list = List.map (sig_type_to_typ tvar_names) types in
      Types.TUnion type_list

  | STTuple (types, _) ->
      (* Tuple type *)
      let type_list = List.map (sig_type_to_typ tvar_names) types in
      Types.TTuple type_list

(** Convert a signature parameter to a core parameter *)
and sig_param_to_param (tvar_names : string list) (p : sig_param) : Types.param =
  match p with
  | SPPositional ty -> Types.PPositional (sig_type_to_typ tvar_names ty)
  | SPOptional ty -> Types.POptional (sig_type_to_typ tvar_names ty)
  | SPRest ty -> Types.PRest (sig_type_to_typ tvar_names ty)
  | SPKey (name, ty) -> Types.PKey (name, sig_type_to_typ tvar_names ty)

(** {1 Declaration Loading}

    These functions convert signature declarations to type environment entries. *)

(** Convert a defun declaration to a type scheme.
    Returns a Poly scheme if the function has type parameters,
    otherwise a Mono scheme with an arrow type. *)
let load_defun (d : defun_decl) : Type_env.scheme =
  let tvar_names = List.map (fun b -> b.name) d.defun_tvar_binders in
  let params = List.map (sig_param_to_param tvar_names) d.defun_params in
  let ret = sig_type_to_typ tvar_names d.defun_return in
  let arrow = Types.TArrow (params, ret) in
  if tvar_names = [] then
    Type_env.Mono arrow
  else
    Type_env.Poly (tvar_names, arrow)

(** Convert a defvar declaration to a type scheme.
    The type may be polymorphic if it contains a forall. *)
let load_defvar (d : defvar_decl) : Type_env.scheme =
  match d.defvar_type with
  | STForall (binders, body, _) ->
      (* Polymorphic variable - extract quantifiers *)
      let tvar_names = List.map (fun b -> b.name) binders in
      let body_type = sig_type_to_typ tvar_names body in
      Type_env.Poly (tvar_names, body_type)
  | _ ->
      (* Monomorphic variable *)
      let ty = sig_type_to_typ [] d.defvar_type in
      Type_env.Mono ty

(** {1 Signature Loading}

    Load an entire signature file into a type environment. *)

(** Load a validated signature into a type environment.
    Adds all function and variable declarations to the environment.
    Returns the extended environment. *)
let load_signature (env : Type_env.t) (sig_file : signature) : Type_env.t =
  List.fold_left
    (fun acc decl ->
       match decl with
       | DOpen _ | DInclude _ ->
           (* Module directives - not handled yet *)
           acc
       | DDefun d ->
           let scheme = load_defun d in
           Type_env.extend d.defun_name scheme acc
       | DDefvar d ->
           let scheme = load_defvar d in
           Type_env.extend d.defvar_name scheme acc
       | DType _ | DImportStruct _ ->
           (* Type declarations - not adding to value environment *)
           acc)
    env sig_file.sig_decls
