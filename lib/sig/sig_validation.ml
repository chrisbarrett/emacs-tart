(** Signature validation.

    Validates signature ASTs before loading, checking that all type variables
    are in scope and referenced types exist.

    Key validations:
    - Type variables must be explicitly bound in quantifiers
    - Bounded quantifiers must reference valid types
    - Referenced types must be in scope (from opens or type decls) *)

open Sig_ast
module Types = Core.Types

(** {1 Validation Errors} *)

type validation_error = { message : string; span : Syntax.Location.span }
(** Error during signature validation *)

type 'a result = ('a, validation_error) Result.t
(** Result type for validation *)

(** Create a validation error *)
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

(** {1 Primitive Types} *)

(** Check if a name is a primitive type.

    This now only checks for intrinsic names (with %tart-intrinsic% prefix).
    User-facing names like "int", "string" etc. are handled by the prelude. *)
let is_primitive name = Types.is_intrinsic_name name

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
  | STInfer (_, _) ->
      (* Infer placeholders are always valid - they become fresh tvars *)
      Ok ()

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
      | SPKey (_, ty) -> validate_type ctx ty
      | SPLiteral _ -> Ok ())
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

(** Validate a single defun clause *)
let validate_clause ctx (c : defun_clause) : unit result =
  let* () = validate_params ctx c.clause_params in
  validate_type ctx c.clause_return

(** Validate a defun declaration *)
let validate_defun ctx (d : defun_decl) : unit result =
  (* Add type parameters to context *)
  let var_names = List.map (fun b -> b.name) d.defun_tvar_binders in
  let inner_ctx = with_tvars ctx var_names in
  (* Validate binder bounds in outer context *)
  let* () = validate_binder_bounds ctx d.defun_tvar_binders in
  (* Validate all clauses *)
  List.fold_left
    (fun acc clause ->
      let* () = acc in
      validate_clause inner_ctx clause)
    (Ok ()) d.defun_clauses

(** Validate a defvar declaration *)
let validate_defvar ctx (d : defvar_decl) : unit result =
  validate_type ctx d.defvar_type

(** Validate a single type binding *)
let validate_type_binding ctx (b : type_binding) : unit result =
  (* Add type parameters to context *)
  let var_names = List.map (fun p -> p.name) b.tb_params in
  let inner_ctx = with_tvars ctx var_names in
  (* Validate binder bounds in outer context *)
  let* () = validate_binder_bounds ctx b.tb_params in
  (* Validate body if present *)
  match b.tb_body with
  | None -> Ok () (* Opaque type, no body to validate *)
  | Some body -> validate_type inner_ctx body

(** Validate a type declaration (group of bindings) *)
let validate_type_decl ctx (d : type_decl) : unit result =
  List.fold_left
    (fun acc b ->
      let* () = acc in
      validate_type_binding ctx b)
    (Ok ()) d.type_bindings

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
  | DForall d ->
      (* Add scope type variables to context, then validate inner decls *)
      let scope_var_names = List.map (fun b -> b.name) d.forall_tvar_binders in
      let scope_ctx = with_tvars ctx scope_var_names in
      List.fold_left
        (fun acc inner_decl ->
          let* () = acc in
          validate_decl scope_ctx inner_decl)
        (Ok ()) d.forall_decls
  | DLetType d -> validate_type_decl ctx d

(** {1 Signature Validation} *)

(** Build context from declarations. Adds all type declarations to the context
    so they can be referenced. *)
let build_context (sig_file : signature) : tvar_context =
  let rec add_decl_types ctx decl =
    match decl with
    | DType d ->
        List.fold_left (fun c b -> with_type c b.tb_name) ctx d.type_bindings
    | DImportStruct d -> with_type ctx d.struct_name
    | DData d -> with_type ctx d.data_name
    | DForall d ->
        (* Recursively collect types from scope declarations *)
        List.fold_left add_decl_types ctx d.forall_decls
    | DLetType d ->
        List.fold_left (fun c b -> with_type c b.tb_name) ctx d.type_bindings
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
    validation_error list =
  let base_ctx = build_context sig_file in
  let ctx =
    List.fold_left (fun c name -> with_type c name) base_ctx prelude_type_names
  in
  List.filter_map
    (fun decl ->
      match validate_decl ctx decl with Ok () -> None | Error e -> Some e)
    sig_file.sig_decls
