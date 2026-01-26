(** Top-level type checking API.

    This module provides the entry point for type checking Elisp programs. It
    handles sequences of top-level forms, accumulating type bindings from defun
    and other definition forms.

    By default, all type checking functions use an environment pre-populated
    with types for built-in functions (car, +, concat, etc.). This can be
    overridden by passing a custom ~env parameter. *)

open Core.Types
module Env = Core.Type_env
module C = Constraint
module Sig_parser = Sig.Sig_parser
module Sig_loader = Sig.Sig_loader
module Forall_infer = Sig.Forall_infer
module Sig_ast = Sig.Sig_ast
module Loc = Syntax.Location

(** Default environment with built-in function types *)
let default_env () = Builtin_types.initial_env ()

(** Result of type-checking a single top-level form *)
type form_result =
  | DefunForm of { name : string; fn_type : typ }
  | DefvarForm of { name : string; var_type : typ }
  | TartDeclareForm of { name : string; var_type : typ }
  | ExprForm of { ty : typ }

type check_result = {
  env : Env.t;  (** Final type environment with all bindings *)
  forms : form_result list;  (** Results for each top-level form *)
  errors : Unify.error list;  (** Any type errors encountered *)
  undefineds : Infer.undefined_var list;  (** Undefined variable references *)
}
(** Result of type-checking a program *)

(** Parse a tart type annotation from an S-expression.

    Returns the parsed sig_type or None if parsing fails. *)
let parse_tart_type (type_sexp : Syntax.Sexp.t) : Sig_ast.sig_type option =
  match Sig_parser.parse_sig_type type_sexp with
  | Ok sig_type -> Some sig_type
  | Error _ -> None

(** Substitute type variable names (as TCon) with actual TVars in a type.

    Used to convert signature types to inferable types. *)
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
            | PKey (n, t) -> PKey (n, substitute_tvar_names subst t))
          params
      in
      TArrow (params', substitute_tvar_names subst ret)
  | TApp (con, args) -> TApp (con, List.map (substitute_tvar_names subst) args)
  | TForall (vars, body) ->
      (* Remove substituted vars from the substitution to avoid capture *)
      let subst' = List.filter (fun (n, _) -> not (List.mem n vars)) subst in
      TForall (vars, substitute_tvar_names subst' body)
  | TUnion types -> TUnion (List.map (substitute_tvar_names subst) types)
  | TTuple types -> TTuple (List.map (substitute_tvar_names subst) types)

(** Convert a sig_type to a typ at the given level.

    Applies forall inference, creates fresh TVars for type parameters, and
    substitutes them in the type body. *)
let sig_type_to_typ_with_tvars (level : int) (sig_type : Sig_ast.sig_type) : typ
    =
  (* Apply forall inference to get implicit quantifiers *)
  let sig_type = Forall_infer.infer_sig_type ~known_types:[] sig_type in

  (* Extract type variable names from forall, if present *)
  let tvar_names, inner_type =
    match sig_type with
    | Sig_ast.STForall (binders, inner, _) ->
        (List.map (fun b -> b.Sig_ast.name) binders, inner)
    | _ -> ([], sig_type)
  in

  (* Create fresh type variables for polymorphic type parameters *)
  let tvar_subst = List.map (fun name -> (name, fresh_tvar level)) tvar_names in

  (* Convert sig_type to typ and substitute fresh TVars *)
  let base_ty = Sig_loader.sig_type_to_typ tvar_names inner_type in
  substitute_tvar_names tvar_subst base_ty

(** Extract a tart annotation from defvar/defconst initialization.

    Checks if the init form is [(tart TYPE VALUE)] and returns the type and
    value if so. Returns [None] if no annotation is present. *)
let extract_defvar_tart_annotation (init : Syntax.Sexp.t) :
    (Syntax.Sexp.t * Syntax.Sexp.t) option =
  let open Syntax.Sexp in
  match init with
  | List ([ Symbol ("tart", _); type_sexp; value ], _) -> Some (type_sexp, value)
  | _ -> None

(** Check a defvar/defconst form with optional tart annotation.

    If the init is [(tart TYPE VALUE)], binds NAME with TYPE in the environment
    and checks VALUE against TYPE. Otherwise, infers the type from the init. *)
let check_defvar (env : Env.t) (name : string) (init : Syntax.Sexp.t option)
    (_span : Loc.span) :
    Env.t * form_result * Unify.error list * Infer.undefined_var list =
  match init with
  | Some init_expr -> (
      match extract_defvar_tart_annotation init_expr with
      | Some (type_sexp, value) -> (
          (* Has tart annotation: (defvar NAME (tart TYPE VALUE)) *)
          match parse_tart_type type_sexp with
          | Some sig_type ->
              let var_type =
                sig_type_to_typ_with_tvars (Env.current_level env) sig_type
              in

              (* Check the value against the declared type *)
              let value_result = Infer.infer env value in
              let value_span = Syntax.Sexp.span_of value in
              let annotation_constraint =
                C.equal
                  ~context:(C.TartAnnotation { declared_type = var_type })
                  var_type value_result.Infer.ty value_span
              in
              let all_constraints =
                C.add annotation_constraint value_result.Infer.constraints
              in
              let errors = Unify.solve_all all_constraints in

              (* Bind the variable with the declared type *)
              let scheme =
                Generalize.generalize (Env.current_level env) var_type
              in
              let env' = Env.extend name scheme env in

              ( env',
                DefvarForm { name; var_type },
                errors,
                value_result.Infer.undefineds )
          | None ->
              (* Parse failed - fall through to regular inference *)
              let result = Infer.infer env init_expr in
              let errors = Unify.solve_all result.Infer.constraints in
              let scheme =
                Generalize.generalize (Env.current_level env) result.Infer.ty
              in
              let env' = Env.extend name scheme env in
              ( env',
                DefvarForm { name; var_type = result.Infer.ty },
                errors,
                result.Infer.undefineds ))
      | None ->
          (* No tart annotation: infer the type from init *)
          let result = Infer.infer env init_expr in
          let errors = Unify.solve_all result.Infer.constraints in
          let scheme =
            Generalize.generalize (Env.current_level env) result.Infer.ty
          in
          let env' = Env.extend name scheme env in
          ( env',
            DefvarForm { name; var_type = result.Infer.ty },
            errors,
            result.Infer.undefineds ))
  | None -> (
      (* (defvar NAME) with no init - respect existing tart-declare type if present *)
      match Env.lookup name env with
      | Some scheme ->
          (* Variable already declared (e.g., via tart-declare) - keep that type *)
          let var_type = Env.instantiate scheme env in
          (env, DefvarForm { name; var_type }, [], [])
      | None ->
          (* New variable without init - use Any type *)
          let var_type = Prim.any in
          let env' = Env.extend_mono name var_type env in
          (env', DefvarForm { name; var_type }, [], []))

(** Check a tart-declare form: (tart-declare NAME TYPE)

    Binds NAME with TYPE in the environment. Does not require an initial value.
*)
let check_tart_declare (env : Env.t) (name : string) (type_sexp : Syntax.Sexp.t)
    : Env.t * form_result * Unify.error list * Infer.undefined_var list =
  match parse_tart_type type_sexp with
  | Some sig_type ->
      let var_type =
        sig_type_to_typ_with_tvars (Env.current_level env) sig_type
      in
      let scheme = Generalize.generalize (Env.current_level env) var_type in
      let env' = Env.extend name scheme env in
      (env', TartDeclareForm { name; var_type }, [], [])
  | None ->
      (* Parse failed - use Any type *)
      let var_type = Prim.any in
      let env' = Env.extend_mono name var_type env in
      (env', TartDeclareForm { name; var_type }, [], [])

(** Try to match a defvar or defconst form.

    Returns [Some (name, init_opt, span)] if matched, [None] otherwise. *)
let match_defvar (sexp : Syntax.Sexp.t) :
    (string * Syntax.Sexp.t option * Loc.span) option =
  let open Syntax.Sexp in
  match sexp with
  | List
      ( (Symbol ("defvar", _) | Symbol ("defconst", _))
        :: Symbol (name, _)
        :: init :: _,
        span ) ->
      Some (name, Some init, span)
  | List
      ( (Symbol ("defvar", _) | Symbol ("defconst", _)) :: Symbol (name, _) :: _,
        span ) ->
      Some (name, None, span)
  | _ -> None

(** Try to match a tart-declare form.

    Returns [Some (name, type_sexp)] if matched, [None] otherwise. *)
let match_tart_declare (sexp : Syntax.Sexp.t) : (string * Syntax.Sexp.t) option
    =
  let open Syntax.Sexp in
  match sexp with
  | List ([ Symbol ("tart-declare", _); Symbol (name, _); type_sexp ], _) ->
      Some (name, type_sexp)
  | _ -> None

(** Check a single top-level form and update the environment.

    Returns the updated environment, form result, errors, and undefined vars. *)
let check_form (env : Env.t) (sexp : Syntax.Sexp.t) :
    Env.t * form_result * Unify.error list * Infer.undefined_var list =
  reset_tvar_counter ();

  (* Try to match tart-declare first *)
  match match_tart_declare sexp with
  | Some (name, type_sexp) -> check_tart_declare env name type_sexp
  | None -> (
      (* Try to match defvar/defconst *)
      match match_defvar sexp with
      | Some (name, init, span) -> check_defvar env name init span
      | None -> (
          (* Try to handle it as a defun *)
          match Infer.infer_defun env sexp with
          | Some defun_result ->
              (* Bind the function name in the environment *)
              let scheme =
                Generalize.generalize (Env.current_level env)
                  defun_result.Infer.fn_type
              in
              let env' = Env.extend defun_result.Infer.name scheme env in
              (* Solve constraints and collect errors *)
              let errors =
                Unify.solve_all defun_result.Infer.defun_constraints
              in
              ( env',
                DefunForm
                  {
                    name = defun_result.Infer.name;
                    fn_type = defun_result.Infer.fn_type;
                  },
                errors,
                defun_result.Infer.defun_undefineds )
          | None ->
              (* Regular expression *)
              let result = Infer.infer env sexp in
              let errors = Unify.solve_all result.Infer.constraints in
              ( env,
                ExprForm { ty = result.Infer.ty },
                errors,
                result.Infer.undefineds )))

(** Check a sequence of top-level forms.

    Processes forms in order, accumulating type bindings from defuns. Uses the
    default environment with built-in types unless overridden. *)
let check_program ?(env = default_env ()) (forms : Syntax.Sexp.t list) :
    check_result =
  let rec loop env forms results errors undefineds =
    match forms with
    | [] ->
        {
          env;
          forms = List.rev results;
          errors = List.rev errors;
          undefineds = List.rev undefineds;
        }
    | form :: rest ->
        let env', result, form_errors, form_undefs = check_form env form in
        loop env' rest (result :: results)
          (List.rev_append form_errors errors)
          (List.rev_append form_undefs undefineds)
  in
  loop env forms [] [] []

(** Check a single expression and return its type.

    This is a convenience function for checking a single expression without
    accumulating environment changes. Uses the default environment with built-in
    types unless overridden. *)
let check_expr ?(env = default_env ()) (sexp : Syntax.Sexp.t) :
    typ * Unify.error list =
  reset_tvar_counter ();
  let result = Infer.infer env sexp in
  let errors = Unify.solve_all result.Infer.constraints in
  (repr result.Infer.ty, errors)

(** Convert a form result to a string for display *)
let form_result_to_string = function
  | DefunForm { name; fn_type } ->
      Printf.sprintf "(defun %s %s)" name (to_string fn_type)
  | DefvarForm { name; var_type } ->
      Printf.sprintf "(defvar %s %s)" name (to_string var_type)
  | TartDeclareForm { name; var_type } ->
      Printf.sprintf "(tart-declare %s %s)" name (to_string var_type)
  | ExprForm { ty } -> to_string ty
