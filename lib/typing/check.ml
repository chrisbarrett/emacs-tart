(** Top-level type checking API.

    This module provides the entry point for type checking Elisp programs.
    It handles sequences of top-level forms, accumulating type bindings
    from defun and other definition forms.
*)

open Core.Types
module Env = Core.Type_env
module C = Constraint

(** Result of type-checking a single top-level form *)
type form_result =
  | DefunForm of { name : string; fn_type : typ }
  | ExprForm of { ty : typ }

(** Result of type-checking a program *)
type check_result = {
  env : Env.t;  (** Final type environment with all bindings *)
  forms : form_result list;  (** Results for each top-level form *)
  errors : Unify.error list;  (** Any type errors encountered *)
}

(** Check a single top-level form and update the environment.

    Returns the updated environment and the form result.
*)
let check_form (env : Env.t) (sexp : Syntax.Sexp.t) : Env.t * form_result * Unify.error list =
  reset_tvar_counter ();

  (* First, try to handle it as a defun *)
  match Infer.infer_defun env sexp with
  | Some defun_result ->
      (* Bind the function name in the environment *)
      let scheme = Generalize.generalize (Env.current_level env) defun_result.Infer.fn_type in
      let env' = Env.extend defun_result.Infer.name scheme env in
      (* Solve constraints and collect errors *)
      let errors = Unify.solve_all defun_result.Infer.defun_constraints in
      (env', DefunForm { name = defun_result.Infer.name; fn_type = defun_result.Infer.fn_type }, errors)
  | None ->
      (* Regular expression *)
      let result = Infer.infer env sexp in
      let errors = Unify.solve_all result.Infer.constraints in
      (env, ExprForm { ty = result.Infer.ty }, errors)

(** Check a sequence of top-level forms.

    Processes forms in order, accumulating type bindings from defuns.
*)
let check_program ?(env = Env.empty) (forms : Syntax.Sexp.t list) : check_result =
  let rec loop env forms results errors =
    match forms with
    | [] ->
        { env; forms = List.rev results; errors = List.rev errors }
    | form :: rest ->
        let env', result, form_errors = check_form env form in
        loop env' rest (result :: results) (List.rev_append form_errors errors)
  in
  loop env forms [] []

(** Check a single expression and return its type.

    This is a convenience function for checking a single expression
    without accumulating environment changes.
*)
let check_expr ?(env = Env.empty) (sexp : Syntax.Sexp.t) : typ * Unify.error list =
  reset_tvar_counter ();
  let result = Infer.infer env sexp in
  let errors = Unify.solve_all result.Infer.constraints in
  (repr result.Infer.ty, errors)

(** Convert a form result to a string for display *)
let form_result_to_string = function
  | DefunForm { name; fn_type } ->
      Printf.sprintf "(defun %s %s)" name (to_string fn_type)
  | ExprForm { ty } ->
      to_string ty
