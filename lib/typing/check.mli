(** Top-level type checking API.

    This module provides the entry point for type checking Elisp programs. It
    handles sequences of top-level forms, accumulating type bindings from defun
    and other definition forms.

    By default, all type checking functions use an environment pre-populated
    with types for built-in functions (car, +, concat, etc.). *)

(** {1 Types} *)

(** Result of type-checking a single top-level form. *)
type form_result =
  | DefunForm of { name : string; fn_type : Core.Types.typ }
  | DefvarForm of { name : string; var_type : Core.Types.typ }
  | TartDeclareForm of { name : string; var_type : Core.Types.typ }
  | TartTypeForm of { name : string; params : string list }
  | ExprForm of { ty : Core.Types.typ }

type check_result = {
  env : Core.Type_env.t;  (** Final type environment with all bindings *)
  forms : form_result list;  (** Results for each top-level form *)
  errors : Unify.error list;  (** Any type errors encountered *)
  undefineds : Infer.undefined_var list;  (** Undefined variable references *)
  clause_diagnostics : Infer.resolved_clause_diagnostic list;
      (** Clause diagnostics emitted during multi-clause dispatch *)
  aliases : Sig.Sig_loader.alias_context;
      (** File-local type aliases from tart-type forms *)
}
(** Result of type-checking a program. *)

(** {1 Environments} *)

val default_env : unit -> Core.Type_env.t
(** Default environment with built-in function types. *)

(** {1 Type checking} *)

val check_form :
  Core.Type_env.t ->
  Syntax.Sexp.t ->
  Core.Type_env.t * form_result * Unify.error list * Infer.undefined_var list
(** [check_form env sexp] type-checks a single top-level form.

    Returns the updated environment (with new bindings from defun), the form
    result, any type errors encountered, and any undefined variable references.
*)

val check_program : ?env:Core.Type_env.t -> Syntax.Sexp.t list -> check_result
(** [check_program ?env forms] type-checks a sequence of top-level forms.

    Processes forms in order, accumulating type bindings from defuns. Uses the
    default environment with built-in types unless overridden. *)

val check_expr :
  ?env:Core.Type_env.t -> Syntax.Sexp.t -> Core.Types.typ * Unify.error list
(** [check_expr ?env sexp] type-checks a single expression.

    A convenience function for checking a single expression without accumulating
    environment changes. Returns the inferred type and any errors. Uses the
    default environment unless overridden. *)

(** {1 Pretty-printing} *)

val form_result_to_string : form_result -> string
(** Convert a form result to a string for display. *)
