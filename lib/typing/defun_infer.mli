(** Defun inference: type-check top-level definitions.

    Extracted from {!Infer} to reduce module size. Functions receive
    [~infer_lambda] or [~infer_progn] as parameters to break the mutual
    recursion with the main inference engine. *)

(** {1 Declaration extraction} *)

val extract_tart_declare :
  Syntax.Sexp.t list -> Syntax.Sexp.t option * Syntax.Sexp.t list
(** [extract_tart_declare body] extracts [(declare (tart TYPE))] from a defun
    body.

    Returns [(Some type_sexp, remaining_body)] if a tart declaration is found,
    or [(None, body)] if not. *)

val parse_tart_type : Syntax.Sexp.t -> Sig.Sig_ast.sig_type option
(** [parse_tart_type sexp] parses a tart type annotation.

    Returns [Some sig_type] on success, [None] on parse failure. *)

(** {1 Inference} *)

val infer_defun_as_expr :
  infer_lambda:
    (Core.Type_env.t ->
    Syntax.Sexp.t list ->
    Syntax.Sexp.t list ->
    Syntax.Location.span ->
    Infer_types.result) ->
  Core.Type_env.t ->
  string ->
  Syntax.Sexp.t list ->
  Syntax.Sexp.t list ->
  Syntax.Location.span ->
  Infer_types.result
(** [infer_defun_as_expr ~infer_lambda env name params body span] infers a defun
    as an expression.

    Returns [symbol] as the expression type (defun's return value is the
    function name). *)

val infer_defun :
  infer_progn:
    (Core.Type_env.t ->
    Syntax.Sexp.t list ->
    Syntax.Location.span ->
    Infer_types.result) ->
  Core.Type_env.t ->
  Syntax.Sexp.t ->
  Infer_types.defun_result option
(** [infer_defun ~infer_progn env sexp] infers the type of a defun and returns
    the binding info.

    Returns [Some] with the function name and type if [sexp] is a defun form, or
    [None] otherwise. If the body contains [(declare (tart TYPE))], the declared
    type is used and the body is checked against it. *)
