(** Backquote (quasiquote) evaluation.

    Handles the expansion of backquoted forms with unquote and unquote-splicing.
    Backquote allows constructing list structures with evaluated parts:

    - [`x] with no unquotes becomes [x] quoted
    - [`,y] evaluates [y]
    - [`,@ys] evaluates [ys] and splices the result
    - [`(a ,b ,@c d)] builds a list with [b] evaluated and [c] spliced

    This module is used during macro expansion to process backquoted
    template forms. *)

val eval_bq : Value.env -> Env.global -> Syntax.Sexp.t -> Value.value
(** [eval_bq env global sexp] evaluates a backquoted form.

    Entry point for backquote evaluation from the evaluator.
    Handles nested backquotes by tracking depth. *)
