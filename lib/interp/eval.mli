(** Core evaluator for the Elisp interpreter.

    Implements evaluation of Elisp expressions in a pure environment.
    Special forms are handled directly; function calls go through apply.

    This evaluator supports:
    - Core special forms: quote, if, let, let*, lambda, progn, setq, while, cond
    - Definition forms: defun, defmacro, defvar, defconst
    - Higher-order functions: funcall, apply, mapcar, mapc
    - Macro expansion: macroexpand, macroexpand-1 *)

(** {1 Errors} *)

(** Evaluation error with source location. *)
type eval_error = {
  message : string;
  span : Syntax.Location.span;
}

exception Eval_error of eval_error
(** Raised when evaluation encounters an error. *)

(** {1 Conversion} *)

val sexp_to_value : Syntax.Sexp.t -> Value.value
(** Convert an S-expression to a runtime value (for quote). *)

val value_to_sexp : Syntax.Location.span -> Value.value -> Syntax.Sexp.t
(** Convert a value back to an S-expression (for macro expansion).
    The span is used for the generated nodes. *)

(** {1 Evaluation} *)

val eval : Value.env -> Env.global -> Syntax.Sexp.t -> Value.value
(** [eval env global sexp] evaluates an S-expression in the given environment.
    @raise Eval_error on evaluation errors. *)

val apply : Env.global -> Value.value -> Value.value list -> (Value.value, string) result
(** [apply global fn args] applies a function to arguments.
    The function must be a closure or builtin. *)

(** {1 Macro expansion} *)

val expand_macro_1 : Env.global -> Syntax.Location.span -> string -> Syntax.Sexp.t -> Syntax.Sexp.t
(** [expand_macro_1 global span name form] expands a single macro call.
    Returns the original form if [name] is not a macro. *)

val macroexpand_all : Env.global -> Syntax.Sexp.t -> Syntax.Sexp.t
(** [macroexpand_all global sexp] recursively expands all macros in a form.
    Does not expand inside [quote]. *)

(** {1 Top-level evaluation} *)

val eval_toplevel : Env.global -> Syntax.Sexp.t -> (Value.value, eval_error) result
(** Evaluate a top-level form, catching exceptions. *)

val eval_string : Env.global -> string -> (Value.value, eval_error) result
(** Parse and evaluate a string of code.
    Returns the value of the last expression. *)

val make_interpreter : unit -> Env.global
(** Create a fresh interpreter state with all built-in functions loaded. *)
