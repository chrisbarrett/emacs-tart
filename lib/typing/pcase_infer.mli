(** Pcase pattern matching and destructuring inference.

    Extracts bindings from pcase patterns and generates type constraints for
    pcase and pcase-let expressions. *)

open Core.Types

(** {1 Types} *)

type pattern_binding = { pb_name : string; pb_type : typ }
(** A binding extracted from a pcase pattern: name and its type. *)

(** {1 Pattern binding extraction} *)

val extract_pattern_bindings :
  Core.Type_env.t -> typ -> Syntax.Sexp.t -> pattern_binding list
(** [extract_pattern_bindings env scrutinee_ty pattern] extracts all variable
    bindings from a pcase pattern.

    Supports backquote ADT patterns, underscore wildcards, unquote bindings,
    [and]/[or] combinators, [let], [app], [pred], [guard], and [map] patterns.
*)

(** {1 Inference} *)

val infer_pcase :
  infer:(Core.Type_env.t -> Syntax.Sexp.t -> Infer_types.result) ->
  infer_progn:
    (Core.Type_env.t ->
    Syntax.Sexp.t list ->
    Syntax.Location.span ->
    Infer_types.result) ->
  Core.Type_env.t ->
  Syntax.Sexp.t ->
  Syntax.Sexp.t list ->
  Syntax.Location.span ->
  Infer_types.result
(** [infer_pcase ~infer ~infer_progn env expr clauses span] infers the type of a
    pcase expression.

    Each clause's pattern-bound variables are available in the body. All branch
    bodies must have the same type (the result type of the pcase). *)

val infer_pcase_let :
  infer:(Core.Type_env.t -> Syntax.Sexp.t -> Infer_types.result) ->
  infer_progn:
    (Core.Type_env.t ->
    Syntax.Sexp.t list ->
    Syntax.Location.span ->
    Infer_types.result) ->
  Core.Type_env.t ->
  Syntax.Sexp.t list ->
  Syntax.Sexp.t list ->
  Syntax.Location.span ->
  Infer_types.result
(** [infer_pcase_let ~infer ~infer_progn env bindings body span] infers the type
    of a pcase-let expression.

    Each binding destructures an expression via a pattern. Map patterns generate
    row type constraints on the expression. *)
