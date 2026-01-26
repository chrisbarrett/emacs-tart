(** S-expression AST for Emacs Lisp.

    This module defines the core S-expression type used to represent
    parsed Elisp code. Reader macros are desugared during parsing:
    - ['x] becomes [(quote x)]
    - [`x] becomes [(backquote x)]
    - [,x] becomes [(unquote x)]
    - [,@x] becomes [(unquote-splicing x)]
    - [#'f] becomes [(function f)] *)

(** S-expression node types.

    Each node carries its source span for error reporting and IDE features. *)
type t =
  | Int of int * Location.span
  | Float of float * Location.span
  | String of string * Location.span
  | Symbol of string * Location.span
  | Keyword of string * Location.span  (** Keywords like [:foo] *)
  | Char of int * Location.span        (** Character literals [?a], [?\n], etc. *)
  | List of t list * Location.span
  | Vector of t list * Location.span   (** [#(...)] vectors *)
  | Cons of t * t * Location.span      (** Dotted pairs [(a . b)] *)
  | Error of string * Location.span    (** Error node for recovery *)

(** {1 Comparison} *)

val equal : t -> t -> bool

(** {1 Pretty-printing} *)

val pp : Format.formatter -> t -> unit
val show : t -> string

(** {1 Accessors} *)

val span_of : t -> Location.span
(** Get the source span of any S-expression. *)

val with_span : t -> Location.span -> t
(** [with_span sexp span] returns [sexp] with its span replaced. *)

(** {1 Serialization} *)

val to_string : t -> string
(** Pretty-print an S-expression in Elisp syntax (without locations).
    Reader macros are printed in their sugared form. *)

(** {1 Position Lookup} *)

val find_at_position : line:int -> col:int -> t -> t option
(** [find_at_position ~line ~col sexp] finds the innermost S-expression
    containing the given position. Position is 0-based (LSP convention). *)

val find_at_position_in_forms : line:int -> col:int -> t list -> t option
(** [find_at_position_in_forms ~line ~col forms] finds the innermost
    S-expression at a position across multiple top-level forms. *)

(** {1 Position Context} *)

(** Result of find_with_context: the target sexp and optionally the enclosing
    application if the target is in function position. *)
type position_context = {
  target : t;  (** The innermost sexp at the position *)
  enclosing_application : t option;  (** The enclosing [(fn args...)] if target is fn *)
}

val find_with_context : line:int -> col:int -> t -> position_context option
(** [find_with_context ~line ~col sexp] finds the innermost S-expression
    at a position, tracking whether it's in function position of an application. *)

val find_with_context_in_forms : line:int -> col:int -> t list -> position_context option
(** [find_with_context_in_forms ~line ~col forms] finds the innermost
    S-expression at a position across multiple forms, with context. *)
