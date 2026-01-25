(** Source location tracking for error reporting and IDE features.

    This module provides types and utilities for tracking source positions
    and spans in parsed files. Every AST node carries location information
    for precise error messages and editor integration. *)

(** A position in a source file. *)
type pos = {
  file : string;  (** File path *)
  line : int;     (** 1-based line number *)
  col : int;      (** 0-based column *)
  offset : int;   (** 0-based byte offset from start of file *)
}

(** A span between two positions. *)
type span = {
  start_pos : pos;
  end_pos : pos;
}

(** {1 Comparison} *)

val equal_pos : pos -> pos -> bool
val equal_span : span -> span -> bool

(** {1 Pretty-printing} *)

val pp_pos : Format.formatter -> pos -> unit
val pp_span : Format.formatter -> span -> unit
val show_pos : pos -> string
val show_span : span -> string

(** {1 Constructors} *)

val make_pos : file:string -> line:int -> col:int -> offset:int -> pos
(** Create a position with explicit values. *)

val make_span : start_pos:pos -> end_pos:pos -> span
(** Create a span between two positions. *)

val dummy_pos : pos
(** A dummy position for generated code. *)

val dummy_span : span
(** A dummy span for generated code. *)

(** {1 Operations} *)

val merge : span -> span -> span
(** [merge s1 s2] returns a span covering both input spans. *)

(** {1 Conversion from Lexing} *)

val pos_of_lexing : string -> Lexing.position -> pos
(** [pos_of_lexing file lp] converts a [Lexing.position] to our [pos] type. *)

val span_of_lexing : string -> Lexing.position -> Lexing.position -> span
(** [span_of_lexing file start_lp end_lp] creates a span from lexer positions. *)
