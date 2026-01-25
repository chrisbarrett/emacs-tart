(** Shared state between lexer and parser.

    This module manages mutable state that needs to be shared between
    the lexer and parser during parsing, such as the current filename. *)

val set_filename : string -> unit
(** Set the current filename being parsed. *)

val get_filename : unit -> string
(** Get the current filename being parsed. *)
