(** Top-level reader API for Elisp S-expressions.

    This module provides the main entry points for parsing Elisp source code
    into S-expression ASTs. It supports error recovery, continuing to parse
    after encountering errors to produce partial results. *)

type parse_error = { message : string; span : Location.span }
(** A parse error with location information. *)

type parse_result = { sexps : Sexp.t list; errors : parse_error list }
(** Result of parsing, including both successful parses and errors. *)

(** {1 Parsing with error recovery} *)

val parse_string : ?filename:string -> string -> parse_result
(** [parse_string ?filename source] parses a string into S-expressions.
    Continues parsing after errors, collecting all errors encountered.
    @param filename Optional filename for error messages (default: ["<string>"])
*)

val parse_file : string -> parse_result
(** [parse_file filename] reads and parses a file into S-expressions. Continues
    parsing after errors, collecting all errors encountered. *)

(** {1 Parsing with exceptions} *)

val parse_string_exn : ?filename:string -> string -> Sexp.t list
(** [parse_string_exn ?filename source] parses a string into S-expressions.
    @raise Failure if any parse errors occur. *)

(** {1 Single expression parsing} *)

val parse_one : ?filename:string -> string -> (Sexp.t, string) result
(** [parse_one ?filename source] parses exactly one S-expression from a string.
    Returns [Error] if the input is empty, has errors, or contains multiple
    expressions. *)

val parse_one_exn : ?filename:string -> string -> Sexp.t
(** [parse_one_exn ?filename source] parses exactly one S-expression.
    @raise Failure
      if parsing fails or input doesn't contain exactly one expression. *)
