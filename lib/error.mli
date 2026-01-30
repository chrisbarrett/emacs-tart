(** Unified error type system for composable, machine-readable errors.

    This module provides a single error type that wraps all error kinds (type
    errors, parse errors, I/O errors, CLI errors) with uniform serialization and
    recoverability classification. *)

(** {1 Error Types} *)

(** Unified error type wrapping all subsystem errors. *)
type t =
  | Type of Typing.Diagnostic.t  (** Type checking error *)
  | Parse of { message : string; span : Syntax.Location.span }
      (** Parse error with location *)
  | Eval of { message : string; span : Syntax.Location.span }
      (** Evaluation error with location *)
  | Io of { path : string; message : string }  (** Generic file I/O error *)
  | File of Errors.File_error.t
      (** Structured file I/O error with suggestions *)
  | Cli of { message : string; hint : string option }  (** CLI usage error *)

(** {1 Recoverability} *)

val is_fatal : t -> bool
(** [is_fatal err] returns [true] if processing should stop immediately.

    Fatal errors (Io, File, Cli) prevent further processing. Recoverable errors
    (Type, Parse, Eval) allow collecting more errors. *)

(** {1 Location Access} *)

val location : t -> Syntax.Location.span option
(** [location err] returns the primary source span if available.

    Type, Parse, and Eval errors have locations; Io and Cli do not. *)

(** {1 Construction} *)

val parse_error : message:string -> span:Syntax.Location.span -> t
(** Create a parse error. *)

val eval_error : message:string -> span:Syntax.Location.span -> t
(** Create an evaluation error. *)

val io_error : path:string -> exn:exn -> t
(** Create an I/O error from a system exception.

    Extracts a human-readable message from the exception. *)

val cli_error : message:string -> ?hint:string -> unit -> t
(** Create a CLI usage error with optional hint. *)

val of_diagnostic : Typing.Diagnostic.t -> t
(** Wrap a type diagnostic as an error. *)

val of_diagnostics : Typing.Diagnostic.t list -> t list
(** Wrap multiple type diagnostics as errors. *)

val of_file_error : Errors.File_error.t -> t
(** Convert a structured file error to a unified error. *)

(** {1 Formatting} *)

val to_string : t -> string
(** Format an error as a human-readable string.

    Output format follows compiler conventions:
    {v
      error[E0308]: type mismatch
        --> init.el:42:10
        |
        = expected: String
        = found: Int
        |
      help: convert the integer to a string: (number-to-string ...)
    v}

    For CLI errors:
    {v
      error: no input files
      hint: use --help for usage
    v}

    For I/O errors:
    {v
      error: init.el: No such file or directory
    v} *)

val to_json : t -> Yojson.Safe.t
(** Serialize an error to JSON.

    Output format:
    {v
      {
        "kind": "type",
        "code": "E0308",
        "severity": "error",
        "message": "type mismatch",
        "location": {...},
        ...
      }
    v}

    Different error kinds have different fields:
    - Type: full diagnostic with expected/actual/related/help
    - Parse/Eval: message + location
    - Io: path + message
    - Cli: message + optional hint *)

(** {1 Error Accumulator} *)

type error = t
(** Alias for the error type to use within submodules. *)

module Acc : sig
  type 'a t
  (** Accumulator for collecting multiple errors. *)

  val empty : 'a t
  (** Create an empty accumulator. *)

  val add : error -> 'a t -> 'a t
  (** Add an error to the accumulator. *)

  val add_list : error list -> 'a t -> 'a t
  (** Add multiple errors to the accumulator. *)

  val to_list : 'a t -> error list
  (** Get accumulated errors in the order they were added. *)

  val has_errors : 'a t -> bool
  (** Check if any errors have been accumulated. *)
end

(** {1 Reporting} *)

val report : t list -> unit
(** Report errors to stderr with summary count.

    Prints each error followed by a summary line showing the count. Example:
    {v
      error[E0308]: type mismatch
        --> init.el:42:10
        ...

      error[E0425]: variable `strng` is not defined
        --> init.el:50:5
        ...

      Found 2 errors
    v} *)

val report_json : t list -> unit
(** Report errors as JSON to stdout.

    Outputs a JSON array of error objects:
    {v
      [
        {
          "kind": "type",
          "code": "E0308",
          ...
        },
        ...
      ]
    v} *)
