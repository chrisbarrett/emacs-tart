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
  | Io of { path : string; message : string }  (** File I/O error *)
  | Cli of { message : string; hint : string option }  (** CLI usage error *)

(** {1 Recoverability} *)

val is_fatal : t -> bool
(** [is_fatal err] returns [true] if processing should stop immediately.

    Fatal errors (Io, Cli) prevent further processing. Recoverable errors (Type,
    Parse, Eval) allow collecting more errors. *)

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
