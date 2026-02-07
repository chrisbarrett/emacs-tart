(** Output formatters and serializers for diagnostics.

    Provides human-readable, compact, and JSON output formats for
    {!Diagnostic.t} values. Separated from {!Diagnostic} to keep construction
    logic distinct from presentation. *)

module Loc = Syntax.Location
module Types = Core.Types

(** {1 Primitives} *)

val format_pos : Loc.pos -> string
(** Format a source position for display. *)

val format_span : Loc.span -> string
(** Format a source span for display. *)

val severity_to_int : Diagnostic.severity -> int
(** Map severity to LSP DiagnosticSeverity integer value.

    Error→1, Warning→2, Hint→4. *)

val format_severity : Diagnostic.severity -> string
(** Format severity for display. *)

val error_type_of_code : Diagnostic.error_code option -> string
(** Convert error code to error type string for header. *)

(** {1 Text formatters} *)

val to_string : Diagnostic.t -> string
(** Format a diagnostic as a human-readable string.

    Output format (similar to rustc/clang):
    {v
      error[E0001]: type mismatch: expected Int but found String
        --> file.el:10:5
        |
        = expected: Int
        = found: String
        |
      note: expected type from function signature
        --> file.el:5:1
        |
      help: convert the integer to a string: (number-to-string ...)
    v} *)

val to_string_human : Diagnostic.t -> string
(** Format a diagnostic in Elm-style human-readable format with source excerpts.

    Per Spec 45: Shows Elm-style headers, source excerpts with underlines,
    conversational prose, and colored output when TTY is detected. *)

val to_string_compact : Diagnostic.t -> string
(** Format a diagnostic in a compact single-line format.

    Useful for IDE integration and machine parsing. Format:
    [file:line:col: severity[CODE]: message [expected: T1, found: T2]] *)

val to_string_list : Diagnostic.t list -> string
(** Format multiple diagnostics. *)

(** {1 JSON Serialization} *)

val location_to_json : Loc.span -> Yojson.Safe.t
(** Serialize a source location to JSON. *)

val to_json : Diagnostic.t -> Yojson.Safe.t
(** Serialize a diagnostic to JSON.

    Output format:
    {v
      {
        "kind": "type",
        "code": "E0001",
        "severity": "error",
        "message": "type mismatch",
        "location": { "file": "init.el", "line": 42, "column": 10 },
        "expected": "String",
        "actual": "Int",
        "related": [...],
        "help": [...]
      }
    v} *)
