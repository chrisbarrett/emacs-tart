(** Source code excerpt rendering for error messages.

    Implements Spec 45: Source Excerpts in Error Messages. Provides Elm-style
    friendly errors showing source code at error sites with visual underlines.
*)

module Loc = Syntax.Location

(** {1 Source Line Extraction (R1)} *)

val get_lines :
  file:string -> start_line:int -> end_line:int -> string list option
(** Read lines from a file. Returns None if file unreadable. Per R1: Extract
    relevant lines from source file. Per R8: Graceful degradation for unreadable
    files. *)

(** {1 Excerpt Rendering (R2, R3)} *)

val render_span : Loc.span -> string option
(** Render source excerpt for a span with line numbers and underlines. Returns
    None if source file cannot be read.

    Per R2: Show lines with carets underneath pointing to the span. Per R3: Show
    line numbers in a consistent-width gutter. *)

val render_span_with_label : Loc.span -> label:string -> string option
(** Render source excerpt with a label annotation after the underline. Useful
    for branch mismatches where we want to label each excerpt. *)

(** {1 Formatting Helpers (R4)} *)

val format_header : error_type:string -> Loc.span -> string
(** Format Elm-style header with dashes spanning to location.

    Per R4: Use a friendly header with dashes.
    Example: {v -- TYPE MISMATCH -------------------------------- init.el:42:10 v} *)

val format_location : Loc.span -> string
(** Format file location for display (colored if TTY). *)

(** {1 Conversational Prose (R5)} *)

type prose_context =
  | FunctionArg of { fn_name : string; arg_index : int }
  | IfBranch of { is_then : bool }
  | DeclaredReturn of { fn_name : string }
  | TartAnnotation
  | NoContext  (** Context for generating conversational prose. *)

val intro_prose : prose_context -> string
(** Generate introduction prose for the error context. *)

val expected_prose : expected:string -> prose_context -> string
(** Generate prose explaining the expected type. *)

val actual_prose : actual:string -> prose_context -> string
(** Generate prose explaining the actual type found. *)

(** {1 Fallback (R8)} *)

val fallback_format : expected:string -> actual:string -> string
(** Fallback format when source is unavailable. *)

(** {1 Syntax Highlighting (R12)} *)

val highlight_lisp_line : string -> string
(** Apply minimal Lisp syntax highlighting to a source line. Returns plain text
    if colors are disabled. *)
