(** Type error diagnostics for user-facing error messages.

    This module provides structured diagnostic information for type errors,
    including source locations, expected vs actual types, and related locations
    (provenance) for understanding where types originated. *)

(** {1 Types} *)

(** Severity level for diagnostics. *)
type severity = Error | Warning | Hint

type related_location = { span : Syntax.Location.span; message : string }
(** A related location with context. *)

type t = {
  severity : severity;
  span : Syntax.Location.span;  (** Primary location of the error *)
  message : string;  (** Main error message *)
  expected : Core.Types.typ option;  (** Expected type (if applicable) *)
  actual : Core.Types.typ option;  (** Actual type found (if applicable) *)
  related : related_location list;  (** Related locations with context *)
}
(** A structured diagnostic message. *)

(** {1 Construction} *)

val type_mismatch :
  span:Syntax.Location.span ->
  expected:Core.Types.typ ->
  actual:Core.Types.typ ->
  ?related:related_location list ->
  unit ->
  t
(** Create a type mismatch diagnostic. *)

val arity_mismatch :
  span:Syntax.Location.span ->
  expected:int ->
  actual:int ->
  ?related:related_location list ->
  unit ->
  t
(** Create an arity mismatch diagnostic. *)

val occurs_check :
  span:Syntax.Location.span ->
  tvar_id:Core.Types.tvar_id ->
  typ:Core.Types.typ ->
  ?related:related_location list ->
  unit ->
  t
(** Create an occurs check (infinite type) diagnostic. *)

(** {1 Conversion from unification errors} *)

val of_unify_error : Unify.error -> t
(** Convert a unification error to a diagnostic. *)

val of_unify_errors : Unify.error list -> t list
(** Convert a list of unification errors to diagnostics. *)

(** {1 Formatting} *)

val format_pos : Syntax.Location.pos -> string
(** Format a source position for display. *)

val format_span : Syntax.Location.span -> string
(** Format a source span for display. *)

val format_severity : severity -> string
(** Format severity for display. *)

val to_string : t -> string
(** Format a diagnostic as a human-readable string.

    Output format (similar to rustc/clang):
    {v
      file.el:10:5: error: Type mismatch: expected Int but found String
        = expected: Int
        = found: String
        = note: expected type from function signature at file.el:5:1
    v} *)

val to_string_compact : t -> string
(** Format a diagnostic in a compact single-line format.

    Useful for IDE integration and machine parsing. Format:
    [file:line:col: severity: message [expected: T1, found: T2]] *)

val to_string_list : t list -> string
(** Format multiple diagnostics. *)

(** {1 Inspection} *)

val span : t -> Syntax.Location.span
(** Get the primary span of a diagnostic. *)

val all_spans : t -> Syntax.Location.span list
(** Get all spans (primary and related) from a diagnostic. *)

val is_error : t -> bool
(** Check if a diagnostic is an error (vs warning/hint). *)

val count_errors : t list -> int
(** Count errors in a list of diagnostics. *)
