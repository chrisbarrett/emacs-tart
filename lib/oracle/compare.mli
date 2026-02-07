(** Oracle comparison: tart parser vs Emacs reader.

    Parses input with tart, reads the same input with Emacs, and compares the
    canonical printed forms to detect discrepancies. *)

(** {1 Types} *)

type comparison_result =
  | Match
  | Mismatch of { tart_output : string; emacs_output : string }
  | Tart_error of Syntax.Read.parse_error
  | Emacs_error of Emacs_reader.emacs_error

(** {1 Comparison} *)

val compare_string : ?timeout_ms:int -> string -> comparison_result
(** [compare_string ?timeout_ms input] parses [input] with tart and reads it
    with Emacs, comparing the canonical [prin1-to-string] outputs. Returns
    [Match] when both agree, [Mismatch] when outputs differ, or the
    corresponding error variant if either reader fails. *)

val compare_file : ?timeout_ms:int -> string -> comparison_result list
(** [compare_file ?timeout_ms path] reads all top-level forms from [path] using
    both tart and Emacs, returning a per-form comparison. If Emacs fails
    entirely, returns a single-element list with [Emacs_error]. *)

(** {1 Normalisation} *)

val normalise : string -> string
(** [normalise s] applies light normalisation to a canonical Elisp printed form
    so that insignificant differences (trailing whitespace, float formatting)
    don't cause false mismatches. *)
