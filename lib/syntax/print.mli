(** AST printer for Sexp.t to valid Emacs Lisp.

    Produces output that:
    - Re-parses to the same AST (semantic equivalence)
    - Is valid Elisp accepted by Emacs reader
    - Correctly escapes strings and characters *)

(** {1 Character Modifiers} *)

module Modifiers : sig
  val meta_bit : int
  val control_bit : int
  val shift_bit : int
  val hyper_bit : int
  val super_bit : int
  val alt_bit : int

  val char_mask : int
  (** Mask to extract base character from modified char. *)

  val extract :
    int -> int * [ `Alt | `Control | `Hyper | `Meta | `Shift | `Super ] list
  (** Extract base character and modifiers from a character code. *)

  val is_control_char : int -> bool
  (** Check if a character is a control character (0-31). *)

  val control_to_letter : int -> char
  (** Convert control character to its base letter (e.g., 24 -> 'x'). *)
end

(** {1 String Escaping} *)

val escape_string_char : char -> string
(** Escape a single character for string output. *)

val escape_string : string -> string
(** Escape a string for Elisp output. *)

(** {1 Character Printing} *)

val print_char : int -> string
(** Print a character literal with proper escapes and modifiers.

    Examples:
    - [print_char 97] = ["?a"]
    - [print_char 10] = ["?\\n"]
    - [print_char 24] = ["?\\C-x"]
    - [print_char (0x8000000 lor 120)] = ["?\\M-x"] *)

(** {1 S-expression Printing} *)

val to_string : Sexp.t -> string
(** Print an S-expression to valid Emacs Lisp.

    Output is semantically equivalent (re-parses to same AST).

    Examples:
    - [to_string (Int (42, _))] = ["42"]
    - [to_string (List ([Symbol ("quote", _); Symbol ("x", _)], _))] = ["'x"]
    - [to_string (Vector ([Int (1, _); Int (2, _)], _))] = ["#(1 2)"] *)

val to_strings : Sexp.t list -> string list
(** Print multiple S-expressions to a string list. *)
