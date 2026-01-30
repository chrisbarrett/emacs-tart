(** AST printer for Sexp.t to valid Emacs Lisp.

    Produces output that:
    - Re-parses to the same AST (semantic equivalence)
    - Is valid Elisp accepted by Emacs reader
    - Correctly escapes strings and characters *)

(** Character modifier bits used by Emacs. *)
module Modifiers = struct
  let meta_bit = 0x8000000
  let control_bit = 0x4000000 (* For explicit control marking *)
  let shift_bit = 0x2000000
  let hyper_bit = 0x1000000
  let super_bit = 0x800000
  let alt_bit = 0x400000

  (** Mask to extract base character from modified char. *)
  let char_mask = 0x1FFFFF (* 21 bits for Unicode codepoints *)

  (** Extract modifiers from a character code. *)
  let extract code =
    let base = code land char_mask in
    let mods = [] in
    let mods = if code land meta_bit <> 0 then `Meta :: mods else mods in
    let mods = if code land control_bit <> 0 then `Control :: mods else mods in
    let mods = if code land shift_bit <> 0 then `Shift :: mods else mods in
    let mods = if code land hyper_bit <> 0 then `Hyper :: mods else mods in
    let mods = if code land super_bit <> 0 then `Super :: mods else mods in
    let mods = if code land alt_bit <> 0 then `Alt :: mods else mods in
    (base, List.rev mods)

  (** Check if a character is a control character (0-31). *)
  let is_control_char c = c >= 0 && c <= 31

  (** Convert control character to its base letter (e.g., 24 -> 'x'). *)
  let control_to_letter c =
    if c >= 0 && c <= 26 then Char.chr (c + 64)
      (* ^@ through ^Z -> @ through Z *)
    else Char.chr (c + 64)
end

(** Escape a single character for string output.

    Returns the escaped form suitable for inside double quotes. *)
let escape_string_char c =
  match c with
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | '\r' -> "\\r"
  | '\\' -> "\\\\"
  | '"' -> "\\\""
  | '\007' -> "\\a" (* bell *)
  | '\b' -> "\\b" (* backspace *)
  | '\012' -> "\\f" (* form feed *)
  | '\027' -> "\\e" (* escape *)
  | c when Char.code c >= 32 && Char.code c < 127 -> String.make 1 c
  | c when Char.code c >= 128 -> String.make 1 c (* UTF-8 pass-through *)
  | c -> Printf.sprintf "\\x%02x" (Char.code c)

(** Escape a string for Elisp output. *)
let escape_string s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c -> Buffer.add_string buf (escape_string_char c)) s;
  Buffer.contents buf

(** Print a character literal.

    Handles:
    - Simple printable characters: ?a
    - Escape sequences: ?\n, ?\t, ?\\
    - Control characters: ?\C-x
    - Modifier bits: ?\M-x, ?\C-\M-x *)
let print_char code =
  let base, mods = Modifiers.extract code in

  (* Build modifier prefix *)
  let mod_prefix =
    let parts =
      List.map
        (function
          | `Meta -> "\\M-"
          | `Control -> "\\C-"
          | `Shift -> "\\S-"
          | `Hyper -> "\\H-"
          | `Alt -> "\\A-"
          | `Super -> "\\s-")
        mods
    in
    String.concat "" parts
  in

  (* Check if base is a control character that needs \C- notation *)
  let base_str, needs_control =
    if Modifiers.is_control_char base && not (List.mem `Control mods) then
      (* Control character without explicit control modifier - add \C- *)
      let letter = Modifiers.control_to_letter base in
      (String.make 1 letter, true)
    else
      (* Regular character or explicit control modifier *)
      match base with
      | 10 -> ("\\n", false)
      | 9 -> ("\\t", false)
      | 13 -> ("\\r", false)
      | 92 -> ("\\\\", false)
      | 32 -> ("\\s", false) (* space as \s to avoid ambiguity *)
      | 127 -> ("\\d", false) (* delete *)
      | c when c >= 32 && c < 127 -> (String.make 1 (Char.chr c), false)
      | c when c >= 128 ->
          (* UTF-8 character - use \x escape for safety *)
          (Printf.sprintf "\\x%02x" c, false)
      | c -> (Printf.sprintf "\\x%02x" c, false)
  in

  if needs_control then "?\\" ^ "C-" ^ mod_prefix ^ base_str
  else "?" ^ mod_prefix ^ base_str

(** Print an S-expression to valid Emacs Lisp. *)
let rec to_string (sexp : Sexp.t) : string =
  match sexp with
  | Sexp.Int (n, _) -> string_of_int n
  | Sexp.Float (f, _) -> string_of_float f
  | Sexp.String (s, _) -> "\"" ^ escape_string s ^ "\""
  | Sexp.Symbol (s, _) -> s
  | Sexp.Keyword (s, _) -> ":" ^ s
  | Sexp.Char (c, _) -> print_char c
  | Sexp.List (elts, _) -> (
      (* Check for special forms that were reader macros *)
      match elts with
      | [ Sexp.Symbol ("quote", _); x ] -> "'" ^ to_string x
      | [ Sexp.Symbol ("backquote", _); x ] -> "`" ^ to_string x
      | [ Sexp.Symbol ("unquote", _); x ] -> "," ^ to_string x
      | [ Sexp.Symbol ("unquote-splicing", _); x ] -> ",@" ^ to_string x
      | [ Sexp.Symbol ("function", _); x ] -> "#'" ^ to_string x
      | _ -> "(" ^ String.concat " " (List.map to_string elts) ^ ")")
  | Sexp.Vector (elts, _) ->
      "#(" ^ String.concat " " (List.map to_string elts) ^ ")"
  | Sexp.Cons (car, cdr, _) ->
      (* Handle improper lists: collect elements until we hit a non-Cons tail *)
      let rec collect acc = function
        | Sexp.Cons (a, b, _) -> collect (a :: acc) b
        | tail -> (List.rev acc, tail)
      in
      let elts, tail = collect [ car ] cdr in
      "("
      ^ String.concat " " (List.map to_string elts)
      ^ " . " ^ to_string tail ^ ")"
  | Sexp.Error (msg, _) -> Printf.sprintf "#<error: %s>" msg

(** Print an S-expression list to multiple lines. *)
let to_strings (sexps : Sexp.t list) : string list = List.map to_string sexps
