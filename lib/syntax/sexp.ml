(** S-expression AST for Emacs Lisp *)

open Location

(** S-expression node types.

    Each node carries its source span for error reporting and IDE features.
    Reader macros are desugared to their canonical forms:
    - 'x -> (quote x)
    - `x -> (backquote x)
    - ,x -> (unquote x)
    - ,@x -> (unquote-splicing x)
    - #'f -> (function f) *)
type t =
  | Int of int * span
  | Float of float * span
  | String of string * span
  | Symbol of string * span
  | Keyword of string * span  (** Keywords like :foo *)
  | Char of int * span  (** Character literals ?a, ?\n, etc. *)
  | List of t list * span
  | Vector of t list * span  (** #(...) vectors *)
  | Cons of t * t * span  (** Dotted pairs (a . b) *)
  | Error of string * span  (** Error node for recovery *)
[@@deriving show, eq]

(** Get the source span of any S-expression *)
let span_of = function
  | Int (_, s)
  | Float (_, s)
  | String (_, s)
  | Symbol (_, s)
  | Keyword (_, s)
  | Char (_, s)
  | List (_, s)
  | Vector (_, s)
  | Cons (_, _, s)
  | Error (_, s) ->
      s

(** Set the span of an S-expression *)
let with_span sexp span =
  match sexp with
  | Int (v, _) -> Int (v, span)
  | Float (v, _) -> Float (v, span)
  | String (v, _) -> String (v, span)
  | Symbol (v, _) -> Symbol (v, span)
  | Keyword (v, _) -> Keyword (v, span)
  | Char (v, _) -> Char (v, span)
  | List (v, _) -> List (v, span)
  | Vector (v, _) -> Vector (v, span)
  | Cons (a, b, _) -> Cons (a, b, span)
  | Error (v, _) -> Error (v, span)

(** Pretty-print an S-expression (without locations) *)
let rec to_string = function
  | Int (n, _) -> string_of_int n
  | Float (f, _) -> string_of_float f
  | String (s, _) -> Printf.sprintf "%S" s
  | Symbol (s, _) -> s
  | Keyword (s, _) -> ":" ^ s
  | Char (c, _) ->
      if c = Char.code '\\' then "?\\\\"
      else if c >= 32 && c < 127 then Printf.sprintf "?%c" (Char.chr c)
      else Printf.sprintf "?\\x%02x" c
  | List (elts, _) -> (
      (* Check for special forms that were reader macros *)
      match elts with
      | [ Symbol ("quote", _); x ] -> "'" ^ to_string x
      | [ Symbol ("backquote", _); x ] -> "`" ^ to_string x
      | [ Symbol ("unquote", _); x ] -> "," ^ to_string x
      | [ Symbol ("unquote-splicing", _); x ] -> ",@" ^ to_string x
      | [ Symbol ("function", _); x ] -> "#'" ^ to_string x
      | _ -> "(" ^ String.concat " " (List.map to_string elts) ^ ")")
  | Vector (elts, _) -> "#(" ^ String.concat " " (List.map to_string elts) ^ ")"
  | Cons (car, cdr, _) ->
      (* Handle improper lists: collect elements until we hit a non-Cons tail *)
      let rec collect acc = function
        | Cons (a, b, _) -> collect (a :: acc) b
        | tail -> (List.rev acc, tail)
      in
      let elts, tail = collect [ car ] cdr in
      "("
      ^ String.concat " " (List.map to_string elts)
      ^ " . " ^ to_string tail ^ ")"
  | Error (msg, _) -> Printf.sprintf "#<error: %s>" msg
