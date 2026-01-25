(** Source location tracking for error reporting and IDE features *)

type pos = {
  file : string;  (** File path *)
  line : int;  (** 1-based line number *)
  col : int;  (** 0-based column *)
  offset : int;  (** 0-based byte offset from start of file *)
}
[@@deriving show, eq]
(** A position in a source file *)

type span = { start_pos : pos; end_pos : pos } [@@deriving show, eq]
(** A span between two positions *)

(** Create a position *)
let make_pos ~file ~line ~col ~offset = { file; line; col; offset }

(** Create a span *)
let make_span ~start_pos ~end_pos = { start_pos; end_pos }

(** A dummy location for generated code *)
let dummy_pos = { file = "<generated>"; line = 0; col = 0; offset = 0 }

let dummy_span = { start_pos = dummy_pos; end_pos = dummy_pos }

(** Merge two spans to cover both *)
let merge s1 s2 =
  let start_pos =
    if s1.start_pos.offset <= s2.start_pos.offset then s1.start_pos
    else s2.start_pos
  in
  let end_pos =
    if s1.end_pos.offset >= s2.end_pos.offset then s1.end_pos else s2.end_pos
  in
  { start_pos; end_pos }

(** Convert a Lexing.position to our pos type *)
let pos_of_lexing file (lp : Lexing.position) =
  {
    file;
    line = lp.pos_lnum;
    col = lp.pos_cnum - lp.pos_bol;
    offset = lp.pos_cnum;
  }

(** Create a span from lexer start and end positions *)
let span_of_lexing file (start_lp : Lexing.position) (end_lp : Lexing.position)
    =
  {
    start_pos = pos_of_lexing file start_lp;
    end_pos = pos_of_lexing file end_lp;
  }
