%{
(** Menhir parser for Emacs Lisp S-expressions *)

open Sexp

(** Convert Menhir position pair to our span - uses filename from shared state *)
let span_of_loc (start_pos, end_pos) =
  Location.span_of_lexing (Parse_state.get_filename ()) start_pos end_pos

(** Make a list span that includes delimiters *)
let list_span ~lparen ~rparen =
  Location.merge (span_of_loc lparen) (span_of_loc rparen)
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> SYMBOL
%token <string> KEYWORD
%token <int> CHAR
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token DOT
%token QUOTE BACKQUOTE COMMA COMMA_AT
%token HASH_QUOTE HASH_LPAREN HASH_S_LPAREN
%token BOOL_VECTOR
%token EOF

(* Entry point returns a list of S-expressions *)
%start <Sexp.t list> file
%start <Sexp.t option> sexp_opt

%%

file:
  | sexps = list(sexp); EOF { sexps }

sexp_opt:
  | s = sexp { Some s }
  | EOF { None }

sexp:
  | i = located(INT) { Int (fst i, snd i) }
  | f = located(FLOAT) { Float (fst f, snd f) }
  | s = located(STRING) { String (fst s, snd s) }
  | s = located(SYMBOL) { Symbol (fst s, snd s) }
  | k = located(KEYWORD) { Keyword (fst k, snd k) }
  | c = located(CHAR) { Char (fst c, snd c) }
  | BOOL_VECTOR { Symbol ("bool-vector", span_of_loc $loc) }  (* opaque type *)
  | l = list_sexp { l }
  | v = vector { v }
  | q = quoted { q }

list_sexp:
  | LPAREN; elts = list(sexp); DOT; last = sexp; RPAREN
    {
      let lp_span = $loc($1) in
      let rp_span = $loc($5) in
      let full_span = list_span ~lparen:lp_span ~rparen:rp_span in
      (* Build a cons chain for dotted lists *)
      let rec build_dotted = function
        | [] -> last
        | [x] -> Cons (x, last, full_span)
        | x :: xs -> Cons (x, build_dotted xs, full_span)
      in
      build_dotted elts
    }
  | LPAREN; elts = list(sexp); RPAREN
    {
      let lp_span = $loc($1) in
      let rp_span = $loc($3) in
      List (elts, list_span ~lparen:lp_span ~rparen:rp_span)
    }

vector:
  | HASH_LPAREN; elts = list(sexp); RPAREN
    {
      let lp_span = $loc($1) in
      let rp_span = $loc($3) in
      Vector (elts, list_span ~lparen:lp_span ~rparen:rp_span)
    }
  | LBRACKET; elts = list(sexp); RBRACKET
    {
      let lp_span = $loc($1) in
      let rp_span = $loc($3) in
      Vector (elts, list_span ~lparen:lp_span ~rparen:rp_span)
    }
  (* Record/hash-table literal #s(type ...) - parse as list with special marker *)
  | HASH_S_LPAREN; elts = list(sexp); RPAREN
    {
      let lp_span = $loc($1) in
      let rp_span = $loc($3) in
      let span = list_span ~lparen:lp_span ~rparen:rp_span in
      (* Wrap in (record ...) form for type-checker to handle *)
      List (Symbol ("record", span_of_loc lp_span) :: elts, span)
    }

quoted:
  | QUOTE; s = sexp
    {
      let q_span = span_of_loc $loc($1) in
      let span = Location.merge q_span (Sexp.span_of s) in
      List ([Symbol ("quote", q_span); s], span)
    }
  | BACKQUOTE; s = sexp
    {
      let q_span = span_of_loc $loc($1) in
      let span = Location.merge q_span (Sexp.span_of s) in
      List ([Symbol ("backquote", q_span); s], span)
    }
  | COMMA; s = sexp
    {
      let q_span = span_of_loc $loc($1) in
      let span = Location.merge q_span (Sexp.span_of s) in
      List ([Symbol ("unquote", q_span); s], span)
    }
  | COMMA_AT; s = sexp
    {
      let q_span = span_of_loc $loc($1) in
      let span = Location.merge q_span (Sexp.span_of s) in
      List ([Symbol ("unquote-splicing", q_span); s], span)
    }
  | HASH_QUOTE; s = sexp
    {
      let q_span = span_of_loc $loc($1) in
      let span = Location.merge q_span (Sexp.span_of s) in
      List ([Symbol ("function", q_span); s], span)
    }

(* Helper to get both value and location *)
located(X):
  | x = X { (x, span_of_loc $loc) }

%%
