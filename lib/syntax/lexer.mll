{
(** Ocamllex lexer for Emacs Lisp S-expressions *)

exception Lexer_error of string * Location.span

(** Create a span from the lexbuf *)
let span_of_lexbuf lexbuf =
  Location.span_of_lexing (Parse_state.get_filename ())
    (Lexing.lexeme_start_p lexbuf)
    (Lexing.lexeme_end_p lexbuf)

(** Decode a hex digit *)
let hex_digit c =
  match c with
  | '0'..'9' -> Char.code c - Char.code '0'
  | 'a'..'f' -> Char.code c - Char.code 'a' + 10
  | 'A'..'F' -> Char.code c - Char.code 'A' + 10
  | _ -> failwith "invalid hex digit"

(** Decode a hex escape sequence *)
let hex_value s start len =
  let rec loop acc i =
    if i >= start + len then acc
    else loop (acc * 16 + hex_digit s.[i]) (i + 1)
  in
  loop 0 start

(** Decode an octal digit *)
let octal_digit c =
  Char.code c - Char.code '0'

(** Decode an octal escape sequence *)
let octal_value s start len =
  let rec loop acc i =
    if i >= start + len then acc
    else loop (acc * 8 + octal_digit s.[i]) (i + 1)
  in
  loop 0 start

(** Unescape backslash-escaped characters in symbols *)
let unescape_symbol sym =
  let buf = Buffer.create (String.length sym) in
  let len = String.length sym in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else if sym.[i] = '\\' && i + 1 < len then begin
      Buffer.add_char buf sym.[i + 1];
      loop (i + 2)
    end else begin
      Buffer.add_char buf sym.[i];
      loop (i + 1)
    end
  in
  loop 0

(** Process string escape sequences *)
let unescape_string s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else if s.[i] = '\\' && i + 1 < len then
      let next = s.[i + 1] in
      match next with
      | 'n' -> Buffer.add_char buf '\n'; loop (i + 2)
      | 't' -> Buffer.add_char buf '\t'; loop (i + 2)
      | 'r' -> Buffer.add_char buf '\r'; loop (i + 2)
      | '\\' -> Buffer.add_char buf '\\'; loop (i + 2)
      | '"' -> Buffer.add_char buf '"'; loop (i + 2)
      | 'a' -> Buffer.add_char buf '\007'; loop (i + 2)  (* bell *)
      | 'b' -> Buffer.add_char buf '\b'; loop (i + 2)
      | 'f' -> Buffer.add_char buf '\012'; loop (i + 2)  (* form feed *)
      | 'v' -> Buffer.add_char buf '\011'; loop (i + 2)  (* vertical tab *)
      | 'e' -> Buffer.add_char buf '\027'; loop (i + 2)  (* escape *)
      | 's' -> Buffer.add_char buf ' '; loop (i + 2)     (* space *)
      | 'd' -> Buffer.add_char buf '\127'; loop (i + 2)  (* delete *)
      | 'x' ->
        (* Hex escape \xNN - read as many hex digits as available *)
        let rec read_hex j acc =
          if j < len then
            match s.[j] with
            | '0'..'9' | 'a'..'f' | 'A'..'F' ->
              read_hex (j + 1) (acc * 16 + hex_digit s.[j])
            | _ -> (j, acc)
          else (j, acc)
        in
        let (j, value) = read_hex (i + 2) 0 in
        if j = i + 2 then begin
          Buffer.add_char buf '\\';
          Buffer.add_char buf 'x';
          loop (i + 2)
        end else begin
          Buffer.add_char buf (Char.chr (value land 0xFF));
          loop j
        end
      | 'u' when i + 5 < len ->
        (* Unicode escape \uNNNN *)
        let value = hex_value s (i + 2) 4 in
        (* Simple ASCII-only handling for now *)
        if value < 128 then Buffer.add_char buf (Char.chr value)
        else Buffer.add_string buf (Printf.sprintf "\\u%04x" value);
        loop (i + 6)
      | '0'..'7' ->
        (* Octal escape \NNN *)
        let rec count_octal j =
          if j < len && j - i <= 4 then
            match s.[j] with
            | '0'..'7' -> count_octal (j + 1)
            | _ -> j - i - 1
          else j - i - 1
        in
        let num_digits = min 3 (count_octal (i + 1)) in
        let value = octal_value s (i + 1) num_digits in
        Buffer.add_char buf (Char.chr (value land 0xFF));
        loop (i + 1 + num_digits)
      | '\n' ->
        (* Line continuation - skip the backslash and newline *)
        loop (i + 2)
      | _ ->
        (* Unknown escape, keep as-is *)
        Buffer.add_char buf '\\';
        Buffer.add_char buf next;
        loop (i + 2)
    else begin
      Buffer.add_char buf s.[i];
      loop (i + 1)
    end
  in
  loop 0

(** Parse a character literal value *)
let rec parse_char_literal s =
  let len = String.length s in
  if len = 0 then failwith "empty character literal"
  else if s.[0] = '\\' then
    if len = 1 then failwith "incomplete escape"
    else
      match s.[1] with
      (* Modifier keys - check these first since they have guards *)
      | 'C' when len >= 3 && s.[2] = '-' ->
        (* Control character ?\C-x *)
        let base_char =
          if len > 3 then parse_char_literal (String.sub s 3 (len - 3))
          else failwith "incomplete control sequence"
        in
        base_char land 0x1F
      | 'M' when len >= 3 && s.[2] = '-' ->
        (* Meta character ?\M-x *)
        let base_char =
          if len > 3 then parse_char_literal (String.sub s 3 (len - 3))
          else failwith "incomplete meta sequence"
        in
        base_char lor 0x8000000  (* Emacs meta bit *)
      | 'S' when len >= 3 && s.[2] = '-' ->
        (* Shift character ?\S-x *)
        let base_char =
          if len > 3 then parse_char_literal (String.sub s 3 (len - 3))
          else failwith "incomplete shift sequence"
        in
        base_char lor 0x2000000  (* Emacs shift bit *)
      | 'H' when len >= 3 && s.[2] = '-' ->
        (* Hyper character ?\H-x *)
        let base_char =
          if len > 3 then parse_char_literal (String.sub s 3 (len - 3))
          else failwith "incomplete hyper sequence"
        in
        base_char lor 0x1000000  (* Emacs hyper bit *)
      | 'A' when len >= 3 && s.[2] = '-' ->
        (* Alt character ?\A-x *)
        let base_char =
          if len > 3 then parse_char_literal (String.sub s 3 (len - 3))
          else failwith "incomplete alt sequence"
        in
        base_char lor 0x400000  (* Emacs alt bit *)
      | 's' when len >= 3 && s.[2] = '-' ->
        (* Super character ?\s-x *)
        let base_char =
          if len > 3 then parse_char_literal (String.sub s 3 (len - 3))
          else failwith "incomplete super sequence"
        in
        base_char lor 0x800000  (* Emacs super bit *)
      | '^' when len >= 2 ->
        (* Alternative control syntax ?\^x *)
        let base_char =
          if len > 2 then Char.code s.[2]
          else failwith "incomplete control sequence"
        in
        base_char land 0x1F
      (* Simple escapes *)
      | 'n' -> Char.code '\n'
      | 't' -> Char.code '\t'
      | 'r' -> Char.code '\r'
      | '\\' -> Char.code '\\'
      | 'a' -> 7   (* bell *)
      | 'b' -> 8   (* backspace *)
      | 'f' -> 12  (* form feed *)
      | 'v' -> 11  (* vertical tab *)
      | 'e' -> 27  (* escape *)
      | 's' -> 32  (* space *)
      | 'd' -> 127 (* delete *)
      | '(' | ')' | '[' | ']' | '"' | '\'' | '`' | ',' | '.' | ';' | '#' | '?' as c ->
        Char.code c
      | 'x' ->
        (* Hex escape *)
        hex_value s 2 (len - 2)
      | 'u' ->
        (* Unicode escape \uNNNN *)
        hex_value s 2 4
      | 'U' ->
        (* Extended unicode escape \UNNNNNNNN *)
        hex_value s 2 8
      | '0'..'7' ->
        (* Octal escape *)
        octal_value s 1 (len - 1)
      | _ -> failwith ("unknown escape: \\" ^ String.make 1 s.[1])
  else
    Char.code s.[0]

}

let whitespace = [' ' '\t' '\r' '\012']  (* includes form-feed *)
let newline = '\n'
let digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let octal_digit = ['0'-'7']

(* Elisp symbols can contain many special characters *)
(* UTF-8 multi-byte sequences: 2-byte starts with 110xxxxx, 3-byte with 1110xxxx, 4-byte with 11110xxx *)
(* Continuation bytes are 10xxxxxx (0x80-0xBF) *)
let utf8_2byte = ['\xC0'-'\xDF'] ['\x80'-'\xBF']
let utf8_3byte = ['\xE0'-'\xEF'] ['\x80'-'\xBF'] ['\x80'-'\xBF']
let utf8_4byte = ['\xF0'-'\xF7'] ['\x80'-'\xBF'] ['\x80'-'\xBF'] ['\x80'-'\xBF']
let utf8_char = utf8_2byte | utf8_3byte | utf8_4byte

let symbol_start = ['a'-'z' 'A'-'Z' '_' '+' '-' '*' '/' '<' '>' '=' '!' '&' '%' '^' '~' '@' '$' '|' ':'] | utf8_char
(* Escaped characters in symbols: \# means literal #, etc. *)
let symbol_escape = '\\' _
let symbol_cont = symbol_start | symbol_escape | digit | ['?' '.']

(* Numbers *)
let int_literal = '-'? digit+
(* Float literals can have optional leading digits (e.g., .5 or 0.5) *)
let float_literal = '-'? (digit+ '.' digit* | '.' digit+) (['e' 'E'] ['+' '-']? digit+)?
let hex_literal = ("0x" | "#x") hex_digit+
let octal_literal = ("0o" | "#o") octal_digit+
let binary_literal = "#b" ['0' '1']+

rule token = parse
  (* Whitespace *)
  | whitespace+    { token lexbuf }
  | newline        { Lexing.new_line lexbuf; token lexbuf }

  (* Comments *)
  | ';' [^'\n']*   { token lexbuf }  (* Line comments *)
  | "#|"           { block_comment 1 lexbuf }

  (* Reader macros *)
  | "\\,@"         { Parser.COMMA_AT }  (* Escaped unquote-splicing *)
  | "\\,"          { Parser.COMMA }     (* Escaped unquote *)
  | ",@"           { Parser.COMMA_AT }
  | ","            { Parser.COMMA }
  | "'"            { Parser.QUOTE }
  | "`"            { Parser.BACKQUOTE }
  | "#'"           { Parser.HASH_QUOTE }
  | "#("           { Parser.HASH_LPAREN }
  | "#s("          { Parser.HASH_S_LPAREN }  (* Record/hash-table literal *)

  (* Bool-vector literal #&length"string" - parse as opaque value *)
  | "#&" digit+    { bool_vector_string lexbuf }

  (* Char-table and sub-char-table literals - opaque internal representation *)
  | "#^^["         { char_table_body 1 lexbuf }  (* Sub-char-table *)
  | "#^["          { char_table_body 1 lexbuf }  (* Char-table *)

  (* Delimiters *)
  | "("            { Parser.LPAREN }
  | ")"            { Parser.RPAREN }
  | "["            { Parser.LBRACKET }
  | "]"            { Parser.RBRACKET }

  (* Character literals *)
  (* Space character literal ? (question mark followed by space) *)
  | "? "           { Parser.CHAR 32 }
  (* UTF-8 character literals like ?', ?«, etc. - decode the Unicode codepoint *)
  | "?" utf8_char as s
                   { (* Decode UTF-8 to get codepoint *)
                     let bytes = String.sub s 1 (String.length s - 1) in
                     let len = String.length bytes in
                     let code =
                       if len = 2 then
                         ((Char.code bytes.[0] land 0x1F) lsl 6) lor
                         (Char.code bytes.[1] land 0x3F)
                       else if len = 3 then
                         ((Char.code bytes.[0] land 0x0F) lsl 12) lor
                         ((Char.code bytes.[1] land 0x3F) lsl 6) lor
                         (Char.code bytes.[2] land 0x3F)
                       else if len = 4 then
                         ((Char.code bytes.[0] land 0x07) lsl 18) lor
                         ((Char.code bytes.[1] land 0x3F) lsl 12) lor
                         ((Char.code bytes.[2] land 0x3F) lsl 6) lor
                         (Char.code bytes.[3] land 0x3F)
                       else
                         Char.code bytes.[0]
                     in
                     Parser.CHAR code }
  (* Simple characters like ?a, ?A, ?0, ?, etc. - most common case *)
  | "?" [^ ' ' '\t' '\n' '\r' '\\'] as s
                   { Parser.CHAR (parse_char_literal (String.sub s 1 (String.length s - 1))) }
  (* Escaped characters with modifiers like ?\C-x, ?\M-x, ?\s-x (super) etc. *)
  | "?" ("\\" ['C' 'M' 'S' 'H' 'A' 's'] '-')+ ("\\" _? | [^ ' ' '\t' '\n' '\r']) as s
                   { Parser.CHAR (parse_char_literal (String.sub s 1 (String.length s - 1))) }
  (* \s (space) - match 3 chars and check if next is dash *)
  | "?\\s"         { (* Check if followed by dash (super modifier) vs standalone (space) *)
                     (* We only consumed "?\s" - peek at next char if any *)
                     let c = try Bytes.get lexbuf.lex_buffer lexbuf.lex_curr_pos with _ -> '\000' in
                     if c = '-' then begin
                       (* It's the start of super modifier like ?\s-x, need more *)
                       (* Back up and let the modifier rule match *)
                       lexbuf.lex_curr_pos <- lexbuf.lex_start_pos;
                       lexbuf.lex_curr_p <- lexbuf.lex_start_p;
                       raise (Lexer_error ("incomplete super modifier", span_of_lexbuf lexbuf))
                     end else
                       Parser.CHAR 32 (* space *) }
  (* Simple escapes like ?\n, ?\t, ?\\ etc. (excluding modifiers C M S H A s and hex/unicode) *)
  | "?\\" [^ 'x' '0'-'7' 'C' 'M' 'S' 'H' 'A' 's' 'u' 'U'] as s
                   { Parser.CHAR (parse_char_literal (String.sub s 1 (String.length s - 1))) }
  (* Octal escapes like ?\012 *)
  | "?\\" (octal_digit octal_digit? octal_digit?) as s
                   { Parser.CHAR (parse_char_literal (String.sub s 1 (String.length s - 1))) }
  (* Hex escapes like ?\x1b *)
  | "?\\x" hex_digit+ as s
                   { Parser.CHAR (parse_char_literal (String.sub s 1 (String.length s - 1))) }
  (* Unicode escapes like ?\u0041, ?\U00000041 *)
  | "?\\u" hex_digit hex_digit hex_digit hex_digit as s
                   { Parser.CHAR (parse_char_literal (String.sub s 1 (String.length s - 1))) }
  | "?\\U" hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit as s
                   { Parser.CHAR (parse_char_literal (String.sub s 1 (String.length s - 1))) }

  (* Numbers - try float first since it's more specific *)
  | float_literal as s
                   { Parser.FLOAT (float_of_string s) }
  | hex_literal as s
                   { (* Handle both 0x and #x formats *)
                     let s' = if s.[0] = '#' then "0" ^ String.sub s 1 (String.length s - 1) else s in
                     Parser.INT (int_of_string s') }
  | octal_literal as s
                   { (* Handle both 0o and #o formats *)
                     let s' = if s.[0] = '#' then "0" ^ String.sub s 1 (String.length s - 1) else s in
                     Parser.INT (int_of_string s') }
  | binary_literal as s
                   { (* #bNNN binary literal - parse manually *)
                     let digits = String.sub s 2 (String.length s - 2) in
                     let value = String.fold_left (fun acc c -> acc * 2 + (if c = '1' then 1 else 0)) 0 digits in
                     Parser.INT value }
  | int_literal as s
                   { Parser.INT (int_of_string s) }

  (* Strings *)
  | '"'            { string_token (Buffer.create 64) (Lexing.lexeme_start_p lexbuf) lexbuf }

  (* Dot - must come before symbol to handle (a . b) *)
  | '.' (whitespace | newline | '(' | ')' | '[' | ']' | eof)
                   {
                     (* Put back the following character *)
                     let len = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
                     if len > 1 then begin
                       let lexeme = Lexing.lexeme lexbuf in
                       if lexeme.[1] = '\n' then Lexing.new_line lexbuf;
                       (* We need to back up one character *)
                       lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1;
                       lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
                         pos_cnum = lexbuf.lex_curr_p.pos_cnum - 1
                       }
                     end;
                     Parser.DOT
                   }

  (* Keywords :foo *)
  | ':' (symbol_cont+) as s
                   { Parser.KEYWORD (String.sub s 1 (String.length s - 1)) }

  (* Symbols *)
  | symbol_start symbol_cont* as s
                   { Parser.SYMBOL (unescape_symbol s) }

  (* Also allow symbols starting with a number if followed by symbol chars *)
  | digit+ symbol_start symbol_cont* as s
                   { Parser.SYMBOL (unescape_symbol s) }

  (* Dot-prefixed symbols like .foo (used with let-alist) *)
  | '.' symbol_start symbol_cont* as s
                   { Parser.SYMBOL (unescape_symbol s) }

  (* Ellipsis symbol ... *)
  | "..."          { Parser.SYMBOL "..." }

  | eof            { Parser.EOF }

  | _ as c         {
      let span = span_of_lexbuf lexbuf in
      raise (Lexer_error (Printf.sprintf "unexpected character: %c" c, span))
    }

and string_token buf start_pos = parse
  | '"'            {
      Parser.STRING (unescape_string (Buffer.contents buf))
    }
  | "\\\n"         { Lexing.new_line lexbuf; string_token buf start_pos lexbuf }
  | '\\' _         { Buffer.add_string buf (Lexing.lexeme lexbuf); string_token buf start_pos lexbuf }
  | '\n'           { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; string_token buf start_pos lexbuf }
  | [^ '"' '\\' '\n']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); string_token buf start_pos lexbuf }
  | eof            {
      let span = Location.span_of_lexing (Parse_state.get_filename ()) start_pos (Lexing.lexeme_end_p lexbuf) in
      raise (Lexer_error ("unterminated string", span))
    }

and block_comment depth = parse
  | "|#"           { if depth = 1 then token lexbuf else block_comment (depth - 1) lexbuf }
  | "#|"           { block_comment (depth + 1) lexbuf }
  | '\n'           { Lexing.new_line lexbuf; block_comment depth lexbuf }
  | _              { block_comment depth lexbuf }
  | eof            {
      let span = span_of_lexbuf lexbuf in
      raise (Lexer_error ("unterminated block comment", span))
    }

(* Bool-vector literal: expects a string following #&N *)
and bool_vector_string = parse
  | '"'            { bool_vector_string_body (Buffer.create 16) lexbuf }
  | _              {
      let span = span_of_lexbuf lexbuf in
      raise (Lexer_error ("expected string after bool-vector length", span))
    }

and bool_vector_string_body buf = parse
  | '"'            { Parser.BOOL_VECTOR }  (* Ignore contents, just parse as opaque type *)
  | '\\' _         { Buffer.add_string buf (Lexing.lexeme lexbuf); bool_vector_string_body buf lexbuf }
  | [^ '"' '\\']   { Buffer.add_string buf (Lexing.lexeme lexbuf); bool_vector_string_body buf lexbuf }
  | eof            {
      let span = span_of_lexbuf lexbuf in
      raise (Lexer_error ("unterminated bool-vector literal", span))
    }

(* Char-table body: skip contents until matching ] is found *)
(* These are opaque internal data structures - we just need to parse past them *)
and char_table_body depth = parse
  | ']'            { if depth = 1 then Parser.BOOL_VECTOR (* reuse opaque token *) else char_table_body (depth - 1) lexbuf }
  | '['            { char_table_body (depth + 1) lexbuf }
  | "#^^["         { char_table_body (depth + 1) lexbuf }  (* Nested sub-char-table *)
  | "#^["          { char_table_body (depth + 1) lexbuf }  (* Nested char-table *)
  | '"'            { char_table_string lexbuf; char_table_body depth lexbuf }  (* Skip strings inside *)
  | '\n'           { Lexing.new_line lexbuf; char_table_body depth lexbuf }
  | [^ ']' '[' '"' '\n' '#']+ { char_table_body depth lexbuf }  (* Skip other content *)
  | '#'            { char_table_body depth lexbuf }  (* Single # not starting special syntax *)
  | eof            {
      let span = span_of_lexbuf lexbuf in
      raise (Lexer_error ("unterminated char-table literal", span))
    }

(* Skip a string inside a char-table *)
and char_table_string = parse
  | '"'            { () }
  | '\\' _         { char_table_string lexbuf }
  | '\n'           { Lexing.new_line lexbuf; char_table_string lexbuf }
  | [^ '"' '\\' '\n']+ { char_table_string lexbuf }
  | eof            {
      let span = span_of_lexbuf lexbuf in
      raise (Lexer_error ("unterminated string in char-table", span))
    }
