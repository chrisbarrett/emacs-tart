(** Top-level reader API for Elisp S-expressions *)

type parse_error = { message : string; span : Location.span }
(** Parse error with location *)

type parse_result = { sexps : Sexp.t list; errors : parse_error list }
(** Result of parsing *)

(** Convert Menhir location to our span *)
let span_of_positions file (start_pos, end_pos) =
  Location.span_of_lexing file start_pos end_pos

(** Parse a string into S-expressions with error recovery *)
let parse_string ?(filename = "<string>") source =
  Parse_state.set_filename filename;
  let lexbuf = Lexing.from_string source in
  Lexing.set_filename lexbuf filename;

  let rec parse_all acc errors =
    try
      match Parser.sexp_opt Lexer.token lexbuf with
      | None -> { sexps = List.rev acc; errors = List.rev errors }
      | Some sexp -> parse_all (sexp :: acc) errors
    with
    | Lexer.Lexer_error (msg, span) ->
        let error = { message = msg; span } in
        (* Skip the problematic character and continue *)
        if Lexing.lexeme_end lexbuf < String.length source then (
          lexbuf.Lexing.lex_curr_pos <- Lexing.lexeme_end lexbuf + 1;
          parse_all acc (error :: errors))
        else { sexps = List.rev acc; errors = List.rev (error :: errors) }
    | Parser.Error ->
        let start_pos = Lexing.lexeme_start_p lexbuf in
        let end_pos = Lexing.lexeme_end_p lexbuf in
        let span = span_of_positions filename (start_pos, end_pos) in
        let error = { message = "syntax error"; span } in
        (* Try to recover by skipping to next valid position *)
        if Lexing.lexeme_end lexbuf < String.length source then (
          (* Skip current token *)
          lexbuf.Lexing.lex_curr_pos <- Lexing.lexeme_end lexbuf;
          parse_all acc (error :: errors))
        else { sexps = List.rev acc; errors = List.rev (error :: errors) }
  in
  parse_all [] []

(** Parse a file into S-expressions *)
let parse_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let source = really_input_string ic n in
  close_in ic;
  parse_string ~filename source

(** Parse a string, raising on error (for simple use cases) *)
let parse_string_exn ?(filename = "<string>") source =
  let result = parse_string ~filename source in
  match result.errors with
  | [] -> result.sexps
  | err :: _ ->
      let loc = err.span.start_pos in
      failwith
        (Printf.sprintf "%s:%d:%d: %s" loc.file loc.line loc.col err.message)

(** Parse a single S-expression from a string *)
let parse_one ?(filename = "<string>") source =
  let result = parse_string ~filename source in
  match (result.errors, result.sexps) with
  | [], [ sexp ] -> Ok sexp
  | [], [] -> Error "empty input"
  | [], _ :: _ :: _ -> Error "multiple expressions in input"
  | err :: _, _ -> Error err.message

(** Parse a single S-expression, raising on error *)
let parse_one_exn ?(filename = "<string>") source =
  match parse_one ~filename source with
  | Ok sexp -> sexp
  | Error msg -> failwith msg
