(** Source code excerpt rendering for error messages.

    Implements Spec 45: Source Excerpts in Error Messages. Provides Elm-style
    friendly errors showing source code at error sites with visual underlines.
*)

module Loc = Syntax.Location

(** Read lines from a file. Returns None if file unreadable.

    Per R1: Extract relevant lines from source file. Per R8: Graceful
    degradation for unreadable files. *)
let get_lines ~file ~start_line ~end_line : string list option =
  if file = "<generated>" || file = "" then None
  else
    try
      let ic = open_in file in
      let lines = ref [] in
      let line_num = ref 1 in
      (try
         while !line_num <= end_line do
           let line = input_line ic in
           if !line_num >= start_line then lines := line :: !lines;
           incr line_num
         done
       with End_of_file -> ());
      close_in ic;
      if !lines = [] then None else Some (List.rev !lines)
    with Sys_error _ -> None

(** Calculate gutter width for line numbers.

    Per R3: Gutter width adapts to largest line number displayed. *)
let gutter_width max_line = String.length (string_of_int max_line)

(** Format a line number for the gutter.

    Per R3: Show line numbers in consistent-width gutter. *)
let format_line_number ~width line_num =
  let num_str = string_of_int line_num in
  let padding = String.make (width - String.length num_str) ' ' in
  Ansi.line_number (padding ^ num_str)

(** Generate underline carets for a span within a line.

    Per R2: Show carets underneath pointing to the span. *)
let underline_for_span ~start_col ~end_col ~line_length =
  let start_col = max 0 start_col in
  let end_col = min line_length (max (start_col + 1) end_col) in
  let spaces = String.make start_col ' ' in
  let carets = String.make (end_col - start_col) '^' in
  spaces ^ Ansi.underline carets

(** Lisp keywords for syntax highlighting per R12. *)
let lisp_keywords =
  [
    "defun";
    "defvar";
    "defconst";
    "defmacro";
    "defcustom";
    "defgroup";
    "let";
    "let*";
    "letrec";
    "if";
    "when";
    "unless";
    "cond";
    "pcase";
    "pcase-let";
    "pcase-let*";
    "lambda";
    "function";
    "funcall";
    "apply";
    "progn";
    "prog1";
    "prog2";
    "save-excursion";
    "save-restriction";
    "save-match-data";
    "with-current-buffer";
    "with-temp-buffer";
    "setq";
    "setq-local";
    "setf";
    "and";
    "or";
    "not";
    "while";
    "dolist";
    "dotimes";
    "cl-loop";
    "catch";
    "throw";
    "condition-case";
    "unwind-protect";
    "declare";
    "interactive";
    "require";
    "provide";
  ]

(** Check if a word is a Lisp keyword. *)
let is_keyword word = List.mem word lisp_keywords

(** Simple Lisp syntax highlighting for a single line.

    Per R12: Apply minimal Lisp syntax highlighting. This is a simplified
    tokenizer that handles the common cases. *)
let highlight_lisp_line line =
  if not (Ansi.use_colors ()) then line
  else
    let buf = Buffer.create (String.length line * 2) in
    let len = String.length line in
    let i = ref 0 in

    while !i < len do
      let c = line.[!i] in
      match c with
      (* Comment - rest of line *)
      | ';' ->
          Buffer.add_string buf (Ansi.comment (String.sub line !i (len - !i)));
          i := len
      (* String literal *)
      | '"' ->
          let start = !i in
          incr i;
          while !i < len && line.[!i] <> '"' do
            if line.[!i] = '\\' && !i + 1 < len then i := !i + 2 else incr i
          done;
          if !i < len then incr i;
          Buffer.add_string buf
            (Ansi.string_lit (String.sub line start (!i - start)))
      (* Quote *)
      | '\'' | '`' ->
          Buffer.add_char buf c;
          incr i;
          (* Collect the following symbol *)
          let start = !i in
          while
            !i < len && not (List.mem line.[!i] [ ' '; '\t'; '('; ')'; '\n' ])
          do
            incr i
          done;
          if !i > start then
            Buffer.add_string buf
              (Ansi.quoted (String.sub line start (!i - start)))
      (* Number at start or after whitespace/paren *)
      | ('0' .. '9' | '-')
        when !i = 0 || List.mem line.[!i - 1] [ ' '; '\t'; '('; '\n' ] ->
          let start = !i in
          if c = '-' then incr i;
          while
            !i < len
            && ((line.[!i] >= '0' && line.[!i] <= '9') || line.[!i] = '.')
          do
            incr i
          done;
          (* Check it's actually a number, not just a dash *)
          if !i > start && (c <> '-' || !i > start + 1) then
            Buffer.add_string buf
              (Ansi.number (String.sub line start (!i - start)))
          else Buffer.add_string buf (String.sub line start (!i - start))
      (* Identifier - check for keyword *)
      | 'a' .. 'z' | 'A' .. 'Z' | '_' | '*' ->
          let start = !i in
          while
            !i < len && not (List.mem line.[!i] [ ' '; '\t'; '('; ')'; '\n' ])
          do
            incr i
          done;
          let word = String.sub line start (!i - start) in
          if is_keyword word then Buffer.add_string buf (Ansi.keyword word)
          else Buffer.add_string buf word
      (* Other characters *)
      | _ ->
          Buffer.add_char buf c;
          incr i
    done;
    Buffer.contents buf

(** Render a single source line with optional underline.

    Per R2, R3: Line with gutter and underline. *)
let render_line ~gutter_width ~line_num ~line ~underline_info =
  let gutter = format_line_number ~width:gutter_width line_num in
  let separator = Ansi.line_number " | " in
  let highlighted_line = highlight_lisp_line line in
  let line_str = gutter ^ separator ^ highlighted_line in
  match underline_info with
  | None -> line_str
  | Some (start_col, end_col) ->
      let underline =
        underline_for_span ~start_col ~end_col ~line_length:(String.length line)
      in
      let underline_gutter =
        String.make gutter_width ' ' ^ Ansi.line_number " | "
      in
      line_str ^ "\n" ^ underline_gutter ^ underline

(** Render source excerpt for a span.

    Per R1-R3: Extract lines and render with gutter and underlines. *)
let render_span (span : Loc.span) : string option =
  let start_line = span.start_pos.line in
  let end_line = span.end_pos.line in
  let file = span.start_pos.file in

  match get_lines ~file ~start_line ~end_line with
  | None -> None
  | Some lines ->
      let gutter_w = gutter_width end_line in
      let rendered =
        List.mapi
          (fun i line ->
            let line_num = start_line + i in
            let underline_info =
              if start_line = end_line then
                (* Single line: underline from start_col to end_col *)
                Some (span.start_pos.col, span.end_pos.col)
              else if line_num = start_line then
                (* First line of multi-line: underline from start_col to end *)
                Some (span.start_pos.col, String.length line)
              else if line_num = end_line then
                (* Last line of multi-line: underline from start to end_col *)
                Some (0, span.end_pos.col)
              else
                (* Middle line: underline entire line *)
                Some (0, String.length line)
            in
            render_line ~gutter_width:gutter_w ~line_num ~line ~underline_info)
          lines
      in
      Some (String.concat "\n" rendered)

(** Render source excerpt with label annotation.

    Useful for branch mismatches where we want to label each excerpt. *)
let render_span_with_label (span : Loc.span) ~label : string option =
  let start_line = span.start_pos.line in
  let end_line = span.end_pos.line in
  let file = span.start_pos.file in

  match get_lines ~file ~start_line ~end_line with
  | None -> None
  | Some lines ->
      let gutter_w = gutter_width end_line in
      let rendered =
        List.mapi
          (fun i line ->
            let line_num = start_line + i in
            let is_last = i = List.length lines - 1 in
            let underline_info =
              if start_line = end_line then
                Some (span.start_pos.col, span.end_pos.col)
              else if line_num = start_line then
                Some (span.start_pos.col, String.length line)
              else if line_num = end_line then Some (0, span.end_pos.col)
              else Some (0, String.length line)
            in
            let base =
              render_line ~gutter_width:gutter_w ~line_num ~line ~underline_info
            in
            if is_last then base ^ " " ^ Ansi.hint label else base)
          lines
      in
      Some (String.concat "\n" rendered)

(** Format file location for display.

    Per R4: Location shown in header line. *)
let format_location (span : Loc.span) : string =
  Ansi.location
    (Printf.sprintf "%s:%d:%d" span.start_pos.file span.start_pos.line
       (span.start_pos.col + 1))

(** Format header line for error type.

    Per R4: Elm-style header with dashes spanning to location. *)
let format_header ~error_type (span : Loc.span) : string =
  let location = format_location span in
  let error_label = Ansi.error ("-- " ^ error_type ^ " ") in
  (* Calculate dashes to fill to ~72 chars *)
  let visible_len = String.length ("-- " ^ error_type ^ " ") in
  let location_visible_len = String.length span.start_pos.file + 10 in
  (* approx *)
  let dashes_len = max 4 (72 - visible_len - location_visible_len - 1) in
  let dashes = String.make dashes_len '-' in
  error_label ^ Ansi.error dashes ^ " " ^ location

(** Generate conversational prose for different error contexts.

    Per R5: Use prose that explains the situation conversationally. *)
type prose_context =
  | FunctionArg of { fn_name : string; arg_index : int }
  | IfBranch of { is_then : bool }
  | DeclaredReturn of { fn_name : string }
  | TartAnnotation
  | NoContext

let intro_prose = function
  | FunctionArg _ -> "I found a type mismatch in this expression:"
  | IfBranch _ -> "The branches of this `if` have different types:"
  | DeclaredReturn _ -> "The return type doesn't match the declaration:"
  | TartAnnotation -> "The expression doesn't match the type annotation:"
  | NoContext -> "I found a type mismatch in this expression:"

let expected_prose ~expected = function
  | FunctionArg { fn_name; arg_index } ->
      Printf.sprintf "The function `%s` expects argument %d to be:\n\n    %s"
        fn_name (arg_index + 1) (Ansi.type_name expected)
  | IfBranch { is_then } ->
      let branch = if is_then then "else" else "then" in
      Printf.sprintf "The %s branch has type:\n\n    %s" branch
        (Ansi.type_name expected)
  | DeclaredReturn { fn_name } ->
      Printf.sprintf "`%s` is declared to return:\n\n    %s" fn_name
        (Ansi.type_name expected)
  | TartAnnotation ->
      Printf.sprintf "The annotation declares type:\n\n    %s"
        (Ansi.type_name expected)
  | NoContext -> Printf.sprintf "Expected:\n\n    %s" (Ansi.type_name expected)

let actual_prose ~actual = function
  | FunctionArg _ ->
      Printf.sprintf "But this expression has type:\n\n    %s"
        (Ansi.type_name actual)
  | IfBranch { is_then } ->
      let branch = if is_then then "then" else "else" in
      Printf.sprintf "But the %s branch has type:\n\n    %s" branch
        (Ansi.type_name actual)
  | DeclaredReturn _ ->
      Printf.sprintf "But the function body returns:\n\n    %s"
        (Ansi.type_name actual)
  | TartAnnotation ->
      Printf.sprintf "But the expression has type:\n\n    %s"
        (Ansi.type_name actual)
  | NoContext -> Printf.sprintf "But found:\n\n    %s" (Ansi.type_name actual)

(** Fallback when source is unavailable.

    Per R8: Graceful degradation without source excerpts. *)
let fallback_format ~expected ~actual : string =
  "(source not available)\n\n"
  ^ Printf.sprintf "Expected: %s\nFound: %s" (Ansi.type_name expected)
      (Ansi.type_name actual)
