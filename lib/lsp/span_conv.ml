(** Span-to-LSP conversion utilities. *)

let range_of_span ~(text : string) (span : Syntax.Location.span) :
    Protocol.range =
  let start_line = span.start_pos.line - 1 in
  let end_line = span.end_pos.line - 1 in
  let start_char =
    match Document.line_text_at text start_line with
    | Some line_text ->
        Document.utf16_offset_of_byte ~line_text ~byte_offset:span.start_pos.col
    | None -> span.start_pos.col
  in
  let end_char =
    match Document.line_text_at text end_line with
    | Some line_text ->
        Document.utf16_offset_of_byte ~line_text ~byte_offset:span.end_pos.col
    | None -> span.end_pos.col
  in
  {
    Protocol.start = { line = start_line; character = start_char };
    end_ = { line = end_line; character = end_char };
  }

let location_of_span ~(text : string) (span : Syntax.Location.span) :
    Protocol.location =
  let uri = Uri.of_filename span.start_pos.file in
  { Protocol.uri; range = range_of_span ~text span }
