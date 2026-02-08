(** Document manager for LSP.

    Stores document contents and applies incremental updates. Implements
    textDocument/didOpen, didChange, didClose. *)

(** {1 Document} *)

type doc = { uri : string; version : int; text : string }

(** {1 Position and Range} *)

type position = { line : int; character : int }
type range = { start : position; end_ : position }

(** {1 Document Store} *)

type t = (string, doc) Hashtbl.t

let create () : t = Hashtbl.create 16

let open_doc (store : t) ~(uri : string) ~(version : int) ~(text : string) :
    unit =
  Hashtbl.replace store uri { uri; version; text }

let close_doc (store : t) ~(uri : string) : unit = Hashtbl.remove store uri
let get_doc (store : t) (uri : string) : doc option = Hashtbl.find_opt store uri

let list_uris (store : t) : string list =
  Hashtbl.fold (fun uri _ acc -> uri :: acc) store []

(** {1 UTF-16 Position Encoding} *)

(** Decode the length of a UTF-8 sequence from its leading byte. Returns the
    number of bytes in the sequence, or 1 for invalid/continuation bytes
    (treating them as single units). *)
let utf8_seq_len (byte : char) : int =
  let b = Char.code byte in
  if b land 0x80 = 0 then 1
  else if b land 0xE0 = 0xC0 then 2
  else if b land 0xF0 = 0xE0 then 3
  else if b land 0xF8 = 0xF0 then 4
  else 1

(** Number of UTF-16 code units for a codepoint encoded in the given number of
    UTF-8 bytes. 4-byte sequences (supplementary plane) require a surrogate pair
    (2 code units); everything else is 1. *)
let utf16_code_units_of_seq_len (seq_len : int) : int =
  if seq_len = 4 then 2 else 1

let utf16_offset_of_byte ~(line_text : string) ~(byte_offset : int) : int =
  let len = String.length line_text in
  let target = min byte_offset len in
  let rec walk byte_pos utf16_pos =
    if byte_pos >= target then utf16_pos
    else
      let seq_len = utf8_seq_len (String.get line_text byte_pos) in
      let units = utf16_code_units_of_seq_len seq_len in
      let next = min (byte_pos + seq_len) len in
      if next > target then utf16_pos else walk next (utf16_pos + units)
  in
  walk 0 0

let byte_offset_of_utf16 ~(line_text : string) ~(utf16_offset : int) : int =
  let len = String.length line_text in
  let rec walk byte_pos utf16_pos =
    if utf16_pos >= utf16_offset || byte_pos >= len then byte_pos
    else
      let seq_len = utf8_seq_len (String.get line_text byte_pos) in
      let units = utf16_code_units_of_seq_len seq_len in
      let next = min (byte_pos + seq_len) len in
      walk next (utf16_pos + units)
  in
  walk 0 0

let line_text_at (text : string) (line_number : int) : string option =
  if line_number < 0 then None
  else
    let len = String.length text in
    let rec find_line current_line pos =
      if current_line = line_number then
        (* Find the end of this line *)
        let end_pos =
          match String.index_from_opt text pos '\n' with
          | Some nl -> nl
          | None -> len
        in
        Some (String.sub text pos (end_pos - pos))
      else
        match String.index_from_opt text pos '\n' with
        | Some nl -> find_line (current_line + 1) (nl + 1)
        | None -> None
    in
    if len = 0 && line_number = 0 then Some "" else find_line 0 0

let utf16_col_to_byte ~(text : string) ~(line : int) ~(col : int) : int =
  match line_text_at text line with
  | Some line_text -> byte_offset_of_utf16 ~line_text ~utf16_offset:col
  | None -> col

(** {1 Incremental Changes} *)

type content_change = { range : range option; text : string }

(** Convert a (line, character) position to a byte offset in text. The
    [character] field is interpreted as a UTF-16 code-unit offset and converted
    to bytes. Returns None if position is out of range. *)
let position_to_offset (text : string) (pos : position) : int option =
  let lines = String.split_on_char '\n' text in
  let rec find_offset line_num char_offset lines =
    match lines with
    | [] ->
        (* Past end of document - could be appending at the very end *)
        if line_num = pos.line && pos.character = 0 then Some char_offset
        else None
    | line :: rest ->
        if line_num = pos.line then
          let byte_char =
            byte_offset_of_utf16 ~line_text:line ~utf16_offset:pos.character
          in
          let line_len = String.length line in
          (* Allow character position up to line_len + 1 to handle cursor at EOL *)
          if byte_char <= line_len then Some (char_offset + byte_char)
          else if byte_char = line_len + 1 && rest = [] then
            (* Special case: at very end of last line (after implicit \n) *)
            Some (char_offset + line_len)
          else None
        else
          (* Add line length + 1 for the newline character *)
          find_offset (line_num + 1) (char_offset + String.length line + 1) rest
  in
  find_offset 0 0 lines

(** Apply a single content change to document text. Returns Error if position is
    out of range. *)
let apply_single_change (text : string) (change : content_change) :
    (string, string) result =
  match change.range with
  | None ->
      (* Full document replacement *)
      Ok change.text
  | Some range -> (
      match
        (position_to_offset text range.start, position_to_offset text range.end_)
      with
      | Some start_offset, Some end_offset ->
          if start_offset > end_offset then Error "Invalid range: start > end"
          else if end_offset > String.length text then
            Error "Range extends past end of document"
          else
            let before = String.sub text 0 start_offset in
            let after =
              String.sub text end_offset (String.length text - end_offset)
            in
            Ok (before ^ change.text ^ after)
      | None, _ -> Error "Start position out of range"
      | _, None -> Error "End position out of range")

let apply_changes (store : t) ~(uri : string) ~(version : int)
    (changes : content_change list) : (unit, string) result =
  match get_doc store uri with
  | None -> Error (Printf.sprintf "Document not open: %s" uri)
  | Some doc -> (
      let rec apply_all text = function
        | [] -> Ok text
        | change :: rest -> (
            match apply_single_change text change with
            | Ok new_text -> apply_all new_text rest
            | Error e -> Error e)
      in
      match apply_all doc.text changes with
      | Ok new_text ->
          Hashtbl.replace store uri { uri; version; text = new_text };
          Ok ()
      | Error e -> Error e)

(** {1 JSON Parsing} *)

let position_of_json (json : Yojson.Safe.t) : position =
  let open Yojson.Safe.Util in
  {
    line = json |> member "line" |> to_int;
    character = json |> member "character" |> to_int;
  }

let range_of_json (json : Yojson.Safe.t) : range =
  let open Yojson.Safe.Util in
  {
    start = json |> member "start" |> position_of_json;
    end_ = json |> member "end" |> position_of_json;
  }

let content_change_of_json (json : Yojson.Safe.t) : content_change =
  let open Yojson.Safe.Util in
  let range =
    match json |> member "range" with
    | `Null -> None
    | r -> Some (range_of_json r)
  in
  let text = json |> member "text" |> to_string in
  { range; text }

let text_document_identifier_of_json (json : Yojson.Safe.t) : string =
  let open Yojson.Safe.Util in
  json |> member "uri" |> to_string

let versioned_text_document_identifier_of_json (json : Yojson.Safe.t) :
    string * int =
  let open Yojson.Safe.Util in
  let uri = json |> member "uri" |> to_string in
  let version = json |> member "version" |> to_int in
  (uri, version)

let text_document_item_of_json (json : Yojson.Safe.t) : string * int * string =
  let open Yojson.Safe.Util in
  let uri = json |> member "uri" |> to_string in
  let version = json |> member "version" |> to_int in
  let text = json |> member "text" |> to_string in
  (uri, version, text)
