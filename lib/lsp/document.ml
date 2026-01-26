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

(** {1 Incremental Changes} *)

type content_change = { range : range option; text : string }

(** Convert a (line, character) position to a byte offset in text. Returns None
    if position is out of range. *)
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
          let line_len = String.length line in
          (* Allow character position up to line_len + 1 to handle cursor at EOL *)
          if pos.character <= line_len then Some (char_offset + pos.character)
          else if pos.character = line_len + 1 && rest = [] then
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
