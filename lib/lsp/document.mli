(** Document manager for LSP.

    Stores document contents and applies incremental updates. Implements
    textDocument/didOpen, didChange, didClose. *)

(** {1 Document} *)

type doc = { uri : string; version : int; text : string }
(** A document with its contents and version *)

(** {1 Position and Range} *)

type position = { line : int; character : int }
(** A position in a document (0-based line and character) *)

type range = { start : position; end_ : position }
(** A range in a document *)

(** {1 Document Store} *)

type t
(** The document store *)

val create : unit -> t
(** Create an empty document store *)

val open_doc : t -> uri:string -> version:int -> text:string -> unit
(** Open a document with initial content *)

val close_doc : t -> uri:string -> unit
(** Close a document *)

val get_doc : t -> string -> doc option
(** Get a document by URI *)

val list_uris : t -> string list
(** List all open document URIs *)

(** {1 UTF-16 Position Encoding} *)

val utf16_offset_of_byte : line_text:string -> byte_offset:int -> int
(** Convert a byte offset within a line to UTF-16 code units. Clamps to the end
    of the line if [byte_offset] exceeds the line length. *)

val byte_offset_of_utf16 : line_text:string -> utf16_offset:int -> int
(** Convert a UTF-16 code-unit offset within a line to a byte offset. Clamps to
    the end of the line if [utf16_offset] exceeds the line length. *)

(** {1 Incremental Changes} *)

type content_change = {
  range : range option;  (** None for full document replacement *)
  text : string;
}
(** A content change event *)

val apply_changes :
  t -> uri:string -> version:int -> content_change list -> (unit, string) result
(** Apply incremental changes to a document. Returns Error if document not found
    or position out of range. *)

(** {1 JSON Parsing} *)

val position_of_json : Yojson.Safe.t -> position
(** Parse position from JSON *)

val range_of_json : Yojson.Safe.t -> range
(** Parse range from JSON *)

val content_change_of_json : Yojson.Safe.t -> content_change
(** Parse content change from JSON *)

val text_document_identifier_of_json : Yojson.Safe.t -> string
(** Parse text document identifier from JSON (returns URI) *)

val versioned_text_document_identifier_of_json : Yojson.Safe.t -> string * int
(** Parse versioned text document identifier from JSON *)

val text_document_item_of_json : Yojson.Safe.t -> string * int * string
(** Parse text document item from JSON (for didOpen) *)
