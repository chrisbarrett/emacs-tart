(** Document manager for LSP.

    Stores document contents and applies incremental updates.
    Implements textDocument/didOpen, didChange, didClose. *)

(** {1 Document} *)

(** A document with its contents and version *)
type doc = {
  uri : string;
  version : int;
  text : string;
}

(** {1 Position and Range} *)

(** A position in a document (0-based line and character) *)
type position = {
  line : int;
  character : int;
}

(** A range in a document *)
type range = {
  start : position;
  end_ : position;
}

(** {1 Document Store} *)

(** The document store *)
type t

(** Create an empty document store *)
val create : unit -> t

(** Open a document with initial content *)
val open_doc : t -> uri:string -> version:int -> text:string -> unit

(** Close a document *)
val close_doc : t -> uri:string -> unit

(** Get a document by URI *)
val get_doc : t -> string -> doc option

(** List all open document URIs *)
val list_uris : t -> string list

(** {1 Incremental Changes} *)

(** A content change event *)
type content_change = {
  range : range option;  (** None for full document replacement *)
  text : string;
}

(** Apply incremental changes to a document.
    Returns Error if document not found or position out of range. *)
val apply_changes :
  t -> uri:string -> version:int -> content_change list -> (unit, string) result

(** {1 JSON Parsing} *)

(** Parse position from JSON *)
val position_of_json : Yojson.Safe.t -> position

(** Parse range from JSON *)
val range_of_json : Yojson.Safe.t -> range

(** Parse content change from JSON *)
val content_change_of_json : Yojson.Safe.t -> content_change

(** Parse text document identifier from JSON (returns URI) *)
val text_document_identifier_of_json : Yojson.Safe.t -> string

(** Parse versioned text document identifier from JSON *)
val versioned_text_document_identifier_of_json : Yojson.Safe.t -> string * int

(** Parse text document item from JSON (for didOpen) *)
val text_document_item_of_json : Yojson.Safe.t -> string * int * string
