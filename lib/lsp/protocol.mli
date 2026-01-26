(** LSP Protocol message types.

    Defines request/response types for the Language Server Protocol.
    Phase 1: initialize, initialized, shutdown. *)

(** {1 Initialize Request} *)

(** Client capabilities (simplified for Phase 1) *)
type client_capabilities = {
  text_document : text_document_client_capabilities option;
}

and text_document_client_capabilities = {
  synchronization : text_document_sync_client_capabilities option;
}

and text_document_sync_client_capabilities = {
  dynamic_registration : bool option;
}

(** Initialize request params *)
type initialize_params = {
  process_id : int option;
  root_uri : string option;
  capabilities : client_capabilities;
}

(** Text document sync kind *)
type text_document_sync_kind =
  | None_
  | Full
  | Incremental

(** Text document sync options *)
type text_document_sync_options = {
  open_close : bool;
  change : text_document_sync_kind;
}

(** Server capabilities *)
type server_capabilities = {
  text_document_sync : text_document_sync_options option;
  hover_provider : bool;
}

(** Initialize result *)
type initialize_result = {
  capabilities : server_capabilities;
}

(** {1 JSON Parsing} *)

(** Parse initialize params from JSON *)
val parse_initialize_params : Yojson.Safe.t -> initialize_params

(** {1 JSON Encoding} *)

(** Encode initialize result to JSON *)
val initialize_result_to_json : initialize_result -> Yojson.Safe.t

(** {1 Server Info} *)

(** Server info included in initialize response *)
type server_info = {
  name : string;
  version : string option;
}

(** Full initialize response with server info *)
val initialize_response_to_json :
  result:initialize_result -> server_info:server_info -> Yojson.Safe.t

(** {1 Diagnostics} *)

(** LSP diagnostic severity *)
type diagnostic_severity =
  | Error
  | Warning
  | Information
  | Hint

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

(** A diagnostic represents a compiler error or warning *)
type diagnostic = {
  range : range;
  severity : diagnostic_severity option;
  message : string;
  source : string option;
}

(** Parameters for textDocument/publishDiagnostics notification *)
type publish_diagnostics_params = {
  uri : string;
  version : int option;
  diagnostics : diagnostic list;
}

(** Encode publishDiagnostics params to JSON *)
val publish_diagnostics_params_to_json : publish_diagnostics_params -> Yojson.Safe.t

(** {1 Hover} *)

(** Markup content kind *)
type markup_kind =
  | PlainText
  | Markdown

(** Markup content for hover *)
type markup_content = {
  kind : markup_kind;
  value : string;
}

(** Hover result *)
type hover = {
  contents : markup_content;
  range : range option;
}

(** Hover params *)
type hover_params = {
  text_document : string;  (* URI *)
  position : position;
}

(** Parse hover params from JSON *)
val parse_hover_params : Yojson.Safe.t -> hover_params

(** Encode hover result to JSON *)
val hover_to_json : hover -> Yojson.Safe.t
