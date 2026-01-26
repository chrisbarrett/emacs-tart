(** LSP Protocol message types.

    Defines request/response types for the Language Server Protocol. Phase 1:
    initialize, initialized, shutdown. *)

(** {1 Initialize Request} *)

type client_capabilities = {
  text_document : text_document_client_capabilities option;
}
(** Client capabilities (simplified for Phase 1) *)

and text_document_client_capabilities = {
  synchronization : text_document_sync_client_capabilities option;
}

and text_document_sync_client_capabilities = {
  dynamic_registration : bool option;
}

type initialize_params = {
  process_id : int option;
  root_uri : string option;
  capabilities : client_capabilities;
}
(** Initialize request params *)

(** Text document sync kind *)
type text_document_sync_kind = None_ | Full | Incremental

type text_document_sync_options = {
  open_close : bool;
  change : text_document_sync_kind;
}
(** Text document sync options *)

type server_capabilities = {
  text_document_sync : text_document_sync_options option;
  hover_provider : bool;
  definition_provider : bool;
  references_provider : bool;
}
(** Server capabilities *)

type initialize_result = { capabilities : server_capabilities }
(** Initialize result *)

(** {1 JSON Parsing} *)

val parse_initialize_params : Yojson.Safe.t -> initialize_params
(** Parse initialize params from JSON *)

(** {1 JSON Encoding} *)

val initialize_result_to_json : initialize_result -> Yojson.Safe.t
(** Encode initialize result to JSON *)

(** {1 Server Info} *)

type server_info = { name : string; version : string option }
(** Server info included in initialize response *)

val initialize_response_to_json :
  result:initialize_result -> server_info:server_info -> Yojson.Safe.t
(** Full initialize response with server info *)

(** {1 Diagnostics} *)

(** LSP diagnostic severity *)
type diagnostic_severity = Error | Warning | Information | Hint

type position = { line : int; character : int }
(** A position in a document (0-based line and character) *)

type range = { start : position; end_ : position }
(** A range in a document *)

type location = { uri : string; range : range }
(** A location in a document (uri + range) *)

type diagnostic_related_information = { location : location; message : string }
(** Related information for a diagnostic.

    Used to point to related code that helps explain the error, such as where an
    expected type originated or the other branch in a branch type mismatch. *)

type diagnostic = {
  range : range;
  severity : diagnostic_severity option;
  code : string option;
  message : string;
  source : string option;
  related_information : diagnostic_related_information list;
}
(** A diagnostic represents a compiler error or warning *)

type publish_diagnostics_params = {
  uri : string;
  version : int option;
  diagnostics : diagnostic list;
}
(** Parameters for textDocument/publishDiagnostics notification *)

val publish_diagnostics_params_to_json :
  publish_diagnostics_params -> Yojson.Safe.t
(** Encode publishDiagnostics params to JSON *)

(** {1 Hover} *)

(** Markup content kind *)
type markup_kind = PlainText | Markdown

type markup_content = { kind : markup_kind; value : string }
(** Markup content for hover *)

type hover = { contents : markup_content; range : range option }
(** Hover result *)

type hover_params = { text_document : string; (* URI *) position : position }
(** Hover params *)

val parse_hover_params : Yojson.Safe.t -> hover_params
(** Parse hover params from JSON *)

val hover_to_json : hover -> Yojson.Safe.t
(** Encode hover result to JSON *)

(** {1 Go to Definition} *)

type definition_params = { def_text_document : string; def_position : position }
(** Definition request params *)

val parse_definition_params : Yojson.Safe.t -> definition_params
(** Parse definition params from JSON *)

(** Definition result *)
type definition_result =
  | DefLocation of location  (** Single definition location *)
  | DefLocations of location list  (** Multiple definition locations *)
  | DefNull  (** No definition found *)

val definition_result_to_json : definition_result -> Yojson.Safe.t
(** Encode definition result to JSON *)

(** {1 Find References} *)

type references_params = {
  ref_text_document : string;
  ref_position : position;
  include_declaration : bool;
}
(** References request params *)

val parse_references_params : Yojson.Safe.t -> references_params
(** Parse references params from JSON *)

type references_result = location list option
(** References result *)

val references_result_to_json : references_result -> Yojson.Safe.t
(** Encode references result to JSON *)
