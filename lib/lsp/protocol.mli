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
