(** JSON-RPC 2.0 protocol implementation for LSP.

    This module implements the JSON-RPC 2.0 protocol over stdio with
    Content-Length headers as specified by the Language Server Protocol.

    Message format:
    {v
    Content-Length: <length>\r\n
    \r\n
    <json content>
    v} *)

(** {1 Message Types} *)

type message = {
  id : Yojson.Safe.t option;  (** Request ID; None for notifications *)
  method_ : string;  (** Method name *)
  params : Yojson.Safe.t option;  (** Method parameters *)
}
(** JSON-RPC request/notification message *)

type response = {
  id : Yojson.Safe.t;  (** Request ID *)
  result : Yojson.Safe.t option;  (** Success result *)
  error : response_error option;  (** Error response *)
}
(** JSON-RPC response *)

and response_error = {
  code : int;
  message : string;
  data : Yojson.Safe.t option;
}
(** JSON-RPC error object *)

(** {1 Standard Error Codes} *)

val parse_error : int
val invalid_request : int
val method_not_found : int
val invalid_params : int
val internal_error : int

(** LSP-specific error codes *)

val server_not_initialized : int
val request_cancelled : int
val content_modified : int

(** {1 Reading Messages} *)

(** Error type for message reading *)
type read_error =
  | Eof
  | InvalidHeader of string
  | InvalidJson of string
  | InvalidMessage of string

val read_message : In_channel.t -> (message, read_error) result
(** Read a single JSON-RPC message from an input channel.

    Reads Content-Length header, then reads that many bytes of JSON content,
    parses and validates as a JSON-RPC message.

    @return Ok message on success, Error read_error on failure *)

(** {1 Writing Messages} *)

val write_response : Out_channel.t -> response -> unit
(** Write a JSON-RPC response to an output channel.

    Formats with Content-Length header and flushes output. *)

val write_notification :
  Out_channel.t -> method_:string -> params:Yojson.Safe.t -> unit
(** Write a JSON-RPC notification to an output channel.

    Notifications have no id and expect no response. *)

val write_request :
  Out_channel.t -> id:int -> method_:string -> params:Yojson.Safe.t -> unit
(** Write a JSON-RPC request to an output channel.

    Requests have an id and expect a response from the client. Used for
    server-initiated requests like [client/registerCapability]. *)

(** {1 Response Helpers} *)

val success_response : id:Yojson.Safe.t -> result:Yojson.Safe.t -> response
(** Create a success response *)

val error_response :
  id:Yojson.Safe.t ->
  code:int ->
  message:string ->
  ?data:Yojson.Safe.t ->
  unit ->
  response
(** Create an error response *)

(** {1 Debugging} *)

val message_to_string : message -> string
(** Format a message for debugging *)

val response_to_string : response -> string
(** Format a response for debugging *)

val read_error_to_string : read_error -> string
(** Format a read_error for display *)
