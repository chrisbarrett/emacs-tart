(** Reusable test helpers for exercising the LSP server.

    Builds JSON-RPC messages, runs the server in-process, and parses responses.
*)

(** {1 Message Construction} *)

val make_message :
  ?id:Yojson.Safe.t -> method_:string -> ?params:Yojson.Safe.t -> unit -> string
(** Build a JSON-RPC 2.0 message string with Content-Length header. *)

val initialize_msg :
  ?id:int ->
  ?root_uri:string ->
  ?capabilities:Yojson.Safe.t ->
  ?initialization_options:Yojson.Safe.t ->
  unit ->
  string
(** [initialize_msg ?id ?root_uri ?capabilities ?initialization_options ()]
    builds an initialize request. Defaults: [id = 1], [root_uri] omitted,
    [capabilities = \{\}], [initialization_options] omitted. *)

val initialized_msg : unit -> string
(** Notification sent after initialize response is received. *)

val shutdown_msg : ?id:int -> unit -> string
(** [shutdown_msg ?id ()] builds a shutdown request. Default [id = 99]. *)

val exit_msg : unit -> string
(** Exit notification. *)

val did_open_msg : uri:string -> ?version:int -> text:string -> unit -> string
(** [did_open_msg ~uri ~text ()] builds a textDocument/didOpen notification.
    Default [version = 1]. *)

val did_change_msg :
  uri:string -> version:int -> changes:Yojson.Safe.t list -> unit -> string
(** Incremental textDocument/didChange notification. *)

val did_change_full_msg :
  uri:string -> version:int -> text:string -> unit -> string
(** Full-document textDocument/didChange notification. *)

val did_close_msg : uri:string -> unit -> string
(** textDocument/didClose notification. *)

val hover_msg :
  id:int -> uri:string -> line:int -> character:int -> unit -> string
(** textDocument/hover request. *)

val definition_msg :
  id:int -> uri:string -> line:int -> character:int -> unit -> string
(** textDocument/definition request. *)

val references_msg :
  id:int -> uri:string -> line:int -> character:int -> unit -> string
(** textDocument/references request. *)

val completion_msg :
  id:int -> uri:string -> line:int -> character:int -> unit -> string
(** textDocument/completion request. *)

val signature_help_msg :
  id:int -> uri:string -> line:int -> character:int -> unit -> string
(** textDocument/signatureHelp request. *)

val document_symbol_msg : id:int -> uri:string -> unit -> string
(** textDocument/documentSymbol request. *)

val code_action_msg :
  id:int ->
  uri:string ->
  start_line:int ->
  start_character:int ->
  end_line:int ->
  end_character:int ->
  ?diagnostics:Yojson.Safe.t list ->
  unit ->
  string
(** textDocument/codeAction request. *)

val folding_range_msg : id:int -> uri:string -> unit -> string
(** textDocument/foldingRange request. *)

val semantic_tokens_msg : id:int -> uri:string -> unit -> string
(** textDocument/semanticTokens/full request. *)

val inlay_hint_msg :
  id:int ->
  uri:string ->
  start_line:int ->
  start_character:int ->
  end_line:int ->
  end_character:int ->
  unit ->
  string
(** textDocument/inlayHint request. *)

val type_definition_msg :
  id:int -> uri:string -> line:int -> character:int -> unit -> string
(** textDocument/typeDefinition request. *)

val prepare_rename_msg :
  id:int -> uri:string -> line:int -> character:int -> unit -> string
(** textDocument/prepareRename request. *)

val rename_msg :
  id:int ->
  uri:string ->
  line:int ->
  character:int ->
  new_name:string ->
  unit ->
  string
(** textDocument/rename request. *)

val did_save_msg : uri:string -> unit -> string
(** textDocument/didSave notification. *)

val workspace_symbol_msg : id:int -> query:string -> unit -> string
(** workspace/symbol request. *)

val did_change_watched_files_msg : changes:(string * int) list -> unit -> string
(** workspace/didChangeWatchedFiles notification. Each change is a [(uri, type)]
    pair where type is 1=created, 2=changed, 3=deleted. *)

val cancel_request_msg : id:int -> unit -> string
(** [$/cancelRequest] notification. Signals that a pending request with the
    given [id] should be cancelled. *)

val did_change_configuration_msg : settings:Yojson.Safe.t -> unit -> string
(** workspace/didChangeConfiguration notification. *)

val call_hierarchy_prepare_msg :
  id:int -> uri:string -> line:int -> character:int -> unit -> string
(** textDocument/prepareCallHierarchy request. *)

val incoming_calls_msg : id:int -> item:Yojson.Safe.t -> unit -> string
(** callHierarchy/incomingCalls request. *)

val outgoing_calls_msg : id:int -> item:Yojson.Safe.t -> unit -> string
(** callHierarchy/outgoingCalls request. *)

val type_hierarchy_prepare_msg :
  id:int -> uri:string -> line:int -> character:int -> unit -> string
(** textDocument/prepareTypeHierarchy request. *)

val supertypes_msg : id:int -> item:Yojson.Safe.t -> unit -> string
(** typeHierarchy/supertypes request. *)

val subtypes_msg : id:int -> item:Yojson.Safe.t -> unit -> string
(** typeHierarchy/subtypes request. *)

val code_lens_msg : id:int -> uri:string -> unit -> string
(** textDocument/codeLens request. *)

(** {1 Session Runners} *)

type session_result = {
  exit_code : int;
  messages : Yojson.Safe.t list;
  server : Lsp.Server.t;
}
(** Result of running a server session. *)

val run_session : string list -> session_result
(** [run_session msgs] feeds [msgs] directly to a fresh server and returns the
    exit code, parsed output messages, and server instance. No
    init/shutdown/exit is prepended—caller provides the full sequence. *)

val run_initialized_session : string list -> session_result
(** [run_initialized_session msgs] wraps [msgs] with initialize (id 1) +
    shutdown (id 99) + exit, then runs the server. The caller's messages are
    sandwiched between initialize and shutdown. *)

(** {1 Response Queries} *)

val find_response : id:int -> Yojson.Safe.t list -> Yojson.Safe.t option
(** Find a response message whose [id] matches the given integer. *)

val find_request : method_:string -> Yojson.Safe.t list -> Yojson.Safe.t option
(** Find the first server-initiated request with the given method name (has both
    [method] and [id] fields). *)

val find_notification :
  method_:string -> Yojson.Safe.t list -> Yojson.Safe.t option
(** Find the first notification with the given method name. *)

val find_all_notifications :
  method_:string -> Yojson.Safe.t list -> Yojson.Safe.t list
(** Find all notifications with the given method name. *)

val find_diagnostics : uri:string -> Yojson.Safe.t list -> Yojson.Safe.t option
(** Find a publishDiagnostics notification for the given URI. *)

val find_last_diagnostics :
  uri:string -> Yojson.Safe.t list -> Yojson.Safe.t option
(** Find the last publishDiagnostics notification for the given URI. *)

val response_result : Yojson.Safe.t -> Yojson.Safe.t
(** Extract the [result] field from a response. Raises if absent. *)

val response_error : Yojson.Safe.t -> Yojson.Safe.t option
(** Extract the [error] field from a response, if present. *)

(** {1 Message Parsing} *)

val parse_messages : string -> Yojson.Safe.t list
(** Parse all Content-Length-delimited JSON-RPC messages from raw output. *)

(** {1 String Helpers} *)

val contains_string : needle:string -> string -> bool
(** [contains_string ~needle haystack] returns [true] if [needle] is a substring
    of [haystack]. *)
