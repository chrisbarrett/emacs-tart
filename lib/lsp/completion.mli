(** Completion provider for the LSP server.

    Provides textDocument/completion responses with candidates from local
    definitions, loaded signatures, and the type environment. *)

val extract_prefix_at_position : string -> int -> int -> string * int
(** [extract_prefix_at_position text line col] scans backwards from the position
    to find the start of the current symbol. Returns the prefix string and its
    start column. *)

val collect_local_completions :
  Syntax.Sexp.t list -> Protocol.completion_item list
(** Collect completion candidates from definitions in the document. Extracts
    function and variable names from defun/defvar/defconst forms. *)

val collect_env_completions : Core.Type_env.t -> Protocol.completion_item list
(** Collect completion candidates from a type environment. Creates completion
    items for all bound names with their types. *)

val filter_by_prefix :
  string -> Protocol.completion_item list -> Protocol.completion_item list
(** Filter completions by prefix match (case-insensitive). *)

val deduplicate_completions :
  Protocol.completion_item list -> Protocol.completion_item list
(** Deduplicate completion items by label, preferring items with types. *)

val handle :
  config:Typing.Module_check.config ->
  uri:string ->
  doc_text:string ->
  line:int ->
  col:int ->
  (Yojson.Safe.t, Rpc.response_error) result
(** Handle a textDocument/completion request.

    Takes the module config and document context instead of the full server
    type, keeping the module decoupled from server state. *)
