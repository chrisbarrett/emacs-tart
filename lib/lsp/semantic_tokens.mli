(** Semantic token computation for the LSP server.

    Classifies AST nodes and comment lines into semantic tokens for
    textDocument/semanticTokens/full and textDocument/semanticTokens/full/delta
    requests. Tokens are delta-encoded as specified by the LSP protocol. *)

type raw_token = {
  rt_line : int;  (** 0-based line number *)
  rt_col : int;  (** Start column in UTF-16 code units *)
  rt_length : int;  (** Length in UTF-16 code units *)
  rt_type : Protocol.semantic_token_type;
  rt_modifiers : Protocol.semantic_token_modifier list;
}
(** A raw (absolute-position) semantic token before delta encoding. *)

val collect_sexp_tokens : text:string -> Syntax.Sexp.t list -> raw_token list
(** [collect_sexp_tokens ~text sexps] walks parsed S-expressions and produces
    semantic tokens for symbols, literals, keywords, definition forms, and
    parameters. Positions are converted to 0-based lines with UTF-16 columns. *)

val collect_comment_tokens : string -> raw_token list
(** [collect_comment_tokens text] scans raw text for [;]-prefixed comment lines
    and emits one [STComment] token per line. *)

val delta_encode : raw_token list -> int list
(** [delta_encode tokens] sorts tokens by position and produces the flat
    delta-encoded [int list] for the LSP semantic tokens response:
    [deltaLine, deltaStartChar, length, tokenType, tokenModifiers, ...]. *)

(** {1 Token Cache} *)

type cache
(** Mutable cache of previous token arrays per URI, used for delta computation.
    Each entry stores a result ID and the flat int array. *)

val create_cache : unit -> cache
(** Create an empty semantic tokens cache. *)

val invalidate : cache -> string -> unit
(** [invalidate cache uri] removes the cached entry for [uri], e.g., on document
    close. *)

(** {1 Delta Computation} *)

val compute_edits : int array -> int array -> Protocol.semantic_tokens_edit list
(** [compute_edits old_data new_data] computes the minimal sequence of edits to
    transform [old_data] into [new_data]. Returns an empty list when the arrays
    are equal. *)

(** {1 Request Handlers} *)

val handle :
  uri:string ->
  doc_text:string ->
  cache:cache ->
  (Yojson.Safe.t, Rpc.response_error) result
(** Handle a textDocument/semanticTokens/full request.

    Parses the document, collects AST tokens and comment tokens, delta-encodes,
    caches the result, and returns the JSON result with a [resultId]. *)

val handle_delta :
  uri:string ->
  doc_text:string ->
  previous_result_id:string ->
  cache:cache ->
  (Yojson.Safe.t, Rpc.response_error) result
(** Handle a textDocument/semanticTokens/full/delta request.

    Computes the new token array, looks up the cached previous array by
    [previous_result_id], computes edits, caches the new array, and returns
    either a delta or full response. Falls back to a full response when no cache
    entry matches. *)
