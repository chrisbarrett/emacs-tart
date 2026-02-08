(** Semantic token computation for the LSP server.

    Classifies AST nodes and comment lines into semantic tokens for
    textDocument/semanticTokens/full requests. Tokens are delta-encoded as
    specified by the LSP protocol. *)

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

val handle :
  uri:string -> doc_text:string -> (Yojson.Safe.t, Rpc.response_error) result
(** Handle a textDocument/semanticTokens/full request.

    Parses the document, collects AST tokens and comment tokens, delta-encodes,
    and returns the JSON result. *)
