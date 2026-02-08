(** Workspace symbol search for the LSP server.

    Provides cross-file symbol lookup across all open [.el] and [.tart]
    documents, triggered by workspace/symbol requests. Symbols are matched with
    case-insensitive substring matching on the symbol name. *)

val symbols_from_el_doc :
  uri:string -> text:string -> Protocol.symbol_information list
(** [symbols_from_el_doc ~uri ~text] parses an Emacs Lisp document and extracts
    top-level definitions ([defun], [defvar], [defconst], [defmacro]) as flat
    symbol information items. The container name is derived from the filename.
*)

val symbols_from_tart_doc :
  uri:string -> text:string -> Protocol.symbol_information list
(** [symbols_from_tart_doc ~uri ~text] parses a [.tart] signature file and
    extracts declarations ([DDefun], [DDefvar], [DType], [DData],
    [DImportStruct]) as flat symbol information items. The container name is the
    module name from the signature. *)

val handle :
  documents:Document.t ->
  query:string ->
  (Yojson.Safe.t, Rpc.response_error) result
(** Handle a workspace/symbol request.

    Collects symbols from all open [.el] and [.tart] documents, filters by
    case-insensitive substring match on [query] (empty query returns all
    symbols), and returns the JSON result. *)
