(** Inlay hint computation for the LSP server.

    Provides inline type annotations for [defun] return types and [let]/[let*]
    binding types, triggered by textDocument/inlayHint requests. Hints are
    suppressed when the type is trivially obvious (e.g., a string literal bound
    to a variable). *)

val handle :
  uri:string ->
  doc_text:string ->
  range:Protocol.range ->
  (Yojson.Safe.t, Rpc.response_error) result
(** Handle a textDocument/inlayHint request.

    Parses the document, type-checks it, collects inlay hints for [defun] return
    types and [let]/[let*] binding types within the requested [range], and
    returns the JSON result. Hints use UTF-16 positions. *)
