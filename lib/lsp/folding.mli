(** Folding range computation for the LSP server.

    Provides fold regions for top-level forms, let blocks, multi-line strings,
    and comment blocks, triggered by textDocument/foldingRange requests. *)

val collect_sexp_folds : Syntax.Sexp.t list -> Protocol.folding_range list
(** [collect_sexp_folds sexps] walks parsed S-expressions and collects
    multi-line fold ranges for top-level definition forms, [let]/[let*] blocks,
    and multi-line string literals.

    Returned ranges use 0-based line numbers. No [kind] is set for code folds.
*)

val collect_comment_folds : string -> Protocol.folding_range list
(** [collect_comment_folds text] scans raw text for contiguous runs of comment
    lines (lines starting with [;], ignoring leading whitespace). Runs of 2+
    lines produce a fold with [kind = Some FRComment]. *)

val handle :
  uri:string -> doc_text:string -> (Yojson.Safe.t, Rpc.response_error) result
(** Handle a textDocument/foldingRange request.

    Parses the document, collects sexp folds and comment folds, deduplicates by
    start line, and returns the JSON result. *)
