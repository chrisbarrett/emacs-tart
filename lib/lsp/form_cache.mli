(** Form-level caching for incremental type checking.

    This module implements query-based caching for the LSP server. Each
    top-level form (defun, defvar, etc.) is cached by a hash of its source text.
    When a document changes, only forms whose source has changed are
    re-type-checked.

    Cache invalidation strategy:
    - If the form's source changes → re-check
    - If the module's signature (.tart) changes → invalidate all forms
    - If a required module's signature changes → invalidate all forms *)

(** {1 Form Identification} *)

type form_id = {
  name : string option;  (** Name for defun/defvar, None for expressions *)
  source_hash : int;  (** Hash of the S-expression's string representation *)
}
(** A form's identity, used for cache lookup *)

val form_id_of_sexp : Syntax.Sexp.t -> form_id
(** Compute the form ID for a top-level S-expression *)

(** {1 Types} *)

type t
(** Global cache mapping document URIs to their form caches *)

type check_stats = {
  total_forms : int;  (** Total number of top-level forms *)
  cached_forms : int;  (** Number of forms with cache hits *)
  checked_forms : int;  (** Number of forms that were re-checked *)
}
(** Statistics for a check operation *)

(** {1 Cache Management} *)

val create : unit -> t
(** Create an empty global cache *)

val remove_document : t -> string -> unit
(** Remove a document's cache (call on textDocument/didClose) *)

val invalidate_document : t -> string -> unit
(** Invalidate all cached forms for a document.

    Use this when a dependency changes to force re-checking. Unlike
    [remove_document], this keeps the document cache entry but clears all cached
    form results. *)

(** {1 Incremental Type Checking} *)

val check_with_cache :
  cache:t ->
  config:Typing.Module_check.config ->
  filename:string ->
  uri:string ->
  sibling_sig_content:string option ->
  Syntax.Sexp.t list ->
  Typing.Module_check.check_result * check_stats
(** Check a document with caching.

    This is the main entry point for incremental type checking: 1. Parse the
    document 2. Check if config has changed (invalidate all if so) 3. For each
    form, check cache and re-type-check if needed 4. Combine results

    @param cache The global form cache
    @param config Module check configuration
    @param filename Path to the .el file
    @param uri LSP document URI
    @param sibling_sig_content
      Content of sibling .tart file, if any (for cache invalidation)
    @param sexps Parsed S-expressions
    @return The check result and statistics *)
