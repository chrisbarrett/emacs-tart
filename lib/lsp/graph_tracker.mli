(** Graph integration for LSP document lifecycle.

    Updates the dependency graph when documents are opened, changed, or closed.
    This implements R4 of Spec 27 (incremental updates).

    Behavior:
    - didOpen: Parse document, extract edges, add to graph
    - didChange: Re-parse, re-extract edges, update graph
    - didClose: Keep graph entry (file still exists on disk) *)

(** {1 Module ID Extraction} *)

val module_id_of_filename : string -> Graph.Dependency_graph.module_id
(** Extract module ID from a file path.

    For "/path/to/foo.el" returns "foo". For "/path/to/bar.tart" returns "bar".
*)

val filename_of_uri : string -> string
(** Extract filename from a file:// URI.

    Returns the path portion, or the raw URI if not a file:// URI. *)

(** {1 Graph Updates} *)

val update_document :
  Graph.Dependency_graph.t -> uri:string -> text:string -> unit
(** Update the graph for an opened or changed document.

    Re-extracts dependencies from the document and updates the graph. For .el
    files: extracts require, autoload, sibling, and core typing edges. For .tart
    files: extracts open and include edges. *)

val close_document : Graph.Dependency_graph.t -> uri:string -> unit
(** Handle document close.

    Per spec: keeps the graph entry because the file still exists on disk and
    other modules may depend on it. This is intentionally a no-op. *)

(** {1 Invalidation Cascade} *)

val dependent_uris :
  Graph.Dependency_graph.t ->
  module_id:Graph.Dependency_graph.module_id ->
  open_uris:string list ->
  string list
(** Get dependent URIs from a list of open document URIs.

    Given a module ID and a list of open URIs, returns the URIs whose
    corresponding modules depend on the given module (directly or transitively).
    Use this to implement the invalidation cascade: when module X changes, find
    all open documents that depend on X and invalidate their caches. *)

val module_id_of_uri : string -> Graph.Dependency_graph.module_id
(** Get module ID from a URI. Combines [filename_of_uri] and
    [module_id_of_filename]. *)
