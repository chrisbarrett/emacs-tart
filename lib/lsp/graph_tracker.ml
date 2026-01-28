(** Graph integration for LSP document lifecycle.

    Updates the dependency graph when documents are opened, changed, or closed.
*)

module Dep_graph = Graph.Dependency_graph
module Builder = Graph.Graph_builder
module Sig_parser = Sig.Sig_parser
module Read = Syntax.Read

(** {1 Module ID Extraction} *)

(** Extract module ID from a file path.

    For "/path/to/foo.el" returns "foo". For "/path/to/bar.tart" returns "bar".
*)
let module_id_of_filename (filename : string) : Dep_graph.module_id =
  let basename = Filename.basename filename in
  if Filename.check_suffix basename ".el" then
    Filename.chop_suffix basename ".el"
  else if Filename.check_suffix basename ".tart" then
    Filename.chop_suffix basename ".tart"
  else basename

(** Extract filename from a file:// URI.

    Returns the path portion, or the raw URI if not a file:// URI. *)
let filename_of_uri (uri : string) : string =
  if String.length uri > 7 && String.sub uri 0 7 = "file://" then
    String.sub uri 7 (String.length uri - 7)
  else uri

(** {1 Dependency Extraction} *)

(** Extract all edges for an .el file.

    Includes:
    - Require edges from (require 'foo)
    - Autoload edges from (autoload 'fn "bar")
    - Sibling edge to foo.tart if it exists
    - Core typings edge *)
let extract_el_edges ~(filename : string) (forms : Syntax.Sexp.t list) :
    Dep_graph.edge list =
  let source_edges = Builder.extract_from_sexp forms in
  let sibling_edge = Builder.sibling_edge_for_el_file filename in
  let core_edge = Builder.make_core_typings_edge () in
  (* Combine all edges *)
  core_edge :: (Option.to_list sibling_edge @ source_edges)

(** Extract all edges for a .tart file.

    Includes:
    - Open edges from (open 'foo)
    - Include edges from (include 'bar) *)
let extract_tart_edges ~(module_name : string) (forms : Syntax.Sexp.t list) :
    Dep_graph.edge list =
  match Sig_parser.parse_signature ~module_name forms with
  | Ok sig_ -> Builder.extract_from_signature sig_
  | Error _ ->
      (* On parse error, return empty edges. The signature file has errors
         that will be reported separately. *)
      []

(** {1 Graph Updates} *)

(** Update the graph for an opened or changed document.

    Re-extracts dependencies and updates the graph. *)
let update_document (graph : Dep_graph.t) ~(uri : string) ~(text : string) :
    unit =
  let filename = filename_of_uri uri in
  let module_id = module_id_of_filename filename in
  let is_tart = Filename.check_suffix filename ".tart" in
  (* Parse the document *)
  let parse_result = Read.parse_string ~filename text in
  let forms = parse_result.Read.sexps in
  (* Extract edges based on file type *)
  let edges =
    if is_tart then extract_tart_edges ~module_name:module_id forms
    else extract_el_edges ~filename forms
  in
  (* Update the graph with new edges *)
  Dep_graph.add_edges graph module_id edges

(** Handle didClose - keep the graph entry (file still exists on disk).

    Per spec R4: "didClose: Keep in graph (file exists on disk)" We don't remove
    the module from the graph because the file still exists and other modules
    may depend on it. *)
let close_document (_graph : Dep_graph.t) ~(uri : string) : unit =
  (* Intentionally do nothing - the graph entry is kept.
     The file still exists on disk and may be opened again later. *)
  ignore uri
