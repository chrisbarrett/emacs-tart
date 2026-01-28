(** Dependency extraction from parsed files.

    This module extracts dependency edges from parsed Elisp (.el) and signature
    (.tart) files for use with the dependency graph.

    Edge types extracted:
    - From .el files: Require ([(require 'foo)]), Autoload
      ([(autoload 'fn "bar")])
    - From .tart files: Open ([(open 'seq)]), Include ([(include 'dash)]) *)

(** {1 Extraction from .el Files} *)

val extract_from_sexp : Syntax.Sexp.t list -> Dependency_graph.edge list
(** [extract_from_sexp forms] extracts dependency edges from parsed Elisp forms.

    Extracts:
    - [(require 'foo)] -> Require edge to "foo"
    - [(require 'foo "file")] -> Require edge to "foo"
    - [(autoload 'fn "bar")] -> Autoload edge to "bar"

    Returns a list of unique edges (duplicates removed). *)

(** {1 Extraction from .tart Files} *)

val extract_from_signature : Sig.Sig_ast.signature -> Dependency_graph.edge list
(** [extract_from_signature sig] extracts dependency edges from a parsed
    signature file.

    Extracts:
    - [(open 'seq)] -> Open edge to "seq"
    - [(include 'dash)] -> Include edge to "dash"

    Returns a list of unique edges (duplicates removed). *)

(** {1 Sibling Edge} *)

val sibling_edge_for_el_file : string -> Dependency_graph.edge option
(** [sibling_edge_for_el_file el_path] returns a Sibling edge if a .tart file
    exists in the same directory as the .el file. The el_path should be the full
    path to the .el file.

    For example, if [el_path] is "/project/foo.el" and "/project/foo.tart"
    exists, returns [Some { target = "foo"; kind = Sibling }].

    Returns [None] if no sibling signature file is found. *)

val make_sibling_edge : string -> Dependency_graph.edge
(** [make_sibling_edge target] creates a Sibling edge to the given target
    module. Use this when you know the sibling file exists. *)

(** {1 Core Typings Pseudo-Module} *)

val core_typings_module_id : Dependency_graph.module_id
(** The pseudo-module ID representing core typings. All .el files implicitly
    depend on this module. When core typings change, all dependents should be
    invalidated. *)

val make_core_typings_edge : unit -> Dependency_graph.edge
(** [make_core_typings_edge ()] creates a Require edge to the core typings
    pseudo-module. Add this edge to every .el file's dependency list. *)

val is_core_typings_module : Dependency_graph.module_id -> bool
(** [is_core_typings_module module_id] returns true if the given module_id
    represents the core typings pseudo-module. *)
