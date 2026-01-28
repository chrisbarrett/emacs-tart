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

val sibling_edge : string -> Dependency_graph.edge option
(** [sibling_edge module_id] returns a Sibling edge if a .tart file exists for
    the given .el module. The module_id should be the basename without
    extension. Returns [None] if no sibling signature file is found.

    Note: This is a convenience function; the caller is responsible for
    determining whether a sibling file exists on disk or in memory. *)

val make_sibling_edge : string -> Dependency_graph.edge
(** [make_sibling_edge target] creates a Sibling edge to the given target
    module. Use this when you know the sibling file exists. *)
