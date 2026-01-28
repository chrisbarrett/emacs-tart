(** Module dependency graph for incremental re-checking.

    This module tracks dependencies between Emacs Lisp modules to enable
    efficient cache invalidation when a module changes. The graph supports both
    forward edges (what does X depend on) and reverse edges (what depends on X).

    Edge types:
    - Require: `(require 'foo)` in .el files
    - Autoload: `(autoload 'fn "bar")` in .el files
    - Open: `(open 'seq)` in .tart files
    - Include: `(include 'dash)` in .tart files
    - Sibling: implicit edge from `foo.el` to `foo.tart` *)

(** {1 Types} *)

type module_id = string
(** Module identifier, e.g. "foo" or "typings/emacs/31.0/data" *)

(** Kinds of dependency edges *)
type edge_kind =
  | Require  (** `(require 'foo)` in .el *)
  | Autoload  (** `(autoload 'fn "bar")` in .el *)
  | Open  (** `(open 'seq)` in .tart *)
  | Include  (** `(include 'dash)` in .tart *)
  | Sibling  (** implicit: `foo.el` -> `foo.tart` *)

type edge = { target : module_id; kind : edge_kind }
(** A dependency edge with its kind *)

type t
(** The dependency graph *)

(** {1 Construction} *)

val create : unit -> t
(** Create an empty dependency graph *)

(** {1 Edge Management} *)

val add_edges : t -> module_id -> edge list -> unit
(** [add_edges g module_id edges] adds edges from [module_id] to the targets.
    This replaces any existing edges for [module_id]. *)

val remove_module : t -> module_id -> unit
(** [remove_module g module_id] removes all edges from and to [module_id]. *)

val get_edges : t -> module_id -> edge list
(** [get_edges g module_id] returns all edges from [module_id]. *)

(** {1 Dependency Queries} *)

val direct_dependents : t -> module_id -> module_id list
(** [direct_dependents g module_id] returns modules that directly depend on
    [module_id]. *)

val dependents : t -> module_id -> module_id list
(** [dependents g module_id] returns all modules that transitively depend on
    [module_id], including indirect dependents. The result is in breadth-first
    order. *)

val direct_dependencies : t -> module_id -> module_id list
(** [direct_dependencies g module_id] returns modules that [module_id] directly
    depends on. *)

val dependencies : t -> module_id -> module_id list
(** [dependencies g module_id] returns all modules that [module_id] transitively
    depends on. *)

(** {1 Cycle Detection} *)

type cycle = module_id list
(** A cycle is a list of modules forming a cycle, starting and ending with the
    same module *)

val detect_cycles : t -> cycle list
(** [detect_cycles g] returns all cycles in the graph. Each cycle is reported
    once, starting from the lexicographically smallest module in the cycle. *)

(** {1 Inspection} *)

val modules : t -> module_id list
(** [modules g] returns all module IDs in the graph. *)

val mem : t -> module_id -> bool
(** [mem g module_id] returns true if [module_id] has any edges in the graph. *)

val is_empty : t -> bool
(** [is_empty g] returns true if the graph has no edges. *)
