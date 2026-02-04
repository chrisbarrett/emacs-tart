(** Prelude loading and management.

    The prelude is a set of implicit utility types that are automatically
    available in all .tart files without explicit import. The prelude types
    bridge compiler intrinsics to ergonomic everyday types.

    Prelude type aliases:
    - t: The symbol 't (Elisp's canonical truthy value)
    - any: Universal type (truthy | nil)
    - bool: Boolean type (t | nil)
    - list: Homogeneous list ((cons a (list a)) | nil)
    - option: Add nil to a truthy type (a | nil) with bound (a : truthy)
    - is: Type subtraction (a - nil) - removes nil from a type
    - nonempty: Non-empty list (is (list a)) - list without nil

    Prelude opaque types:
    - buffer: Emacs buffer
    - window: Emacs window
    - frame: Emacs frame
    - marker: Position marker in a buffer
    - overlay: Text overlay in a buffer
    - process: Subprocess or network connection
    - bool-vector: Bit vector
    - char-table: Character property table

    The prelude is loaded before any other .tart file and its bindings cannot be
    shadowed (per Spec 07 R17). *)

(** {1 Prelude Type Aliases} *)

val prelude_aliases : (string * Sig_loader.type_alias) list
(** The prelude type alias definitions, suitable for merging into an alias
    context during signature loading. *)

(** {1 Prelude Opaque Types} *)

val prelude_opaque_defs : (string * string list) list
(** The prelude opaque type definitions. Each entry is (name, params) where
    params is the list of type parameter names. *)

val prelude_type_names : string list
(** Names of all prelude types (aliases and opaques), for shadowing checks. *)

(** {1 Prelude Context} *)

val prelude_alias_context : unit -> Sig_loader.alias_context
(** Build an alias context containing prelude type aliases. This context should
    be merged with other aliases during signature loading. *)

val prelude_opaque_context : unit -> Sig_loader.opaque_context
(** Build an opaque context containing prelude opaque types. *)

val prelude_type_context : unit -> Sig_loader.type_context
(** Build a type context containing prelude types. This includes prelude aliases
    and prelude opaque types (buffer, window, frame, marker, overlay, process,
    bool-vector, char-table). *)

(** {1 Shadowing Checks} *)

val is_prelude_type : string -> bool
(** Check if a name is a prelude type. Used to prevent shadowing. *)
