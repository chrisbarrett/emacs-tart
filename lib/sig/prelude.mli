(** Prelude loading and management.

    The prelude is a set of implicit utility types that are automatically
    available in all .tart files without explicit import. The prelude types
    bridge compiler intrinsics to ergonomic everyday types.

    Prelude types:
    - t: The symbol 't (Elisp's canonical truthy value)
    - any: Universal type (truthy | nil)
    - bool: Boolean type (t | nil)
    - list: Homogeneous list ((cons a (list a)) | nil)
    - option: Add nil to a truthy type (a | nil) with bound (a : truthy)

    The prelude is loaded before any other .tart file and its bindings cannot be
    shadowed (per Spec 07 R17). *)

(** {1 Prelude Type Aliases} *)

val prelude_aliases : (string * Sig_loader.type_alias) list
(** The prelude type alias definitions, suitable for merging into an alias
    context during signature loading. *)

val prelude_type_names : string list
(** Names of all prelude types, for shadowing checks. *)

(** {1 Prelude Context} *)

val prelude_alias_context : unit -> Sig_loader.alias_context
(** Build an alias context containing prelude type aliases. This context should
    be merged with other aliases during signature loading. *)

val prelude_type_context : unit -> Sig_loader.type_context
(** Build a type context containing prelude types. This includes prelude aliases
    but no opaque types (prelude defines only aliases). *)

(** {1 Shadowing Checks} *)

val is_prelude_type : string -> bool
(** Check if a name is a prelude type. Used to prevent shadowing. *)
