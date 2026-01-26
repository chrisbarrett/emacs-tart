(** Signature search path and module resolution.

    This module implements the search path configuration (R15) and module
    discovery order (R16) for finding `.tart` signature files.

    Discovery order: 1. Sibling file: `module.tart` next to `module.el` 2.
    Search path: Each directory in the configured search path 3. Stdlib: Bundled
    signatures shipped with tart

    The first match wins, allowing project-local overrides. *)

(** {1 Search Path Configuration} *)

type t
(** Search path configuration. Contains a list of directories to search for
    `.tart` files. *)

val empty : t
(** Empty search path (no directories). *)

val of_dirs : string list -> t
(** Create a search path from a list of directories. *)

val with_stdlib : string -> t -> t
(** Create a search path with stdlib directory. *)

val prepend_dir : string -> t -> t
(** Add a directory to the front of the search path (higher precedence). *)

val append_dir : string -> t -> t
(** Add a directory to the end of the search path (lower precedence). *)

(** {1 File Discovery} *)

val find_signature : t -> string -> string option
(** Find a `.tart` file using the search path. Searches in order: each
    search_dir, then stdlib_dir.

    @param t The search path configuration
    @param module_name The module name (e.g., "cl-lib")
    @return The path to the `.tart` file, if found *)

val find_sibling : string -> string -> string option
(** Find a sibling `.tart` file for an `.el` file.

    @param el_path Path to the `.el` file being type-checked
    @param module_name The required module name
    @return The path to the sibling `.tart` file, if found *)

(** {1 Module Resolution} *)

val parse_signature_file : string -> Sig_ast.signature option
(** Parse a `.tart` file and return its signature AST.

    @param path Path to the `.tart` file
    @return The parsed signature, or None if parsing fails *)

val make_resolver : ?el_path:string -> t -> Sig_loader.module_resolver
(** Create a module resolver from a search path configuration. This resolver
    implements the full discovery order: 1. Sibling `.tart` next to the current
    file (if el_path provided) 2. Each directory in the search path 3. Bundled
    stdlib

    @param el_path Optional path to the `.el` file being type-checked
    @param search_path The search path configuration
    @return A resolver function suitable for load_signature_with_resolver *)

(** {1 Loading Utilities} *)

val load_module :
  search_path:t ->
  ?el_path:string ->
  env:Core.Type_env.t ->
  string ->
  Core.Type_env.t option
(** Load signatures for a module using the search path.

    @param search_path The search path configuration
    @param el_path Optional path to the `.el` file being type-checked
    @param env Base type environment to extend
    @param module_name The module name to load
    @return Extended type environment, or None if module not found *)
