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

val with_emacs_version : Emacs_version.version -> t -> t
(** Set Emacs version for typings lookup. Used for version-specific c-core
    typings with fallback chain. *)

val with_typings_root : string -> t -> t
(** Set the typings root directory (e.g., /path/to/typings/emacs).

    When combined with [with_emacs_version], enables version-specific typings
    lookup with fallback chain: exact → minor → major → latest. *)

val typings_root : t -> string option
(** Get the typings root directory, if configured. *)

val prepend_dir : string -> t -> t
(** Add a directory to the front of the search path (higher precedence). *)

val append_dir : string -> t -> t
(** Add a directory to the end of the search path (lower precedence). *)

(** {1 File Discovery} *)

val find_signature : t -> string -> string option
(** Find a `.tart` file using the search path. Searches in order: each
    search_dir, then versioned typings (with fallback), then stdlib_dir.

    @param t The search path configuration
    @param module_name The module name (e.g., "cl-lib")
    @return The path to the `.tart` file, if found *)

(** {2 Version Fallback} *)

val version_fallback_candidates : Emacs_version.version -> string list
(** Generate version fallback candidates for a given version.

    Implements R3 of Spec 24: search order is exact → minor → major → latest.

    For 31.0.50 returns ["31.0.50"; "31.0"; "31"; "latest"] For 31.0 returns
    ["31.0"; "31"; "latest"] For 31 returns ["31"; "latest"] *)

val find_typings_dir :
  typings_root:string -> version:Emacs_version.version -> string option
(** Find a typings directory for a version using fallback chain.

    Returns the first existing directory in the fallback chain, or None if no
    version directory exists. *)

val find_sibling : string -> string -> string option
(** Find a sibling `.tart` file for an `.el` file.

    @param el_path Path to the `.el` file being type-checked
    @param module_name The required module name
    @return The path to the sibling `.tart` file, if found *)

(** {1 Signature Errors} *)

(** Error kind for signature file issues *)
type sig_error_kind =
  | LexerError of string  (** Lexer error (e.g., invalid character) *)
  | ParseError of string  (** Parser error (e.g., invalid syntax) *)
  | ValidationError of string  (** Validation error (e.g., unbound type var) *)
  | IOError of string  (** File read error *)

type sig_error = {
  path : string;  (** Path to the .tart file *)
  kind : sig_error_kind;  (** Type of error *)
  span : Syntax.Location.span;  (** Location in the file *)
}
(** An error that occurred while loading a signature file *)

val string_of_sig_error : sig_error -> string
(** Format a sig_error for display *)

(** {1 Module Resolution} *)

val parse_signature_file_with_errors :
  string -> (Sig_ast.signature, sig_error list) result
(** Parse a `.tart` file and return its signature AST or errors.

    @param path Path to the `.tart` file
    @return Ok signature or Error list of errors *)

val parse_and_validate_signature :
  string -> (Sig_ast.signature, sig_error list) result
(** Parse and validate a `.tart` file, returning all errors.

    This function combines parsing and validation to catch all issues:
    - Lexer errors (invalid characters)
    - Parser errors (invalid syntax)
    - Validation errors (unbound type variables, invalid types)

    @param path Path to the `.tart` file
    @return Ok signature or Error list of all errors *)

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

val make_has_el_file : ?el_path:string -> t -> string -> bool
(** Create a has_el_file checker from a search path configuration.

    Checks whether a module has a corresponding .el file, used to enforce Spec
    07 R19: auxiliary .tart files (no .el) can be included but not opened.

    @param el_path Optional path to the .el file being type-checked
    @param search_path The search path configuration
    @return A function that checks if a module has a corresponding .el file *)

(** {1 Loading Utilities} *)

val load_module :
  search_path:t ->
  ?el_path:string ->
  ?with_prelude:bool ->
  env:Core.Type_env.t ->
  string ->
  Core.Type_env.t option
(** Load signatures for a module using the search path.

    @param search_path The search path configuration
    @param el_path Optional path to the `.el` file being type-checked
    @param with_prelude If true (default), prelude types are available
    @param env Base type environment to extend
    @param module_name The module name to load
    @return Extended type environment, or None if module not found *)

val load_module_with_sig :
  search_path:t ->
  ?el_path:string ->
  ?with_prelude:bool ->
  env:Core.Type_env.t ->
  string ->
  (Core.Type_env.t * Sig_ast.signature) option
(** Load signatures for a module and also return the signature AST.

    Like [load_module] but also returns the parsed signature AST for further
    processing (e.g., instance extraction).

    @param search_path The search path configuration
    @param el_path Optional path to the `.el` file being type-checked
    @param with_prelude If true (default), prelude types are available
    @param env Base type environment to extend
    @param module_name The module name to load
    @return Extended type environment and signature AST, or None if not found *)

(** {1 C-Core Loading} *)

val list_c_core_files : string -> string list
(** List all .tart files in a c-core directory.

    @param c_core_dir Path to the c-core directory
    @return Sorted list of full paths to .tart files *)

val load_c_core_files :
  c_core_dir:string ->
  ?with_prelude:bool ->
  ?source_version:Core.Type_env.emacs_version ->
  Core.Type_env.t ->
  Core.Type_env.t
(** Load all c-core signature files into a type environment.

    Iterates through all .tart files in the c-core directory and loads their
    signatures into the environment. Each file is treated as an independent
    module. Duplicate function definitions across files will use the last loaded
    value.

    @param c_core_dir Path to the c-core directory containing .tart files
    @param with_prelude If true (default), prelude types are available
    @param source_version
      Optional Emacs version to associate with loaded names (Spec 50)
    @param env Base type environment to extend
    @return Extended type environment with all c-core signatures *)

val load_c_core : search_path:t -> Core.Type_env.t -> Core.Type_env.t
(** Load c-core signatures from versioned typings.

    Uses the version fallback chain to find the c-core directory, then loads all
    .tart files from it.

    @param search_path The search path configuration
    @param env Base type environment to extend
    @return Extended type environment with c-core signatures *)

(** {1 Lisp-Core Loading} *)

val load_lisp_core : search_path:t -> Core.Type_env.t -> Core.Type_env.t
(** Load lisp-core signatures from versioned typings.

    Uses the version fallback chain to find the lisp-core directory, then loads
    all .tart files from it. Lisp-core contains signatures for functions and
    macros defined in Emacs Lisp (as opposed to C primitives in c-core).

    @param search_path The search path configuration
    @param env Base type environment to extend
    @return Extended type environment with lisp-core signatures *)

(** {1 Feature Version Resolution} *)

val resolve_feature_version :
  search_path:t -> string -> Core.Type_env.emacs_version option
(** Resolve a feature/module name to its source Emacs version.

    Looks up the module in the search path and extracts the version from the
    typings directory path. Returns [None] if the module is not found or is not
    from versioned typings. Used by redundant guard detection (Spec 49 R14). *)
