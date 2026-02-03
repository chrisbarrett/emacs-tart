(** Signature loader and validator.

    This module converts signature AST to the type environment, validating type
    variable scoping and resolving type references.

    Key features:
    - Implicit forall inference: type variables in signatures without explicit
      quantifiers are collected in left-to-right order
    - Type alias expansion during loading
    - Opaque type resolution

    Key validations:
    - When explicit quantifiers are provided, type variables must be bound
    - Bounded quantifiers must reference valid types
    - Referenced types must be in scope (from opens or type decls) *)

(** {1 Load Errors} *)

type load_error = { message : string; span : Syntax.Location.span }
(** Error during signature loading *)

(** {1 Type Variable Context} *)

type tvar_context
(** Context for type variable resolution. Tracks which type variables are in
    scope. *)

val empty_context : tvar_context
(** Empty context *)

val with_tvars : tvar_context -> string list -> tvar_context
(** Add bound type variables to context *)

val with_type : tvar_context -> string -> tvar_context
(** Add a defined type to context *)

(** {1 Type Validation} *)

val validate_type :
  tvar_context -> Sig_ast.sig_type -> (unit, load_error) result
(** Validate a sig_type, checking that all type variables are in scope. Returns
    Ok () if valid, Error with the first unbound variable otherwise. *)

(** {1 Declaration Validation} *)

val validate_decl : tvar_context -> Sig_ast.decl -> (unit, load_error) result
(** Validate a single declaration *)

(** {1 Signature Validation} *)

val build_context : Sig_ast.signature -> tvar_context
(** Build context from declarations. Adds all type declarations to the context
    so they can be referenced. *)

val validate_signature : Sig_ast.signature -> (unit, load_error) result
(** Validate an entire signature file. Returns Ok () if all declarations are
    valid, or the first error. *)

val validate_signature_all : Sig_ast.signature -> load_error list
(** Validate a signature and collect all errors (not just the first). *)

(** {1 Type Alias Context} *)

type alias_param = {
  ap_name : string;  (** Parameter name (e.g., "a") *)
  ap_bound : Sig_ast.sig_type option;  (** Upper bound (e.g., truthy) *)
}
(** A type parameter with optional bound *)

type type_alias = {
  alias_params : alias_param list;  (** Type parameters with optional bounds *)
  alias_body : Sig_ast.sig_type;  (** The definition body *)
}
(** A type alias definition with optional parameters *)

type alias_context
(** Context for type alias expansion *)

val empty_aliases : alias_context
(** Empty alias context *)

val lookup_alias : string -> alias_context -> type_alias option
(** Look up a type alias *)

val add_alias : string -> type_alias -> alias_context -> alias_context
(** Add a type alias to the context *)

val alias_names : alias_context -> string list
(** Get all alias names from the context *)

val build_alias_context : Sig_ast.signature -> alias_context
(** Build alias context from signature declarations. Only includes type
    declarations with bodies (aliases, not opaque types). *)

(** {1 Opaque Type Context} *)

type opaque_type = {
  opaque_params : string list;
      (** Phantom type parameters (e.g., [a] in tagged) *)
  opaque_con : string;  (** The generated type constructor name *)
}
(** An opaque type declaration with its parameters *)

type opaque_context
(** Context for opaque type lookup *)

val empty_opaques : opaque_context
(** Empty opaque context *)

val lookup_opaque : string -> opaque_context -> opaque_type option
(** Look up an opaque type *)

val opaque_con_name : string -> string -> string
(** Generate a unique type constructor name for an opaque type. Format:
    module_name/type_name *)

val add_opaque : string -> opaque_type -> opaque_context -> opaque_context
(** Add an opaque type to the context *)

val build_opaque_context : string -> Sig_ast.signature -> opaque_context
(** Build opaque context from signature declarations. Only includes type
    declarations without bodies (opaque types). *)

(** {1 Type Context}

    Combined context for type resolution during signature loading. Contains both
    aliases and opaque types. *)

type type_context = { tc_aliases : alias_context; tc_opaques : opaque_context }

val empty_type_context : type_context
(** Empty type context with no aliases or opaques *)

(** {1 Type Conversion} *)

val sig_type_to_typ_with_aliases :
  alias_context -> string list -> Sig_ast.sig_type -> Core.Types.typ
(** Convert a signature type to a core type with alias expansion. [aliases] is
    the alias context for expansion. [tvar_names] is the list of bound type
    variable names in scope. Type variables are represented as TCon for later
    substitution. *)

val sig_type_to_typ : string list -> Sig_ast.sig_type -> Core.Types.typ
(** Convert a signature type to a core type (without alias expansion).
    [tvar_names] is the list of bound type variable names in scope. Type
    variables are represented as TCon for later substitution. *)

val sig_param_to_param : string list -> Sig_ast.sig_param -> Core.Types.param
(** Convert a signature parameter to a core parameter *)

(** {1 Declaration Loading} *)

val load_defun : Sig_ast.defun_decl -> Core.Type_env.scheme
(** Convert a defun declaration to a type scheme without alias expansion.
    Returns a Poly scheme if the function has type parameters, otherwise a Mono
    scheme with an arrow type. For proper intrinsic type name resolution, prefer
    [load_defun_with_ctx] with the prelude context. *)

val load_defun_with_ctx :
  type_context -> Sig_ast.defun_decl -> Core.Type_env.scheme
(** Convert a defun declaration to a type scheme with alias expansion. Use this
    with the prelude context to ensure primitive type names (int, string, etc.)
    are resolved to their intrinsic representations. *)

val load_defvar : Sig_ast.defvar_decl -> Core.Type_env.scheme
(** Convert a defvar declaration to a type scheme without alias expansion. The
    type may be polymorphic if it contains a forall. For proper intrinsic type
    name resolution, prefer [load_defvar_with_ctx] with the prelude context. *)

val load_defvar_with_ctx :
  type_context -> Sig_ast.defvar_decl -> Core.Type_env.scheme
(** Convert a defvar declaration to a type scheme with alias expansion. Use this
    with the prelude context to ensure primitive type names (int, string, etc.)
    are resolved to their intrinsic representations. *)

(** {1 Module Resolution} *)

type module_resolver = string -> Sig_ast.signature option
(** Module resolution callback type. Takes a module name and returns the parsed
    signature if found. *)

val no_resolver : module_resolver
(** A dummy resolver that finds nothing. Used as default. *)

(** {1 Signature Loading} *)

val load_signature_with_resolver :
  ?prelude_ctx:type_context ->
  ?prelude_type_names:string list ->
  resolver:module_resolver ->
  Core.Type_env.t ->
  Sig_ast.signature ->
  Core.Type_env.t
(** Load a validated signature into a type environment with module resolution.
    Applies forall inference per-declaration during loading, using accumulated
    type context so that types from includes/opens are recognized. Adds all
    function and variable declarations to the environment. Type aliases are
    expanded and opaque types are resolved during loading. Open directives
    import types only; include directives re-export values. Returns the extended
    environment.

    @param prelude_ctx
      Optional prelude type context. When provided, prelude type aliases (list,
      option, etc.) are available for use in the signature without explicit
      import.
    @param prelude_type_names
      Optional list of prelude type names. When provided, these names cannot be
      redefined by the signature (no-shadowing rule, Spec 07 R17).
    @param resolver Function to resolve module names to signatures
    @param env Base type environment to extend
    @param sig_file The signature to load *)

val load_signature : Core.Type_env.t -> Sig_ast.signature -> Core.Type_env.t
(** Load a validated signature into a type environment. This is the simple
    interface without module resolution. Applies forall inference
    per-declaration during loading. Open and include directives will be ignored
    (no resolver provided). Adds all function and variable declarations to the
    environment. Type aliases are expanded during loading. Returns the extended
    environment. *)
