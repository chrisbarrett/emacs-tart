(** Signature loader and validator.

    This module converts signature AST to the type environment,
    validating type variable scoping and resolving type references.

    Key validations:
    - Type variables must be explicitly bound in quantifiers
    - Bounded quantifiers must reference valid types
    - Referenced types must be in scope (from opens or type decls)
*)

(** {1 Load Errors} *)

(** Error during signature loading *)
type load_error = {
  message : string;
  span : Syntax.Location.span;
}

(** {1 Type Variable Context} *)

(** Context for type variable resolution.
    Tracks which type variables are in scope. *)
type tvar_context

(** Empty context *)
val empty_context : tvar_context

(** Add bound type variables to context *)
val with_tvars : tvar_context -> string list -> tvar_context

(** Add a defined type to context *)
val with_type : tvar_context -> string -> tvar_context

(** {1 Type Validation} *)

(** Validate a sig_type, checking that all type variables are in scope.
    Returns Ok () if valid, Error with the first unbound variable otherwise. *)
val validate_type : tvar_context -> Sig_ast.sig_type -> (unit, load_error) result

(** {1 Declaration Validation} *)

(** Validate a single declaration *)
val validate_decl : tvar_context -> Sig_ast.decl -> (unit, load_error) result

(** {1 Signature Validation} *)

(** Build context from declarations.
    Adds all type declarations to the context so they can be referenced. *)
val build_context : Sig_ast.signature -> tvar_context

(** Validate an entire signature file.
    Returns Ok () if all declarations are valid, or the first error. *)
val validate_signature : Sig_ast.signature -> (unit, load_error) result

(** Validate a signature and collect all errors (not just the first). *)
val validate_signature_all : Sig_ast.signature -> load_error list

(** {1 Type Alias Context} *)

(** A type alias definition with optional parameters *)
type type_alias = {
  alias_params : string list;  (** Type parameters (e.g., [a e] in result) *)
  alias_body : Sig_ast.sig_type;       (** The definition body *)
}

(** Context for type alias expansion *)
type alias_context

(** Empty alias context *)
val empty_aliases : alias_context

(** Look up a type alias *)
val lookup_alias : string -> alias_context -> type_alias option

(** Build alias context from signature declarations.
    Only includes type declarations with bodies (aliases, not opaque types). *)
val build_alias_context : Sig_ast.signature -> alias_context

(** {1 Opaque Type Context} *)

(** An opaque type declaration with its parameters *)
type opaque_type = {
  opaque_params : string list;  (** Phantom type parameters (e.g., [a] in tagged) *)
  opaque_con : string;          (** The generated type constructor name *)
}

(** Context for opaque type lookup *)
type opaque_context

(** Empty opaque context *)
val empty_opaques : opaque_context

(** Look up an opaque type *)
val lookup_opaque : string -> opaque_context -> opaque_type option

(** Generate a unique type constructor name for an opaque type.
    Format: module_name/type_name *)
val opaque_con_name : string -> string -> string

(** Build opaque context from signature declarations.
    Only includes type declarations without bodies (opaque types). *)
val build_opaque_context : string -> Sig_ast.signature -> opaque_context

(** {1 Type Conversion} *)

(** Convert a signature type to a core type with alias expansion.
    [aliases] is the alias context for expansion.
    [tvar_names] is the list of bound type variable names in scope.
    Type variables are represented as TCon for later substitution. *)
val sig_type_to_typ_with_aliases :
  alias_context -> string list -> Sig_ast.sig_type -> Core.Types.typ

(** Convert a signature type to a core type (without alias expansion).
    [tvar_names] is the list of bound type variable names in scope.
    Type variables are represented as TCon for later substitution. *)
val sig_type_to_typ : string list -> Sig_ast.sig_type -> Core.Types.typ

(** Convert a signature parameter to a core parameter *)
val sig_param_to_param : string list -> Sig_ast.sig_param -> Core.Types.param

(** {1 Declaration Loading} *)

(** Convert a defun declaration to a type scheme.
    Returns a Poly scheme if the function has type parameters,
    otherwise a Mono scheme with an arrow type. *)
val load_defun : Sig_ast.defun_decl -> Core.Type_env.scheme

(** Convert a defvar declaration to a type scheme.
    The type may be polymorphic if it contains a forall. *)
val load_defvar : Sig_ast.defvar_decl -> Core.Type_env.scheme

(** {1 Module Resolution} *)

(** Module resolution callback type.
    Takes a module name and returns the parsed signature if found. *)
type module_resolver = string -> Sig_ast.signature option

(** A dummy resolver that finds nothing. Used as default. *)
val no_resolver : module_resolver

(** {1 Signature Loading} *)

(** Load a validated signature into a type environment with module resolution.
    Adds all function and variable declarations to the environment.
    Type aliases are expanded and opaque types are resolved during loading.
    Open directives import types only; include directives re-export values.
    Returns the extended environment.

    @param resolver Function to resolve module names to signatures
    @param env Base type environment to extend
    @param sig_file The signature to load *)
val load_signature_with_resolver :
  resolver:module_resolver ->
  Core.Type_env.t ->
  Sig_ast.signature ->
  Core.Type_env.t

(** Load a validated signature into a type environment.
    This is the simple interface without module resolution.
    Open and include directives will be ignored (no resolver provided).
    Adds all function and variable declarations to the environment.
    Type aliases are expanded during loading.
    Returns the extended environment. *)
val load_signature : Core.Type_env.t -> Sig_ast.signature -> Core.Type_env.t
