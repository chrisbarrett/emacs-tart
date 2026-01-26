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

(** {1 Type Conversion} *)

(** Convert a signature type to a core type.
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

(** {1 Signature Loading} *)

(** Load a validated signature into a type environment.
    Adds all function and variable declarations to the environment.
    Returns the extended environment. *)
val load_signature : Core.Type_env.t -> Sig_ast.signature -> Core.Type_env.t
