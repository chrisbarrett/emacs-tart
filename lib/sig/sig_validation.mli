(** Signature validation.

    Validates signature ASTs before loading, checking that all type variables
    are in scope and referenced types exist.

    Key validations:
    - Type variables must be explicitly bound in quantifiers
    - Bounded quantifiers must reference valid types
    - Referenced types must be in scope (from opens or type decls) *)

(** {1 Validation Errors} *)

type validation_error = { message : string; span : Syntax.Location.span }
(** Error during signature validation *)

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
  tvar_context -> Sig_ast.sig_type -> (unit, validation_error) result
(** Validate a sig_type, checking that all type variables are in scope. Returns
    Ok () if valid, Error with the first unbound variable otherwise. *)

(** {1 Declaration Validation} *)

val validate_decl :
  tvar_context -> Sig_ast.decl -> (unit, validation_error) result
(** Validate a single declaration *)

(** {1 Signature Validation} *)

val build_context : Sig_ast.signature -> tvar_context
(** Build context from declarations. Adds all type declarations to the context
    so they can be referenced. *)

val validate_signature :
  ?prelude_type_names:string list ->
  Sig_ast.signature ->
  (unit, validation_error) result
(** Validate an entire signature file. Returns Ok () if all declarations are
    valid, or the first error.

    @param prelude_type_names
      Optional list of type names from the prelude that should be considered
      valid. This allows signatures to reference prelude types like buffer,
      window, etc. without declaring them locally. *)

val validate_signature_all :
  ?prelude_type_names:string list -> Sig_ast.signature -> validation_error list
(** Validate a signature and collect all errors (not just the first).

    @param prelude_type_names
      Optional list of type names from the prelude that should be considered
      valid. *)
