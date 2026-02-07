(** Module-aware type checking.

    This module extends the basic type checker with module boundary support:
    - Loads sibling `.tart` files for signature verification
    - Loads required modules from the search path
    - Verifies implementations match declared signatures

    R1: Any `.el` file can be type-checked via LSP R2: Verify implementations
    match `.tart` signatures R3: Load signatures from search path for required
    modules *)

(** {1 Configuration} *)

type config
(** Configuration for module-aware type checking *)

val default_config : unit -> config
(** Create a default configuration with no search path *)

val with_stdlib : string -> config -> config
(** Add stdlib directory to configuration *)

val with_search_dirs : string list -> config -> config
(** Set search directories for finding `.tart` files *)

val search_path : config -> Sig.Search_path.t
(** Get the search path from a config *)

val with_search_path : Sig.Search_path.t -> config -> config
(** Set the search path in a config *)

val with_declared_version : Core.Type_env.emacs_version -> config -> config
(** Set the declared minimum Emacs version (from Package-Requires) *)

(** {1 Errors} *)

type mismatch_error = {
  name : string;
  expected : Core.Types.typ;
  actual : Core.Types.typ;
  impl_span : Syntax.Location.span;
      (** Location of implementation in .el file *)
  sig_span : Syntax.Location.span;  (** Location of signature in .tart file *)
}
(** Error when implementation doesn't match signature *)

(** {1 Warnings} *)

type missing_signature_warning = { name : string; span : Syntax.Location.span }
(** Warning when a public function is not in the signature file *)

(** {1 Kind Errors} *)

type kind_check_error = {
  kind_error : Kind_infer.kind_error;
  span : Syntax.Location.span;
}
(** A kind error with its source location *)

(** {1 Check Result} *)

type check_result = {
  type_errors : Unify.error list;  (** Type errors from inference *)
  mismatch_errors : mismatch_error list;  (** Signature mismatch errors *)
  missing_signature_warnings : missing_signature_warning list;
      (** Warnings for public functions not in signature *)
  undefined_errors : Infer.undefined_var list;  (** Undefined variable errors *)
  exhaustiveness_warnings : Exhaustiveness.warning list;
      (** Warnings for non-exhaustive pcase matches *)
  kind_errors : kind_check_error list;
      (** Kind mismatch errors in signatures *)
  clause_diagnostics : Infer.resolved_clause_diagnostic list;
      (** Diagnostics emitted from multi-clause dispatch *)
  version_diagnostics : Diagnostic.t list;
      (** Version constraint warnings (E0900/E0901) *)
  signature_env : Core.Type_env.t option;
      (** Environment from loaded signature, if any *)
  final_env : Core.Type_env.t;  (** Final type environment after checking *)
}
(** Result of module-aware type checking *)

(** {1 Type Checking} *)

val check_module :
  config:config -> filename:string -> Syntax.Sexp.t list -> check_result
(** Type-check an elisp file with module awareness.

    This is the main entry point for module-aware type checking: 1. Load sibling
    `.tart` signature if present 2. Load signatures for required modules 3.
    Type-check the implementation 4. If signature exists, verify implementations
    match

    @param config Module check configuration
    @param filename Path to the `.el` file (for sibling lookup)
    @param sexps Parsed S-expressions from the file
    @return Check result with type errors and mismatch errors *)

(** {1 Require Detection} *)

val extract_requires : Syntax.Sexp.t list -> string list
(** Extract module names from require forms in parsed sexps *)

(** {1 Autoload Detection (R7)} *)

val extract_module_prefixes : string -> string list
(** Extract candidate module prefixes from a function name.

    Example: [extract_module_prefixes "my-package-autoload-fn"] returns
    [["my-package-autoload"; "my-package"; "my"]].

    Returns an empty list for single-word names (no hyphens). *)

val collect_all_call_symbols : Syntax.Sexp.t list -> string list
(** Collect all symbol references in function call position from parsed sexps.

    Only symbols at the head of a list (function calls) are collected. Returns a
    deduplicated, sorted list. *)

(** {1 Diagnostics} *)

val mismatch_to_diagnostic : mismatch_error -> Diagnostic.t
(** Convert a mismatch error to a diagnostic *)

val missing_signature_to_diagnostic : missing_signature_warning -> Diagnostic.t
(** Convert a missing signature warning to a diagnostic *)

val clause_diagnostic_to_diagnostic :
  Infer.resolved_clause_diagnostic -> Diagnostic.t
(** Convert a resolved clause diagnostic to a [Diagnostic.t].

    Maps severity: DiagError → Error, DiagWarn → Warning, DiagNote → Hint. *)

val diagnostics_of_result : check_result -> Diagnostic.t list
(** Get all diagnostics from a check result *)

val check_version_constraints :
  declared:Core.Type_env.emacs_version ->
  env:Core.Type_env.t ->
  Syntax.Sexp.t list ->
  Diagnostic.t list
(** Check version constraints for all function calls.

    Given a declared minimum Emacs version (from Package-Requires) and the type
    environment (with fn_versions from sig loading), produces warnings for calls
    to functions that require a newer version or were removed.

    Calls inside feature-guarded branches (featurep, fboundp, boundp,
    bound-and-true-p) are exempt from warnings (Spec 50 R11). *)
