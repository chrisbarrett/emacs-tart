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

(** {1 Errors} *)

type mismatch_error = {
  name : string;
  expected : Core.Types.typ;
  actual : Core.Types.typ;
  span : Syntax.Location.span;
}
(** Error when implementation doesn't match signature *)

(** {1 Check Result} *)

type check_result = {
  type_errors : Unify.error list;  (** Type errors from inference *)
  mismatch_errors : mismatch_error list;  (** Signature mismatch errors *)
  signature_env : Core.Type_env.t option;
      (** Environment from loaded signature, if any *)
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

(** {1 Diagnostics} *)

val mismatch_to_diagnostic : mismatch_error -> Diagnostic.t
(** Convert a mismatch error to a diagnostic *)

val diagnostics_of_result : check_result -> Diagnostic.t list
(** Get all diagnostics from a check result *)
