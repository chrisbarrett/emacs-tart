(** Type environment for tracking variable bindings during type inference.

    The type environment maps variable names to their type schemes. It also
    tracks the current level for let-generalization.

    Additionally tracks predicate information for type narrowing (Spec 52). When
    a function is declared with a predicate return type like [(x is string)],
    the predicate info is stored so that conditionals can narrow variable types.
*)

(** {1 Type Predicates} *)

type predicate_info = {
  param_index : int;  (** Index of the parameter being narrowed (0-based) *)
  param_name : string;  (** Name of the parameter (for validation) *)
  narrowed_type : Types.typ;  (** Type the parameter narrows to when true *)
}
(** Information about a type predicate function.

    When a function is declared as a predicate, it narrows the type of a
    specific parameter when called in a conditional. For example:
    {[
      (defun stringp (x) -> (x is string))
    ]}
    Declares [stringp] as narrowing parameter [x] to [string] when true. *)

(** {1 Loaded Clauses} *)

(** Severity level for clause diagnostics.

    Mirrors [Sig_ast.diagnostic_severity] but lives in the core layer so
    inference can consume it without depending on the sig library. *)
type diagnostic_severity =
  | DiagError  (** Error: the usage is incorrect *)
  | DiagWarn  (** Warning: the usage is risky *)
  | DiagNote  (** Note: informational message *)

type loaded_diagnostic = {
  ld_severity : diagnostic_severity;
  ld_message : string;  (** Format string with [%s] placeholders *)
  ld_args : string list;
      (** Type variable names for [%s] substitution at the call site *)
}
(** A diagnostic annotation carried through from a clause in a [.tart] file.

    When a clause with a diagnostic is selected during overload resolution, the
    diagnostic is emitted at the call site. [%s] placeholders in the message are
    replaced by the stringified types of the referenced type variables. *)

type loaded_clause = {
  lc_params : Types.param list;  (** Parameter types for this clause *)
  lc_return : Types.typ;  (** Return type for this clause *)
  lc_diagnostic : loaded_diagnostic option;
      (** Optional diagnostic emitted when this clause matches *)
}
(** A single clause from a multi-clause defun, preserved through loading.

    During signature loading, multi-clause defuns are merged into a single union
    arrow type for backward compatibility. The clause list is preserved
    alongside for call-site overload resolution (Spec 56). *)

(** {1 Type Schemes} *)

(** A type scheme is a possibly-polymorphic type.

    - [Mono ty] is a monomorphic type (no quantified variables).
    - [Poly (vars, ty)] is a polymorphic type with bound type variables.

    Type schemes are created during let-generalization when the RHS is a
    syntactic value (lambda, literal, variable, constructor application). *)
type scheme = Mono of Types.typ | Poly of string list * Types.typ

(** {1 Environment} *)

type t = {
  bindings : (string * scheme) list;
      (** Variable namespace: let, setq, defvar, lambda params *)
  fn_bindings : (string * scheme) list;
      (** Function namespace: defun, defalias, flet *)
  fn_clauses : (string * loaded_clause list) list;
      (** Multi-clause function signatures: maps function names to their
          preserved clause list for call-site overload resolution (Spec 56).
          Only present for multi-clause defuns loaded from .tart files. *)
  predicates : (string * predicate_info) list;
      (** Type predicates: maps function names to their predicate info *)
  feature_loader : (string -> t -> t) option;
      (** Optional callback to load a feature's signatures into the env (Spec
          49). Called with the feature name and current env; returns extended
          env. Set by module_check.ml before type checking begins. *)
  level : int;  (** Current scope level for generalization *)
}
(** Type environment with dual namespaces for Elisp's Lisp-2 semantics.

    Elisp has separate namespaces for variables and functions. A symbol can have
    both a value and a function definition. This environment tracks both:
    - [bindings] for variables (accessed via symbol evaluation)
    - [fn_bindings] for functions (accessed via function position or #'name) *)

(** {1 Creation} *)

val empty : t
(** Empty environment at level 0. *)

val of_list : (string * scheme) list -> t
(** Create an environment with initial bindings. *)

(** {1 Level management} *)

val current_level : t -> int
(** Get the current level. *)

val enter_level : t -> t
(** Enter a new scope (increment level). *)

val exit_level : t -> t
(** Exit a scope (decrement level). *)

(** {1 Lookup} *)

val lookup : string -> t -> scheme option
(** Look up a name, checking variable namespace first, then function namespace.

    This provides backward compatibility while supporting Lisp-2 semantics. Use
    [lookup_fn] to look up only in function namespace. *)

val lookup_fn : string -> t -> scheme option
(** Look up a name in the function namespace only. *)

val lookup_var : string -> t -> scheme option
(** Look up a name in the variable namespace only (no fallback to functions).

    Use this for setq/assignment checking where we only want to check if a
    variable binding already exists, not if a function with that name exists. *)

val names : t -> string list
(** Get all names bound in the variable namespace. *)

val fn_names : t -> string list
(** Get all names bound in the function namespace. *)

(** {1 Extension (Variable Namespace)} *)

val extend : string -> scheme -> t -> t
(** Extend the variable namespace with a new binding. *)

val extend_mono : string -> Types.typ -> t -> t
(** Extend variable namespace with a monomorphic binding. *)

val extend_monos : (string * Types.typ) list -> t -> t
(** Extend variable namespace with multiple monomorphic bindings. *)

val extend_poly : string -> string list -> Types.typ -> t -> t
(** Extend variable namespace with a polymorphic binding. *)

(** {1 Extension (Function Namespace)} *)

val extend_fn : string -> scheme -> t -> t
(** Extend the function namespace with a new binding. *)

val extend_fn_mono : string -> Types.typ -> t -> t
(** Extend function namespace with a monomorphic binding. *)

val extend_fn_poly : string -> string list -> Types.typ -> t -> t
(** Extend function namespace with a polymorphic binding. *)

val extend_fn_clauses : string -> loaded_clause list -> t -> t
(** Register the preserved clause list for a multi-clause defun. *)

val lookup_fn_clauses : string -> t -> loaded_clause list option
(** Look up the preserved clause list for a function, if any. *)

(** {1 Predicates} *)

val lookup_predicate : string -> t -> predicate_info option
(** Look up predicate info for a function name. *)

val extend_predicate : string -> predicate_info -> t -> t
(** Register a function as a type predicate. *)

val with_narrowed_var : string -> Types.typ -> t -> t
(** [with_narrowed_var name ty env] overrides [name]'s type to [ty].

    Used by predicate narrowing to refine a variable's type in conditional
    branches. Shadows any existing binding for [name] in the variable namespace.
*)

(** {1 Feature Loading (Spec 49)} *)

val set_feature_loader : (string -> t -> t) -> t -> t
(** [set_feature_loader loader env] installs a callback for loading feature
    signatures on demand. The loader is called with a feature name and the
    current env, and returns the env extended with the feature's signatures. *)

val load_feature : string -> t -> t
(** [load_feature name env] loads a feature's signatures into the env.

    Returns the extended env if a loader is set and the feature is found, or the
    original env if no loader is set. *)

(** {1 Instantiation} *)

val instantiate : scheme -> t -> Types.typ
(** [instantiate scheme env] instantiates a type scheme at the current level.

    For monomorphic types, returns the type as-is. For polymorphic types,
    replaces bound variables with fresh type variables. *)

(** {1 Pretty-printing} *)

val scheme_to_string : scheme -> string
(** Convert a scheme to string for debugging. *)

val to_string : t -> string
(** Pretty-print the environment for debugging. *)
