(** Type error diagnostics for user-facing error messages.

    This module provides structured diagnostic information for type errors,
    including source locations, expected vs actual types, and related locations
    (provenance) for understanding where types originated.

    Error codes follow Spec 47 (Error Code Registry):
    - Type Errors (E0001–E0099)
    - Name Errors (E0100–E0199)
    - Arity Errors (E0200–E0299)
    - Kind Errors (E0300–E0399)
    - Pattern Errors (E0400–E0499)
    - Row/Record Errors (E0500–E0599)
    - Union Errors (E0600–E0699)
    - Module Errors (E0700–E0799)
    - File Errors (E0800–E0899) *)

(** {1 Types} *)

(** Error codes for categorizing diagnostics.

    Codes are assigned per Spec 47 (Error Code Registry). Once assigned, codes
    are never reassigned. *)
type error_code =
  (* Type Errors (E0001–E0099) *)
  | TypeMismatch  (** E0001: Expected one type, found another *)
  | BranchMismatch  (** E0002: If/cond branches have incompatible types *)
  | InfiniteType  (** E0003: Occurs check failed (recursive type) *)
  | SignatureMismatch
      (** E0004: Implementation doesn't match declared signature *)
  | AnnotationMismatch  (** E0005: Expression doesn't match tart annotation *)
  | ReturnMismatch  (** E0006: Function body doesn't match declared return *)
  | UnificationFailed  (** E0007: Types cannot be unified *)
  | DisjointEquality  (** E0008: eq/eql args are provably disjoint *)
  (* Name Errors (E0100–E0199) *)
  | UndefinedVariable  (** E0100: Variable not in scope *)
  | UndefinedFunction  (** E0101: Function not in scope *)
  | UndefinedType  (** E0102: Type not in scope *)
  | UndefinedField  (** E0103: Field not present in record type *)
  | MissingSignature  (** E0104: Function defined but not in .tart file *)
  | AmbiguousName  (** E0105: Name resolves to multiple definitions *)
  (* Arity Errors (E0200–E0299) *)
  | WrongArity  (** E0200: Wrong number of arguments to function *)
  | WrongTypeArity  (** E0201: Wrong number of type arguments *)
  | MissingRequired  (** E0202: Required argument not provided *)
  | UnknownKeyword  (** E0203: Unknown keyword argument *)
  (* Kind Errors (E0300–E0399) *)
  | KindMismatch  (** E0300: Expected one kind, found another *)
  | InfiniteKind  (** E0301: Occurs check failed at kind level *)
  | TypeArityMismatch  (** E0302: Type constructor applied to wrong # of args *)
  (* Pattern Errors (E0400–E0499) *)
  | NonExhaustive  (** E0400: Pattern match doesn't cover all cases *)
  | RedundantPattern  (** E0401: Pattern can never match *)
  | InvalidPattern  (** E0402: Pattern syntax not supported *)
  (* Row/Record Errors (E0500–E0599) *)
  | MissingField  (** E0500: Required field not present *)
  | DuplicateField  (** E0501: Field specified multiple times *)
  | RowMismatch  (** E0502: Row types cannot be unified *)
  | ClosedRowExtra  (** E0503: Extra field in closed row type *)
  (* Union Errors (E0600–E0699) *)
  | UnionMismatch  (** E0600: Value does not match any union variant *)
  | EmptyUnion  (** E0601: Type subtraction produced empty type *)
  | AmbiguousVariant  (** E0602: Cannot determine which union variant *)
  (* Module Errors (E0700–E0799) *)
  | MissingModule  (** E0700: Required module not found *)
  | CircularDependency  (** E0701: Modules have circular require *)
  | SignatureNotFound  (** E0702: No .tart signature file found *)
  (* File Errors (E0800–E0899) *)
  | FileNotFound  (** E0800: Source file does not exist *)
  | FileUnreadable  (** E0801: Source file cannot be read *)
  | ParseError  (** E0802: Syntax error in source file *)
  (* Version Errors (E0900–E0999) *)
  | VersionTooLow  (** E0900: Feature requires newer Emacs version *)
  | VersionTooHigh  (** E0901: Feature removed in declared Emacs version *)
  | VersionParseFailed  (** E0902: Package-Requires parse error *)
  | RedundantGuard  (** E0903: Feature guard is redundant given min version *)

val error_code_to_string : error_code -> string
(** Format an error code for display. *)

(** Severity level for diagnostics. *)
type severity = Error | Warning | Hint

type related_location = { span : Syntax.Location.span; message : string }
(** A related location with context. *)

type t = {
  severity : severity;
  code : error_code option;  (** Error code for categorization *)
  span : Syntax.Location.span;  (** Primary location of the error *)
  message : string;  (** Main error message *)
  expected : Core.Types.typ option;  (** Expected type (if applicable) *)
  actual : Core.Types.typ option;  (** Actual type found (if applicable) *)
  related : related_location list;  (** Related locations with context *)
  help : string list;  (** Suggested fixes *)
}
(** A structured diagnostic message. *)

(** {1 Construction} *)

val type_mismatch :
  span:Syntax.Location.span ->
  expected:Core.Types.typ ->
  actual:Core.Types.typ ->
  ?related:related_location list ->
  unit ->
  t
(** Create a type mismatch diagnostic with help suggestions.

    Automatically detects Option/nil mismatches and suggests appropriate fixes
    like [when-let] or [or] for nil handling. *)

val arity_mismatch :
  span:Syntax.Location.span ->
  expected:int ->
  actual:int ->
  ?related:related_location list ->
  unit ->
  t
(** Create an arity mismatch diagnostic. *)

val arity_mismatch_with_context :
  span:Syntax.Location.span ->
  expected:int ->
  actual:int ->
  context:Constraint.context ->
  unit ->
  t
(** Create an arity mismatch diagnostic with function context.

    When context is [FunctionArg], includes a note showing the function
    signature and formats expected arity as a range for optional params (e.g.,
    "2-3 arguments" or "1+ arguments" for rest params). *)

val occurs_check :
  span:Syntax.Location.span ->
  tvar_id:Core.Types.tvar_id ->
  typ:Core.Types.typ ->
  ?related:related_location list ->
  unit ->
  t
(** Create an occurs check (infinite type) diagnostic. *)

val missing_signature : span:Syntax.Location.span -> name:string -> unit -> t
(** Create a warning for a public function not in signature file. *)

val signature_mismatch :
  name:string ->
  impl_span:Syntax.Location.span ->
  impl_type:Core.Types.typ ->
  sig_span:Syntax.Location.span ->
  sig_type:Core.Types.typ ->
  unit ->
  t
(** Create a signature mismatch diagnostic (E0004) showing both locations.

    Used when a function's implementation type doesn't match its declared
    signature. [impl_span] is the location in the [.el] file, [sig_span] is the
    location in the [.tart] file. *)

val undefined_variable :
  span:Syntax.Location.span ->
  name:string ->
  candidates:string list ->
  unit ->
  t
(** Create an undefined variable diagnostic (E0100) with typo suggestions.

    Takes the undefined name and a list of candidate names from the environment
    to generate "did you mean?" suggestions using Levenshtein distance. *)

val branch_mismatch :
  span:Syntax.Location.span ->
  this_type:Core.Types.typ ->
  other_branch_span:Syntax.Location.span ->
  other_type:Core.Types.typ ->
  is_then:bool ->
  unit ->
  t
(** Create a branch type mismatch diagnostic (E0002).

    Used when if/cond branches have incompatible types. [span] is the location
    of the branch with the error, [other_branch_span] is the location of the
    other branch, and [is_then] indicates whether this is the then branch. *)

(** {1 Conversion from unification errors} *)

val of_unify_error : Unify.error -> t
(** Convert a unification error to a diagnostic. *)

val of_unify_errors : Unify.error list -> t list
(** Convert a list of unification errors to diagnostics. *)

(** {1 Inspection} *)

val span : t -> Syntax.Location.span
(** Get the primary span of a diagnostic. *)

val code : t -> error_code option
(** Get the error code of a diagnostic. *)

val help : t -> string list
(** Get the help suggestions for a diagnostic. *)

val all_spans : t -> Syntax.Location.span list
(** Get all spans (primary and related) from a diagnostic. *)

val is_error : t -> bool
(** Check if a diagnostic is an error (vs warning/hint). *)

val count_errors : t list -> int
(** Count errors in a list of diagnostics. *)

val non_exhaustive_match :
  span:Syntax.Location.span -> message:string -> unit -> t
(** Create a non-exhaustive pattern match warning.

    Used when a pcase expression doesn't cover all constructors of an ADT.
    Includes a help suggestion to add a wildcard pattern. *)

val kind_mismatch :
  span:Syntax.Location.span ->
  expected:Kind.kind ->
  found:Kind.kind ->
  location:string ->
  unit ->
  t
(** Create a kind mismatch diagnostic (E0300).

    Used when a type application has mismatched kinds, e.g., applying a type
    variable of kind [*] as if it were kind [* -> *]. *)

val of_kind_error : Syntax.Location.span -> Kind_infer.kind_error -> t
(** Convert a kind inference error to a diagnostic.

    The span is the location of the declaration that failed kind checking. *)

val version_too_low :
  span:Syntax.Location.span ->
  name:string ->
  required:Core.Type_env.emacs_version ->
  declared:Core.Type_env.emacs_version ->
  unit ->
  t
(** Create a version-too-low warning (E0900).

    Used when code calls a function that requires a newer Emacs version than the
    package declares via Package-Requires. Includes help text suggesting a
    version bump or feature guard. *)

val version_too_high :
  span:Syntax.Location.span ->
  name:string ->
  removed_after:Core.Type_env.emacs_version ->
  declared:Core.Type_env.emacs_version ->
  unit ->
  t
(** Create a version-too-high warning (E0901).

    Used when code calls a function that was removed after a certain Emacs
    version, but the package declares a higher minimum. *)

val redundant_guard :
  span:Syntax.Location.span ->
  guard_name:string ->
  available_since:Core.Type_env.emacs_version ->
  declared:Core.Type_env.emacs_version ->
  unit ->
  t
(** Create a redundant guard warning (E0903).

    Used when a feature guard like [(featurep 'json)] or [(fboundp 'f)] is
    redundant because the package's minimum Emacs version already guarantees
    availability. Types still resolve; this is a warning only (Spec 49 R14). *)
