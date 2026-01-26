(** Type error diagnostics for user-facing error messages.

    This module provides structured diagnostic information for type errors,
    including source locations, expected vs actual types, and related locations
    (provenance) for understanding where types originated.

    Error codes follow Rust conventions:
    - E0308: Type mismatch
    - E0317: Incompatible branch types
    - E0061: Wrong number of arguments
    - E0106: Infinite type (occurs check) *)

(** {1 Types} *)

(** Error codes for categorizing diagnostics. *)
type error_code =
  | E0308  (** Type mismatch *)
  | E0317  (** Incompatible branch types *)
  | E0061  (** Wrong number of arguments (arity) *)
  | E0106  (** Infinite type (occurs check) *)
  | E0425  (** Undefined variable *)
  | E0509  (** Kind mismatch *)

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
(** Create a signature mismatch diagnostic (E0308) showing both locations.

    Used when a function's implementation type doesn't match its declared
    signature. [impl_span] is the location in the [.el] file, [sig_span] is the
    location in the [.tart] file. *)

val undefined_variable :
  span:Syntax.Location.span ->
  name:string ->
  candidates:string list ->
  unit ->
  t
(** Create an undefined variable diagnostic (E0425) with typo suggestions.

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
(** Create a branch type mismatch diagnostic (E0317).

    Used when if/cond branches have incompatible types. [span] is the location
    of the branch with the error, [other_branch_span] is the location of the
    other branch, and [is_then] indicates whether this is the then branch. *)

(** {1 Conversion from unification errors} *)

val of_unify_error : Unify.error -> t
(** Convert a unification error to a diagnostic. *)

val of_unify_errors : Unify.error list -> t list
(** Convert a list of unification errors to diagnostics. *)

(** {1 Formatting} *)

val format_pos : Syntax.Location.pos -> string
(** Format a source position for display. *)

val format_span : Syntax.Location.span -> string
(** Format a source span for display. *)

val format_severity : severity -> string
(** Format severity for display. *)

val to_string : t -> string
(** Format a diagnostic as a human-readable string.

    Output format (similar to rustc/clang):
    {v
      error[E0308]: type mismatch: expected Int but found String
        --> file.el:10:5
        |
        = expected: Int
        = found: String
        |
      note: expected type from function signature
        --> file.el:5:1
        |
      help: convert the integer to a string: (number-to-string ...)
    v} *)

val to_string_compact : t -> string
(** Format a diagnostic in a compact single-line format.

    Useful for IDE integration and machine parsing. Format:
    [file:line:col: severity[CODE]: message [expected: T1, found: T2]] *)

val to_string_list : t list -> string
(** Format multiple diagnostics. *)

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
(** Create a kind mismatch diagnostic (E0509).

    Used when a type application has mismatched kinds, e.g., applying a type
    variable of kind [*] as if it were kind [* -> *]. *)

val of_kind_error : Syntax.Location.span -> Kind_infer.kind_error -> t
(** Convert a kind inference error to a diagnostic.

    The span is the location of the declaration that failed kind checking. *)
