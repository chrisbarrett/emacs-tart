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
  | E0001  (** TypeMismatch: Expected one type, found another *)
  | E0002  (** BranchMismatch: If/cond branches have incompatible types *)
  | E0003  (** InfiniteType: Occurs check failed (recursive type) *)
  | E0004
      (** SignatureMismatch: Implementation doesn't match declared signature *)
  | E0005  (** AnnotationMismatch: Expression doesn't match tart annotation *)
  | E0006  (** ReturnMismatch: Function body doesn't match declared return *)
  | E0007  (** UnificationFailed: Types cannot be unified *)
  | E0008  (** DisjointEquality: eq/eql args are provably disjoint *)
  (* Name Errors (E0100–E0199) *)
  | E0100  (** UndefinedVariable: Variable not in scope *)
  | E0101  (** UndefinedFunction: Function not in scope *)
  | E0102  (** UndefinedType: Type not in scope *)
  | E0104  (** MissingSignature: Function defined but not in .tart file *)
  (* Arity Errors (E0200–E0299) *)
  | E0200  (** WrongArity: Wrong number of arguments to function *)
  | E0201  (** WrongTypeArity: Wrong number of type arguments *)
  (* Kind Errors (E0300–E0399) *)
  | E0300  (** KindMismatch: Expected one kind, found another *)
  | E0301  (** InfiniteKind: Occurs check failed at kind level *)
  | E0302  (** TypeArityMismatch: Type constructor applied to wrong # of args *)
  (* Pattern Errors (E0400–E0499) *)
  | E0400  (** NonExhaustive: Pattern match doesn't cover all cases *)
  (* Module Errors (E0700–E0799) *)
  | E0702  (** SignatureNotFound: No .tart signature file found *)

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
      error[E0001]: type mismatch: expected Int but found String
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

val to_string_human : t -> string
(** Format a diagnostic in Elm-style human-readable format with source excerpts.

    Per Spec 45: Shows Elm-style headers, source excerpts with underlines,
    conversational prose, and colored output when TTY is detected.

    Output format:
    {v
      -- TYPE MISMATCH ---------------------------------------- file.el:42:10

      I found a type mismatch in this expression:

      42 |   (upcase count)
         |           ^^^^^

      The function `upcase` expects argument 1 to be:

          String

      But this expression has type:

          Int

      Hint: convert the integer to a string: (number-to-string ...)
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
(** Create a kind mismatch diagnostic (E0300).

    Used when a type application has mismatched kinds, e.g., applying a type
    variable of kind [*] as if it were kind [* -> *]. *)

val of_kind_error : Syntax.Location.span -> Kind_infer.kind_error -> t
(** Convert a kind inference error to a diagnostic.

    The span is the location of the declaration that failed kind checking. *)

val missing_instance :
  span:Syntax.Location.span ->
  class_name:string ->
  typ:Core.Types.typ ->
  unit ->
  t
(** Create a missing type class instance diagnostic.

    Used when a function with type class constraints is called but no instance
    exists for the required constraint.

    Note: Type classes are not yet fully implemented. Error code will be
    assigned when type class support is added to the type system. *)

(** {1 JSON Serialization} *)

val to_json : t -> Yojson.Safe.t
(** Serialize a diagnostic to JSON.

    Output format:
    {v
      {
        "kind": "type",
        "code": "E0001",
        "severity": "error",
        "message": "type mismatch",
        "location": { "file": "init.el", "line": 42, "column": 10 },
        "expected": "String",
        "actual": "Int",
        "related": [...],
        "help": [...]
      }
    v} *)
