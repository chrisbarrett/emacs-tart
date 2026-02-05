(** Emacs C layer coverage analysis.

    Compares C definitions (DEFUNs, DEFVARs, DEFSYMs) scanned from Emacs source
    against loaded typings to determine type coverage.

    See Spec 29, R8 for requirements. *)

(** {1 Types} *)

type coverage_status =
  | Covered
  | Uncovered  (** Whether a C definition has a matching type signature. *)

type c_coverage_item = {
  definition : C_scanner.c_definition;
  status : coverage_status;
}
(** A single C definition with its coverage status. *)

type c_coverage_result = {
  items : c_coverage_item list;
  source_dir : string;  (** Path to the Emacs source directory *)
  emacs_version : string;  (** Emacs version string *)
  files_scanned : int;  (** Number of C files scanned *)
}
(** Results from C layer coverage analysis. *)

type c_coverage_summary = {
  total_public : int;  (** Total public symbols *)
  covered_public : int;  (** Symbols with type signatures *)
  uncovered_public : int;  (** Symbols without type signatures *)
  total_private : int;  (** Private symbols (excluded from percentage) *)
}
(** Summary statistics for C layer coverage. *)

(** {1 Typings Loading} *)

val load_typings :
  typings_root:string -> version:Sig.Emacs_version.version -> Core.Type_env.t
(** Load typings for a given Emacs version.

    Uses the version fallback chain: exact → minor → major → latest. *)

(** {1 Coverage Calculation} *)

val has_signature : Core.Type_env.t -> string -> bool
(** Check if a name exists in the type environment. *)

val calculate_coverage :
  source_dir:string ->
  emacs_version:string ->
  typings_root:string ->
  version:Sig.Emacs_version.version ->
  C_scanner.c_definition list ->
  c_coverage_result
(** Calculate coverage for C definitions.

    [R8]: Compare scanned symbols against loaded typings. A symbol is "covered"
    if it has a type signature in the typings.

    @param source_dir Path to the Emacs source directory (for report header)
    @param emacs_version Version string for the report header
    @param typings_root Root directory for versioned typings
    @param version Emacs version for typings lookup (used for fallback chain)
    @param definitions List of C definitions from the scanner *)

(** {1 Summary Statistics} *)

val summarize : c_coverage_result -> c_coverage_summary
(** Compute summary statistics from coverage results. *)

val coverage_percentage : c_coverage_summary -> float
(** Calculate coverage percentage (0.0 to 100.0). *)

(** {1 Filtering} *)

val uncovered_public : c_coverage_result -> c_coverage_item list
(** Get all uncovered public definitions, sorted alphabetically. *)

val covered_public : c_coverage_result -> c_coverage_item list
(** Get all covered public definitions, sorted alphabetically. *)

val private_definitions : c_coverage_result -> c_coverage_item list
(** Get all private definitions, sorted alphabetically. *)
