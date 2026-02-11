(** Emacs coverage analysis for C and Elisp layers.

    Compares C definitions (DEFUNs, DEFVARs, DEFSYMs) and Elisp definitions
    (defun, defvar, etc.) scanned from Emacs source against loaded typings to
    determine type coverage.

    See Spec 29, R8 and Spec 95 for requirements. *)

(** {1 Types} *)

type coverage_status =
  | Covered
  | Uncovered  (** Whether a definition has a matching type signature. *)

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

(** {1 C Typings Loading} *)

val load_typings :
  typings_root:string -> version:Sig.Emacs_version.version -> Core.Type_env.t
(** Load C layer typings for a given Emacs version.

    Uses the version fallback chain: exact → minor → major → latest. *)

(** {1 C Coverage Calculation} *)

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

(** {1 C Summary Statistics} *)

val summarize : c_coverage_result -> c_coverage_summary
(** Compute summary statistics from C coverage results. *)

val coverage_percentage : c_coverage_summary -> float
(** Calculate C coverage percentage (0.0 to 100.0). *)

(** {1 C Filtering} *)

val uncovered_public : c_coverage_result -> c_coverage_item list
(** Get all uncovered public C definitions, sorted alphabetically. *)

val covered_public : c_coverage_result -> c_coverage_item list
(** Get all covered public C definitions, sorted alphabetically. *)

val private_definitions : c_coverage_result -> c_coverage_item list
(** Get all private C definitions, sorted alphabetically. *)

(** {1 Elisp Layer Types} *)

type elisp_coverage_item = {
  definition : Definition_extractor.definition;
  status : coverage_status;
}
(** A single Elisp definition with its coverage status. *)

type elisp_file_result = {
  filename : string;  (** Source .el file path *)
  items : elisp_coverage_item list;  (** Definitions from this file *)
}
(** Coverage results for a single Elisp file. *)

type elisp_coverage_result = {
  file_results : elisp_file_result list;
  source_dir : string;  (** Path to the Emacs source directory *)
  emacs_version : string;  (** Emacs version string *)
}
(** Results from Elisp layer coverage analysis. *)

type elisp_coverage_summary = {
  elisp_total_public : int;  (** Total public Elisp symbols *)
  elisp_covered_public : int;  (** Elisp symbols with type signatures *)
  elisp_uncovered_public : int;  (** Elisp symbols without type signatures *)
  elisp_total_private : int;
      (** Private Elisp symbols (excluded from percentage) *)
}
(** Summary statistics for Elisp layer coverage. *)

(** {1 Elisp Typings Loading} *)

val load_lisp_typings :
  typings_root:string -> version:Sig.Emacs_version.version -> Core.Type_env.t
(** Load Elisp layer typings for a given Emacs version.

    Uses the version fallback chain: exact → minor → major → latest. *)

(** {1 Elisp Coverage Calculation} *)

val scan_elisp_files : string -> Definition_extractor.extraction_result list
(** Scan all .el files under the [lisp/] subdirectory of the given source
    directory. *)

val calculate_elisp_coverage :
  source_dir:string ->
  emacs_version:string ->
  typings_root:string ->
  version:Sig.Emacs_version.version ->
  elisp_coverage_result
(** Calculate coverage for Elisp definitions.

    [Spec 95 R1-R4]: Scan [lisp/*.el] files and compare definitions against
    [lisp-core] typings. Files without matching typings show 0/N public
    coverage.

    @param source_dir Path to the Emacs source directory
    @param emacs_version Version string for the report header
    @param typings_root Root directory for versioned typings
    @param version Emacs version for typings lookup *)

(** {1 Elisp Summary Statistics} *)

val elisp_summarize : elisp_coverage_result -> elisp_coverage_summary
(** Compute summary statistics from Elisp coverage results. *)

val elisp_coverage_percentage : elisp_coverage_summary -> float
(** Calculate Elisp coverage percentage (0.0 to 100.0). *)

(** {1 Elisp Filtering} *)

val elisp_uncovered_public : elisp_coverage_result -> elisp_coverage_item list
(** Get all uncovered public Elisp definitions, sorted alphabetically. *)

val elisp_covered_public : elisp_coverage_result -> elisp_coverage_item list
(** Get all covered public Elisp definitions, sorted alphabetically. *)

val elisp_private_definitions :
  elisp_coverage_result -> elisp_coverage_item list
(** Get all private Elisp definitions, sorted alphabetically. *)

(** {1 Combined Summary} *)

type combined_summary = {
  c_summary : c_coverage_summary;
  elisp_summary : elisp_coverage_summary;
  total_public : int;  (** Total public symbols across both layers *)
  total_covered : int;  (** Total covered symbols across both layers *)
  total_uncovered : int;  (** Total uncovered symbols across both layers *)
  total_private : int;  (** Total private symbols across both layers *)
}
(** Aggregate summary across C and Elisp layers. *)

val combine_summaries :
  c_coverage_summary -> elisp_coverage_summary -> combined_summary
(** Combine C and Elisp coverage summaries into a unified summary. *)

val combined_coverage_percentage : combined_summary -> float
(** Calculate combined coverage percentage (0.0 to 100.0). *)
