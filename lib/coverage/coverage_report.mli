(** Coverage report generation.

    This module compares definitions from .el files against signatures in .tart
    files to calculate coverage.

    See Spec 28, R10-R13 for requirements. *)

(** {1 Types} *)

type coverage_status =
  | Covered
  | Uncovered  (** Whether a definition has a matching signature. *)

type coverage_item = {
  definition : Definition_extractor.definition;
  source_file : string;  (** Path to the .el file *)
  status : coverage_status;
}
(** A single definition with its coverage status. *)

type coverage_result = { items : coverage_item list; files_scanned : int }
(** Results from coverage analysis. *)

type coverage_summary = {
  total_public : int;
  covered_public : int;
  total_private : int;
}
(** Summary statistics for coverage. *)

(** {1 Signature Matching} *)

val has_signature : Core.Type_env.t -> string -> bool
(** Check if a definition name exists in a type environment. *)

val sibling_tart_path : string -> string
(** Get the sibling .tart path for a .el file. *)

val load_sibling_signatures : string -> Core.Type_env.t -> Core.Type_env.t
(** Load signatures from a sibling .tart file if it exists. *)

(** {1 Coverage Calculation} *)

val analyze_file : search_path:Sig.Search_path.t -> string -> coverage_item list
(** Calculate coverage for a single file.

    Loads the sibling .tart file (if present) and c-core signatures, then checks
    each definition against the combined environment. *)

val analyze_files :
  search_path:Sig.Search_path.t -> string list -> coverage_result
(** Calculate coverage for multiple files. *)

(** {1 Summary Statistics} *)

val summarize : coverage_result -> coverage_summary
(** Compute summary statistics from coverage results. *)

val coverage_percentage : coverage_summary -> float
(** Calculate coverage percentage (0.0 to 100.0). *)

(** {1 Filtering} *)

val uncovered_public : coverage_result -> coverage_item list
(** Get all uncovered public definitions. *)

val private_definitions : coverage_result -> coverage_item list
(** Get all private definitions. *)

val covered_public : coverage_result -> coverage_item list
(** Get all covered public definitions. *)
