(** Report formatting for coverage output.

    This module generates human-readable and JSON output formats
    for coverage reports.

    @see Spec 28, R11-R13, R15, R18 for requirements. *)

(** {1 Types} *)

type format = Human | Json  (** Output format selection. *)

type output_config = {
  format : format;
  verbose : bool;  (** Show additional details *)
}
(** Configuration for report output. *)

val default_output_config : output_config
(** Default output configuration: human format, non-verbose. *)

(** {1 Human-Readable Format} *)

val format_location : Coverage_report.coverage_item -> string
(** Format a source location for display (file:line). *)

val format_item : Coverage_report.coverage_item -> string
(** Format an identifier with its location. *)

val format_summary :
  files:int -> summary:Coverage_report.coverage_summary -> string
(** Generate the human-readable summary section. *)

val format_uncovered : Coverage_report.coverage_item list -> string
(** Generate the uncovered identifiers section. *)

val format_private : Coverage_report.coverage_item list -> string
(** Generate the private identifiers section. *)

val format_covered : Coverage_report.coverage_item list -> string
(** Generate the covered identifiers section (verbose only). *)

val format_human :
  config:output_config -> Coverage_report.coverage_result -> string
(** Generate full human-readable report. *)

(** {1 JSON Format} *)

val json_escape : string -> string
(** Escape a string for JSON. *)

val json_string_array : string list -> string
(** Format a list of strings as JSON array. *)

val format_json :
  config:output_config -> Coverage_report.coverage_result -> string
(** Generate JSON report. *)

(** {1 Report Generation} *)

val format_report :
  config:output_config -> Coverage_report.coverage_result -> string
(** Generate report in the configured format. *)
