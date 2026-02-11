(** Per-file coverage table renderer.

    Renders per-file coverage data as aligned text tables or JSON. Shared by
    [emacs-coverage] and [coverage] subcommands.

    See Spec 96 for requirements. *)

(** {1 Types} *)

type file_row = {
  filename : string;  (** Source file basename *)
  private_count : int;  (** Count of private (["--"]) identifiers *)
  public_covered : int;  (** Public identifiers with type signatures *)
  public_total : int;  (** Total public identifiers *)
  coverage_pct : float;  (** Coverage percentage (0.0–100.0) *)
  uncovered_names : string list;  (** Names of uncovered public identifiers *)
}
(** A single row in the coverage table. *)

type color_mode =
  | Auto  (** Color if stdout is a TTY *)
  | Always  (** Always emit color *)
  | Off  (** Never emit color *)

type output_format =
  | Human  (** Aligned text table *)
  | Json  (** JSON structure *)

type table_config = { color : color_mode; format : output_format }
(** Configuration for table rendering. *)

val default_config : table_config
(** Default configuration: [Auto] color, [Human] format. *)

(** {1 Row Construction} *)

val rows_of_c_result : Emacs_coverage.c_coverage_result -> file_row list
(** Build table rows from C layer coverage results, one row per source file. *)

val rows_of_elisp_result : Emacs_coverage.elisp_coverage_result -> file_row list
(** Build table rows from Elisp layer coverage results, one row per [.el] file.
*)

(** {1 Sorting} *)

val default_sort : file_row list -> file_row list
(** Sort rows: [.c] files first, then [.el] files; alphabetical within each
    group. *)

(** {1 Rendering} *)

val render_table :
  config:table_config -> emacs_version:string -> file_row list -> string
(** Render a coverage table in the configured format.

    [R1]: Columns are FILENAME, PRIVATE, PUBLIC, COVERAGE. [R3]: Color
    thresholds: green ≥ 95%, yellow ≥ 50%, red < 50%. [R4]: Color controlled by
    [config.color]. In JSON mode, [emacs_version] is included in the output. *)

val render_json : emacs_version:string -> file_row list -> string
(** Render coverage data as JSON.

    [R5]: Produces [{emacs_version, files, totals}] structure. *)
