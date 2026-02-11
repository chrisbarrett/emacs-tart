(** Per-file coverage table renderer.

    Renders per-file coverage data as aligned text tables or JSON. Shared by
    [emacs-coverage] and [coverage] subcommands.

    See Specs 96–97 for requirements. *)

(** {1 Types} *)

type uncovered_detail = {
  file : string;  (** Source file path *)
  line : int;  (** 1-based line number *)
  col : int;  (** 0-based column *)
  identifier : string;  (** Uncovered identifier name *)
}
(** Location detail for an uncovered identifier, used in drill-down output. *)

type file_row = {
  filename : string;  (** Source file basename *)
  private_count : int;  (** Count of private (["--"]) identifiers *)
  public_covered : int;  (** Public identifiers with type signatures *)
  public_total : int;  (** Total public identifiers *)
  coverage_pct : float;  (** Coverage percentage (0.0–100.0) *)
  uncovered_names : string list;  (** Names of uncovered public identifiers *)
  uncovered_details : uncovered_detail list;
      (** Location details for uncovered identifiers (drill-down). *)
}
(** A single row in the coverage table. *)

type color_mode =
  | Auto  (** Color if stdout is a TTY *)
  | Always  (** Always emit color *)
  | Off  (** Never emit color *)

type output_format =
  | Human  (** Aligned text table *)
  | Json  (** JSON structure *)

type sort_column =
  | Default  (** [.c] first, then [.el]; alphabetical within groups *)
  | Name  (** Strictly alphabetical by filename *)
  | Coverage  (** By coverage percentage *)
  | Public  (** By public covered count *)
  | Private  (** By private identifier count *)

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

val sort_rows :
  column:sort_column -> reverse:bool -> file_row list -> file_row list
(** Sort rows by the specified column.

    [Spec 97 R1–R2]: Sort by name, coverage, public, or private count. [reverse]
    reverses the sort order. *)

(** {1 Filtering} *)

val filter_rows :
  ?min_pct:float -> ?max_pct:float -> file_row list -> file_row list
(** Filter rows by coverage percentage range.

    [Spec 97 R3]: Keep rows with [coverage_pct >= min_pct] and
    [coverage_pct <= max_pct]. *)

val match_positional : string list -> file_row list -> file_row list
(** Filter rows matching positional arguments.

    [Spec 97 R4]: Each argument is interpreted as:
    - Glob if it contains [*] or [?]
    - Filename if it contains [.el] or [.c]
    - Feature name otherwise (matches any file with that basename)

    Multiple arguments combine as OR filters (any match includes the row). *)

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

val render_drilldown_human :
  config:table_config -> emacs_version:string -> file_row list -> string
(** Render filtered table with drill-down lines appended.

    [Spec 97 R5]: Table followed by blank line and [file:line:col: identifier]
    lines for each uncovered identifier, sorted by file then line number. *)

val render_drilldown_json : emacs_version:string -> file_row list -> string
(** Render JSON with [uncovered_details] array.

    [Spec 97 R6]: Includes per-identifier location details. *)
