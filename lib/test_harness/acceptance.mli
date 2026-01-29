(** Fixture-based acceptance tests for the type checker.

    This module implements a test harness that runs `tart check` against `.el`
    fixture files and verifies the output against `.expected` sidecar files.

    @see Spec 25 for full requirements. *)

(** {1 Types} *)

type expectation =
  | Pass  (** Expected to type-check without errors *)
  | Fail  (** Expected to fail type-checking *)

type expected_file = {
  outcome : expectation;
  diagnostics : string list;  (** Expected diagnostic lines (substring match) *)
}
(** Parsed contents of a `.expected` file. *)

type fixture_directives = {
  emacs_version : string option;
      (** Override Emacs version for typings lookup *)
}
(** Directives parsed from fixture file comments. *)

type tart_output = { exit_code : int; stdout : string; stderr : string }
(** Result of running `tart check`. *)

type fixture_result = {
  path : string;
  expected : expected_file;
  actual : tart_output;
  passed : bool;
  diff : string option;  (** If failed, shows what went wrong *)
}
(** Result of checking a single fixture. *)

type summary = {
  total : int;
  num_passed : int;
  num_failed : int;
  results : fixture_result list;
}
(** Summary of running all fixtures. *)

(** {1 Directive Parser} *)

val parse_fixture_directives : string -> fixture_directives
(** Parse test directives from fixture file comments.

    Supported directives:
    - `test: emacs-version X.Y` - Use specific Emacs version for typings *)

(** {1 Expected File Parser} *)

val parse_expected_file : string -> expected_file option
(** Parse an `.expected` file content.

    Format:
    - Line 1: PASS or FAIL
    - Remaining lines: expected diagnostic messages (substring match)

    Returns None if the file format is invalid. *)

val read_expected_file : string -> expected_file option
(** Read and parse an `.expected` file from disk. *)

(** {1 Fixture Discovery} *)

val expected_path_of_el : string -> string
(** Find the `.expected` file path for an `.el` fixture. *)

val discover_fixtures : string -> string list
(** Discover all `.el` fixtures in a directory (recursively).

    Only returns fixtures that have a corresponding `.expected` file. *)

(** {1 Running Tart} *)

val run_tart_check :
  tart_bin:string -> ?directives:fixture_directives -> string -> tart_output
(** Run `tart check` on a file and capture output.

    @param tart_bin Path to tart executable
    @param directives Optional fixture directives (e.g., emacs-version)
    @param path Path to the `.el` file to check *)

(** {1 Main API} *)

val check_fixture : tart_bin:string -> path:string -> fixture_result
(** Check a single fixture file.

    @param tart_bin Path to tart executable
    @param path Path to the `.el` fixture file
    @return Fixture result with pass/fail status and diff *)

val run_all : tart_bin:string -> dir:string -> summary
(** Run all fixtures in a directory.

    @param tart_bin Path to tart executable
    @param dir Directory containing fixtures
    @return Summary with results for all fixtures *)

val run_all_parallel :
  tart_bin:string -> dir:string -> ?jobs:int -> unit -> summary
(** Run fixtures in parallel for speed.

    @param tart_bin Path to tart executable
    @param dir Directory containing fixtures
    @param jobs Number of parallel jobs (default: 4)
    @return Summary with results for all fixtures *)

(** {1 Output Formatting} *)

val format_result : fixture_result -> string
(** Format a fixture result for display. *)

val format_summary : summary -> string
(** Format a summary for display. *)
