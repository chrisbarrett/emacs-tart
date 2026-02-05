(** Fixture-based acceptance tests for the type checker.

    This module implements a test harness that runs `tart check` against `.el`
    fixture files and verifies the output against `.expected` sidecar files.

    See Spec 25 for full requirements. *)

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

(** Parse test directives from fixture file comments.

    Supported directives:
    - `test: emacs-version X.Y` - Use specific Emacs version for typings *)
let parse_fixture_directives (el_path : string) : fixture_directives =
  let emacs_version = ref None in
  (try
     let ic = In_channel.open_text el_path in
     (try
        while true do
          let line = input_line ic in
          let line = String.trim line in
          (* Look for ;; test: emacs-version X.Y *)
          if String.length line > 0 && line.[0] = ';' then
            try
              let re =
                Str.regexp {|;+[ \t]*test:[ \t]*emacs-version[ \t]+\([0-9.]+\)|}
              in
              if Str.string_match re line 0 then
                emacs_version := Some (Str.matched_group 1 line)
            with _ -> ()
        done
      with End_of_file -> ());
     In_channel.close ic
   with _ -> ());
  { emacs_version = !emacs_version }

(** {1 Expected File Parser} *)

(** Parse the first line to determine expected outcome. *)
let parse_outcome (line : string) : expectation option =
  let line = String.trim line in
  if String.uppercase_ascii line = "PASS" then Some Pass
  else if String.uppercase_ascii line = "FAIL" then Some Fail
  else None

(** Parse an `.expected` file.

    Format:
    - Line 1: PASS or FAIL
    - Remaining lines: expected diagnostic messages (substring match)

    Returns None if the file format is invalid. *)
let parse_expected_file (content : string) : expected_file option =
  let lines = String.split_on_char '\n' content in
  match lines with
  | [] -> None
  | first :: rest -> (
      match parse_outcome first with
      | None -> None
      | Some outcome ->
          (* Filter out empty lines from diagnostics *)
          let diagnostics =
            rest |> List.map String.trim
            |> List.filter (fun s -> String.length s > 0)
          in
          Some { outcome; diagnostics })

(** Read and parse an `.expected` file from disk. *)
let read_expected_file (path : string) : expected_file option =
  if not (Sys.file_exists path) then None
  else
    try
      let ic = In_channel.open_text path in
      let content = In_channel.input_all ic in
      In_channel.close ic;
      parse_expected_file content
    with _ -> None

(** {1 Fixture Discovery} *)

(** Find the `.expected` file for an `.el` fixture. *)
let expected_path_of_el (el_path : string) : string =
  if Filename.check_suffix el_path ".el" then
    Filename.chop_suffix el_path ".el" ^ ".expected"
  else el_path ^ ".expected"

(** Discover all `.el` fixtures in a directory (recursively). *)
let discover_fixtures (dir : string) : string list =
  let rec walk acc path =
    if Sys.is_directory path then
      let entries = Sys.readdir path |> Array.to_list in
      List.fold_left
        (fun acc entry -> walk acc (Filename.concat path entry))
        acc entries
    else if Filename.check_suffix path ".el" then
      (* Only include if there's a corresponding .expected file *)
      let expected_path = expected_path_of_el path in
      if Sys.file_exists expected_path then path :: acc else acc
    else acc
  in
  walk [] dir |> List.sort String.compare

(** {1 Running Tart} *)

(** Run `tart` on a file and capture output.

    Uses the default check mode (no subcommand needed). If directives specify an
    Emacs version, passes --emacs-version flag. *)
let run_tart_check ~(tart_bin : string)
    ?(directives : fixture_directives option) (el_path : string) : tart_output =
  (* Create temporary files for stdout/stderr *)
  let stdout_file = Filename.temp_file "tart_stdout" ".txt" in
  let stderr_file = Filename.temp_file "tart_stderr" ".txt" in
  (* Build command with optional version flag *)
  let version_flag =
    match directives with
    | Some { emacs_version = Some v } ->
        Printf.sprintf "--emacs-version %s " (Filename.quote v)
    | _ -> ""
  in
  (* tart defaults to type-check mode, no need for 'check' subcommand *)
  let cmd =
    Printf.sprintf "%s %s%s > %s 2> %s" (Filename.quote tart_bin) version_flag
      (Filename.quote el_path)
      (Filename.quote stdout_file)
      (Filename.quote stderr_file)
  in
  let exit_code = Sys.command cmd in
  let read_file path =
    try
      let ic = In_channel.open_text path in
      let content = In_channel.input_all ic in
      In_channel.close ic;
      content
    with _ -> ""
  in
  let stdout = read_file stdout_file in
  let stderr = read_file stderr_file in
  (* Clean up temp files *)
  (try Sys.remove stdout_file with _ -> ());
  (try Sys.remove stderr_file with _ -> ());
  { exit_code; stdout; stderr }

(** {1 Verification} *)

(** Check if all expected diagnostics appear in the output. *)
let check_diagnostics ~(expected : string list) ~(actual : string) : bool =
  List.for_all
    (fun expected_substr ->
      (* Substring match in stdout or stderr *)
      let contains s substr =
        try
          let _ = Str.search_forward (Str.regexp_string substr) s 0 in
          true
        with Not_found -> false
      in
      contains actual expected_substr)
    expected

(** Generate a diff message explaining what went wrong. *)
let generate_diff ~(expected : expected_file) ~(actual : tart_output) : string =
  let buf = Buffer.create 256 in
  let expected_outcome =
    match expected.outcome with Pass -> "PASS" | Fail -> "FAIL"
  in
  let actual_outcome = if actual.exit_code = 0 then "PASS" else "FAIL" in
  if expected_outcome <> actual_outcome then
    Buffer.add_string buf
      (Printf.sprintf "Expected outcome: %s, got: %s\n" expected_outcome
         actual_outcome);
  (* Check for missing diagnostics *)
  let combined_output = actual.stdout ^ "\n" ^ actual.stderr in
  let missing =
    List.filter
      (fun d ->
        try
          let _ = Str.search_forward (Str.regexp_string d) combined_output 0 in
          false
        with Not_found -> true)
      expected.diagnostics
  in
  if missing <> [] then (
    Buffer.add_string buf "Missing expected diagnostics:\n";
    List.iter
      (fun d -> Buffer.add_string buf (Printf.sprintf "  - %s\n" d))
      missing);
  if actual.stderr <> "" then (
    Buffer.add_string buf "\nActual stderr:\n";
    Buffer.add_string buf actual.stderr);
  if actual.stdout <> "" then (
    Buffer.add_string buf "\nActual stdout:\n";
    Buffer.add_string buf actual.stdout);
  Buffer.contents buf

(** Verify a fixture result against expectations. *)
let verify_fixture ~(expected : expected_file) ~(actual : tart_output) : bool =
  let outcome_match =
    match expected.outcome with
    | Pass -> actual.exit_code = 0
    | Fail -> actual.exit_code <> 0
  in
  let diagnostics_match =
    check_diagnostics ~expected:expected.diagnostics
      ~actual:(actual.stdout ^ "\n" ^ actual.stderr)
  in
  outcome_match && diagnostics_match

(** {1 Main API} *)

(** Check a single fixture file.

    @param tart_bin Path to tart executable
    @param path Path to the `.el` fixture file
    @return Fixture result with pass/fail status and diff *)
let check_fixture ~(tart_bin : string) ~(path : string) : fixture_result =
  let expected_path = expected_path_of_el path in
  match read_expected_file expected_path with
  | None ->
      (* No .expected file or invalid format *)
      {
        path;
        expected = { outcome = Pass; diagnostics = [] };
        actual = { exit_code = -1; stdout = ""; stderr = "" };
        passed = false;
        diff =
          Some (Printf.sprintf "No valid .expected file at %s" expected_path);
      }
  | Some expected ->
      let directives = parse_fixture_directives path in
      let actual = run_tart_check ~tart_bin ~directives path in
      let passed = verify_fixture ~expected ~actual in
      let diff =
        if passed then None else Some (generate_diff ~expected ~actual)
      in
      { path; expected; actual; passed; diff }

(** Run all fixtures in a directory.

    @param tart_bin Path to tart executable
    @param dir Directory containing fixtures
    @return Summary with results for all fixtures *)
let run_all ~(tart_bin : string) ~(dir : string) : summary =
  let fixtures = discover_fixtures dir in
  let fixture_results =
    List.map (fun path -> check_fixture ~tart_bin ~path) fixtures
  in
  let passed_count =
    List.length (List.filter (fun r -> r.passed) fixture_results)
  in
  let failed_count = List.length fixture_results - passed_count in
  {
    total = List.length fixture_results;
    num_passed = passed_count;
    num_failed = failed_count;
    results = fixture_results;
  }

(** Run fixtures in parallel for speed.

    @param tart_bin Path to tart executable
    @param dir Directory containing fixtures
    @param jobs Number of parallel jobs (default: number of CPUs)
    @return Summary with results for all fixtures *)
let run_all_parallel ~(tart_bin : string) ~(dir : string) ?(jobs = 4) () :
    summary =
  let fixtures = discover_fixtures dir in
  (* Simple parallel execution using Domainslib would be ideal, but for now
     we'll use a simpler approach with batching *)
  let fixture_results =
    if jobs <= 1 then
      List.map (fun path -> check_fixture ~tart_bin ~path) fixtures
    else
      (* For parallel execution, we'd ideally use Domain.spawn, but that
         requires OCaml 5.0+. For now, fall back to sequential. *)
      List.map (fun path -> check_fixture ~tart_bin ~path) fixtures
  in
  let passed_count =
    List.length (List.filter (fun r -> r.passed) fixture_results)
  in
  let failed_count = List.length fixture_results - passed_count in
  {
    total = List.length fixture_results;
    num_passed = passed_count;
    num_failed = failed_count;
    results = fixture_results;
  }

(** {1 Output Formatting} *)

(** Format a fixture result for display. *)
let format_result (result : fixture_result) : string =
  let status = if result.passed then "PASS" else "FAIL" in
  let base = Printf.sprintf "[%s] %s" status result.path in
  match result.diff with
  | None -> base
  | Some diff -> Printf.sprintf "%s\n%s" base diff

(** Format a summary for display. *)
let format_summary (summary : summary) : string =
  let buf = Buffer.create 1024 in
  (* Show failed fixtures first *)
  let failed = List.filter (fun r -> not r.passed) summary.results in
  List.iter
    (fun r ->
      Buffer.add_string buf (format_result r);
      Buffer.add_char buf '\n')
    failed;
  (* Summary line *)
  Buffer.add_string buf
    (Printf.sprintf "\n%d/%d fixtures passed (%d failed)\n" summary.num_passed
       summary.total summary.num_failed);
  Buffer.contents buf
