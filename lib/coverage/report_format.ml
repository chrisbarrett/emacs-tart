(** Report formatting for coverage output.

    This module generates human-readable and JSON output formats for coverage
    reports.

    See Spec 28, R11-R13, R15, R18 for requirements. *)

open Coverage_report

(** {1 Types} *)

type format = Human | Json  (** Output format selection. *)

type output_config = {
  format : format;
  verbose : bool;  (** Show additional details *)
}
(** Configuration for report output. *)

let default_output_config : output_config = { format = Human; verbose = false }

(** {1 Human-Readable Format} *)

(** Format a source location for display. *)
let format_location (item : coverage_item) : string =
  let span = item.definition.span in
  let file = Filename.basename item.source_file in
  Printf.sprintf "%s:%d" file span.start_pos.line

(** Format an identifier with its location. *)
let format_item (item : coverage_item) : string =
  Printf.sprintf "  %-24s (%s)" item.definition.name (format_location item)

(** Generate the human-readable summary section. *)
let format_summary ~(files : int) ~(summary : coverage_summary) : string =
  let percentage = coverage_percentage summary in
  let uncovered = summary.total_public - summary.covered_public in
  String.concat "\n"
    [
      "Coverage Report";
      "===============";
      "";
      Printf.sprintf "Files scanned: %d" files;
      Printf.sprintf "Total definitions: %d"
        (summary.total_public + summary.total_private);
      Printf.sprintf "  Public: %d" summary.total_public;
      Printf.sprintf "  Private: %d" summary.total_private;
      "";
      Printf.sprintf "Public coverage: %d/%d (%.1f%%)" summary.covered_public
        summary.total_public percentage;
      (if uncovered > 0 then Printf.sprintf "Uncovered: %d" uncovered else "");
    ]

(** Generate the uncovered identifiers section. *)
let format_uncovered (items : coverage_item list) : string =
  if items = [] then ""
  else
    let header =
      Printf.sprintf "\nUncovered public identifiers (%d):" (List.length items)
    in
    let lines = List.map format_item items in
    String.concat "\n" (header :: lines)

(** Generate the private identifiers section. *)
let format_private (items : coverage_item list) : string =
  if items = [] then ""
  else
    let header =
      Printf.sprintf "\nPrivate identifiers excluded (%d):" (List.length items)
    in
    let lines = List.map format_item items in
    String.concat "\n" (header :: lines)

(** Generate the covered identifiers section (verbose only). *)
let format_covered (items : coverage_item list) : string =
  if items = [] then ""
  else
    let header =
      Printf.sprintf "\nCovered public identifiers (%d):" (List.length items)
    in
    let lines = List.map format_item items in
    String.concat "\n" (header :: lines)

(** Generate full human-readable report. *)
let format_human ~(config : output_config) (result : coverage_result) : string =
  let summary = summarize result in
  let uncovered_items = uncovered_public result in
  let private_items = private_definitions result in
  let parts =
    [
      format_summary ~files:result.files_scanned ~summary;
      format_uncovered uncovered_items;
      format_private private_items;
    ]
  in
  let parts =
    if config.verbose then parts @ [ format_covered (covered_public result) ]
    else parts
  in
  String.concat "\n" (List.filter (fun s -> s <> "") parts)

(** {1 JSON Format} *)

(** Escape a string for JSON. *)
let json_escape (s : string) : string =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | _ -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(** Format a list of strings as JSON array. *)
let json_string_array (items : string list) : string =
  let quoted = List.map (fun s -> "\"" ^ json_escape s ^ "\"") items in
  "[" ^ String.concat ", " quoted ^ "]"

(** Format item as JSON object for detailed output. *)
let json_item (item : coverage_item) : string =
  Printf.sprintf
    "{\"name\": \"%s\", \"file\": \"%s\", \"line\": %d, \"kind\": \"%s\"}"
    (json_escape item.definition.name)
    (json_escape item.source_file)
    item.definition.span.start_pos.line
    (Definition_extractor.kind_to_string item.definition.kind)

(** Generate JSON report. *)
let format_json ~(config : output_config) (result : coverage_result) : string =
  let summary = summarize result in
  let uncovered_items = uncovered_public result in
  let private_items = private_definitions result in
  let uncovered_names = List.map (fun i -> i.definition.name) uncovered_items in
  let private_names = List.map (fun i -> i.definition.name) private_items in
  let base =
    Printf.sprintf
      {|{
  "files_scanned": %d,
  "public": {
    "total": %d,
    "covered": %d,
    "uncovered": %s
  },
  "private": {
    "total": %d,
    "identifiers": %s
  }|}
      result.files_scanned summary.total_public summary.covered_public
      (json_string_array uncovered_names)
      summary.total_private
      (json_string_array private_names)
  in
  if config.verbose then
    let covered_items = covered_public result in
    let uncovered_detailed =
      "[" ^ String.concat ", " (List.map json_item uncovered_items) ^ "]"
    in
    let covered_detailed =
      "[" ^ String.concat ", " (List.map json_item covered_items) ^ "]"
    in
    base
    ^ Printf.sprintf
        {|,
  "details": {
    "uncovered": %s,
    "covered": %s
  }
}|}
        uncovered_detailed covered_detailed
  else base ^ "\n}"

(** {1 Report Generation} *)

(** Generate report in the configured format. *)
let format_report ~(config : output_config) (result : coverage_result) : string
    =
  match config.format with
  | Human -> format_human ~config result
  | Json -> format_json ~config result
