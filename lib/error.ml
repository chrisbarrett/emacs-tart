(** Unified error type system for composable, machine-readable errors. *)

module Loc = Syntax.Location
module Diagnostic = Typing.Diagnostic

type t =
  | Type of Diagnostic.t
  | Parse of { message : string; span : Loc.span }
  | Eval of { message : string; span : Loc.span }
  | Io of { path : string; message : string }
  | File of Errors.File_error.t  (** Structured file I/O error *)
  | Cli of { message : string; hint : string option }

let is_fatal = function
  | Type _ -> false (* Recoverable: collect all type errors *)
  | Parse _ -> false (* Recoverable: continue parsing next form *)
  | Eval _ -> false (* Recoverable: report and continue *)
  | Io _ -> true (* Fatal: cannot proceed without file *)
  | File _ -> true (* Fatal: cannot proceed without file *)
  | Cli _ -> true (* Fatal: invalid invocation *)

let is_error = function
  | Type d -> Diagnostic.is_error d
  | Parse _ | Eval _ | Io _ | File _ | Cli _ -> true

let location = function
  | Type d -> Some (Diagnostic.span d)
  | Parse { span; _ } -> Some span
  | Eval { span; _ } -> Some span
  | Io _ -> None
  | File _ -> None (* File errors don't have code spans *)
  | Cli _ -> None

let parse_error ~message ~span = Parse { message; span }
let eval_error ~message ~span = Eval { message; span }

let io_error ~path ~exn =
  let message =
    match exn with
    | Sys_error msg -> msg
    | Unix.Unix_error (err, _, _) -> Unix.error_message err
    | exn -> Printexc.to_string exn
  in
  Io { path; message }

let cli_error ~message ?hint () = Cli { message; hint }
let of_diagnostic d = Type d
let of_diagnostics ds = List.map of_diagnostic ds
let of_file_error (err : Errors.File_error.t) = File err

(** Format a source span for display *)
let format_span (span : Loc.span) : string = Diagnostic.format_span span

(** Format an error as a human-readable string (compact format). *)
let to_string = function
  | Type d -> Diagnostic.to_string d
  | Parse { message; span } ->
      Printf.sprintf "error: parse error: %s\n  --> %s" message
        (format_span span)
  | Eval { message; span } ->
      Printf.sprintf "error: evaluation error: %s\n  --> %s" message
        (format_span span)
  | Io { path; message } -> Printf.sprintf "error: %s: %s" path message
  | File file_err -> Errors.File_error.to_string file_err
  | Cli { message; hint } -> (
      match hint with
      | Some h -> Printf.sprintf "error: %s\nhint: %s" message h
      | None -> Printf.sprintf "error: %s" message)

(** Format an error in Elm-style human-readable format with source excerpts.

    Per Spec 45: Shows Elm-style headers, source excerpts with underlines,
    conversational prose, and colored output when TTY is detected. *)
let to_string_human = function
  | Type d -> Diagnostic.to_string_human d
  | Parse { message; span } ->
      Printf.sprintf "-- PARSE ERROR %s\n\n%s\n" (format_span span) message
  | Eval { message; span } ->
      Printf.sprintf "-- EVALUATION ERROR %s\n\n%s\n" (format_span span) message
  | Io { path; message } ->
      Printf.sprintf "-- FILE ERROR %s\n\n%s\n" path message
  | File file_err -> Errors.File_error.to_string file_err
  | Cli { message; hint } -> (
      match hint with
      | Some h -> Printf.sprintf "-- CLI ERROR\n\n%s\n\nHint: %s\n" message h
      | None -> Printf.sprintf "-- CLI ERROR\n\n%s\n" message)

(** Serialize a source location to JSON. *)
let location_to_json (span : Loc.span) : Yojson.Safe.t =
  `Assoc
    [
      ("file", `String span.start_pos.file);
      ("line", `Int span.start_pos.line);
      ("column", `Int (span.start_pos.col + 1));
      ("end_line", `Int span.end_pos.line);
      ("end_column", `Int (span.end_pos.col + 1));
    ]

(** Serialize an error to JSON. *)
let to_json = function
  | Type d -> Diagnostic.to_json d
  | Parse { message; span } ->
      `Assoc
        [
          ("kind", `String "parse");
          ("severity", `String "error");
          ("message", `String message);
          ("location", location_to_json span);
        ]
  | Eval { message; span } ->
      `Assoc
        [
          ("kind", `String "eval");
          ("severity", `String "error");
          ("message", `String message);
          ("location", location_to_json span);
        ]
  | Io { path; message } ->
      `Assoc
        [
          ("kind", `String "io");
          ("severity", `String "error");
          ("path", `String path);
          ("message", `String message);
        ]
  | File file_err -> Errors.File_error.to_json file_err
  | Cli { message; hint } ->
      let base =
        [
          ("kind", `String "cli");
          ("severity", `String "error");
          ("message", `String message);
        ]
      in
      let with_hint =
        match hint with Some h -> ("hint", `String h) :: base | None -> base
      in
      `Assoc with_hint

type error = t
(** Alias for the error type to use within submodules. *)

(** Error accumulator for collecting multiple errors. *)
module Acc = struct
  type 'a t = { errors : error list; value : 'a option }

  let empty = { errors = []; value = None }
  let add err acc = { acc with errors = err :: acc.errors }
  let add_list errs acc = { acc with errors = List.rev_append errs acc.errors }
  let to_list acc = List.rev acc.errors
  let has_errors acc = acc.errors <> []
end

(** Report errors to stderr in compact format with summary count.

    Prints each error followed by a summary line showing the count. Example:
    {v
      error[E0001]: type mismatch
        --> init.el:42:10
        ...

      error[E0100]: variable `strng` is not defined
        --> init.el:50:5
        ...

      Found 2 errors
    v} *)
let report (errors : t list) : unit =
  let count = List.length errors in
  List.iter (fun err -> prerr_endline (to_string err)) errors;
  if count > 0 then
    let plural = if count = 1 then "error" else "errors" in
    prerr_endline (Printf.sprintf "\nFound %d %s" count plural)

(** Report errors to stderr in Elm-style human format with source excerpts.

    Per Spec 45: Shows Elm-style headers, source excerpts with underlines,
    conversational prose, and colored output when TTY is detected.

    Prints each error followed by a summary line showing the count. Example:
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

      Found 1 error
    v} *)
let report_human (errors : t list) : unit =
  let error_count = List.length (List.filter is_error errors) in
  let warning_count = List.length errors - error_count in
  List.iter (fun err -> prerr_endline (to_string_human err)) errors;
  if error_count > 0 || warning_count > 0 then
    let parts = [] in
    let parts =
      if error_count > 0 then
        let plural = if error_count = 1 then "error" else "errors" in
        Printf.sprintf "%d %s" error_count plural :: parts
      else parts
    in
    let parts =
      if warning_count > 0 then
        let plural = if warning_count = 1 then "warning" else "warnings" in
        Printf.sprintf "%d %s" warning_count plural :: parts
      else parts
    in
    prerr_endline
      (Printf.sprintf "\nFound %s" (String.concat ", " (List.rev parts)))

let report_json (errors : t list) : unit =
  let json_array = `List (List.map to_json errors) in
  print_endline (Yojson.Safe.pretty_to_string json_array)
