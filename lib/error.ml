(** Unified error type system for composable, machine-readable errors. *)

module Loc = Syntax.Location
module Diagnostic = Typing.Diagnostic

type t =
  | Type of Diagnostic.t
  | Parse of { message : string; span : Loc.span }
  | Eval of { message : string; span : Loc.span }
  | Io of { path : string; message : string }
  | Cli of { message : string; hint : string option }

let is_fatal = function
  | Type _ -> false (* Recoverable: collect all type errors *)
  | Parse _ -> false (* Recoverable: continue parsing next form *)
  | Eval _ -> false (* Recoverable: report and continue *)
  | Io _ -> true (* Fatal: cannot proceed without file *)
  | Cli _ -> true (* Fatal: invalid invocation *)

let location = function
  | Type d -> Some (Diagnostic.span d)
  | Parse { span; _ } -> Some span
  | Eval { span; _ } -> Some span
  | Io _ -> None
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

(** Format a source span for display *)
let format_span (span : Loc.span) : string = Diagnostic.format_span span

(** Format an error as a human-readable string. *)
let to_string = function
  | Type d -> Diagnostic.to_string d
  | Parse { message; span } ->
      Printf.sprintf "error: parse error: %s\n  --> %s" message
        (format_span span)
  | Eval { message; span } ->
      Printf.sprintf "error: evaluation error: %s\n  --> %s" message
        (format_span span)
  | Io { path; message } -> Printf.sprintf "error: %s: %s" path message
  | Cli { message; hint } -> (
      match hint with
      | Some h -> Printf.sprintf "error: %s\nhint: %s" message h
      | None -> Printf.sprintf "error: %s" message)

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

(** Report errors to stderr with summary count.

    Prints each error followed by a summary line showing the count. Example:
    {v
      error[E0308]: type mismatch
        --> init.el:42:10
        ...

      error[E0425]: variable `strng` is not defined
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

let report_json (errors : t list) : unit =
  let json_array = `List (List.map to_json errors) in
  print_endline (Yojson.Safe.pretty_to_string json_array)
