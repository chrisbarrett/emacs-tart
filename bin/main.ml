(** Tart CLI entry point *)

(** Format a parse error for display in compiler-style format *)
let format_parse_error (err : Tart.Read.parse_error) : string =
  let pos = err.span.start_pos in
  Printf.sprintf "%s:%d:%d: error: %s" pos.file pos.line (pos.col + 1)
    err.message

(** Type-check a single file, returning error count *)
let check_file filename : int =
  (* Parse the file *)
  let parse_result = Tart.Read.parse_file filename in

  (* Print parse errors *)
  let parse_error_count = List.length parse_result.errors in
  List.iter
    (fun err -> prerr_endline (format_parse_error err))
    parse_result.errors;

  (* Type-check if we have any successfully parsed forms *)
  if parse_result.sexps = [] then parse_error_count
  else
    let check_result = Tart.Check.check_program parse_result.sexps in

    (* Convert unify errors to diagnostics and print them *)
    let diagnostics =
      Tart.Diagnostic.of_unify_errors check_result.errors
    in
    List.iter
      (fun d -> prerr_endline (Tart.Diagnostic.to_string_compact d))
      diagnostics;

    parse_error_count + Tart.Diagnostic.count_errors diagnostics

let () =
  let usage =
    "tart [OPTIONS] FILE [FILES...]\n\nA type checker for Emacs Lisp.\n\nOptions:"
  in
  let files = ref [] in
  let speclist =
    [
      ( "--version",
        Arg.Unit
          (fun () ->
            print_endline ("tart " ^ Tart.version);
            exit 0),
        " Print version and exit" );
    ]
  in
  Arg.parse speclist (fun f -> files := f :: !files) usage;

  (* Files come in reverse order from Arg.parse *)
  let files = List.rev !files in

  if files = [] then (
    prerr_endline "tart: no input files. Use --help for usage.";
    exit 2)
  else
    (* Check each file and count total errors *)
    let total_errors =
      List.fold_left (fun acc f -> acc + check_file f) 0 files
    in

    (* Exit code: 0 if no errors, 1 if errors *)
    if total_errors > 0 then exit 1
