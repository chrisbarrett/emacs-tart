(** Tart CLI entry point

    Subcommand dispatch for type-checking, evaluation, macro expansion,
    and LSP server. *)

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
    let diagnostics = Tart.Diagnostic.of_unify_errors check_result.errors in
    List.iter
      (fun d -> prerr_endline (Tart.Diagnostic.to_string_compact d))
      diagnostics;

    parse_error_count + Tart.Diagnostic.count_errors diagnostics

(** Default command: type-check files *)
let cmd_check files =
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

(** Eval subcommand: evaluate expression and print result with type *)
let cmd_eval _expr =
  prerr_endline "tart eval: not yet implemented";
  exit 2

(** Expand subcommand: print macro-expanded source *)
let cmd_expand _file =
  prerr_endline "tart expand: not yet implemented";
  exit 2

(** REPL subcommand: interactive read-eval-print loop *)
let cmd_repl () =
  prerr_endline "tart repl: not yet implemented";
  exit 2

(** LSP subcommand: start language server *)
let cmd_lsp _port =
  prerr_endline "tart lsp: not yet implemented";
  exit 2

(** Print version and exit *)
let print_version () =
  print_endline ("tart " ^ Tart.version);
  exit 0

(** Print help and exit *)
let print_help () =
  print_endline
    {|tart - A type checker for Emacs Lisp

Usage:
  tart [OPTIONS] FILE [FILES...]   Type-check files (default command)
  tart eval <EXPR>                 Evaluate expression, print value and type
  tart expand FILE                 Print fully macro-expanded source
  tart repl                        Interactive REPL
  tart lsp [OPTIONS]               Start LSP server

Options:
  --version    Print version and exit
  --help, -h   Print this help message

Eval options:
  <EXPR>       S-expression to evaluate

Expand options:
  --load FILE  Load macros from FILE before expanding

LSP options:
  --port PORT  Listen on TCP port instead of stdio

Exit codes:
  0  Success
  1  Error in input (type error, parse error, eval error)
  2  Usage error (bad arguments)|};
  exit 0

(** Subcommand dispatch *)
let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [] ->
      prerr_endline "tart: no input files. Use --help for usage.";
      exit 2
  | [ "--version" ] | [ "-v" ] -> print_version ()
  | [ "--help" ] | [ "-h" ] -> print_help ()
  (* Subcommands *)
  | "eval" :: rest -> (
      match rest with
      | [] ->
          prerr_endline "tart eval: missing expression";
          exit 2
      | [ "--help" ] | [ "-h" ] ->
          print_endline "Usage: tart eval <EXPR>";
          print_endline "";
          print_endline "Evaluate an Elisp expression and print the result.";
          exit 0
      | [ expr ] -> cmd_eval expr
      | _ ->
          prerr_endline "tart eval: expected single expression";
          exit 2)
  | "expand" :: rest -> (
      match rest with
      | [] ->
          prerr_endline "tart expand: missing file";
          exit 2
      | [ "--help" ] | [ "-h" ] ->
          print_endline "Usage: tart expand [--load FILE] FILE";
          print_endline "";
          print_endline "Macro-expand FILE and print the result.";
          print_endline "";
          print_endline "Options:";
          print_endline "  --load FILE  Load macros from FILE before expanding";
          exit 0
      | [ file ] -> cmd_expand file
      | [ "--load"; _load_file; file ] -> cmd_expand file
      | _ ->
          prerr_endline "tart expand: invalid arguments";
          exit 2)
  | "repl" :: rest -> (
      match rest with
      | [] -> cmd_repl ()
      | [ "--help" ] | [ "-h" ] ->
          print_endline "Usage: tart repl";
          print_endline "";
          print_endline "Start an interactive REPL.";
          print_endline "";
          print_endline "REPL commands:";
          print_endline "  ,quit, ,q    Exit REPL";
          print_endline "  ,type EXPR   Show type without evaluating";
          print_endline "  ,expand EXPR Show macro expansion";
          print_endline "  ,env         List current bindings";
          print_endline "  ,help        Show available commands";
          exit 0
      | _ ->
          prerr_endline "tart repl: unexpected arguments";
          exit 2)
  | "lsp" :: rest ->
      let port = ref None in
      let show_help = ref false in
      let rec parse_lsp_args = function
        | [] -> ()
        | "--help" :: _ | "-h" :: _ -> show_help := true
        | "--port" :: p :: rest ->
            port := Some (int_of_string p);
            parse_lsp_args rest
        | arg :: _ ->
            prerr_endline ("tart lsp: unknown option: " ^ arg);
            exit 2
      in
      parse_lsp_args rest;
      if !show_help then (
        print_endline "Usage: tart lsp [--port PORT]";
        print_endline "";
        print_endline "Start the LSP server.";
        print_endline "";
        print_endline "Options:";
        print_endline "  --port PORT  Listen on TCP port instead of stdio";
        exit 0)
      else cmd_lsp !port
  (* Default: type-check files *)
  | files ->
      (* Filter out --version and --help that might appear with files *)
      let files, opts =
        List.partition (fun s -> not (String.starts_with ~prefix:"-" s)) files
      in
      if List.mem "--help" opts || List.mem "-h" opts then print_help ()
      else if List.mem "--version" opts || List.mem "-v" opts then
        print_version ()
      else if opts <> [] then (
        prerr_endline ("tart: unknown option: " ^ List.hd opts);
        exit 2)
      else cmd_check files
