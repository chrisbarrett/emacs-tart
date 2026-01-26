(** Tart CLI entry point

    Subcommand dispatch for type-checking, evaluation, macro expansion,
    and LSP server. *)

(** Format a parse error for display in compiler-style format *)
let format_parse_error (err : Tart.Read.parse_error) : string =
  let pos = err.span.start_pos in
  Printf.sprintf "%s:%d:%d: error: %s" pos.file pos.line (pos.col + 1)
    err.message

(** Type-check a single file with a given environment.

    Returns the updated environment (with definitions from this file)
    and the error count for this file. *)
let check_file env filename : Tart.Type_env.t * int =
  (* Parse the file *)
  let parse_result = Tart.Read.parse_file filename in

  (* Print parse errors *)
  let parse_error_count = List.length parse_result.errors in
  List.iter
    (fun err -> prerr_endline (format_parse_error err))
    parse_result.errors;

  (* Type-check if we have any successfully parsed forms *)
  if parse_result.sexps = [] then (env, parse_error_count)
  else
    let check_result = Tart.Check.check_program ~env parse_result.sexps in

    (* Convert unify errors to diagnostics and print them *)
    let diagnostics = Tart.Diagnostic.of_unify_errors check_result.errors in
    List.iter
      (fun d -> prerr_endline (Tart.Diagnostic.to_string_compact d))
      diagnostics;

    let error_count = parse_error_count + Tart.Diagnostic.count_errors diagnostics in
    (check_result.env, error_count)

(** Default command: type-check files.

    Files are processed in order; definitions from earlier files
    are visible to later files. *)
let cmd_check files =
  if files = [] then (
    prerr_endline "tart: no input files. Use --help for usage.";
    exit 2)
  else
    (* Check each file, accumulating the environment *)
    let initial_env = Tart.Check.default_env () in
    let _, total_errors =
      List.fold_left
        (fun (env, acc_errors) file ->
          let env', file_errors = check_file env file in
          (env', acc_errors + file_errors))
        (initial_env, 0) files
    in
    (* Exit code: 0 if no errors, 1 if errors *)
    if total_errors > 0 then exit 1

(** Eval subcommand: evaluate expression and print result with type.

    Parses the expression, evaluates it in the interpreter,
    infers its type, and prints "<value> :: <type>".

    Exits with code 1 on parse errors, eval errors, or type errors. *)
let cmd_eval expr =
  (* Parse the expression *)
  let parse_result = Tart.Read.parse_string expr in

  (* Check for parse errors *)
  if parse_result.errors <> [] then (
    List.iter
      (fun err -> prerr_endline (format_parse_error err))
      parse_result.errors;
    exit 1);

  (* Check we got exactly one expression *)
  match parse_result.sexps with
  | [] ->
      prerr_endline "tart eval: empty expression";
      exit 1
  | [ sexp ] -> (
      (* Create interpreter state *)
      let global = Tart.Eval.make_interpreter () in

      (* Evaluate the expression *)
      match Tart.Eval.eval_toplevel global sexp with
      | Error eval_err ->
          let pos = eval_err.span.start_pos in
          prerr_endline
            (Printf.sprintf "%s:%d:%d: error: %s" pos.file pos.line
               (pos.col + 1) eval_err.message);
          exit 1
      | Ok value ->
          (* Infer the type of the expression and solve constraints *)
          let ty, errors = Tart.Check.check_expr sexp in

          (* Check for type errors *)
          if errors <> [] then (
            let diagnostics = Tart.Diagnostic.of_unify_errors errors in
            List.iter
              (fun d -> prerr_endline (Tart.Diagnostic.to_string_compact d))
              diagnostics;
            exit 1);

          (* Print result: value :: type *)
          let value_str = Tart.Value.to_string value in
          let type_str = Tart.Types.to_string ty in
          Printf.printf "%s :: %s\n" value_str type_str)
  | _ ->
      prerr_endline "tart eval: expected single expression";
      exit 1

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
