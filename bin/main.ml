(** Tart CLI entry point

    Subcommand dispatch for type-checking, evaluation, macro expansion, and LSP
    server. *)

(** Format a parse error for display in compiler-style format *)
let format_parse_error (err : Tart.Read.parse_error) : string =
  let pos = err.span.start_pos in
  Printf.sprintf "%s:%d:%d: error: %s" pos.file pos.line (pos.col + 1)
    err.message

(** Type-check a single file with a given environment.

    Returns the updated environment (with definitions from this file) and the
    error count for this file. *)
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

    let error_count =
      parse_error_count + Tart.Diagnostic.count_errors diagnostics
    in
    (check_result.env, error_count)

(** Default command: type-check files.

    Files are processed in order; definitions from earlier files are visible to
    later files. *)
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

    Parses the expression, evaluates it in the interpreter, infers its type, and
    prints "<value> :: <type>".

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

(** Expand subcommand: print macro-expanded source.

    Parses the file, expands all macros, and pretty-prints the result.
    Optionally loads macros from other files via --load. *)
let cmd_expand ~load_files file =
  (* Create interpreter with builtins *)
  let global = Tart.Eval.make_interpreter () in

  (* Load macro definitions from --load files *)
  List.iter
    (fun load_file ->
      let parse_result = Tart.Read.parse_file load_file in
      if parse_result.errors <> [] then (
        List.iter
          (fun err -> prerr_endline (format_parse_error err))
          parse_result.errors;
        exit 1);
      Tart.Expand.load_macros global parse_result.sexps)
    load_files;

  (* Parse the main file *)
  let parse_result = Tart.Read.parse_file file in

  (* Check for parse errors *)
  if parse_result.errors <> [] then (
    List.iter
      (fun err -> prerr_endline (format_parse_error err))
      parse_result.errors;
    exit 1);

  (* Expand each top-level form and print *)
  List.iter
    (fun sexp ->
      match Tart.Expand.expand_all global sexp with
      | Tart.Expand.Expanded expanded ->
          print_endline (Tart.Sexp.to_string expanded)
      | Tart.Expand.Expansion_error { message; span } ->
          let pos = span.start_pos in
          prerr_endline
            (Printf.sprintf "%s:%d:%d: error: %s" pos.file pos.line
               (pos.col + 1) message);
          exit 1)
    parse_result.sexps

(** Parse and extract the argument from a REPL command like ",type (+ 1 2)" *)
let parse_repl_cmd_arg cmd input =
  let prefix = "," ^ cmd in
  let prefix_len = String.length prefix in
  if String.length input > prefix_len then
    let rest = String.sub input prefix_len (String.length input - prefix_len) in
    String.trim rest
  else ""

type repl_state = {
  interp : Tart.Env.global;
  mutable type_env : Tart.Type_env.t;
}
(** REPL state *)

(** Show type of expression without evaluating *)
let repl_type state input =
  let expr = parse_repl_cmd_arg "type" input in
  if expr = "" then prerr_endline "Usage: ,type <expr>"
  else
    let parse_result = Tart.Read.parse_string expr in
    if parse_result.errors <> [] then
      List.iter
        (fun err -> prerr_endline (format_parse_error err))
        parse_result.errors
    else
      match parse_result.sexps with
      | [] -> prerr_endline ",type: empty expression"
      | [ sexp ] ->
          let ty, errors = Tart.Check.check_expr ~env:state.type_env sexp in
          if errors <> [] then
            let diagnostics = Tart.Diagnostic.of_unify_errors errors in
            List.iter
              (fun d -> prerr_endline (Tart.Diagnostic.to_string_compact d))
              diagnostics
          else print_endline (Tart.Types.to_string ty)
      | _ -> prerr_endline ",type: expected single expression"

(** Show macro expansion of expression *)
let repl_expand state input =
  let expr = parse_repl_cmd_arg "expand" input in
  if expr = "" then prerr_endline "Usage: ,expand <expr>"
  else
    let parse_result = Tart.Read.parse_string expr in
    if parse_result.errors <> [] then
      List.iter
        (fun err -> prerr_endline (format_parse_error err))
        parse_result.errors
    else
      match parse_result.sexps with
      | [] -> prerr_endline ",expand: empty expression"
      | [ sexp ] -> (
          match Tart.Expand.expand_all state.interp sexp with
          | Tart.Expand.Expanded expanded ->
              print_endline (Tart.Sexp.to_string expanded)
          | Tart.Expand.Expansion_error { message; span } ->
              let pos = span.start_pos in
              prerr_endline
                (Printf.sprintf "%s:%d:%d: error: %s" pos.file pos.line
                   (pos.col + 1) message))
      | _ -> prerr_endline ",expand: expected single expression"

(** Show current bindings *)
let repl_env state =
  (* Show interpreter globals *)
  print_endline "=== Values ===";
  Hashtbl.iter
    (fun name value ->
      let type_str =
        match Tart.Type_env.lookup name state.type_env with
        | Some scheme -> " :: " ^ Tart.Type_env.scheme_to_string scheme
        | None -> ""
      in
      Printf.printf "%s = %s%s\n" name (Tart.Value.to_string value) type_str)
    state.interp.globals;
  (* Show macros *)
  if Hashtbl.length state.interp.macros > 0 then (
    print_endline "\n=== Macros ===";
    Hashtbl.iter (fun name _ -> Printf.printf "%s\n" name) state.interp.macros)

(** Show help message *)
let repl_help () =
  print_endline "REPL Commands:";
  print_endline "  ,quit, ,q      Exit REPL";
  print_endline "  ,type <expr>   Show type without evaluating";
  print_endline "  ,expand <expr> Show macro expansion";
  print_endline "  ,env           List current bindings";
  print_endline "  ,help          Show this help";
  print_endline "";
  print_endline "Enter any Elisp expression to evaluate it."

(** Evaluate input and display result *)
let repl_eval state input =
  let parse_result = Tart.Read.parse_string input in
  if parse_result.errors <> [] then
    List.iter
      (fun err -> prerr_endline (format_parse_error err))
      parse_result.errors
  else if parse_result.sexps = [] then ()
  else
    List.iter
      (fun sexp ->
        match Tart.Eval.eval_toplevel state.interp sexp with
        | Error eval_err ->
            let pos = eval_err.span.start_pos in
            prerr_endline
              (Printf.sprintf "%s:%d:%d: error: %s" pos.file pos.line
                 (pos.col + 1) eval_err.message)
        | Ok value ->
            (* Update type environment for defuns *)
            let env', _form_result, errors, _undefs =
              Tart.Check.check_form state.type_env sexp
            in
            state.type_env <- env';
            (* Infer type of the expression *)
            let ty, type_errors =
              Tart.Check.check_expr ~env:state.type_env sexp
            in
            let all_errors = errors @ type_errors in
            if all_errors <> [] then
              let diagnostics = Tart.Diagnostic.of_unify_errors all_errors in
              List.iter
                (fun d -> prerr_endline (Tart.Diagnostic.to_string_compact d))
                diagnostics
            else
              let value_str = Tart.Value.to_string value in
              let type_str = Tart.Types.to_string ty in
              Printf.printf "%s :: %s\n" value_str type_str)
      parse_result.sexps

(** Check if input appears to be incomplete (unbalanced parens) *)
let is_incomplete input =
  let count = ref 0 in
  let in_string = ref false in
  let escape = ref false in
  String.iter
    (fun c ->
      if !escape then escape := false
      else
        match c with
        | '\\' -> escape := true
        | '"' -> in_string := not !in_string
        | '(' when not !in_string -> incr count
        | ')' when not !in_string -> decr count
        | _ -> ())
    input;
  !count > 0

(** Read a potentially multi-line input *)
let read_input () =
  let rec loop acc prompt =
    print_string prompt;
    flush stdout;
    match In_channel.input_line In_channel.stdin with
    | None -> None (* EOF *)
    | Some line ->
        let combined = if acc = "" then line else acc ^ "\n" ^ line in
        if is_incomplete combined then loop combined "... > " else Some combined
  in
  loop "" "tart> "

(** REPL subcommand: interactive read-eval-print loop *)
let cmd_repl () =
  print_endline ("tart " ^ Tart.version ^ " REPL");
  print_endline "Type ,help for commands, ,quit to exit.";
  print_endline "";

  let state =
    {
      interp = Tart.Eval.make_interpreter ();
      type_env = Tart.Check.default_env ();
    }
  in

  let running = ref true in
  while !running do
    match read_input () with
    | None ->
        (* EOF (Ctrl-D) *)
        print_newline ();
        running := false
    | Some input ->
        let trimmed = String.trim input in
        if trimmed = "" then ()
        else if trimmed = ",quit" || trimmed = ",q" then running := false
        else if trimmed = ",help" then repl_help ()
        else if trimmed = ",env" then repl_env state
        else if String.starts_with ~prefix:",type" trimmed then
          repl_type state trimmed
        else if String.starts_with ~prefix:",expand" trimmed then
          repl_expand state trimmed
        else if String.starts_with ~prefix:"," trimmed then
          prerr_endline
            ("Unknown command: " ^ trimmed ^ ". Type ,help for commands.")
        else repl_eval state input
  done

(** Coverage subcommand: measure type signature coverage *)
let cmd_coverage ~format ~verbose ~fail_under ~exclude paths =
  let config = { Tart.File_scanner.exclude_patterns = exclude } in
  let files = Tart.File_scanner.scan_paths ~config paths in
  if files = [] then (
    prerr_endline "tart coverage: no .el files found";
    exit 1);

  (* Build search path for signature lookup *)
  let typings_root =
    let exe_dir = Filename.dirname Sys.executable_name in
    Filename.concat (Filename.dirname exe_dir) "share/tart/typings/emacs"
  in
  let version =
    Tart.Emacs_version.detect_or_default ~default:Tart.Emacs_version.latest ()
  in
  let search_path =
    Tart.Search_path.empty
    |> Tart.Search_path.with_typings_root typings_root
    |> Tart.Search_path.with_emacs_version version
  in

  (* Analyze files *)
  let result = Tart.Coverage_report.analyze_files ~search_path files in
  let summary = Tart.Coverage_report.summarize result in

  (* Generate and print report *)
  let output_config = { Tart.Report_format.format; verbose } in
  print_endline (Tart.Report_format.format_report ~config:output_config result);

  (* Check threshold if specified *)
  match fail_under with
  | Some threshold ->
      let percentage = Tart.Coverage_report.coverage_percentage summary in
      if percentage < float_of_int threshold then exit 1
  | None -> ()

(** LSP subcommand: start language server *)
let cmd_lsp ~log_level port =
  match port with
  | None ->
      (* stdio mode *)
      let server =
        Tart.Server.create ~log_level ~ic:In_channel.stdin
          ~oc:Out_channel.stdout ()
      in
      let exit_code = Tart.Server.run server in
      exit exit_code
  | Some p ->
      (* TCP mode *)
      let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, p) in
      let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.setsockopt socket Unix.SO_REUSEADDR true;
      Unix.bind socket addr;
      Unix.listen socket 1;
      prerr_endline (Printf.sprintf "[tart-lsp] Listening on port %d" p);
      let client_socket, _client_addr = Unix.accept socket in
      prerr_endline "[tart-lsp] Client connected";
      let ic = Unix.in_channel_of_descr client_socket in
      let oc = Unix.out_channel_of_descr client_socket in
      let server = Tart.Server.create ~log_level ~ic ~oc () in
      let exit_code = Tart.Server.run server in
      Unix.close client_socket;
      Unix.close socket;
      exit exit_code

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
  tart coverage [OPTIONS] [PATH]   Measure type signature coverage

Options:
  --version              Print version and exit
  --help, -h             Print this help message
  --emacs-version VER    Use typings for Emacs version VER (e.g., 31.0)

Eval options:
  <EXPR>       S-expression to evaluate

Expand options:
  --load FILE  Load macros from FILE before expanding

LSP options:
  --port PORT  Listen on TCP port instead of stdio

Coverage options:
  --format=FORMAT  Output format: human (default), json
  --verbose        Show additional details
  --fail-under=N   Exit 1 if coverage below N percent
  --exclude=PAT    Exclude files matching pattern (e.g., *-test.el)

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
      let load_files = ref [] in
      let target_file = ref None in
      let show_help = ref false in
      let rec parse_expand_args = function
        | [] -> ()
        | "--help" :: _ | "-h" :: _ -> show_help := true
        | "--load" :: f :: rest ->
            load_files := !load_files @ [ f ];
            parse_expand_args rest
        | "--load" :: [] ->
            prerr_endline "tart expand: --load requires a file";
            exit 2
        | arg :: _ when String.starts_with ~prefix:"-" arg ->
            prerr_endline ("tart expand: unknown option: " ^ arg);
            exit 2
        | file :: rest -> (
            match !target_file with
            | None ->
                target_file := Some file;
                parse_expand_args rest
            | Some _ ->
                prerr_endline "tart expand: only one file allowed";
                exit 2)
      in
      parse_expand_args rest;
      if !show_help then (
        print_endline "Usage: tart expand [--load FILE]... FILE";
        print_endline "";
        print_endline "Macro-expand FILE and print the result.";
        print_endline "";
        print_endline "Options:";
        print_endline
          "  --load FILE  Load macros from FILE before expanding (repeatable)";
        exit 0)
      else
        match !target_file with
        | None ->
            prerr_endline "tart expand: missing file";
            exit 2
        | Some file -> cmd_expand ~load_files:!load_files file)
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
      let log_level = ref Tart.Server.Normal in
      let rec parse_lsp_args = function
        | [] -> ()
        | "--help" :: _ | "-h" :: _ -> show_help := true
        | "--port" :: p :: rest ->
            port := Some (int_of_string p);
            parse_lsp_args rest
        | "--log-level" :: "debug" :: rest ->
            log_level := Tart.Server.Debug;
            parse_lsp_args rest
        | "--log-level" :: "quiet" :: rest ->
            log_level := Tart.Server.Quiet;
            parse_lsp_args rest
        | "--log-level" :: "normal" :: rest ->
            log_level := Tart.Server.Normal;
            parse_lsp_args rest
        | "--log-level" :: _ :: _ ->
            prerr_endline
              "tart lsp: --log-level must be debug, normal, or quiet";
            exit 2
        | "--log-level" :: [] ->
            prerr_endline "tart lsp: --log-level requires an argument";
            exit 2
        | arg :: _ ->
            prerr_endline ("tart lsp: unknown option: " ^ arg);
            exit 2
      in
      parse_lsp_args rest;
      if !show_help then (
        print_endline "Usage: tart lsp [OPTIONS]";
        print_endline "";
        print_endline "Start the LSP server.";
        print_endline "";
        print_endline "Options:";
        print_endline "  --port PORT        Listen on TCP port instead of stdio";
        print_endline
          "  --log-level LEVEL  Set log level: debug, normal (default), quiet";
        exit 0)
      else cmd_lsp ~log_level:!log_level !port
  | "coverage" :: rest | "cov" :: rest ->
      let format = ref Tart.Report_format.Human in
      let verbose = ref false in
      let fail_under = ref None in
      let exclude = ref [] in
      let show_help = ref false in
      let paths = ref [] in
      let rec parse_coverage_args = function
        | [] -> ()
        | "--help" :: _ | "-h" :: _ -> show_help := true
        | "--verbose" :: rest ->
            verbose := true;
            parse_coverage_args rest
        | "--format=json" :: rest ->
            format := Tart.Report_format.Json;
            parse_coverage_args rest
        | "--format=human" :: rest ->
            format := Tart.Report_format.Human;
            parse_coverage_args rest
        | arg :: _rest when String.starts_with ~prefix:"--format=" arg ->
            prerr_endline "tart coverage: --format must be 'human' or 'json'";
            exit 2
        | arg :: rest when String.starts_with ~prefix:"--fail-under=" arg -> (
            let value = String.sub arg 13 (String.length arg - 13) in
            match int_of_string_opt value with
            | Some n when n >= 0 && n <= 100 ->
                fail_under := Some n;
                parse_coverage_args rest
            | _ ->
                prerr_endline "tart coverage: --fail-under requires 0-100";
                exit 2)
        | arg :: rest when String.starts_with ~prefix:"--exclude=" arg ->
            let pattern = String.sub arg 10 (String.length arg - 10) in
            exclude := !exclude @ [ pattern ];
            parse_coverage_args rest
        | arg :: _ when String.starts_with ~prefix:"-" arg ->
            prerr_endline ("tart coverage: unknown option: " ^ arg);
            exit 2
        | path :: rest ->
            paths := !paths @ [ path ];
            parse_coverage_args rest
      in
      parse_coverage_args rest;
      if !show_help then (
        print_endline "Usage: tart coverage [OPTIONS] [PATH...]";
        print_endline "";
        print_endline "Measure type signature coverage for Elisp files.";
        print_endline "";
        print_endline "Options:";
        print_endline "  --format=FORMAT  Output: human (default), json";
        print_endline "  --verbose        Show covered identifiers";
        print_endline "  --fail-under=N   Exit 1 if coverage below N%";
        print_endline "  --exclude=PAT    Exclude files matching pattern";
        exit 0)
      else
        cmd_coverage ~format:!format ~verbose:!verbose ~fail_under:!fail_under
          ~exclude:!exclude !paths
  (* Default: type-check files *)
  | files ->
      (* Parse check command options *)
      let emacs_version = ref None in
      let show_help = ref false in
      let show_version = ref false in
      let file_list = ref [] in
      let rec parse_check_args = function
        | [] -> ()
        | "--help" :: _ | "-h" :: _ -> show_help := true
        | "--version" :: _ | "-v" :: _ -> show_version := true
        | "--emacs-version" :: v :: rest ->
            (match Tart.Emacs_version.parse_version v with
            | Some ver -> emacs_version := Some ver
            | None ->
                prerr_endline
                  (Printf.sprintf "tart: invalid Emacs version '%s'" v);
                prerr_endline "Expected format: MAJOR.MINOR (e.g., 31.0)";
                exit 2);
            parse_check_args rest
        | "--emacs-version" :: [] ->
            prerr_endline "tart: --emacs-version requires a version argument";
            exit 2
        | arg :: _ when String.starts_with ~prefix:"-" arg ->
            prerr_endline ("tart: unknown option: " ^ arg);
            exit 2
        | file :: rest ->
            file_list := !file_list @ [ file ];
            parse_check_args rest
      in
      parse_check_args files;
      if !show_help then print_help ()
      else if !show_version then print_version ()
      else (
        (* Log the Emacs version being used *)
        (match !emacs_version with
        | Some ver ->
            prerr_endline
              (Printf.sprintf "[tart] Using Emacs version: %s"
                 (Tart.Emacs_version.version_to_string ver))
        | None -> ());
        cmd_check !file_list)
