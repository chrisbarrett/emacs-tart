(** Tart CLI entry point

    Subcommand dispatch for type-checking, evaluation, macro expansion, and LSP
    server. Uses Cmdliner for declarative argument parsing. *)

open Cmdliner

(* ============================================================================
   Output Format Type
   ============================================================================ *)

type output_format = Human | Json

let output_format_enum = Arg.enum [ ("human", Human); ("json", Json) ]

(* ============================================================================
   Helper Functions
   ============================================================================ *)

(** Format a parse error for display in compiler-style format *)
let format_parse_error (err : Tart.Read.parse_error) : string =
  let pos = err.span.start_pos in
  Printf.sprintf "%s:%d:%d: error: %s" pos.file pos.line (pos.col + 1)
    err.message

(** Find typings root directory with verbose logging. *)
let find_typings_root ~verbose : string =
  let open Tart.Verbose_log in
  let exe_path = Sys.executable_name in
  let exe_dir = Filename.dirname exe_path in
  verbose_log verbose "Executable: %s" exe_path;
  verbose_log verbose "Typings root candidates:";

  let candidates =
    [
      Filename.concat
        (Filename.dirname
           (Filename.dirname (Filename.dirname (Filename.dirname exe_dir))))
        "typings/emacs";
      Filename.concat (Filename.dirname exe_dir) "typings/emacs";
      Filename.concat (Filename.dirname exe_dir) "share/tart/typings/emacs";
    ]
  in

  let dir_exists path =
    try (Unix.stat path).Unix.st_kind = Unix.S_DIR
    with Unix.Unix_error _ -> false
  in

  let rec try_candidates found_path = function
    | [] -> found_path
    | path :: rest ->
        let exists = dir_exists path in
        verbose_log verbose "  %s (%s)" path
          (if exists then "found" else "not found");
        if exists then try_candidates (Some path) rest
        else try_candidates found_path rest
  in

  match try_candidates None candidates with
  | Some path ->
      verbose_log verbose "Using typings root: %s" path;
      path
  | None ->
      let fallback = List.hd (List.rev candidates) in
      verbose_log verbose "Using typings root: %s (may not exist)" fallback;
      fallback

(** Detect Emacs version with verbose logging. *)
let detect_emacs_version ~verbose ~typings_root : Tart.Emacs_version.version =
  let open Tart.Verbose_log in
  verbose_log verbose "Detecting Emacs version...";

  let emacs_path =
    try
      let ic = Unix.open_process_in "which emacs 2>/dev/null" in
      let path =
        try String.trim (input_line ic) with End_of_file -> "not found"
      in
      ignore (Unix.close_process_in ic);
      path
    with _ -> "not found"
  in
  verbose_log verbose "Emacs binary: %s" emacs_path;

  let detection_result = Tart.Emacs_version.detect () in
  let version =
    match detection_result with
    | Tart.Emacs_version.Detected v ->
        verbose_log verbose "Detected version: %s"
          (Tart.Emacs_version.version_to_string v);
        v
    | Tart.Emacs_version.NotFound ->
        verbose_log verbose "Emacs not found, using default";
        Tart.Emacs_version.latest
    | Tart.Emacs_version.ParseError msg ->
        verbose_log verbose "Version parse error: %s, using default" msg;
        Tart.Emacs_version.latest
  in

  let fallback_chain = Tart.Search_path.version_fallback_candidates version in
  verbose_log verbose "Version fallback chain: %s"
    (String.concat " -> " fallback_chain);

  (match Tart.Search_path.find_typings_dir ~typings_root ~version with
  | Some dir ->
      let selected_version = Filename.basename dir in
      verbose_log verbose "Using typings version: %s" selected_version
  | None -> verbose_log verbose "No typings directory found");

  version

(** Log typings files being loaded. *)
let log_typings_loading ~verbose ~typings_root ~version =
  let open Tart.Verbose_log in
  match Tart.Search_path.find_typings_dir ~typings_root ~version with
  | None -> ()
  | Some typings_dir ->
      let c_core_dir = Filename.concat typings_dir "c-core" in
      let files = Tart.Search_path.list_c_core_files c_core_dir in
      if files = [] then verbose_log verbose "No c-core typings files found"
      else (
        verbose_log verbose "Loading c-core typings from %s" c_core_dir;
        let total_sigs = ref 0 in
        List.iter
          (fun path ->
            match Tart.Search_path.parse_signature_file path with
            | None ->
                verbose_log verbose "  %s: (parse error)"
                  (Filename.basename path)
            | Some sig_file ->
                let count = List.length sig_file.Tart.Sig_ast.sig_decls in
                total_sigs := !total_sigs + count;
                verbose_log verbose "  %s: %d signatures"
                  (Filename.basename path) count)
          files;
        verbose_log verbose "Total signatures loaded: %d" !total_sigs)

(** Scan C source files with verbose logging. *)
let scan_c_source_verbose ~verbose ~src_dir : Tart.C_scanner.c_definition list =
  let open Tart.Verbose_log in
  verbose_log verbose "Scanning C source: %s" src_dir;

  let entries =
    if Sys.file_exists src_dir && Sys.is_directory src_dir then
      Sys.readdir src_dir |> Array.to_list
    else []
  in
  let c_files =
    entries
    |> List.filter (fun f -> Filename.check_suffix f ".c")
    |> List.sort String.compare
  in

  let total_defuns = ref 0 in
  let total_defvars = ref 0 in
  let total_defsyms = ref 0 in
  let all_defs = ref [] in

  List.iter
    (fun filename ->
      let path = Filename.concat src_dir filename in
      let defs = Tart.C_scanner.scan_file path in
      let defuns =
        List.filter (fun d -> d.Tart.C_scanner.kind = Tart.C_scanner.Defun) defs
        |> List.length
      in
      let defvars =
        List.filter
          (fun d -> d.Tart.C_scanner.kind = Tart.C_scanner.Defvar)
          defs
        |> List.length
      in
      let defsyms =
        List.filter
          (fun d -> d.Tart.C_scanner.kind = Tart.C_scanner.Defsym)
          defs
        |> List.length
      in
      if defuns > 0 || defvars > 0 || defsyms > 0 then
        verbose_log verbose "  %s: %d DEFUNs, %d DEFVARs, %d DEFSYMs" filename
          defuns defvars defsyms;
      total_defuns := !total_defuns + defuns;
      total_defvars := !total_defvars + defvars;
      total_defsyms := !total_defsyms + defsyms;
      all_defs := !all_defs @ defs)
    c_files;

  verbose_log verbose "Total: %d DEFUNs, %d DEFVARs, %d DEFSYMs" !total_defuns
    !total_defvars !total_defsyms;
  !all_defs

(* ============================================================================
   Command Implementations
   ============================================================================ *)

(** Type-check a single file with a given environment. *)
let check_file env filename : Tart.Type_env.t * Tart.Error.t list =
  let parse_result = Tart.Read.parse_file filename in

  let parse_errors =
    List.map
      (fun (err : Tart.Read.parse_error) ->
        Tart.Error.parse_error ~message:err.message ~span:err.span)
      parse_result.errors
  in

  if parse_result.sexps = [] then (env, parse_errors)
  else
    let check_result = Tart.Check.check_program ~env parse_result.sexps in
    let type_diagnostics =
      Tart.Diagnostic.of_unify_errors check_result.errors
    in
    let candidates = Tart.Type_env.names check_result.env in
    let undefined_diagnostics =
      List.map
        (fun (undef : Tart.Infer.undefined_var) ->
          Tart.Diagnostic.undefined_variable ~span:undef.span ~name:undef.name
            ~candidates ())
        check_result.undefineds
    in
    let diagnostics = type_diagnostics @ undefined_diagnostics in
    let type_errors = Tart.Error.of_diagnostics diagnostics in
    (check_result.env, parse_errors @ type_errors)

(** Validate a file exists and is readable, returning structured error on
    failure. *)
let validate_file path : (unit, Tart.File_error.t) result =
  Tart.File_error.check_file path

(** Check subcommand: type-check files *)
let run_check format emacs_version files =
  if files = [] then (
    let err =
      Tart.Error.cli_error ~message:"no input files"
        ~hint:"use --help for usage" ()
    in
    prerr_endline (Tart.Error.to_string err);
    exit 2);
  (* Validate all files exist first, collecting file errors *)
  let file_errors, valid_files =
    List.fold_left
      (fun (errors, valid) path ->
        match validate_file path with
        | Ok () -> (errors, path :: valid)
        | Error file_err -> (Tart.Error.of_file_error file_err :: errors, valid))
      ([], []) files
  in
  let valid_files = List.rev valid_files in
  let file_errors = List.rev file_errors in
  (* Build initial environment with c-core signatures *)
  let typings_root = find_typings_root ~verbose:false in
  let version =
    match emacs_version with
    | Some v -> v
    | None -> (
        match Tart.Emacs_version.detect () with
        | Tart.Emacs_version.Detected v -> v
        | _ -> Tart.Emacs_version.latest)
  in
  let search_path =
    Tart.Search_path.empty
    |> Tart.Search_path.with_typings_root typings_root
    |> Tart.Search_path.with_emacs_version version
  in
  let base_env = Tart.Check.default_env () in
  let initial_env = Tart.Search_path.load_c_core ~search_path base_env in
  let initial_env = Tart.Search_path.load_lisp_core ~search_path initial_env in
  let _, type_errors =
    List.fold_left
      (fun (env, acc_errors) file ->
        let env', errs = check_file env file in
        (env', acc_errors @ errs))
      (initial_env, []) valid_files
  in
  let all_errors = file_errors @ type_errors in
  (match format with
  | Human -> Tart.Error.report_human all_errors
  | Json -> Tart.Error.report_json all_errors);
  if all_errors <> [] then exit 1

(** Eval subcommand: evaluate expression *)
let run_eval expr =
  let parse_result = Tart.Read.parse_string expr in

  if parse_result.errors <> [] then (
    List.iter
      (fun err -> prerr_endline (format_parse_error err))
      parse_result.errors;
    exit 1);
  match parse_result.sexps with
  | [] ->
      prerr_endline "tart eval: empty expression";
      exit 1
  | [ sexp ] -> (
      let global = Tart.Eval.make_interpreter () in
      match Tart.Eval.eval_toplevel global sexp with
      | Error eval_err ->
          let pos = eval_err.span.start_pos in
          prerr_endline
            (Printf.sprintf "%s:%d:%d: error: %s" pos.file pos.line
               (pos.col + 1) eval_err.message);
          exit 1
      | Ok value ->
          let ty, errors = Tart.Check.check_expr sexp in
          if errors <> [] then (
            let diagnostics = Tart.Diagnostic.of_unify_errors errors in
            List.iter
              (fun d -> prerr_endline (Tart.Diagnostic.to_string d))
              diagnostics;
            exit 1)
          else
            let value_str = Tart.Value.to_string value in
            let type_str = Tart.Types.to_string ty in
            Printf.printf "%s :: %s\n" value_str type_str)
  | _ ->
      prerr_endline "tart eval: expected single expression";
      exit 1

(** Expand subcommand: macro-expand and print *)
let run_expand load_files file =
  let global = Tart.Eval.make_interpreter () in

  (* Validate --load files first *)
  List.iter
    (fun load_file ->
      match validate_file load_file with
      | Error file_err ->
          prerr_endline (Tart.File_error.to_string file_err);
          exit 1
      | Ok () ->
          let parse_result = Tart.Read.parse_file load_file in
          if parse_result.errors <> [] then (
            List.iter
              (fun err -> prerr_endline (format_parse_error err))
              parse_result.errors;
            exit 1)
          else Tart.Expand.load_macros global parse_result.sexps)
    load_files;

  (* Validate target file *)
  (match validate_file file with
  | Error file_err ->
      prerr_endline (Tart.File_error.to_string file_err);
      exit 1
  | Ok () -> ());

  let parse_result = Tart.Read.parse_file file in
  if parse_result.errors <> [] then (
    List.iter
      (fun err -> prerr_endline (format_parse_error err))
      parse_result.errors;
    exit 1);
  let expand_error = ref false in
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
          expand_error := true)
    parse_result.sexps;
  if !expand_error then exit 1

(* REPL helpers *)
type repl_state = {
  interp : Tart.Env.global;
  mutable type_env : Tart.Type_env.t;
}

let parse_repl_cmd_arg cmd input =
  let prefix = "," ^ cmd in
  let prefix_len = String.length prefix in
  if String.length input > prefix_len then
    let rest = String.sub input prefix_len (String.length input - prefix_len) in
    String.trim rest
  else ""

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
              (fun d -> prerr_endline (Tart.Diagnostic.to_string d))
              diagnostics
          else print_endline (Tart.Types.to_string ty)
      | _ -> prerr_endline ",type: expected single expression"

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

let repl_env state =
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
  if Hashtbl.length state.interp.macros > 0 then (
    print_endline "\n=== Macros ===";
    Hashtbl.iter (fun name _ -> Printf.printf "%s\n" name) state.interp.macros)

let repl_help () =
  print_endline "REPL Commands:";
  print_endline "  ,quit, ,q      Exit REPL";
  print_endline "  ,type <expr>   Show type without evaluating";
  print_endline "  ,expand <expr> Show macro expansion";
  print_endline "  ,env           List current bindings";
  print_endline "  ,help          Show this help";
  print_endline "";
  print_endline "Enter any Elisp expression to evaluate it."

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
            let env', _form_result, errors, _undefs =
              Tart.Check.check_form state.type_env sexp
            in
            state.type_env <- env';
            let ty, type_errors =
              Tart.Check.check_expr ~env:state.type_env sexp
            in
            let all_errors = errors @ type_errors in
            if all_errors <> [] then
              let diagnostics = Tart.Diagnostic.of_unify_errors all_errors in
              List.iter
                (fun d -> prerr_endline (Tart.Diagnostic.to_string d))
                diagnostics
            else
              let value_str = Tart.Value.to_string value in
              let type_str = Tart.Types.to_string ty in
              Printf.printf "%s :: %s\n" value_str type_str)
      parse_result.sexps

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

let read_input () =
  let rec loop acc prompt =
    print_string prompt;
    flush stdout;
    match In_channel.input_line In_channel.stdin with
    | None -> None
    | Some line ->
        let combined = if acc = "" then line else acc ^ "\n" ^ line in
        if is_incomplete combined then loop combined "... > " else Some combined
  in
  loop "" "tart> "

(** Repl subcommand: interactive REPL *)
let run_repl () =
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

(** LSP subcommand: start language server *)
let run_lsp log_level port =
  match port with
  | None ->
      let server =
        Tart.Server.create ~log_level ~ic:In_channel.stdin
          ~oc:Out_channel.stdout ()
      in
      let exit_code = Tart.Server.run server in
      exit exit_code
  | Some p ->
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

(** Coverage subcommand: measure type coverage *)
let run_coverage format verbose fail_under exclude paths =
  let config = { Tart.File_scanner.exclude_patterns = exclude } in
  let files = Tart.File_scanner.scan_paths ~config paths in
  if files = [] then (
    prerr_endline "tart coverage: no .el files found";
    exit 1);
  let typings_root = find_typings_root ~verbose in
  let version = detect_emacs_version ~verbose ~typings_root in
  log_typings_loading ~verbose ~typings_root ~version;
  let search_path =
    Tart.Search_path.empty
    |> Tart.Search_path.with_typings_root typings_root
    |> Tart.Search_path.with_emacs_version version
  in

  let result = Tart.Coverage_report.analyze_files ~search_path files in
  let summary = Tart.Coverage_report.summarize result in

  let output_config = { Tart.Report_format.format; verbose } in
  print_endline (Tart.Report_format.format_report ~config:output_config result);

  match fail_under with
  | Some threshold ->
      let percentage = Tart.Coverage_report.coverage_percentage summary in
      if percentage < float_of_int threshold then exit 1
  | None -> ()

(** Emacs-coverage subcommand: measure C layer coverage *)
let run_emacs_coverage verbose emacs_source emacs_version_opt =
  let open Tart.Verbose_log in
  let source_result = Tart.Emacs_source.discover ~explicit_path:emacs_source in
  match source_result with
  | Tart.Emacs_source.NotFound _ | Tart.Emacs_source.InvalidPath _ ->
      prerr_endline (Tart.Emacs_source.format_error source_result);
      exit 1
  | Tart.Emacs_source.Found { source_dir; version = detected_version } ->
      let typings_root = find_typings_root ~verbose in

      verbose_log verbose "Detecting Emacs version...";
      verbose_log verbose "Emacs source: %s" source_dir;
      verbose_log verbose "Detected version (from source): %s" detected_version;

      let version_str, version =
        match emacs_version_opt with
        | Some v ->
            verbose_log verbose "Using override version: %s"
              (Tart.Emacs_version.version_to_string v);
            (Tart.Emacs_version.version_to_string v, v)
        | None -> (
            match Tart.Emacs_version.parse_version detected_version with
            | Some v -> (detected_version, v)
            | None ->
                verbose_log verbose "Could not parse version, using default: %s"
                  (Tart.Emacs_version.version_to_string
                     Tart.Emacs_version.latest);
                ( Tart.Emacs_version.version_to_string Tart.Emacs_version.latest,
                  Tart.Emacs_version.latest ))
      in

      let fallback_chain =
        Tart.Search_path.version_fallback_candidates version
      in
      verbose_log verbose "Version fallback chain: %s"
        (String.concat " -> " fallback_chain);

      (match Tart.Search_path.find_typings_dir ~typings_root ~version with
      | Some dir ->
          let selected_version = Filename.basename dir in
          verbose_log verbose "Using typings version: %s" selected_version
      | None -> verbose_log verbose "No typings directory found");

      log_typings_loading ~verbose ~typings_root ~version;

      let src_dir = Filename.concat source_dir "src" in
      let definitions = scan_c_source_verbose ~verbose ~src_dir in

      let result =
        Tart.Emacs_coverage.calculate_coverage ~source_dir
          ~emacs_version:version_str ~typings_root ~version definitions
      in
      let summary = Tart.Emacs_coverage.summarize result in
      let percentage = Tart.Emacs_coverage.coverage_percentage summary in

      if verbose then (
        verbose_log verbose "Matching symbols against typings...";
        verbose_log verbose "Sample matches:";
        let covered = Tart.Emacs_coverage.covered_public result in
        List.iteri
          (fun i item ->
            if i < 5 then
              verbose_log verbose "  %s: COVERED (%s:%d)"
                item.Tart.Emacs_coverage.definition.name
                item.Tart.Emacs_coverage.definition.file
                item.Tart.Emacs_coverage.definition.line)
          covered;
        let uncovered = Tart.Emacs_coverage.uncovered_public result in
        List.iteri
          (fun i item ->
            if i < 5 then
              verbose_log verbose "  %s: UNCOVERED"
                item.Tart.Emacs_coverage.definition.name)
          uncovered;
        verbose_log verbose "Match complete: %d/%d DEFUNs covered (%.1f%%)"
          summary.covered_public summary.total_public percentage);

      print_endline "=== C Layer Coverage ===";
      Printf.printf "Emacs source: %s\n" source_dir;
      Printf.printf "Emacs version: %s\n" version_str;
      Printf.printf "Files scanned: %d\n" result.files_scanned;
      Printf.printf "Total public symbols: %d\n" summary.total_public;
      Printf.printf "Covered: %d (%.1f%%)\n" summary.covered_public percentage;
      Printf.printf "Uncovered: %d\n" summary.uncovered_public;
      print_newline ();

      let private_defs = Tart.Emacs_coverage.private_definitions result in
      if private_defs <> [] then (
        Printf.printf "Private symbols excluded: %d\n" summary.total_private;
        List.iter
          (fun item ->
            Printf.printf "  %s\n" item.Tart.Emacs_coverage.definition.name)
          (List.filteri (fun i _ -> i < 10) private_defs);
        if List.length private_defs > 10 then
          Printf.printf "  ... and %d more\n" (List.length private_defs - 10);
        print_newline ());

      let uncovered = Tart.Emacs_coverage.uncovered_public result in
      if uncovered <> [] then (
        print_endline "Uncovered public:";
        List.iter
          (fun item ->
            Printf.printf "  %s\n" item.Tart.Emacs_coverage.definition.name)
          uncovered)

(* ============================================================================
   Cmdliner Argument Definitions
   ============================================================================ *)

(* Common arguments *)
let format_arg =
  let doc = "Output format. $(docv) is either 'human' or 'json'." in
  Arg.(
    value & opt output_format_enum Human & info [ "format" ] ~docv:"FORMAT" ~doc)

let verbose_arg =
  let doc = "Show diagnostic output." in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

let emacs_version_conv =
  let parse s =
    match Tart.Emacs_version.parse_version s with
    | Some v -> Ok v
    | None -> Error (`Msg "expected format: MAJOR.MINOR (e.g., 31.0)")
  in
  let print fmt v =
    Format.pp_print_string fmt (Tart.Emacs_version.version_to_string v)
  in
  (parse, print)

let emacs_version_arg =
  let doc = "Use typings for Emacs version $(docv)." in
  Arg.(
    value
    & opt (some (conv emacs_version_conv)) None
    & info [ "emacs-version" ] ~docv:"VER" ~doc)

(* Check subcommand *)
let check_files_arg =
  let doc = "Elisp files to type-check." in
  (* Use string instead of file to handle errors with structured File_error *)
  Arg.(value & pos_all string [] & info [] ~docv:"FILE" ~doc)

let check_cmd =
  let doc = "Type-check Elisp files" in
  let man =
    [
      `S Manpage.s_description;
      `P "Type-check one or more Emacs Lisp files and report any type errors.";
    ]
  in
  let info = Cmd.info "check" ~doc ~man in
  Cmd.v info
    Term.(const run_check $ format_arg $ emacs_version_arg $ check_files_arg)

(* Eval subcommand *)
let eval_expr_arg =
  let doc = "S-expression to evaluate." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"EXPR" ~doc)

let eval_cmd =
  let doc = "Evaluate an expression and print the result" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Parse, evaluate, and type-check a single S-expression, then print the \
         value and its type.";
    ]
  in
  let info = Cmd.info "eval" ~doc ~man in
  Cmd.v info Term.(const run_eval $ eval_expr_arg)

(* Expand subcommand *)
let expand_load_arg =
  let doc = "Load macros from $(docv) before expanding (repeatable)." in
  (* Use string instead of file to handle errors with structured File_error *)
  Arg.(value & opt_all string [] & info [ "load" ] ~docv:"FILE" ~doc)

let expand_file_arg =
  let doc = "File to macro-expand." in
  (* Use string instead of file to handle errors with structured File_error *)
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let expand_cmd =
  let doc = "Macro-expand a file and print the result" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Load macros from optional --load files, then macro-expand the target \
         file and print the result.";
    ]
  in
  let info = Cmd.info "expand" ~doc ~man in
  Cmd.v info Term.(const run_expand $ expand_load_arg $ expand_file_arg)

(* Repl subcommand *)
let repl_cmd =
  let doc = "Start an interactive REPL" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Start an interactive read-eval-print loop for Emacs Lisp. Type ,help \
         for available commands.";
    ]
  in
  let info = Cmd.info "repl" ~doc ~man in
  Cmd.v info Term.(const run_repl $ const ())

(* LSP subcommand *)
let lsp_port_conv =
  let parse s =
    match int_of_string_opt s with
    | Some n when n > 0 && n < 65536 -> Ok n
    | Some _ -> Error (`Msg "port must be 1-65535")
    | None -> Error (`Msg "port must be an integer")
  in
  let print fmt n = Format.pp_print_int fmt n in
  (parse, print)

let lsp_port_arg =
  let doc = "Listen on TCP port $(docv) instead of stdio." in
  Arg.(
    value
    & opt (some (conv lsp_port_conv)) None
    & info [ "port" ] ~docv:"PORT" ~doc)

let lsp_log_level_arg =
  let level_enum =
    Arg.enum
      [
        ("debug", Tart.Server.Debug);
        ("normal", Tart.Server.Normal);
        ("quiet", Tart.Server.Quiet);
      ]
  in
  let doc = "Set log level. $(docv) is one of: debug, normal, quiet." in
  Arg.(
    value
    & opt level_enum Tart.Server.Normal
    & info [ "log-level" ] ~docv:"LEVEL" ~doc)

let lsp_cmd =
  let doc = "Start the LSP server" in
  let man =
    [
      `S Manpage.s_description;
      `P "Start a Language Server Protocol server for editor integration.";
      `P "By default, communicates over stdio. Use --port for TCP mode.";
    ]
  in
  let info = Cmd.info "lsp" ~doc ~man in
  Cmd.v info Term.(const run_lsp $ lsp_log_level_arg $ lsp_port_arg)

(* Coverage subcommand *)
let coverage_format_enum =
  Arg.enum
    [ ("human", Tart.Report_format.Human); ("json", Tart.Report_format.Json) ]

let coverage_format_arg =
  let doc = "Output format. $(docv) is either 'human' or 'json'." in
  Arg.(
    value
    & opt coverage_format_enum Tart.Report_format.Human
    & info [ "format" ] ~docv:"FORMAT" ~doc)

let coverage_fail_under_conv =
  let parse s =
    match int_of_string_opt s with
    | Some n when n >= 0 && n <= 100 -> Ok (Some n)
    | Some _ -> Error (`Msg "value must be 0-100")
    | None -> Error (`Msg "value must be an integer")
  in
  let print fmt = function
    | Some n -> Format.pp_print_int fmt n
    | None -> Format.pp_print_string fmt ""
  in
  (parse, print)

let coverage_fail_under_arg =
  let doc = "Exit 1 if coverage is below $(docv) percent." in
  Arg.(
    value
    & opt (conv coverage_fail_under_conv) None
    & info [ "fail-under" ] ~docv:"N" ~doc)

let coverage_exclude_arg =
  let doc = "Exclude files matching $(docv) (e.g., *-test.el). Repeatable." in
  Arg.(value & opt_all string [] & info [ "exclude" ] ~docv:"PATTERN" ~doc)

let coverage_paths_arg =
  let doc = "Paths to scan for .el files (default: current directory)." in
  Arg.(value & pos_all string [ "." ] & info [] ~docv:"PATH" ~doc)

let coverage_cmd =
  let doc = "Measure type signature coverage" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Scan Elisp files and report what percentage of definitions have type \
         signatures.";
    ]
  in
  let info = Cmd.info "coverage" ~doc ~man in
  Cmd.v info
    Term.(
      const run_coverage $ coverage_format_arg $ verbose_arg
      $ coverage_fail_under_arg $ coverage_exclude_arg $ coverage_paths_arg)

(* Emacs-coverage subcommand *)
let emacs_source_arg =
  let doc = "Path to Emacs source directory." in
  Arg.(value & opt (some dir) None & info [ "emacs-source" ] ~docv:"PATH" ~doc)

let emacs_coverage_cmd =
  let doc = "Measure Emacs C layer type coverage" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Scan Emacs C source for DEFUN, DEFVAR, and DEFSYM declarations, then \
         report how many have type signatures.";
    ]
  in
  let info = Cmd.info "emacs-coverage" ~doc ~man in
  Cmd.v info
    Term.(
      const run_emacs_coverage $ verbose_arg $ emacs_source_arg
      $ emacs_version_arg)

(* ============================================================================
   Main Command
   ============================================================================ *)

let default_cmd =
  Term.(const run_check $ format_arg $ emacs_version_arg $ check_files_arg)

let main_cmd =
  let doc = "A type checker for Emacs Lisp" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Tart provides static type checking for Emacs Lisp. Without a \
         subcommand, it type-checks the given files.";
      `S Manpage.s_commands;
      `S Manpage.s_exit_status;
      `P
        "0 on success, 1 on input errors (type/parse/eval errors), 2 on usage \
         errors (bad arguments).";
    ]
  in
  let info = Cmd.info "tart" ~version:Tart.version ~doc ~man in
  Cmd.group ~default:default_cmd info
    [
      check_cmd;
      eval_cmd;
      expand_cmd;
      repl_cmd;
      lsp_cmd;
      coverage_cmd;
      emacs_coverage_cmd;
    ]

(** Determine if an argument looks like a file path rather than a subcommand. *)
let looks_like_file arg =
  (* Check if it has a file extension or starts with . or / *)
  Filename.extension arg <> ""
  || String.starts_with ~prefix:"." arg
  || String.starts_with ~prefix:"/" arg

let () =
  (* For backward compatibility: if first non-option arg looks like a file,
     prepend "check" to the arguments so Cmdliner dispatches correctly *)
  let args = Array.to_list Sys.argv in
  let adjusted_args =
    match args with
    | prog :: rest ->
        (* Find first non-option argument *)
        let find_first = function
          | [] -> rest (* No positional args, keep as is *)
          | arg :: _ when String.starts_with ~prefix:"-" arg ->
              rest (* Options first, keep as is *)
          | arg :: _ when looks_like_file arg ->
              (* First arg looks like a file - prepend "check" *)
              "check" :: rest
          | _ -> rest (* First arg is a subcommand, keep as is *)
        in
        prog :: find_first rest
    | [] -> args
  in
  (* Replace argv with adjusted args *)
  let adjusted_array = Array.of_list adjusted_args in
  for
    i = 0 to min (Array.length Sys.argv - 1) (Array.length adjusted_array - 1)
  do
    Sys.argv.(i) <- adjusted_array.(i)
  done;
  (* Cmdliner will read from Sys.argv, so we need a different approach *)
  exit (Cmd.eval ~argv:(Array.of_list adjusted_args) main_cmd)
