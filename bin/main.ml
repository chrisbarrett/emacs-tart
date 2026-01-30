(** Tart CLI entry point

    Subcommand dispatch for type-checking, evaluation, macro expansion, and LSP
    server. *)

(** Output format for check command *)
type output_format = Human | Json

(** Format a parse error for display in compiler-style format *)
let format_parse_error (err : Tart.Read.parse_error) : string =
  let pos = err.span.start_pos in
  Printf.sprintf "%s:%d:%d: error: %s" pos.file pos.line (pos.col + 1)
    err.message

(** Type-check a single file with a given environment.

    Returns the updated environment (with definitions from this file) and list
    of errors. *)
let check_file env filename : Tart.Type_env.t * Tart.Error.t list =
  (* Parse the file *)
  let parse_result = Tart.Read.parse_file filename in

  (* Convert parse errors to Error.t *)
  let parse_errors =
    List.map
      (fun (err : Tart.Read.parse_error) ->
        Tart.Error.parse_error ~message:err.message ~span:err.span)
      parse_result.errors
  in

  (* Type-check if we have any successfully parsed forms *)
  if parse_result.sexps = [] then (env, parse_errors)
  else
    let check_result = Tart.Check.check_program ~env parse_result.sexps in

    (* Convert unify errors to diagnostics *)
    let type_diagnostics =
      Tart.Diagnostic.of_unify_errors check_result.errors
    in

    (* Convert undefined variable errors to diagnostics *)
    let candidates = Tart.Type_env.names check_result.env in
    let undefined_diagnostics =
      List.map
        (fun (undef : Tart.Infer.undefined_var) ->
          Tart.Diagnostic.undefined_variable ~span:undef.span ~name:undef.name
            ~candidates ())
        check_result.undefineds
    in

    (* Combine all diagnostics and convert to Error.t *)
    let diagnostics = type_diagnostics @ undefined_diagnostics in
    let type_errors = Tart.Error.of_diagnostics diagnostics in

    (check_result.env, parse_errors @ type_errors)

(** Default command: type-check files.

    Files are processed in order; definitions from earlier files are visible to
    later files. *)
let cmd_check ~format files =
  if files = [] then (
    let err =
      Tart.Error.cli_error ~message:"no input files"
        ~hint:"use --help for usage" ()
    in
    prerr_endline (Tart.Error.to_string err);
    exit 2)
  else
    (* Check each file, accumulating the environment and errors *)
    let initial_env = Tart.Check.default_env () in
    let _, all_errors =
      List.fold_left
        (fun (env, acc_errors) file ->
          let env', file_errors = check_file env file in
          (env', acc_errors @ file_errors))
        (initial_env, []) files
    in
    (* Report errors in the requested format *)
    (match format with
    | Human -> Tart.Error.report all_errors
    | Json -> Tart.Error.report_json all_errors);
    (* Exit code: 0 if no errors, 1 if errors *)
    if all_errors <> [] then exit 1

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
              (fun d -> prerr_endline (Tart.Diagnostic.to_string d))
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
              (fun d -> prerr_endline (Tart.Diagnostic.to_string d))
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
                (fun d -> prerr_endline (Tart.Diagnostic.to_string d))
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

(** Find typings root directory with verbose logging.

    Tries multiple candidate paths and logs each attempt when verbose mode is
    enabled. Returns the first path that exists, or the last candidate if none
    exist (let downstream code handle the error). *)
let find_typings_root ~verbose : string =
  let open Tart.Verbose_log in
  let exe_path = Sys.executable_name in
  let exe_dir = Filename.dirname exe_path in
  verbose_log verbose "Executable: %s" exe_path;
  verbose_log verbose "Typings root candidates:";

  (* List of candidate paths relative to executable directory.
     Order matters: first found wins. *)
  let candidates =
    [
      (* Development via dune exec: _build/install/default/bin -> repo/typings *)
      (* Go up 4 levels from _build/install/default/bin to repo root *)
      Filename.concat
        (Filename.dirname
           (Filename.dirname (Filename.dirname (Filename.dirname exe_dir))))
        "typings/emacs";
      (* Development: typings/emacs in repo root (for direct bin execution) *)
      Filename.concat (Filename.dirname exe_dir) "typings/emacs";
      (* Installed: ../share/tart/typings/emacs relative to bin *)
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
      (* Fall back to installed location even if not found *)
      let fallback = List.hd (List.rev candidates) in
      verbose_log verbose "Using typings root: %s (may not exist)" fallback;
      fallback

(** Detect Emacs version with verbose logging.

    Logs the detection process and fallback chain when verbose mode is enabled.
*)
let detect_emacs_version ~verbose ~typings_root : Tart.Emacs_version.version =
  let open Tart.Verbose_log in
  verbose_log verbose "Detecting Emacs version...";

  (* Try to find emacs binary location *)
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

  (* Detect version *)
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

  (* Log the fallback chain *)
  let fallback_chain = Tart.Search_path.version_fallback_candidates version in
  verbose_log verbose "Version fallback chain: %s"
    (String.concat " -> " fallback_chain);

  (* Log the selected typings version *)
  (match Tart.Search_path.find_typings_dir ~typings_root ~version with
  | Some dir ->
      let selected_version = Filename.basename dir in
      verbose_log verbose "Using typings version: %s" selected_version
  | None -> verbose_log verbose "No typings directory found");

  version

(** Log typings files being loaded.

    Lists all .tart files in the c-core directory and reports signature counts.
*)
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

(** Scan C source files with verbose logging.

    For emacs-coverage: logs scanning progress and per-file counts. *)
let scan_c_source_verbose ~verbose ~src_dir : Tart.C_scanner.c_definition list =
  let open Tart.Verbose_log in
  verbose_log verbose "Scanning C source: %s" src_dir;

  (* Read directory and get C files *)
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

  (* Scan each file and accumulate totals *)
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

(** Coverage subcommand: measure type signature coverage *)
let cmd_coverage ~format ~verbose ~fail_under ~exclude paths =
  let config = { Tart.File_scanner.exclude_patterns = exclude } in
  let files = Tart.File_scanner.scan_paths ~config paths in
  if files = [] then (
    prerr_endline "tart coverage: no .el files found";
    exit 1);

  (* Build search path for signature lookup *)
  let typings_root = find_typings_root ~verbose in
  let version = detect_emacs_version ~verbose ~typings_root in
  log_typings_loading ~verbose ~typings_root ~version;
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

(** Emacs coverage subcommand: measure C layer type coverage *)
let cmd_emacs_coverage ~verbose ~emacs_source ~emacs_version_opt =
  let open Tart.Verbose_log in
  (* Discover Emacs source directory *)
  let source_result = Tart.Emacs_source.discover ~explicit_path:emacs_source in
  match source_result with
  | Tart.Emacs_source.NotFound _ | Tart.Emacs_source.InvalidPath _ ->
      prerr_endline (Tart.Emacs_source.format_error source_result);
      exit 1
  | Tart.Emacs_source.Found { source_dir; version = detected_version } ->
      (* Get typings root first (needed for fallback logging) *)
      let typings_root = find_typings_root ~verbose in

      (* Log version detection *)
      verbose_log verbose "Detecting Emacs version...";
      verbose_log verbose "Emacs source: %s" source_dir;
      verbose_log verbose "Detected version (from source): %s" detected_version;

      (* Use specified version or detected version *)
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

      (* Log the fallback chain *)
      let fallback_chain =
        Tart.Search_path.version_fallback_candidates version
      in
      verbose_log verbose "Version fallback chain: %s"
        (String.concat " -> " fallback_chain);

      (* Log the selected typings version *)
      (match Tart.Search_path.find_typings_dir ~typings_root ~version with
      | Some dir ->
          let selected_version = Filename.basename dir in
          verbose_log verbose "Using typings version: %s" selected_version
      | None -> verbose_log verbose "No typings directory found");

      (* Log typings files being loaded *)
      log_typings_loading ~verbose ~typings_root ~version;

      (* Scan C source files *)
      let src_dir = Filename.concat source_dir "src" in
      let definitions = scan_c_source_verbose ~verbose ~src_dir in

      (* Calculate coverage *)
      let result =
        Tart.Emacs_coverage.calculate_coverage ~source_dir
          ~emacs_version:version_str ~typings_root ~version definitions
      in
      let summary = Tart.Emacs_coverage.summarize result in
      let percentage = Tart.Emacs_coverage.coverage_percentage summary in

      (* Log verbose match summary *)
      if verbose then (
        verbose_log verbose "Matching symbols against typings...";
        verbose_log verbose "Sample matches:";
        (* Show first 5 covered *)
        let covered = Tart.Emacs_coverage.covered_public result in
        List.iteri
          (fun i item ->
            if i < 5 then
              verbose_log verbose "  %s: COVERED (%s:%d)"
                item.Tart.Emacs_coverage.definition.name
                item.Tart.Emacs_coverage.definition.file
                item.Tart.Emacs_coverage.definition.line)
          covered;
        (* Show first 5 uncovered *)
        let uncovered = Tart.Emacs_coverage.uncovered_public result in
        List.iteri
          (fun i item ->
            if i < 5 then
              verbose_log verbose "  %s: UNCOVERED"
                item.Tart.Emacs_coverage.definition.name)
          uncovered;
        verbose_log verbose "Match complete: %d/%d DEFUNs covered (%.1f%%)"
          summary.covered_public summary.total_public percentage);

      (* Print report *)
      print_endline "=== C Layer Coverage ===";
      Printf.printf "Emacs source: %s\n" source_dir;
      Printf.printf "Emacs version: %s\n" version_str;
      Printf.printf "Files scanned: %d\n" result.files_scanned;
      Printf.printf "Total public symbols: %d\n" summary.total_public;
      Printf.printf "Covered: %d (%.1f%%)\n" summary.covered_public percentage;
      Printf.printf "Uncovered: %d\n" summary.uncovered_public;
      print_newline ();

      (* Print private symbols *)
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

      (* Print uncovered public symbols *)
      let uncovered = Tart.Emacs_coverage.uncovered_public result in
      if uncovered <> [] then (
        print_endline "Uncovered public:";
        List.iter
          (fun item ->
            Printf.printf "  %s\n" item.Tart.Emacs_coverage.definition.name)
          uncovered)

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
  tart emacs-coverage [OPTIONS]    Measure Emacs C layer type coverage

Options:
  --version              Print version and exit
  --help, -h             Print this help message
  --format=FORMAT        Output format: human (default), json
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

Emacs coverage options:
  --emacs-source PATH  Path to Emacs source directory
  --emacs-version VER  Use typings for Emacs version VER (e.g., 31.0)

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
      let err =
        Tart.Error.cli_error ~message:"no input files"
          ~hint:"use --help for usage" ()
      in
      prerr_endline (Tart.Error.to_string err);
      exit 2
  | [ "--version" ] | [ "-v" ] -> print_version ()
  | [ "--help" ] | [ "-h" ] -> print_help ()
  (* Subcommands *)
  | "eval" :: rest -> (
      match rest with
      | [] ->
          let err = Tart.Error.cli_error ~message:"missing expression" () in
          prerr_endline (Tart.Error.to_string err);
          exit 2
      | [ "--help" ] | [ "-h" ] ->
          print_endline "Usage: tart eval <EXPR>";
          print_endline "";
          print_endline "Evaluate an Elisp expression and print the result.";
          exit 0
      | [ expr ] -> cmd_eval expr
      | _ ->
          let err =
            Tart.Error.cli_error ~message:"expected single expression" ()
          in
          prerr_endline (Tart.Error.to_string err);
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
            let err =
              Tart.Error.cli_error ~message:"--load requires a file" ()
            in
            prerr_endline (Tart.Error.to_string err);
            exit 2
        | arg :: _ when String.starts_with ~prefix:"-" arg ->
            let err =
              Tart.Error.cli_error
                ~message:(Printf.sprintf "unknown option: %s" arg)
                ~hint:"use --help for available options" ()
            in
            prerr_endline (Tart.Error.to_string err);
            exit 2
        | file :: rest -> (
            match !target_file with
            | None ->
                target_file := Some file;
                parse_expand_args rest
            | Some _ ->
                let err =
                  Tart.Error.cli_error ~message:"only one file allowed" ()
                in
                prerr_endline (Tart.Error.to_string err);
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
            let err = Tart.Error.cli_error ~message:"missing file" () in
            prerr_endline (Tart.Error.to_string err);
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
          let err = Tart.Error.cli_error ~message:"unexpected arguments" () in
          prerr_endline (Tart.Error.to_string err);
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
            let err =
              Tart.Error.cli_error ~message:"invalid log level"
                ~hint:"valid levels: debug, normal, quiet" ()
            in
            prerr_endline (Tart.Error.to_string err);
            exit 2
        | "--log-level" :: [] ->
            let err =
              Tart.Error.cli_error ~message:"--log-level requires an argument"
                ()
            in
            prerr_endline (Tart.Error.to_string err);
            exit 2
        | arg :: _ ->
            let err =
              Tart.Error.cli_error
                ~message:(Printf.sprintf "unknown option: %s" arg)
                ~hint:"use --help for available options" ()
            in
            prerr_endline (Tart.Error.to_string err);
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
        | "--verbose" :: rest | "-v" :: rest ->
            verbose := true;
            parse_coverage_args rest
        | "--format=json" :: rest ->
            format := Tart.Report_format.Json;
            parse_coverage_args rest
        | "--format=human" :: rest ->
            format := Tart.Report_format.Human;
            parse_coverage_args rest
        | arg :: _rest when String.starts_with ~prefix:"--format=" arg ->
            let err =
              Tart.Error.cli_error
                ~message:(Printf.sprintf "invalid format: %s" arg)
                ~hint:"valid formats: human, json" ()
            in
            prerr_endline (Tart.Error.to_string err);
            exit 2
        | arg :: rest when String.starts_with ~prefix:"--fail-under=" arg -> (
            let value = String.sub arg 13 (String.length arg - 13) in
            match int_of_string_opt value with
            | Some n when n >= 0 && n <= 100 ->
                fail_under := Some n;
                parse_coverage_args rest
            | _ ->
                let err =
                  Tart.Error.cli_error
                    ~message:"--fail-under requires a value from 0 to 100" ()
                in
                prerr_endline (Tart.Error.to_string err);
                exit 2)
        | arg :: rest when String.starts_with ~prefix:"--exclude=" arg ->
            let pattern = String.sub arg 10 (String.length arg - 10) in
            exclude := !exclude @ [ pattern ];
            parse_coverage_args rest
        | arg :: _ when String.starts_with ~prefix:"-" arg ->
            let err =
              Tart.Error.cli_error
                ~message:(Printf.sprintf "unknown option: %s" arg)
                ~hint:"use --help for available options" ()
            in
            prerr_endline (Tart.Error.to_string err);
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
        print_endline "  --verbose, -v    Show diagnostic output";
        print_endline "  --fail-under=N   Exit 1 if coverage below N%";
        print_endline "  --exclude=PAT    Exclude files matching pattern";
        exit 0)
      else
        cmd_coverage ~format:!format ~verbose:!verbose ~fail_under:!fail_under
          ~exclude:!exclude !paths
  | "check" :: rest ->
      (* Explicit check subcommand - same as default behavior *)
      let emacs_version = ref None in
      let format = ref Human in
      let show_help = ref false in
      let file_list = ref [] in
      let rec parse_check_args = function
        | [] -> ()
        | "--help" :: _ | "-h" :: _ -> show_help := true
        | "--format=json" :: rest ->
            format := Json;
            parse_check_args rest
        | "--format=human" :: rest ->
            format := Human;
            parse_check_args rest
        | arg :: _ when String.starts_with ~prefix:"--format=" arg ->
            let err =
              Tart.Error.cli_error
                ~message:(Printf.sprintf "invalid format: %s" arg)
                ~hint:"valid formats: human, json" ()
            in
            prerr_endline (Tart.Error.to_string err);
            exit 2
        | "--emacs-version" :: v :: rest ->
            (match Tart.Emacs_version.parse_version v with
            | Some ver -> emacs_version := Some ver
            | None ->
                let err =
                  Tart.Error.cli_error
                    ~message:(Printf.sprintf "invalid Emacs version '%s'" v)
                    ~hint:"expected format: MAJOR.MINOR (e.g., 31.0)" ()
                in
                prerr_endline (Tart.Error.to_string err);
                exit 2);
            parse_check_args rest
        | "--emacs-version" :: [] ->
            let err =
              Tart.Error.cli_error
                ~message:"--emacs-version requires a version argument" ()
            in
            prerr_endline (Tart.Error.to_string err);
            exit 2
        | arg :: _ when String.starts_with ~prefix:"-" arg ->
            let err =
              Tart.Error.cli_error
                ~message:(Printf.sprintf "unknown option: %s" arg)
                ~hint:"use --help for available options" ()
            in
            prerr_endline (Tart.Error.to_string err);
            exit 2
        | file :: rest ->
            file_list := !file_list @ [ file ];
            parse_check_args rest
      in
      parse_check_args rest;
      if !show_help then (
        print_endline "Usage: tart check [OPTIONS] FILE [FILES...]";
        print_endline "";
        print_endline "Type-check Elisp files.";
        print_endline "";
        print_endline "Options:";
        print_endline
          "  --format=FORMAT      Output format: human (default), json";
        print_endline "  --emacs-version VER  Use typings for Emacs version VER";
        exit 0)
      else (
        (* Log the Emacs version being used *)
        (match !emacs_version with
        | Some ver ->
            prerr_endline
              (Printf.sprintf "[tart] Using Emacs version: %s"
                 (Tart.Emacs_version.version_to_string ver))
        | None -> ());
        cmd_check ~format:!format !file_list)
  | "emacs-coverage" :: rest ->
      let emacs_source = ref None in
      let emacs_version = ref None in
      let verbose = ref false in
      let show_help = ref false in
      let rec parse_emacs_coverage_args = function
        | [] -> ()
        | "--help" :: _ | "-h" :: _ -> show_help := true
        | "--verbose" :: rest | "-v" :: rest ->
            verbose := true;
            parse_emacs_coverage_args rest
        | "--emacs-source" :: path :: rest ->
            emacs_source := Some path;
            parse_emacs_coverage_args rest
        | "--emacs-source" :: [] ->
            let err =
              Tart.Error.cli_error ~message:"--emacs-source requires a path" ()
            in
            prerr_endline (Tart.Error.to_string err);
            exit 2
        | "--emacs-version" :: v :: rest ->
            (match Tart.Emacs_version.parse_version v with
            | Some ver -> emacs_version := Some ver
            | None ->
                let err =
                  Tart.Error.cli_error
                    ~message:(Printf.sprintf "invalid Emacs version '%s'" v)
                    ~hint:"expected format: MAJOR.MINOR (e.g., 31.0)" ()
                in
                prerr_endline (Tart.Error.to_string err);
                exit 2);
            parse_emacs_coverage_args rest
        | "--emacs-version" :: [] ->
            let err =
              Tart.Error.cli_error
                ~message:"--emacs-version requires a version argument" ()
            in
            prerr_endline (Tart.Error.to_string err);
            exit 2
        | arg :: _ ->
            let err =
              Tart.Error.cli_error
                ~message:(Printf.sprintf "unknown option: %s" arg)
                ~hint:"use --help for available options" ()
            in
            prerr_endline (Tart.Error.to_string err);
            exit 2
      in
      parse_emacs_coverage_args rest;
      if !show_help then (
        print_endline "Usage: tart emacs-coverage [OPTIONS]";
        print_endline "";
        print_endline "Measure type coverage for Emacs C layer primitives.";
        print_endline "";
        print_endline "Options:";
        print_endline "  --emacs-source PATH  Path to Emacs source directory";
        print_endline "  --emacs-version VER  Use typings for Emacs version VER";
        print_endline "  --verbose, -v        Show diagnostic output";
        exit 0)
      else
        cmd_emacs_coverage ~verbose:!verbose ~emacs_source:!emacs_source
          ~emacs_version_opt:!emacs_version
  (* Default: type-check files *)
  | files ->
      (* Parse check command options *)
      let emacs_version = ref None in
      let format = ref Human in
      let show_help = ref false in
      let show_version = ref false in
      let file_list = ref [] in
      let rec parse_check_args = function
        | [] -> ()
        | "--help" :: _ | "-h" :: _ -> show_help := true
        | "--version" :: _ | "-v" :: _ -> show_version := true
        | "--format=json" :: rest ->
            format := Json;
            parse_check_args rest
        | "--format=human" :: rest ->
            format := Human;
            parse_check_args rest
        | arg :: _ when String.starts_with ~prefix:"--format=" arg ->
            let err =
              Tart.Error.cli_error
                ~message:(Printf.sprintf "invalid format: %s" arg)
                ~hint:"valid formats: human, json" ()
            in
            prerr_endline (Tart.Error.to_string err);
            exit 2
        | "--emacs-version" :: v :: rest ->
            (match Tart.Emacs_version.parse_version v with
            | Some ver -> emacs_version := Some ver
            | None ->
                let err =
                  Tart.Error.cli_error
                    ~message:(Printf.sprintf "invalid Emacs version '%s'" v)
                    ~hint:"expected format: MAJOR.MINOR (e.g., 31.0)" ()
                in
                prerr_endline (Tart.Error.to_string err);
                exit 2);
            parse_check_args rest
        | "--emacs-version" :: [] ->
            let err =
              Tart.Error.cli_error
                ~message:"--emacs-version requires a version argument" ()
            in
            prerr_endline (Tart.Error.to_string err);
            exit 2
        | arg :: _ when String.starts_with ~prefix:"-" arg ->
            let err =
              Tart.Error.cli_error
                ~message:(Printf.sprintf "unknown option: %s" arg)
                ~hint:"use --help for available options" ()
            in
            prerr_endline (Tart.Error.to_string err);
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
        cmd_check ~format:!format !file_list)
