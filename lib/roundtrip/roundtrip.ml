(** Round-trip test harness.

    Verifies parser/printer consistency by checking that
    parse(print(parse(file))) = parse(file), and optionally that Emacs's native
    reader agrees with tart's output. *)

module Read = Syntax.Read
module Print = Syntax.Print
module Sexp = Syntax.Sexp
module Compare = Oracle.Compare
module Log = Tart_log.Log

(** {1 Result Types} *)

type result =
  | Pass
  | Cached
  | Parse_error of { path : string; error : string }
  | Mismatch of {
      path : string;
      form_index : int;
      expected : string;
      actual : string;
      diff : string;
    }
  | Emacs_mismatch of {
      path : string;
      tart_output : string;
      emacs_output : string;
    }

type summary = {
  total : int;
  passed : int;
  failed : int;
  cached : int;
  failures : (string * result) list;
}

(** {1 Diff} *)

let make_diff ~expected ~actual =
  let expected_lines = String.split_on_char '\n' expected in
  let actual_lines = String.split_on_char '\n' actual in
  let buf = Buffer.create 256 in
  let max_len = max (List.length expected_lines) (List.length actual_lines) in
  for i = 0 to max_len - 1 do
    let exp =
      if i < List.length expected_lines then List.nth expected_lines i else ""
    in
    let act =
      if i < List.length actual_lines then List.nth actual_lines i else ""
    in
    if String.equal exp act then
      Buffer.add_string buf (Printf.sprintf "  %s\n" exp)
    else begin
      Buffer.add_string buf (Printf.sprintf "- %s\n" exp);
      Buffer.add_string buf (Printf.sprintf "+ %s\n" act)
    end
  done;
  Buffer.contents buf

(** {1 Per-File Checks} *)

let check_file path =
  let result = Read.parse_file path in
  match result.errors with
  | err :: _ ->
      let msg =
        Printf.sprintf "%s (line %d)" err.message err.span.start_pos.line
      in
      Parse_error { path; error = msg }
  | [] ->
      let rec check_forms i = function
        | [] -> Pass
        | sexp :: rest -> (
            let printed = Print.to_string sexp in
            let reparse = Read.parse_one printed in
            match reparse with
            | Error msg ->
                let diff =
                  make_diff ~expected:(Sexp.to_string sexp)
                    ~actual:("REPARSE ERROR: " ^ msg)
                in
                Mismatch
                  {
                    path;
                    form_index = i;
                    expected = Print.to_string sexp;
                    actual = "REPARSE ERROR: " ^ msg;
                    diff;
                  }
            | Ok reparsed ->
                let expected = Print.to_string sexp in
                let actual = Print.to_string reparsed in
                if String.equal expected actual then check_forms (i + 1) rest
                else
                  let diff = make_diff ~expected ~actual in
                  Mismatch { path; form_index = i; expected; actual; diff })
      in
      check_forms 0 result.sexps

let check_file_with_emacs ?(timeout_ms = 5000) path =
  let results = Compare.compare_file ~timeout_ms path in
  let rec find_mismatch = function
    | [] -> Pass
    | Compare.Match :: rest -> find_mismatch rest
    | Compare.Mismatch { tart_output; emacs_output } :: _ ->
        Emacs_mismatch { path; tart_output; emacs_output }
    | Compare.Tart_error err :: _ -> Parse_error { path; error = err.message }
    | Compare.Emacs_error err :: _ ->
        let msg =
          match err with
          | Oracle.Emacs_reader.Read_error { message; _ } ->
              "read error: " ^ message
          | Emacs_not_found -> "emacs not found"
          | Emacs_failed { exit_code; stderr } ->
              Printf.sprintf "emacs failed (exit %d): %s" exit_code stderr
          | Timeout { timeout_ms = ms } ->
              Printf.sprintf "emacs timeout (%dms)" ms
        in
        Parse_error { path; error = "Emacs: " ^ msg }
  in
  find_mismatch results

(** {1 Cached Checks} *)

let cache_key path =
  let binary = Cache.Content_cache.binary_path () in
  Cache.Content_cache.compute_key ~binary ~input:path

let check_file_cached ?(no_cache = false) path =
  if not no_cache then begin
    let key = cache_key path in
    match Cache.Content_cache.retrieve ~key with
    | Some "roundtrip:pass" ->
        Log.debug "cache hit: %s" path;
        Cached
    | _ ->
        let result = check_file path in
        (match result with
        | Pass -> Cache.Content_cache.store ~key ~data:"roundtrip:pass"
        | _ -> ());
        result
  end
  else check_file path

(** {1 Corpus Run} *)

let run_corpus ?(no_cache = false) ?(with_emacs = false) ?(timeout_ms = 5000)
    files =
  let total = List.length files in
  let passed = ref 0 in
  let failed = ref 0 in
  let cached = ref 0 in
  let failures = ref [] in
  List.iter
    (fun path ->
      let result = check_file_cached ~no_cache path in
      (match result with
      | Pass -> incr passed
      | Cached -> incr cached
      | Parse_error _ | Mismatch _ | Emacs_mismatch _ ->
          incr failed;
          failures := (path, result) :: !failures);
      (* Oracle check if requested and structural check passed *)
      if with_emacs then
        match result with
        | Pass | Cached -> (
            let emacs_result = check_file_with_emacs ~timeout_ms path in
            match emacs_result with
            | Pass | Cached -> ()
            | _ ->
                (* Demote a pass/cached to failure *)
                (match result with
                | Pass -> decr passed
                | Cached -> decr cached
                | _ -> ());
                incr failed;
                failures := (path, emacs_result) :: !failures)
        | _ -> ())
    files;
  {
    total;
    passed = !passed;
    failed = !failed;
    cached = !cached;
    failures = List.rev !failures;
  }

(** {1 Summary} *)

let summary_to_string s =
  Printf.sprintf "%d total, %d passed, %d failed, %d cached" s.total s.passed
    s.failed s.cached
