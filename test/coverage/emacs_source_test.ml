(** Tests for Emacs source discovery. *)

open Coverage.Emacs_source

(** {1 Path Validation} *)

let test_is_valid_nonexistent () =
  Alcotest.(check bool)
    "nonexistent path is invalid" false
    (is_valid_emacs_source "/nonexistent/path")

let test_is_valid_file_not_directory () =
  Alcotest.(check bool)
    "file is invalid" false
    (is_valid_emacs_source "dune-project")

let test_is_valid_no_src_subdir () =
  (* A directory without src/ subdir is invalid *)
  Alcotest.(check bool)
    "no src/ subdir is invalid" false
    (is_valid_emacs_source "/tmp")

(** {1 from_path} *)

let test_from_path_nonexistent () =
  match from_path "/nonexistent/path/to/emacs" with
  | InvalidPath msg ->
      Alcotest.(check bool)
        "error mentions path" true
        (String.length msg > 0 && String.sub msg 0 4 = "Path")
  | _ -> Alcotest.fail "expected InvalidPath"

let test_from_path_not_a_directory () =
  match from_path "dune-project" with
  | InvalidPath msg ->
      Alcotest.(check bool)
        "error mentions not a directory" true
        (String.length msg > 0)
  | _ -> Alcotest.fail "expected InvalidPath"

let test_from_path_no_c_files () =
  (* Use /tmp which exists but has no src/*.c *)
  match from_path "/tmp" with
  | InvalidPath msg ->
      Alcotest.(check bool) "error mentions src/*.c" true (String.length msg > 0)
  | _ -> Alcotest.fail "expected InvalidPath for /tmp"

(** {1 discover} *)

let test_discover_explicit_path () =
  (* Using explicit nonexistent path should fail with InvalidPath *)
  match discover ~explicit_path:(Some "/nonexistent/path") with
  | InvalidPath _ -> ()
  | _ -> Alcotest.fail "expected InvalidPath"

let test_discover_auto_detect () =
  (* Auto-detect - may succeed or fail depending on system *)
  let result = discover ~explicit_path:None in
  match result with
  | Found { source_dir; version } ->
      (* If found, validate structure *)
      Alcotest.(check bool)
        "source_dir not empty" true
        (String.length source_dir > 0);
      Alcotest.(check bool) "version not empty" true (String.length version > 0)
  | NotFound _ ->
      (* OK - Emacs source might not be available *)
      ()
  | InvalidPath _ ->
      (* OK - detected path might be invalid *)
      ()

(** {1 Error Formatting} *)

let test_format_error_not_found () =
  let msg = format_error (NotFound "test reason") in
  Alcotest.(check bool)
    "contains Error:" true
    (String.length msg > 0 && String.sub msg 0 6 = "Error:");
  Alcotest.(check bool)
    "contains reason" true
    (try
       let _ = Str.search_forward (Str.regexp_string "test reason") msg 0 in
       true
     with Not_found -> false)

let test_format_error_invalid_path () =
  let msg = format_error (InvalidPath "bad path") in
  Alcotest.(check bool)
    "contains Error:" true
    (String.length msg > 0 && String.sub msg 0 6 = "Error:");
  Alcotest.(check bool)
    "contains path info" true
    (try
       let _ = Str.search_forward (Str.regexp_string "bad path") msg 0 in
       true
     with Not_found -> false)

let test_format_error_found () =
  let msg = format_error (Found { source_dir = "/path"; version = "31.0" }) in
  Alcotest.(check string) "no error for Found" "" msg

(** {1 Auto-Version Integration Tests} *)

(** Create a temporary typings directory tree for testing fallback chain. *)
let with_temp_typings_dir versions f =
  let tmpdir = Filename.temp_file "typings_" "" in
  Sys.remove tmpdir;
  Unix.mkdir tmpdir 0o755;
  let cleanup () =
    let rec remove_dir dir =
      if Sys.is_directory dir then (
        Array.iter
          (fun fn ->
            let path = Filename.concat dir fn in
            if Sys.is_directory path then remove_dir path else Sys.remove path)
          (Sys.readdir dir);
        Unix.rmdir dir)
      else Sys.remove dir
    in
    try remove_dir tmpdir with _ -> ()
  in
  Fun.protect ~finally:cleanup (fun () ->
      List.iter
        (fun ver_str ->
          let ver_dir = Filename.concat tmpdir ver_str in
          Unix.mkdir ver_dir 0o755;
          let c_core_dir = Filename.concat ver_dir "c-core" in
          Unix.mkdir c_core_dir 0o755)
        versions;
      f tmpdir)

let test_detected_version_selects_typings () =
  (* Simulate: Emacs reports version "31.0.50", typings exist for "31.0" *)
  let detected = "31.0.50" in
  match Sig.Emacs_version.parse_version detected with
  | None -> Alcotest.fail "should parse 31.0.50"
  | Some version ->
      with_temp_typings_dir [ "31.0" ] (fun typings_root ->
          match Sig.Search_path.find_typings_dir ~typings_root ~version with
          | Some dir ->
              Alcotest.(check string)
                "falls back to 31.0"
                (Filename.concat typings_root "31.0")
                dir
          | None -> Alcotest.fail "should find 31.0 via fallback")

let test_detected_version_exact_match () =
  (* Simulate: Emacs reports "31.0", typings exist for "31.0" exactly *)
  let detected = "31.0" in
  match Sig.Emacs_version.parse_version detected with
  | None -> Alcotest.fail "should parse 31.0"
  | Some version ->
      with_temp_typings_dir [ "31.0" ] (fun typings_root ->
          match Sig.Search_path.find_typings_dir ~typings_root ~version with
          | Some dir ->
              Alcotest.(check string)
                "selects exact 31.0"
                (Filename.concat typings_root "31.0")
                dir
          | None -> Alcotest.fail "should find exact 31.0")

let test_detected_version_falls_back_to_latest () =
  (* Simulate: Emacs reports "99.0", only "latest" typings exist *)
  let detected = "99.0" in
  match Sig.Emacs_version.parse_version detected with
  | None -> Alcotest.fail "should parse 99.0"
  | Some version ->
      with_temp_typings_dir [ "latest" ] (fun typings_root ->
          match Sig.Search_path.find_typings_dir ~typings_root ~version with
          | Some dir ->
              Alcotest.(check string)
                "falls back to latest"
                (Filename.concat typings_root "latest")
                dir
          | None -> Alcotest.fail "should find latest via fallback")

let test_override_version_ignores_detected () =
  (* Simulate: detected "31.0.50" but user passes --emacs-version 30.1 *)
  let override = "30.1" in
  match Sig.Emacs_version.parse_version override with
  | None -> Alcotest.fail "should parse 30.1"
  | Some version ->
      with_temp_typings_dir [ "30.1"; "31.0" ] (fun typings_root ->
          match Sig.Search_path.find_typings_dir ~typings_root ~version with
          | Some dir ->
              Alcotest.(check string)
                "uses override 30.1"
                (Filename.concat typings_root "30.1")
                dir
          | None -> Alcotest.fail "should find 30.1")

let test_unparseable_version_uses_default () =
  (* Simulate: detected version is "unknown", should use latest as fallback *)
  let detected = "unknown" in
  match Sig.Emacs_version.parse_version detected with
  | Some _ -> Alcotest.fail "should not parse 'unknown'"
  | None ->
      (* When parsing fails, main.ml falls back to Emacs_version.latest *)
      let version = Sig.Emacs_version.latest in
      with_temp_typings_dir [ "latest" ] (fun typings_root ->
          match Sig.Search_path.find_typings_dir ~typings_root ~version with
          | Some dir ->
              Alcotest.(check string)
                "uses latest as default"
                (Filename.concat typings_root "latest")
                dir
          | None -> Alcotest.fail "should find latest")

let test_fallback_chain_order () =
  (* Version 31.0.50 should try 31.0.50 -> 31.0 -> 31 -> latest *)
  let detected = "31.0.50" in
  match Sig.Emacs_version.parse_version detected with
  | None -> Alcotest.fail "should parse"
  | Some version ->
      let candidates = Sig.Search_path.version_fallback_candidates version in
      Alcotest.(check (list string))
        "correct chain"
        [ "31.0.50"; "31.0"; "31"; "latest" ]
        candidates

(** {1 Test Suites} *)

let auto_version_tests =
  [
    Alcotest.test_case "detected version selects typings" `Quick
      test_detected_version_selects_typings;
    Alcotest.test_case "exact version match" `Quick
      test_detected_version_exact_match;
    Alcotest.test_case "falls back to latest" `Quick
      test_detected_version_falls_back_to_latest;
    Alcotest.test_case "override ignores detected" `Quick
      test_override_version_ignores_detected;
    Alcotest.test_case "unparseable uses default" `Quick
      test_unparseable_version_uses_default;
    Alcotest.test_case "fallback chain order" `Quick test_fallback_chain_order;
  ]

let validation_tests =
  [
    Alcotest.test_case "nonexistent" `Quick test_is_valid_nonexistent;
    Alcotest.test_case "file not dir" `Quick test_is_valid_file_not_directory;
    Alcotest.test_case "no src subdir" `Quick test_is_valid_no_src_subdir;
  ]

let from_path_tests =
  [
    Alcotest.test_case "nonexistent" `Quick test_from_path_nonexistent;
    Alcotest.test_case "not a dir" `Quick test_from_path_not_a_directory;
    Alcotest.test_case "no c files" `Quick test_from_path_no_c_files;
  ]

let discover_tests =
  [
    Alcotest.test_case "explicit path" `Quick test_discover_explicit_path;
    Alcotest.test_case "auto detect" `Quick test_discover_auto_detect;
  ]

let error_format_tests =
  [
    Alcotest.test_case "not found" `Quick test_format_error_not_found;
    Alcotest.test_case "invalid path" `Quick test_format_error_invalid_path;
    Alcotest.test_case "found (no error)" `Quick test_format_error_found;
  ]

let () =
  Alcotest.run "emacs_source"
    [
      ("validation", validation_tests);
      ("from_path", from_path_tests);
      ("discover", discover_tests);
      ("error_format", error_format_tests);
      ("auto_version", auto_version_tests);
    ]
