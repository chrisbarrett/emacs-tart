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

(** {1 Test Suites} *)

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
    ]
