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

(** {1 Version Resolution Tests} *)

(** Mock git ls-remote output with standard Emacs release tags. *)
let mock_tags_output () : (string, resolution_error) result =
  Ok
    "abc1234\trefs/tags/emacs-28.1\n\
     abc1235\trefs/tags/emacs-28.1^{}\n\
     abc1236\trefs/tags/emacs-28.2\n\
     abc1237\trefs/tags/emacs-28.2^{}\n\
     abc1238\trefs/tags/emacs-29.1\n\
     abc1239\trefs/tags/emacs-29.1^{}\n\
     abc1240\trefs/tags/emacs-29.2\n\
     abc1241\trefs/tags/emacs-29.2^{}\n\
     abc1242\trefs/tags/emacs-29.3\n\
     abc1243\trefs/tags/emacs-29.3^{}\n\
     abc1244\trefs/tags/emacs-29.4\n\
     abc1245\trefs/tags/emacs-29.4^{}\n\
     abc1246\trefs/tags/emacs-30.1\n\
     abc1247\trefs/tags/emacs-30.1^{}\n\
     abc1248\trefs/tags/emacs-30.1.1\n\
     abc1249\trefs/tags/emacs-30.1.1^{}\n"

(** {2 is_sha} *)

let test_is_sha_valid_short () =
  Alcotest.(check bool) "7-char hex is SHA" true (is_sha "abc1234")

let test_is_sha_valid_full () =
  Alcotest.(check bool)
    "40-char hex is SHA" true
    (is_sha "abc1234567890abc1234567890abc1234567890a")

let test_is_sha_too_short () =
  Alcotest.(check bool) "6-char hex is not SHA" false (is_sha "abc123")

let test_is_sha_non_hex () =
  Alcotest.(check bool) "non-hex is not SHA" false (is_sha "not_a_sha")

let test_is_sha_mixed () =
  Alcotest.(check bool) "mixed case hex is SHA" true (is_sha "AbCdEf1")

(** {2 is_dev_identifier} *)

let test_is_dev_dev () =
  Alcotest.(check bool) "dev is dev" true (is_dev_identifier "dev")

let test_is_dev_devel () =
  Alcotest.(check bool) "devel is dev" true (is_dev_identifier "devel")

let test_is_dev_git () =
  Alcotest.(check bool) "git is dev" true (is_dev_identifier "git")

let test_is_dev_case_insensitive () =
  Alcotest.(check bool) "DEV is dev" true (is_dev_identifier "DEV")

let test_is_dev_other () =
  Alcotest.(check bool) "main is not dev" false (is_dev_identifier "main")

(** {2 parse_emacs_tag} *)

let test_parse_tag_two_part () =
  match parse_emacs_tag "emacs-29.1" with
  | Some v ->
      Alcotest.(check int) "major" 29 v.major;
      Alcotest.(check int) "minor" 1 v.minor;
      Alcotest.(check (option int)) "no patch" None v.patch
  | None -> Alcotest.fail "should parse emacs-29.1"

let test_parse_tag_three_part () =
  match parse_emacs_tag "emacs-30.1.1" with
  | Some v ->
      Alcotest.(check int) "major" 30 v.major;
      Alcotest.(check int) "minor" 1 v.minor;
      Alcotest.(check (option int)) "patch" (Some 1) v.patch
  | None -> Alcotest.fail "should parse emacs-30.1.1"

let test_parse_tag_invalid () =
  match parse_emacs_tag "v1.0.0" with
  | None -> ()
  | Some _ -> Alcotest.fail "should not parse non-emacs tag"

(** {2 sort_tags_descending} *)

let test_sort_tags () =
  let tags = [ "emacs-28.1"; "emacs-30.1"; "emacs-29.1" ] in
  let sorted = sort_tags_descending tags in
  let tag_names = List.map fst sorted in
  Alcotest.(check (list string))
    "sorted descending"
    [ "emacs-30.1"; "emacs-29.1"; "emacs-28.1" ]
    tag_names

(** {2 list_remote_tags} *)

let test_list_remote_tags_filters_derefs () =
  let result = list_remote_tags ~fetch_tags:mock_tags_output () in
  match result with
  | Ok tags ->
      (* Should not contain any ^{} tags *)
      List.iter
        (fun t ->
          Alcotest.(check bool)
            (Printf.sprintf "%s should not end with ^{}" t)
            false
            (Filename.check_suffix t "^{}"))
        tags
  | Error _ -> Alcotest.fail "should succeed"

let test_list_remote_tags_count () =
  match list_remote_tags ~fetch_tags:mock_tags_output () with
  | Ok tags ->
      (* 8 unique release tags from mock output *)
      Alcotest.(check int) "8 release tags" 8 (List.length tags)
  | Error _ -> Alcotest.fail "should succeed"

(** {2 resolve_version} *)

let test_resolve_dev () =
  match resolve_version ~fetch_tags:mock_tags_output "dev" with
  | Ok Dev -> ()
  | Ok _ -> Alcotest.fail "expected Dev"
  | Error _ -> Alcotest.fail "should succeed"

let test_resolve_devel () =
  match resolve_version ~fetch_tags:mock_tags_output "devel" with
  | Ok Dev -> ()
  | Ok _ -> Alcotest.fail "expected Dev"
  | Error _ -> Alcotest.fail "should succeed"

let test_resolve_git () =
  match resolve_version ~fetch_tags:mock_tags_output "git" with
  | Ok Dev -> ()
  | Ok _ -> Alcotest.fail "expected Dev"
  | Error _ -> Alcotest.fail "should succeed"

let test_resolve_sha () =
  match resolve_version ~fetch_tags:mock_tags_output "abc1234" with
  | Ok (Commit sha) -> Alcotest.(check string) "sha preserved" "abc1234" sha
  | Ok _ -> Alcotest.fail "expected Commit"
  | Error _ -> Alcotest.fail "should succeed"

let test_resolve_latest () =
  match resolve_version ~fetch_tags:mock_tags_output "latest" with
  | Ok (Release { tag; version }) ->
      Alcotest.(check string) "latest tag" "emacs-30.1.1" tag;
      Alcotest.(check int) "major" 30 version.major;
      Alcotest.(check int) "minor" 1 version.minor;
      Alcotest.(check (option int)) "patch" (Some 1) version.patch
  | Ok _ -> Alcotest.fail "expected Release"
  | Error _ -> Alcotest.fail "should succeed"

let test_resolve_major_shorthand () =
  (* "29" should resolve to latest 29.x release *)
  match resolve_version ~fetch_tags:mock_tags_output "29" with
  | Ok (Release { tag; version }) ->
      Alcotest.(check string) "latest 29.x tag" "emacs-29.4" tag;
      Alcotest.(check int) "major" 29 version.major;
      Alcotest.(check int) "minor" 4 version.minor
  | Ok _ -> Alcotest.fail "expected Release"
  | Error _ -> Alcotest.fail "should succeed"

let test_resolve_major_minor () =
  (* "29.1" should resolve to exact tag *)
  match resolve_version ~fetch_tags:mock_tags_output "29.1" with
  | Ok (Release { tag; version }) ->
      Alcotest.(check string) "exact tag" "emacs-29.1" tag;
      Alcotest.(check int) "major" 29 version.major;
      Alcotest.(check int) "minor" 1 version.minor
  | Ok _ -> Alcotest.fail "expected Release"
  | Error _ -> Alcotest.fail "should succeed"

let test_resolve_full_version () =
  (* "30.1.1" should resolve to exact tag *)
  match resolve_version ~fetch_tags:mock_tags_output "30.1.1" with
  | Ok (Release { tag; version }) ->
      Alcotest.(check string) "exact tag" "emacs-30.1.1" tag;
      Alcotest.(check int) "major" 30 version.major;
      Alcotest.(check int) "minor" 1 version.minor;
      Alcotest.(check (option int)) "patch" (Some 1) version.patch
  | Ok _ -> Alcotest.fail "expected Release"
  | Error _ -> Alcotest.fail "should succeed"

let test_resolve_nonexistent_major () =
  (* "99" should fail with recent tags *)
  match resolve_version ~fetch_tags:mock_tags_output "99" with
  | Ok _ -> Alcotest.fail "should fail"
  | Error (No_matching_tag msg) ->
      Alcotest.(check bool)
        "mentions Emacs 99" true
        (try
           let _ = Str.search_forward (Str.regexp_string "99") msg 0 in
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "includes recent tags" true
        (try
           let _ = Str.search_forward (Str.regexp_string "emacs-") msg 0 in
           true
         with Not_found -> false)
  | Error (Git_error _) -> Alcotest.fail "expected No_matching_tag"

let test_resolve_nonexistent_exact () =
  (* "29.99" should fail *)
  match resolve_version ~fetch_tags:mock_tags_output "29.99" with
  | Ok _ -> Alcotest.fail "should fail"
  | Error (No_matching_tag msg) ->
      Alcotest.(check bool)
        "mentions tag" true
        (try
           let _ = Str.search_forward (Str.regexp_string "emacs-29.99") msg 0 in
           true
         with Not_found -> false)
  | Error (Git_error _) -> Alcotest.fail "expected No_matching_tag"

let test_resolve_invalid_specifier () =
  (* "abc" is not a SHA (too short), not a dev identifier, not a version *)
  match resolve_version ~fetch_tags:mock_tags_output "abc" with
  | Ok _ -> Alcotest.fail "should fail"
  | Error (No_matching_tag _) -> ()
  | Error (Git_error _) -> Alcotest.fail "expected No_matching_tag"

let test_resolve_git_error () =
  let fail_tags () = Error (Git_error "network unreachable") in
  match resolve_version ~fetch_tags:fail_tags "29" with
  | Ok _ -> Alcotest.fail "should fail"
  | Error (Git_error msg) ->
      Alcotest.(check bool)
        "contains error" true
        (try
           let _ =
             Str.search_forward (Str.regexp_string "network unreachable") msg 0
           in
           true
         with Not_found -> false)
  | Error (No_matching_tag _) -> Alcotest.fail "expected Git_error"

(** {2 format_resolution_error} *)

let test_format_no_matching_tag () =
  let msg = format_resolution_error (No_matching_tag "test reason") in
  Alcotest.(check bool)
    "contains Error:" true
    (String.length msg > 0 && String.sub msg 0 6 = "Error:");
  Alcotest.(check bool)
    "contains usage" true
    (try
       let _ = Str.search_forward (Str.regexp_string "--emacs-version") msg 0 in
       true
     with Not_found -> false)

let test_format_git_error () =
  let msg = format_resolution_error (Git_error "connection refused") in
  Alcotest.(check bool)
    "contains Error:" true
    (String.length msg > 0 && String.sub msg 0 6 = "Error:")

(** {2 resolved_version_to_string} *)

let test_resolved_release_to_string () =
  let rv =
    Release
      { tag = "emacs-29.1"; version = { major = 29; minor = 1; patch = None } }
  in
  let s = resolved_version_to_string rv in
  Alcotest.(check bool)
    "contains tag" true
    (try
       let _ = Str.search_forward (Str.regexp_string "emacs-29.1") s 0 in
       true
     with Not_found -> false)

let test_resolved_dev_to_string () =
  let s = resolved_version_to_string Dev in
  Alcotest.(check bool)
    "contains main" true
    (try
       let _ = Str.search_forward (Str.regexp_string "main") s 0 in
       true
     with Not_found -> false)

let test_resolved_commit_to_string () =
  let s = resolved_version_to_string (Commit "abc1234") in
  Alcotest.(check bool)
    "contains sha" true
    (try
       let _ = Str.search_forward (Str.regexp_string "abc1234") s 0 in
       true
     with Not_found -> false)

(** {1 Test Suites} *)

let is_sha_tests =
  [
    Alcotest.test_case "valid short SHA" `Quick test_is_sha_valid_short;
    Alcotest.test_case "valid full SHA" `Quick test_is_sha_valid_full;
    Alcotest.test_case "too short" `Quick test_is_sha_too_short;
    Alcotest.test_case "non-hex" `Quick test_is_sha_non_hex;
    Alcotest.test_case "mixed case" `Quick test_is_sha_mixed;
  ]

let dev_identifier_tests =
  [
    Alcotest.test_case "dev" `Quick test_is_dev_dev;
    Alcotest.test_case "devel" `Quick test_is_dev_devel;
    Alcotest.test_case "git" `Quick test_is_dev_git;
    Alcotest.test_case "case insensitive" `Quick test_is_dev_case_insensitive;
    Alcotest.test_case "other" `Quick test_is_dev_other;
  ]

let parse_tag_tests =
  [
    Alcotest.test_case "two-part tag" `Quick test_parse_tag_two_part;
    Alcotest.test_case "three-part tag" `Quick test_parse_tag_three_part;
    Alcotest.test_case "invalid tag" `Quick test_parse_tag_invalid;
  ]

let sort_tags_tests =
  [ Alcotest.test_case "descending order" `Quick test_sort_tags ]

let list_tags_tests =
  [
    Alcotest.test_case "filters derefs" `Quick
      test_list_remote_tags_filters_derefs;
    Alcotest.test_case "correct count" `Quick test_list_remote_tags_count;
  ]

let resolve_version_tests =
  [
    Alcotest.test_case "dev identifier" `Quick test_resolve_dev;
    Alcotest.test_case "devel identifier" `Quick test_resolve_devel;
    Alcotest.test_case "git identifier" `Quick test_resolve_git;
    Alcotest.test_case "SHA" `Quick test_resolve_sha;
    Alcotest.test_case "latest" `Quick test_resolve_latest;
    Alcotest.test_case "major shorthand" `Quick test_resolve_major_shorthand;
    Alcotest.test_case "major.minor" `Quick test_resolve_major_minor;
    Alcotest.test_case "full version" `Quick test_resolve_full_version;
    Alcotest.test_case "nonexistent major" `Quick test_resolve_nonexistent_major;
    Alcotest.test_case "nonexistent exact" `Quick test_resolve_nonexistent_exact;
    Alcotest.test_case "invalid specifier" `Quick test_resolve_invalid_specifier;
    Alcotest.test_case "git error propagation" `Quick test_resolve_git_error;
  ]

let resolution_error_tests =
  [
    Alcotest.test_case "no matching tag" `Quick test_format_no_matching_tag;
    Alcotest.test_case "git error" `Quick test_format_git_error;
  ]

let resolved_to_string_tests =
  [
    Alcotest.test_case "release" `Quick test_resolved_release_to_string;
    Alcotest.test_case "dev" `Quick test_resolved_dev_to_string;
    Alcotest.test_case "commit" `Quick test_resolved_commit_to_string;
  ]

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

(** {1 Source Acquisition Tests} *)

(** Helper: create a temp directory for cache testing. *)
let with_temp_cache_root f =
  let tmpdir = Filename.temp_file "cache_" "" in
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
  Fun.protect ~finally:cleanup (fun () -> f tmpdir)

(** Mock clone that creates a dummy source dir at dest. *)
let mock_clone_ok ~branch:_ ~dest =
  (try Unix.mkdir dest 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let src_dir = Filename.concat dest "src" in
  Unix.mkdir src_dir 0o755;
  let oc = open_out (Filename.concat src_dir "main.c") in
  output_string oc "int main() {}";
  close_out oc;
  Ok "cloned"

(** Mock fetch that always succeeds. *)
let mock_fetch_ok ~sha:_ ~cwd:_ = Ok "fetched"

(** Mock download that always fails. *)
let mock_download_fail ~url:_ ~dest:_ = Error "404 not found"

(** Mock clone that always fails. *)
let mock_clone_fail ~branch:_ ~dest:_ = Error "network unreachable"

(** Mock fetch that always fails. *)
let mock_fetch_fail ~sha:_ ~cwd:_ = Error "fetch error"

(** {2 cache_key_of_resolved} *)

let test_cache_key_release () =
  let rv =
    Release
      { tag = "emacs-29.4"; version = { major = 29; minor = 4; patch = None } }
  in
  Alcotest.(check string) "release key" "29.4" (cache_key_of_resolved rv)

let test_cache_key_release_patch () =
  let rv =
    Release
      {
        tag = "emacs-30.1.1";
        version = { major = 30; minor = 1; patch = Some 1 };
      }
  in
  Alcotest.(check string)
    "release key with patch" "30.1.1" (cache_key_of_resolved rv)

let test_cache_key_dev () =
  Alcotest.(check string) "dev key" "dev" (cache_key_of_resolved Dev)

let test_cache_key_commit () =
  Alcotest.(check string)
    "commit key" "abc1234"
    (cache_key_of_resolved (Commit "abc1234"))

(** {2 sources_cache_dir} *)

let test_sources_cache_dir () =
  let dir = sources_cache_dir () in
  (* Should end with emacs-sources *)
  Alcotest.(check bool)
    "ends with emacs-sources" true
    (Filename.basename dir = "emacs-sources")

(** {2 acquire_source — Release} *)

let test_acquire_release_tarball () =
  (* Mock: download succeeds → tarball path used *)
  with_temp_cache_root (fun cache_root ->
      let rv =
        Release
          {
            tag = "emacs-29.4";
            version = { major = 29; minor = 4; patch = None };
          }
      in
      (* We need a mock that creates the extracted dir structure, not just the tarball *)
      let run_download ~url:_ ~dest =
        (* Create the dest file so tarball "download" succeeds *)
        let oc = open_out dest in
        output_string oc "data";
        close_out oc;
        Ok "ok"
      in
      (* tar will fail on our fake data, but that exercises the fallback *)
      (* Instead, use a clone fallback approach by having download fail *)
      ignore run_download;
      (* Test with download failing → clone fallback *)
      let result =
        acquire_source ~run_download:mock_download_fail ~run_clone:mock_clone_ok
          ~cache_root rv
      in
      match result with
      | Ok path ->
          Alcotest.(check bool)
            "path contains 29.4" true
            (try
               let _ = Str.search_forward (Str.regexp_string "29.4") path 0 in
               true
             with Not_found -> false);
          Alcotest.(check bool) "dir exists" true (Sys.file_exists path)
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "expected Ok, got %s"
               (acquisition_error_to_string e)))

let test_acquire_release_cache_hit () =
  with_temp_cache_root (fun cache_root ->
      let rv =
        Release
          {
            tag = "emacs-29.4";
            version = { major = 29; minor = 4; patch = None };
          }
      in
      (* Pre-create the cache directory *)
      let final_dir = Filename.concat cache_root "29.4" in
      Unix.mkdir final_dir 0o755;
      (* Even with failing mocks, should return cache hit *)
      let result =
        acquire_source ~run_download:mock_download_fail
          ~run_clone:mock_clone_fail ~cache_root rv
      in
      match result with
      | Ok path -> Alcotest.(check string) "cache hit path" final_dir path
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "expected cache hit, got %s"
               (acquisition_error_to_string e)))

let test_acquire_release_clone_fallback () =
  with_temp_cache_root (fun cache_root ->
      let rv =
        Release
          {
            tag = "emacs-29.4";
            version = { major = 29; minor = 4; patch = None };
          }
      in
      (* Download fails → falls back to clone *)
      let result =
        acquire_source ~run_download:mock_download_fail ~run_clone:mock_clone_ok
          ~cache_root rv
      in
      match result with
      | Ok path ->
          Alcotest.(check bool) "path exists" true (Sys.file_exists path);
          (* Should have src/main.c from mock_clone_ok *)
          Alcotest.(check bool)
            "has src dir" true
            (Sys.file_exists (Filename.concat path "src"))
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "expected Ok, got %s"
               (acquisition_error_to_string e)))

let test_acquire_release_both_fail () =
  with_temp_cache_root (fun cache_root ->
      let rv =
        Release
          {
            tag = "emacs-29.4";
            version = { major = 29; minor = 4; patch = None };
          }
      in
      let result =
        acquire_source ~run_download:mock_download_fail
          ~run_clone:mock_clone_fail ~cache_root rv
      in
      match result with
      | Ok _ -> Alcotest.fail "expected error"
      | Error (Clone_failed _) -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "expected Clone_failed, got %s"
               (acquisition_error_to_string e)))

(** {2 acquire_source — Dev} *)

let test_acquire_dev_fresh () =
  with_temp_cache_root (fun cache_root ->
      let result = acquire_source ~run_clone:mock_clone_ok ~cache_root Dev in
      match result with
      | Ok path ->
          Alcotest.(check bool) "path exists" true (Sys.file_exists path);
          Alcotest.(check bool)
            "path ends with dev" true
            (Filename.basename path = "dev")
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "expected Ok, got %s"
               (acquisition_error_to_string e)))

let test_acquire_dev_update () =
  with_temp_cache_root (fun cache_root ->
      (* Pre-create dev dir as a "clone" with .git *)
      let dev_dir = Filename.concat cache_root "dev" in
      Unix.mkdir dev_dir 0o755;
      let git_dir = Filename.concat dev_dir ".git" in
      Unix.mkdir git_dir 0o755;
      let result = acquire_source ~run_fetch:mock_fetch_ok ~cache_root Dev in
      match result with
      | Ok path -> Alcotest.(check string) "reuses dir" dev_dir path
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "expected Ok, got %s"
               (acquisition_error_to_string e)))

let test_acquire_dev_clone_fail () =
  with_temp_cache_root (fun cache_root ->
      let result = acquire_source ~run_clone:mock_clone_fail ~cache_root Dev in
      match result with
      | Ok _ -> Alcotest.fail "expected error"
      | Error (Clone_failed _) -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "expected Clone_failed, got %s"
               (acquisition_error_to_string e)))

(** {2 acquire_source — Commit} *)

let test_acquire_commit_fresh () =
  with_temp_cache_root (fun cache_root ->
      let result =
        acquire_source ~run_clone:mock_clone_ok ~run_fetch:mock_fetch_ok
          ~cache_root (Commit "abc1234def")
      in
      match result with
      | Ok path ->
          Alcotest.(check bool) "path exists" true (Sys.file_exists path);
          Alcotest.(check bool)
            "path ends with sha" true
            (Filename.basename path = "abc1234def")
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "expected Ok, got %s"
               (acquisition_error_to_string e)))

let test_acquire_commit_cache_hit () =
  with_temp_cache_root (fun cache_root ->
      (* Pre-create the SHA dir *)
      let sha_dir = Filename.concat cache_root "abc1234" in
      Unix.mkdir sha_dir 0o755;
      let result =
        acquire_source ~run_clone:mock_clone_fail ~run_fetch:mock_fetch_fail
          ~cache_root (Commit "abc1234")
      in
      match result with
      | Ok path -> Alcotest.(check string) "cache hit" sha_dir path
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "expected cache hit, got %s"
               (acquisition_error_to_string e)))

let test_acquire_commit_clone_fail () =
  with_temp_cache_root (fun cache_root ->
      let result =
        acquire_source ~run_clone:mock_clone_fail ~cache_root (Commit "abc1234")
      in
      match result with
      | Ok _ -> Alcotest.fail "expected error"
      | Error (Clone_failed _) -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "expected Clone_failed, got %s"
               (acquisition_error_to_string e)))

let test_acquire_commit_fetch_fail () =
  with_temp_cache_root (fun cache_root ->
      let result =
        acquire_source ~run_clone:mock_clone_ok ~run_fetch:mock_fetch_fail
          ~cache_root (Commit "abc1234")
      in
      match result with
      | Ok _ -> Alcotest.fail "expected error"
      | Error (Fetch_failed _) -> ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "expected Fetch_failed, got %s"
               (acquisition_error_to_string e)))

(** {2 Atomic writes} *)

let test_acquire_atomic_no_partial () =
  with_temp_cache_root (fun cache_root ->
      (* Clone succeeds but creates dir, then we verify no .tmp dirs remain *)
      let rv =
        Release
          {
            tag = "emacs-29.4";
            version = { major = 29; minor = 4; patch = None };
          }
      in
      let _result =
        acquire_source ~run_download:mock_download_fail ~run_clone:mock_clone_ok
          ~cache_root rv
      in
      (* No .tmp directories should remain *)
      let entries = try Array.to_list (Sys.readdir cache_root) with _ -> [] in
      let tmp_dirs =
        List.filter
          (fun e -> String.length e > 4 && String.sub e 0 4 = ".tmp")
          entries
      in
      Alcotest.(check int) "no .tmp dirs remain" 0 (List.length tmp_dirs))

let test_acquire_failure_no_partial () =
  with_temp_cache_root (fun cache_root ->
      let rv =
        Release
          {
            tag = "emacs-29.4";
            version = { major = 29; minor = 4; patch = None };
          }
      in
      let _result =
        acquire_source ~run_download:mock_download_fail
          ~run_clone:mock_clone_fail ~cache_root rv
      in
      (* No .tmp dirs or final dir should remain *)
      let entries = try Array.to_list (Sys.readdir cache_root) with _ -> [] in
      Alcotest.(check int)
        "no dirs remain after failure" 0 (List.length entries))

(** {2 acquisition_error_to_string} *)

let test_acquisition_error_download () =
  let s = acquisition_error_to_string (Download_failed "404") in
  Alcotest.(check bool)
    "contains Download" true
    (try
       let _ = Str.search_forward (Str.regexp_string "Download") s 0 in
       true
     with Not_found -> false)

let test_acquisition_error_clone () =
  let s = acquisition_error_to_string (Clone_failed "timeout") in
  Alcotest.(check bool)
    "contains Clone" true
    (try
       let _ = Str.search_forward (Str.regexp_string "Clone") s 0 in
       true
     with Not_found -> false)

let test_acquisition_error_fetch () =
  let s = acquisition_error_to_string (Fetch_failed "refused") in
  Alcotest.(check bool)
    "contains Fetch" true
    (try
       let _ = Str.search_forward (Str.regexp_string "Fetch") s 0 in
       true
     with Not_found -> false)

let test_acquisition_error_cache () =
  let s = acquisition_error_to_string (Cache_error "permission denied") in
  Alcotest.(check bool)
    "contains Cache" true
    (try
       let _ = Str.search_forward (Str.regexp_string "Cache") s 0 in
       true
     with Not_found -> false)

(** {2 Test suites for source acquisition} *)

let cache_key_tests =
  [
    Alcotest.test_case "release key" `Quick test_cache_key_release;
    Alcotest.test_case "release key with patch" `Quick
      test_cache_key_release_patch;
    Alcotest.test_case "dev key" `Quick test_cache_key_dev;
    Alcotest.test_case "commit key" `Quick test_cache_key_commit;
  ]

let sources_dir_tests =
  [ Alcotest.test_case "ends with emacs-sources" `Quick test_sources_cache_dir ]

let acquire_release_tests =
  [
    Alcotest.test_case "tarball fallback to clone" `Quick
      test_acquire_release_tarball;
    Alcotest.test_case "cache hit" `Quick test_acquire_release_cache_hit;
    Alcotest.test_case "clone fallback" `Quick
      test_acquire_release_clone_fallback;
    Alcotest.test_case "both fail" `Quick test_acquire_release_both_fail;
  ]

let acquire_dev_tests =
  [
    Alcotest.test_case "fresh clone" `Quick test_acquire_dev_fresh;
    Alcotest.test_case "update existing" `Quick test_acquire_dev_update;
    Alcotest.test_case "clone fails" `Quick test_acquire_dev_clone_fail;
  ]

let acquire_commit_tests =
  [
    Alcotest.test_case "fresh clone+fetch" `Quick test_acquire_commit_fresh;
    Alcotest.test_case "cache hit" `Quick test_acquire_commit_cache_hit;
    Alcotest.test_case "clone fails" `Quick test_acquire_commit_clone_fail;
    Alcotest.test_case "fetch fails" `Quick test_acquire_commit_fetch_fail;
  ]

let atomic_write_tests =
  [
    Alcotest.test_case "success leaves no tmp" `Quick
      test_acquire_atomic_no_partial;
    Alcotest.test_case "failure leaves no partial" `Quick
      test_acquire_failure_no_partial;
  ]

let acquisition_error_tests =
  [
    Alcotest.test_case "download error" `Quick test_acquisition_error_download;
    Alcotest.test_case "clone error" `Quick test_acquisition_error_clone;
    Alcotest.test_case "fetch error" `Quick test_acquisition_error_fetch;
    Alcotest.test_case "cache error" `Quick test_acquisition_error_cache;
  ]

let () =
  Alcotest.run "emacs_source"
    [
      ("validation", validation_tests);
      ("from_path", from_path_tests);
      ("discover", discover_tests);
      ("error_format", error_format_tests);
      ("auto_version", auto_version_tests);
      ("is_sha", is_sha_tests);
      ("dev_identifier", dev_identifier_tests);
      ("parse_emacs_tag", parse_tag_tests);
      ("sort_tags", sort_tags_tests);
      ("list_remote_tags", list_tags_tests);
      ("resolve_version", resolve_version_tests);
      ("resolution_error_format", resolution_error_tests);
      ("resolved_to_string", resolved_to_string_tests);
      ("cache_key", cache_key_tests);
      ("sources_cache_dir", sources_dir_tests);
      ("acquire_release", acquire_release_tests);
      ("acquire_dev", acquire_dev_tests);
      ("acquire_commit", acquire_commit_tests);
      ("atomic_writes", atomic_write_tests);
      ("acquisition_error_format", acquisition_error_tests);
    ]
