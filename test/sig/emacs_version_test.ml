(** Tests for Emacs version detection *)

module V = Sig.Emacs_version

(* =============================================================================
   Version Parsing
   ============================================================================= *)

let test_parse_version_full () =
  match V.parse_version "31.0.50" with
  | Some v ->
      Alcotest.(check int) "major" 31 v.major;
      Alcotest.(check int) "minor" 0 v.minor;
      Alcotest.(check (option int)) "patch" (Some 50) v.patch
  | None -> Alcotest.fail "parse failed"

let test_parse_version_short () =
  match V.parse_version "30.1" with
  | Some v ->
      Alcotest.(check int) "major" 30 v.major;
      Alcotest.(check int) "minor" 1 v.minor;
      Alcotest.(check (option int)) "patch" None v.patch
  | None -> Alcotest.fail "parse failed"

let test_parse_version_invalid () =
  Alcotest.(check bool) "invalid" true (V.parse_version "foo" = None)

let test_parse_version_empty () =
  Alcotest.(check bool) "empty" true (V.parse_version "" = None)

(* =============================================================================
   Emacs Output Parsing
   ============================================================================= *)

let test_parse_emacs_output () =
  let output = "GNU Emacs 31.0.50\nCopyright...\n" in
  match V.parse_emacs_version_output output with
  | Some v ->
      Alcotest.(check int) "major" 31 v.major;
      Alcotest.(check int) "minor" 0 v.minor;
      Alcotest.(check (option int)) "patch" (Some 50) v.patch
  | None -> Alcotest.fail "parse failed"

let test_parse_emacs_output_29 () =
  let output = "GNU Emacs 29.4\nCopyright...\n" in
  match V.parse_emacs_version_output output with
  | Some v ->
      Alcotest.(check int) "major" 29 v.major;
      Alcotest.(check int) "minor" 4 v.minor;
      Alcotest.(check (option int)) "patch" None v.patch
  | None -> Alcotest.fail "parse failed"

let test_parse_emacs_output_with_build () =
  let output =
    "GNU Emacs 31.0.50 (build 1, aarch64-apple-darwin24.1.0)\nCopyright...\n"
  in
  match V.parse_emacs_version_output output with
  | Some v ->
      Alcotest.(check int) "major" 31 v.major;
      Alcotest.(check int) "minor" 0 v.minor;
      Alcotest.(check (option int)) "patch" (Some 50) v.patch
  | None -> Alcotest.fail "parse failed"

let test_parse_emacs_output_invalid () =
  let output = "Some other program\n" in
  Alcotest.(check bool)
    "invalid" true
    (V.parse_emacs_version_output output = None)

(* =============================================================================
   Version Formatting
   ============================================================================= *)

let test_version_to_string_full () =
  let v = { V.major = 31; minor = 0; patch = Some 50 } in
  Alcotest.(check string) "full" "31.0.50" (V.version_to_string v)

let test_version_to_string_short () =
  let v = { V.major = 30; minor = 1; patch = None } in
  Alcotest.(check string) "short" "30.1" (V.version_to_string v)

let test_version_to_dir () =
  let v = { V.major = 31; minor = 0; patch = Some 50 } in
  Alcotest.(check string) "dir" "31.0" (V.version_to_dir v)

(* =============================================================================
   Version Comparison
   ============================================================================= *)

let test_compare_major () =
  let v1 = { V.major = 30; minor = 0; patch = None } in
  let v2 = { V.major = 31; minor = 0; patch = None } in
  Alcotest.(check bool) "30 < 31" true (V.compare_version v1 v2 < 0)

let test_compare_minor () =
  let v1 = { V.major = 31; minor = 0; patch = None } in
  let v2 = { V.major = 31; minor = 1; patch = None } in
  Alcotest.(check bool) "31.0 < 31.1" true (V.compare_version v1 v2 < 0)

let test_compare_patch () =
  let v1 = { V.major = 31; minor = 0; patch = Some 1 } in
  let v2 = { V.major = 31; minor = 0; patch = Some 50 } in
  Alcotest.(check bool) "31.0.1 < 31.0.50" true (V.compare_version v1 v2 < 0)

let test_compare_equal () =
  let v1 = { V.major = 31; minor = 0; patch = Some 50 } in
  let v2 = { V.major = 31; minor = 0; patch = Some 50 } in
  Alcotest.(check int) "equal" 0 (V.compare_version v1 v2)

(* =============================================================================
   Default Version
   ============================================================================= *)

let test_latest () =
  Alcotest.(check int) "latest major" 31 V.latest.major;
  Alcotest.(check int) "latest minor" 0 V.latest.minor

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "emacs_version"
    [
      ( "parse version",
        [
          Alcotest.test_case "full version" `Quick test_parse_version_full;
          Alcotest.test_case "short version" `Quick test_parse_version_short;
          Alcotest.test_case "invalid" `Quick test_parse_version_invalid;
          Alcotest.test_case "empty" `Quick test_parse_version_empty;
        ] );
      ( "parse emacs output",
        [
          Alcotest.test_case "standard output" `Quick test_parse_emacs_output;
          Alcotest.test_case "emacs 29" `Quick test_parse_emacs_output_29;
          Alcotest.test_case "with build info" `Quick
            test_parse_emacs_output_with_build;
          Alcotest.test_case "invalid output" `Quick
            test_parse_emacs_output_invalid;
        ] );
      ( "formatting",
        [
          Alcotest.test_case "full to string" `Quick test_version_to_string_full;
          Alcotest.test_case "short to string" `Quick
            test_version_to_string_short;
          Alcotest.test_case "to dir" `Quick test_version_to_dir;
        ] );
      ( "comparison",
        [
          Alcotest.test_case "major" `Quick test_compare_major;
          Alcotest.test_case "minor" `Quick test_compare_minor;
          Alcotest.test_case "patch" `Quick test_compare_patch;
          Alcotest.test_case "equal" `Quick test_compare_equal;
        ] );
      ("defaults", [ Alcotest.test_case "latest" `Quick test_latest ]);
    ]
