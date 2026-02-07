(** Tests for Package-Requires header parsing *)

module P = Sig.Package_header
module V = Sig.Emacs_version

let check_version msg expected actual =
  match actual with
  | Some v ->
      Alcotest.(check int) (msg ^ " major") expected.V.major v.V.major;
      Alcotest.(check int) (msg ^ " minor") expected.V.minor v.V.minor
  | None -> Alcotest.fail (msg ^ ": expected Some, got None")

let check_none msg actual = Alcotest.(check bool) msg true (actual = None)

(* =============================================================================
   parse_package_requires — standard formats
   ============================================================================= *)

let test_standard () =
  let content =
    {|;;; my-pkg.el --- Description  -*- lexical-binding: t; -*-

;; Author: Someone
;; Package-Requires: ((emacs "29.1"))
;; Version: 1.0.0
|}
  in
  check_version "standard"
    { major = 29; minor = 1; patch = None }
    (P.parse_package_requires content)

let test_multiple_deps () =
  let content =
    {|;;; my-pkg.el --- Description  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "28.1") (seq "2.24"))
|}
  in
  check_version "multiple deps"
    { major = 28; minor = 1; patch = None }
    (P.parse_package_requires content)

let test_emacs_not_first () =
  let content =
    {|;; Package-Requires: ((dash "2.19") (emacs "27.1") (s "1.12"))
|}
  in
  check_version "emacs not first"
    { major = 27; minor = 1; patch = None }
    (P.parse_package_requires content)

let test_three_semicolons () =
  let content = {|;;; Package-Requires: ((emacs "30.1"))
|} in
  check_version "three semicolons"
    { major = 30; minor = 1; patch = None }
    (P.parse_package_requires content)

let test_with_patch () =
  let content = {|;; Package-Requires: ((emacs "31.0.50"))
|} in
  check_version "with patch"
    { major = 31; minor = 0; patch = Some 50 }
    (P.parse_package_requires content)

let test_leading_spaces () =
  let content = {|  ;; Package-Requires: ((emacs "29.1"))
|} in
  check_version "leading spaces"
    { major = 29; minor = 1; patch = None }
    (P.parse_package_requires content)

(* =============================================================================
   parse_package_requires — missing/malformed
   ============================================================================= *)

let test_missing_header () =
  let content =
    {|;;; my-pkg.el --- Description  -*- lexical-binding: t; -*-

;; Author: Someone
;; Version: 1.0.0

(provide 'my-pkg)
|}
  in
  check_none "missing header" (P.parse_package_requires content)

let test_malformed_version () =
  let content = {|;; Package-Requires: ((emacs "not-a-version"))
|} in
  check_none "malformed version" (P.parse_package_requires content)

let test_no_emacs_dep () =
  let content = {|;; Package-Requires: ((seq "2.24") (dash "2.19"))
|} in
  check_none "no emacs dep" (P.parse_package_requires content)

let test_empty_requires () =
  let content = {|;; Package-Requires: ()
|} in
  check_none "empty requires" (P.parse_package_requires content)

let test_empty_content () =
  check_none "empty content" (P.parse_package_requires "")

let test_no_comment_prefix () =
  let content = {|Package-Requires: ((emacs "29.1"))
|} in
  check_none "no comment prefix" (P.parse_package_requires content)

(* =============================================================================
   parse_package_requires — line limit
   ============================================================================= *)

let test_beyond_50_lines () =
  let lines =
    List.init 55 (fun i ->
        if i = 52 then ";; Package-Requires: ((emacs \"29.1\"))"
        else Printf.sprintf ";; line %d" i)
  in
  let content = String.concat "\n" lines in
  check_none "beyond 50 lines" (P.parse_package_requires content)

let test_at_line_49 () =
  let lines =
    List.init 51 (fun i ->
        if i = 49 then ";; Package-Requires: ((emacs \"29.1\"))"
        else Printf.sprintf ";; line %d" i)
  in
  let content = String.concat "\n" lines in
  check_version "at line 49"
    { major = 29; minor = 1; patch = None }
    (P.parse_package_requires content)

(* =============================================================================
   parse_package_requires — case insensitivity
   ============================================================================= *)

let test_case_insensitive_key () =
  let content = {|;; package-requires: ((emacs "29.1"))
|} in
  check_version "lowercase key"
    { major = 29; minor = 1; patch = None }
    (P.parse_package_requires content)

let test_case_insensitive_emacs () =
  let content = {|;; Package-Requires: ((Emacs "29.1"))
|} in
  check_version "capitalized emacs"
    { major = 29; minor = 1; patch = None }
    (P.parse_package_requires content)

(* =============================================================================
   find_package_version — file I/O
   ============================================================================= *)

let test_find_nonexistent () =
  check_none "nonexistent file"
    (P.find_package_version "/tmp/tart-test-nonexistent-12345.el")

let test_find_real_file () =
  let path = Filename.temp_file "tart-pkg-test" ".el" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let oc = open_out path in
      output_string oc
        {|;;; test.el --- Test  -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "28.1"))
|};
      close_out oc;
      check_version "real file"
        { major = 28; minor = 1; patch = None }
        (P.find_package_version path))

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "package_header"
    [
      ( "standard formats",
        [
          Alcotest.test_case "standard" `Quick test_standard;
          Alcotest.test_case "multiple deps" `Quick test_multiple_deps;
          Alcotest.test_case "emacs not first" `Quick test_emacs_not_first;
          Alcotest.test_case "three semicolons" `Quick test_three_semicolons;
          Alcotest.test_case "with patch version" `Quick test_with_patch;
          Alcotest.test_case "leading spaces" `Quick test_leading_spaces;
        ] );
      ( "missing and malformed",
        [
          Alcotest.test_case "missing header" `Quick test_missing_header;
          Alcotest.test_case "malformed version" `Quick test_malformed_version;
          Alcotest.test_case "no emacs dep" `Quick test_no_emacs_dep;
          Alcotest.test_case "empty requires" `Quick test_empty_requires;
          Alcotest.test_case "empty content" `Quick test_empty_content;
          Alcotest.test_case "no comment prefix" `Quick test_no_comment_prefix;
        ] );
      ( "line limit",
        [
          Alcotest.test_case "beyond 50 lines" `Quick test_beyond_50_lines;
          Alcotest.test_case "at line 49" `Quick test_at_line_49;
        ] );
      ( "case insensitivity",
        [
          Alcotest.test_case "lowercase key" `Quick test_case_insensitive_key;
          Alcotest.test_case "capitalized emacs" `Quick
            test_case_insensitive_emacs;
        ] );
      ( "find_package_version",
        [
          Alcotest.test_case "nonexistent file" `Quick test_find_nonexistent;
          Alcotest.test_case "real file" `Quick test_find_real_file;
        ] );
    ]
