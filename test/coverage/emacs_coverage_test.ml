(** Tests for Emacs C layer coverage. *)

open Coverage.Emacs_coverage
open Coverage.C_scanner

(** {1 Test Helpers} *)

(** Create a temporary directory with typings files and return typings_root. *)
let with_temp_typings version_dirs f =
  let tmpdir = Filename.temp_file "typings_" "" in
  Sys.remove tmpdir;
  Unix.mkdir tmpdir 0o755;
  let cleanup () =
    (* Recursively remove the directory *)
    let rec remove_dir dir =
      if Sys.is_directory dir then (
        Array.iter
          (fun f ->
            let path = Filename.concat dir f in
            if Sys.is_directory path then remove_dir path else Sys.remove path)
          (Sys.readdir dir);
        Unix.rmdir dir)
      else Sys.remove dir
    in
    try remove_dir tmpdir with _ -> ()
  in
  Fun.protect ~finally:cleanup (fun () ->
      (* Create each version directory with c-core subdirectory *)
      List.iter
        (fun (ver_str, tart_files) ->
          let ver_dir = Filename.concat tmpdir ver_str in
          Unix.mkdir ver_dir 0o755;
          let c_core_dir = Filename.concat ver_dir "c-core" in
          Unix.mkdir c_core_dir 0o755;
          List.iter
            (fun (name, content) ->
              let path = Filename.concat c_core_dir name in
              let oc = open_out path in
              output_string oc content;
              close_out oc)
            tart_files)
        version_dirs;
      f tmpdir)

(** {1 Basic Coverage Tests} *)

let test_coverage_with_signatures () =
  (* Create typings with 'car' and 'cdr' signatures *)
  with_temp_typings
    [
      ( "31.0",
        [
          ("data.tart", "(defun car (any) -> any)\n(defun cdr (any) -> any)\n");
        ] );
    ]
    (fun typings_root ->
      let version = Sig.Emacs_version.{ major = 31; minor = 0; patch = None } in
      let definitions =
        [
          { name = "car"; kind = Defun; file = "data.c"; line = 100 };
          { name = "cdr"; kind = Defun; file = "data.c"; line = 200 };
          { name = "cons"; kind = Defun; file = "alloc.c"; line = 50 };
        ]
      in
      let result =
        calculate_coverage ~source_dir:"/emacs/src" ~emacs_version:"31.0"
          ~typings_root ~version definitions
      in
      Alcotest.(check int) "3 items" 3 (List.length result.items);
      let covered = covered_public result in
      let uncovered = uncovered_public result in
      Alcotest.(check int) "2 covered (car, cdr)" 2 (List.length covered);
      Alcotest.(check int) "1 uncovered (cons)" 1 (List.length uncovered);
      Alcotest.(check string)
        "uncovered is cons" "cons" (List.hd uncovered).definition.name)

let test_coverage_empty_typings () =
  (* No typings = nothing covered *)
  with_temp_typings
    [ ("31.0", [ ("empty.tart", "") ]) ]
    (fun typings_root ->
      let version = Sig.Emacs_version.{ major = 31; minor = 0; patch = None } in
      let definitions =
        [
          { name = "car"; kind = Defun; file = "data.c"; line = 1 };
          { name = "cdr"; kind = Defun; file = "data.c"; line = 2 };
        ]
      in
      let result =
        calculate_coverage ~source_dir:"/src" ~emacs_version:"31.0"
          ~typings_root ~version definitions
      in
      let uncovered = uncovered_public result in
      Alcotest.(check int) "2 uncovered" 2 (List.length uncovered))

(** {1 Summary Statistics Tests} *)

let test_summary_statistics () =
  with_temp_typings
    [ ("31.0", [ ("data.tart", "(defun car (any) -> any)\n") ]) ]
    (fun typings_root ->
      let version = Sig.Emacs_version.{ major = 31; minor = 0; patch = None } in
      let definitions =
        [
          { name = "car"; kind = Defun; file = "data.c"; line = 1 };
          { name = "cdr"; kind = Defun; file = "data.c"; line = 2 };
          { name = "cons"; kind = Defun; file = "alloc.c"; line = 1 };
        ]
      in
      let result =
        calculate_coverage ~source_dir:"/src" ~emacs_version:"31.0"
          ~typings_root ~version definitions
      in
      let summary = summarize result in
      Alcotest.(check int) "3 total public" 3 summary.total_public;
      Alcotest.(check int) "1 covered" 1 summary.covered_public;
      Alcotest.(check int) "2 uncovered" 2 summary.uncovered_public;
      Alcotest.(check int) "0 private" 0 summary.total_private)

let test_summary_with_private () =
  with_temp_typings
    [ ("31.0", [ ("data.tart", "(defun car (any) -> any)\n") ]) ]
    (fun typings_root ->
      let version = Sig.Emacs_version.{ major = 31; minor = 0; patch = None } in
      let definitions =
        [
          { name = "car"; kind = Defun; file = "data.c"; line = 1 };
          { name = "internal--foo"; kind = Defun; file = "foo.c"; line = 1 };
          { name = "cdr"; kind = Defun; file = "data.c"; line = 2 };
        ]
      in
      let result =
        calculate_coverage ~source_dir:"/src" ~emacs_version:"31.0"
          ~typings_root ~version definitions
      in
      let summary = summarize result in
      Alcotest.(check int) "2 public" 2 summary.total_public;
      Alcotest.(check int) "1 private" 1 summary.total_private)

let test_coverage_percentage () =
  with_temp_typings
    [ ("31.0", [ ("data.tart", "(defun car (any) -> any)\n") ]) ]
    (fun typings_root ->
      let version = Sig.Emacs_version.{ major = 31; minor = 0; patch = None } in
      let definitions =
        [
          { name = "car"; kind = Defun; file = "data.c"; line = 1 };
          { name = "cdr"; kind = Defun; file = "data.c"; line = 2 };
        ]
      in
      let result =
        calculate_coverage ~source_dir:"/src" ~emacs_version:"31.0"
          ~typings_root ~version definitions
      in
      let summary = summarize result in
      let pct = coverage_percentage summary in
      (* 1 out of 2 = 50% *)
      Alcotest.(check (float 0.1)) "50%" 50.0 pct)

let test_coverage_percentage_empty () =
  (* No definitions = 100% coverage *)
  with_temp_typings
    [ ("31.0", [ ("empty.tart", "") ]) ]
    (fun typings_root ->
      let version = Sig.Emacs_version.{ major = 31; minor = 0; patch = None } in
      let definitions = [] in
      let result =
        calculate_coverage ~source_dir:"/src" ~emacs_version:"31.0"
          ~typings_root ~version definitions
      in
      let summary = summarize result in
      let pct = coverage_percentage summary in
      Alcotest.(check (float 0.1)) "100% when empty" 100.0 pct)

(** {1 Version Fallback Tests} *)

let test_version_fallback () =
  (* Only latest typings exist, version 31.0.50 should fall back *)
  with_temp_typings
    [ ("latest", [ ("data.tart", "(defun car (any) -> any)\n") ]) ]
    (fun typings_root ->
      let version =
        Sig.Emacs_version.{ major = 31; minor = 0; patch = Some 50 }
      in
      let definitions =
        [ { name = "car"; kind = Defun; file = "data.c"; line = 1 } ]
      in
      let result =
        calculate_coverage ~source_dir:"/src" ~emacs_version:"31.0.50"
          ~typings_root ~version definitions
      in
      let covered = covered_public result in
      Alcotest.(check int) "1 covered via fallback" 1 (List.length covered))

(** {1 Filtering Tests} *)

let test_uncovered_public_sorted () =
  with_temp_typings
    [ ("31.0", [ ("empty.tart", "") ]) ]
    (fun typings_root ->
      let version = Sig.Emacs_version.{ major = 31; minor = 0; patch = None } in
      let definitions =
        [
          { name = "zebra"; kind = Defun; file = "z.c"; line = 1 };
          { name = "apple"; kind = Defun; file = "a.c"; line = 1 };
          { name = "mango"; kind = Defun; file = "m.c"; line = 1 };
        ]
      in
      let result =
        calculate_coverage ~source_dir:"/src" ~emacs_version:"31.0"
          ~typings_root ~version definitions
      in
      let uncovered = uncovered_public result in
      let names = List.map (fun i -> i.definition.name) uncovered in
      Alcotest.(check (list string))
        "sorted alphabetically"
        [ "apple"; "mango"; "zebra" ]
        names)

let test_private_definitions_sorted () =
  with_temp_typings
    [ ("31.0", [ ("empty.tart", "") ]) ]
    (fun typings_root ->
      let version = Sig.Emacs_version.{ major = 31; minor = 0; patch = None } in
      let definitions =
        [
          { name = "z--private"; kind = Defun; file = "z.c"; line = 1 };
          { name = "public"; kind = Defun; file = "p.c"; line = 1 };
          { name = "a--private"; kind = Defun; file = "a.c"; line = 1 };
        ]
      in
      let result =
        calculate_coverage ~source_dir:"/src" ~emacs_version:"31.0"
          ~typings_root ~version definitions
      in
      let private_ = private_definitions result in
      let names = List.map (fun i -> i.definition.name) private_ in
      Alcotest.(check (list string))
        "sorted alphabetically"
        [ "a--private"; "z--private" ]
        names)

(** {1 Metadata Tests} *)

let test_result_metadata () =
  with_temp_typings
    [ ("31.0", [ ("empty.tart", "") ]) ]
    (fun typings_root ->
      let version = Sig.Emacs_version.{ major = 31; minor = 0; patch = None } in
      let definitions =
        [
          { name = "foo"; kind = Defun; file = "a.c"; line = 1 };
          { name = "bar"; kind = Defun; file = "b.c"; line = 1 };
        ]
      in
      let result =
        calculate_coverage ~source_dir:"/custom/path" ~emacs_version:"31.0"
          ~typings_root ~version definitions
      in
      Alcotest.(check string) "source_dir" "/custom/path" result.source_dir;
      Alcotest.(check string) "emacs_version" "31.0" result.emacs_version;
      Alcotest.(check int) "2 files scanned" 2 result.files_scanned)

(** {1 Load Typings Tests} *)

let test_load_typings () =
  with_temp_typings
    [
      ( "31.0",
        [
          ("data.tart", "(defun car (any) -> any)\n(defun cdr (any) -> any)\n");
        ] );
    ]
    (fun typings_root ->
      let version = Sig.Emacs_version.{ major = 31; minor = 0; patch = None } in
      let env = load_typings ~typings_root ~version in
      Alcotest.(check bool) "car exists" true (has_signature env "car");
      Alcotest.(check bool) "cdr exists" true (has_signature env "cdr");
      Alcotest.(check bool) "cons missing" false (has_signature env "cons"))

(** {1 Test Suites} *)

let coverage_tests =
  [
    Alcotest.test_case "with signatures" `Quick test_coverage_with_signatures;
    Alcotest.test_case "empty typings" `Quick test_coverage_empty_typings;
  ]

let summary_tests =
  [
    Alcotest.test_case "statistics" `Quick test_summary_statistics;
    Alcotest.test_case "with private" `Quick test_summary_with_private;
    Alcotest.test_case "percentage" `Quick test_coverage_percentage;
    Alcotest.test_case "percentage empty" `Quick test_coverage_percentage_empty;
  ]

let fallback_tests =
  [ Alcotest.test_case "version fallback" `Quick test_version_fallback ]

let filtering_tests =
  [
    Alcotest.test_case "uncovered sorted" `Quick test_uncovered_public_sorted;
    Alcotest.test_case "private sorted" `Quick test_private_definitions_sorted;
  ]

let metadata_tests =
  [ Alcotest.test_case "result metadata" `Quick test_result_metadata ]

let typings_tests =
  [ Alcotest.test_case "load typings" `Quick test_load_typings ]

let () =
  Alcotest.run "emacs_coverage"
    [
      ("coverage", coverage_tests);
      ("summary", summary_tests);
      ("fallback", fallback_tests);
      ("filtering", filtering_tests);
      ("metadata", metadata_tests);
      ("typings", typings_tests);
    ]
