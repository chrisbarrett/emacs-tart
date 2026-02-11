(** Tests for Emacs coverage (C and Elisp layers). *)

open Coverage.Emacs_coverage
open Coverage.C_scanner

(** {1 Test Helpers} *)

(** Create a temporary directory with typings files and return typings_root.

    [version_dirs] is a list of
    [(version_string, c_tart_files, lisp_tart_files)] triples. Each version gets
    a [c-core/] subdirectory populated with [c_tart_files] and optionally a
    [lisp-core/] subdirectory populated with [lisp_tart_files]. *)
let with_temp_typings version_dirs f =
  let tmpdir = Filename.temp_file "typings_" "" in
  Sys.remove tmpdir;
  Unix.mkdir tmpdir 0o755;
  let cleanup () =
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
      List.iter
        (fun (ver_str, c_tart_files, lisp_tart_files) ->
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
            c_tart_files;
          if lisp_tart_files <> [] then (
            let lisp_core_dir = Filename.concat ver_dir "lisp-core" in
            Unix.mkdir lisp_core_dir 0o755;
            List.iter
              (fun (name, content) ->
                let path = Filename.concat lisp_core_dir name in
                let oc = open_out path in
                output_string oc content;
                close_out oc)
              lisp_tart_files))
        version_dirs;
      f tmpdir)

(** Create a temporary Emacs source tree with [lisp/*.el] files. Returns the
    source directory path. *)
let with_temp_source el_files f =
  let tmpdir = Filename.temp_file "emacs_src_" "" in
  Sys.remove tmpdir;
  Unix.mkdir tmpdir 0o755;
  let lisp_dir = Filename.concat tmpdir "lisp" in
  Unix.mkdir lisp_dir 0o755;
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
        (fun (name, content) ->
          let path = Filename.concat lisp_dir name in
          let oc = open_out path in
          output_string oc content;
          close_out oc)
        el_files;
      f tmpdir)

(** {1 Basic C Coverage Tests} *)

let test_coverage_with_signatures () =
  with_temp_typings
    [
      ( "31.0",
        [
          ("data.tart", "(defun car (any) -> any)\n(defun cdr (any) -> any)\n");
        ],
        [] );
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
  with_temp_typings
    [ ("31.0", [ ("empty.tart", "") ], []) ]
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

(** {1 C Summary Statistics Tests} *)

let test_summary_statistics () =
  with_temp_typings
    [ ("31.0", [ ("data.tart", "(defun car (any) -> any)\n") ], []) ]
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
    [ ("31.0", [ ("data.tart", "(defun car (any) -> any)\n") ], []) ]
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
    [ ("31.0", [ ("data.tart", "(defun car (any) -> any)\n") ], []) ]
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
    [ ("31.0", [ ("empty.tart", "") ], []) ]
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
    [ ("latest", [ ("data.tart", "(defun car (any) -> any)\n") ], []) ]
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

(** {1 C Filtering Tests} *)

let test_uncovered_public_sorted () =
  with_temp_typings
    [ ("31.0", [ ("empty.tart", "") ], []) ]
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
      let names =
        List.map (fun (i : c_coverage_item) -> i.definition.name) uncovered
      in
      Alcotest.(check (list string))
        "sorted alphabetically"
        [ "apple"; "mango"; "zebra" ]
        names)

let test_private_definitions_sorted () =
  with_temp_typings
    [ ("31.0", [ ("empty.tart", "") ], []) ]
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
      let names =
        List.map (fun (i : c_coverage_item) -> i.definition.name) private_
      in
      Alcotest.(check (list string))
        "sorted alphabetically"
        [ "a--private"; "z--private" ]
        names)

(** {1 C Metadata Tests} *)

let test_result_metadata () =
  with_temp_typings
    [ ("31.0", [ ("empty.tart", "") ], []) ]
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
        ],
        [] );
    ]
    (fun typings_root ->
      let version = Sig.Emacs_version.{ major = 31; minor = 0; patch = None } in
      let env = load_typings ~typings_root ~version in
      Alcotest.(check bool) "car exists" true (has_signature env "car");
      Alcotest.(check bool) "cdr exists" true (has_signature env "cdr");
      Alcotest.(check bool) "cons missing" false (has_signature env "cons"))

(** {1 Elisp Coverage Tests} *)

let test_elisp_coverage_with_signatures () =
  with_temp_typings
    [
      ( "31.0",
        [],
        [
          ( "simple.tart",
            "(defun widget-apply (&rest any) -> any)\n\
             (defvar widget-keymap any)\n" );
        ] );
    ]
    (fun typings_root ->
      with_temp_source
        [
          ( "simple.el",
            "(defun widget-apply (widget property &rest args)\n\
            \  nil)\n\
             (defvar widget-keymap nil)\n\
             (defun widget-create (type &rest args)\n\
            \  nil)\n" );
        ]
        (fun source_dir ->
          let version =
            Sig.Emacs_version.{ major = 31; minor = 0; patch = None }
          in
          let result =
            calculate_elisp_coverage ~source_dir ~emacs_version:"31.0"
              ~typings_root ~version
          in
          Alcotest.(check int)
            "1 file result" 1
            (List.length result.file_results);
          let summary = elisp_summarize result in
          Alcotest.(check int) "3 total public" 3 summary.elisp_total_public;
          Alcotest.(check int) "2 covered" 2 summary.elisp_covered_public;
          Alcotest.(check int) "1 uncovered" 1 summary.elisp_uncovered_public))

let test_elisp_coverage_no_typings () =
  (* File exists but no matching .tart → 0/N public coverage *)
  with_temp_typings
    [ ("31.0", [], []) ]
    (fun typings_root ->
      with_temp_source
        [
          ( "orphan.el",
            "(defun orphan-func (x)\n  x)\n(defvar orphan-var nil)\n" );
        ]
        (fun source_dir ->
          let version =
            Sig.Emacs_version.{ major = 31; minor = 0; patch = None }
          in
          let result =
            calculate_elisp_coverage ~source_dir ~emacs_version:"31.0"
              ~typings_root ~version
          in
          let summary = elisp_summarize result in
          Alcotest.(check int) "2 total public" 2 summary.elisp_total_public;
          Alcotest.(check int) "0 covered" 0 summary.elisp_covered_public;
          Alcotest.(check int) "2 uncovered" 2 summary.elisp_uncovered_public))

let test_elisp_private_excluded () =
  (* Private identifiers (containing --) excluded from coverage percentage *)
  with_temp_typings
    [ ("31.0", [], []) ]
    (fun typings_root ->
      with_temp_source
        [
          ( "priv.el",
            "(defun my-pkg-public (x)\n  x)\n(defun my-pkg--private (x)\n  x)\n"
          );
        ]
        (fun source_dir ->
          let version =
            Sig.Emacs_version.{ major = 31; minor = 0; patch = None }
          in
          let result =
            calculate_elisp_coverage ~source_dir ~emacs_version:"31.0"
              ~typings_root ~version
          in
          let summary = elisp_summarize result in
          Alcotest.(check int) "1 total public" 1 summary.elisp_total_public;
          Alcotest.(check int) "1 total private" 1 summary.elisp_total_private))

let test_elisp_no_lisp_dir () =
  (* Source dir without lisp/ → empty results *)
  let tmpdir = Filename.temp_file "empty_src_" "" in
  Sys.remove tmpdir;
  Unix.mkdir tmpdir 0o755;
  Fun.protect
    ~finally:(fun () -> try Unix.rmdir tmpdir with _ -> ())
    (fun () ->
      with_temp_typings
        [ ("31.0", [], []) ]
        (fun typings_root ->
          let version =
            Sig.Emacs_version.{ major = 31; minor = 0; patch = None }
          in
          let result =
            calculate_elisp_coverage ~source_dir:tmpdir ~emacs_version:"31.0"
              ~typings_root ~version
          in
          Alcotest.(check int)
            "0 file results" 0
            (List.length result.file_results)))

let test_elisp_percentage () =
  with_temp_typings
    [
      ("31.0", [], [ ("pct.tart", "(defun covered-fn (&rest any) -> any)\n") ]);
    ]
    (fun typings_root ->
      with_temp_source
        [
          ( "pct.el",
            "(defun covered-fn (x)\n  x)\n(defun uncovered-fn (x)\n  x)\n" );
        ]
        (fun source_dir ->
          let version =
            Sig.Emacs_version.{ major = 31; minor = 0; patch = None }
          in
          let result =
            calculate_elisp_coverage ~source_dir ~emacs_version:"31.0"
              ~typings_root ~version
          in
          let summary = elisp_summarize result in
          let pct = elisp_coverage_percentage summary in
          Alcotest.(check (float 0.1)) "50%" 50.0 pct))

let test_elisp_filtering () =
  with_temp_typings
    [ ("31.0", [], [ ("filt.tart", "(defun alpha-fn (&rest any) -> any)\n") ]) ]
    (fun typings_root ->
      with_temp_source
        [
          ( "filt.el",
            "(defun alpha-fn (x)\n\
            \  x)\n\
             (defun beta-fn (x)\n\
            \  x)\n\
             (defun gamma--priv (x)\n\
            \  x)\n" );
        ]
        (fun source_dir ->
          let version =
            Sig.Emacs_version.{ major = 31; minor = 0; patch = None }
          in
          let result =
            calculate_elisp_coverage ~source_dir ~emacs_version:"31.0"
              ~typings_root ~version
          in
          let covered = elisp_covered_public result in
          let uncovered = elisp_uncovered_public result in
          let private_ = elisp_private_definitions result in
          Alcotest.(check int) "1 covered" 1 (List.length covered);
          Alcotest.(check int) "1 uncovered" 1 (List.length uncovered);
          Alcotest.(check int) "1 private" 1 (List.length private_);
          let covered_names =
            List.map
              (fun (i : elisp_coverage_item) -> i.definition.name)
              covered
          in
          let uncovered_names =
            List.map
              (fun (i : elisp_coverage_item) -> i.definition.name)
              uncovered
          in
          Alcotest.(check (list string)) "covered" [ "alpha-fn" ] covered_names;
          Alcotest.(check (list string))
            "uncovered" [ "beta-fn" ] uncovered_names))

(** {1 Combined Summary Tests} *)

let test_combined_summary () =
  let c_summary =
    {
      total_public = 10;
      covered_public = 7;
      uncovered_public = 3;
      total_private = 2;
    }
  in
  let elisp_summary =
    {
      elisp_total_public = 20;
      elisp_covered_public = 15;
      elisp_uncovered_public = 5;
      elisp_total_private = 3;
    }
  in
  let combined = combine_summaries c_summary elisp_summary in
  Alcotest.(check int) "total public" 30 combined.total_public;
  Alcotest.(check int) "total covered" 22 combined.total_covered;
  Alcotest.(check int) "total uncovered" 8 combined.total_uncovered;
  Alcotest.(check int) "total private" 5 combined.total_private;
  let pct = combined_coverage_percentage combined in
  Alcotest.(check (float 0.1)) "73.3%" 73.3 pct

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

let elisp_coverage_tests =
  [
    Alcotest.test_case "with signatures" `Quick
      test_elisp_coverage_with_signatures;
    Alcotest.test_case "no typings" `Quick test_elisp_coverage_no_typings;
    Alcotest.test_case "private excluded" `Quick test_elisp_private_excluded;
    Alcotest.test_case "no lisp dir" `Quick test_elisp_no_lisp_dir;
    Alcotest.test_case "percentage" `Quick test_elisp_percentage;
    Alcotest.test_case "filtering" `Quick test_elisp_filtering;
  ]

let combined_tests =
  [ Alcotest.test_case "combined summary" `Quick test_combined_summary ]

let () =
  Alcotest.run "emacs_coverage"
    [
      ("coverage", coverage_tests);
      ("summary", summary_tests);
      ("fallback", fallback_tests);
      ("filtering", filtering_tests);
      ("metadata", metadata_tests);
      ("typings", typings_tests);
      ("elisp_coverage", elisp_coverage_tests);
      ("combined", combined_tests);
    ]
