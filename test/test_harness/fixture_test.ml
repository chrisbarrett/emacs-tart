(** Integration tests that run actual fixtures through the acceptance harness.

    These tests verify that the core typings fixtures all pass type checking. *)

open Test_harness.Acceptance

(** Find the tart binary.

    Searches in order: 1. TART_BIN environment variable 2. Relative paths from
    test execution directory

    Tests run from _build/default/test/test_harness/ *)
let find_tart_bin () : string option =
  (* Check environment variable first *)
  match Sys.getenv_opt "TART_BIN" with
  | Some bin when Sys.file_exists bin -> Some bin
  | _ ->
      let candidates =
        [
          (* From test directory in _build/default/test/test_harness/ *)
          "../../bin/main.exe";
          (* From workspace root *)
          "_build/default/bin/main.exe";
        ]
      in
      List.find_opt Sys.file_exists candidates

(** Find the fixtures directory.

    Searches in order: 1. FIXTURES_DIR environment variable 2. Relative paths
    from test execution directory

    Tests run from _build/default/test/test_harness/ with fixtures copied there
*)
let find_fixtures_dir () : string option =
  match Sys.getenv_opt "FIXTURES_DIR" with
  | Some dir when Sys.is_directory dir -> Some dir
  | _ ->
      let candidates =
        [
          (* From test directory - fixtures are copied as dependency *)
          "../fixtures/typing";
          (* From workspace root *)
          "test/fixtures/typing";
        ]
      in
      List.find_opt Sys.file_exists candidates

(** Run a single fixture and assert it passes. *)
let run_fixture_test ~tart_bin ~fixture_path () =
  let result = check_fixture ~tart_bin ~path:fixture_path in
  if not result.passed then
    let msg =
      Printf.sprintf "Fixture %s failed:\n%s" result.path
        (Option.value result.diff ~default:"(no diff)")
    in
    Alcotest.fail msg

(** Generate test cases for all fixtures in a directory. *)
let fixture_tests_for_dir ~tart_bin ~dir : unit Alcotest.test_case list =
  let fixtures = discover_fixtures dir in
  List.map
    (fun path ->
      let name = Filename.basename path in
      Alcotest.test_case name `Quick
        (run_fixture_test ~tart_bin ~fixture_path:path))
    fixtures

(** Main test runner. *)
let () =
  match (find_tart_bin (), find_fixtures_dir ()) with
  | None, _ ->
      (* Skip tests if tart binary not found *)
      Printf.eprintf
        "Warning: tart binary not found, skipping fixture tests.\n\
         Set TART_BIN environment variable or build with 'dune build'.\n";
      Alcotest.run "fixtures" []
  | _, None ->
      (* Skip tests if fixtures not found *)
      Printf.eprintf
        "Warning: fixtures directory not found, skipping fixture tests.\n\
         Set FIXTURES_DIR environment variable.\n";
      Alcotest.run "fixtures" []
  | Some tart_bin, Some fixtures_dir ->
      (* Run all fixture tests *)
      let core_dir = Filename.concat fixtures_dir "core" in
      let version_dir = Filename.concat fixtures_dir "version" in
      let regression_dir = Filename.concat fixtures_dir "regression" in
      let errors_dir = Filename.concat fixtures_dir "errors" in
      (* Collect error category subdirectories *)
      let error_tests =
        if Sys.file_exists errors_dir && Sys.is_directory errors_dir then
          Sys.readdir errors_dir |> Array.to_list
          |> List.filter_map (fun name ->
                 let subdir = Filename.concat errors_dir name in
                 if Sys.is_directory subdir then
                   let tests = fixture_tests_for_dir ~tart_bin ~dir:subdir in
                   if tests <> [] then Some ("errors/" ^ name, tests) else None
                 else None)
        else []
      in
      let tests =
        (if Sys.file_exists core_dir && Sys.is_directory core_dir then
           [ ("core-typings", fixture_tests_for_dir ~tart_bin ~dir:core_dir) ]
         else [])
        @ (if Sys.file_exists version_dir && Sys.is_directory version_dir then
             [
               ( "version-specific",
                 fixture_tests_for_dir ~tart_bin ~dir:version_dir );
             ]
           else [])
        @ (if Sys.file_exists regression_dir && Sys.is_directory regression_dir
           then
             [
               ( "regression",
                 fixture_tests_for_dir ~tart_bin ~dir:regression_dir );
             ]
           else [])
        @ error_tests
      in
      Alcotest.run "fixtures" tests
