(** Tests for signature search path and module resolution (R15, R16) *)

open Sig
module Type_env = Core.Type_env
module Types = Core.Types
module Check = Typing.Check

(** {1 Test Fixtures} *)

(** Create a temporary directory for testing. *)
let make_temp_dir () =
  let dir = Filename.temp_file "tart-test" "" in
  Unix.unlink dir;
  Unix.mkdir dir 0o755;
  dir

(** Remove a directory and its contents recursively. *)
let rec rm_rf path =
  if Sys.is_directory path then (
    Array.iter
      (fun name -> rm_rf (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Unix.unlink path

(** Write content to a file, creating parent directories as needed. *)
let write_file path content =
  Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc content)

(** Wrapper to run a test with temporary directories, cleaning up after. *)
let with_temp_dirs n f =
  let dirs = List.init n (fun _ -> make_temp_dir ()) in
  Fun.protect ~finally:(fun () -> List.iter rm_rf dirs) (fun () -> f dirs)

(** {1 Search Path Configuration Tests} *)

let test_empty_search_path () =
  let sp = Search_path.empty in
  (* Empty search path should find nothing *)
  let result = Search_path.find_signature sp "nonexistent" in
  Alcotest.(check (option string)) "empty path finds nothing" None result

let test_search_path_of_dirs () =
  with_temp_dirs 2 (fun dirs ->
      match dirs with
      | [ dir1; dir2 ] ->
          (* Create a .tart file in dir2 *)
          write_file
            (Filename.concat dir2 "mymod.tart")
            {|
          (defun my-func (int) -> int)
        |};
          let sp = Search_path.of_dirs [ dir1; dir2 ] in
          let result = Search_path.find_signature sp "mymod" in
          let expected = Some (Filename.concat dir2 "mymod.tart") in
          Alcotest.(check (option string))
            "finds file in second dir" expected result
      | _ -> Alcotest.fail "expected 2 dirs")

let test_search_path_precedence () =
  with_temp_dirs 2 (fun dirs ->
      match dirs with
      | [ dir1; dir2 ] ->
          (* Create .tart files in both directories *)
          write_file (Filename.concat dir1 "mymod.tart") "(defvar x int)";
          write_file (Filename.concat dir2 "mymod.tart") "(defvar x string)";
          let sp = Search_path.of_dirs [ dir1; dir2 ] in
          let result = Search_path.find_signature sp "mymod" in
          (* dir1 should win (first in search path) *)
          let expected = Some (Filename.concat dir1 "mymod.tart") in
          Alcotest.(check (option string)) "first dir wins" expected result
      | _ -> Alcotest.fail "expected 2 dirs")

let test_search_path_with_stdlib () =
  with_temp_dirs 2 (fun dirs ->
      match dirs with
      | [ search_dir; stdlib_dir ] ->
          (* Create .tart file only in stdlib *)
          write_file
            (Filename.concat stdlib_dir "cl-lib.tart")
            {|
          (defun cl-mapcar [a b] (((a -> b)) (list a)) -> (list b))
        |};
          let sp =
            Search_path.of_dirs [ search_dir ]
            |> Search_path.with_stdlib stdlib_dir
          in
          let result = Search_path.find_signature sp "cl-lib" in
          let expected = Some (Filename.concat stdlib_dir "cl-lib.tart") in
          Alcotest.(check (option string)) "finds in stdlib" expected result
      | _ -> Alcotest.fail "expected 2 dirs")

let test_search_path_prefers_search_over_stdlib () =
  with_temp_dirs 2 (fun dirs ->
      match dirs with
      | [ search_dir; stdlib_dir ] ->
          (* Create .tart files in both search dir and stdlib *)
          write_file (Filename.concat search_dir "seq.tart") "(defvar x int)";
          write_file (Filename.concat stdlib_dir "seq.tart") "(defvar x string)";
          let sp =
            Search_path.of_dirs [ search_dir ]
            |> Search_path.with_stdlib stdlib_dir
          in
          let result = Search_path.find_signature sp "seq" in
          (* search dir should win over stdlib *)
          let expected = Some (Filename.concat search_dir "seq.tart") in
          Alcotest.(check (option string))
            "search dir wins over stdlib" expected result
      | _ -> Alcotest.fail "expected 2 dirs")

(** {1 Sibling File Tests (R16 - step 1)} *)

let test_find_sibling () =
  with_temp_dirs 1 (fun dirs ->
      match dirs with
      | [ dir ] ->
          let el_path = Filename.concat dir "mymod.el" in
          let tart_path = Filename.concat dir "mymod.tart" in
          write_file el_path "(defun my-func (x) x)";
          write_file tart_path "(defun my-func (int) -> int)";
          let result = Search_path.find_sibling el_path "mymod" in
          Alcotest.(check (option string))
            "finds sibling" (Some tart_path) result
      | _ -> Alcotest.fail "expected 1 dir")

let test_find_sibling_not_found () =
  with_temp_dirs 1 (fun dirs ->
      match dirs with
      | [ dir ] ->
          let el_path = Filename.concat dir "mymod.el" in
          write_file el_path "(defun my-func (x) x)";
          (* No .tart file *)
          let result = Search_path.find_sibling el_path "mymod" in
          Alcotest.(check (option string)) "no sibling" None result
      | _ -> Alcotest.fail "expected 1 dir")

(** {1 Module Discovery Order Tests (R16)} *)

(** Test that sibling files take precedence over search path. R16: "The first
    match wins, allowing project-local overrides." *)
let test_sibling_takes_precedence () =
  with_temp_dirs 2 (fun dirs ->
      match dirs with
      | [ project_dir; search_dir ] -> (
          let el_path = Filename.concat project_dir "mymod.el" in
          let sibling_tart = Filename.concat project_dir "mymod.tart" in
          let search_tart = Filename.concat search_dir "mymod.tart" in
          (* Create both files with different content *)
          write_file el_path "(defun my-func (x) x)";
          write_file sibling_tart "(defvar sibling int)";
          write_file search_tart "(defvar search-path string)";
          let sp = Search_path.of_dirs [ search_dir ] in
          let resolver = Search_path.make_resolver ~el_path sp in
          let result = resolver "mymod" in
          (* Sibling should win *)
          match result with
          | None -> Alcotest.fail "should find module"
          | Some sig_file ->
              (* Check that we got the sibling version (has 'sibling' defvar) *)
              let has_sibling =
                List.exists
                  (function
                    | Sig_ast.DDefvar d -> d.defvar_name = "sibling"
                    | _ -> false)
                  sig_file.sig_decls
              in
              Alcotest.(check bool) "sibling version loaded" true has_sibling)
      | _ -> Alcotest.fail "expected 2 dirs")

(** Test full discovery order: sibling -> search path -> stdlib. R16: Module
    discovery order *)
let test_full_discovery_order () =
  with_temp_dirs 3 (fun dirs ->
      match dirs with
      | [ project_dir; search_dir; stdlib_dir ] -> (
          let el_path = Filename.concat project_dir "project.el" in
          write_file el_path "";
          (* Only stdlib has the module *)
          write_file
            (Filename.concat stdlib_dir "stdlib-mod.tart")
            {|
          (defvar stdlib-var int)
        |};
          let sp =
            Search_path.of_dirs [ search_dir ]
            |> Search_path.with_stdlib stdlib_dir
          in
          let resolver = Search_path.make_resolver ~el_path sp in
          let result = resolver "stdlib-mod" in
          match result with
          | None -> Alcotest.fail "should find module in stdlib"
          | Some sig_file ->
              let has_stdlib_var =
                List.exists
                  (function
                    | Sig_ast.DDefvar d -> d.defvar_name = "stdlib-var"
                    | _ -> false)
                  sig_file.sig_decls
              in
              Alcotest.(check bool) "found stdlib module" true has_stdlib_var)
      | _ -> Alcotest.fail "expected 3 dirs")

(** {1 Parse and Load Tests} *)

let test_parse_signature_file () =
  with_temp_dirs 1 (fun dirs ->
      match dirs with
      | [ dir ] -> (
          let tart_path = Filename.concat dir "example.tart" in
          write_file tart_path
            {|
          (defun example-fn (int string) -> bool)
          (defvar example-var (list int))
        |};
          let result = Search_path.parse_signature_file tart_path in
          match result with
          | None -> Alcotest.fail "should parse file"
          | Some sig_file ->
              Alcotest.(check string)
                "module name from filename" "example" sig_file.sig_module;
              Alcotest.(check int)
                "has 2 declarations" 2
                (List.length sig_file.sig_decls))
      | _ -> Alcotest.fail "expected 1 dir")

let test_parse_invalid_file () =
  with_temp_dirs 1 (fun dirs ->
      match dirs with
      | [ dir ] ->
          let tart_path = Filename.concat dir "bad.tart" in
          write_file tart_path "(defun bad syntax";
          (* Invalid syntax *)
          let result = Search_path.parse_signature_file tart_path in
          Alcotest.(check bool)
            "invalid file returns None" true (Option.is_none result)
      | _ -> Alcotest.fail "expected 1 dir")

(** {1 End-to-End Loading Tests} *)

(** Helper to type check an expression with a given environment *)
let check_expr_str ~env s =
  Types.reset_tvar_counter ();
  match Syntax.Read.parse_one ~filename:"<test>" s with
  | Error msg -> failwith ("parse error: " ^ msg)
  | Ok sexp -> Check.check_expr ~env sexp

(** Test that loaded signatures from search path work with type checker. R15:
    "Call to seq-map with wrong types produces error" *)
let test_loaded_signature_type_checks () =
  with_temp_dirs 1 (fun dirs ->
      match dirs with
      | [ search_dir ] ->
          (* Create a signature file in search path *)
          write_file
            (Filename.concat search_dir "math.tart")
            {|
          (defun math-add (int int) -> int)
          (defun math-concat (string string) -> string)
        |};
          let sp = Search_path.of_dirs [ search_dir ] in
          let env =
            match
              Search_path.load_module ~search_path:sp ~env:Type_env.empty "math"
            with
            | None -> Alcotest.fail "should load math module"
            | Some env -> env
          in
          (* Call with correct types should succeed *)
          let ty1, errors1 = check_expr_str ~env "(math-add 1 2)" in
          Alcotest.(check int)
            "no errors for correct call" 0 (List.length errors1);
          Alcotest.(check string) "result is Int" "Int" (Types.to_string ty1);
          (* Call with wrong types should produce error *)
          let _, errors2 = check_expr_str ~env "(math-add \"a\" \"b\")" in
          Alcotest.(check bool)
            "error for wrong types" true
            (List.length errors2 > 0)
      | _ -> Alcotest.fail "expected 1 dir")

(** Test that search path resolver works with open/include. This verifies the
    resolver integrates correctly with sig_loader. *)
let test_resolver_with_open () =
  with_temp_dirs 1 (fun dirs ->
      match dirs with
      | [ search_dir ] -> (
          (* Create a base module with a type alias *)
          write_file
            (Filename.concat search_dir "base-types.tart")
            {|
          (type int-list (list int))
        |};
          (* Create a module that opens base-types *)
          write_file
            (Filename.concat search_dir "consumer.tart")
            {|
          (open 'base-types)
          (defun process (int-list) -> int)
        |};
          let sp = Search_path.of_dirs [ search_dir ] in
          let env =
            match
              Search_path.load_module ~search_path:sp ~env:Type_env.empty
                "consumer"
            with
            | None -> Alcotest.fail "should load consumer module"
            | Some env -> env
          in
          (* Verify the function is loaded with expanded type *)
          match Type_env.lookup "process" env with
          | None -> Alcotest.fail "process not found"
          | Some scheme ->
              let scheme_str = Type_env.scheme_to_string scheme in
              (* int-list should expand to (List Int) *)
              Alcotest.(check bool)
                "type alias expanded" true
                (try
                   let _ =
                     Str.search_forward (Str.regexp_string "List") scheme_str 0
                   in
                   true
                 with Not_found -> false))
      | _ -> Alcotest.fail "expected 1 dir")

(** Test prepend_dir and append_dir. *)
let test_prepend_append_dir () =
  with_temp_dirs 3 (fun dirs ->
      match dirs with
      | [ dir1; dir2; dir3 ] ->
          (* Create file only in dir3 *)
          write_file (Filename.concat dir3 "mod3.tart") "(defvar v int)";
          (* Create file in dir1 to test precedence *)
          write_file (Filename.concat dir1 "mod1.tart") "(defvar v int)";
          (* Start with dir2, prepend dir1, append dir3 *)
          let sp =
            Search_path.of_dirs [ dir2 ]
            |> Search_path.prepend_dir dir1
            |> Search_path.append_dir dir3
          in
          (* mod1 should be found in dir1 (prepended) *)
          let result1 = Search_path.find_signature sp "mod1" in
          Alcotest.(check (option string))
            "prepended dir searched"
            (Some (Filename.concat dir1 "mod1.tart"))
            result1;
          (* mod3 should be found in dir3 (appended) *)
          let result3 = Search_path.find_signature sp "mod3" in
          Alcotest.(check (option string))
            "appended dir searched"
            (Some (Filename.concat dir3 "mod3.tart"))
            result3
      | _ -> Alcotest.fail "expected 3 dirs")

(** {1 Version Fallback Tests (R3 of Spec 24)} *)

module V = Sig.Emacs_version

(** Test version fallback candidate generation *)
let test_version_fallback_candidates_full () =
  let v = { V.major = 31; minor = 0; patch = Some 50 } in
  let candidates = Search_path.version_fallback_candidates v in
  Alcotest.(check (list string))
    "full version candidates"
    [ "31.0.50"; "31.0"; "31"; "latest" ]
    candidates

let test_version_fallback_candidates_minor () =
  let v = { V.major = 31; minor = 0; patch = None } in
  let candidates = Search_path.version_fallback_candidates v in
  Alcotest.(check (list string))
    "minor version candidates" [ "31.0"; "31"; "latest" ] candidates

let test_version_fallback_candidates_dedup () =
  (* When major = 31 and minor = 0, "31.0" and "31" are different but "31" is deduped *)
  let v = { V.major = 30; minor = 1; patch = None } in
  let candidates = Search_path.version_fallback_candidates v in
  Alcotest.(check (list string))
    "deduplicates candidates" [ "30.1"; "30"; "latest" ] candidates

(** Test versioned typings lookup with fallback *)
let test_find_typings_dir_exact () =
  with_temp_dirs 1 (fun dirs ->
      match dirs with
      | [ typings_root ] ->
          (* Create exact version directory *)
          let version_dir = Filename.concat typings_root "31.0" in
          Unix.mkdir version_dir 0o755;
          let v = { V.major = 31; minor = 0; patch = None } in
          let result = Search_path.find_typings_dir ~typings_root ~version:v in
          Alcotest.(check (option string))
            "finds exact version" (Some version_dir) result
      | _ -> Alcotest.fail "expected 1 dir")

let test_find_typings_dir_fallback_to_latest () =
  with_temp_dirs 1 (fun dirs ->
      match dirs with
      | [ typings_root ] ->
          (* Create only latest directory, not specific version *)
          let latest_dir = Filename.concat typings_root "latest" in
          Unix.mkdir latest_dir 0o755;
          let v = { V.major = 31; minor = 0; patch = Some 50 } in
          let result = Search_path.find_typings_dir ~typings_root ~version:v in
          Alcotest.(check (option string))
            "falls back to latest" (Some latest_dir) result
      | _ -> Alcotest.fail "expected 1 dir")

let test_find_typings_dir_fallback_chain () =
  with_temp_dirs 1 (fun dirs ->
      match dirs with
      | [ typings_root ] ->
          (* Create only major version directory *)
          let major_dir = Filename.concat typings_root "31" in
          Unix.mkdir major_dir 0o755;
          let v = { V.major = 31; minor = 0; patch = Some 50 } in
          let result = Search_path.find_typings_dir ~typings_root ~version:v in
          Alcotest.(check (option string))
            "falls back to major version" (Some major_dir) result
      | _ -> Alcotest.fail "expected 1 dir")

let test_find_typings_dir_not_found () =
  with_temp_dirs 1 (fun dirs ->
      match dirs with
      | [ typings_root ] ->
          (* Don't create any version directories *)
          let v = { V.major = 31; minor = 0; patch = None } in
          let result = Search_path.find_typings_dir ~typings_root ~version:v in
          Alcotest.(check (option string))
            "returns None when not found" None result
      | _ -> Alcotest.fail "expected 1 dir")

(** Test versioned typings integration with search path *)
let test_search_path_with_versioned_typings () =
  with_temp_dirs 1 (fun dirs ->
      match dirs with
      | [ typings_root ] ->
          (* Create versioned typings structure *)
          let version_dir = Filename.concat typings_root "31.0" in
          Unix.mkdir version_dir 0o755;
          let c_core_dir = Filename.concat version_dir "c-core" in
          Unix.mkdir c_core_dir 0o755;
          write_file
            (Filename.concat c_core_dir "data.tart")
            "(defun test-func (int) -> int)";
          let v = { V.major = 31; minor = 0; patch = None } in
          let sp =
            Search_path.empty
            |> Search_path.with_typings_root typings_root
            |> Search_path.with_emacs_version v
          in
          let result = Search_path.find_signature sp "data" in
          Alcotest.(check (option string))
            "finds in versioned typings"
            (Some (Filename.concat c_core_dir "data.tart"))
            result
      | _ -> Alcotest.fail "expected 1 dir")

let test_search_dirs_take_precedence_over_typings () =
  with_temp_dirs 2 (fun dirs ->
      match dirs with
      | [ search_dir; typings_root ] ->
          (* Create in search dir *)
          write_file
            (Filename.concat search_dir "data.tart")
            "(defvar search-version int)";
          (* Create versioned typings *)
          let version_dir = Filename.concat typings_root "31.0" in
          Unix.mkdir version_dir 0o755;
          let c_core_dir = Filename.concat version_dir "c-core" in
          Unix.mkdir c_core_dir 0o755;
          write_file
            (Filename.concat c_core_dir "data.tart")
            "(defvar typings-version int)";
          let v = { V.major = 31; minor = 0; patch = None } in
          let sp =
            Search_path.of_dirs [ search_dir ]
            |> Search_path.with_typings_root typings_root
            |> Search_path.with_emacs_version v
          in
          let result = Search_path.find_signature sp "data" in
          (* Search dirs should win *)
          Alcotest.(check (option string))
            "search dirs take precedence"
            (Some (Filename.concat search_dir "data.tart"))
            result
      | _ -> Alcotest.fail "expected 2 dirs")

(** {1 Bundled C-Core Typings Tests (Spec 24 R5)} *)

(** Find the typings directory relative to test working directory *)
let typings_dir =
  let rec find_typings path =
    let candidate = Filename.concat path "typings" in
    if Sys.file_exists candidate && Sys.is_directory candidate then candidate
    else
      let parent = Filename.dirname path in
      if parent = path then "../../../../typings" else find_typings parent
  in
  find_typings (Sys.getcwd ())

(** Test that data.tart parses successfully (Spec 24 R5) *)
let test_parse_data_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/data.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("data.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "data.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "data" sig_file.sig_module;
      (* Should have many declarations: arithmetic, predicates, etc *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 50);
      (* Check specific required functions from data.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has +" true (has_defun "+");
      Alcotest.(check bool) "has -" true (has_defun "-");
      Alcotest.(check bool) "has car" true (has_defun "car");
      Alcotest.(check bool) "has cdr" true (has_defun "cdr");
      Alcotest.(check bool) "has eq" true (has_defun "eq");
      Alcotest.(check bool) "has null" true (has_defun "null");
      Alcotest.(check bool) "has symbolp" true (has_defun "symbolp");
      Alcotest.(check bool) "has integerp" true (has_defun "integerp")

(** Test that data.tart can be loaded into type environment *)
let test_load_data_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  match Search_path.load_module ~search_path:sp ~env:Type_env.empty "data" with
  | None -> Alcotest.fail "failed to load data module"
  | Some env ->
      (* Check that + is loaded *)
      (match Type_env.lookup "+" env with
      | None -> Alcotest.fail "+ not found in env"
      | Some _ -> ());
      (* Check that car is loaded *)
      (match Type_env.lookup "car" env with
      | None -> Alcotest.fail "car not found in env"
      | Some _ -> ());
      (* Check that eq is loaded *)
      (match Type_env.lookup "eq" env with
      | None -> Alcotest.fail "eq not found in env"
      | Some _ -> ());
      (* Verify type checking works with a predicate (uses Any, avoids num/int subtyping) *)
      let ty, errors = check_expr_str ~env "(null nil)" in
      Alcotest.(check int) "null no errors" 0 (List.length errors);
      let ty_str = Types.to_string ty in
      Alcotest.(check bool)
        "null returns bool" true
        (try
           let _ = Str.search_forward (Str.regexp_string "ool") ty_str 0 in
           true
         with Not_found -> false)

(** Test that fns.tart parses successfully (Spec 24 R5) *)
let test_parse_fns_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/fns.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("fns.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "fns.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "fns" sig_file.sig_module;
      (* Should have many declarations *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 50);
      (* Check specific required functions from fns.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has length" true (has_defun "length");
      Alcotest.(check bool) "has concat" true (has_defun "concat");
      Alcotest.(check bool) "has mapcar" true (has_defun "mapcar");
      Alcotest.(check bool) "has assoc" true (has_defun "assoc");
      Alcotest.(check bool) "has member" true (has_defun "member");
      Alcotest.(check bool) "has gethash" true (has_defun "gethash");
      Alcotest.(check bool) "has puthash" true (has_defun "puthash")

(** Test that fns.tart can be loaded into type environment *)
let test_load_fns_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  match Search_path.load_module ~search_path:sp ~env:Type_env.empty "fns" with
  | None -> Alcotest.fail "failed to load fns module"
  | Some env -> (
      (* Check that length is loaded *)
      (match Type_env.lookup "length" env with
      | None -> Alcotest.fail "length not found in env"
      | Some _ -> ());
      (* Check that mapcar is loaded *)
      (match Type_env.lookup "mapcar" env with
      | None -> Alcotest.fail "mapcar not found in env"
      | Some _ -> ());
      (* Check that gethash is loaded *)
      match Type_env.lookup "gethash" env with
      | None -> Alcotest.fail "gethash not found in env"
      | Some _ -> ())

let () =
  Alcotest.run "search_path"
    [
      ( "search-path-config",
        [
          Alcotest.test_case "empty search path" `Quick test_empty_search_path;
          Alcotest.test_case "search path of dirs" `Quick
            test_search_path_of_dirs;
          Alcotest.test_case "search path precedence" `Quick
            test_search_path_precedence;
          Alcotest.test_case "search path with stdlib" `Quick
            test_search_path_with_stdlib;
          Alcotest.test_case "prefers search over stdlib" `Quick
            test_search_path_prefers_search_over_stdlib;
          Alcotest.test_case "prepend and append dir" `Quick
            test_prepend_append_dir;
        ] );
      ( "sibling-files",
        [
          Alcotest.test_case "find sibling" `Quick test_find_sibling;
          Alcotest.test_case "sibling not found" `Quick
            test_find_sibling_not_found;
        ] );
      ( "discovery-order",
        [
          Alcotest.test_case "sibling takes precedence" `Quick
            test_sibling_takes_precedence;
          Alcotest.test_case "full discovery order" `Quick
            test_full_discovery_order;
        ] );
      ( "parsing",
        [
          Alcotest.test_case "parse signature file" `Quick
            test_parse_signature_file;
          Alcotest.test_case "parse invalid file" `Quick test_parse_invalid_file;
        ] );
      ( "end-to-end",
        [
          Alcotest.test_case "loaded signature type checks" `Quick
            test_loaded_signature_type_checks;
          Alcotest.test_case "resolver with open" `Quick test_resolver_with_open;
        ] );
      ( "version-fallback",
        [
          Alcotest.test_case "candidates full version" `Quick
            test_version_fallback_candidates_full;
          Alcotest.test_case "candidates minor version" `Quick
            test_version_fallback_candidates_minor;
          Alcotest.test_case "candidates dedup" `Quick
            test_version_fallback_candidates_dedup;
          Alcotest.test_case "find exact version" `Quick
            test_find_typings_dir_exact;
          Alcotest.test_case "fallback to latest" `Quick
            test_find_typings_dir_fallback_to_latest;
          Alcotest.test_case "fallback chain" `Quick
            test_find_typings_dir_fallback_chain;
          Alcotest.test_case "not found" `Quick test_find_typings_dir_not_found;
          Alcotest.test_case "versioned typings in search" `Quick
            test_search_path_with_versioned_typings;
          Alcotest.test_case "search dirs precedence" `Quick
            test_search_dirs_take_precedence_over_typings;
        ] );
      ( "bundled-c-core",
        [
          Alcotest.test_case "parse data.tart" `Quick test_parse_data_tart;
          Alcotest.test_case "load data.tart" `Quick test_load_data_tart;
          Alcotest.test_case "parse fns.tart" `Quick test_parse_fns_tart;
          Alcotest.test_case "load fns.tart" `Quick test_load_fns_tart;
        ] );
    ]
