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
          Alcotest.(check string) "result is int" "int" (Types.to_string ty1);
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
              (* int-list should expand to (list int) *)
              Alcotest.(check bool)
                "type alias expanded" true
                (try
                   let _ =
                     Str.search_forward (Str.regexp_string "list") scheme_str 0
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
      (* null returns Prim.bool which is (Or t nil) *)
      Alcotest.(check string)
        "null returns bool" "(Or t nil)" (Types.to_string ty)

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

(** Test that eval.tart parses successfully (Spec 24 R5) *)
let test_parse_eval_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/eval.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("eval.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "eval.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "eval" sig_file.sig_module;
      (* Should have many declarations *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 20);
      (* Check specific required functions from eval.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has eval" true (has_defun "eval");
      Alcotest.(check bool) "has funcall" true (has_defun "funcall");
      Alcotest.(check bool) "has apply" true (has_defun "apply");
      Alcotest.(check bool) "has throw" true (has_defun "throw");
      Alcotest.(check bool) "has signal" true (has_defun "signal");
      Alcotest.(check bool) "has macroexpand" true (has_defun "macroexpand")

(** Test that eval.tart can be loaded into type environment *)
let test_load_eval_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  match Search_path.load_module ~search_path:sp ~env:Type_env.empty "eval" with
  | None -> Alcotest.fail "failed to load eval module"
  | Some env -> (
      (* Check that eval is loaded *)
      (match Type_env.lookup "eval" env with
      | None -> Alcotest.fail "eval not found in env"
      | Some _ -> ());
      (* Check that funcall is loaded *)
      (match Type_env.lookup "funcall" env with
      | None -> Alcotest.fail "funcall not found in env"
      | Some _ -> ());
      (* Check that signal is loaded *)
      match Type_env.lookup "signal" env with
      | None -> Alcotest.fail "signal not found in env"
      | Some _ -> ())

(** Test that alloc.tart parses successfully (Spec 24 R5) *)
let test_parse_alloc_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/alloc.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("alloc.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "alloc.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "alloc" sig_file.sig_module;
      (* Should have declarations for construction and GC *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 10);
      (* Check specific required functions from alloc.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has cons" true (has_defun "cons");
      Alcotest.(check bool) "has list" true (has_defun "list");
      Alcotest.(check bool) "has vector" true (has_defun "vector");
      Alcotest.(check bool) "has make-marker" true (has_defun "make-marker");
      Alcotest.(check bool) "has make-symbol" true (has_defun "make-symbol");
      Alcotest.(check bool)
        "has garbage-collect" true
        (has_defun "garbage-collect")

(** Test that alloc.tart can be loaded into type environment *)
let test_load_alloc_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  match Search_path.load_module ~search_path:sp ~env:Type_env.empty "alloc" with
  | None -> Alcotest.fail "failed to load alloc module"
  | Some env -> (
      (* Check that cons is loaded *)
      (match Type_env.lookup "cons" env with
      | None -> Alcotest.fail "cons not found in env"
      | Some _ -> ());
      (* Check that list is loaded *)
      (match Type_env.lookup "list" env with
      | None -> Alcotest.fail "list not found in env"
      | Some _ -> ());
      (* Check that garbage-collect is loaded *)
      match Type_env.lookup "garbage-collect" env with
      | None -> Alcotest.fail "garbage-collect not found in env"
      | Some _ -> ())

(** Test that buffer.tart parses successfully (Spec 24 R5) *)
let test_parse_buffer_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/buffer.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("buffer.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "buffer.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "buffer" sig_file.sig_module;
      (* Should have declarations for buffer operations *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 30);
      (* Check specific required functions from buffer.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool)
        "has current-buffer" true
        (has_defun "current-buffer");
      Alcotest.(check bool) "has set-buffer" true (has_defun "set-buffer");
      Alcotest.(check bool) "has get-buffer" true (has_defun "get-buffer");
      Alcotest.(check bool) "has buffer-name" true (has_defun "buffer-name");
      Alcotest.(check bool)
        "has buffer-file-name" true
        (has_defun "buffer-file-name");
      Alcotest.(check bool)
        "has buffer-modified-p" true
        (has_defun "buffer-modified-p");
      Alcotest.(check bool) "has kill-buffer" true (has_defun "kill-buffer");
      Alcotest.(check bool) "has buffer-list" true (has_defun "buffer-list")

(** Test that buffer.tart can be loaded into type environment *)
let test_load_buffer_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  match
    Search_path.load_module ~search_path:sp ~env:Type_env.empty "buffer"
  with
  | None -> Alcotest.fail "failed to load buffer module"
  | Some env -> (
      (* Check that current-buffer is loaded *)
      (match Type_env.lookup "current-buffer" env with
      | None -> Alcotest.fail "current-buffer not found in env"
      | Some _ -> ());
      (* Check that set-buffer is loaded *)
      (match Type_env.lookup "set-buffer" env with
      | None -> Alcotest.fail "set-buffer not found in env"
      | Some _ -> ());
      (* Check that kill-buffer is loaded *)
      match Type_env.lookup "kill-buffer" env with
      | None -> Alcotest.fail "kill-buffer not found in env"
      | Some _ -> ())

(** Test that window.tart parses successfully (Spec 24 R5) *)
let test_parse_window_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/window.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("window.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "window.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "window" sig_file.sig_module;
      (* Should have declarations for window operations *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 50);
      (* Check specific required functions from window.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool)
        "has selected-window" true
        (has_defun "selected-window");
      Alcotest.(check bool) "has select-window" true (has_defun "select-window");
      Alcotest.(check bool) "has window-buffer" true (has_defun "window-buffer");
      Alcotest.(check bool) "has window-point" true (has_defun "window-point");
      Alcotest.(check bool) "has window-start" true (has_defun "window-start");
      Alcotest.(check bool) "has delete-window" true (has_defun "delete-window");
      Alcotest.(check bool) "has split-window" true (has_defun "split-window");
      Alcotest.(check bool) "has next-window" true (has_defun "next-window")

(** Test that window.tart can be loaded into type environment *)
let test_load_window_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  (* First check if the signature validates *)
  let path = Filename.concat typings_dir "emacs/31.0/c-core/window.tart" in
  (match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "window.tart failed to parse in load test"
  | Some sig_file -> (
      match Sig_loader.validate_signature sig_file with
      | Ok () -> ()
      | Error err ->
          Alcotest.fail
            ("window.tart validation failed: " ^ err.message ^ " at "
            ^ Syntax.Location.show_span err.span)));
  match
    Search_path.load_module ~search_path:sp ~env:Type_env.empty "window"
  with
  | None -> Alcotest.fail "failed to load window module"
  | Some env -> (
      (* Check that selected-window is loaded *)
      (match Type_env.lookup "selected-window" env with
      | None -> Alcotest.fail "selected-window not found in env"
      | Some _ -> ());
      (* Check that window-buffer is loaded *)
      (match Type_env.lookup "window-buffer" env with
      | None -> Alcotest.fail "window-buffer not found in env"
      | Some _ -> ());
      (* Check that delete-window is loaded *)
      match Type_env.lookup "delete-window" env with
      | None -> Alcotest.fail "delete-window not found in env"
      | Some _ -> ())

(** Test that frame.tart parses successfully (Spec 24 R5) *)
let test_parse_frame_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/frame.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("frame.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "frame.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "frame" sig_file.sig_module;
      (* Should have declarations for frame operations *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 30);
      (* Check specific required functions from frame.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool)
        "has selected-frame" true
        (has_defun "selected-frame");
      Alcotest.(check bool) "has select-frame" true (has_defun "select-frame");
      Alcotest.(check bool) "has make-frame" true (has_defun "make-frame");
      Alcotest.(check bool) "has delete-frame" true (has_defun "delete-frame");
      Alcotest.(check bool) "has frame-list" true (has_defun "frame-list");
      Alcotest.(check bool)
        "has frame-parameters" true
        (has_defun "frame-parameters");
      Alcotest.(check bool) "has raise-frame" true (has_defun "raise-frame");
      Alcotest.(check bool) "has frame-parent" true (has_defun "frame-parent")

(** Test that frame.tart can be loaded into type environment *)
let test_load_frame_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  (* First check if the signature validates *)
  let path = Filename.concat typings_dir "emacs/31.0/c-core/frame.tart" in
  (match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "frame.tart failed to parse in load test"
  | Some sig_file -> (
      match Sig_loader.validate_signature sig_file with
      | Ok () -> ()
      | Error err ->
          Alcotest.fail
            ("frame.tart validation failed: " ^ err.message ^ " at "
            ^ Syntax.Location.show_span err.span)));
  match Search_path.load_module ~search_path:sp ~env:Type_env.empty "frame" with
  | None -> Alcotest.fail "failed to load frame module"
  | Some env -> (
      (* Check that selected-frame is loaded *)
      (match Type_env.lookup "selected-frame" env with
      | None -> Alcotest.fail "selected-frame not found in env"
      | Some _ -> ());
      (* Check that make-frame is loaded *)
      (match Type_env.lookup "make-frame" env with
      | None -> Alcotest.fail "make-frame not found in env"
      | Some _ -> ());
      (* Check that delete-frame is loaded *)
      match Type_env.lookup "delete-frame" env with
      | None -> Alcotest.fail "delete-frame not found in env"
      | Some _ -> ())

(** Test that fileio.tart parses successfully (Spec 24 R5) *)
let test_parse_fileio_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/fileio.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("fileio.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "fileio.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "fileio" sig_file.sig_module;
      (* Should have declarations for file operations *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 40);
      (* Check specific required functions from fileio.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool)
        "has expand-file-name" true
        (has_defun "expand-file-name");
      Alcotest.(check bool) "has file-exists-p" true (has_defun "file-exists-p");
      Alcotest.(check bool)
        "has file-name-directory" true
        (has_defun "file-name-directory");
      Alcotest.(check bool) "has write-region" true (has_defun "write-region");
      Alcotest.(check bool)
        "has insert-file-contents" true
        (has_defun "insert-file-contents");
      Alcotest.(check bool) "has rename-file" true (has_defun "rename-file");
      Alcotest.(check bool)
        "has delete-file-internal" true
        (has_defun "delete-file-internal");
      Alcotest.(check bool) "has copy-file" true (has_defun "copy-file")

(** Test that fileio.tart can be loaded into type environment *)
let test_load_fileio_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  (* First check if the signature validates *)
  let path = Filename.concat typings_dir "emacs/31.0/c-core/fileio.tart" in
  (match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "fileio.tart failed to parse in load test"
  | Some sig_file -> (
      match Sig_loader.validate_signature sig_file with
      | Ok () -> ()
      | Error err ->
          Alcotest.fail
            ("fileio.tart validation failed: " ^ err.message ^ " at "
            ^ Syntax.Location.show_span err.span)));
  match
    Search_path.load_module ~search_path:sp ~env:Type_env.empty "fileio"
  with
  | None -> Alcotest.fail "failed to load fileio module"
  | Some env -> (
      (* Check that expand-file-name is loaded *)
      (match Type_env.lookup "expand-file-name" env with
      | None -> Alcotest.fail "expand-file-name not found in env"
      | Some _ -> ());
      (* Check that file-exists-p is loaded *)
      (match Type_env.lookup "file-exists-p" env with
      | None -> Alcotest.fail "file-exists-p not found in env"
      | Some _ -> ());
      (* Check that write-region is loaded *)
      match Type_env.lookup "write-region" env with
      | None -> Alcotest.fail "write-region not found in env"
      | Some _ -> ())

(** Test that editfns.tart parses successfully (Spec 24 R5) *)
let test_parse_editfns_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/editfns.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("editfns.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "editfns.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "editfns" sig_file.sig_module;
      (* Should have declarations for editing operations *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 50);
      (* Check specific required functions from editfns.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has point" true (has_defun "point");
      Alcotest.(check bool) "has point-min" true (has_defun "point-min");
      Alcotest.(check bool) "has point-max" true (has_defun "point-max");
      Alcotest.(check bool) "has goto-char" true (has_defun "goto-char");
      Alcotest.(check bool) "has insert" true (has_defun "insert");
      Alcotest.(check bool) "has delete-region" true (has_defun "delete-region");
      Alcotest.(check bool)
        "has buffer-substring" true
        (has_defun "buffer-substring");
      Alcotest.(check bool) "has format" true (has_defun "format")

(** Test that editfns.tart can be loaded into type environment *)
let test_load_editfns_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  (* First check if the signature validates *)
  let path = Filename.concat typings_dir "emacs/31.0/c-core/editfns.tart" in
  (match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "editfns.tart failed to parse in load test"
  | Some sig_file -> (
      match Sig_loader.validate_signature sig_file with
      | Ok () -> ()
      | Error err ->
          Alcotest.fail
            ("editfns.tart validation failed: " ^ err.message ^ " at "
            ^ Syntax.Location.show_span err.span)));
  match
    Search_path.load_module ~search_path:sp ~env:Type_env.empty "editfns"
  with
  | None -> Alcotest.fail "failed to load editfns module"
  | Some env -> (
      (* Check that point is loaded *)
      (match Type_env.lookup "point" env with
      | None -> Alcotest.fail "point not found in env"
      | Some _ -> ());
      (* Check that insert is loaded *)
      (match Type_env.lookup "insert" env with
      | None -> Alcotest.fail "insert not found in env"
      | Some _ -> ());
      (* Check that format is loaded *)
      match Type_env.lookup "format" env with
      | None -> Alcotest.fail "format not found in env"
      | Some _ -> ())

(** Test that search.tart parses successfully (Spec 24 R5) *)
let test_parse_search_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/search.tart" in
  if not (Sys.file_exists path) then Alcotest.skip ();
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "search.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "search" sig_file.sig_module;
      (* Should have declarations for search operations (17 defuns in search.c) *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls >= 15);
      (* Check specific required functions from search.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool)
        "has re-search-forward" true
        (has_defun "re-search-forward");
      Alcotest.(check bool)
        "has re-search-backward" true
        (has_defun "re-search-backward");
      Alcotest.(check bool) "has looking-at" true (has_defun "looking-at");
      Alcotest.(check bool) "has string-match" true (has_defun "string-match");
      (* match-string is in subr.el, not search.c *)
      Alcotest.(check bool)
        "has match-beginning" true
        (has_defun "match-beginning");
      Alcotest.(check bool) "has replace-match" true (has_defun "replace-match")

(** Test that search.tart can be loaded into type environment *)
let test_load_search_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  (* First check if the signature validates *)
  let path = Filename.concat typings_dir "emacs/31.0/c-core/search.tart" in
  if not (Sys.file_exists path) then Alcotest.skip ();
  (match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "search.tart failed to parse in load test"
  | Some sig_file -> (
      match Sig_loader.validate_signature sig_file with
      | Ok () -> ()
      | Error err ->
          Alcotest.fail
            ("search.tart validation failed: " ^ err.message ^ " at "
            ^ Syntax.Location.show_span err.span)));
  match
    Search_path.load_module ~search_path:sp ~env:Type_env.empty "search"
  with
  | None -> Alcotest.fail "failed to load search module"
  | Some env -> (
      (* Check that re-search-forward is loaded *)
      (match Type_env.lookup "re-search-forward" env with
      | None -> Alcotest.fail "re-search-forward not found in env"
      | Some _ -> ());
      (* Check that match-beginning is loaded (match-string is in subr.el) *)
      (match Type_env.lookup "match-beginning" env with
      | None -> Alcotest.fail "match-beginning not found in env"
      | Some _ -> ());
      (* Check that looking-at is loaded *)
      match Type_env.lookup "looking-at" env with
      | None -> Alcotest.fail "looking-at not found in env"
      | Some _ -> ())

(** Test that process.tart parses successfully (Spec 24 R5) *)
let test_parse_process_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/process.tart" in
  if not (Sys.file_exists path) then Alcotest.skip ();
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "process.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "process" sig_file.sig_module;
      (* Should have declarations for process operations (64 defuns in process.c) *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls >= 50);
      (* Check specific required functions from process.c *)
      (* Note: start-process and call-process are in callproc.c, not process.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has make-process" true (has_defun "make-process");
      Alcotest.(check bool)
        "has process-send-string" true
        (has_defun "process-send-string");
      Alcotest.(check bool) "has processp" true (has_defun "processp");
      Alcotest.(check bool) "has process-list" true (has_defun "process-list");
      Alcotest.(check bool)
        "has delete-process" true
        (has_defun "delete-process");
      Alcotest.(check bool)
        "has process-status" true
        (has_defun "process-status")

(** Test that process.tart can be loaded into type environment *)
let test_load_process_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  (* First check if the signature validates *)
  let path = Filename.concat typings_dir "emacs/31.0/c-core/process.tart" in
  if not (Sys.file_exists path) then Alcotest.skip ();
  (match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "process.tart failed to parse in load test"
  | Some sig_file -> (
      match Sig_loader.validate_signature sig_file with
      | Ok () -> ()
      | Error err ->
          Alcotest.fail
            ("process.tart validation failed: " ^ err.message ^ " at "
            ^ Syntax.Location.show_span err.span)));
  match
    Search_path.load_module ~search_path:sp ~env:Type_env.empty "process"
  with
  | None -> Alcotest.fail "failed to load process module"
  | Some env -> (
      (* Check that make-process is loaded (start-process is in callproc.c) *)
      (match Type_env.lookup "make-process" env with
      | None -> Alcotest.fail "make-process not found in env"
      | Some _ -> ());
      (* Check that process-send-string is loaded *)
      (match Type_env.lookup "process-send-string" env with
      | None -> Alcotest.fail "process-send-string not found in env"
      | Some _ -> ());
      (* Check that processp is loaded (call-process is in callproc.c) *)
      match Type_env.lookup "processp" env with
      | None -> Alcotest.fail "processp not found in env"
      | Some _ -> ())

(** Test that keyboard.tart parses successfully (Spec 24 R5) *)
let test_parse_keyboard_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/keyboard.tart" in
  if not (Sys.file_exists path) then Alcotest.skip ();
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "keyboard.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "keyboard" sig_file.sig_module;
      (* Should have declarations for keyboard operations (37 defuns in keyboard.c) *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls >= 30);
      (* Check specific required functions from keyboard.c *)
      (* Note: sit-for is in dispnew.c, not keyboard.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool)
        "has read-key-sequence" true
        (has_defun "read-key-sequence");
      Alcotest.(check bool) "has read-event" true (has_defun "read-event");
      Alcotest.(check bool) "has read-char" true (has_defun "read-char");
      Alcotest.(check bool)
        "has this-command-keys" true
        (has_defun "this-command-keys");
      Alcotest.(check bool)
        "has recursive-edit" true
        (has_defun "recursive-edit");
      Alcotest.(check bool)
        "has input-pending-p" true
        (has_defun "input-pending-p");
      Alcotest.(check bool) "has discard-input" true (has_defun "discard-input")

(** Test that keyboard.tart can be loaded into type environment *)
let test_load_keyboard_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  (* First check if the signature validates *)
  let path = Filename.concat typings_dir "emacs/31.0/c-core/keyboard.tart" in
  if not (Sys.file_exists path) then Alcotest.skip ();
  (match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "keyboard.tart failed to parse in load test"
  | Some sig_file -> (
      match Sig_loader.validate_signature sig_file with
      | Ok () -> ()
      | Error err ->
          Alcotest.fail
            ("keyboard.tart validation failed: " ^ err.message ^ " at "
            ^ Syntax.Location.show_span err.span)));
  match
    Search_path.load_module ~search_path:sp ~env:Type_env.empty "keyboard"
  with
  | None -> Alcotest.fail "failed to load keyboard module"
  | Some env -> (
      (* Check that read-key-sequence is loaded *)
      (match Type_env.lookup "read-key-sequence" env with
      | None -> Alcotest.fail "read-key-sequence not found in env"
      | Some _ -> ());
      (* Check that read-event is loaded *)
      (match Type_env.lookup "read-event" env with
      | None -> Alcotest.fail "read-event not found in env"
      | Some _ -> ());
      (* Check that discard-input is loaded (sit-for is in dispnew.c) *)
      match Type_env.lookup "discard-input" env with
      | None -> Alcotest.fail "discard-input not found in env"
      | Some _ -> ())

(** Test that keymap.tart parses successfully (Spec 24 R5) *)
let test_parse_keymap_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/keymap.tart" in
  if not (Sys.file_exists path) then Alcotest.skip ();
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "keymap.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "keymap" sig_file.sig_module;
      (* Should have declarations for keymap operations *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 20);
      (* Check specific required functions from keymap.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has keymapp" true (has_defun "keymapp");
      Alcotest.(check bool) "has make-keymap" true (has_defun "make-keymap");
      Alcotest.(check bool)
        "has make-sparse-keymap" true
        (has_defun "make-sparse-keymap");
      Alcotest.(check bool) "has define-key" true (has_defun "define-key");
      Alcotest.(check bool) "has lookup-key" true (has_defun "lookup-key");
      Alcotest.(check bool) "has key-binding" true (has_defun "key-binding");
      Alcotest.(check bool) "has keymap-parent" true (has_defun "keymap-parent")

(** Test that keymap.tart can be loaded into type environment *)
let test_load_keymap_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  (* First check if the signature validates *)
  let path = Filename.concat typings_dir "emacs/31.0/c-core/keymap.tart" in
  if not (Sys.file_exists path) then Alcotest.skip ();
  (match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "keymap.tart failed to parse in load test"
  | Some sig_file -> (
      match Sig_loader.validate_signature sig_file with
      | Ok () -> ()
      | Error err ->
          Alcotest.fail
            ("keymap.tart validation failed: " ^ err.message ^ " at "
            ^ Syntax.Location.show_span err.span)));
  match
    Search_path.load_module ~search_path:sp ~env:Type_env.empty "keymap"
  with
  | None -> Alcotest.fail "failed to load keymap module"
  | Some env -> (
      (* Check that keymapp is loaded *)
      (match Type_env.lookup "keymapp" env with
      | None -> Alcotest.fail "keymapp not found in env"
      | Some _ -> ());
      (* Check that define-key is loaded *)
      (match Type_env.lookup "define-key" env with
      | None -> Alcotest.fail "define-key not found in env"
      | Some _ -> ());
      (* Check that lookup-key is loaded *)
      match Type_env.lookup "lookup-key" env with
      | None -> Alcotest.fail "lookup-key not found in env"
      | Some _ -> ())

(** Test that minibuf.tart parses successfully (Spec 24 R5) *)
let test_parse_minibuf_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/minibuf.tart" in
  if not (Sys.file_exists path) then Alcotest.skip ();
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "minibuf.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "minibuf" sig_file.sig_module;
      (* Should have declarations for minibuffer operations (22 defuns in minibuf.c) *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls >= 15);
      (* Check specific required functions from minibuf.c *)
      (* Note: yes-or-no-p and y-or-n-p are in fns.c, not minibuf.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool)
        "has read-from-minibuffer" true
        (has_defun "read-from-minibuffer");
      Alcotest.(check bool) "has read-string" true (has_defun "read-string");
      Alcotest.(check bool)
        "has completing-read" true
        (has_defun "completing-read");
      Alcotest.(check bool)
        "has try-completion" true
        (has_defun "try-completion");
      Alcotest.(check bool)
        "has all-completions" true
        (has_defun "all-completions");
      Alcotest.(check bool) "has minibufferp" true (has_defun "minibufferp");
      Alcotest.(check bool)
        "has active-minibuffer-window" true
        (has_defun "active-minibuffer-window")

(** Test that minibuf.tart can be loaded into type environment *)
let test_load_minibuf_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  (* First check if the signature validates *)
  let path = Filename.concat typings_dir "emacs/31.0/c-core/minibuf.tart" in
  if not (Sys.file_exists path) then Alcotest.skip ();
  (match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "minibuf.tart failed to parse in load test"
  | Some sig_file -> (
      match Sig_loader.validate_signature sig_file with
      | Ok () -> ()
      | Error err ->
          Alcotest.fail
            ("minibuf.tart validation failed: " ^ err.message ^ " at "
            ^ Syntax.Location.show_span err.span)));
  match
    Search_path.load_module ~search_path:sp ~env:Type_env.empty "minibuf"
  with
  | None -> Alcotest.fail "failed to load minibuf module"
  | Some env -> (
      (* Check that read-string is loaded *)
      (match Type_env.lookup "read-string" env with
      | None -> Alcotest.fail "read-string not found in env"
      | Some _ -> ());
      (* Check that completing-read is loaded *)
      (match Type_env.lookup "completing-read" env with
      | None -> Alcotest.fail "completing-read not found in env"
      | Some _ -> ());
      (* Check that try-completion is loaded (yes-or-no-p is in fns.c) *)
      match Type_env.lookup "try-completion" env with
      | None -> Alcotest.fail "try-completion not found in env"
      | Some _ -> ())

(** Test that textprop.tart parses successfully (Spec 24 R5) *)
let test_parse_textprop_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/textprop.tart" in
  if not (Sys.file_exists path) then Alcotest.skip ();
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "textprop.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "textprop" sig_file.sig_module;
      (* Should have declarations for text property operations (20 defuns in textprop.c) *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls >= 15);
      (* Check specific required functions from textprop.c *)
      (* Note: propertize is in fns.c, not textprop.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool)
        "has get-text-property" true
        (has_defun "get-text-property");
      Alcotest.(check bool)
        "has put-text-property" true
        (has_defun "put-text-property");
      Alcotest.(check bool)
        "has add-text-properties" true
        (has_defun "add-text-properties");
      Alcotest.(check bool)
        "has remove-text-properties" true
        (has_defun "remove-text-properties");
      Alcotest.(check bool)
        "has next-property-change" true
        (has_defun "next-property-change");
      Alcotest.(check bool)
        "has text-property-any" true
        (has_defun "text-property-any")

(** Test that textprop.tart can be loaded into type environment *)
let test_load_textprop_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  (* First check if the signature validates *)
  let path = Filename.concat typings_dir "emacs/31.0/c-core/textprop.tart" in
  if not (Sys.file_exists path) then Alcotest.skip ();
  (match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "textprop.tart failed to parse in load test"
  | Some sig_file -> (
      match Sig_loader.validate_signature sig_file with
      | Ok () -> ()
      | Error err ->
          Alcotest.fail
            ("textprop.tart validation failed: " ^ err.message ^ " at "
            ^ Syntax.Location.show_span err.span)));
  match
    Search_path.load_module ~search_path:sp ~env:Type_env.empty "textprop"
  with
  | None -> Alcotest.fail "failed to load textprop module"
  | Some env -> (
      (* Check that get-text-property is loaded *)
      (match Type_env.lookup "get-text-property" env with
      | None -> Alcotest.fail "get-text-property not found in env"
      | Some _ -> ());
      (* Check that put-text-property is loaded *)
      (match Type_env.lookup "put-text-property" env with
      | None -> Alcotest.fail "put-text-property not found in env"
      | Some _ -> ());
      (* Check that text-property-any is loaded (propertize is in fns.c) *)
      match Type_env.lookup "text-property-any" env with
      | None -> Alcotest.fail "text-property-any not found in env"
      | Some _ -> ())

(** Test that print.tart parses successfully (Spec 24 R5) *)
let test_parse_print_tart () =
  let path = Filename.concat typings_dir "emacs/31.0/c-core/print.tart" in
  if not (Sys.file_exists path) then Alcotest.skip ();
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "print.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "print" sig_file.sig_module;
      (* Should have declarations for print operations (11 defuns in print.c) *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls >= 8);
      (* Check specific required functions from print.c *)
      (* Note: message and format are in editfns.c, not print.c *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has prin1" true (has_defun "prin1");
      Alcotest.(check bool) "has princ" true (has_defun "princ");
      Alcotest.(check bool) "has print" true (has_defun "print");
      Alcotest.(check bool) "has write-char" true (has_defun "write-char");
      Alcotest.(check bool) "has terpri" true (has_defun "terpri");
      Alcotest.(check bool)
        "has prin1-to-string" true
        (has_defun "prin1-to-string")

(** Test that print.tart can be loaded into type environment *)
let test_load_print_tart () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let sp = Search_path.of_dirs [ c_core_dir ] in
  (* First check if the signature validates *)
  let path = Filename.concat typings_dir "emacs/31.0/c-core/print.tart" in
  if not (Sys.file_exists path) then Alcotest.skip ();
  (match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "print.tart failed to parse in load test"
  | Some sig_file -> (
      match Sig_loader.validate_signature sig_file with
      | Ok () -> ()
      | Error err ->
          Alcotest.fail
            ("print.tart validation failed: " ^ err.message ^ " at "
            ^ Syntax.Location.show_span err.span)));
  match Search_path.load_module ~search_path:sp ~env:Type_env.empty "print" with
  | None -> Alcotest.fail "failed to load print module"
  | Some env -> (
      (* Check that prin1 is loaded *)
      (match Type_env.lookup "prin1" env with
      | None -> Alcotest.fail "prin1 not found in env"
      | Some _ -> ());
      (* Check that write-char is loaded (message is in editfns.c) *)
      (match Type_env.lookup "write-char" env with
      | None -> Alcotest.fail "write-char not found in env"
      | Some _ -> ());
      (* Check that terpri is loaded (format is in editfns.c) *)
      match Type_env.lookup "terpri" env with
      | None -> Alcotest.fail "terpri not found in env"
      | Some _ -> ())

(** Test that list_c_core_files finds all c-core files *)
let test_list_c_core_files () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let files = Search_path.list_c_core_files c_core_dir in
  (* Should have c-core files (9 created so far, eventually 16) *)
  Alcotest.(check bool) "has at least 9 files" true (List.length files >= 9);
  (* Should include data.tart *)
  Alcotest.(check bool)
    "includes data.tart" true
    (List.exists (fun f -> Filename.basename f = "data.tart") files);
  (* Should include fileio.tart *)
  Alcotest.(check bool)
    "includes fileio.tart" true
    (List.exists (fun f -> Filename.basename f = "fileio.tart") files)

(** Test that load_c_core_files loads all c-core signatures *)
let test_load_c_core_files () =
  let c_core_dir = Filename.concat typings_dir "emacs/31.0/c-core" in
  let env = Search_path.load_c_core_files ~c_core_dir Type_env.empty in
  (* Should have functions from data.tart *)
  (match Type_env.lookup "car" env with
  | None -> Alcotest.fail "car not found in merged env"
  | Some _ -> ());
  (* Should have functions from fns.tart *)
  (match Type_env.lookup "mapcar" env with
  | None -> Alcotest.fail "mapcar not found in merged env"
  | Some _ -> ());
  (* Should have functions from print.tart *)
  (match Type_env.lookup "message" env with
  | None -> Alcotest.fail "message not found in merged env"
  | Some _ -> ());
  (* Should have functions from buffer.tart *)
  (match Type_env.lookup "current-buffer" env with
  | None -> Alcotest.fail "current-buffer not found in merged env"
  | Some _ -> ());
  (* Should have functions from fileio.tart *)
  match Type_env.lookup "file-exists-p" env with
  | None -> Alcotest.fail "file-exists-p not found in merged env"
  | Some _ -> ()

(** Test that load_c_core works with search path config *)
let test_load_c_core () =
  let version =
    match Emacs_version.parse_version "31.0" with
    | Some v -> v
    | None -> Alcotest.fail "Failed to parse version"
  in
  let emacs_typings = Filename.concat typings_dir "emacs" in
  let sp =
    Search_path.empty
    |> Search_path.with_typings_root emacs_typings
    |> Search_path.with_emacs_version version
  in
  let env = Search_path.load_c_core ~search_path:sp Type_env.empty in
  (* Should have functions from multiple c-core files *)
  (match Type_env.lookup "car" env with
  | None -> Alcotest.fail "car not found via load_c_core"
  | Some _ -> ());
  (match Type_env.lookup "mapcar" env with
  | None -> Alcotest.fail "mapcar not found via load_c_core"
  | Some _ -> ());
  (* Should have functions from fileio.tart *)
  match Type_env.lookup "expand-file-name" env with
  | None -> Alcotest.fail "expand-file-name not found via load_c_core"
  | Some _ -> ()

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
          Alcotest.test_case "parse eval.tart" `Quick test_parse_eval_tart;
          Alcotest.test_case "load eval.tart" `Quick test_load_eval_tart;
          Alcotest.test_case "parse alloc.tart" `Quick test_parse_alloc_tart;
          Alcotest.test_case "load alloc.tart" `Quick test_load_alloc_tart;
          Alcotest.test_case "parse buffer.tart" `Quick test_parse_buffer_tart;
          Alcotest.test_case "load buffer.tart" `Quick test_load_buffer_tart;
          Alcotest.test_case "parse window.tart" `Quick test_parse_window_tart;
          Alcotest.test_case "load window.tart" `Quick test_load_window_tart;
          Alcotest.test_case "parse frame.tart" `Quick test_parse_frame_tart;
          Alcotest.test_case "load frame.tart" `Quick test_load_frame_tart;
          Alcotest.test_case "parse fileio.tart" `Quick test_parse_fileio_tart;
          Alcotest.test_case "load fileio.tart" `Quick test_load_fileio_tart;
          Alcotest.test_case "parse editfns.tart" `Quick test_parse_editfns_tart;
          Alcotest.test_case "load editfns.tart" `Quick test_load_editfns_tart;
          Alcotest.test_case "parse search.tart" `Quick test_parse_search_tart;
          Alcotest.test_case "load search.tart" `Quick test_load_search_tart;
          Alcotest.test_case "parse process.tart" `Quick test_parse_process_tart;
          Alcotest.test_case "load process.tart" `Quick test_load_process_tart;
          Alcotest.test_case "parse keyboard.tart" `Quick
            test_parse_keyboard_tart;
          Alcotest.test_case "load keyboard.tart" `Quick test_load_keyboard_tart;
          Alcotest.test_case "parse keymap.tart" `Quick test_parse_keymap_tart;
          Alcotest.test_case "load keymap.tart" `Quick test_load_keymap_tart;
          Alcotest.test_case "parse minibuf.tart" `Quick test_parse_minibuf_tart;
          Alcotest.test_case "load minibuf.tart" `Quick test_load_minibuf_tart;
          Alcotest.test_case "parse textprop.tart" `Quick
            test_parse_textprop_tart;
          Alcotest.test_case "load textprop.tart" `Quick test_load_textprop_tart;
          Alcotest.test_case "parse print.tart" `Quick test_parse_print_tart;
          Alcotest.test_case "load print.tart" `Quick test_load_print_tart;
        ] );
      ( "c-core-loading",
        [
          Alcotest.test_case "list c-core files" `Quick test_list_c_core_files;
          Alcotest.test_case "load c-core files" `Quick test_load_c_core_files;
          Alcotest.test_case "load c-core via search path" `Quick
            test_load_c_core;
        ] );
    ]
