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

(** {1 Bundled Stdlib Tests (R17)} *)

(** Path to the bundled stdlib directory. We use a relative path from the test
    working directory. *)
let stdlib_dir =
  (* dune runs tests from _build/default/test/sig, so go up to project root *)
  let rec find_stdlib path =
    let candidate = Filename.concat path "stdlib" in
    if Sys.file_exists candidate && Sys.is_directory candidate then candidate
    else
      let parent = Filename.dirname path in
      if parent = path then
        (* At root, try relative to cwd *)
        "../../../../stdlib"
      else find_stdlib parent
  in
  find_stdlib (Sys.getcwd ())

(** Test that builtins.tart parses successfully (R17) *)
let test_parse_builtins () =
  let path = Filename.concat stdlib_dir "builtins.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("builtins.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "builtins.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "builtins" sig_file.sig_module;
      (* Should have many declarations: arithmetic, lists, strings, etc *)
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 20);
      (* Check specific required functions from R17 *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has +" true (has_defun "+");
      Alcotest.(check bool) "has car" true (has_defun "car");
      Alcotest.(check bool) "has concat" true (has_defun "concat");
      Alcotest.(check bool) "has stringp" true (has_defun "stringp");
      Alcotest.(check bool) "has error" true (has_defun "error")

(** Test that cl-lib.tart parses successfully (R17) *)
let test_parse_cl_lib () =
  let path = Filename.concat stdlib_dir "cl-lib.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("cl-lib.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "cl-lib.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "cl-lib" sig_file.sig_module;
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 10);
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has cl-mapcar" true (has_defun "cl-mapcar");
      Alcotest.(check bool) "has cl-reduce" true (has_defun "cl-reduce")

(** Test that seq.tart parses successfully (R17) *)
let test_parse_seq () =
  let path = Filename.concat stdlib_dir "seq.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("seq.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "seq.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "seq" sig_file.sig_module;
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 10);
      (* seq.tart has the seq type alias *)
      let has_type name =
        List.exists
          (function Sig_ast.DType t -> t.type_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has seq type" true (has_type "seq");
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has seq-map" true (has_defun "seq-map")

(** Test that buffers.tart parses successfully *)
let test_parse_buffers () =
  let path = Filename.concat stdlib_dir "buffers.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("buffers.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "buffers.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "buffers" sig_file.sig_module;
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 30);
      (* buffers.tart should have the buffer opaque type *)
      let has_type name =
        List.exists
          (function Sig_ast.DType t -> t.type_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has buffer type" true (has_type "buffer");
      Alcotest.(check bool) "has marker type" true (has_type "marker");
      (* Check key buffer functions *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool)
        "has current-buffer" true
        (has_defun "current-buffer");
      Alcotest.(check bool) "has get-buffer" true (has_defun "get-buffer");
      Alcotest.(check bool) "has point" true (has_defun "point");
      Alcotest.(check bool) "has insert" true (has_defun "insert");
      Alcotest.(check bool)
        "has search-forward" true
        (has_defun "search-forward")

(** Test that buffers can be loaded into type environment *)
let test_load_buffers () =
  let sp = Search_path.empty |> Search_path.with_stdlib stdlib_dir in
  match
    Search_path.load_module ~search_path:sp ~env:Type_env.empty "buffers"
  with
  | None -> Alcotest.fail "failed to load buffers module"
  | Some env ->
      (* Check that current-buffer is loaded *)
      (match Type_env.lookup "current-buffer" env with
      | None -> Alcotest.fail "current-buffer not found in env"
      | Some _ -> ());
      (* Check that point is loaded *)
      (match Type_env.lookup "point" env with
      | None -> Alcotest.fail "point not found in env"
      | Some _ -> ());
      (* Check that insert is loaded *)
      (match Type_env.lookup "insert" env with
      | None -> Alcotest.fail "insert not found in env"
      | Some _ -> ());
      (* Verify type checking works with buffer functions *)
      let _, errors = check_expr_str ~env "(point)" in
      Alcotest.(check int) "point: no errors" 0 (List.length errors);
      let ty, errors2 = check_expr_str ~env "(get-buffer \"test\")" in
      Alcotest.(check int) "get-buffer: no errors" 0 (List.length errors2);
      (* Result should be (Buffer | Nil) *)
      let ty_str = Types.to_string ty in
      Alcotest.(check bool)
        "returns optional buffer" true
        (try
           let _ = Str.search_forward (Str.regexp_string "Nil") ty_str 0 in
           true
         with Not_found -> false)

(** Test that windows.tart parses successfully *)
let test_parse_windows () =
  let path = Filename.concat stdlib_dir "windows.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("windows.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "windows.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "windows" sig_file.sig_module;
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 30);
      (* windows.tart should have the window opaque type *)
      let has_type name =
        List.exists
          (function Sig_ast.DType t -> t.type_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has window type" true (has_type "window");
      Alcotest.(check bool) "has frame type" true (has_type "frame");
      (* Check key window functions *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool)
        "has selected-window" true
        (has_defun "selected-window");
      Alcotest.(check bool)
        "has get-buffer-window" true
        (has_defun "get-buffer-window");
      Alcotest.(check bool) "has window-list" true (has_defun "window-list");
      Alcotest.(check bool) "has split-window" true (has_defun "split-window")

(** Test that windows can be loaded into type environment *)
let test_load_windows () =
  let sp = Search_path.empty |> Search_path.with_stdlib stdlib_dir in
  (* First load builtins and buffers for dependency types *)
  let base_env =
    match
      Search_path.load_module ~search_path:sp ~env:Type_env.empty "builtins"
    with
    | None -> Alcotest.fail "failed to load builtins module"
    | Some env -> env
  in
  let base_env =
    match Search_path.load_module ~search_path:sp ~env:base_env "buffers" with
    | None -> Alcotest.fail "failed to load buffers module"
    | Some env -> env
  in
  match Search_path.load_module ~search_path:sp ~env:base_env "windows" with
  | None -> Alcotest.fail "failed to load windows module"
  | Some env ->
      (* Check that selected-window is loaded *)
      (match Type_env.lookup "selected-window" env with
      | None -> Alcotest.fail "selected-window not found in env"
      | Some _ -> ());
      (* Check that window-buffer is loaded *)
      (match Type_env.lookup "window-buffer" env with
      | None -> Alcotest.fail "window-buffer not found in env"
      | Some _ -> ());
      (* Check that split-window is loaded *)
      (match Type_env.lookup "split-window" env with
      | None -> Alcotest.fail "split-window not found in env"
      | Some _ -> ());
      (* Verify type checking works with window functions *)
      let _, errors = check_expr_str ~env "(selected-window)" in
      Alcotest.(check int) "selected-window: no errors" 0 (List.length errors);
      (* Check window-buffer returns buffer type (also tests optional param with 0 args) *)
      let ty, errors2 = check_expr_str ~env "(window-buffer)" in
      Alcotest.(check int) "window-buffer: no errors" 0 (List.length errors2);
      (* Result should be Buffer (the opened type from buffers.tart) *)
      let ty_str = Types.to_string ty in
      Alcotest.(check bool)
        "returns buffer type" true
        (try
           let _ = Str.search_forward (Str.regexp_string "uffer") ty_str 0 in
           true
         with Not_found -> false)

(** Test that builtins can be loaded into type environment (R17) Verify:
    Built-in calls type-check; coverage test for all listed functions *)
let test_load_builtins () =
  let sp = Search_path.empty |> Search_path.with_stdlib stdlib_dir in
  match
    Search_path.load_module ~search_path:sp ~env:Type_env.empty "builtins"
  with
  | None -> Alcotest.fail "failed to load builtins module"
  | Some env -> (
      (* Check that + is loaded *)
      (match Type_env.lookup "+" env with
      | None -> Alcotest.fail "+ not found in env"
      | Some _ -> ());
      (* Check that car is loaded *)
      (match Type_env.lookup "car" env with
      | None -> Alcotest.fail "car not found in env"
      | Some _ -> ());
      (* Check that mapcar is loaded *)
      (match Type_env.lookup "mapcar" env with
      | None -> Alcotest.fail "mapcar not found in env"
      | Some _ -> ());
      (* Check that error returns never *)
      match Type_env.lookup "error" env with
      | None -> Alcotest.fail "error not found in env"
      | Some scheme ->
          let scheme_str = Type_env.scheme_to_string scheme in
          Alcotest.(check bool)
            "error returns never" true
            (try
               let _ =
                 Str.search_forward (Str.regexp_string "Never") scheme_str 0
               in
               true
             with Not_found -> false))

(** Test that cl-lib can be loaded into type environment (R16) Verify: (require
    'cl-lib) loads cl-lib.tart signatures from stdlib *)
let test_load_cl_lib () =
  let sp = Search_path.empty |> Search_path.with_stdlib stdlib_dir in
  (* First load builtins so we have basic types like list *)
  let base_env =
    match
      Search_path.load_module ~search_path:sp ~env:Type_env.empty "builtins"
    with
    | None -> Alcotest.fail "failed to load builtins module"
    | Some env -> env
  in
  (* Then load cl-lib *)
  match Search_path.load_module ~search_path:sp ~env:base_env "cl-lib" with
  | None -> Alcotest.fail "failed to load cl-lib module"
  | Some env ->
      (* Check that cl-mapcar is loaded *)
      (match Type_env.lookup "cl-mapcar" env with
      | None -> Alcotest.fail "cl-mapcar not found in env"
      | Some _ -> ());
      (* Verify type checking works with cl-lib functions *)
      let ty, errors = check_expr_str ~env "(cl-first (list 1 2 3))" in
      Alcotest.(check int) "no errors" 0 (List.length errors);
      (* Result should be (Int | Nil) *)
      let ty_str = Types.to_string ty in
      Alcotest.(check bool)
        "returns optional int" true
        (try
           let _ = Str.search_forward (Str.regexp_string "Int") ty_str 0 in
           true
         with Not_found -> false)

(** Test that frames.tart parses successfully *)
let test_parse_frames () =
  let path = Filename.concat stdlib_dir "frames.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("frames.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "frames.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "frames" sig_file.sig_module;
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 30);
      (* Check key frame functions *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has make-frame" true (has_defun "make-frame");
      Alcotest.(check bool)
        "has selected-frame" true
        (has_defun "selected-frame");
      Alcotest.(check bool) "has frame-list" true (has_defun "frame-list");
      Alcotest.(check bool) "has delete-frame" true (has_defun "delete-frame");
      Alcotest.(check bool)
        "has frame-parameters" true
        (has_defun "frame-parameters")

(** Test that frames can be loaded into type environment *)
let test_load_frames () =
  let sp = Search_path.empty |> Search_path.with_stdlib stdlib_dir in
  (* First load builtins and buffers for dependency types *)
  let base_env =
    match
      Search_path.load_module ~search_path:sp ~env:Type_env.empty "builtins"
    with
    | None -> Alcotest.fail "failed to load builtins module"
    | Some env -> env
  in
  let base_env =
    match Search_path.load_module ~search_path:sp ~env:base_env "buffers" with
    | None -> Alcotest.fail "failed to load buffers module"
    | Some env -> env
  in
  let base_env =
    match Search_path.load_module ~search_path:sp ~env:base_env "windows" with
    | None -> Alcotest.fail "failed to load windows module"
    | Some env -> env
  in
  match Search_path.load_module ~search_path:sp ~env:base_env "frames" with
  | None -> Alcotest.fail "failed to load frames module"
  | Some env ->
      (* Check that selected-frame is loaded *)
      (match Type_env.lookup "selected-frame" env with
      | None -> Alcotest.fail "selected-frame not found in env"
      | Some _ -> ());
      (* Check that make-frame is loaded *)
      (match Type_env.lookup "make-frame" env with
      | None -> Alcotest.fail "make-frame not found in env"
      | Some _ -> ());
      (* Check that frame-list is loaded *)
      (match Type_env.lookup "frame-list" env with
      | None -> Alcotest.fail "frame-list not found in env"
      | Some _ -> ());
      (* Verify type checking works with frame functions *)
      let _, errors = check_expr_str ~env "(selected-frame)" in
      Alcotest.(check int) "selected-frame: no errors" 0 (List.length errors);
      (* Check frame-list returns list of frames *)
      let ty, errors2 = check_expr_str ~env "(frame-list)" in
      Alcotest.(check int) "frame-list: no errors" 0 (List.length errors2);
      (* Result should be (List Frame) *)
      let ty_str = Types.to_string ty in
      Alcotest.(check bool)
        "returns list type" true
        (try
           let _ = Str.search_forward (Str.regexp_string "List") ty_str 0 in
           true
         with Not_found -> false)

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
      ( "bundled-stdlib",
        [
          Alcotest.test_case "parse builtins.tart" `Quick test_parse_builtins;
          Alcotest.test_case "parse cl-lib.tart" `Quick test_parse_cl_lib;
          Alcotest.test_case "parse seq.tart" `Quick test_parse_seq;
          Alcotest.test_case "parse buffers.tart" `Quick test_parse_buffers;
          Alcotest.test_case "parse windows.tart" `Quick test_parse_windows;
          Alcotest.test_case "parse frames.tart" `Quick test_parse_frames;
          Alcotest.test_case "load builtins into env" `Quick test_load_builtins;
          Alcotest.test_case "load cl-lib into env" `Quick test_load_cl_lib;
          Alcotest.test_case "load buffers into env" `Quick test_load_buffers;
          Alcotest.test_case "load windows into env" `Quick test_load_windows;
          Alcotest.test_case "load frames into env" `Quick test_load_frames;
        ] );
    ]
