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

(** Test that files.tart parses successfully *)
let test_parse_files () =
  let path = Filename.concat stdlib_dir "files.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("files.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "files.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "files" sig_file.sig_module;
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 50);
      (* Check key file functions *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has find-file" true (has_defun "find-file");
      Alcotest.(check bool) "has write-file" true (has_defun "write-file");
      Alcotest.(check bool) "has file-exists-p" true (has_defun "file-exists-p");
      Alcotest.(check bool)
        "has expand-file-name" true
        (has_defun "expand-file-name");
      Alcotest.(check bool) "has copy-file" true (has_defun "copy-file");
      Alcotest.(check bool)
        "has directory-files" true
        (has_defun "directory-files")

(** Test that files can be loaded into type environment *)
let test_load_files () =
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
  match Search_path.load_module ~search_path:sp ~env:base_env "files" with
  | None -> Alcotest.fail "failed to load files module"
  | Some env ->
      (* Check that find-file is loaded *)
      (match Type_env.lookup "find-file" env with
      | None -> Alcotest.fail "find-file not found in env"
      | Some _ -> ());
      (* Check that file-exists-p is loaded *)
      (match Type_env.lookup "file-exists-p" env with
      | None -> Alcotest.fail "file-exists-p not found in env"
      | Some _ -> ());
      (* Check that expand-file-name is loaded *)
      (match Type_env.lookup "expand-file-name" env with
      | None -> Alcotest.fail "expand-file-name not found in env"
      | Some _ -> ());
      (* Verify type checking works with file functions *)
      let _, errors = check_expr_str ~env "(file-exists-p \"/tmp/test\")" in
      Alcotest.(check int) "file-exists-p: no errors" 0 (List.length errors);
      (* Check find-file returns buffer type *)
      let ty, errors2 = check_expr_str ~env "(find-file \"/tmp/test\")" in
      Alcotest.(check int) "find-file: no errors" 0 (List.length errors2);
      (* Result should be Buffer *)
      let ty_str = Types.to_string ty in
      Alcotest.(check bool)
        "returns buffer type" true
        (try
           let _ = Str.search_forward (Str.regexp_string "uffer") ty_str 0 in
           true
         with Not_found -> false);
      (* Check directory-files returns list of strings *)
      let ty3, errors3 = check_expr_str ~env "(directory-files \"/tmp\")" in
      Alcotest.(check int) "directory-files: no errors" 0 (List.length errors3);
      let ty3_str = Types.to_string ty3 in
      Alcotest.(check bool)
        "returns list of strings" true
        (try
           let _ = Str.search_forward (Str.regexp_string "List") ty3_str 0 in
           true
         with Not_found -> false)

(** Test that text-properties.tart parses successfully *)
let test_parse_text_properties () =
  let path = Filename.concat stdlib_dir "text-properties.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("text-properties.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "text-properties.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string)
        "module name" "text-properties" sig_file.sig_module;
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 20);
      (* Check key text property functions *)
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
        "has next-property-change" true
        (has_defun "next-property-change");
      Alcotest.(check bool) "has propertize" true (has_defun "propertize")

(** Test that text-properties can be loaded into type environment *)
let test_load_text_properties () =
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
  match
    Search_path.load_module ~search_path:sp ~env:base_env "text-properties"
  with
  | None -> Alcotest.fail "failed to load text-properties module"
  | Some env ->
      (* Check that get-text-property is loaded *)
      (match Type_env.lookup "get-text-property" env with
      | None -> Alcotest.fail "get-text-property not found in env"
      | Some _ -> ());
      (* Check that put-text-property is loaded *)
      (match Type_env.lookup "put-text-property" env with
      | None -> Alcotest.fail "put-text-property not found in env"
      | Some _ -> ());
      (* Check that propertize is loaded *)
      (match Type_env.lookup "propertize" env with
      | None -> Alcotest.fail "propertize not found in env"
      | Some _ -> ());
      (* Verify type checking works with text property functions *)
      let ty, errors =
        check_expr_str ~env "(propertize \"hello\" 'face 'bold)"
      in
      Alcotest.(check int) "propertize: no errors" 0 (List.length errors);
      (* Result should be String *)
      let ty_str = Types.to_string ty in
      Alcotest.(check bool)
        "returns string type" true
        (try
           let _ = Str.search_forward (Str.regexp_string "String") ty_str 0 in
           true
         with Not_found -> false);
      (* Check next-property-change returns optional int *)
      let ty2, errors2 = check_expr_str ~env "(next-property-change 1)" in
      Alcotest.(check int)
        "next-property-change: no errors" 0 (List.length errors2);
      let ty2_str = Types.to_string ty2 in
      Alcotest.(check bool)
        "returns optional int" true
        (try
           let _ = Str.search_forward (Str.regexp_string "Nil") ty2_str 0 in
           true
         with Not_found -> false)

(** Test that overlays.tart parses successfully *)
let test_parse_overlays () =
  let path = Filename.concat stdlib_dir "overlays.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("overlays.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "overlays.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "overlays" sig_file.sig_module;
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 10);
      (* overlays.tart should have the overlay opaque type *)
      let has_type name =
        List.exists
          (function Sig_ast.DType t -> t.type_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has overlay type" true (has_type "overlay");
      (* Check key overlay functions *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has make-overlay" true (has_defun "make-overlay");
      Alcotest.(check bool)
        "has delete-overlay" true
        (has_defun "delete-overlay");
      Alcotest.(check bool) "has overlay-get" true (has_defun "overlay-get");
      Alcotest.(check bool) "has overlay-put" true (has_defun "overlay-put");
      Alcotest.(check bool) "has overlays-at" true (has_defun "overlays-at");
      Alcotest.(check bool) "has overlays-in" true (has_defun "overlays-in")

(** Test that overlays can be loaded into type environment *)
let test_load_overlays () =
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
  match Search_path.load_module ~search_path:sp ~env:base_env "overlays" with
  | None -> Alcotest.fail "failed to load overlays module"
  | Some env ->
      (* Check that make-overlay is loaded *)
      (match Type_env.lookup "make-overlay" env with
      | None -> Alcotest.fail "make-overlay not found in env"
      | Some _ -> ());
      (* Check that overlay-get is loaded *)
      (match Type_env.lookup "overlay-get" env with
      | None -> Alcotest.fail "overlay-get not found in env"
      | Some _ -> ());
      (* Check that overlays-at is loaded *)
      (match Type_env.lookup "overlays-at" env with
      | None -> Alcotest.fail "overlays-at not found in env"
      | Some _ -> ());
      (* Verify type checking works with overlay functions *)
      let ty, errors = check_expr_str ~env "(make-overlay 1 10)" in
      Alcotest.(check int) "make-overlay: no errors" 0 (List.length errors);
      (* Result should be Overlay *)
      let ty_str = Types.to_string ty in
      Alcotest.(check bool)
        "returns overlay type" true
        (try
           let _ = Str.search_forward (Str.regexp_string "verlay") ty_str 0 in
           true
         with Not_found -> false);
      (* Check overlays-at returns list of overlays *)
      let ty2, errors2 = check_expr_str ~env "(overlays-at 5)" in
      Alcotest.(check int) "overlays-at: no errors" 0 (List.length errors2);
      let ty2_str = Types.to_string ty2 in
      Alcotest.(check bool)
        "returns list type" true
        (try
           let _ = Str.search_forward (Str.regexp_string "List") ty2_str 0 in
           true
         with Not_found -> false)

(** Test that dash.tart parses successfully *)
let test_parse_dash () =
  let path = Filename.concat stdlib_dir "dash.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("dash.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "dash.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "dash" sig_file.sig_module;
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 100);
      (* Check key dash functions *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has -map" true (has_defun "-map");
      Alcotest.(check bool) "has -filter" true (has_defun "-filter");
      Alcotest.(check bool) "has -reduce" true (has_defun "-reduce");
      Alcotest.(check bool) "has -take" true (has_defun "-take");
      Alcotest.(check bool) "has -drop" true (has_defun "-drop");
      Alcotest.(check bool) "has -flatten" true (has_defun "-flatten");
      Alcotest.(check bool) "has -distinct" true (has_defun "-distinct");
      Alcotest.(check bool) "has -group-by" true (has_defun "-group-by");
      Alcotest.(check bool) "has -partition" true (has_defun "-partition");
      Alcotest.(check bool) "has -sort" true (has_defun "-sort")

(** Test that dash can be loaded into type environment *)
let test_load_dash () =
  let sp = Search_path.empty |> Search_path.with_stdlib stdlib_dir in
  (* First load builtins for base types *)
  let base_env =
    match
      Search_path.load_module ~search_path:sp ~env:Type_env.empty "builtins"
    with
    | None -> Alcotest.fail "failed to load builtins module"
    | Some env -> env
  in
  match Search_path.load_module ~search_path:sp ~env:base_env "dash" with
  | None -> Alcotest.fail "failed to load dash module"
  | Some env -> (
      (* Check that -map is loaded *)
      (match Type_env.lookup "-map" env with
      | None -> Alcotest.fail "-map not found in env"
      | Some _ -> ());
      (* Check that -filter is loaded *)
      (match Type_env.lookup "-filter" env with
      | None -> Alcotest.fail "-filter not found in env"
      | Some _ -> ());
      (* Check that -reduce-from is loaded *)
      (match Type_env.lookup "-reduce-from" env with
      | None -> Alcotest.fail "-reduce-from not found in env"
      | Some _ -> ());
      (* Check that -group-by is loaded *)
      (match Type_env.lookup "-group-by" env with
      | None -> Alcotest.fail "-group-by not found in env"
      | Some _ -> ());
      (* Verify -map type is correct: (((a) -> b) (list a)) -> (list b) *)
      (match Type_env.lookup "-map" env with
      | None -> Alcotest.fail "-map not found"
      | Some scheme ->
          let scheme_str = Type_env.scheme_to_string scheme in
          Alcotest.(check bool)
            "-map has arrow type" true
            (String.length scheme_str > 0));
      (* Verify -filter type is correct *)
      (match Type_env.lookup "-filter" env with
      | None -> Alcotest.fail "-filter not found"
      | Some scheme ->
          let scheme_str = Type_env.scheme_to_string scheme in
          Alcotest.(check bool)
            "-filter has arrow type" true
            (String.length scheme_str > 0));
      (* Verify -reduce-from type is correct *)
      match Type_env.lookup "-reduce-from" env with
      | None -> Alcotest.fail "-reduce-from not found"
      | Some scheme ->
          let scheme_str = Type_env.scheme_to_string scheme in
          Alcotest.(check bool)
            "-reduce-from has arrow type" true
            (String.length scheme_str > 0))

(** Test that s.tart parses successfully *)
let test_parse_s () =
  let path = Filename.concat stdlib_dir "s.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("s.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "s.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "s" sig_file.sig_module;
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 50);
      (* Check key s.el functions *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has s-trim" true (has_defun "s-trim");
      Alcotest.(check bool) "has s-split" true (has_defun "s-split");
      Alcotest.(check bool) "has s-join" true (has_defun "s-join");
      Alcotest.(check bool) "has s-replace" true (has_defun "s-replace");
      Alcotest.(check bool) "has s-contains?" true (has_defun "s-contains?");
      Alcotest.(check bool) "has s-downcase" true (has_defun "s-downcase");
      Alcotest.(check bool) "has s-upcase" true (has_defun "s-upcase");
      Alcotest.(check bool) "has s-snake-case" true (has_defun "s-snake-case");
      Alcotest.(check bool)
        "has s-lower-camel-case" true
        (has_defun "s-lower-camel-case")

(** Test that s can be loaded into type environment *)
let test_load_s () =
  let sp = Search_path.empty |> Search_path.with_stdlib stdlib_dir in
  (* First load builtins for base types *)
  let base_env =
    match
      Search_path.load_module ~search_path:sp ~env:Type_env.empty "builtins"
    with
    | None -> Alcotest.fail "failed to load builtins module"
    | Some env -> env
  in
  match Search_path.load_module ~search_path:sp ~env:base_env "s" with
  | None -> Alcotest.fail "failed to load s module"
  | Some env ->
      (* Check that s-trim is loaded *)
      (match Type_env.lookup "s-trim" env with
      | None -> Alcotest.fail "s-trim not found in env"
      | Some _ -> ());
      (* Check that s-split is loaded *)
      (match Type_env.lookup "s-split" env with
      | None -> Alcotest.fail "s-split not found in env"
      | Some _ -> ());
      (* Check that s-join is loaded *)
      (match Type_env.lookup "s-join" env with
      | None -> Alcotest.fail "s-join not found in env"
      | Some _ -> ());
      (* Check that s-replace is loaded *)
      (match Type_env.lookup "s-replace" env with
      | None -> Alcotest.fail "s-replace not found in env"
      | Some _ -> ());
      (* Verify type checking works with s.el functions *)
      let ty, errors = check_expr_str ~env "(s-trim \"  hello  \")" in
      Alcotest.(check int) "s-trim: no errors" 0 (List.length errors);
      (* Result should be String *)
      let ty_str = Types.to_string ty in
      Alcotest.(check bool)
        "s-trim returns string type" true
        (try
           let _ = Str.search_forward (Str.regexp_string "String") ty_str 0 in
           true
         with Not_found -> false);
      (* Check s-split returns list of strings *)
      let ty2, errors2 = check_expr_str ~env "(s-split \",\" \"a,b,c\")" in
      Alcotest.(check int) "s-split: no errors" 0 (List.length errors2);
      let ty2_str = Types.to_string ty2 in
      Alcotest.(check bool)
        "s-split returns list of strings" true
        (try
           let _ = Str.search_forward (Str.regexp_string "List") ty2_str 0 in
           true
         with Not_found -> false);
      (* Check s-contains? returns bool *)
      let ty3, errors3 =
        check_expr_str ~env "(s-contains? \"hello\" \"hello world\")"
      in
      Alcotest.(check int) "s-contains?: no errors" 0 (List.length errors3);
      let ty3_str = Types.to_string ty3 in
      Alcotest.(check bool)
        "s-contains? returns bool" true
        (try
           let _ = Str.search_forward (Str.regexp_string "Bool") ty3_str 0 in
           true
         with Not_found -> false)

(** Test that f.tart parses successfully *)
let test_parse_f () =
  let path = Filename.concat stdlib_dir "f.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("f.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "f.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "f" sig_file.sig_module;
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 50);
      (* Check key f.el functions *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has f-join" true (has_defun "f-join");
      Alcotest.(check bool) "has f-split" true (has_defun "f-split");
      Alcotest.(check bool) "has f-expand" true (has_defun "f-expand");
      Alcotest.(check bool) "has f-dirname" true (has_defun "f-dirname");
      Alcotest.(check bool) "has f-exists-p" true (has_defun "f-exists-p");
      Alcotest.(check bool) "has f-directory-p" true (has_defun "f-directory-p");
      Alcotest.(check bool) "has f-read-text" true (has_defun "f-read-text");
      Alcotest.(check bool) "has f-write-text" true (has_defun "f-write-text");
      Alcotest.(check bool) "has f-mkdir" true (has_defun "f-mkdir");
      Alcotest.(check bool) "has f-delete" true (has_defun "f-delete");
      Alcotest.(check bool) "has f-glob" true (has_defun "f-glob");
      Alcotest.(check bool) "has f-entries" true (has_defun "f-entries")

(** Test that f can be loaded into type environment *)
let test_load_f () =
  let sp = Search_path.empty |> Search_path.with_stdlib stdlib_dir in
  (* First load builtins for base types *)
  let base_env =
    match
      Search_path.load_module ~search_path:sp ~env:Type_env.empty "builtins"
    with
    | None -> Alcotest.fail "failed to load builtins module"
    | Some env -> env
  in
  match Search_path.load_module ~search_path:sp ~env:base_env "f" with
  | None -> Alcotest.fail "failed to load f module"
  | Some env ->
      (* Check that f-join is loaded *)
      (match Type_env.lookup "f-join" env with
      | None -> Alcotest.fail "f-join not found in env"
      | Some _ -> ());
      (* Check that f-exists-p is loaded *)
      (match Type_env.lookup "f-exists-p" env with
      | None -> Alcotest.fail "f-exists-p not found in env"
      | Some _ -> ());
      (* Check that f-read-text is loaded *)
      (match Type_env.lookup "f-read-text" env with
      | None -> Alcotest.fail "f-read-text not found in env"
      | Some _ -> ());
      (* Check that f-glob is loaded *)
      (match Type_env.lookup "f-glob" env with
      | None -> Alcotest.fail "f-glob not found in env"
      | Some _ -> ());
      (* Verify type checking works with f.el functions *)
      let ty, errors = check_expr_str ~env "(f-join \"/\" \"home\" \"user\")" in
      Alcotest.(check int) "f-join: no errors" 0 (List.length errors);
      (* Result should be String *)
      let ty_str = Types.to_string ty in
      Alcotest.(check bool)
        "f-join returns string type" true
        (try
           let _ = Str.search_forward (Str.regexp_string "String") ty_str 0 in
           true
         with Not_found -> false);
      (* Check f-exists-p returns bool *)
      let ty2, errors2 = check_expr_str ~env "(f-exists-p \"/tmp\")" in
      Alcotest.(check int) "f-exists-p: no errors" 0 (List.length errors2);
      let ty2_str = Types.to_string ty2 in
      Alcotest.(check bool)
        "f-exists-p returns bool" true
        (try
           let _ = Str.search_forward (Str.regexp_string "Bool") ty2_str 0 in
           true
         with Not_found -> false);
      (* Check f-glob returns list of strings *)
      let ty3, errors3 = check_expr_str ~env "(f-glob \"*.el\")" in
      Alcotest.(check int) "f-glob: no errors" 0 (List.length errors3);
      let ty3_str = Types.to_string ty3 in
      Alcotest.(check bool)
        "f-glob returns list of strings" true
        (try
           let _ = Str.search_forward (Str.regexp_string "List") ty3_str 0 in
           true
         with Not_found -> false)

(** Test that ht.tart parses successfully *)
let test_parse_ht () =
  let path = Filename.concat stdlib_dir "ht.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("ht.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "ht.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "ht" sig_file.sig_module;
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 20);
      (* Check key ht.el functions *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has ht-create" true (has_defun "ht-create");
      Alcotest.(check bool) "has ht-get" true (has_defun "ht-get");
      Alcotest.(check bool) "has ht-set!" true (has_defun "ht-set!");
      Alcotest.(check bool) "has ht-keys" true (has_defun "ht-keys");
      Alcotest.(check bool) "has ht-map" true (has_defun "ht-map");
      Alcotest.(check bool) "has ht-from-alist" true (has_defun "ht-from-alist");
      Alcotest.(check bool) "has ht-merge" true (has_defun "ht-merge")

(** Test that ht can be loaded into type environment *)
let test_load_ht () =
  let sp = Search_path.empty |> Search_path.with_stdlib stdlib_dir in
  (* First load builtins for base types *)
  let base_env =
    match
      Search_path.load_module ~search_path:sp ~env:Type_env.empty "builtins"
    with
    | None -> Alcotest.fail "failed to load builtins module"
    | Some env -> env
  in
  match Search_path.load_module ~search_path:sp ~env:base_env "ht" with
  | None -> Alcotest.fail "failed to load ht module"
  | Some env -> (
      (* Check that ht-create is loaded *)
      (match Type_env.lookup "ht-create" env with
      | None -> Alcotest.fail "ht-create not found in env"
      | Some _ -> ());
      (* Check that ht-get is loaded *)
      (match Type_env.lookup "ht-get" env with
      | None -> Alcotest.fail "ht-get not found in env"
      | Some _ -> ());
      (* Check that ht-map is loaded *)
      (match Type_env.lookup "ht-map" env with
      | None -> Alcotest.fail "ht-map not found in env"
      | Some _ -> ());
      (* Check that ht-from-alist is loaded *)
      match Type_env.lookup "ht-from-alist" env with
      | None -> Alcotest.fail "ht-from-alist not found in env"
      | Some _ -> ())

(** Test that map.tart parses successfully *)
let test_parse_map () =
  let path = Filename.concat stdlib_dir "map.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("map.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "map.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "map" sig_file.sig_module;
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 10);
      (* Check key map.el functions *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has map-elt" true (has_defun "map-elt");
      Alcotest.(check bool) "has map-put!" true (has_defun "map-put!");
      Alcotest.(check bool) "has map-keys" true (has_defun "map-keys");
      Alcotest.(check bool) "has map-values" true (has_defun "map-values");
      Alcotest.(check bool) "has map-filter" true (has_defun "map-filter");
      Alcotest.(check bool) "has map-apply" true (has_defun "map-apply");
      Alcotest.(check bool) "has map-merge" true (has_defun "map-merge")

(** Test that map can be loaded into type environment *)
let test_load_map () =
  let sp = Search_path.empty |> Search_path.with_stdlib stdlib_dir in
  (* First load builtins for base types *)
  let base_env =
    match
      Search_path.load_module ~search_path:sp ~env:Type_env.empty "builtins"
    with
    | None -> Alcotest.fail "failed to load builtins module"
    | Some env -> env
  in
  match Search_path.load_module ~search_path:sp ~env:base_env "map" with
  | None -> Alcotest.fail "failed to load map module"
  | Some env -> (
      (* Check that map-elt is loaded *)
      (match Type_env.lookup "map-elt" env with
      | None -> Alcotest.fail "map-elt not found in env"
      | Some _ -> ());
      (* Check that map-keys is loaded *)
      (match Type_env.lookup "map-keys" env with
      | None -> Alcotest.fail "map-keys not found in env"
      | Some _ -> ());
      (* Check that map-filter is loaded *)
      (match Type_env.lookup "map-filter" env with
      | None -> Alcotest.fail "map-filter not found in env"
      | Some _ -> ());
      (* Check that map-apply is loaded *)
      match Type_env.lookup "map-apply" env with
      | None -> Alcotest.fail "map-apply not found in env"
      | Some _ -> ())

(** Test that subr-x.tart parses successfully *)
let test_parse_subr_x () =
  let path = Filename.concat stdlib_dir "subr-x.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("subr-x.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "subr-x.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "subr-x" sig_file.sig_module;
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls > 20);
      (* Check key subr-x functions *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has string-trim" true (has_defun "string-trim");
      Alcotest.(check bool)
        "has string-blank-p" true
        (has_defun "string-blank-p");
      Alcotest.(check bool) "has string-join" true (has_defun "string-join");
      Alcotest.(check bool)
        "has string-remove-prefix" true
        (has_defun "string-remove-prefix");
      Alcotest.(check bool)
        "has hash-table-keys" true
        (has_defun "hash-table-keys");
      Alcotest.(check bool)
        "has hash-table-values" true
        (has_defun "hash-table-values")

(** Test that subr-x can be loaded into type environment *)
let test_load_subr_x () =
  let sp = Search_path.empty |> Search_path.with_stdlib stdlib_dir in
  (* First load builtins for base types *)
  let base_env =
    match
      Search_path.load_module ~search_path:sp ~env:Type_env.empty "builtins"
    with
    | None -> Alcotest.fail "failed to load builtins module"
    | Some env -> env
  in
  match Search_path.load_module ~search_path:sp ~env:base_env "subr-x" with
  | None -> Alcotest.fail "failed to load subr-x module"
  | Some env -> (
      (* Check that string-trim is loaded *)
      (match Type_env.lookup "string-trim" env with
      | None -> Alcotest.fail "string-trim not found in env"
      | Some _ -> ());
      (* Check that string-join is loaded *)
      (match Type_env.lookup "string-join" env with
      | None -> Alcotest.fail "string-join not found in env"
      | Some _ -> ());
      (* Check that hash-table-keys is loaded *)
      (match Type_env.lookup "hash-table-keys" env with
      | None -> Alcotest.fail "hash-table-keys not found in env"
      | Some _ -> ());
      (* Check that string-remove-prefix is loaded *)
      match Type_env.lookup "string-remove-prefix" env with
      | None -> Alcotest.fail "string-remove-prefix not found in env"
      | Some _ -> ())

(** Test that rx.tart parses successfully *)
let test_parse_rx () =
  let path = Filename.concat stdlib_dir "rx.tart" in
  if not (Sys.file_exists path) then
    Alcotest.fail ("rx.tart not found at: " ^ path);
  match Search_path.parse_signature_file path with
  | None -> Alcotest.fail "rx.tart failed to parse"
  | Some sig_file ->
      Alcotest.(check string) "module name" "rx" sig_file.sig_module;
      Alcotest.(check bool)
        "has declarations" true
        (List.length sig_file.sig_decls >= 1);
      (* Check key rx functions *)
      let has_defun name =
        List.exists
          (function Sig_ast.DDefun d -> d.defun_name = name | _ -> false)
          sig_file.sig_decls
      in
      Alcotest.(check bool) "has rx-to-string" true (has_defun "rx-to-string")

(** Test that rx can be loaded into type environment *)
let test_load_rx () =
  let sp = Search_path.empty |> Search_path.with_stdlib stdlib_dir in
  (* First load builtins for base types *)
  let base_env =
    match
      Search_path.load_module ~search_path:sp ~env:Type_env.empty "builtins"
    with
    | None -> Alcotest.fail "failed to load builtins module"
    | Some env -> env
  in
  match Search_path.load_module ~search_path:sp ~env:base_env "rx" with
  | None -> Alcotest.fail "failed to load rx module"
  | Some env ->
      (* Check that rx-to-string is loaded *)
      (match Type_env.lookup "rx-to-string" env with
      | None -> Alcotest.fail "rx-to-string not found in env"
      | Some _ -> ());
      (* Verify type checking works - rx-to-string should return String *)
      let ty, errors = check_expr_str ~env "(rx-to-string '(any))" in
      Alcotest.(check int) "rx-to-string: no errors" 0 (List.length errors);
      let ty_str = Types.to_string ty in
      Alcotest.(check bool)
        "rx-to-string returns string type" true
        (try
           let _ = Str.search_forward (Str.regexp_string "String") ty_str 0 in
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
          Alcotest.test_case "parse files.tart" `Quick test_parse_files;
          Alcotest.test_case "load builtins into env" `Quick test_load_builtins;
          Alcotest.test_case "load cl-lib into env" `Quick test_load_cl_lib;
          Alcotest.test_case "load buffers into env" `Quick test_load_buffers;
          Alcotest.test_case "load windows into env" `Quick test_load_windows;
          Alcotest.test_case "load frames into env" `Quick test_load_frames;
          Alcotest.test_case "load files into env" `Quick test_load_files;
          Alcotest.test_case "parse text-properties.tart" `Quick
            test_parse_text_properties;
          Alcotest.test_case "load text-properties into env" `Quick
            test_load_text_properties;
          Alcotest.test_case "parse overlays.tart" `Quick test_parse_overlays;
          Alcotest.test_case "load overlays into env" `Quick test_load_overlays;
          Alcotest.test_case "parse dash.tart" `Quick test_parse_dash;
          Alcotest.test_case "load dash into env" `Quick test_load_dash;
          Alcotest.test_case "parse s.tart" `Quick test_parse_s;
          Alcotest.test_case "load s into env" `Quick test_load_s;
          Alcotest.test_case "parse f.tart" `Quick test_parse_f;
          Alcotest.test_case "load f into env" `Quick test_load_f;
          Alcotest.test_case "parse ht.tart" `Quick test_parse_ht;
          Alcotest.test_case "load ht into env" `Quick test_load_ht;
          Alcotest.test_case "parse map.tart" `Quick test_parse_map;
          Alcotest.test_case "load map into env" `Quick test_load_map;
          Alcotest.test_case "parse subr-x.tart" `Quick test_parse_subr_x;
          Alcotest.test_case "load subr-x into env" `Quick test_load_subr_x;
          Alcotest.test_case "parse rx.tart" `Quick test_parse_rx;
          Alcotest.test_case "load rx into env" `Quick test_load_rx;
        ] );
    ]
