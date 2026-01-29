(** Tests for C source scanner. *)

open Coverage.C_scanner

(** {1 Test Fixtures} *)

(** Create a temporary C file with given content and return path. *)
let with_temp_c_file content f =
  let tmpfile = Filename.temp_file "test_" ".c" in
  let oc = open_out tmpfile in
  output_string oc content;
  close_out oc;
  Fun.protect ~finally:(fun () -> Sys.remove tmpfile) (fun () -> f tmpfile)

(** Create a temporary directory with given files. *)
let with_temp_dir files f =
  let tmpdir = Filename.temp_file "testdir_" "" in
  Sys.remove tmpdir;
  Unix.mkdir tmpdir 0o755;
  let cleanup () =
    Array.iter
      (fun f -> try Sys.remove (Filename.concat tmpdir f) with _ -> ())
      (Sys.readdir tmpdir);
    Unix.rmdir tmpdir
  in
  Fun.protect ~finally:cleanup (fun () ->
      List.iter
        (fun (name, content) ->
          let path = Filename.concat tmpdir name in
          let oc = open_out path in
          output_string oc content;
          close_out oc)
        files;
      f tmpdir)

(** {1 DEFUN Tests} *)

let test_defun_simple () =
  let content =
    {|
DEFUN ("car", Fcar, Scar, 1, 1, 0,
       doc: /* Return the car of LIST. */)
  (Lisp_Object list)
{
  return XCAR (list);
}
|}
  in
  with_temp_c_file content (fun path ->
      let defs = scan_file path in
      Alcotest.(check int) "one defun" 1 (List.length defs);
      let d = List.hd defs in
      Alcotest.(check string) "name is car" "car" d.name;
      Alcotest.(check bool) "kind is Defun" true (d.kind = Defun))

let test_defun_multiline () =
  let content =
    {|
DEFUN ("delete-and-extract-region",
       Fdelete_and_extract_region,
       Sdelete_and_extract_region, 2, 2, 0,
       doc: /* Delete text between START and END. */)
  (Lisp_Object start, Lisp_Object end)
{
  return del_range_1 (start, end, 1, 1);
}
|}
  in
  with_temp_c_file content (fun path ->
      let defs = scan_file path in
      Alcotest.(check int) "one defun" 1 (List.length defs);
      let d = List.hd defs in
      Alcotest.(check string) "name" "delete-and-extract-region" d.name)

let test_defun_multiple () =
  let content =
    {|
DEFUN ("car", Fcar, Scar, 1, 1, 0, doc: /* */)
  (Lisp_Object list) { return XCAR (list); }

DEFUN ("cdr", Fcdr, Scdr, 1, 1, 0, doc: /* */)
  (Lisp_Object list) { return XCDR (list); }

DEFUN ("cons", Fcons, Scons, 2, 2, 0, doc: /* */)
  (Lisp_Object car, Lisp_Object cdr) { return Fcons (car, cdr); }
|}
  in
  with_temp_c_file content (fun path ->
      let defs = scan_file path in
      Alcotest.(check int) "three defuns" 3 (List.length defs);
      let names = List.map (fun d -> d.name) defs in
      Alcotest.(check (list string)) "names" [ "car"; "cdr"; "cons" ] names)

let test_defun_line_numbers () =
  let content =
    {|
DEFUN ("foo", Ffoo, Sfoo, 0, 0, 0, doc: /**/)
DEFUN ("bar", Fbar, Sbar, 0, 0, 0, doc: /**/)
|}
  in
  with_temp_c_file content (fun path ->
      let defs = scan_file path in
      Alcotest.(check int) "two defuns" 2 (List.length defs);
      Alcotest.(check int) "foo on line 2" 2 (List.hd defs).line;
      Alcotest.(check int) "bar on line 3" 3 (List.nth defs 1).line)

(** {1 DEFVAR Tests} *)

let test_defvar_lisp () =
  let content =
    {|
DEFVAR_LISP ("load-path", Vload_path,
             doc: /* List of directories to search. */);
|}
  in
  with_temp_c_file content (fun path ->
      let defs = scan_file path in
      Alcotest.(check int) "one defvar" 1 (List.length defs);
      let d = List.hd defs in
      Alcotest.(check string) "name" "load-path" d.name;
      Alcotest.(check bool) "kind is Defvar" true (d.kind = Defvar))

let test_defvar_int () =
  let content =
    {|
DEFVAR_INT ("gc-cons-threshold", gc_cons_threshold,
            doc: /* GC threshold. */);
|}
  in
  with_temp_c_file content (fun path ->
      let defs = scan_file path in
      Alcotest.(check int) "one defvar" 1 (List.length defs);
      let d = List.hd defs in
      Alcotest.(check string) "name" "gc-cons-threshold" d.name;
      Alcotest.(check bool) "kind is Defvar" true (d.kind = Defvar))

let test_defvar_bool () =
  let content =
    {|
DEFVAR_BOOL ("debug-on-error", debug_on_error,
             doc: /* Debug. */);
|}
  in
  with_temp_c_file content (fun path ->
      let defs = scan_file path in
      Alcotest.(check int) "one defvar" 1 (List.length defs);
      let d = List.hd defs in
      Alcotest.(check string) "name" "debug-on-error" d.name)

(** {1 DEFSYM Tests} *)

let test_defsym () =
  let content =
    {|
DEFSYM (Qnil, "nil");
DEFSYM (Qt, "t");
DEFSYM (Qlambda, "lambda");
|}
  in
  with_temp_c_file content (fun path ->
      let defs = scan_file path in
      Alcotest.(check int) "three defsyms" 3 (List.length defs);
      let names = List.map (fun d -> d.name) defs in
      Alcotest.(check (list string)) "names" [ "nil"; "t"; "lambda" ] names;
      List.iter
        (fun d -> Alcotest.(check bool) "kind is Defsym" true (d.kind = Defsym))
        defs)

let test_defsym_with_underscores () =
  let content = {|
DEFSYM (Qerror_message_string, "error-message-string");
|} in
  with_temp_c_file content (fun path ->
      let defs = scan_file path in
      Alcotest.(check int) "one defsym" 1 (List.length defs);
      Alcotest.(check string) "name" "error-message-string" (List.hd defs).name)

(** {1 Mixed Content Tests} *)

let test_mixed_definitions () =
  let content =
    {|
DEFUN ("car", Fcar, Scar, 1, 1, 0, doc: /* */)
  (Lisp_Object list) { return XCAR (list); }

DEFVAR_LISP ("load-path", Vload_path, doc: /* */);

DEFSYM (Qt, "t");
|}
  in
  with_temp_c_file content (fun path ->
      let defs = scan_file path in
      Alcotest.(check int) "three definitions" 3 (List.length defs);
      let defuns = List.filter (fun d -> d.kind = Defun) defs in
      let defvars = List.filter (fun d -> d.kind = Defvar) defs in
      let defsyms = List.filter (fun d -> d.kind = Defsym) defs in
      Alcotest.(check int) "one defun" 1 (List.length defuns);
      Alcotest.(check int) "one defvar" 1 (List.length defvars);
      Alcotest.(check int) "one defsym" 1 (List.length defsyms))

(** {1 Directory Scanning Tests} *)

let test_scan_dir () =
  with_temp_dir
    [
      ("foo.c", {|DEFUN ("foo", Ffoo, Sfoo, 0, 0, 0, doc: /**/)|});
      ("bar.c", {|DEFUN ("bar", Fbar, Sbar, 0, 0, 0, doc: /**/)|});
      ("not_c.txt", {|DEFUN ("ignored", Fi, Si, 0, 0, 0, doc: /**/)|});
    ]
    (fun dir ->
      let defs = scan_dir dir in
      Alcotest.(check int) "two defuns from .c files" 2 (List.length defs);
      let names = List.map (fun d -> d.name) defs |> List.sort String.compare in
      Alcotest.(check (list string)) "names" [ "bar"; "foo" ] names)

let test_scan_dir_nonexistent () =
  let defs = scan_dir "/nonexistent/path" in
  Alcotest.(check int) "empty list" 0 (List.length defs)

(** {1 Private Identifier Tests} *)

let test_is_private () =
  Alcotest.(check bool)
    "internal--foo is private" true
    (is_private "internal--foo");
  Alcotest.(check bool) "foo--bar is private" true (is_private "foo--bar");
  Alcotest.(check bool) "foo-bar is public" false (is_private "foo-bar");
  Alcotest.(check bool) "car is public" false (is_private "car")

let test_partition_public_private () =
  let content =
    {|
DEFUN ("public-fn", Fpublic_fn, Spublic_fn, 0, 0, 0, doc: /**/)
DEFUN ("internal--private", Finternal_private, Si, 0, 0, 0, doc: /**/)
DEFUN ("another-public", Fanother, Sa, 0, 0, 0, doc: /**/)
|}
  in
  with_temp_c_file content (fun path ->
      let defs = scan_file path in
      let public, private_ = partition_public_private defs in
      Alcotest.(check int) "two public" 2 (List.length public);
      Alcotest.(check int) "one private" 1 (List.length private_);
      Alcotest.(check string)
        "private name" "internal--private" (List.hd private_).name)

(** {1 Edge Cases} *)

let test_empty_file () =
  with_temp_c_file "" (fun path ->
      let defs = scan_file path in
      Alcotest.(check int) "no definitions" 0 (List.length defs))

let test_nonexistent_file () =
  let defs = scan_file "/nonexistent/file.c" in
  Alcotest.(check int) "empty list" 0 (List.length defs)

let test_file_with_file_name () =
  with_temp_c_file {|DEFUN ("test", Ft, St, 0, 0, 0, doc: /**/)|} (fun path ->
      let defs = scan_file path in
      Alcotest.(check int) "one defun" 1 (List.length defs);
      let expected_file = Filename.basename path in
      Alcotest.(check string)
        "file is basename" expected_file (List.hd defs).file)

(** {1 Test Suites} *)

let defun_tests =
  [
    Alcotest.test_case "simple" `Quick test_defun_simple;
    Alcotest.test_case "multiline" `Quick test_defun_multiline;
    Alcotest.test_case "multiple" `Quick test_defun_multiple;
    Alcotest.test_case "line numbers" `Quick test_defun_line_numbers;
  ]

let defvar_tests =
  [
    Alcotest.test_case "lisp" `Quick test_defvar_lisp;
    Alcotest.test_case "int" `Quick test_defvar_int;
    Alcotest.test_case "bool" `Quick test_defvar_bool;
  ]

let defsym_tests =
  [
    Alcotest.test_case "simple" `Quick test_defsym;
    Alcotest.test_case "underscores" `Quick test_defsym_with_underscores;
  ]

let mixed_tests =
  [ Alcotest.test_case "mixed definitions" `Quick test_mixed_definitions ]

let dir_tests =
  [
    Alcotest.test_case "scan_dir" `Quick test_scan_dir;
    Alcotest.test_case "nonexistent" `Quick test_scan_dir_nonexistent;
  ]

let private_tests =
  [
    Alcotest.test_case "is_private" `Quick test_is_private;
    Alcotest.test_case "partition" `Quick test_partition_public_private;
  ]

let edge_case_tests =
  [
    Alcotest.test_case "empty file" `Quick test_empty_file;
    Alcotest.test_case "nonexistent file" `Quick test_nonexistent_file;
    Alcotest.test_case "file name" `Quick test_file_with_file_name;
  ]

let () =
  Alcotest.run "c_scanner"
    [
      ("defun", defun_tests);
      ("defvar", defvar_tests);
      ("defsym", defsym_tests);
      ("mixed", mixed_tests);
      ("directory", dir_tests);
      ("private", private_tests);
      ("edge_cases", edge_case_tests);
    ]
