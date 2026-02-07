(** Unit tests for Emacs_corpus. *)

module Corpus = Tart.Emacs_corpus

(* =============================================================================
   Helpers
   ============================================================================= *)

(** Create a fresh temporary directory. *)
let make_temp_dir prefix =
  let base = Filename.get_temp_dir_name () in
  let name =
    Printf.sprintf "%s_%d_%f" prefix (Unix.getpid ()) (Unix.gettimeofday ())
  in
  let dir = Filename.concat base name in
  Unix.mkdir dir 0o755;
  dir

(** Run a block with XDG_CACHE_HOME pointing to a fresh temp dir. *)
let with_xdg f =
  let dir = make_temp_dir "corpus_test" in
  let old_xdg =
    try Some (Unix.getenv "XDG_CACHE_HOME") with Not_found -> None
  in
  Unix.putenv "XDG_CACHE_HOME" dir;
  let result =
    try f dir
    with exn ->
      (match old_xdg with
      | Some v -> Unix.putenv "XDG_CACHE_HOME" v
      | None -> Unix.putenv "XDG_CACHE_HOME" "");
      raise exn
  in
  (match old_xdg with
  | Some v -> Unix.putenv "XDG_CACHE_HOME" v
  | None -> Unix.putenv "XDG_CACHE_HOME" "");
  result

(** Write a file at [path] with [contents]. Creates parent dirs. *)
let write_file path contents =
  let dir = Filename.dirname path in
  let rec mkdir_p d =
    if not (Sys.file_exists d) then begin
      mkdir_p (Filename.dirname d);
      try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end
  in
  mkdir_p dir;
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

(* =============================================================================
   corpus_dir: XDG_CACHE_HOME and fallback
   ============================================================================= *)

let test_corpus_dir_xdg () =
  with_xdg (fun dir ->
      let result = Corpus.corpus_dir () in
      let expected = Filename.concat (Filename.concat dir "tart") "emacs-src" in
      Alcotest.(check string) "XDG override" expected result)

let test_corpus_dir_fallback () =
  Unix.putenv "XDG_CACHE_HOME" "";
  let result = Corpus.corpus_dir () in
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let expected =
    Filename.concat
      (Filename.concat (Filename.concat home ".cache") "tart")
      "emacs-src"
  in
  Alcotest.(check string) "fallback to ~/.cache/tart/emacs-src" expected result

let corpus_dir_tests =
  [
    ("XDG_CACHE_HOME override", `Quick, test_corpus_dir_xdg);
    ("fallback to ~/.cache/tart/emacs-src", `Quick, test_corpus_dir_fallback);
  ]

(* =============================================================================
   version_to_tag: converts version to git tag
   ============================================================================= *)

let test_version_to_tag_31_1 () =
  let v : Tart.Emacs_version.version =
    { major = 31; minor = 1; patch = None }
  in
  let tag = Corpus.version_to_tag v in
  Alcotest.(check string) "31.1 tag" "emacs-31.1" tag

let test_version_to_tag_29_4 () =
  let v : Tart.Emacs_version.version =
    { major = 29; minor = 4; patch = None }
  in
  let tag = Corpus.version_to_tag v in
  Alcotest.(check string) "29.4 tag" "emacs-29.4" tag

let test_version_to_tag_30_0 () =
  let v : Tart.Emacs_version.version =
    { major = 30; minor = 0; patch = None }
  in
  let tag = Corpus.version_to_tag v in
  Alcotest.(check string) "30.0 tag" "emacs-30.0" tag

let version_to_tag_tests =
  [
    ("31.1 -> emacs-31.1", `Quick, test_version_to_tag_31_1);
    ("29.4 -> emacs-29.4", `Quick, test_version_to_tag_29_4);
    ("30.0 -> emacs-30.0", `Quick, test_version_to_tag_30_0);
  ]

(* =============================================================================
   run_git: captures stdout/stderr
   ============================================================================= *)

let test_run_git_version () =
  match Corpus.run_git [ "--version" ] with
  | Ok output ->
      Alcotest.(check bool)
        "starts with git version" true
        (String.length output > 0)
  | Error _ -> Alcotest.fail "git --version should succeed"

let test_run_git_bad_command () =
  match Corpus.run_git [ "not-a-real-command-xyzzy" ] with
  | Ok _ -> Alcotest.fail "bad command should fail"
  | Error _ -> Alcotest.(check pass) "error returned" () ()

let test_run_git_with_cwd () =
  let dir = make_temp_dir "git_cwd" in
  (* Init a git repo *)
  ignore (Corpus.run_git [ "init"; dir ]);
  match Corpus.run_git ~cwd:dir [ "status" ] with
  | Ok output ->
      Alcotest.(check bool) "has output" true (String.length output > 0)
  | Error _ -> Alcotest.fail "git status in valid repo should succeed"

let run_git_tests =
  [
    ("captures stdout", `Quick, test_run_git_version);
    ("returns error on bad command", `Quick, test_run_git_bad_command);
    ("respects cwd", `Quick, test_run_git_with_cwd);
  ]

(* =============================================================================
   detect_tag: parses version to tag
   ============================================================================= *)

let test_detect_tag () =
  match Corpus.detect_tag () with
  | Ok tag ->
      Alcotest.(check bool)
        "starts with emacs-" true
        (String.length tag > 6 && String.sub tag 0 6 = "emacs-")
  | Error Corpus.No_emacs ->
      (* Acceptable in CI without Emacs *)
      Alcotest.(check pass) "No_emacs is acceptable" () ()
  | Error e -> Alcotest.fail (Corpus.corpus_error_to_string e)

let detect_tag_tests =
  [ ("parses system Emacs version", `Quick, test_detect_tag) ]

(* =============================================================================
   list_el_files: discovers .el in temp dir
   ============================================================================= *)

let test_list_el_files_empty () =
  with_xdg (fun _dir ->
      (* No corpus dir exists *)
      let files = Corpus.list_el_files () in
      Alcotest.(check int) "empty when no corpus" 0 (List.length files))

let test_list_el_files_discovers () =
  with_xdg (fun dir ->
      (* Create fake corpus dir with .el files *)
      let corpus = Filename.concat (Filename.concat dir "tart") "emacs-src" in
      write_file (Filename.concat corpus "foo.el") "(defun foo ())";
      write_file (Filename.concat corpus "bar.el") "(defun bar ())";
      write_file (Filename.concat corpus "not-el.txt") "ignore me";
      let files = Corpus.list_el_files () in
      Alcotest.(check int) "finds 2 .el files" 2 (List.length files);
      (* Should be sorted *)
      let basenames = List.map Filename.basename files in
      Alcotest.(check (list string)) "sorted" [ "bar.el"; "foo.el" ] basenames)

let test_list_el_files_recursive () =
  with_xdg (fun dir ->
      let corpus = Filename.concat (Filename.concat dir "tart") "emacs-src" in
      write_file (Filename.concat corpus "top.el") "";
      write_file
        (Filename.concat (Filename.concat corpus "lisp") "nested.el")
        "";
      write_file
        (Filename.concat
           (Filename.concat (Filename.concat corpus "lisp") "sub")
           "deep.el")
        "";
      let files = Corpus.list_el_files () in
      Alcotest.(check int) "finds 3 .el files recursively" 3 (List.length files);
      (* All are absolute paths *)
      List.iter
        (fun f ->
          Alcotest.(check bool)
            "absolute path" true
            (Filename.is_relative f = false))
        files)

let list_el_files_tests =
  [
    ("empty when no corpus", `Quick, test_list_el_files_empty);
    ("discovers .el files", `Quick, test_list_el_files_discovers);
    ("recursive discovery", `Quick, test_list_el_files_recursive);
  ]

(* =============================================================================
   clean: removes directory, returns bytes freed
   ============================================================================= *)

let test_clean_nonexistent () =
  with_xdg (fun _dir ->
      let freed = Corpus.clean () in
      Alcotest.(check int) "0 bytes when no corpus" 0 freed)

let test_clean_removes_dir () =
  with_xdg (fun dir ->
      let corpus = Filename.concat (Filename.concat dir "tart") "emacs-src" in
      write_file (Filename.concat corpus "test.el") "some content here";
      Alcotest.(check bool) "dir exists before" true (Sys.file_exists corpus);
      let freed = Corpus.clean () in
      Alcotest.(check bool) "dir removed" false (Sys.file_exists corpus);
      Alcotest.(check bool) "bytes freed > 0" true (freed > 0))

let test_clean_reports_bytes () =
  with_xdg (fun dir ->
      let corpus = Filename.concat (Filename.concat dir "tart") "emacs-src" in
      let content = String.make 1000 'x' in
      write_file (Filename.concat corpus "big.el") content;
      let freed = Corpus.clean () in
      Alcotest.(check bool) "freed >= content size" true (freed >= 1000))

let clean_tests =
  [
    ("0 bytes when no corpus", `Quick, test_clean_nonexistent);
    ("removes directory", `Quick, test_clean_removes_dir);
    ("reports bytes freed", `Quick, test_clean_reports_bytes);
  ]

(* =============================================================================
   is_cloned: directory existence check
   ============================================================================= *)

let test_is_cloned_false () =
  with_xdg (fun _dir ->
      Alcotest.(check bool) "false when no dir" false (Corpus.is_cloned ()))

let test_is_cloned_true () =
  with_xdg (fun dir ->
      let corpus = Filename.concat (Filename.concat dir "tart") "emacs-src" in
      write_file (Filename.concat corpus "marker") "";
      Alcotest.(check bool) "true when dir exists" true (Corpus.is_cloned ()))

let is_cloned_tests =
  [
    ("false when no directory", `Quick, test_is_cloned_false);
    ("true when directory exists", `Quick, test_is_cloned_true);
  ]

(* =============================================================================
   corpus_error_to_string: all constructors
   ============================================================================= *)

let contains s sub =
  let sub_len = String.length sub in
  let s_len = String.length s in
  if sub_len > s_len then false
  else
    let rec check i =
      if i > s_len - sub_len then false
      else if String.sub s i sub_len = sub then true
      else check (i + 1)
    in
    check 0

let test_error_clone_failed () =
  let s = Corpus.corpus_error_to_string (Clone_failed "timeout") in
  Alcotest.(check bool) "contains Clone" true (contains s "Clone");
  Alcotest.(check bool) "contains message" true (contains s "timeout")

let test_error_fetch_failed () =
  let s = Corpus.corpus_error_to_string (Fetch_failed "network down") in
  Alcotest.(check bool) "contains Fetch" true (contains s "Fetch");
  Alcotest.(check bool) "contains message" true (contains s "network down")

let test_error_checkout_failed () =
  let s = Corpus.corpus_error_to_string (Checkout_failed "bad ref") in
  Alcotest.(check bool) "contains Checkout" true (contains s "Checkout");
  Alcotest.(check bool) "contains message" true (contains s "bad ref")

let test_error_no_emacs () =
  let s = Corpus.corpus_error_to_string No_emacs in
  Alcotest.(check bool)
    "mentions Emacs" true
    (contains s "Emacs" || contains s "emacs")

let test_error_invalid_ref () =
  let s = Corpus.corpus_error_to_string (Invalid_ref "bad") in
  Alcotest.(check bool) "contains Invalid" true (contains s "Invalid");
  Alcotest.(check bool) "contains ref" true (contains s "bad")

let error_types_tests =
  [
    ("Clone_failed", `Quick, test_error_clone_failed);
    ("Fetch_failed", `Quick, test_error_fetch_failed);
    ("Checkout_failed", `Quick, test_error_checkout_failed);
    ("No_emacs", `Quick, test_error_no_emacs);
    ("Invalid_ref", `Quick, test_error_invalid_ref);
  ]

(* =============================================================================
   ensure_clone: no-ops when dir exists
   ============================================================================= *)

let test_ensure_clone_noop_when_exists () =
  with_xdg (fun dir ->
      let corpus = Filename.concat (Filename.concat dir "tart") "emacs-src" in
      write_file (Filename.concat corpus "marker") "";
      (* Should be a no-op since dir already exists *)
      match Corpus.ensure_clone ~tag:"emacs-31.1" () with
      | Ok () -> Alcotest.(check pass) "no-op success" () ()
      | Error e ->
          Alcotest.fail
            (Printf.sprintf "should have been no-op: %s"
               (Corpus.corpus_error_to_string e)))

let ensure_clone_tests =
  [ ("no-op when dir exists", `Quick, test_ensure_clone_noop_when_exists) ]

(* =============================================================================
   Runner
   ============================================================================= *)

let () =
  Alcotest.run "Emacs_corpus"
    [
      ("corpus_dir", corpus_dir_tests);
      ("version_to_tag", version_to_tag_tests);
      ("run_git", run_git_tests);
      ("detect_tag", detect_tag_tests);
      ("list_el_files", list_el_files_tests);
      ("clean", clean_tests);
      ("is_cloned", is_cloned_tests);
      ("error_types", error_types_tests);
      ("ensure_clone", ensure_clone_tests);
    ]
