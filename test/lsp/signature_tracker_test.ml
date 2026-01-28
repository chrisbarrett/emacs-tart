(** Tests for the signature tracker *)

module Tracker = Lsp.Signature_tracker

(* =============================================================================
   Construction
   ============================================================================= *)

let test_create () =
  let t = Tracker.create () in
  Alcotest.(check bool)
    "initially empty" false
    (Tracker.mem t "file:///test.tart")

(* =============================================================================
   Buffer Management
   ============================================================================= *)

let test_set_and_get () =
  let t = Tracker.create () in
  let uri = "file:///test/foo.tart" in
  let text = "(defun foo (int) -> int)" in
  Tracker.set t ~uri ~text;
  Alcotest.(check (option string))
    "get returns text" (Some text) (Tracker.get t uri)

let test_set_updates () =
  let t = Tracker.create () in
  let uri = "file:///test/foo.tart" in
  Tracker.set t ~uri ~text:"version1";
  Tracker.set t ~uri ~text:"version2";
  Alcotest.(check (option string))
    "get returns latest" (Some "version2") (Tracker.get t uri)

let test_remove () =
  let t = Tracker.create () in
  let uri = "file:///test/foo.tart" in
  Tracker.set t ~uri ~text:"test";
  Tracker.remove t uri;
  Alcotest.(check (option string)) "get returns None" None (Tracker.get t uri)

let test_mem () =
  let t = Tracker.create () in
  let uri = "file:///test/foo.tart" in
  Alcotest.(check bool) "not present initially" false (Tracker.mem t uri);
  Tracker.set t ~uri ~text:"test";
  Alcotest.(check bool) "present after set" true (Tracker.mem t uri);
  Tracker.remove t uri;
  Alcotest.(check bool) "not present after remove" false (Tracker.mem t uri)

(* =============================================================================
   Filename/URI Helpers
   ============================================================================= *)

let test_filename_of_uri () =
  Alcotest.(check string)
    "file:// uri" "/path/to/foo.tart"
    (Tracker.filename_of_uri "file:///path/to/foo.tart");
  Alcotest.(check string)
    "non-file uri" "http://example.com"
    (Tracker.filename_of_uri "http://example.com")

let test_is_tart_file () =
  Alcotest.(check bool)
    ".tart file" true
    (Tracker.is_tart_file "file:///test/foo.tart");
  Alcotest.(check bool)
    ".el file" false
    (Tracker.is_tart_file "file:///test/foo.el");
  Alcotest.(check bool)
    "other file" false
    (Tracker.is_tart_file "file:///test/foo.txt")

let test_module_name_of_uri () =
  Alcotest.(check (option string))
    "tart file" (Some "foo")
    (Tracker.module_name_of_uri "file:///test/foo.tart");
  Alcotest.(check (option string))
    "el file" None
    (Tracker.module_name_of_uri "file:///test/foo.el")

let test_get_by_path () =
  let t = Tracker.create () in
  let uri = "file:///test/foo.tart" in
  Tracker.set t ~uri ~text:"contents";
  Alcotest.(check (option string))
    "lookup by path" (Some "contents")
    (Tracker.get_by_path t "/test/foo.tart")

(* =============================================================================
   Sibling File Helpers
   ============================================================================= *)

let test_sibling_el_uri () =
  Alcotest.(check (option string))
    "tart -> el" (Some "file:///test/foo.el")
    (Tracker.sibling_el_uri "file:///test/foo.tart");
  Alcotest.(check (option string))
    "non-tart returns None" None
    (Tracker.sibling_el_uri "file:///test/foo.el")

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "signature_tracker"
    [
      ("construction", [ Alcotest.test_case "create" `Quick test_create ]);
      ( "buffer management",
        [
          Alcotest.test_case "set and get" `Quick test_set_and_get;
          Alcotest.test_case "set updates" `Quick test_set_updates;
          Alcotest.test_case "remove" `Quick test_remove;
          Alcotest.test_case "mem" `Quick test_mem;
        ] );
      ( "uri helpers",
        [
          Alcotest.test_case "filename of uri" `Quick test_filename_of_uri;
          Alcotest.test_case "is tart file" `Quick test_is_tart_file;
          Alcotest.test_case "module name of uri" `Quick test_module_name_of_uri;
          Alcotest.test_case "get by path" `Quick test_get_by_path;
        ] );
      ( "sibling helpers",
        [ Alcotest.test_case "sibling el uri" `Quick test_sibling_el_uri ] );
    ]
