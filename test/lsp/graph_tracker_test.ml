(** Tests for the graph tracker LSP integration *)

module Graph_tracker = Tart.Server

(* We test via the server module which exposes the graph *)
module Dep_graph = Tart.Dependency_graph

(* =============================================================================
   Helpers
   ============================================================================= *)

(** Create a test server for graph testing *)
let make_test_server () =
  let ic = In_channel.stdin in
  let oc = Out_channel.stdout in
  Tart.Server.create ~ic ~oc ()

(* =============================================================================
   Module ID Extraction Tests
   ============================================================================= *)

let test_module_id_from_el () =
  let id = Lsp.Graph_tracker.module_id_of_filename "/path/to/foo.el" in
  Alcotest.(check string) "el file" "foo" id

let test_module_id_from_tart () =
  let id = Lsp.Graph_tracker.module_id_of_filename "/path/to/bar.tart" in
  Alcotest.(check string) "tart file" "bar" id

let test_module_id_from_other () =
  let id = Lsp.Graph_tracker.module_id_of_filename "/path/to/baz.txt" in
  Alcotest.(check string) "other file" "baz.txt" id

let test_filename_of_uri () =
  let filename = Lsp.Uri.to_filename "file:///path/to/foo.el" in
  Alcotest.(check string) "extract path" "/path/to/foo.el" filename

let test_filename_of_uri_non_file () =
  let filename = Lsp.Uri.to_filename "http://example.com/foo.el" in
  Alcotest.(check string) "non-file URI" "http://example.com/foo.el" filename

(* =============================================================================
   Graph Update Tests
   ============================================================================= *)

let test_update_el_document () =
  let server = make_test_server () in
  let graph = Tart.Server.dependency_graph server in
  let uri = "file:///test/foo.el" in
  let text = "(require 'bar)\n(require 'baz)" in
  Lsp.Graph_tracker.update_document graph ~uri ~text;
  (* Check that module is in the graph *)
  Alcotest.(check bool) "module in graph" true (Dep_graph.mem graph "foo");
  (* Check dependencies *)
  let deps = Dep_graph.direct_dependencies graph "foo" in
  (* Should include bar, baz, and core typings *)
  Alcotest.(check bool) "has bar" true (List.mem "bar" deps);
  Alcotest.(check bool) "has baz" true (List.mem "baz" deps);
  Alcotest.(check bool)
    "has core typings" true
    (List.mem Tart.Graph_builder.core_typings_module_id deps)

let test_update_tart_document () =
  let server = make_test_server () in
  let graph = Tart.Server.dependency_graph server in
  let uri = "file:///test/foo.tart" in
  let text = "(open 'seq)\n(defun my-fn (int) -> int)" in
  Lsp.Graph_tracker.update_document graph ~uri ~text;
  (* Check that module is in the graph *)
  Alcotest.(check bool) "module in graph" true (Dep_graph.mem graph "foo");
  (* Check dependencies *)
  let deps = Dep_graph.direct_dependencies graph "foo" in
  Alcotest.(check bool) "has seq" true (List.mem "seq" deps)

let test_update_document_re_extract () =
  let server = make_test_server () in
  let graph = Tart.Server.dependency_graph server in
  let uri = "file:///test/foo.el" in
  (* First update with bar dependency *)
  let text1 = "(require 'bar)" in
  Lsp.Graph_tracker.update_document graph ~uri ~text:text1;
  let deps1 = Dep_graph.direct_dependencies graph "foo" in
  Alcotest.(check bool) "has bar initially" true (List.mem "bar" deps1);
  (* Second update changes to baz *)
  let text2 = "(require 'baz)" in
  Lsp.Graph_tracker.update_document graph ~uri ~text:text2;
  let deps2 = Dep_graph.direct_dependencies graph "foo" in
  Alcotest.(check bool) "no longer has bar" false (List.mem "bar" deps2);
  Alcotest.(check bool) "now has baz" true (List.mem "baz" deps2)

let test_close_document_keeps_entry () =
  let server = make_test_server () in
  let graph = Tart.Server.dependency_graph server in
  let uri = "file:///test/foo.el" in
  let text = "(require 'bar)" in
  Lsp.Graph_tracker.update_document graph ~uri ~text;
  Alcotest.(check bool)
    "module in graph before" true
    (Dep_graph.mem graph "foo");
  (* Close document *)
  Lsp.Graph_tracker.close_document graph ~uri;
  (* Entry should still be present per spec R4 *)
  Alcotest.(check bool) "module still in graph" true (Dep_graph.mem graph "foo")

let test_autoload_edges () =
  let server = make_test_server () in
  let graph = Tart.Server.dependency_graph server in
  let uri = "file:///test/foo.el" in
  let text = {|(autoload 'my-fn "my-module" "Doc" t)|} in
  Lsp.Graph_tracker.update_document graph ~uri ~text;
  let deps = Dep_graph.direct_dependencies graph "foo" in
  Alcotest.(check bool) "has autoload dep" true (List.mem "my-module" deps)

let test_include_edges () =
  let server = make_test_server () in
  let graph = Tart.Server.dependency_graph server in
  let uri = "file:///test/foo.tart" in
  let text = "(include 'dash)" in
  Lsp.Graph_tracker.update_document graph ~uri ~text;
  let deps = Dep_graph.direct_dependencies graph "foo" in
  Alcotest.(check bool) "has include dep" true (List.mem "dash" deps)

let test_reverse_index () =
  let server = make_test_server () in
  let graph = Tart.Server.dependency_graph server in
  (* foo depends on bar *)
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/foo.el"
    ~text:"(require 'bar)";
  (* baz also depends on bar *)
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/baz.el"
    ~text:"(require 'bar)";
  (* Check reverse index - who depends on bar? *)
  let dependents = Dep_graph.direct_dependents graph "bar" in
  Alcotest.(check bool) "foo depends on bar" true (List.mem "foo" dependents);
  Alcotest.(check bool) "baz depends on bar" true (List.mem "baz" dependents)

(* =============================================================================
   Invalidation Cascade Tests
   ============================================================================= *)

let test_dependent_uris () =
  let server = make_test_server () in
  let graph = Tart.Server.dependency_graph server in
  (* Set up dependency: foo depends on bar *)
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/foo.el"
    ~text:"(require 'bar)";
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/bar.el" ~text:"";
  (* foo.el and bar.el are open *)
  let open_uris = [ "file:///test/foo.el"; "file:///test/bar.el" ] in
  (* When bar changes, foo should be a dependent *)
  let deps =
    Lsp.Graph_tracker.dependent_uris graph ~module_id:"bar" ~open_uris
  in
  Alcotest.(check bool)
    "foo is dependent" true
    (List.mem "file:///test/foo.el" deps);
  Alcotest.(check bool)
    "bar is not self-dependent" false
    (List.mem "file:///test/bar.el" deps)

let test_dependent_uris_transitive () =
  let server = make_test_server () in
  let graph = Tart.Server.dependency_graph server in
  (* Set up transitive dependency: foo -> bar -> baz *)
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/foo.el"
    ~text:"(require 'bar)";
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/bar.el"
    ~text:"(require 'baz)";
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/baz.el" ~text:"";
  let open_uris =
    [ "file:///test/foo.el"; "file:///test/bar.el"; "file:///test/baz.el" ]
  in
  (* When baz changes, both foo and bar should be dependents *)
  let deps =
    Lsp.Graph_tracker.dependent_uris graph ~module_id:"baz" ~open_uris
  in
  Alcotest.(check bool)
    "bar is direct dependent" true
    (List.mem "file:///test/bar.el" deps);
  Alcotest.(check bool)
    "foo is transitive dependent" true
    (List.mem "file:///test/foo.el" deps)

let test_dependent_uris_only_open () =
  let server = make_test_server () in
  let graph = Tart.Server.dependency_graph server in
  (* foo depends on bar, but foo is not in open_uris *)
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/foo.el"
    ~text:"(require 'bar)";
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/bar.el" ~text:"";
  let open_uris = [ "file:///test/bar.el" ] in
  let deps =
    Lsp.Graph_tracker.dependent_uris graph ~module_id:"bar" ~open_uris
  in
  Alcotest.(check int) "no open dependents" 0 (List.length deps)

let test_module_id_of_uri () =
  let id = Lsp.Graph_tracker.module_id_of_uri "file:///test/foo.el" in
  Alcotest.(check string) "from uri" "foo" id

(* =============================================================================
   Cycle Detection Tests
   ============================================================================= *)

let severity_equal (a : Lsp.Protocol.diagnostic_severity option)
    (b : Lsp.Protocol.diagnostic_severity option) =
  match (a, b) with
  | Some Lsp.Protocol.Error, Some Lsp.Protocol.Error -> true
  | Some Lsp.Protocol.Warning, Some Lsp.Protocol.Warning -> true
  | Some Lsp.Protocol.Information, Some Lsp.Protocol.Information -> true
  | Some Lsp.Protocol.Hint, Some Lsp.Protocol.Hint -> true
  | None, None -> true
  | _ -> false

let test_cycle_detection_simple () =
  let server = make_test_server () in
  let graph = Tart.Server.dependency_graph server in
  (* Set up cycle: foo -> bar -> foo *)
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/foo.el"
    ~text:"(require 'bar)";
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/bar.el"
    ~text:"(require 'foo)";
  (* Check for cycles in foo *)
  let diags =
    Lsp.Graph_tracker.check_cycles_for_module graph ~uri:"file:///test/foo.el"
  in
  Alcotest.(check int) "one cycle" 1 (List.length diags);
  let diag = List.hd diags in
  Alcotest.(check bool)
    "severity is Warning for .el" true
    (severity_equal diag.severity (Some Lsp.Protocol.Warning))

let test_cycle_detection_tart_error () =
  let server = make_test_server () in
  let graph = Tart.Server.dependency_graph server in
  (* Set up cycle with .tart file: foo.tart -> bar.tart -> foo.tart *)
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/foo.tart"
    ~text:"(open 'bar)";
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/bar.tart"
    ~text:"(open 'foo)";
  (* Check for cycles in foo.tart *)
  let diags =
    Lsp.Graph_tracker.check_cycles_for_module graph ~uri:"file:///test/foo.tart"
  in
  Alcotest.(check int) "one cycle" 1 (List.length diags);
  let diag = List.hd diags in
  Alcotest.(check bool)
    "severity is Error for .tart" true
    (severity_equal diag.severity (Some Lsp.Protocol.Error))

let test_cycle_detection_no_cycle () =
  let server = make_test_server () in
  let graph = Tart.Server.dependency_graph server in
  (* Linear dependency: foo -> bar -> baz (no cycle) *)
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/foo.el"
    ~text:"(require 'bar)";
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/bar.el"
    ~text:"(require 'baz)";
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/baz.el" ~text:"";
  let diags =
    Lsp.Graph_tracker.check_cycles_for_module graph ~uri:"file:///test/foo.el"
  in
  Alcotest.(check int) "no cycles" 0 (List.length diags)

let test_cycle_message () =
  let server = make_test_server () in
  let graph = Tart.Server.dependency_graph server in
  (* foo -> bar -> foo *)
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/foo.el"
    ~text:"(require 'bar)";
  Lsp.Graph_tracker.update_document graph ~uri:"file:///test/bar.el"
    ~text:"(require 'foo)";
  let diags =
    Lsp.Graph_tracker.check_cycles_for_module graph ~uri:"file:///test/foo.el"
  in
  let diag = List.hd diags in
  Alcotest.(check bool)
    "message mentions cycle" true
    (String.sub diag.message 0 12 = "Circular dep")

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "graph_tracker"
    [
      ( "module_id",
        [
          Alcotest.test_case "from el file" `Quick test_module_id_from_el;
          Alcotest.test_case "from tart file" `Quick test_module_id_from_tart;
          Alcotest.test_case "from other file" `Quick test_module_id_from_other;
          Alcotest.test_case "filename of uri" `Quick test_filename_of_uri;
          Alcotest.test_case "filename of non-file uri" `Quick
            test_filename_of_uri_non_file;
          Alcotest.test_case "module id of uri" `Quick test_module_id_of_uri;
        ] );
      ( "graph updates",
        [
          Alcotest.test_case "update el document" `Quick test_update_el_document;
          Alcotest.test_case "update tart document" `Quick
            test_update_tart_document;
          Alcotest.test_case "re-extract on change" `Quick
            test_update_document_re_extract;
          Alcotest.test_case "close keeps entry" `Quick
            test_close_document_keeps_entry;
          Alcotest.test_case "autoload edges" `Quick test_autoload_edges;
          Alcotest.test_case "include edges" `Quick test_include_edges;
          Alcotest.test_case "reverse index" `Quick test_reverse_index;
        ] );
      ( "invalidation cascade",
        [
          Alcotest.test_case "dependent uris" `Quick test_dependent_uris;
          Alcotest.test_case "transitive dependents" `Quick
            test_dependent_uris_transitive;
          Alcotest.test_case "only open uris" `Quick
            test_dependent_uris_only_open;
        ] );
      ( "cycle detection",
        [
          Alcotest.test_case "simple cycle" `Quick test_cycle_detection_simple;
          Alcotest.test_case "tart error severity" `Quick
            test_cycle_detection_tart_error;
          Alcotest.test_case "no cycle" `Quick test_cycle_detection_no_cycle;
          Alcotest.test_case "cycle message" `Quick test_cycle_message;
        ] );
    ]
