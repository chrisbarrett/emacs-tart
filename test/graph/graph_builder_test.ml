(** Tests for the graph builder *)

module Graph = Tart.Dependency_graph
module Builder = Tart.Graph_builder
module Read = Tart.Read
module Sig_parser = Tart.Sig_parser

(* =============================================================================
   Helpers
   ============================================================================= *)

let parse_el source = (Read.parse_string source).sexps

let parse_tart source =
  let sexps = (Read.parse_string source).sexps in
  match Sig_parser.parse_signature ~module_name:"test" sexps with
  | Ok sig_ -> sig_
  | Error errors ->
      let msgs =
        List.map (fun e -> e.Sig_parser.message) errors |> String.concat "; "
      in
      failwith ("Parse error: " ^ msgs)

let edge_list_testable =
  Alcotest.testable
    (fun fmt edges ->
      Format.fprintf fmt "[%s]"
        (String.concat ", "
           (List.map
              (fun e ->
                Printf.sprintf "(%s, %s)" e.Graph.target
                  (match e.Graph.kind with
                  | Graph.Require -> "Require"
                  | Graph.Autoload -> "Autoload"
                  | Graph.Open -> "Open"
                  | Graph.Include -> "Include"
                  | Graph.Sibling -> "Sibling"))
              edges)))
    (fun a b ->
      let sort = List.sort compare in
      let a' = List.map (fun e -> (e.Graph.target, e.Graph.kind)) a in
      let b' = List.map (fun e -> (e.Graph.target, e.Graph.kind)) b in
      sort a' = sort b')

(* =============================================================================
   .el File Extraction
   ============================================================================= *)

let test_require_quoted () =
  let forms = parse_el "(require 'foo)" in
  let edges = Builder.extract_from_sexp forms in
  Alcotest.(check edge_list_testable)
    "require edge"
    [ { Graph.target = "foo"; kind = Graph.Require } ]
    edges

let test_require_with_filename () =
  let forms = parse_el {|(require 'foo "foo.el")|} in
  let edges = Builder.extract_from_sexp forms in
  Alcotest.(check edge_list_testable)
    "require edge"
    [ { Graph.target = "foo"; kind = Graph.Require } ]
    edges

let test_autoload () =
  let forms = parse_el {|(autoload 'my-func "my-module")|} in
  let edges = Builder.extract_from_sexp forms in
  Alcotest.(check edge_list_testable)
    "autoload edge"
    [ { Graph.target = "my-module"; kind = Graph.Autoload } ]
    edges

let test_autoload_with_docstring () =
  let forms = parse_el {|(autoload 'my-func "my-module" "Doc string" t)|} in
  let edges = Builder.extract_from_sexp forms in
  Alcotest.(check edge_list_testable)
    "autoload edge"
    [ { Graph.target = "my-module"; kind = Graph.Autoload } ]
    edges

let test_multiple_requires () =
  let forms =
    parse_el {|
    (require 'foo)
    (require 'bar)
    (require 'baz)
  |}
  in
  let edges = Builder.extract_from_sexp forms in
  Alcotest.(check int) "three edges" 3 (List.length edges);
  Alcotest.(check edge_list_testable)
    "require edges"
    [
      { Graph.target = "foo"; kind = Graph.Require };
      { Graph.target = "bar"; kind = Graph.Require };
      { Graph.target = "baz"; kind = Graph.Require };
    ]
    edges

let test_mixed_requires_and_autoloads () =
  let forms =
    parse_el
      {|
    (require 'foo)
    (autoload 'bar-fn "bar")
    (require 'baz)
  |}
  in
  let edges = Builder.extract_from_sexp forms in
  Alcotest.(check int) "three edges" 3 (List.length edges);
  Alcotest.(check edge_list_testable)
    "mixed edges"
    [
      { Graph.target = "foo"; kind = Graph.Require };
      { Graph.target = "bar"; kind = Graph.Autoload };
      { Graph.target = "baz"; kind = Graph.Require };
    ]
    edges

let test_nested_require () =
  let forms =
    parse_el {|
    (when (featurep 'gui)
      (require 'foo))
  |}
  in
  let edges = Builder.extract_from_sexp forms in
  Alcotest.(check edge_list_testable)
    "nested require"
    [ { Graph.target = "foo"; kind = Graph.Require } ]
    edges

let test_duplicate_requires () =
  let forms = parse_el {|
    (require 'foo)
    (require 'foo)
  |} in
  let edges = Builder.extract_from_sexp forms in
  Alcotest.(check int) "deduplicated" 1 (List.length edges)

let test_no_deps () =
  let forms = parse_el "(defun my-fn () nil)" in
  let edges = Builder.extract_from_sexp forms in
  Alcotest.(check edge_list_testable) "no edges" [] edges

let test_invalid_require () =
  (* require without quoted symbol should be ignored *)
  let forms = parse_el "(require foo)" in
  let edges = Builder.extract_from_sexp forms in
  Alcotest.(check edge_list_testable) "no edges" [] edges

let test_invalid_autoload () =
  (* autoload without string module name should be ignored *)
  let forms = parse_el "(autoload 'fn foo)" in
  let edges = Builder.extract_from_sexp forms in
  Alcotest.(check edge_list_testable) "no edges" [] edges

(* =============================================================================
   .tart File Extraction
   ============================================================================= *)

let test_open () =
  let sig_ = parse_tart "(open 'foo)" in
  let edges = Builder.extract_from_signature sig_ in
  Alcotest.(check edge_list_testable)
    "open edge"
    [ { Graph.target = "foo"; kind = Graph.Open } ]
    edges

let test_include () =
  let sig_ = parse_tart "(include 'bar)" in
  let edges = Builder.extract_from_signature sig_ in
  Alcotest.(check edge_list_testable)
    "include edge"
    [ { Graph.target = "bar"; kind = Graph.Include } ]
    edges

let test_multiple_opens () =
  let sig_ =
    parse_tart {|
    (open 'foo)
    (open 'bar)
    (open 'baz)
  |}
  in
  let edges = Builder.extract_from_signature sig_ in
  Alcotest.(check int) "three edges" 3 (List.length edges);
  Alcotest.(check edge_list_testable)
    "open edges"
    [
      { Graph.target = "foo"; kind = Graph.Open };
      { Graph.target = "bar"; kind = Graph.Open };
      { Graph.target = "baz"; kind = Graph.Open };
    ]
    edges

let test_mixed_open_include () =
  let sig_ = parse_tart {|
    (open 'foo)
    (include 'bar)
  |} in
  let edges = Builder.extract_from_signature sig_ in
  Alcotest.(check edge_list_testable)
    "mixed edges"
    [
      { Graph.target = "foo"; kind = Graph.Open };
      { Graph.target = "bar"; kind = Graph.Include };
    ]
    edges

let test_tart_with_defun () =
  let sig_ =
    parse_tart {|
    (open 'seq)
    (defun my-fn (int) -> int)
  |}
  in
  let edges = Builder.extract_from_signature sig_ in
  Alcotest.(check edge_list_testable)
    "only open edge"
    [ { Graph.target = "seq"; kind = Graph.Open } ]
    edges

let test_nested_in_forall () =
  let sig_ =
    parse_tart
      {|
    (forall [a]
      (open 'foo)
      (defun my-fn (a) -> a))
  |}
  in
  let edges = Builder.extract_from_signature sig_ in
  Alcotest.(check edge_list_testable)
    "open in scope"
    [ { Graph.target = "foo"; kind = Graph.Open } ]
    edges

let test_no_deps_tart () =
  let sig_ = parse_tart "(defun my-fn (int) -> int)" in
  let edges = Builder.extract_from_signature sig_ in
  Alcotest.(check edge_list_testable) "no edges" [] edges

(* =============================================================================
   Sibling Edge
   ============================================================================= *)

let test_make_sibling_edge () =
  let edge = Builder.make_sibling_edge "foo" in
  Alcotest.(check string) "target" "foo" edge.Graph.target;
  Alcotest.(check bool) "kind is Sibling" true (edge.Graph.kind = Graph.Sibling)

(* Create a temporary directory for testing sibling edge detection *)
let with_temp_dir f =
  let dir = Filename.get_temp_dir_name () in
  let test_dir =
    Filename.concat dir
      ("graph_builder_test_" ^ string_of_int (Random.int 100000))
  in
  Unix.mkdir test_dir 0o755;
  Fun.protect
    ~finally:(fun () ->
      (* Clean up test files *)
      Array.iter
        (fun file -> Unix.unlink (Filename.concat test_dir file))
        (Sys.readdir test_dir);
      Unix.rmdir test_dir)
    (fun () -> f test_dir)

let test_sibling_edge_found () =
  with_temp_dir (fun dir ->
      (* Create foo.el and foo.tart *)
      let el_path = Filename.concat dir "foo.el" in
      let tart_path = Filename.concat dir "foo.tart" in
      Out_channel.with_open_text el_path (fun oc ->
          output_string oc "(provide 'foo)");
      Out_channel.with_open_text tart_path (fun oc ->
          output_string oc "(defun foo () -> nil)");

      (* Check sibling edge is found *)
      let edge = Builder.sibling_edge_for_el_file el_path in
      match edge with
      | Some e ->
          Alcotest.(check string) "target" "foo" e.Graph.target;
          Alcotest.(check bool) "kind" true (e.Graph.kind = Graph.Sibling)
      | None -> Alcotest.fail "Expected sibling edge to be found")

let test_sibling_edge_not_found () =
  with_temp_dir (fun dir ->
      (* Create only foo.el, no foo.tart *)
      let el_path = Filename.concat dir "bar.el" in
      Out_channel.with_open_text el_path (fun oc ->
          output_string oc "(provide 'bar)");

      (* Check no sibling edge *)
      let edge = Builder.sibling_edge_for_el_file el_path in
      Alcotest.(check bool) "no sibling" true (Option.is_none edge))

let test_sibling_edge_non_el_file () =
  (* Non-.el file should return None *)
  let edge = Builder.sibling_edge_for_el_file "/some/path/foo.txt" in
  Alcotest.(check bool) "not el file" true (Option.is_none edge)

(* =============================================================================
   Core Typings Pseudo-Module
   ============================================================================= *)

let test_core_typings_module_id () =
  let id = Builder.core_typings_module_id in
  (* Should be a special marker that won't conflict with real modules *)
  Alcotest.(check bool)
    "starts with @@" true
    (String.length id > 2 && String.sub id 0 2 = "@@")

let test_make_core_typings_edge () =
  let edge = Builder.make_core_typings_edge () in
  Alcotest.(check string)
    "target" Builder.core_typings_module_id edge.Graph.target;
  Alcotest.(check bool) "kind is Require" true (edge.Graph.kind = Graph.Require)

let test_is_core_typings_module () =
  Alcotest.(check bool)
    "true for core module" true
    (Builder.is_core_typings_module Builder.core_typings_module_id);
  Alcotest.(check bool)
    "false for regular module" false
    (Builder.is_core_typings_module "cl-lib")

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Random.self_init ();
  Alcotest.run "graph_builder"
    [
      ( "el extraction",
        [
          Alcotest.test_case "require quoted" `Quick test_require_quoted;
          Alcotest.test_case "require with filename" `Quick
            test_require_with_filename;
          Alcotest.test_case "autoload" `Quick test_autoload;
          Alcotest.test_case "autoload with docstring" `Quick
            test_autoload_with_docstring;
          Alcotest.test_case "multiple requires" `Quick test_multiple_requires;
          Alcotest.test_case "mixed requires and autoloads" `Quick
            test_mixed_requires_and_autoloads;
          Alcotest.test_case "nested require" `Quick test_nested_require;
          Alcotest.test_case "duplicate requires" `Quick test_duplicate_requires;
          Alcotest.test_case "no deps" `Quick test_no_deps;
          Alcotest.test_case "invalid require" `Quick test_invalid_require;
          Alcotest.test_case "invalid autoload" `Quick test_invalid_autoload;
        ] );
      ( "tart extraction",
        [
          Alcotest.test_case "open" `Quick test_open;
          Alcotest.test_case "include" `Quick test_include;
          Alcotest.test_case "multiple opens" `Quick test_multiple_opens;
          Alcotest.test_case "mixed open include" `Quick test_mixed_open_include;
          Alcotest.test_case "with defun" `Quick test_tart_with_defun;
          Alcotest.test_case "nested in forall" `Quick test_nested_in_forall;
          Alcotest.test_case "no deps" `Quick test_no_deps_tart;
        ] );
      ( "sibling edge",
        [
          Alcotest.test_case "make sibling edge" `Quick test_make_sibling_edge;
          Alcotest.test_case "sibling edge found" `Quick test_sibling_edge_found;
          Alcotest.test_case "sibling edge not found" `Quick
            test_sibling_edge_not_found;
          Alcotest.test_case "sibling edge non-el file" `Quick
            test_sibling_edge_non_el_file;
        ] );
      ( "core typings",
        [
          Alcotest.test_case "module id" `Quick test_core_typings_module_id;
          Alcotest.test_case "make edge" `Quick test_make_core_typings_edge;
          Alcotest.test_case "is core typings" `Quick
            test_is_core_typings_module;
        ] );
    ]
