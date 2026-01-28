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

let test_nested_in_type_scope () =
  let sig_ =
    parse_tart
      {|
    (type-scope [a]
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

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
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
          Alcotest.test_case "nested in type-scope" `Quick
            test_nested_in_type_scope;
          Alcotest.test_case "no deps" `Quick test_no_deps_tart;
        ] );
      ( "sibling edge",
        [ Alcotest.test_case "make sibling edge" `Quick test_make_sibling_edge ]
      );
    ]
