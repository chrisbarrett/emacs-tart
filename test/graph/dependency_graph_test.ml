(** Tests for the dependency graph *)

module Graph = Tart.Dependency_graph

(* =============================================================================
   Basic Graph Operations
   ============================================================================= *)

let test_create_empty () =
  let g = Graph.create () in
  Alcotest.(check bool) "empty graph" true (Graph.is_empty g)

let test_add_edges () =
  let g = Graph.create () in
  let edges = [ { Graph.target = "bar"; kind = Graph.Require } ] in
  Graph.add_edges g "foo" edges;
  Alcotest.(check bool) "not empty after add" false (Graph.is_empty g);
  Alcotest.(check bool) "foo in graph" true (Graph.mem g "foo")

let test_get_edges () =
  let g = Graph.create () in
  let edges =
    [
      { Graph.target = "bar"; kind = Graph.Require };
      { Graph.target = "baz"; kind = Graph.Autoload };
    ]
  in
  Graph.add_edges g "foo" edges;
  let retrieved = Graph.get_edges g "foo" in
  Alcotest.(check int) "two edges" 2 (List.length retrieved)

let test_get_edges_empty () =
  let g = Graph.create () in
  let edges = Graph.get_edges g "nonexistent" in
  Alcotest.(check int) "no edges" 0 (List.length edges)

let test_remove_module () =
  let g = Graph.create () in
  Graph.add_edges g "foo" [ { Graph.target = "bar"; kind = Graph.Require } ];
  Graph.add_edges g "bar" [ { Graph.target = "baz"; kind = Graph.Require } ];
  Graph.remove_module g "foo";
  Alcotest.(check bool) "foo removed" false (Graph.mem g "foo");
  Alcotest.(check bool) "bar still exists" true (Graph.mem g "bar")

let test_replace_edges () =
  let g = Graph.create () in
  Graph.add_edges g "foo" [ { Graph.target = "bar"; kind = Graph.Require } ];
  Graph.add_edges g "foo" [ { Graph.target = "baz"; kind = Graph.Require } ];
  let edges = Graph.get_edges g "foo" in
  Alcotest.(check int) "one edge after replace" 1 (List.length edges);
  Alcotest.(check string) "target is baz" "baz" (List.hd edges).Graph.target

(* =============================================================================
   Direct Dependency Queries
   ============================================================================= *)

let test_direct_dependencies () =
  let g = Graph.create () in
  Graph.add_edges g "foo"
    [
      { Graph.target = "bar"; kind = Graph.Require };
      { Graph.target = "baz"; kind = Graph.Autoload };
    ];
  let deps = Graph.direct_dependencies g "foo" in
  Alcotest.(check int) "two direct deps" 2 (List.length deps);
  Alcotest.(check bool) "has bar" true (List.mem "bar" deps);
  Alcotest.(check bool) "has baz" true (List.mem "baz" deps)

let test_direct_dependents () =
  let g = Graph.create () in
  Graph.add_edges g "foo" [ { Graph.target = "bar"; kind = Graph.Require } ];
  Graph.add_edges g "baz" [ { Graph.target = "bar"; kind = Graph.Require } ];
  let deps = Graph.direct_dependents g "bar" in
  Alcotest.(check int) "two dependents" 2 (List.length deps);
  Alcotest.(check bool) "has foo" true (List.mem "foo" deps);
  Alcotest.(check bool) "has baz" true (List.mem "baz" deps)

let test_no_dependents () =
  let g = Graph.create () in
  Graph.add_edges g "foo" [ { Graph.target = "bar"; kind = Graph.Require } ];
  let deps = Graph.direct_dependents g "foo" in
  Alcotest.(check int) "no dependents" 0 (List.length deps)

(* =============================================================================
   Transitive Dependency Queries
   ============================================================================= *)

let test_transitive_dependencies () =
  let g = Graph.create () in
  Graph.add_edges g "a" [ { Graph.target = "b"; kind = Graph.Require } ];
  Graph.add_edges g "b" [ { Graph.target = "c"; kind = Graph.Require } ];
  Graph.add_edges g "c" [ { Graph.target = "d"; kind = Graph.Require } ];
  let deps = Graph.dependencies g "a" in
  Alcotest.(check int) "three transitive deps" 3 (List.length deps);
  Alcotest.(check bool) "has b" true (List.mem "b" deps);
  Alcotest.(check bool) "has c" true (List.mem "c" deps);
  Alcotest.(check bool) "has d" true (List.mem "d" deps)

let test_transitive_dependents () =
  let g = Graph.create () in
  Graph.add_edges g "a" [ { Graph.target = "b"; kind = Graph.Require } ];
  Graph.add_edges g "b" [ { Graph.target = "c"; kind = Graph.Require } ];
  Graph.add_edges g "c" [ { Graph.target = "d"; kind = Graph.Require } ];
  let deps = Graph.dependents g "d" in
  Alcotest.(check int) "three transitive dependents" 3 (List.length deps);
  Alcotest.(check bool) "has c" true (List.mem "c" deps);
  Alcotest.(check bool) "has b" true (List.mem "b" deps);
  Alcotest.(check bool) "has a" true (List.mem "a" deps)

let test_diamond_dependencies () =
  let g = Graph.create () in
  (* Diamond: a -> b, a -> c, b -> d, c -> d *)
  Graph.add_edges g "a"
    [
      { Graph.target = "b"; kind = Graph.Require };
      { Graph.target = "c"; kind = Graph.Require };
    ];
  Graph.add_edges g "b" [ { Graph.target = "d"; kind = Graph.Require } ];
  Graph.add_edges g "c" [ { Graph.target = "d"; kind = Graph.Require } ];
  let deps = Graph.dependencies g "a" in
  Alcotest.(check int) "three deps (d counted once)" 3 (List.length deps);
  let dependents = Graph.dependents g "d" in
  Alcotest.(check int)
    "three dependents (a counted once)" 3 (List.length dependents)

(* =============================================================================
   Edge Kinds
   ============================================================================= *)

let test_edge_kinds () =
  let g = Graph.create () in
  Graph.add_edges g "foo"
    [
      { Graph.target = "a"; kind = Graph.Require };
      { Graph.target = "b"; kind = Graph.Autoload };
      { Graph.target = "c"; kind = Graph.Open };
      { Graph.target = "d"; kind = Graph.Include };
      { Graph.target = "e"; kind = Graph.Sibling };
    ];
  let edges = Graph.get_edges g "foo" in
  Alcotest.(check int) "five edges" 5 (List.length edges);
  let kinds =
    List.map
      (fun e ->
        match e.Graph.kind with
        | Graph.Require -> "require"
        | Graph.Autoload -> "autoload"
        | Graph.Open -> "open"
        | Graph.Include -> "include"
        | Graph.Sibling -> "sibling")
      edges
  in
  Alcotest.(check bool) "has require" true (List.mem "require" kinds);
  Alcotest.(check bool) "has autoload" true (List.mem "autoload" kinds);
  Alcotest.(check bool) "has open" true (List.mem "open" kinds);
  Alcotest.(check bool) "has include" true (List.mem "include" kinds);
  Alcotest.(check bool) "has sibling" true (List.mem "sibling" kinds)

(* =============================================================================
   Cycle Detection
   ============================================================================= *)

let test_no_cycles () =
  let g = Graph.create () in
  Graph.add_edges g "a" [ { Graph.target = "b"; kind = Graph.Require } ];
  Graph.add_edges g "b" [ { Graph.target = "c"; kind = Graph.Require } ];
  let cycles = Graph.detect_cycles g in
  Alcotest.(check int) "no cycles" 0 (List.length cycles)

let test_simple_cycle () =
  let g = Graph.create () in
  Graph.add_edges g "a" [ { Graph.target = "b"; kind = Graph.Require } ];
  Graph.add_edges g "b" [ { Graph.target = "a"; kind = Graph.Require } ];
  let cycles = Graph.detect_cycles g in
  Alcotest.(check int) "one cycle" 1 (List.length cycles);
  let cycle = List.hd cycles in
  Alcotest.(check int) "cycle length 2" 2 (List.length cycle)

let test_three_node_cycle () =
  let g = Graph.create () in
  Graph.add_edges g "a" [ { Graph.target = "b"; kind = Graph.Require } ];
  Graph.add_edges g "b" [ { Graph.target = "c"; kind = Graph.Require } ];
  Graph.add_edges g "c" [ { Graph.target = "a"; kind = Graph.Require } ];
  let cycles = Graph.detect_cycles g in
  Alcotest.(check int) "one cycle" 1 (List.length cycles);
  let cycle = List.hd cycles in
  Alcotest.(check int) "cycle length 3" 3 (List.length cycle)

let test_self_loop () =
  let g = Graph.create () in
  Graph.add_edges g "a" [ { Graph.target = "a"; kind = Graph.Require } ];
  let cycles = Graph.detect_cycles g in
  Alcotest.(check int) "one cycle (self-loop)" 1 (List.length cycles)

(* =============================================================================
   Module List
   ============================================================================= *)

let test_modules_list () =
  let g = Graph.create () in
  Graph.add_edges g "foo" [ { Graph.target = "bar"; kind = Graph.Require } ];
  Graph.add_edges g "baz" [ { Graph.target = "bar"; kind = Graph.Require } ];
  let mods = Graph.modules g in
  Alcotest.(check int) "three modules" 3 (List.length mods);
  Alcotest.(check bool) "has foo" true (List.mem "foo" mods);
  Alcotest.(check bool) "has bar" true (List.mem "bar" mods);
  Alcotest.(check bool) "has baz" true (List.mem "baz" mods)

(* =============================================================================
   Test Suite
   ============================================================================= *)

let () =
  Alcotest.run "dependency_graph"
    [
      ( "basic",
        [
          Alcotest.test_case "create empty" `Quick test_create_empty;
          Alcotest.test_case "add edges" `Quick test_add_edges;
          Alcotest.test_case "get edges" `Quick test_get_edges;
          Alcotest.test_case "get edges empty" `Quick test_get_edges_empty;
          Alcotest.test_case "remove module" `Quick test_remove_module;
          Alcotest.test_case "replace edges" `Quick test_replace_edges;
        ] );
      ( "direct queries",
        [
          Alcotest.test_case "direct dependencies" `Quick
            test_direct_dependencies;
          Alcotest.test_case "direct dependents" `Quick test_direct_dependents;
          Alcotest.test_case "no dependents" `Quick test_no_dependents;
        ] );
      ( "transitive queries",
        [
          Alcotest.test_case "transitive dependencies" `Quick
            test_transitive_dependencies;
          Alcotest.test_case "transitive dependents" `Quick
            test_transitive_dependents;
          Alcotest.test_case "diamond dependencies" `Quick
            test_diamond_dependencies;
        ] );
      ( "edge kinds",
        [ Alcotest.test_case "all edge kinds" `Quick test_edge_kinds ] );
      ( "cycle detection",
        [
          Alcotest.test_case "no cycles" `Quick test_no_cycles;
          Alcotest.test_case "simple cycle" `Quick test_simple_cycle;
          Alcotest.test_case "three node cycle" `Quick test_three_node_cycle;
          Alcotest.test_case "self loop" `Quick test_self_loop;
        ] );
      ( "module list",
        [ Alcotest.test_case "modules list" `Quick test_modules_list ] );
    ]
