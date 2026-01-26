(** Tests for the Instance module - type class instance resolution. *)

open Typing.Instance
open Core.Types

(** {1 Test Helpers} *)

(** Create a simple monomorphic instance *)
let simple_instance class_name type_name =
  {
    inst_class = class_name;
    inst_type = TCon type_name;
    inst_tvars = [];
    inst_constraints = [];
  }

(** Create a parameterized instance like (Eq (list a)) with constraint (Eq a) *)
let parameterized_instance class_name type_con tvar constraint_class =
  {
    inst_class = class_name;
    inst_type = TApp (TCon type_con, [ TCon tvar ]);
    inst_tvars = [ tvar ];
    inst_constraints = [ (constraint_class, TCon tvar) ];
  }

(** {1 Registry Tests} *)

let test_empty_registry () =
  let reg = empty_registry in
  Alcotest.(check int)
    "empty has no instances" 0
    (List.length (all_instances reg))

let test_add_instance () =
  let reg = empty_registry in
  let inst = simple_instance "Eq" "Int" in
  let reg' = add_instance inst reg in
  Alcotest.(check int) "one instance" 1 (List.length (all_instances reg'))

let test_instances_for_class () =
  let reg = empty_registry in
  let eq_int = simple_instance "Eq" "Int" in
  let eq_string = simple_instance "Eq" "String" in
  let ord_int = simple_instance "Ord" "Int" in
  let reg' =
    add_instance eq_int (add_instance eq_string (add_instance ord_int reg))
  in
  let eq_instances = instances_for_class "Eq" reg' in
  Alcotest.(check int) "two Eq instances" 2 (List.length eq_instances);
  let ord_instances = instances_for_class "Ord" reg' in
  Alcotest.(check int) "one Ord instance" 1 (List.length ord_instances)

(** {1 Simple Resolution Tests} *)

let test_resolve_simple_found () =
  let reg = add_instance (simple_instance "Eq" "Int") empty_registry in
  match resolve_constraint reg ("Eq", TCon "Int") with
  | Resolved -> ()
  | NotFound _ -> Alcotest.fail "Expected Resolved"
  | Recursive _ -> Alcotest.fail "Expected Resolved, got Recursive"

let test_resolve_simple_not_found () =
  let reg = add_instance (simple_instance "Eq" "Int") empty_registry in
  match resolve_constraint reg ("Eq", TCon "String") with
  | NotFound (cls, _ty) -> Alcotest.(check string) "class name" "Eq" cls
  | Resolved -> Alcotest.fail "Expected NotFound"
  | Recursive _ -> Alcotest.fail "Expected NotFound"

let test_resolve_wrong_class () =
  let reg = add_instance (simple_instance "Eq" "Int") empty_registry in
  match resolve_constraint reg ("Ord", TCon "Int") with
  | NotFound (cls, _ty) -> Alcotest.(check string) "class name" "Ord" cls
  | Resolved -> Alcotest.fail "Expected NotFound"
  | Recursive _ -> Alcotest.fail "Expected NotFound"

(** {1 Parameterized Instance Tests} *)

let test_resolve_parameterized_returns_recursive () =
  (* (instance [a] (Eq a) => (Eq (list a))) *)
  let inst = parameterized_instance "Eq" "List" "a" "Eq" in
  let reg = add_instance inst empty_registry in
  match resolve_constraint reg ("Eq", TApp (TCon "List", [ TCon "Int" ])) with
  | Recursive constraints ->
      Alcotest.(check int) "one sub-constraint" 1 (List.length constraints);
      let cls, ty = List.hd constraints in
      Alcotest.(check string) "constraint class" "Eq" cls;
      Alcotest.(check string) "constraint type" "Int" (to_string ty)
  | Resolved -> Alcotest.fail "Expected Recursive"
  | NotFound _ -> Alcotest.fail "Expected Recursive"

(** {1 Full Resolution Tests} *)

let test_resolve_all_simple () =
  let reg =
    empty_registry
    |> add_instance (simple_instance "Eq" "Int")
    |> add_instance (simple_instance "Ord" "Int")
  in
  match resolve_all reg [ ("Eq", TCon "Int"); ("Ord", TCon "Int") ] with
  | Ok () -> ()
  | Error (cls, _ty) ->
      Alcotest.fail (Printf.sprintf "Expected Ok, got Error for %s" cls)

let test_resolve_all_missing () =
  let reg = add_instance (simple_instance "Eq" "Int") empty_registry in
  match resolve_all reg [ ("Eq", TCon "Int"); ("Ord", TCon "Int") ] with
  | Error (cls, _ty) -> Alcotest.(check string) "missing class" "Ord" cls
  | Ok () -> Alcotest.fail "Expected Error"

let test_resolve_all_recursive () =
  (* Given (Eq int) and (Eq (list a)) with constraint (Eq a),
     resolving (Eq (list int)) should succeed by recursively resolving (Eq int) *)
  let reg =
    empty_registry
    |> add_instance (simple_instance "Eq" "Int")
    |> add_instance (parameterized_instance "Eq" "List" "a" "Eq")
  in
  match resolve_all reg [ ("Eq", TApp (TCon "List", [ TCon "Int" ])) ] with
  | Ok () -> ()
  | Error (cls, ty) ->
      Alcotest.fail
        (Printf.sprintf "Expected Ok, got Error for (%s %s)" cls (to_string ty))

let test_resolve_all_recursive_missing () =
  (* Given only (Eq (list a)) with constraint (Eq a),
     resolving (Eq (list buffer)) should fail because (Eq buffer) doesn't exist *)
  let reg =
    add_instance (parameterized_instance "Eq" "List" "a" "Eq") empty_registry
  in
  match resolve_all reg [ ("Eq", TApp (TCon "List", [ TCon "Buffer" ])) ] with
  | Error (cls, ty) ->
      Alcotest.(check string) "missing class" "Eq" cls;
      Alcotest.(check string) "missing type" "Buffer" (to_string ty)
  | Ok () -> Alcotest.fail "Expected Error for missing (Eq Buffer)"

(** {1 Load Instance Tests} *)

let make_instance_decl ~class_name ~type_sexp ~tvars ~constraints =
  let open Sig.Sig_ast in
  {
    inst_class = class_name;
    inst_type = type_sexp;
    inst_tvar_binders =
      List.map
        (fun name ->
          { name; bound = None; kind = None; loc = Syntax.Location.dummy_span })
        tvars;
    inst_constraints = constraints;
    inst_methods = [];
    inst_loc = Syntax.Location.dummy_span;
  }

let test_load_simple_instance () =
  let open Sig.Sig_ast in
  let decl =
    make_instance_decl ~class_name:"Eq"
      ~type_sexp:(STCon ("Int", Syntax.Location.dummy_span))
      ~tvars:[] ~constraints:[]
  in
  let reg = load_instance decl empty_registry in
  Alcotest.(check int) "one instance" 1 (List.length (all_instances reg));
  let inst = List.hd (all_instances reg) in
  Alcotest.(check string) "class name" "Eq" inst.inst_class;
  Alcotest.(check string) "type" "Int" (to_string inst.inst_type)

let test_load_parameterized_instance () =
  let open Sig.Sig_ast in
  let decl =
    make_instance_decl ~class_name:"Eq"
      ~type_sexp:
        (STApp
           ( "List",
             [ STVar ("a", Syntax.Location.dummy_span) ],
             Syntax.Location.dummy_span ))
      ~tvars:[ "a" ]
      ~constraints:[ ("Eq", STVar ("a", Syntax.Location.dummy_span)) ]
  in
  let reg = load_instance decl empty_registry in
  let inst = List.hd (all_instances reg) in
  Alcotest.(check string) "class name" "Eq" inst.inst_class;
  Alcotest.(check (list string)) "tvars" [ "a" ] inst.inst_tvars;
  Alcotest.(check int) "one constraint" 1 (List.length inst.inst_constraints)

(** {1 Superclass Constraint Tests (R6)} *)

(** Create a class with superclass constraints *)
let make_class name tvar superclasses =
  { cls_name = name; cls_tvar = tvar; cls_superclasses = superclasses }

(** R6: Superclass constraint satisfied *)
let test_superclass_constraint_satisfied () =
  (* (class (Ord a) (Eq a) ...) requires Eq
     If we have (Eq int) and (Ord int), resolving (Ord int) should succeed *)
  let ord_class = make_class "Ord" "a" [ "Eq" ] in
  let eq_int = simple_instance "Eq" "Int" in
  let ord_int = simple_instance "Ord" "Int" in
  let reg =
    empty_registry |> add_instance eq_int |> add_instance ord_int
    |> add_class ord_class
  in
  match resolve_all reg [ ("Ord", TCon "Int") ] with
  | Ok () -> ()
  | Error (cls, ty) ->
      Alcotest.fail
        (Printf.sprintf "Expected Ok, got Error (%s %s)" cls (to_string ty))

(** R6: Superclass constraint missing *)
let test_superclass_constraint_missing () =
  (* (class (Ord a) (Eq a) ...) requires Eq
     If we have only (Ord int) but no (Eq int), resolving (Ord int) should fail *)
  let ord_class = make_class "Ord" "a" [ "Eq" ] in
  let ord_int = simple_instance "Ord" "Int" in
  let reg = empty_registry |> add_instance ord_int |> add_class ord_class in
  match resolve_all reg [ ("Ord", TCon "Int") ] with
  | Error (cls, ty) ->
      Alcotest.(check string) "missing superclass" "Eq" cls;
      Alcotest.(check string) "for type" "Int" (to_string ty)
  | Ok () -> Alcotest.fail "Expected Error for missing superclass (Eq Int)"

(** R6: Multiple superclass constraints *)
let test_multiple_superclasses () =
  (* (class (Num a) (Eq a) (Ord a) ...) requires both Eq and Ord *)
  let num_class = make_class "Num" "a" [ "Eq"; "Ord" ] in
  let ord_class = make_class "Ord" "a" [ "Eq" ] in
  (* We need: Eq int, Ord int, Num int *)
  let eq_int = simple_instance "Eq" "Int" in
  let ord_int = simple_instance "Ord" "Int" in
  let num_int = simple_instance "Num" "Int" in
  let reg =
    empty_registry |> add_instance eq_int |> add_instance ord_int
    |> add_instance num_int |> add_class num_class |> add_class ord_class
  in
  match resolve_all reg [ ("Num", TCon "Int") ] with
  | Ok () -> ()
  | Error (cls, ty) ->
      Alcotest.fail
        (Printf.sprintf "Expected Ok, got Error (%s %s)" cls (to_string ty))

(** R6: Transitive superclass constraints *)
let test_transitive_superclasses () =
  (* Num requires Ord, Ord requires Eq
     So Num transitively requires Eq int via Ord int *)
  let ord_class = make_class "Ord" "a" [ "Eq" ] in
  let num_class = make_class "Num" "a" [ "Ord" ] in
  let eq_int = simple_instance "Eq" "Int" in
  let ord_int = simple_instance "Ord" "Int" in
  let num_int = simple_instance "Num" "Int" in
  let reg =
    empty_registry |> add_instance eq_int |> add_instance ord_int
    |> add_instance num_int |> add_class ord_class |> add_class num_class
  in
  match resolve_all reg [ ("Num", TCon "Int") ] with
  | Ok () -> ()
  | Error (cls, ty) ->
      Alcotest.fail
        (Printf.sprintf "Expected Ok, got Error (%s %s)" cls (to_string ty))

(** R6: Transitive superclass missing deep in chain *)
let test_transitive_superclass_missing () =
  (* Num requires Ord, Ord requires Eq
     Missing Eq int should fail when resolving Num int *)
  let ord_class = make_class "Ord" "a" [ "Eq" ] in
  let num_class = make_class "Num" "a" [ "Ord" ] in
  let ord_int = simple_instance "Ord" "Int" in
  let num_int = simple_instance "Num" "Int" in
  let reg =
    empty_registry |> add_instance ord_int |> add_instance num_int
    |> add_class ord_class |> add_class num_class
  in
  match resolve_all reg [ ("Num", TCon "Int") ] with
  | Error (cls, _ty) ->
      (* Should fail because Eq int is missing (required by Ord) *)
      Alcotest.(check string) "missing is Eq" "Eq" cls
  | Ok () -> Alcotest.fail "Expected Error for missing transitive superclass"

(** {1 HKT Class Tests (R7)} *)

(** Create a simple HKT instance (type constructor as instance head) *)
let hkt_instance class_name type_con =
  {
    inst_class = class_name;
    inst_type = TCon type_con;
    inst_tvars = [];
    inst_constraints = [];
  }

(** R7: HKT instance resolution for type constructors *)
let test_hkt_instance_resolution () =
  (* (instance (Functor list) ...) - the instance head is a type constructor *)
  let functor_list = hkt_instance "Functor" "List" in
  let reg = empty_registry |> add_instance functor_list in
  match resolve_all reg [ ("Functor", TCon "List") ] with
  | Ok () -> ()
  | Error (cls, ty) ->
      Alcotest.fail
        (Printf.sprintf "Expected Ok, got Error (%s %s)" cls (to_string ty))

(** R7: HKT instance not found for different constructor *)
let test_hkt_instance_not_found () =
  (* (instance (Functor list)) but resolving (Functor option) should fail *)
  let functor_list = hkt_instance "Functor" "List" in
  let reg = empty_registry |> add_instance functor_list in
  match resolve_all reg [ ("Functor", TCon "Option") ] with
  | Error (cls, ty) ->
      Alcotest.(check string) "class" "Functor" cls;
      Alcotest.(check string) "type" "Option" (to_string ty)
  | Ok () -> Alcotest.fail "Expected Error for missing (Functor Option)"

(** R7: Multiple HKT instances for same class *)
let test_multiple_hkt_instances () =
  (* (instance (Functor list)) and (instance (Functor option)) *)
  let functor_list = hkt_instance "Functor" "List" in
  let functor_option = hkt_instance "Functor" "Option" in
  let reg =
    empty_registry |> add_instance functor_list |> add_instance functor_option
  in
  (* Both should resolve *)
  match
    resolve_all reg [ ("Functor", TCon "List"); ("Functor", TCon "Option") ]
  with
  | Ok () -> ()
  | Error (cls, ty) ->
      Alcotest.fail
        (Printf.sprintf "Expected Ok, got Error (%s %s)" cls (to_string ty))

(** {1 Overlap Detection Tests (R9)} *)

(** R9: No overlap between different classes *)
let test_no_overlap_different_classes () =
  let eq_int = simple_instance "Eq" "Int" in
  let ord_int = simple_instance "Ord" "Int" in
  Alcotest.(check bool)
    "different classes don't overlap" false
    (instances_overlap eq_int ord_int)

(** R9: No overlap between different concrete types *)
let test_no_overlap_different_types () =
  let eq_int = simple_instance "Eq" "Int" in
  let eq_string = simple_instance "Eq" "String" in
  Alcotest.(check bool)
    "different types don't overlap" false
    (instances_overlap eq_int eq_string)

(** R9: Overlap between specific and parameterized instance *)
let test_overlap_specific_and_parameterized () =
  (* (instance (Eq (list int))) and (instance [a] (Eq (list a)))
     overlap because the parameterized instance can match (list int) *)
  let eq_list_int =
    {
      inst_class = "Eq";
      inst_type = TApp (TCon "List", [ TCon "Int" ]);
      inst_tvars = [];
      inst_constraints = [];
    }
  in
  let eq_list_a =
    {
      inst_class = "Eq";
      inst_type = TApp (TCon "List", [ TCon "a" ]);
      inst_tvars = [ "a" ];
      inst_constraints = [ ("Eq", TCon "a") ];
    }
  in
  Alcotest.(check bool)
    "specific and parameterized overlap" true
    (instances_overlap eq_list_int eq_list_a)

(** R9: No overlap between different type constructors *)
let test_no_overlap_different_constructors () =
  let eq_list_a = parameterized_instance "Eq" "List" "a" "Eq" in
  let eq_option_a =
    {
      inst_class = "Eq";
      inst_type = TApp (TCon "Option", [ TCon "a" ]);
      inst_tvars = [ "a" ];
      inst_constraints = [ ("Eq", TCon "a") ];
    }
  in
  Alcotest.(check bool)
    "different constructors don't overlap" false
    (instances_overlap eq_list_a eq_option_a)

(** R9: find_overlaps returns overlapping pairs *)
let test_find_overlaps () =
  let eq_list_int =
    {
      inst_class = "Eq";
      inst_type = TApp (TCon "List", [ TCon "Int" ]);
      inst_tvars = [];
      inst_constraints = [];
    }
  in
  let eq_list_a = parameterized_instance "Eq" "List" "a" "Eq" in
  let eq_string = simple_instance "Eq" "String" in
  let reg =
    empty_registry |> add_instance eq_list_int |> add_instance eq_list_a
    |> add_instance eq_string
  in
  let overlaps = find_overlaps reg in
  Alcotest.(check int) "one overlap found" 1 (List.length overlaps);
  let overlap = List.hd overlaps in
  Alcotest.(check string) "overlap class" "Eq" overlap.overlap_class

(** R9: find_overlaps returns empty for no overlaps *)
let test_find_overlaps_none () =
  let eq_int = simple_instance "Eq" "Int" in
  let eq_string = simple_instance "Eq" "String" in
  let ord_int = simple_instance "Ord" "Int" in
  let reg =
    empty_registry |> add_instance eq_int |> add_instance eq_string
    |> add_instance ord_int
  in
  let overlaps = find_overlaps reg in
  Alcotest.(check int) "no overlaps" 0 (List.length overlaps)

(** {1 Test Runner} *)

let () =
  Alcotest.run "instance"
    [
      ( "registry",
        [
          Alcotest.test_case "empty registry" `Quick test_empty_registry;
          Alcotest.test_case "add instance" `Quick test_add_instance;
          Alcotest.test_case "instances for class" `Quick
            test_instances_for_class;
        ] );
      ( "simple-resolution",
        [
          Alcotest.test_case "found" `Quick test_resolve_simple_found;
          Alcotest.test_case "not found" `Quick test_resolve_simple_not_found;
          Alcotest.test_case "wrong class" `Quick test_resolve_wrong_class;
        ] );
      ( "parameterized-resolution",
        [
          Alcotest.test_case "returns recursive" `Quick
            test_resolve_parameterized_returns_recursive;
        ] );
      ( "full-resolution",
        [
          Alcotest.test_case "all simple" `Quick test_resolve_all_simple;
          Alcotest.test_case "missing" `Quick test_resolve_all_missing;
          Alcotest.test_case "recursive success" `Quick
            test_resolve_all_recursive;
          Alcotest.test_case "recursive missing" `Quick
            test_resolve_all_recursive_missing;
        ] );
      ( "superclass-constraints",
        [
          Alcotest.test_case "satisfied" `Quick
            test_superclass_constraint_satisfied;
          Alcotest.test_case "missing" `Quick test_superclass_constraint_missing;
          Alcotest.test_case "multiple superclasses" `Quick
            test_multiple_superclasses;
          Alcotest.test_case "transitive" `Quick test_transitive_superclasses;
          Alcotest.test_case "transitive missing" `Quick
            test_transitive_superclass_missing;
        ] );
      ( "hkt-instances",
        [
          Alcotest.test_case "type constructor instance" `Quick
            test_hkt_instance_resolution;
          Alcotest.test_case "not found different constructor" `Quick
            test_hkt_instance_not_found;
          Alcotest.test_case "multiple HKT instances" `Quick
            test_multiple_hkt_instances;
        ] );
      ( "overlap-detection",
        [
          Alcotest.test_case "different classes no overlap" `Quick
            test_no_overlap_different_classes;
          Alcotest.test_case "different types no overlap" `Quick
            test_no_overlap_different_types;
          Alcotest.test_case "specific and parameterized overlap" `Quick
            test_overlap_specific_and_parameterized;
          Alcotest.test_case "different constructors no overlap" `Quick
            test_no_overlap_different_constructors;
          Alcotest.test_case "find overlaps" `Quick test_find_overlaps;
          Alcotest.test_case "find overlaps none" `Quick test_find_overlaps_none;
        ] );
      ( "load-instance",
        [
          Alcotest.test_case "simple" `Quick test_load_simple_instance;
          Alcotest.test_case "parameterized" `Quick
            test_load_parameterized_instance;
        ] );
    ]
