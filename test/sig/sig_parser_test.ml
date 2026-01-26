(** Tests for signature file parser *)

open Sig

(** Helper to parse a string as a type expression *)
let parse_type_str s =
  let parse_result = Syntax.Read.parse_string s in
  match parse_result.sexps with
  | [ sexp ] -> Sig_parser.parse_sig_type sexp
  | _ ->
      Error
        {
          Sig_parser.message = "Expected single expression";
          span = Syntax.Location.dummy_span;
        }

(** Helper to parse a string as a declaration *)
let parse_decl_str s =
  let parse_result = Syntax.Read.parse_string s in
  match parse_result.sexps with
  | [ sexp ] -> Sig_parser.parse_decl sexp
  | _ ->
      Error
        {
          Sig_parser.message = "Expected single expression";
          span = Syntax.Location.dummy_span;
        }

(** {1 Type Expression Tests} *)

let test_primitive_types () =
  (* int *)
  (match parse_type_str "int" with
  | Ok (Sig_ast.STCon ("int", _)) -> ()
  | _ -> Alcotest.fail "Expected int type constant");

  (* string *)
  (match parse_type_str "string" with
  | Ok (Sig_ast.STCon ("string", _)) -> ()
  | _ -> Alcotest.fail "Expected string type constant");

  (* nil *)
  (match parse_type_str "nil" with
  | Ok (Sig_ast.STCon ("nil", _)) -> ()
  | _ -> Alcotest.fail "Expected nil type constant");

  (* truthy *)
  match parse_type_str "truthy" with
  | Ok (Sig_ast.STCon ("truthy", _)) -> ()
  | _ -> Alcotest.fail "Expected truthy type constant"

let test_type_variables () =
  (* a - not a primitive, so treated as type variable *)
  (match parse_type_str "a" with
  | Ok (Sig_ast.STVar ("a", _)) -> ()
  | _ -> Alcotest.fail "Expected type variable a");

  (* foo - user type *)
  match parse_type_str "foo" with
  | Ok (Sig_ast.STVar ("foo", _)) -> ()
  | _ -> Alcotest.fail "Expected type variable foo"

let test_type_applications () =
  (* (list int) *)
  (match parse_type_str "(list int)" with
  | Ok (Sig_ast.STApp ("list", [ Sig_ast.STCon ("int", _) ], _)) -> ()
  | _ -> Alcotest.fail "Expected (list int) application");

  (* (option string) *)
  (match parse_type_str "(option string)" with
  | Ok (Sig_ast.STApp ("option", [ Sig_ast.STCon ("string", _) ], _)) -> ()
  | _ -> Alcotest.fail "Expected (option string) application");

  (* (hash-table string int) *)
  match parse_type_str "(hash-table string int)" with
  | Ok
      (Sig_ast.STApp
         ( "hash-table",
           [ Sig_ast.STCon ("string", _); Sig_ast.STCon ("int", _) ],
           _ )) ->
      ()
  | _ -> Alcotest.fail "Expected (hash-table string int) application"

let test_arrow_types () =
  (* (int) -> string *)
  (match parse_type_str "((int) -> string)" with
  | Ok
      (Sig_ast.STArrow
         ( [ Sig_ast.SPPositional (Sig_ast.STCon ("int", _)) ],
           Sig_ast.STCon ("string", _),
           _ )) ->
      ()
  | _ -> Alcotest.fail "Expected (int) -> string arrow type");

  (* int -> string (single param without parens) *)
  (match parse_type_str "(int -> string)" with
  | Ok
      (Sig_ast.STArrow
         ( [ Sig_ast.SPPositional (Sig_ast.STCon ("int", _)) ],
           Sig_ast.STCon ("string", _),
           _ )) ->
      ()
  | _ -> Alcotest.fail "Expected int -> string arrow type");

  (* () -> nil (no params) *)
  match parse_type_str "(() -> nil)" with
  | Ok (Sig_ast.STArrow ([], Sig_ast.STCon ("nil", _), _)) -> ()
  | _ -> Alcotest.fail "Expected () -> nil arrow type"

let test_polymorphic_types () =
  (* [a] (a) -> a *)
  match parse_type_str "([a] (a) -> a)" with
  | Ok
      (Sig_ast.STForall
         ([ { name = "a"; bound = None; _ } ], Sig_ast.STArrow (_, _, _), _)) ->
      ()
  | Ok other ->
      Alcotest.fail
        (Printf.sprintf "Got unexpected type: %s"
           (match other with
           | Sig_ast.STForall _ -> "STForall"
           | Sig_ast.STArrow _ -> "STArrow"
           | _ -> "other"))
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_union_types () =
  (* (int | string) *)
  (match parse_type_str "((int | string))" with
  | Ok
      (Sig_ast.STUnion
         ([ Sig_ast.STCon ("int", _); Sig_ast.STCon ("string", _) ], _)) ->
      ()
  | Ok _ -> Alcotest.fail "Expected union type with int and string"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message));

  (* (int | string | nil) *)
  match parse_type_str "((int | string | nil))" with
  | Ok
      (Sig_ast.STUnion
         ( [
             Sig_ast.STCon ("int", _);
             Sig_ast.STCon ("string", _);
             Sig_ast.STCon ("nil", _);
           ],
           _ )) ->
      ()
  | _ -> Alcotest.fail "Expected union type with three alternatives"

let test_tuple_types () =
  (* (tuple int string) *)
  match parse_type_str "(tuple int string)" with
  | Ok
      (Sig_ast.STTuple
         ([ Sig_ast.STCon ("int", _); Sig_ast.STCon ("string", _) ], _)) ->
      ()
  | _ -> Alcotest.fail "Expected tuple type"

(** {1 Declaration Tests} *)

let test_defun_simple () =
  (* (defun add (int int) -> int) *)
  match parse_decl_str "(defun add (int int) -> int)" with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "add";
           defun_tvar_binders = [];
           defun_constraints = [];
           defun_params =
             [
               Sig_ast.SPPositional (Sig_ast.STCon ("int", _));
               Sig_ast.SPPositional (Sig_ast.STCon ("int", _));
             ];
           defun_return = Sig_ast.STCon ("int", _);
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected defun with correct structure"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_defun_polymorphic () =
  (* (defun identity [a] (a) -> a) *)
  match parse_decl_str "(defun identity [a] (a) -> a)" with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "identity";
           defun_tvar_binders = [ { name = "a"; bound = None; _ } ];
           defun_constraints = [];
           defun_params = [ Sig_ast.SPPositional (Sig_ast.STVar ("a", _)) ];
           defun_return = Sig_ast.STVar ("a", _);
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected polymorphic defun with correct structure"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_defun_with_constraint () =
  (* (defun elem [a] (Eq a) => (a (list a)) -> bool) *)
  match parse_decl_str "(defun elem [a] (Eq a) => (a (list a)) -> bool)" with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "elem";
           defun_tvar_binders = [ { name = "a"; _ } ];
           defun_constraints = [ ("Eq", Sig_ast.STVar ("a", _)) ];
           defun_params =
             [
               Sig_ast.SPPositional (Sig_ast.STVar ("a", _));
               Sig_ast.SPPositional (Sig_ast.STApp ("list", _, _));
             ];
           defun_return = Sig_ast.STCon ("bool", _);
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected defun with constraint"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_defun_with_multiple_constraints () =
  (* (defun sort-unique [a] (Eq a) (Ord a) => ((list a)) -> (list a)) *)
  match
    parse_decl_str
      "(defun sort-unique [a] (Eq a) (Ord a) => ((list a)) -> (list a))"
  with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "sort-unique";
           defun_tvar_binders = [ { name = "a"; _ } ];
           defun_constraints =
             [ ("Eq", Sig_ast.STVar ("a", _)); ("Ord", Sig_ast.STVar ("a", _)) ];
           defun_params =
             [ Sig_ast.SPPositional (Sig_ast.STApp ("list", _, _)) ];
           defun_return = Sig_ast.STApp ("list", _, _);
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected defun with multiple constraints"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_defun_constraint_no_quantifiers () =
  (* (defun member-p (Eq a) => (a (list a)) -> bool) - no explicit [a] *)
  match parse_decl_str "(defun member-p (Eq a) => (a (list a)) -> bool)" with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "member-p";
           defun_tvar_binders = [];
           (* no explicit quantifiers *)
           defun_constraints = [ ("Eq", Sig_ast.STVar ("a", _)) ];
           defun_params =
             [
               Sig_ast.SPPositional (Sig_ast.STVar ("a", _));
               Sig_ast.SPPositional (Sig_ast.STApp ("list", _, _));
             ];
           defun_return = Sig_ast.STCon ("bool", _);
           _;
         }) ->
      ()
  | Ok _ ->
      Alcotest.fail "Expected defun with constraint but no explicit quantifiers"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_defvar () =
  (* (defvar my-default string) *)
  match parse_decl_str "(defvar my-default string)" with
  | Ok
      (Sig_ast.DDefvar
         {
           defvar_name = "my-default";
           defvar_type = Sig_ast.STCon ("string", _);
           _;
         }) ->
      ()
  | _ -> Alcotest.fail "Expected defvar declaration"

let test_type_alias () =
  (* (type int-list (list int)) *)
  match parse_decl_str "(type int-list (list int))" with
  | Ok
      (Sig_ast.DType
         {
           type_name = "int-list";
           type_params = [];
           type_body =
             Some (Sig_ast.STApp ("list", [ Sig_ast.STCon ("int", _) ], _));
           _;
         }) ->
      ()
  | _ -> Alcotest.fail "Expected type alias"

let test_type_opaque () =
  (* (type buffer) *)
  match parse_decl_str "(type buffer)" with
  | Ok
      (Sig_ast.DType
         { type_name = "buffer"; type_params = []; type_body = None; _ }) ->
      ()
  | _ -> Alcotest.fail "Expected opaque type"

let test_type_parameterized () =
  (* (type result [a e] ((ok a) | (err e))) *)
  match parse_decl_str "(type result [a e] ((ok a) | (err e)))" with
  | Ok
      (Sig_ast.DType
         {
           type_name = "result";
           type_params = [ { name = "a"; _ }; { name = "e"; _ } ];
           type_body = Some (Sig_ast.STUnion _);
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected parameterized type with union body"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_open_directive () =
  (* (open 'seq) *)
  match parse_decl_str "(open 'seq)" with
  | Ok (Sig_ast.DOpen ("seq", _)) -> ()
  | _ -> Alcotest.fail "Expected open directive"

let test_include_directive () =
  (* (include 'base) *)
  match parse_decl_str "(include 'base)" with
  | Ok (Sig_ast.DInclude ("base", _)) -> ()
  | _ -> Alcotest.fail "Expected include directive"

(** {1 Data Declaration Tests} *)

let test_data_simple () =
  (* (data result [a e] (Ok a) (Err e)) *)
  match parse_decl_str "(data result [a e] (Ok a) (Err e))" with
  | Ok
      (Sig_ast.DData
         {
           data_name = "result";
           data_params = [ { name = "a"; _ }; { name = "e"; _ } ];
           data_ctors =
             [
               { ctor_name = "Ok"; ctor_fields = [ Sig_ast.STVar ("a", _) ]; _ };
               {
                 ctor_name = "Err";
                 ctor_fields = [ Sig_ast.STVar ("e", _) ];
                 _;
               };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected data declaration with correct structure"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_data_nullary () =
  (* (data bool (True) (False)) *)
  match parse_decl_str "(data bool (True) (False))" with
  | Ok
      (Sig_ast.DData
         {
           data_name = "bool";
           data_params = [];
           data_ctors =
             [
               { ctor_name = "True"; ctor_fields = []; _ };
               { ctor_name = "False"; ctor_fields = []; _ };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected data declaration with nullary constructors"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_data_multi_field () =
  (* (data point (Point2D int int)) *)
  match parse_decl_str "(data point (Point2D int int))" with
  | Ok
      (Sig_ast.DData
         {
           data_name = "point";
           data_params = [];
           data_ctors =
             [
               {
                 ctor_name = "Point2D";
                 ctor_fields =
                   [ Sig_ast.STCon ("int", _); Sig_ast.STCon ("int", _) ];
                 _;
               };
             ];
           _;
         }) ->
      ()
  | Ok _ ->
      Alcotest.fail "Expected data declaration with multi-field constructor"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_data_recursive () =
  (* (data tree [a] (Leaf a) (Node (tree a) (tree a))) *)
  match parse_decl_str "(data tree [a] (Leaf a) (Node (tree a) (tree a)))" with
  | Ok
      (Sig_ast.DData
         {
           data_name = "tree";
           data_params = [ { name = "a"; _ } ];
           data_ctors =
             [
               { ctor_name = "Leaf"; ctor_fields = [ _ ]; _ };
               { ctor_name = "Node"; ctor_fields = [ _; _ ]; _ };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected recursive data declaration"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

(** {1 Class Declaration Tests} *)

let test_class_simple () =
  (* (class (Eq a) (eq (a a) -> bool)) *)
  match parse_decl_str "(class (Eq a) (eq (a a) -> bool))" with
  | Ok
      (Sig_ast.DClass
         {
           class_name = "Eq";
           class_tvar_binder = { name = "a"; bound = None; kind = None; _ };
           class_superclasses = [];
           class_methods =
             [
               {
                 method_name = "eq";
                 method_tvar_binders = [];
                 method_params =
                   [
                     Sig_ast.SPPositional (Sig_ast.STVar ("a", _));
                     Sig_ast.SPPositional (Sig_ast.STVar ("a", _));
                   ];
                 method_return = Sig_ast.STCon ("bool", _);
                 _;
               };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected simple class declaration"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_class_multiple_methods () =
  (* (class (Eq a) (eq (a a) -> bool) (neq (a a) -> bool)) *)
  match
    parse_decl_str "(class (Eq a) (eq (a a) -> bool) (neq (a a) -> bool))"
  with
  | Ok
      (Sig_ast.DClass
         {
           class_name = "Eq";
           class_methods =
             [ { method_name = "eq"; _ }; { method_name = "neq"; _ } ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected class with two methods"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_class_with_superclass () =
  (* (class (Ord a) (Eq a) (compare (a a) -> int)) *)
  match parse_decl_str "(class (Ord a) (Eq a) (compare (a a) -> int))" with
  | Ok
      (Sig_ast.DClass
         {
           class_name = "Ord";
           class_tvar_binder = { name = "a"; _ };
           class_superclasses = [ ("Eq", Sig_ast.STVar ("a", _)) ];
           class_methods = [ { method_name = "compare"; _ } ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected class with superclass"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_class_hkt () =
  (* (class (Functor (f : (* -> *))) (fmap [a b] (((a) -> b) (f a)) -> (f b))) *)
  match
    parse_decl_str
      "(class (Functor (f : (* -> *))) (fmap [a b] (((a) -> b) (f a)) -> (f \
       b)))"
  with
  | Ok
      (Sig_ast.DClass
         {
           class_name = "Functor";
           class_tvar_binder =
             {
               name = "f";
               kind = Some (Sig_ast.SKArrow (Sig_ast.SKStar, Sig_ast.SKStar));
               _;
             };
           class_superclasses = [];
           class_methods =
             [
               {
                 method_name = "fmap";
                 method_tvar_binders = [ { name = "a"; _ }; { name = "b"; _ } ];
                 _;
               };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected HKT class declaration"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_class_multiple_superclasses () =
  (* (class (Num a) (Eq a) (Ord a) (add (a a) -> a)) *)
  match parse_decl_str "(class (Num a) (Eq a) (Ord a) (add (a a) -> a))" with
  | Ok
      (Sig_ast.DClass
         {
           class_name = "Num";
           class_superclasses =
             [ ("Eq", Sig_ast.STVar ("a", _)); ("Ord", Sig_ast.STVar ("a", _)) ];
           class_methods = [ { method_name = "add"; _ } ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected class with multiple superclasses"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_class_no_methods_error () =
  (* (class (Eq a)) - should fail, no methods *)
  match parse_decl_str "(class (Eq a))" with
  | Ok _ -> Alcotest.fail "Expected error for class with no methods"
  | Error e ->
      Alcotest.(check bool)
        "error mentions methods" true
        (String.length e.message > 0)

(** {1 Instance Declaration Tests} *)

let test_instance_simple () =
  (* (instance (Eq int) (eq . =)) *)
  match parse_decl_str "(instance (Eq int) (eq . =))" with
  | Ok
      (Sig_ast.DInstance
         {
           inst_class = "Eq";
           inst_type = Sig_ast.STCon ("int", _);
           inst_tvar_binders = [];
           inst_constraints = [];
           inst_methods = [ { impl_method = "eq"; impl_fn = "="; _ } ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected simple instance declaration"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_instance_multiple_methods () =
  (* (instance (Eq int) (eq . =) (neq . /=)) *)
  match parse_decl_str "(instance (Eq int) (eq . =) (neq . /=))" with
  | Ok
      (Sig_ast.DInstance
         {
           inst_class = "Eq";
           inst_methods =
             [
               { impl_method = "eq"; impl_fn = "="; _ };
               { impl_method = "neq"; impl_fn = "/="; _ };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected instance with two methods"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_instance_parameterized () =
  (* (instance [a] (Eq a) => (Eq (list a)) (eq . list-eq)) *)
  match
    parse_decl_str "(instance [a] (Eq a) => (Eq (list a)) (eq . list-eq))"
  with
  | Ok
      (Sig_ast.DInstance
         {
           inst_class = "Eq";
           inst_type = Sig_ast.STApp ("list", [ Sig_ast.STVar ("a", _) ], _);
           inst_tvar_binders = [ { name = "a"; _ } ];
           inst_constraints = [ ("Eq", Sig_ast.STVar ("a", _)) ];
           inst_methods = [ { impl_method = "eq"; impl_fn = "list-eq"; _ } ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected parameterized instance declaration"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_instance_hkt () =
  (* (instance (Functor list) (fmap . mapcar)) *)
  match parse_decl_str "(instance (Functor list) (fmap . mapcar))" with
  | Ok
      (Sig_ast.DInstance
         {
           inst_class = "Functor";
           inst_type = Sig_ast.STVar ("list", _);
           inst_tvar_binders = [];
           inst_constraints = [];
           inst_methods = [ { impl_method = "fmap"; impl_fn = "mapcar"; _ } ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected HKT instance declaration"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_instance_multiple_constraints () =
  (* (instance [a] (Eq a) (Ord a) => (Sortable (list a)) (sort . list-sort)) *)
  match
    parse_decl_str
      "(instance [a] (Eq a) (Ord a) => (Sortable (list a)) (sort . list-sort))"
  with
  | Ok
      (Sig_ast.DInstance
         {
           inst_class = "Sortable";
           inst_tvar_binders = [ { name = "a"; _ } ];
           inst_constraints =
             [ ("Eq", Sig_ast.STVar ("a", _)); ("Ord", Sig_ast.STVar ("a", _)) ];
           inst_methods = [ { impl_method = "sort"; impl_fn = "list-sort"; _ } ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected instance with multiple constraints"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_instance_no_methods_error () =
  (* (instance (Eq int)) - should fail, no methods *)
  match parse_decl_str "(instance (Eq int))" with
  | Ok _ -> Alcotest.fail "Expected error for instance with no methods"
  | Error e ->
      Alcotest.(check bool)
        "error mentions method" true
        (String.length e.message > 0)

(** {1 Signature File Tests} *)

let test_parse_signature () =
  let src =
    {|
    (type buffer)
    (defun buffer-name (buffer) -> string)
    (defvar default-directory string)
  |}
  in
  let parse_result = Syntax.Read.parse_string src in
  match Sig_parser.parse_signature ~module_name:"test" parse_result.sexps with
  | Ok sig_file ->
      Alcotest.(check string) "module name" "test" sig_file.sig_module;
      Alcotest.(check int) "decl count" 3 (List.length sig_file.sig_decls)
  | Error errors ->
      let msgs = List.map (fun e -> e.Sig_parser.message) errors in
      Alcotest.fail
        (Printf.sprintf "Parse errors: %s" (String.concat ", " msgs))

let () =
  Alcotest.run "sig_parser"
    [
      ( "type-expressions",
        [
          Alcotest.test_case "primitive types" `Quick test_primitive_types;
          Alcotest.test_case "type variables" `Quick test_type_variables;
          Alcotest.test_case "type applications" `Quick test_type_applications;
          Alcotest.test_case "arrow types" `Quick test_arrow_types;
          Alcotest.test_case "polymorphic types" `Quick test_polymorphic_types;
          Alcotest.test_case "union types" `Quick test_union_types;
          Alcotest.test_case "tuple types" `Quick test_tuple_types;
        ] );
      ( "declarations",
        [
          Alcotest.test_case "defun simple" `Quick test_defun_simple;
          Alcotest.test_case "defun polymorphic" `Quick test_defun_polymorphic;
          Alcotest.test_case "defun with constraint" `Quick
            test_defun_with_constraint;
          Alcotest.test_case "defun with multiple constraints" `Quick
            test_defun_with_multiple_constraints;
          Alcotest.test_case "defun constraint no quantifiers" `Quick
            test_defun_constraint_no_quantifiers;
          Alcotest.test_case "defvar" `Quick test_defvar;
          Alcotest.test_case "type alias" `Quick test_type_alias;
          Alcotest.test_case "type opaque" `Quick test_type_opaque;
          Alcotest.test_case "type parameterized" `Quick test_type_parameterized;
          Alcotest.test_case "open directive" `Quick test_open_directive;
          Alcotest.test_case "include directive" `Quick test_include_directive;
          Alcotest.test_case "data simple" `Quick test_data_simple;
          Alcotest.test_case "data nullary" `Quick test_data_nullary;
          Alcotest.test_case "data multi-field" `Quick test_data_multi_field;
          Alcotest.test_case "data recursive" `Quick test_data_recursive;
        ] );
      ( "class-declarations",
        [
          Alcotest.test_case "class simple" `Quick test_class_simple;
          Alcotest.test_case "class multiple methods" `Quick
            test_class_multiple_methods;
          Alcotest.test_case "class with superclass" `Quick
            test_class_with_superclass;
          Alcotest.test_case "class HKT" `Quick test_class_hkt;
          Alcotest.test_case "class multiple superclasses" `Quick
            test_class_multiple_superclasses;
          Alcotest.test_case "class no methods error" `Quick
            test_class_no_methods_error;
        ] );
      ( "instance-declarations",
        [
          Alcotest.test_case "instance simple" `Quick test_instance_simple;
          Alcotest.test_case "instance multiple methods" `Quick
            test_instance_multiple_methods;
          Alcotest.test_case "instance parameterized" `Quick
            test_instance_parameterized;
          Alcotest.test_case "instance HKT" `Quick test_instance_hkt;
          Alcotest.test_case "instance multiple constraints" `Quick
            test_instance_multiple_constraints;
          Alcotest.test_case "instance no methods error" `Quick
            test_instance_no_methods_error;
        ] );
      ( "signature-files",
        [ Alcotest.test_case "parse signature" `Quick test_parse_signature ] );
    ]
