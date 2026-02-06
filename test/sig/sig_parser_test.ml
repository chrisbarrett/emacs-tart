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
         ( [ Sig_ast.SPPositional (_, Sig_ast.STCon ("int", _)) ],
           Sig_ast.STCon ("string", _),
           _ )) ->
      ()
  | _ -> Alcotest.fail "Expected (int) -> string arrow type");

  (* int -> string (single param without parens) *)
  (match parse_type_str "(int -> string)" with
  | Ok
      (Sig_ast.STArrow
         ( [ Sig_ast.SPPositional (_, Sig_ast.STCon ("int", _)) ],
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

let test_row_types_closed () =
  (* {name string age int} - closed row *)
  match parse_type_str "{name string age int}" with
  | Ok
      (Sig_ast.STRow
         ( {
             srow_fields =
               [
                 ("name", Sig_ast.STCon ("string", _));
                 ("age", Sig_ast.STCon ("int", _));
               ];
             srow_var = None;
           },
           _ )) ->
      ()
  | Ok _ -> Alcotest.fail "Expected closed row with name and age fields"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_row_types_open () =
  (* {name string & r} - open row with row variable *)
  match parse_type_str "{name string & r}" with
  | Ok
      (Sig_ast.STRow
         ( {
             srow_fields = [ ("name", Sig_ast.STCon ("string", _)) ];
             srow_var = Some "r";
           },
           _ )) ->
      ()
  | Ok _ -> Alcotest.fail "Expected open row with name field and row variable r"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_row_types_empty () =
  (* {} - empty row *)
  match parse_type_str "{}" with
  | Ok (Sig_ast.STRow ({ srow_fields = []; srow_var = None }, _)) -> ()
  | Ok _ -> Alcotest.fail "Expected empty closed row"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_row_types_plist () =
  (* {:name string :age int} - plist-style row *)
  match parse_type_str "{:name string :age int}" with
  | Ok
      (Sig_ast.STRow
         ( {
             srow_fields =
               [
                 (":name", Sig_ast.STCon ("string", _));
                 (":age", Sig_ast.STCon ("int", _));
               ];
             srow_var = None;
           },
           _ )) ->
      ()
  | Ok _ -> Alcotest.fail "Expected plist-style closed row"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_row_types_nested () =
  (* {person {name string age int}} - nested row type *)
  match parse_type_str "{person {name string}}" with
  | Ok
      (Sig_ast.STRow
         ( {
             srow_fields =
               [
                 ( "person",
                   Sig_ast.STRow
                     ( {
                         srow_fields = [ ("name", Sig_ast.STCon ("string", _)) ];
                         srow_var = None;
                       },
                       _ ) );
               ];
             srow_var = None;
           },
           _ )) ->
      ()
  | Ok _ -> Alcotest.fail "Expected row with nested row field"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

(** {1 Type Predicate Tests} *)

let test_predicate_type_simple () =
  (* (x is string) - type predicate for string *)
  match parse_type_str "(x is string)" with
  | Ok (Sig_ast.STPredicate ("x", Sig_ast.STCon ("string", _), _)) -> ()
  | Ok _ -> Alcotest.fail "Expected predicate type (x is string)"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_predicate_type_application () =
  (* (x is (list any)) - type predicate with type application *)
  match parse_type_str "(x is (list any))" with
  | Ok (Sig_ast.STPredicate ("x", Sig_ast.STApp ("list", _, _), _)) -> ()
  | Ok _ -> Alcotest.fail "Expected predicate type with list app"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_predicate_in_defun () =
  (* (defun stringp (x) -> (x is string)) *)
  match parse_decl_str "(defun stringp (x) -> (x is string))" with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "stringp";
           defun_clauses =
             [
               {
                 clause_params =
                   [ Sig_ast.SPPositional (_, Sig_ast.STVar ("x", _)) ];
                 clause_return =
                   Sig_ast.STPredicate ("x", Sig_ast.STCon ("string", _), _);
                 _;
               };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected defun with predicate return type"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_named_parameter () =
  (* (defun stringp (((x any))) -> (x is string)) - named param for predicates *)
  match parse_decl_str "(defun stringp (((x any))) -> (x is string))" with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "stringp";
           defun_clauses =
             [
               {
                 clause_params =
                   [ Sig_ast.SPPositional (Some "x", Sig_ast.STCon ("any", _)) ];
                 clause_return =
                   Sig_ast.STPredicate ("x", Sig_ast.STCon ("string", _), _);
                 _;
               };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected defun with named param"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_named_parameter_optional () =
  (* Named param in optional position *)
  match parse_decl_str "(defun f (&optional ((x int))) -> int)" with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "f";
           defun_clauses =
             [
               {
                 clause_params =
                   [ Sig_ast.SPOptional (Some "x", Sig_ast.STCon ("int", _)) ];
                 clause_return = Sig_ast.STCon ("int", _);
                 _;
               };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected defun with named optional param"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_type_app_not_named_param () =
  (* (seq a) should be type application, not named param *)
  match parse_decl_str "(defun f [a] ((seq a)) -> a)" with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "f";
           defun_clauses =
             [
               {
                 clause_params =
                   [ Sig_ast.SPPositional (None, Sig_ast.STApp ("seq", _, _)) ];
                 clause_return = Sig_ast.STVar ("a", _);
                 _;
               };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected defun with type app param (not named)"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

(** {1 Declaration Tests} *)

let test_defun_simple () =
  (* (defun add (int int) -> int) *)
  match parse_decl_str "(defun add (int int) -> int)" with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "add";
           defun_tvar_binders = [];
           defun_clauses =
             [
               {
                 clause_params =
                   [
                     Sig_ast.SPPositional (_, Sig_ast.STCon ("int", _));
                     Sig_ast.SPPositional (_, Sig_ast.STCon ("int", _));
                   ];
                 clause_return = Sig_ast.STCon ("int", _);
                 _;
               };
             ];
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
           defun_clauses =
             [
               {
                 clause_params =
                   [ Sig_ast.SPPositional (_, Sig_ast.STVar ("a", _)) ];
                 clause_return = Sig_ast.STVar ("a", _);
                 _;
               };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected polymorphic defun with correct structure"
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
          Alcotest.test_case "row types closed" `Quick test_row_types_closed;
          Alcotest.test_case "row types open" `Quick test_row_types_open;
          Alcotest.test_case "row types empty" `Quick test_row_types_empty;
          Alcotest.test_case "row types plist" `Quick test_row_types_plist;
          Alcotest.test_case "row types nested" `Quick test_row_types_nested;
        ] );
      ( "predicate-types",
        [
          Alcotest.test_case "simple predicate" `Quick
            test_predicate_type_simple;
          Alcotest.test_case "predicate with app" `Quick
            test_predicate_type_application;
          Alcotest.test_case "predicate in defun" `Quick test_predicate_in_defun;
        ] );
      ( "named-parameters",
        [
          Alcotest.test_case "named param for predicate" `Quick
            test_named_parameter;
          Alcotest.test_case "named optional param" `Quick
            test_named_parameter_optional;
          Alcotest.test_case "type app not confused with named param" `Quick
            test_type_app_not_named_param;
        ] );
      ( "declarations",
        [
          Alcotest.test_case "defun simple" `Quick test_defun_simple;
          Alcotest.test_case "defun polymorphic" `Quick test_defun_polymorphic;
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
      ( "signature-files",
        [ Alcotest.test_case "parse signature" `Quick test_parse_signature ] );
    ]
