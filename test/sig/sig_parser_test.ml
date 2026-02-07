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

(** {1 Wildcard Type Tests} *)

let test_wildcard_anonymous () =
  (* _ → STInfer (None, _) *)
  match parse_type_str "_" with
  | Ok (Sig_ast.STInfer (None, _)) -> ()
  | Ok _ -> Alcotest.fail "Expected STInfer None"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_wildcard_named () =
  (* _foo → STInfer (Some "_foo", _) *)
  match parse_type_str "_foo" with
  | Ok (Sig_ast.STInfer (Some "_foo", _)) -> ()
  | Ok _ -> Alcotest.fail "Expected STInfer Some _foo"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

(** {1 Multi-Clause Tests} *)

let test_multi_clause_predicate () =
  (* (defun stringp ((string) -> t) ((_) -> nil)) *)
  match parse_decl_str "(defun stringp ((string) -> t) ((_) -> nil))" with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "stringp";
           defun_tvar_binders = [];
           defun_clauses =
             [
               {
                 clause_params =
                   [ Sig_ast.SPPositional (_, Sig_ast.STCon ("string", _)) ];
                 clause_return = Sig_ast.STCon ("t", _);
                 _;
               };
               {
                 clause_params =
                   [ Sig_ast.SPPositional (_, Sig_ast.STInfer (None, _)) ];
                 clause_return = Sig_ast.STCon ("nil", _);
                 _;
               };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected multi-clause predicate defun"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_multi_clause_with_binders () =
  (* (defun car [a b] (((cons a b)) -> a) ((nil) -> nil)) *)
  match
    parse_decl_str "(defun car [a b] (((cons a b)) -> a) ((nil) -> nil))"
  with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "car";
           defun_tvar_binders = [ { name = "a"; _ }; { name = "b"; _ } ];
           defun_clauses = [ _; _ ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected multi-clause defun with binders"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_multi_clause_three () =
  (* (defun sequencep ((list any) -> t) (((vector any)) -> t) ((string) -> t) ((_) -> nil)) *)
  match
    parse_decl_str
      "(defun sequencep (((list any)) -> t) (((vector any)) -> t) ((string) -> \
       t) ((_) -> nil))"
  with
  | Ok
      (Sig_ast.DDefun
         { defun_name = "sequencep"; defun_clauses = [ _; _; _; _ ]; _ }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected 4-clause defun"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_literal_keyword_param () =
  (* Keyword literal in clause: (defun f [k v] (((plist k v) :name) -> string) (((plist k v) k) -> v)) *)
  match
    parse_decl_str
      "(defun f [k v] (((plist k v) :name) -> string) (((plist k v) k) -> v))"
  with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "f";
           defun_clauses =
             [
               {
                 clause_params = [ _; Sig_ast.SPLiteral (":name", _) ];
                 clause_return = Sig_ast.STCon ("string", _);
                 _;
               };
               {
                 clause_params =
                   [ _; Sig_ast.SPPositional (_, Sig_ast.STVar ("k", _)) ];
                 clause_return = Sig_ast.STVar ("v", _);
                 _;
               };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected multi-clause with keyword literal"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_literal_quoted_symbol_param () =
  (* Quoted symbol literal in clause: (defun f [k v] (((alist k v) 'name) -> string) (((alist k v) k) -> v)) *)
  match
    parse_decl_str
      "(defun f [k v] (((alist k v) 'name) -> string) (((alist k v) k) -> v))"
  with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "f";
           defun_clauses =
             [ { clause_params = [ _; Sig_ast.SPLiteral ("name", _) ]; _ }; _ ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected multi-clause with quoted symbol literal"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_type_app_param () =
  (* (seq a) should still be type application in params *)
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
  | Ok _ -> Alcotest.fail "Expected defun with type app param"
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

(** {1 Let Declaration Tests} *)

let test_let_simple () =
  let src =
    {|(let ((type int-list (list int)))
      (defun sum (int-list) -> int))|}
  in
  match parse_decl_str src with
  | Ok (Sig_ast.DLet d) ->
      Alcotest.(check int) "one binding" 1 (List.length d.let_bindings);
      Alcotest.(check int) "one body decl" 1 (List.length d.let_body);
      let binding = List.hd d.let_bindings in
      Alcotest.(check string) "binding name" "int-list" binding.ltb_name;
      Alcotest.(check int) "no params" 0 (List.length binding.ltb_params)
  | Ok _ -> Alcotest.fail "Expected DLet"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_let_parameterized () =
  let src =
    {|(let ((type wrapper [a] (list a)))
      (defun wrap [a] (a) -> (wrapper a)))|}
  in
  match parse_decl_str src with
  | Ok (Sig_ast.DLet d) ->
      let binding = List.hd d.let_bindings in
      Alcotest.(check string) "binding name" "wrapper" binding.ltb_name;
      Alcotest.(check int) "one param" 1 (List.length binding.ltb_params)
  | Ok _ -> Alcotest.fail "Expected DLet"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_let_multiple_bindings () =
  let src =
    {|(let ((type int-list (list int))
           (type str-list (list string)))
      (defun sum (int-list) -> int)
      (defun join (str-list) -> string))|}
  in
  match parse_decl_str src with
  | Ok (Sig_ast.DLet d) ->
      Alcotest.(check int) "two bindings" 2 (List.length d.let_bindings);
      Alcotest.(check int) "two body decls" 2 (List.length d.let_body)
  | Ok _ -> Alcotest.fail "Expected DLet"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_let_error_no_bindings () =
  let src = {|(let)|} in
  match parse_decl_str src with
  | Ok _ -> Alcotest.fail "Expected parse error"
  | Error _ -> ()

let test_let_error_no_body () =
  let src = {|(let ((type int-list (list int))))|} in
  match parse_decl_str src with
  | Ok _ -> Alcotest.fail "Expected parse error"
  | Error _ -> ()

(** {1 Clause Diagnostic Tests} *)

let test_single_clause_warn () =
  (* Single-clause defun with warn diagnostic *)
  match
    parse_decl_str
      {|(defun old-fn (any) -> nil
          (warn "old-fn is deprecated; use new-fn"))|}
  with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "old-fn";
           defun_clauses =
             [
               {
                 clause_return = Sig_ast.STCon ("nil", _);
                 clause_diagnostic =
                   Some
                     {
                       diag_severity = Sig_ast.DiagWarn;
                       diag_message = "old-fn is deprecated; use new-fn";
                       diag_args = [];
                       _;
                     };
                 _;
               };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected single-clause with warn diagnostic"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_multi_clause_warn_fallback () =
  (* Multi-clause with warn on fallback clause *)
  match
    parse_decl_str
      {|(defun plist-member [k v]
          (((plist k v) k) -> (plist k v))
          (((list (k | v)) k) -> (list (k | v))
            (warn "plist-member on bare list")))|}
  with
  | Ok
      (Sig_ast.DDefun
         {
           defun_name = "plist-member";
           defun_clauses =
             [
               { clause_diagnostic = None; _ };
               { clause_diagnostic = Some { diag_severity = DiagWarn; _ }; _ };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected multi-clause with warn on second clause"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_diagnostic_with_args () =
  (* Diagnostic with format args referencing type variables *)
  match
    parse_decl_str
      {|(defun f [k v] ((list (k | v)) k) -> (k | nil)
          (warn "expected (plist %s %s), got bare list" k v))|}
  with
  | Ok
      (Sig_ast.DDefun
         {
           defun_clauses =
             [
               {
                 clause_diagnostic =
                   Some
                     {
                       diag_severity = DiagWarn;
                       diag_message = "expected (plist %s %s), got bare list";
                       diag_args = [ "k"; "v" ];
                       _;
                     };
                 _;
               };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected diagnostic with format args"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_diagnostic_error_severity () =
  (* Error severity diagnostic *)
  match
    parse_decl_str
      {|(defun f (any) -> never
          (error "this function must not be called"))|}
  with
  | Ok
      (Sig_ast.DDefun
         {
           defun_clauses =
             [
               { clause_diagnostic = Some { diag_severity = DiagError; _ }; _ };
             ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected error diagnostic"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_diagnostic_note_severity () =
  (* Note severity diagnostic *)
  match
    parse_decl_str
      {|(defun f (any) -> any
          (note "consider using g instead"))|}
  with
  | Ok
      (Sig_ast.DDefun
         {
           defun_clauses =
             [ { clause_diagnostic = Some { diag_severity = DiagNote; _ }; _ } ];
           _;
         }) ->
      ()
  | Ok _ -> Alcotest.fail "Expected note diagnostic"
  | Error e -> Alcotest.fail (Printf.sprintf "Parse error: %s" e.message)

let test_diagnostic_error_mismatched_args () =
  (* Error: %s count doesn't match argument count *)
  match
    parse_decl_str
      {|(defun f [k] (k) -> any
          (warn "got %s and %s" k))|}
  with
  | Error e ->
      (* Should mention placeholder/argument mismatch *)
      let has_placeholder =
        try
          ignore
            (Str.search_forward (Str.regexp_string "placeholder") e.message 0);
          true
        with Not_found -> false
      in
      if not has_placeholder then
        Alcotest.fail
          (Printf.sprintf "Expected 'placeholder' in error: %s" e.message)
  | Ok _ -> Alcotest.fail "Expected error for mismatched %s count"

let test_diagnostic_error_unbound_tvar () =
  (* Error: argument not a bound type variable *)
  match
    parse_decl_str {|(defun f [k] (k) -> any
          (warn "got %s" z))|}
  with
  | Error e ->
      let has_not_bound =
        try
          ignore
            (Str.search_forward (Str.regexp_string "not a bound") e.message 0);
          true
        with Not_found -> false
      in
      if not has_not_bound then
        Alcotest.fail
          (Printf.sprintf "Expected 'not a bound' in error: %s" e.message)
  | Ok _ -> Alcotest.fail "Expected error for unbound type variable"

let test_no_diagnostic () =
  (* Clause without diagnostic still works *)
  match parse_decl_str {|(defun f (any) -> any)|} with
  | Ok
      (Sig_ast.DDefun { defun_clauses = [ { clause_diagnostic = None; _ } ]; _ })
    ->
      ()
  | Ok _ -> Alcotest.fail "Expected clause without diagnostic"
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
      ( "wildcard-types",
        [
          Alcotest.test_case "anonymous wildcard" `Quick test_wildcard_anonymous;
          Alcotest.test_case "named wildcard" `Quick test_wildcard_named;
        ] );
      ( "multi-clause",
        [
          Alcotest.test_case "predicate multi-clause" `Quick
            test_multi_clause_predicate;
          Alcotest.test_case "multi-clause with binders" `Quick
            test_multi_clause_with_binders;
          Alcotest.test_case "three-clause defun" `Quick test_multi_clause_three;
          Alcotest.test_case "type app in params" `Quick test_type_app_param;
          Alcotest.test_case "literal keyword param" `Quick
            test_literal_keyword_param;
          Alcotest.test_case "literal quoted symbol param" `Quick
            test_literal_quoted_symbol_param;
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
      ( "let-declarations",
        [
          Alcotest.test_case "simple let" `Quick test_let_simple;
          Alcotest.test_case "parameterized let" `Quick test_let_parameterized;
          Alcotest.test_case "multiple bindings" `Quick
            test_let_multiple_bindings;
          Alcotest.test_case "error no bindings" `Quick
            test_let_error_no_bindings;
          Alcotest.test_case "error no body" `Quick test_let_error_no_body;
        ] );
      ( "clause-diagnostics",
        [
          Alcotest.test_case "single-clause warn" `Quick test_single_clause_warn;
          Alcotest.test_case "multi-clause warn fallback" `Quick
            test_multi_clause_warn_fallback;
          Alcotest.test_case "diagnostic with args" `Quick
            test_diagnostic_with_args;
          Alcotest.test_case "error severity" `Quick
            test_diagnostic_error_severity;
          Alcotest.test_case "note severity" `Quick
            test_diagnostic_note_severity;
          Alcotest.test_case "mismatched args error" `Quick
            test_diagnostic_error_mismatched_args;
          Alcotest.test_case "unbound tvar error" `Quick
            test_diagnostic_error_unbound_tvar;
          Alcotest.test_case "no diagnostic" `Quick test_no_diagnostic;
        ] );
      ( "signature-files",
        [ Alcotest.test_case "parse signature" `Quick test_parse_signature ] );
    ]
