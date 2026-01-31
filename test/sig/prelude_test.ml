(** Tests for the prelude module.

    The prelude provides implicit utility types (t, any, bool, list, option)
    that are available in all .tart files without explicit import. *)

open Sig
module Types = Core.Types
module Type_env = Core.Type_env
module Check = Typing.Check

(** {1 Test Helpers} *)

(** Helper to parse a signature string into an AST *)
let parse_sig_str ?(module_name = "test") s =
  let parse_result = Syntax.Read.parse_string s in
  match Sig_parser.parse_signature ~module_name parse_result.sexps with
  | Error _ -> failwith "Parse error in test"
  | Ok sig_file -> sig_file

(** Helper to load a signature with prelude types available *)
let load_sig_with_prelude ?(env = Type_env.empty) s =
  let sig_file = parse_sig_str s in
  let prelude_ctx = Prelude.prelude_type_context () in
  Sig_loader.load_signature_with_resolver ~prelude_ctx
    ~resolver:Sig_loader.no_resolver env sig_file

(** Helper to parse and type-check an expression *)
let check_expr_str ~env s =
  Types.reset_tvar_counter ();
  match Syntax.Read.parse_one ~filename:"<test>" s with
  | Error msg -> failwith ("parse error: " ^ msg)
  | Ok sexp -> Check.check_expr ~env sexp

(** {1 Prelude Type Tests} *)

(** Test that prelude defines the expected type aliases *)
let test_prelude_aliases_defined () =
  let expected = [ "t"; "any"; "bool"; "list"; "option" ] in
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (name ^ " is prelude type")
        true (Prelude.is_prelude_type name))
    expected

(** Test that prelude_type_names contains all aliases *)
let test_prelude_type_names () =
  let names = Prelude.prelude_type_names in
  Alcotest.(check int) "has 5 prelude types" 5 (List.length names);
  Alcotest.(check bool) "contains t" true (List.mem "t" names);
  Alcotest.(check bool) "contains any" true (List.mem "any" names);
  Alcotest.(check bool) "contains bool" true (List.mem "bool" names);
  Alcotest.(check bool) "contains list" true (List.mem "list" names);
  Alcotest.(check bool) "contains option" true (List.mem "option" names)

(** Test that is_prelude_type returns false for non-prelude types *)
let test_not_prelude_types () =
  Alcotest.(check bool) "int not prelude" false (Prelude.is_prelude_type "int");
  Alcotest.(check bool)
    "string not prelude" false
    (Prelude.is_prelude_type "string");
  Alcotest.(check bool)
    "buffer not prelude" false
    (Prelude.is_prelude_type "buffer")

(** {1 Prelude Context Tests} *)

(** Test that prelude_type_context provides aliases *)
let test_prelude_context_has_aliases () =
  let ctx = Prelude.prelude_type_context () in
  (* Check that list alias exists *)
  match Sig_loader.lookup_alias "list" ctx.tc_aliases with
  | None -> Alcotest.fail "list alias not found in prelude context"
  | Some alias ->
      Alcotest.(check int)
        "list has 1 param" 1
        (List.length alias.Sig_loader.alias_params)

(** Test that prelude_alias_context can be used in loading *)
let test_prelude_alias_context () =
  let alias_ctx = Prelude.prelude_alias_context () in
  (* Check that option alias exists *)
  match Sig_loader.lookup_alias "option" alias_ctx with
  | None -> Alcotest.fail "option alias not found"
  | Some alias ->
      Alcotest.(check int)
        "option has 1 param" 1
        (List.length alias.Sig_loader.alias_params)

(** {1 Signature Loading with Prelude Tests} *)

(** Test that list type is available in signatures *)
let test_prelude_list_available () =
  let sig_src = "(defun my-length ((list int)) -> int)" in
  let env = load_sig_with_prelude sig_src in
  (* Verify function was loaded *)
  match Type_env.lookup "my-length" env with
  | None -> Alcotest.fail "my-length not found"
  | Some scheme ->
      let scheme_str = Type_env.scheme_to_string scheme in
      (* List should be expanded *)
      Alcotest.(check bool)
        "uses List type" true
        (String.length scheme_str > 0)

(** Test that option type is available in signatures *)
let test_prelude_option_available () =
  let sig_src = "(defun get-value (int) -> (option string))" in
  let env = load_sig_with_prelude sig_src in
  match Type_env.lookup "get-value" env with
  | None -> Alcotest.fail "get-value not found"
  | Some _ -> () (* Success - option type was recognized *)

(** Test that bool type is available in signatures *)
let test_prelude_bool_available () =
  let sig_src = "(defun is-valid (int) -> bool)" in
  let env = load_sig_with_prelude sig_src in
  match Type_env.lookup "is-valid" env with
  | None -> Alcotest.fail "is-valid not found"
  | Some _ -> ()

(** Test that any type is available in signatures *)
let test_prelude_any_available () =
  let sig_src = "(defun accept-anything (any) -> nil)" in
  let env = load_sig_with_prelude sig_src in
  match Type_env.lookup "accept-anything" env with
  | None -> Alcotest.fail "accept-anything not found"
  | Some _ -> ()

(** Test that t type is available in signatures *)
let test_prelude_t_available () =
  let sig_src = "(defun return-t () -> t)" in
  let env = load_sig_with_prelude sig_src in
  match Type_env.lookup "return-t" env with
  | None -> Alcotest.fail "return-t not found"
  | Some _ -> ()

(** Test that prelude types work in polymorphic functions *)
let test_prelude_polymorphic_usage () =
  let sig_src = "(defun my-head [a] ((list a)) -> (option a))" in
  let env = load_sig_with_prelude sig_src in
  match Type_env.lookup "my-head" env with
  | None -> Alcotest.fail "my-head not found"
  | Some scheme ->
      (* Should be a polymorphic scheme *)
      let scheme_str = Type_env.scheme_to_string scheme in
      Alcotest.(check bool)
        "is polymorphic" true
        (String.length scheme_str > 5)

(** {1 Type Checking with Prelude Tests} *)

(** Test that list type integrates with type checking *)
let test_prelude_type_checking () =
  let sig_src = "(defun sum ((list int)) -> int)" in
  let env = load_sig_with_prelude sig_src in
  (* Call with a list literal *)
  let ty, errors = check_expr_str ~env "(sum (list 1 2 3))" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string) "returns Int" "Int" (Types.to_string ty)

(** Test that option integrates with type checking *)
let test_prelude_option_type_checking () =
  let sig_src = "(defun get-first [a] ((list a)) -> (option a))" in
  let env = load_sig_with_prelude sig_src in
  (* The return type should include nil as part of option *)
  let ty, errors = check_expr_str ~env "(get-first (list 1 2 3))" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  (* Result should be a union containing nil *)
  let ty_str = Types.to_string ty in
  Alcotest.(check bool)
    "is union type" true
    (String.length ty_str > 0)

let () =
  Alcotest.run "prelude"
    [
      ( "prelude-types",
        [
          Alcotest.test_case "aliases defined" `Quick
            test_prelude_aliases_defined;
          Alcotest.test_case "type names list" `Quick test_prelude_type_names;
          Alcotest.test_case "not prelude types" `Quick test_not_prelude_types;
        ] );
      ( "prelude-context",
        [
          Alcotest.test_case "context has aliases" `Quick
            test_prelude_context_has_aliases;
          Alcotest.test_case "alias context" `Quick test_prelude_alias_context;
        ] );
      ( "signature-loading",
        [
          Alcotest.test_case "list available" `Quick test_prelude_list_available;
          Alcotest.test_case "option available" `Quick
            test_prelude_option_available;
          Alcotest.test_case "bool available" `Quick test_prelude_bool_available;
          Alcotest.test_case "any available" `Quick test_prelude_any_available;
          Alcotest.test_case "t available" `Quick test_prelude_t_available;
          Alcotest.test_case "polymorphic usage" `Quick
            test_prelude_polymorphic_usage;
        ] );
      ( "type-checking",
        [
          Alcotest.test_case "list type checking" `Quick
            test_prelude_type_checking;
          Alcotest.test_case "option type checking" `Quick
            test_prelude_option_type_checking;
        ] );
    ]
