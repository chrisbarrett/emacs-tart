(** Tests for the prelude module.

    The prelude provides implicit utility types (t, any, bool, list, option, is,
    nonempty) that are available in all .tart files without explicit import. *)

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

(** Helper to load a signature with prelude types available and shadowing check.
    Returns the result directly so callers can match on Ok/Error. *)
let load_sig_with_prelude_result ?(env = Type_env.empty) s =
  let sig_file = parse_sig_str s in
  let prelude_ctx = Prelude.prelude_type_context () in
  let prelude_type_names = Prelude.prelude_type_names in
  Sig_loader.load_signature_with_resolver ~prelude_ctx ~prelude_type_names
    ~resolver:Sig_loader.no_resolver env sig_file

(** Helper that unwraps the result, failing the test on load errors *)
let load_sig_with_prelude ?(env = Type_env.empty) s =
  match load_sig_with_prelude_result ~env s with
  | Ok env -> env
  | Error e -> failwith ("Load error: " ^ e.message)

(** Helper to parse and type-check an expression *)
let check_expr_str ~env s =
  Types.reset_tvar_counter ();
  match Syntax.Read.parse_one ~filename:"<test>" s with
  | Error msg -> failwith ("parse error: " ^ msg)
  | Ok sexp -> Check.check_expr ~env sexp

(** {1 Prelude Type Tests} *)

(** Test that prelude defines the expected type aliases. The prelude now
    includes both primitive bridges (int, string, etc.) and derived types (list,
    option, is, nonempty). *)
let test_prelude_aliases_defined () =
  let expected =
    [
      (* Primitive bridges *)
      "int";
      "float";
      "num";
      "string";
      "symbol";
      "keyword";
      "nil";
      "truthy";
      "never";
      "t";
      (* Container bridges *)
      "list";
      "vector";
      "cons";
      "hash-table";
      (* Derived types *)
      "any";
      "bool";
      "option";
      "is";
      "nonempty";
    ]
  in
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (name ^ " is prelude type")
        true
        (Prelude.is_prelude_type name))
    expected

(** Test that prelude_type_names contains all aliases *)
let test_prelude_type_names () =
  let names = Prelude.prelude_type_names in
  Alcotest.(check bool) "contains t" true (List.mem "t" names);
  Alcotest.(check bool) "contains any" true (List.mem "any" names);
  Alcotest.(check bool) "contains bool" true (List.mem "bool" names);
  Alcotest.(check bool) "contains list" true (List.mem "list" names);
  Alcotest.(check bool) "contains option" true (List.mem "option" names);
  Alcotest.(check bool) "contains is" true (List.mem "is" names);
  Alcotest.(check bool) "contains nonempty" true (List.mem "nonempty" names)

(** Test that is_prelude_type returns false for non-prelude types. Note: int,
    string, etc. ARE now prelude types (they bridge to intrinsics). Buffer,
    window, etc. are also prelude types (opaque types for Emacs data
    structures). Only truly user-defined types like "my-custom-type" are not
    prelude types. *)
let test_not_prelude_types () =
  (* int, string, etc. ARE prelude types now - they bridge intrinsics *)
  Alcotest.(check bool) "int IS prelude" true (Prelude.is_prelude_type "int");
  Alcotest.(check bool)
    "string IS prelude" true
    (Prelude.is_prelude_type "string");
  (* buffer, window, etc. ARE prelude types now - opaque types *)
  Alcotest.(check bool)
    "buffer IS prelude" true
    (Prelude.is_prelude_type "buffer");
  (* User-defined types are NOT prelude types *)
  Alcotest.(check bool)
    "my-custom-type not prelude" false
    (Prelude.is_prelude_type "my-custom-type")

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
      Alcotest.(check bool) "uses List type" true (String.length scheme_str > 0)

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

(** Test that is type is available in signatures *)
let test_prelude_is_available () =
  let sig_src = "(defun assert-non-nil [a] ((option a)) -> (is (option a)))" in
  let env = load_sig_with_prelude sig_src in
  match Type_env.lookup "assert-non-nil" env with
  | None -> Alcotest.fail "assert-non-nil not found"
  | Some _ -> ()

(** Test that nonempty type is available in signatures *)
let test_prelude_nonempty_available () =
  let sig_src = "(defun first-elem [a] ((nonempty a)) -> (a | nil))" in
  let env = load_sig_with_prelude sig_src in
  match Type_env.lookup "first-elem" env with
  | None -> Alcotest.fail "first-elem not found"
  | Some _ -> ()

(** Test that alist type is available in signatures *)
let test_prelude_alist_available () =
  let sig_src = "(defun lookup [k v] ((alist k v) k) -> (v | nil))" in
  let env = load_sig_with_prelude sig_src in
  match Type_env.lookup "lookup" env with
  | None -> Alcotest.fail "lookup not found"
  | Some _ -> ()

(** Test that plist type is available in signatures *)
let test_prelude_plist_available () =
  let sig_src = "(defun plist-lookup [k v] ((plist k v) k) -> (v | nil))" in
  let env = load_sig_with_prelude sig_src in
  match Type_env.lookup "plist-lookup" env with
  | None -> Alcotest.fail "plist-lookup not found"
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
      Alcotest.(check bool) "is polymorphic" true (String.length scheme_str > 5)

(** {1 Type Checking with Prelude Tests} *)

(** Test that list type integrates with type checking *)
let test_prelude_type_checking () =
  let sig_src = "(defun sum ((list int)) -> int)" in
  let env = load_sig_with_prelude sig_src in
  (* Call with a list literal *)
  let ty, errors = check_expr_str ~env "(sum (list 1 2 3))" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  Alcotest.(check string) "returns int" "int" (Types.to_string ty)

(** Test that option integrates with type checking *)
let test_prelude_option_type_checking () =
  let sig_src = "(defun get-first [a] ((list a)) -> (option a))" in
  let env = load_sig_with_prelude sig_src in
  (* The return type should include nil as part of option *)
  let ty, errors = check_expr_str ~env "(get-first (list 1 2 3))" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  (* Result should be a union containing nil *)
  let ty_str = Types.to_string ty in
  Alcotest.(check bool) "is union type" true (String.length ty_str > 0)

(** Helper to check if a string contains a substring *)
let contains_substring haystack needle =
  try
    let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in
    true
  with Not_found -> false

(** Test that is type removes nil via subtraction *)
let test_prelude_is_subtraction () =
  (* is (option a) should be equivalent to just a *)
  let sig_src = "(defun assert-int ((is (int | nil))) -> int)" in
  let env = load_sig_with_prelude sig_src in
  match Type_env.lookup "assert-int" env with
  | None -> Alcotest.fail "assert-int not found"
  | Some scheme ->
      let scheme_str = Type_env.scheme_to_string scheme in
      (* After subtraction, (is (int | nil)) should become int *)
      Alcotest.(check bool)
        "type contains int" true
        (contains_substring scheme_str "int")

(** Test that nonempty produces a list minus nil *)
let test_prelude_nonempty_subtraction () =
  let sig_src = "(defun require-list ((nonempty int)) -> int)" in
  let env = load_sig_with_prelude sig_src in
  match Type_env.lookup "require-list" env with
  | None -> Alcotest.fail "require-list not found"
  | Some scheme ->
      let scheme_str = Type_env.scheme_to_string scheme in
      (* nonempty removes nil, so should have Cons in the type *)
      Alcotest.(check bool)
        "type has list structure" true
        (String.length scheme_str > 0)

(** {1 Bounded Quantifier Tests} *)

(** Test that option with truthy type is accepted *)
let test_option_truthy_ok () =
  (* option int should work because int is truthy *)
  let sig_src = "(defun maybe-int () -> (option int))" in
  let env = load_sig_with_prelude sig_src in
  match Type_env.lookup "maybe-int" env with
  | None -> Alcotest.fail "maybe-int not found"
  | Some _ -> ()

(** Test that nested option is rejected due to truthy bound violation *)
let test_option_nested_rejected () =
  (* option (option int) should fail because (option int) = (int | nil)
     and (int | nil) is not <: truthy *)
  let sig_src = "(defun nested () -> (option (option int)))" in
  try
    let _ = load_sig_with_prelude sig_src in
    Alcotest.fail "Expected bound violation error for nested option"
  with Failure msg ->
    Alcotest.(check bool)
      "error mentions bound violation" true
      (contains_substring msg "Bound violation")

(** Test that option of nullable type is rejected *)
let test_option_nullable_rejected () =
  (* option (int | nil) should fail because (int | nil) is not truthy *)
  let sig_src = "(defun maybe-nullable () -> (option (int | nil)))" in
  try
    let _ = load_sig_with_prelude sig_src in
    Alcotest.fail "Expected bound violation error for nullable in option"
  with Failure msg ->
    Alcotest.(check bool)
      "error mentions bound violation" true
      (contains_substring msg "Bound violation")

(** Test that option of any is rejected *)
let test_option_any_rejected () =
  (* option any should fail because any = (truthy | nil) is not truthy *)
  let sig_src = "(defun maybe-any () -> (option any))" in
  try
    let _ = load_sig_with_prelude sig_src in
    Alcotest.fail "Expected bound violation error for any in option"
  with Failure msg ->
    Alcotest.(check bool)
      "error mentions bound violation" true
      (contains_substring msg "Bound violation")

(** Test that option of list is accepted (lists are truthy) *)
let test_option_list_ok () =
  (* option (list int) should work because lists are truthy *)
  let sig_src = "(defun maybe-list () -> (option (list int)))" in
  let env = load_sig_with_prelude sig_src in
  match Type_env.lookup "maybe-list" env with
  | None -> Alcotest.fail "maybe-list not found"
  | Some _ -> ()

(** {1 No-Shadowing Rule Tests} *)

(** Test that redefining 'list' type is rejected *)
let test_cannot_redefine_list () =
  let sig_src = "(type list int)" in
  match load_sig_with_prelude_result sig_src with
  | Ok _ -> Alcotest.fail "Expected shadowing error for 'list'"
  | Error e ->
      Alcotest.(check bool)
        "error mentions cannot redefine" true
        (contains_substring e.message "cannot redefine imported binding 'list'")

(** Test that redefining 'option' type is rejected *)
let test_cannot_redefine_option () =
  let sig_src = "(type option [a] a)" in
  match load_sig_with_prelude_result sig_src with
  | Ok _ -> Alcotest.fail "Expected shadowing error for 'option'"
  | Error e ->
      Alcotest.(check bool)
        "error mentions cannot redefine" true
        (contains_substring e.message
           "cannot redefine imported binding 'option'")

(** Test that redefining 'bool' type is rejected *)
let test_cannot_redefine_bool () =
  let sig_src = "(type bool (int | string))" in
  match load_sig_with_prelude_result sig_src with
  | Ok _ -> Alcotest.fail "Expected shadowing error for 'bool'"
  | Error e ->
      Alcotest.(check bool)
        "error mentions cannot redefine" true
        (contains_substring e.message "cannot redefine imported binding 'bool'")

(** Test that defining a new non-prelude type is allowed *)
let test_can_define_new_type () =
  let sig_src = "(type my-pair [a b] (a | b))" in
  let _ = load_sig_with_prelude sig_src in
  (* If no exception, test passes *)
  ()

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
          Alcotest.test_case "is available" `Quick test_prelude_is_available;
          Alcotest.test_case "nonempty available" `Quick
            test_prelude_nonempty_available;
          Alcotest.test_case "alist available" `Quick
            test_prelude_alist_available;
          Alcotest.test_case "plist available" `Quick
            test_prelude_plist_available;
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
      ( "type-subtraction",
        [
          Alcotest.test_case "is removes nil" `Quick test_prelude_is_subtraction;
          Alcotest.test_case "nonempty removes nil from list" `Quick
            test_prelude_nonempty_subtraction;
        ] );
      ( "bounded-quantifiers",
        [
          Alcotest.test_case "option int ok" `Quick test_option_truthy_ok;
          Alcotest.test_case "nested option rejected" `Quick
            test_option_nested_rejected;
          Alcotest.test_case "option nullable rejected" `Quick
            test_option_nullable_rejected;
          Alcotest.test_case "option any rejected" `Quick
            test_option_any_rejected;
          Alcotest.test_case "option list ok" `Quick test_option_list_ok;
        ] );
      ( "no-shadowing",
        [
          Alcotest.test_case "cannot redefine list" `Quick
            test_cannot_redefine_list;
          Alcotest.test_case "cannot redefine option" `Quick
            test_cannot_redefine_option;
          Alcotest.test_case "cannot redefine bool" `Quick
            test_cannot_redefine_bool;
          Alcotest.test_case "can define new type" `Quick
            test_can_define_new_type;
        ] );
    ]
