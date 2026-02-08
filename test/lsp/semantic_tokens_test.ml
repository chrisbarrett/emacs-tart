(** Unit tests for the Semantic_tokens module. *)

open Lsp

(** {1 Helpers} *)

let parse (src : string) : Syntax.Sexp.t list =
  (Syntax.Read.parse_string ~filename:"test.el" src).sexps

(** Find all tokens with a given type. *)
let find_tokens (ty : Protocol.semantic_token_type)
    (tokens : Semantic_tokens.raw_token list) : Semantic_tokens.raw_token list =
  List.filter (fun (t : Semantic_tokens.raw_token) -> t.rt_type = ty) tokens

let has_modifier (m : Protocol.semantic_token_modifier)
    (t : Semantic_tokens.raw_token) : bool =
  List.mem m t.rt_modifiers

(** {1 collect_sexp_tokens — definition forms} *)

let test_defun_tokens () =
  let src = "(defun foo (x y)\n  (+ x y))" in
  let tokens = Semantic_tokens.collect_sexp_tokens ~text:src (parse src) in
  (* "defun" keyword *)
  let keywords = find_tokens Protocol.STKeyword tokens in
  let defun_kw =
    List.find_opt
      (fun (t : Semantic_tokens.raw_token) -> t.rt_line = 0 && t.rt_col = 1)
      keywords
  in
  Alcotest.(check bool) "defun is keyword" true (Option.is_some defun_kw);
  (* "foo" function name with definition modifier *)
  let fns = find_tokens Protocol.STFunction tokens in
  Alcotest.(check int) "one function token" 1 (List.length fns);
  let foo = List.hd fns in
  Alcotest.(check bool)
    "foo has definition" true
    (has_modifier Protocol.SMDefinition foo);
  Alcotest.(check int) "foo length" 3 foo.rt_length;
  (* "x" and "y" parameters *)
  let params = find_tokens Protocol.STParameter tokens in
  Alcotest.(check int) "two params" 2 (List.length params)

let test_defvar_tokens () =
  let src = "(defvar my-var 42)" in
  let tokens = Semantic_tokens.collect_sexp_tokens ~text:src (parse src) in
  let vars =
    List.filter
      (fun (t : Semantic_tokens.raw_token) ->
        t.rt_type = Protocol.STVariable && has_modifier Protocol.SMDefinition t)
      tokens
  in
  Alcotest.(check int) "one defined variable" 1 (List.length vars);
  let v = List.hd vars in
  Alcotest.(check int) "my-var length" 6 v.rt_length

let test_defconst_tokens () =
  let src = "(defconst my-const 99)" in
  let tokens = Semantic_tokens.collect_sexp_tokens ~text:src (parse src) in
  let consts =
    List.filter
      (fun (t : Semantic_tokens.raw_token) ->
        t.rt_type = Protocol.STVariable
        && has_modifier Protocol.SMDefinition t
        && has_modifier Protocol.SMReadonly t)
      tokens
  in
  Alcotest.(check int) "one readonly variable" 1 (List.length consts)

let test_defmacro_tokens () =
  let src = "(defmacro my-mac (x) x)" in
  let tokens = Semantic_tokens.collect_sexp_tokens ~text:src (parse src) in
  let macros = find_tokens Protocol.STMacro tokens in
  Alcotest.(check int) "one macro token" 1 (List.length macros);
  let m = List.hd macros in
  Alcotest.(check bool)
    "macro has definition" true
    (has_modifier Protocol.SMDefinition m)

(** {1 collect_sexp_tokens — special forms} *)

let test_special_form_keyword () =
  let src = "(if t 1 2)" in
  let tokens = Semantic_tokens.collect_sexp_tokens ~text:src (parse src) in
  let keywords = find_tokens Protocol.STKeyword tokens in
  let if_kw =
    List.find_opt
      (fun (t : Semantic_tokens.raw_token) -> t.rt_col = 1 && t.rt_length = 2)
      keywords
  in
  Alcotest.(check bool) "if is keyword" true (Option.is_some if_kw)

let test_let_keyword () =
  let src = "(let ((x 1)) x)" in
  let tokens = Semantic_tokens.collect_sexp_tokens ~text:src (parse src) in
  let keywords = find_tokens Protocol.STKeyword tokens in
  let let_kw =
    List.find_opt
      (fun (t : Semantic_tokens.raw_token) -> t.rt_col = 1 && t.rt_length = 3)
      keywords
  in
  Alcotest.(check bool) "let is keyword" true (Option.is_some let_kw)

let test_progn_keyword () =
  let src = "(progn 1 2 3)" in
  let tokens = Semantic_tokens.collect_sexp_tokens ~text:src (parse src) in
  let keywords = find_tokens Protocol.STKeyword tokens in
  let progn_kw =
    List.find_opt
      (fun (t : Semantic_tokens.raw_token) -> t.rt_col = 1 && t.rt_length = 5)
      keywords
  in
  Alcotest.(check bool) "progn is keyword" true (Option.is_some progn_kw)

let test_lambda_params () =
  let src = "(lambda (a b) (+ a b))" in
  let tokens = Semantic_tokens.collect_sexp_tokens ~text:src (parse src) in
  let params = find_tokens Protocol.STParameter tokens in
  Alcotest.(check int) "two lambda params" 2 (List.length params);
  let keywords = find_tokens Protocol.STKeyword tokens in
  let lambda_kw =
    List.find_opt
      (fun (t : Semantic_tokens.raw_token) -> t.rt_col = 1 && t.rt_length = 6)
      keywords
  in
  Alcotest.(check bool) "lambda is keyword" true (Option.is_some lambda_kw)

(** {1 collect_sexp_tokens — literals} *)

let test_string_literal () =
  let src = "\"hello\"" in
  let tokens = Semantic_tokens.collect_sexp_tokens ~text:src (parse src) in
  let strings = find_tokens Protocol.STString tokens in
  Alcotest.(check int) "one string token" 1 (List.length strings)

let test_number_literal () =
  let src = "42" in
  let tokens = Semantic_tokens.collect_sexp_tokens ~text:src (parse src) in
  let nums = find_tokens Protocol.STNumber tokens in
  Alcotest.(check int) "one number token" 1 (List.length nums)

let test_float_literal () =
  let src = "3.14" in
  let tokens = Semantic_tokens.collect_sexp_tokens ~text:src (parse src) in
  let nums = find_tokens Protocol.STNumber tokens in
  Alcotest.(check int) "one float token" 1 (List.length nums)

let test_char_literal () =
  let src = "?a" in
  let tokens = Semantic_tokens.collect_sexp_tokens ~text:src (parse src) in
  let strings = find_tokens Protocol.STString tokens in
  Alcotest.(check int) "char as string" 1 (List.length strings)

let test_keyword_literal () =
  let src = ":foo" in
  let tokens = Semantic_tokens.collect_sexp_tokens ~text:src (parse src) in
  let keywords = find_tokens Protocol.STKeyword tokens in
  Alcotest.(check int) "one keyword token" 1 (List.length keywords)

(** {1 collect_sexp_tokens — plain symbols} *)

let test_plain_symbol () =
  let src = "my-var" in
  let tokens = Semantic_tokens.collect_sexp_tokens ~text:src (parse src) in
  let vars = find_tokens Protocol.STVariable tokens in
  Alcotest.(check int) "plain symbol is variable" 1 (List.length vars);
  Alcotest.(check int) "length" 6 (List.hd vars).rt_length

(** {1 collect_comment_tokens} *)

let test_comment_line () =
  let src = ";; a comment" in
  let tokens = Semantic_tokens.collect_comment_tokens src in
  Alcotest.(check int) "one comment token" 1 (List.length tokens);
  let c = List.hd tokens in
  Alcotest.(check int) "line 0" 0 c.rt_line;
  Alcotest.(check int) "col 0" 0 c.rt_col;
  Alcotest.(check int) "length 12" 12 c.rt_length

let test_comment_mixed () =
  let src = "(defun foo ()\n  ;; body comment\n  nil)" in
  let tokens = Semantic_tokens.collect_comment_tokens src in
  Alcotest.(check int) "one comment" 1 (List.length tokens);
  let c = List.hd tokens in
  Alcotest.(check int) "on line 1" 1 c.rt_line

let test_no_comments () =
  let tokens = Semantic_tokens.collect_comment_tokens "(+ 1 2)" in
  Alcotest.(check int) "no comments" 0 (List.length tokens)

(** {1 delta_encode} *)

let test_delta_encode_simple () =
  let open Semantic_tokens in
  let tokens =
    [
      {
        rt_line = 0;
        rt_col = 0;
        rt_length = 5;
        rt_type = Protocol.STKeyword;
        rt_modifiers = [];
      };
      {
        rt_line = 0;
        rt_col = 6;
        rt_length = 3;
        rt_type = Protocol.STFunction;
        rt_modifiers = [ Protocol.SMDefinition ];
      };
      {
        rt_line = 1;
        rt_col = 2;
        rt_length = 1;
        rt_type = Protocol.STParameter;
        rt_modifiers = [];
      };
    ]
  in
  let data = delta_encode tokens in
  (* Token 1: deltaLine=0, deltaCol=0, len=5, type=keyword(4), mods=0 *)
  (* Token 2: deltaLine=0, deltaCol=6, len=3, type=function(0), mods=1 (definition) *)
  (* Token 3: deltaLine=1, deltaCol=2, len=1, type=parameter(3), mods=0 *)
  let expected = [ 0; 0; 5; 4; 0; 0; 6; 3; 0; 1; 1; 2; 1; 3; 0 ] in
  Alcotest.(check (list int)) "delta encoded" expected data

let test_delta_encode_unsorted () =
  let open Semantic_tokens in
  (* Provide tokens in reverse order; delta_encode should sort them *)
  let tokens =
    [
      {
        rt_line = 1;
        rt_col = 0;
        rt_length = 2;
        rt_type = Protocol.STNumber;
        rt_modifiers = [];
      };
      {
        rt_line = 0;
        rt_col = 0;
        rt_length = 3;
        rt_type = Protocol.STVariable;
        rt_modifiers = [];
      };
    ]
  in
  let data = delta_encode tokens in
  (* Sorted: line 0 first, then line 1 *)
  (* Token 1: deltaLine=0, deltaCol=0, len=3, type=variable(1), mods=0 *)
  (* Token 2: deltaLine=1, deltaCol=0, len=2, type=number(6), mods=0 *)
  let expected = [ 0; 0; 3; 1; 0; 1; 0; 2; 6; 0 ] in
  Alcotest.(check (list int)) "sorted correctly" expected data

let test_delta_encode_empty () =
  let data = Semantic_tokens.delta_encode [] in
  Alcotest.(check (list int)) "empty" [] data

(** {1 handle} *)

let test_handle_empty () =
  match Semantic_tokens.handle ~uri:"file:///tmp/test.el" ~doc_text:"" with
  | Ok json ->
      let s = Yojson.Safe.to_string json in
      Alcotest.(check string) "empty data" {|{"data":[]}|} s
  | Error _ -> Alcotest.fail "expected Ok"

let test_handle_defun () =
  let src = "(defun add (x y)\n  (+ x y))" in
  match Semantic_tokens.handle ~uri:"file:///tmp/test.el" ~doc_text:src with
  | Ok json -> (
      match json with
      | `Assoc fields -> (
          match List.assoc_opt "data" fields with
          | Some (`List items) ->
              Alcotest.(check bool) "has tokens" true (List.length items > 0)
          | _ -> Alcotest.fail "expected data field")
      | _ -> Alcotest.fail "expected assoc")
  | Error _ -> Alcotest.fail "expected Ok"

let test_handle_multiline_sorted () =
  let src = "(defun foo ()\n  (+ 1 2))" in
  match Semantic_tokens.handle ~uri:"file:///tmp/test.el" ~doc_text:src with
  | Ok json -> (
      match json with
      | `Assoc fields -> (
          match List.assoc_opt "data" fields with
          | Some (`List items) ->
              (* Every 5th element (deltaLine) should be >= 0 *)
              let data =
                List.map
                  (function `Int n -> n | _ -> failwith "not int")
                  items
              in
              let rec check_deltas i =
                if i >= List.length data then ()
                else (
                  Alcotest.(check bool)
                    (Printf.sprintf "delta_line[%d] >= 0" i)
                    true
                    (List.nth data i >= 0);
                  check_deltas (i + 5))
              in
              check_deltas 0
          | _ -> Alcotest.fail "expected data field")
      | _ -> Alcotest.fail "expected assoc")
  | Error _ -> Alcotest.fail "expected Ok"

(** {1 Test runner} *)

let () =
  Alcotest.run "semantic_tokens"
    [
      ( "definition-forms",
        [
          Alcotest.test_case "defun" `Quick test_defun_tokens;
          Alcotest.test_case "defvar" `Quick test_defvar_tokens;
          Alcotest.test_case "defconst" `Quick test_defconst_tokens;
          Alcotest.test_case "defmacro" `Quick test_defmacro_tokens;
        ] );
      ( "special-forms",
        [
          Alcotest.test_case "if keyword" `Quick test_special_form_keyword;
          Alcotest.test_case "let keyword" `Quick test_let_keyword;
          Alcotest.test_case "progn keyword" `Quick test_progn_keyword;
          Alcotest.test_case "lambda params" `Quick test_lambda_params;
        ] );
      ( "literals",
        [
          Alcotest.test_case "string" `Quick test_string_literal;
          Alcotest.test_case "number" `Quick test_number_literal;
          Alcotest.test_case "float" `Quick test_float_literal;
          Alcotest.test_case "char" `Quick test_char_literal;
          Alcotest.test_case "keyword" `Quick test_keyword_literal;
        ] );
      ("symbols", [ Alcotest.test_case "plain symbol" `Quick test_plain_symbol ]);
      ( "comments",
        [
          Alcotest.test_case "comment line" `Quick test_comment_line;
          Alcotest.test_case "mixed with code" `Quick test_comment_mixed;
          Alcotest.test_case "no comments" `Quick test_no_comments;
        ] );
      ( "delta-encode",
        [
          Alcotest.test_case "simple encoding" `Quick test_delta_encode_simple;
          Alcotest.test_case "unsorted input" `Quick test_delta_encode_unsorted;
          Alcotest.test_case "empty" `Quick test_delta_encode_empty;
        ] );
      ( "handle",
        [
          Alcotest.test_case "empty document" `Quick test_handle_empty;
          Alcotest.test_case "defun document" `Quick test_handle_defun;
          Alcotest.test_case "multiline sorted" `Quick
            test_handle_multiline_sorted;
        ] );
    ]
