(** Unit tests for the Folding module. *)

open Lsp

(** {1 Helpers} *)

let parse (src : string) : Syntax.Sexp.t list =
  (Syntax.Read.parse_string ~filename:"test.el" src).sexps

(** {1 collect_sexp_folds} *)

let test_multiline_defun () =
  let src = "(defun foo (x)\n  (+ x 1))" in
  let folds = Folding.collect_sexp_folds (parse src) in
  Alcotest.(check int) "one fold" 1 (List.length folds);
  let r = List.hd folds in
  Alcotest.(check int) "start line" 0 r.fr_start_line;
  Alcotest.(check int) "end line" 1 r.fr_end_line;
  Alcotest.(check bool) "no kind" true (r.fr_kind = None)

let test_single_line_defun () =
  let src = "(defun foo () nil)" in
  let folds = Folding.collect_sexp_folds (parse src) in
  Alcotest.(check int) "no folds" 0 (List.length folds)

let test_nested_defun () =
  let src = "(defun outer ()\n  (defun inner ()\n    (+ 1 2)))" in
  let folds = Folding.collect_sexp_folds (parse src) in
  Alcotest.(check int) "two folds" 2 (List.length folds);
  (* Outer fold *)
  let r0 = List.nth folds 0 in
  Alcotest.(check int) "outer start" 0 r0.fr_start_line;
  Alcotest.(check int) "outer end" 2 r0.fr_end_line;
  (* Inner fold *)
  let r1 = List.nth folds 1 in
  Alcotest.(check int) "inner start" 1 r1.fr_start_line;
  Alcotest.(check int) "inner end" 2 r1.fr_end_line

let test_let_block () =
  let src = "(let ((x 1)\n      (y 2))\n  (+ x y))" in
  let folds = Folding.collect_sexp_folds (parse src) in
  Alcotest.(check int) "one fold" 1 (List.length folds);
  Alcotest.(check int) "start" 0 (List.hd folds).fr_start_line;
  Alcotest.(check int) "end" 2 (List.hd folds).fr_end_line

let test_let_star_block () =
  let src = "(let* ((x 1)\n       (y x))\n  y)" in
  let folds = Folding.collect_sexp_folds (parse src) in
  Alcotest.(check int) "one fold" 1 (List.length folds);
  Alcotest.(check int) "start" 0 (List.hd folds).fr_start_line

let test_multiline_string () =
  (* NOTE: The parser currently records string spans from the closing quote
     position, so multi-line strings appear single-line in the AST.  When the
     parser is fixed to record accurate string spans this test should be
     updated to expect a fold. *)
  let src = "\"line1\nline2\nline3\"" in
  let folds = Folding.collect_sexp_folds (parse src) in
  Alcotest.(check int) "no fold (parser limitation)" 0 (List.length folds)

let test_empty_document () =
  let folds = Folding.collect_sexp_folds [] in
  Alcotest.(check int) "empty" 0 (List.length folds)

let test_other_definition_forms () =
  let src =
    "(defvar x\n\
    \  1)\n\n\
     (defconst y\n\
    \  2)\n\n\
     (defmacro m ()\n\
    \  nil)\n\n\
     (defcustom c\n\
    \  3)"
  in
  let folds = Folding.collect_sexp_folds (parse src) in
  Alcotest.(check int) "four folds" 4 (List.length folds)

let test_plain_list_no_fold () =
  let src = "(+ 1\n   2\n   3)" in
  let folds = Folding.collect_sexp_folds (parse src) in
  Alcotest.(check int) "no folds for plain list" 0 (List.length folds)

(** {1 collect_comment_folds} *)

let test_comment_block () =
  let src = ";; comment 1\n;; comment 2\n;; comment 3" in
  let folds = Folding.collect_comment_folds src in
  Alcotest.(check int) "one fold" 1 (List.length folds);
  let r = List.hd folds in
  Alcotest.(check int) "start" 0 r.fr_start_line;
  Alcotest.(check int) "end" 2 r.fr_end_line;
  Alcotest.(check bool) "comment kind" true (r.fr_kind = Some Protocol.FRComment)

let test_single_comment_no_fold () =
  let src = ";; just one comment" in
  let folds = Folding.collect_comment_folds src in
  Alcotest.(check int) "no fold" 0 (List.length folds)

let test_two_comment_blocks () =
  let src = ";; block 1\n;; block 1\n\n;; block 2\n;; block 2\n;; block 2" in
  let folds = Folding.collect_comment_folds src in
  Alcotest.(check int) "two folds" 2 (List.length folds);
  Alcotest.(check int) "first start" 0 (List.nth folds 0).fr_start_line;
  Alcotest.(check int) "first end" 1 (List.nth folds 0).fr_end_line;
  Alcotest.(check int) "second start" 3 (List.nth folds 1).fr_start_line;
  Alcotest.(check int) "second end" 5 (List.nth folds 1).fr_end_line

let test_indented_comments () =
  let src = "  ;; indented 1\n  ;; indented 2" in
  let folds = Folding.collect_comment_folds src in
  Alcotest.(check int) "one fold" 1 (List.length folds)

let test_comment_folds_empty () =
  let folds = Folding.collect_comment_folds "" in
  Alcotest.(check int) "no folds" 0 (List.length folds)

(** {1 handle} *)

let test_handle_empty () =
  match Folding.handle ~uri:"file:///tmp/test.el" ~doc_text:"" with
  | Ok json ->
      let result = Yojson.Safe.to_string json in
      Alcotest.(check string) "empty list" "[]" result
  | Error _ -> Alcotest.fail "expected Ok"

let test_handle_mixed () =
  let src =
    ";; Header comment\n;; Second header line\n\n(defun foo (x)\n  (+ x 1))"
  in
  match Folding.handle ~uri:"file:///tmp/test.el" ~doc_text:src with
  | Ok json -> (
      match json with
      | `List items ->
          Alcotest.(check bool) "has folds" true (List.length items >= 2)
      | _ -> Alcotest.fail "expected list")
  | Error _ -> Alcotest.fail "expected Ok"

(** {1 Test runner} *)

let () =
  Alcotest.run "folding"
    [
      ( "sexp-folds",
        [
          Alcotest.test_case "multi-line defun" `Quick test_multiline_defun;
          Alcotest.test_case "single-line defun" `Quick test_single_line_defun;
          Alcotest.test_case "nested defun" `Quick test_nested_defun;
          Alcotest.test_case "let block" `Quick test_let_block;
          Alcotest.test_case "let* block" `Quick test_let_star_block;
          Alcotest.test_case "multi-line string" `Quick test_multiline_string;
          Alcotest.test_case "empty document" `Quick test_empty_document;
          Alcotest.test_case "other definition forms" `Quick
            test_other_definition_forms;
          Alcotest.test_case "plain list no fold" `Quick test_plain_list_no_fold;
        ] );
      ( "comment-folds",
        [
          Alcotest.test_case "comment block" `Quick test_comment_block;
          Alcotest.test_case "single comment no fold" `Quick
            test_single_comment_no_fold;
          Alcotest.test_case "two comment blocks" `Quick test_two_comment_blocks;
          Alcotest.test_case "indented comments" `Quick test_indented_comments;
          Alcotest.test_case "empty text" `Quick test_comment_folds_empty;
        ] );
      ( "handle",
        [
          Alcotest.test_case "empty document" `Quick test_handle_empty;
          Alcotest.test_case "mixed content" `Quick test_handle_mixed;
        ] );
    ]
