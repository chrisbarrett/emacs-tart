(** Unit tests for the Inlay_hints module. *)

open Lsp

(** {1 Helpers} *)

(** Full range covering an entire document (generous bounds). *)
let full_range : Protocol.range =
  {
    start = { line = 0; character = 0 };
    end_ = { line = 9999; character = 9999 };
  }

(** Call the inlay hints handler and extract the hint list from JSON. *)
let get_hints ?(range = full_range) (src : string) : Yojson.Safe.t list =
  match Inlay_hints.handle ~uri:"file:///test.el" ~doc_text:src ~range with
  | Ok (`List hints) -> hints
  | Ok _ -> []
  | Error _ -> []

(** Extract the label string from a hint JSON object. *)
let hint_label (hint : Yojson.Safe.t) : string =
  Yojson.Safe.Util.(hint |> member "label" |> to_string)

(** Extract the position line from a hint JSON object. *)
let hint_line (hint : Yojson.Safe.t) : int =
  Yojson.Safe.Util.(hint |> member "position" |> member "line" |> to_int)

(** Extract the kind from a hint JSON object. *)
let hint_kind (hint : Yojson.Safe.t) : int option =
  match Yojson.Safe.Util.(hint |> member "kind") with
  | `Int k -> Some k
  | _ -> None

(** {1 defun return type hints} *)

let test_defun_return_type () =
  let src = "(defun add1 (x)\n  (+ x 1))" in
  let hints = get_hints src in
  Alcotest.(check bool) "at least one hint" true (List.length hints > 0);
  let defun_hints = List.filter (fun h -> hint_line h = 0) hints in
  Alcotest.(check bool) "has defun hint" true (List.length defun_hints > 0);
  let h = List.hd defun_hints in
  (* Kind = 1 for IHType *)
  Alcotest.(check (option int)) "kind is type" (Some 1) (hint_kind h);
  (* Label should start with ": " *)
  let lbl = hint_label h in
  Alcotest.(check bool)
    "label starts with colon-space" true
    (String.length lbl >= 2 && String.sub lbl 0 2 = ": ")

let test_defun_no_hint_with_tart_declare () =
  (* When a tart-declare is present for a defun, no return type hint *)
  let src = "(tart-declare add1 ((Int) -> Int))\n(defun add1 (x)\n  (+ x 1))" in
  let hints = get_hints src in
  (* Should have no defun hints (line 1 is the defun) *)
  let defun_hints = List.filter (fun h -> hint_line h = 1) hints in
  Alcotest.(check int) "no defun hint" 0 (List.length defun_hints)

let test_defvar_no_hint () =
  let src = "(defvar my-var 42)" in
  let hints = get_hints src in
  Alcotest.(check int) "no hints for defvar" 0 (List.length hints)

(** {1 let binding type hints} *)

let test_let_binding_type () =
  let src = "(let ((x (+ 1 2)))\n  x)" in
  let hints = get_hints src in
  Alcotest.(check bool) "at least one let hint" true (List.length hints > 0);
  let h = List.hd hints in
  Alcotest.(check (option int)) "kind is type" (Some 1) (hint_kind h);
  let lbl = hint_label h in
  Alcotest.(check bool)
    "label starts with colon-space" true
    (String.length lbl >= 2 && String.sub lbl 0 2 = ": ")

let test_let_star_binding_type () =
  let src = "(let* ((x (+ 1 2))\n       (y (+ x 3)))\n  y)" in
  let hints = get_hints src in
  (* Should have hints for both x and y *)
  Alcotest.(check bool) "at least two let hints" true (List.length hints >= 2)

let test_string_literal_suppressed () =
  let src = "(let ((x \"hello\"))\n  x)" in
  let hints = get_hints src in
  (* String literal binding should be suppressed *)
  Alcotest.(check int) "string binding suppressed" 0 (List.length hints)

let test_number_literal_suppressed () =
  let src = "(let ((x 42))\n  x)" in
  let hints = get_hints src in
  (* Number literal binding should be suppressed *)
  Alcotest.(check int) "number binding suppressed" 0 (List.length hints)

(** {1 empty and edge cases} *)

let test_empty_document () =
  let hints = get_hints "" in
  Alcotest.(check int) "no hints" 0 (List.length hints)

let test_range_filtering () =
  (* Two defuns — request range only covers the second one *)
  let src = "(defun foo (x)\n  (+ x 1))\n\n(defun bar (y)\n  (+ y 2))" in
  let range : Protocol.range =
    { start = { line = 3; character = 0 }; end_ = { line = 4; character = 99 } }
  in
  let hints = get_hints ~range src in
  (* Should only have hints from bar, not foo *)
  let on_line_0 = List.filter (fun h -> hint_line h = 0) hints in
  Alcotest.(check int) "no hints from foo" 0 (List.length on_line_0);
  let on_line_3 = List.filter (fun h -> hint_line h = 3) hints in
  Alcotest.(check bool) "has hint from bar" true (List.length on_line_3 > 0)

(** {1 Test runner} *)

let () =
  Alcotest.run "inlay_hints"
    [
      ( "defun",
        [
          Alcotest.test_case "return type hint" `Quick test_defun_return_type;
          Alcotest.test_case "suppressed with tart-declare" `Quick
            test_defun_no_hint_with_tart_declare;
          Alcotest.test_case "defvar no hint" `Quick test_defvar_no_hint;
        ] );
      ( "let",
        [
          Alcotest.test_case "let binding type" `Quick test_let_binding_type;
          Alcotest.test_case "let* binding types" `Quick
            test_let_star_binding_type;
          Alcotest.test_case "string literal suppressed" `Quick
            test_string_literal_suppressed;
          Alcotest.test_case "number literal suppressed" `Quick
            test_number_literal_suppressed;
        ] );
      ( "edge-cases",
        [
          Alcotest.test_case "empty document" `Quick test_empty_document;
          Alcotest.test_case "range filtering" `Quick test_range_filtering;
        ] );
    ]
