(** Tests for document manager *)

open Lsp

(** {1 Document Store Tests} *)

let test_open_and_get () =
  let store = Document.create () in
  Document.open_doc store ~uri:"file:///test.el" ~version:1 ~text:"hello world";
  match Document.get_doc store "file:///test.el" with
  | Some doc ->
      Alcotest.(check string) "uri" "file:///test.el" doc.uri;
      Alcotest.(check int) "version" 1 doc.version;
      Alcotest.(check string) "text" "hello world" doc.text
  | None -> Alcotest.fail "Document not found"

let test_close () =
  let store = Document.create () in
  Document.open_doc store ~uri:"file:///test.el" ~version:1 ~text:"hello";
  Alcotest.(check bool)
    "exists before close" true
    (Option.is_some (Document.get_doc store "file:///test.el"));
  Document.close_doc store ~uri:"file:///test.el";
  Alcotest.(check bool)
    "not exists after close" true
    (Option.is_none (Document.get_doc store "file:///test.el"))

let test_list_uris () =
  let store = Document.create () in
  Document.open_doc store ~uri:"file:///a.el" ~version:1 ~text:"a";
  Document.open_doc store ~uri:"file:///b.el" ~version:1 ~text:"b";
  let uris = Document.list_uris store |> List.sort String.compare in
  Alcotest.(check (list string)) "uris" [ "file:///a.el"; "file:///b.el" ] uris

(** {1 Incremental Change Tests} *)

let test_full_replacement () =
  let store = Document.create () in
  Document.open_doc store ~uri:"file:///test.el" ~version:1 ~text:"old content";
  let change : Document.content_change =
    { range = None; text = "new content" }
  in
  (match
     Document.apply_changes store ~uri:"file:///test.el" ~version:2 [ change ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  match Document.get_doc store "file:///test.el" with
  | Some doc ->
      Alcotest.(check string) "text" "new content" doc.text;
      Alcotest.(check int) "version" 2 doc.version
  | None -> Alcotest.fail "Document not found"

let test_single_line_insert () =
  let store = Document.create () in
  Document.open_doc store ~uri:"file:///test.el" ~version:1 ~text:"hello world";
  let change : Document.content_change =
    {
      range =
        Some
          {
            start = { line = 0; character = 6 };
            end_ = { line = 0; character = 6 };
          };
      text = "beautiful ";
    }
  in
  (match
     Document.apply_changes store ~uri:"file:///test.el" ~version:2 [ change ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  match Document.get_doc store "file:///test.el" with
  | Some doc -> Alcotest.(check string) "text" "hello beautiful world" doc.text
  | None -> Alcotest.fail "Document not found"

let test_single_line_delete () =
  let store = Document.create () in
  Document.open_doc store ~uri:"file:///test.el" ~version:1 ~text:"hello world";
  let change : Document.content_change =
    {
      range =
        Some
          {
            start = { line = 0; character = 5 };
            end_ = { line = 0; character = 11 };
          };
      text = "";
    }
  in
  (match
     Document.apply_changes store ~uri:"file:///test.el" ~version:2 [ change ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  match Document.get_doc store "file:///test.el" with
  | Some doc -> Alcotest.(check string) "text" "hello" doc.text
  | None -> Alcotest.fail "Document not found"

let test_single_line_replace () =
  let store = Document.create () in
  Document.open_doc store ~uri:"file:///test.el" ~version:1 ~text:"hello world";
  let change : Document.content_change =
    {
      range =
        Some
          {
            start = { line = 0; character = 6 };
            end_ = { line = 0; character = 11 };
          };
      text = "Emacs";
    }
  in
  (match
     Document.apply_changes store ~uri:"file:///test.el" ~version:2 [ change ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  match Document.get_doc store "file:///test.el" with
  | Some doc -> Alcotest.(check string) "text" "hello Emacs" doc.text
  | None -> Alcotest.fail "Document not found"

let test_multiline_text () =
  let store = Document.create () in
  let text = "line 1\nline 2\nline 3" in
  Document.open_doc store ~uri:"file:///test.el" ~version:1 ~text;
  (* Insert at start of line 2 *)
  let change : Document.content_change =
    {
      range =
        Some
          {
            start = { line = 1; character = 0 };
            end_ = { line = 1; character = 0 };
          };
      text = ">> ";
    }
  in
  (match
     Document.apply_changes store ~uri:"file:///test.el" ~version:2 [ change ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  match Document.get_doc store "file:///test.el" with
  | Some doc ->
      Alcotest.(check string) "text" "line 1\n>> line 2\nline 3" doc.text
  | None -> Alcotest.fail "Document not found"

let test_delete_across_lines () =
  let store = Document.create () in
  let text = "line 1\nline 2\nline 3" in
  Document.open_doc store ~uri:"file:///test.el" ~version:1 ~text;
  (* Delete from end of line 1 to start of line 3 *)
  let change : Document.content_change =
    {
      range =
        Some
          {
            start = { line = 0; character = 6 };
            end_ = { line = 2; character = 0 };
          };
      text = "";
    }
  in
  (match
     Document.apply_changes store ~uri:"file:///test.el" ~version:2 [ change ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  match Document.get_doc store "file:///test.el" with
  | Some doc -> Alcotest.(check string) "text" "line 1line 3" doc.text
  | None -> Alcotest.fail "Document not found"

let test_multiple_changes () =
  let store = Document.create () in
  Document.open_doc store ~uri:"file:///test.el" ~version:1 ~text:"aaa bbb ccc";
  (* Apply two changes: first replaces bbb, then aaa
     Note: Changes are applied in order, so positions shift *)
  let changes : Document.content_change list =
    [
      (* First change: replace aaa with XXX (positions 0-3) *)
      {
        range =
          Some
            {
              start = { line = 0; character = 0 };
              end_ = { line = 0; character = 3 };
            };
        text = "XXX";
      };
      (* Second change: replace bbb with YYY (positions 4-7, but after first change still 4-7) *)
      {
        range =
          Some
            {
              start = { line = 0; character = 4 };
              end_ = { line = 0; character = 7 };
            };
        text = "YYY";
      };
    ]
  in
  (match
     Document.apply_changes store ~uri:"file:///test.el" ~version:2 changes
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  match Document.get_doc store "file:///test.el" with
  | Some doc -> Alcotest.(check string) "text" "XXX YYY ccc" doc.text
  | None -> Alcotest.fail "Document not found"

let test_error_document_not_open () =
  let store = Document.create () in
  let change : Document.content_change = { range = None; text = "new" } in
  match
    Document.apply_changes store ~uri:"file:///missing.el" ~version:1 [ change ]
  with
  | Ok () -> Alcotest.fail "Expected error"
  | Error e ->
      Alcotest.(check bool) "error mentions not open" true (String.length e > 0)

(** {1 JSON Parsing Tests} *)

let test_position_of_json () =
  let json = `Assoc [ ("line", `Int 10); ("character", `Int 5) ] in
  let pos = Document.position_of_json json in
  Alcotest.(check int) "line" 10 pos.line;
  Alcotest.(check int) "character" 5 pos.character

let test_range_of_json () =
  let json =
    `Assoc
      [
        ("start", `Assoc [ ("line", `Int 1); ("character", `Int 2) ]);
        ("end", `Assoc [ ("line", `Int 3); ("character", `Int 4) ]);
      ]
  in
  let range = Document.range_of_json json in
  Alcotest.(check int) "start.line" 1 range.start.line;
  Alcotest.(check int) "start.character" 2 range.start.character;
  Alcotest.(check int) "end.line" 3 range.end_.line;
  Alcotest.(check int) "end.character" 4 range.end_.character

let test_content_change_full () =
  let json = `Assoc [ ("text", `String "new content") ] in
  let change = Document.content_change_of_json json in
  Alcotest.(check bool) "no range" true (Option.is_none change.range);
  Alcotest.(check string) "text" "new content" change.text

let test_content_change_incremental () =
  let json =
    `Assoc
      [
        ( "range",
          `Assoc
            [
              ("start", `Assoc [ ("line", `Int 0); ("character", `Int 5) ]);
              ("end", `Assoc [ ("line", `Int 0); ("character", `Int 10) ]);
            ] );
        ("text", `String "replaced");
      ]
  in
  let change = Document.content_change_of_json json in
  Alcotest.(check bool) "has range" true (Option.is_some change.range);
  Alcotest.(check string) "text" "replaced" change.text

let test_text_document_identifier () =
  let json = `Assoc [ ("uri", `String "file:///test.el") ] in
  let uri = Document.text_document_identifier_of_json json in
  Alcotest.(check string) "uri" "file:///test.el" uri

let test_versioned_text_document_identifier () =
  let json =
    `Assoc [ ("uri", `String "file:///test.el"); ("version", `Int 42) ]
  in
  let uri, version = Document.versioned_text_document_identifier_of_json json in
  Alcotest.(check string) "uri" "file:///test.el" uri;
  Alcotest.(check int) "version" 42 version

let test_text_document_item () =
  let json =
    `Assoc
      [
        ("uri", `String "file:///test.el");
        ("version", `Int 1);
        ("languageId", `String "elisp");
        ("text", `String "(defun foo () t)");
      ]
  in
  let uri, version, text = Document.text_document_item_of_json json in
  Alcotest.(check string) "uri" "file:///test.el" uri;
  Alcotest.(check int) "version" 1 version;
  Alcotest.(check string) "text" "(defun foo () t)" text

(** {1 line_text_at Tests} *)

let test_line_text_at_single_line () =
  let text = "hello world" in
  Alcotest.(check (option string))
    "line 0" (Some "hello world")
    (Document.line_text_at text 0);
  Alcotest.(check (option string)) "line 1" None (Document.line_text_at text 1)

let test_line_text_at_multiline () =
  let text = "line 0\nline 1\nline 2" in
  Alcotest.(check (option string))
    "line 0" (Some "line 0")
    (Document.line_text_at text 0);
  Alcotest.(check (option string))
    "line 1" (Some "line 1")
    (Document.line_text_at text 1);
  Alcotest.(check (option string))
    "line 2" (Some "line 2")
    (Document.line_text_at text 2);
  Alcotest.(check (option string)) "line 3" None (Document.line_text_at text 3)

let test_line_text_at_trailing_newline () =
  let text = "hello\n" in
  Alcotest.(check (option string))
    "line 0" (Some "hello")
    (Document.line_text_at text 0);
  Alcotest.(check (option string))
    "line 1 (empty after newline)" (Some "")
    (Document.line_text_at text 1);
  Alcotest.(check (option string)) "line 2" None (Document.line_text_at text 2)

let test_line_text_at_empty () =
  let text = "" in
  Alcotest.(check (option string))
    "line 0 of empty" (Some "")
    (Document.line_text_at text 0);
  Alcotest.(check (option string))
    "line 1 of empty" None
    (Document.line_text_at text 1)

let test_line_text_at_negative () =
  let text = "hello" in
  Alcotest.(check (option string))
    "line -1" None
    (Document.line_text_at text (-1))

let test_line_text_at_multibyte () =
  (* Line with multi-byte UTF-8 content *)
  let text = "abc\n\xE4\xB8\xAD\xE6\x96\x87\n\xF0\x9F\x98\x80" in
  Alcotest.(check (option string))
    "line 0 ascii" (Some "abc")
    (Document.line_text_at text 0);
  Alcotest.(check (option string))
    "line 1 CJK" (Some "\xE4\xB8\xAD\xE6\x96\x87")
    (Document.line_text_at text 1);
  Alcotest.(check (option string))
    "line 2 emoji" (Some "\xF0\x9F\x98\x80")
    (Document.line_text_at text 2)

(** {1 UTF-16 Encoding Tests} *)

let test_utf16_ascii_round_trip () =
  (* ASCII: 1 byte = 1 UTF-16 code unit *)
  let line = "hello" in
  Alcotest.(check int)
    "byte 3 -> utf16 3" 3
    (Document.utf16_offset_of_byte ~line_text:line ~byte_offset:3);
  Alcotest.(check int)
    "utf16 3 -> byte 3" 3
    (Document.byte_offset_of_utf16 ~line_text:line ~utf16_offset:3)

let test_utf16_two_byte_utf8 () =
  (* "café": c(1) a(1) f(1) é(2 bytes, U+00E9) = 5 bytes, 4 UTF-16 units *)
  let line = "caf\xC3\xA9" in
  (* byte offset 4 is inside the é sequence; byte 3 is start of 'é' *)
  Alcotest.(check int)
    "byte 3 (start of é) -> utf16 3" 3
    (Document.utf16_offset_of_byte ~line_text:line ~byte_offset:3);
  Alcotest.(check int)
    "byte 5 (end) -> utf16 4" 4
    (Document.utf16_offset_of_byte ~line_text:line ~byte_offset:5);
  Alcotest.(check int)
    "utf16 3 -> byte 3" 3
    (Document.byte_offset_of_utf16 ~line_text:line ~utf16_offset:3);
  Alcotest.(check int)
    "utf16 4 -> byte 5" 5
    (Document.byte_offset_of_utf16 ~line_text:line ~utf16_offset:4)

let test_utf16_three_byte_cjk () =
  (* "a中b": a(1) 中(3 bytes, U+4E2D, BMP) b(1) = 5 bytes, 3 UTF-16 units *)
  let line = "a\xE4\xB8\xADb" in
  Alcotest.(check int)
    "byte 4 (start of b) -> utf16 2" 2
    (Document.utf16_offset_of_byte ~line_text:line ~byte_offset:4);
  Alcotest.(check int)
    "byte 5 (end) -> utf16 3" 3
    (Document.utf16_offset_of_byte ~line_text:line ~byte_offset:5);
  Alcotest.(check int)
    "utf16 2 -> byte 4" 4
    (Document.byte_offset_of_utf16 ~line_text:line ~utf16_offset:2)

let test_utf16_four_byte_emoji () =
  (* "a😀b": a(1) 😀(4 bytes, U+1F600, surrogate pair = 2 UTF-16 units) b(1)
     = 6 bytes, 4 UTF-16 units *)
  let line = "a\xF0\x9F\x98\x80b" in
  Alcotest.(check int)
    "byte 5 (start of b) -> utf16 3" 3
    (Document.utf16_offset_of_byte ~line_text:line ~byte_offset:5);
  Alcotest.(check int)
    "byte 6 (end) -> utf16 4" 4
    (Document.utf16_offset_of_byte ~line_text:line ~byte_offset:6);
  Alcotest.(check int)
    "utf16 3 -> byte 5" 5
    (Document.byte_offset_of_utf16 ~line_text:line ~utf16_offset:3);
  Alcotest.(check int)
    "utf16 4 -> byte 6" 6
    (Document.byte_offset_of_utf16 ~line_text:line ~utf16_offset:4)

let test_utf16_clamp_past_end () =
  let line = "abc" in
  Alcotest.(check int)
    "byte 10 -> utf16 3 (clamped)" 3
    (Document.utf16_offset_of_byte ~line_text:line ~byte_offset:10);
  Alcotest.(check int)
    "utf16 10 -> byte 3 (clamped)" 3
    (Document.byte_offset_of_utf16 ~line_text:line ~utf16_offset:10)

let test_utf16_empty_line () =
  let line = "" in
  Alcotest.(check int)
    "byte 0 -> utf16 0" 0
    (Document.utf16_offset_of_byte ~line_text:line ~byte_offset:0);
  Alcotest.(check int)
    "utf16 0 -> byte 0" 0
    (Document.byte_offset_of_utf16 ~line_text:line ~utf16_offset:0)

let test_utf16_multiple_emoji () =
  (* "😀😀": each 4 bytes, 2 UTF-16 units = 8 bytes, 4 UTF-16 units *)
  let line = "\xF0\x9F\x98\x80\xF0\x9F\x98\x80" in
  Alcotest.(check int)
    "byte 4 (2nd emoji) -> utf16 2" 2
    (Document.utf16_offset_of_byte ~line_text:line ~byte_offset:4);
  Alcotest.(check int)
    "byte 8 (end) -> utf16 4" 4
    (Document.utf16_offset_of_byte ~line_text:line ~byte_offset:8);
  Alcotest.(check int)
    "utf16 2 -> byte 4" 4
    (Document.byte_offset_of_utf16 ~line_text:line ~utf16_offset:2)

(** {1 UTF-16 Incremental Edit Tests} *)

let test_incremental_edit_with_cjk () =
  (* Text: "a中b\n" where 中 is 3 bytes (U+4E2D, BMP → 1 UTF-16 unit)
     UTF-16 offsets: a=0, 中=1, b=2
     Insert "X" before "b" using UTF-16 character offset 2 *)
  let store = Document.create () in
  let text = "a\xE4\xB8\xADb" in
  Document.open_doc store ~uri:"file:///test.el" ~version:1 ~text;
  let change : Document.content_change =
    {
      range =
        Some
          {
            start = { line = 0; character = 2 };
            end_ = { line = 0; character = 2 };
          };
      text = "X";
    }
  in
  (match
     Document.apply_changes store ~uri:"file:///test.el" ~version:2 [ change ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  match Document.get_doc store "file:///test.el" with
  | Some doc ->
      (* "a中Xb" — X inserted at byte offset 4 (after 中) *)
      Alcotest.(check string) "text" "a\xE4\xB8\xADXb" doc.text
  | None -> Alcotest.fail "Document not found"

let test_incremental_edit_with_emoji () =
  (* Text: "a😀b" where 😀 is 4 bytes (U+1F600, surrogate pair → 2 UTF-16 units)
     UTF-16 offsets: a=0, 😀=1..2, b=3
     Replace "b" using UTF-16 character offset 3 *)
  let store = Document.create () in
  let text = "a\xF0\x9F\x98\x80b" in
  Document.open_doc store ~uri:"file:///test.el" ~version:1 ~text;
  let change : Document.content_change =
    {
      range =
        Some
          {
            start = { line = 0; character = 3 };
            end_ = { line = 0; character = 4 };
          };
      text = "Z";
    }
  in
  (match
     Document.apply_changes store ~uri:"file:///test.el" ~version:2 [ change ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  match Document.get_doc store "file:///test.el" with
  | Some doc ->
      (* "a😀Z" — b replaced by Z *)
      Alcotest.(check string) "text" "a\xF0\x9F\x98\x80Z" doc.text
  | None -> Alcotest.fail "Document not found"

let test_incremental_edit_multiline_with_multibyte () =
  (* Two lines with multi-byte content:
     Line 0: "café"  (c a f é where é is 2 bytes, UTF-16 offsets: c=0 a=1 f=2 é=3)
     Line 1: "a😀b"  (UTF-16 offsets: a=0 😀=1..2 b=3)
     Delete from line 0 char 3 (é) through line 1 char 3 (b) *)
  let store = Document.create () in
  let text = "caf\xC3\xA9\na\xF0\x9F\x98\x80b" in
  Document.open_doc store ~uri:"file:///test.el" ~version:1 ~text;
  let change : Document.content_change =
    {
      range =
        Some
          {
            start = { line = 0; character = 3 };
            end_ = { line = 1; character = 3 };
          };
      text = "";
    }
  in
  (match
     Document.apply_changes store ~uri:"file:///test.el" ~version:2 [ change ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  match Document.get_doc store "file:///test.el" with
  | Some doc ->
      (* "caf" + "b" = "cafb" — deleted é, newline, a😀 *)
      Alcotest.(check string) "text" "cafb" doc.text
  | None -> Alcotest.fail "Document not found"

let test_edit_past_end_clamps () =
  (* Edit with start/end beyond the document end → clamps to end, inserts *)
  let store = Document.create () in
  let text = "hello" in
  Document.open_doc store ~uri:"file:///test.el" ~version:1 ~text;
  let change : Document.content_change =
    {
      range =
        Some
          {
            start = { line = 99; character = 0 };
            end_ = { line = 99; character = 0 };
          };
      text = " world";
    }
  in
  (match
     Document.apply_changes store ~uri:"file:///test.el" ~version:2 [ change ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  match Document.get_doc store "file:///test.el" with
  | Some doc -> Alcotest.(check string) "text" "hello world" doc.text
  | None -> Alcotest.fail "Document not found"

let test_edit_past_line_end_clamps () =
  (* Character offset beyond line length → clamps to line end *)
  let store = Document.create () in
  let text = "ab\ncd" in
  Document.open_doc store ~uri:"file:///test.el" ~version:1 ~text;
  let change : Document.content_change =
    {
      range =
        Some
          {
            start = { line = 0; character = 99 };
            end_ = { line = 0; character = 99 };
          };
      text = "X";
    }
  in
  (match
     Document.apply_changes store ~uri:"file:///test.el" ~version:2 [ change ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  match Document.get_doc store "file:///test.el" with
  | Some doc ->
      (* X inserted at end of line 0, before newline *)
      Alcotest.(check string) "text" "abX\ncd" doc.text
  | None -> Alcotest.fail "Document not found"

let test_incremental_edit_ascii_unchanged () =
  (* Verify ASCII edits still work identically (byte = UTF-16 for ASCII) *)
  let store = Document.create () in
  let text = "hello world" in
  Document.open_doc store ~uri:"file:///test.el" ~version:1 ~text;
  let change : Document.content_change =
    {
      range =
        Some
          {
            start = { line = 0; character = 5 };
            end_ = { line = 0; character = 11 };
          };
      text = " Emacs";
    }
  in
  (match
     Document.apply_changes store ~uri:"file:///test.el" ~version:2 [ change ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  match Document.get_doc store "file:///test.el" with
  | Some doc -> Alcotest.(check string) "text" "hello Emacs" doc.text
  | None -> Alcotest.fail "Document not found"

let () =
  Alcotest.run "document"
    [
      ( "store",
        [
          Alcotest.test_case "open and get" `Quick test_open_and_get;
          Alcotest.test_case "close" `Quick test_close;
          Alcotest.test_case "list_uris" `Quick test_list_uris;
        ] );
      ( "incremental",
        [
          Alcotest.test_case "full replacement" `Quick test_full_replacement;
          Alcotest.test_case "single line insert" `Quick test_single_line_insert;
          Alcotest.test_case "single line delete" `Quick test_single_line_delete;
          Alcotest.test_case "single line replace" `Quick
            test_single_line_replace;
          Alcotest.test_case "multiline text" `Quick test_multiline_text;
          Alcotest.test_case "delete across lines" `Quick
            test_delete_across_lines;
          Alcotest.test_case "multiple changes" `Quick test_multiple_changes;
          Alcotest.test_case "error document not open" `Quick
            test_error_document_not_open;
        ] );
      ( "json",
        [
          Alcotest.test_case "position" `Quick test_position_of_json;
          Alcotest.test_case "range" `Quick test_range_of_json;
          Alcotest.test_case "content change full" `Quick
            test_content_change_full;
          Alcotest.test_case "content change incremental" `Quick
            test_content_change_incremental;
          Alcotest.test_case "text document identifier" `Quick
            test_text_document_identifier;
          Alcotest.test_case "versioned text document identifier" `Quick
            test_versioned_text_document_identifier;
          Alcotest.test_case "text document item" `Quick test_text_document_item;
        ] );
      ( "line_text_at",
        [
          Alcotest.test_case "single line" `Quick test_line_text_at_single_line;
          Alcotest.test_case "multiline" `Quick test_line_text_at_multiline;
          Alcotest.test_case "trailing newline" `Quick
            test_line_text_at_trailing_newline;
          Alcotest.test_case "empty string" `Quick test_line_text_at_empty;
          Alcotest.test_case "negative line" `Quick test_line_text_at_negative;
          Alcotest.test_case "multibyte content" `Quick
            test_line_text_at_multibyte;
        ] );
      ( "utf16",
        [
          Alcotest.test_case "ASCII round-trip" `Quick
            test_utf16_ascii_round_trip;
          Alcotest.test_case "2-byte UTF-8" `Quick test_utf16_two_byte_utf8;
          Alcotest.test_case "3-byte CJK" `Quick test_utf16_three_byte_cjk;
          Alcotest.test_case "4-byte emoji" `Quick test_utf16_four_byte_emoji;
          Alcotest.test_case "clamp past end" `Quick test_utf16_clamp_past_end;
          Alcotest.test_case "empty line" `Quick test_utf16_empty_line;
          Alcotest.test_case "multiple emoji" `Quick test_utf16_multiple_emoji;
        ] );
      ( "utf16_incremental",
        [
          Alcotest.test_case "edit with CJK" `Quick
            test_incremental_edit_with_cjk;
          Alcotest.test_case "edit with emoji" `Quick
            test_incremental_edit_with_emoji;
          Alcotest.test_case "multiline with multibyte" `Quick
            test_incremental_edit_multiline_with_multibyte;
          Alcotest.test_case "ASCII unchanged" `Quick
            test_incremental_edit_ascii_unchanged;
        ] );
      ( "clamping",
        [
          Alcotest.test_case "edit past end clamps" `Quick
            test_edit_past_end_clamps;
          Alcotest.test_case "edit past line end clamps" `Quick
            test_edit_past_line_end_clamps;
        ] );
    ]
