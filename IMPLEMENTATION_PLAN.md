# Implementation Plan — Folding Ranges

> Source: [Spec 71](./specs/71-lsp-server.md) §Folding Ranges, task 16
> from [specs/IMPLEMENTATION_PLAN.md](./specs/IMPLEMENTATION_PLAN.md)

## Problem

The LSP server does not implement `textDocument/foldingRange`. Editors like
VS Code and Emacs (via Eglot) use folding ranges to let users collapse
regions of code. Without this, users get no folding support in `.el` files
opened via tart.

The spec (§Folding Ranges) calls for fold regions on:

- Top-level forms (`defun`, `defvar`, `defconst`, `defmacro`, `defcustom`)
  spanning multiple lines.
- `let`/`let*` binding blocks.
- Comment blocks (consecutive `;`-prefixed lines), with `kind = Comment`.
- Multi-line string literals.

## Current State

- No `foldingRange` types exist in `protocol.{ml,mli}`.
- No `folding_range_provider` in `server_capabilities`.
- No handler for `textDocument/foldingRange` in `server.ml`.
- The parser produces `Syntax.Sexp.t` with `Location.span` on every node,
  which gives start/end line numbers — all the data needed.

## Design

Follow the established delegation pattern used by `completion.ml`,
`signature_help.ml`, and `code_action.ml`: a standalone `folding.{ml,mli}`
module exposes a `handle` function that `server.ml` dispatches to.

Folding ranges come from two sources:

1. **AST-based** — walk parsed S-expressions for multi-line forms, `let`
   blocks, and string literals. Uses `Syntax.Sexp.t` spans directly.
2. **Text-based** — scan raw text for consecutive comment lines. This
   cannot use the AST because comments are not represented in it.

LSP's `FoldingRange` uses 0-based line numbers and optionally a
`FoldingRangeKind` (`comment`, `imports`, `region`). We'll use `comment`
for comment blocks and omit `kind` for code folds (spec says kind is
optional and defaults to no kind).

## Tasks

### Task 1 — Protocol types for folding ranges

**Files:** `lib/lsp/protocol.ml`, `lib/lsp/protocol.mli`

Add types and JSON encoders/parsers:

```ocaml
type folding_range_kind = FRComment | FRImports | FRRegion

type folding_range = {
  fr_start_line : int;
  fr_start_character : int option;
  fr_end_line : int;
  fr_end_character : int option;
  fr_kind : folding_range_kind option;
}

type folding_range_params = { frp_text_document : string }
type folding_range_result = folding_range list option

val parse_folding_range_params : Yojson.Safe.t -> folding_range_params
val folding_range_to_json : folding_range -> Yojson.Safe.t
val folding_range_result_to_json : folding_range_result -> Yojson.Safe.t
```

Add `folding_range_provider : bool` to `server_capabilities` and encode it
in `initialize_result_to_json`.

**Tests:** None needed — pure type definitions + JSON encoding.

### Task 2 — Folding module with AST-based folds

**Files:** `lib/lsp/folding.ml`, `lib/lsp/folding.mli`

Create the folding module. The core function walks `Syntax.Sexp.t list` and
collects multi-line ranges:

```ocaml
val collect_sexp_folds : Syntax.Sexp.t list -> Protocol.folding_range list
```

Rules:

- **Top-level forms**: any `List` whose head is `defun`, `defvar`,
  `defconst`, `defmacro`, `defcustom`, `defsubst`, `cl-defun`,
  `cl-defmacro`, `defclass`, `cl-defstruct` — if its span crosses
  multiple lines, emit a fold from the opening line to the closing line.
- **`let`/`let*` blocks**: any `List` whose head is `let` or `let*` — same
  multi-line rule.
- **Multi-line strings**: any `String` node whose span crosses lines.

All folds use 0-based line numbers (convert from the 1-based
`Location.span`). No `kind` is set for code folds.

Also add:

```ocaml
val collect_comment_folds : string -> Protocol.folding_range list
```

Scans raw text line-by-line. A contiguous run of lines starting with `;`
(ignoring leading whitespace) forms a comment fold with
`kind = Some FRComment`. Runs of fewer than 2 lines are ignored.

The top-level:

```ocaml
val handle :
  uri:string ->
  doc_text:string ->
  (Yojson.Safe.t, Rpc.response_error) result
```

Parses the document, collects sexp folds + comment folds, deduplicates by
start line, and returns the JSON result.

**Tests:** Add `test/lsp/folding_test.ml` (new file) with unit tests:

- Multi-line defun produces fold.
- Single-line defun does NOT produce fold.
- Nested defun produces two folds.
- `let`/`let*` block produces fold.
- Multi-line string produces fold.
- Comment block produces fold with `kind = comment`.
- Single comment line does NOT produce fold.
- Empty document returns empty list.

### Task 3 — Wire into server

**Files:** `lib/lsp/server.ml`

1. Set `folding_range_provider = true` in `capabilities ()`.
2. Add `handle_folding_range` that extracts URI, gets doc text, and
   delegates to `Folding.handle`.
3. Add `"textDocument/foldingRange"` to `dispatch_request`.

**Tests:** Add to `test/lsp/server_test.ml`:

- `test_folding_range_capability_advertised`: verify the initialize
  response includes `foldingRangeProvider = true`.
- `test_folding_range_multiline_defun`: open a file with a multi-line
  defun, send a folding range request, verify a fold is returned spanning
  the correct lines.
- `test_folding_range_comments`: open a file with a comment block, verify
  a fold with `kind = "comment"` is returned.
- `test_folding_range_empty_file`: open an empty file, verify empty
  result.

## Checklist

| # | Task | Status |
|---|------|--------|
| 1 | Protocol types for folding ranges | Done |
| 2 | Folding module with AST-based folds | Done |
| 3 | Wire into server | Done |
