# Implementation Plan — Prepare Rename

> Source: [Spec 71](./specs/71-lsp-server.md) §Prepare Rename, task 14
> from [specs/IMPLEMENTATION_PLAN.md](./specs/IMPLEMENTATION_PLAN.md)

## Problem

The LSP server supports `textDocument/rename` but not
`textDocument/prepareRename`. Without prepare-rename, editors cannot
pre-validate whether a symbol at the cursor is renameable — they either
attempt the rename blindly (resulting in a confusing error or no-op for
builtins, keywords, literals, and comments) or disable the rename UI
entirely until the user invokes it.

The spec (§Prepare Rename) calls for:

- Return `{ range, placeholder }` for user-defined symbols.
- Return `null` for keywords (`:foo`), string literals, comments,
  numbers, chars, and builtin symbols (`t`, `nil`, `+`, `car`, etc.).
- Advertise `renameProvider: { prepareProvider: true }` instead of
  `renameProvider: true` in server capabilities.

## Current State

- `handle_rename` in `server.ml` parses the document, finds the sexp
  at the cursor via `Syntax.Sexp.find_with_context_in_forms`, and uses
  `symbol_name_of_sexp` to extract the name. Returns `null` for
  non-`Symbol` nodes but does not reject builtins.
- `code_action.ml` has `is_builtin_symbol` with a comprehensive
  deny-list of ~70 builtins + all binding forms.
- `rename_provider` is `bool` in `server_capabilities`.
- `range_of_span` converts spans to LSP ranges with UTF-16 positions.
- `lsp_client.ml` uses `position_params` for position-based request
  builders (hover, definition, etc.).

## Design

The logic is simple enough to live inline in `server.ml` (like
`handle_rename`) rather than a separate module.

### Handler Flow

1. Parse params (same shape as hover: text document + position).
2. Look up document, parse, find sexp at cursor.
3. If the target is not a `Symbol` node → return `null`.
4. If the symbol name is a builtin (reuse `Code_action.is_builtin_symbol`)
   → return `null`.
5. Otherwise, return `{ range, placeholder }` where `range` is the
   symbol's span converted via `range_of_span` and `placeholder` is
   the symbol name.

### Capability Change

`rename_provider : bool` → `rename_provider : rename_options option`
where `rename_options = { prepare_provider : bool }`. The JSON encoding
changes from `"renameProvider": true` to
`"renameProvider": { "prepareProvider": true }`. The existing rename
capability test must be updated.

### Exposing `is_builtin_symbol`

`Code_action.is_builtin_symbol` is currently not in `code_action.mli`.
It needs to be exposed so `server.ml` can call it for prepare-rename.
Alternatively, move it to a shared module, but exposing it is simpler
since `server.ml` already depends on `Code_action`.

## Tasks

### Task 1 — Protocol types and capability change

**Files:** `lib/lsp/protocol.ml`, `lib/lsp/protocol.mli`

- Add `rename_options = { prepare_provider : bool }`.
- Change `rename_provider : bool` to `rename_provider : rename_options option`.
- Update `server_capabilities_to_json` to emit
  `{ "prepareProvider": true }` when `prepare_provider = true`.
- Add `prepare_rename_result = { prr_range : range; prr_placeholder : string }`.
- Add `prepare_rename_result_to_json : prepare_rename_result option -> Yojson.Safe.t`.
- Add `prepare_rename_params` type and parser (same structure as
  hover — can reuse the pattern from `parse_hover_params`).

Update `server.ml` `capabilities()` to set
`rename_provider = Some { prepare_provider = true }`.

**Tests:** Update `test_rename_capability_advertised` in
`server_test.ml` to expect `{ "prepareProvider": true }` object
instead of `true`.

### Task 2 — Expose `is_builtin_symbol` and add handler

**Files:** `lib/lsp/code_action.mli`, `lib/lsp/server.ml`

- Expose `is_builtin_symbol : string -> bool` in `code_action.mli`.
- Add `handle_prepare_rename` to `server.ml`:
  1. Parse prepare rename params.
  2. Get document, parse sexps.
  3. Find sexp at cursor with `find_with_context_in_forms`.
  4. Match `ctx.target`: if `Symbol (name, span)` and not
     `Code_action.is_builtin_symbol name`, return
     `{ range = range_of_span ~text span; placeholder = name }`.
  5. Otherwise return `null`.
- Add `"textDocument/prepareRename"` to `dispatch_request`.

### Task 3 — Tests

**Files:** `test/lsp_support/lsp_client.{ml,mli}`,
`test/lsp/server_test.ml`

- Add `prepare_rename_msg` to `lsp_client.{ml,mli}` (same shape as
  `hover_msg`).
- Add tests in "prepare-rename" group:
  - `test_prepare_rename_symbol`: cursor on user-defined symbol →
    returns range and placeholder matching the symbol name.
  - `test_prepare_rename_builtin`: cursor on `+` → returns `null`.
  - `test_prepare_rename_keyword`: cursor on `:foo` keyword → `null`.
  - `test_prepare_rename_number`: cursor on `42` → `null`.
  - `test_prepare_rename_string`: cursor on `"hello"` → `null`.

## Checklist

| # | Task | Status |
|---|------|--------|
| 1 | Protocol types and capability change | Done |
| 2 | Expose is_builtin_symbol and add handler | Not started |
| 3 | Tests | Not started |
