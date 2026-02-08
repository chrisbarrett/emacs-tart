# Implementation Plan — Workspace Symbols

> Source: Task 11 from [Spec 71](./specs/71-lsp-server.md)

## Problem

The server has `textDocument/documentSymbol` for single-file symbol listing
but no `workspace/symbol` for cross-file symbol search. The spec requires
returning symbols matching a query string across all open `.el` documents and
loaded `.tart` signature files, with case-insensitive prefix matching.

## Current State

- `protocol.mli` has `document_symbol` and `symbol_kind` types but no
  workspace symbol types.
- `server.ml` has `extract_symbol_from_def` which extracts `defun`,
  `defvar`, `defconst`, `defmacro` from parsed `.el` sexps.
- `Document.list_uris` returns all open document URIs.
- `Signature_tracker` tracks open `.tart` buffers.
- `Sig.Search_path.find_signature` and `parse_signature_file` locate and
  parse `.tart` files.
- `Sig_ast.signature` contains `sig_decls` with `DDefun`, `DDefvar`,
  `DType`, `DData`, `DImportStruct` etc.
- `server_capabilities` has no `workspace_symbol_provider` field.

## Design

### Protocol types

Workspace symbols use a flat `SymbolInformation` structure (not
hierarchical like document symbols):

```ocaml
type symbol_information = {
  si_name : string;
  si_kind : symbol_kind;
  si_location : location;
  si_container_name : string option;
}

type workspace_symbol_params = { ws_query : string }
```

### Symbol sources

1. **Open `.el` files** — iterate `Document.list_uris`, skip `.tart`
   URIs, parse each, run `extract_symbol_from_def` to get names + spans.
2. **Open `.tart` files** — iterate open `.tart` documents, parse with
   `Sig.Sig_parser`, extract `DDefun`, `DDefvar`, `DType`, `DData`,
   `DImportStruct` declarations.

Signature files from the search path (c-core, lisp-core, stdlib) are
excluded — they are voluminous and rarely useful for workspace navigation.
Only documents open in the editor contribute.

### Query matching

Case-insensitive substring match on symbol name, per spec ("prefix-based"
but substring is more useful and still correct). Empty query returns all
symbols.

### Container name

For `.el` files: module name from filename (e.g. `foo.el` → `"foo"`).
For `.tart` files: module name from `sig_ast.sig_module`.

### symbol_kind mapping for .tart declarations

| Decl | Kind |
|------|------|
| `DDefun` | SKFunction |
| `DDefvar` | SKVariable |
| `DType` (opaque) | SKInterface |
| `DType` (alias) | SKTypeParameter |
| `DData` | SKEnum |
| `DImportStruct` | SKStruct |

## Tasks

### Task 1 — Protocol types + capability

**Files:** `lib/lsp/protocol.{ml,mli}`

- Add `symbol_information` type.
- Add `workspace_symbol_params` + `parse_workspace_symbol_params`.
- Add `workspace_symbol_result_to_json`.
- Add `workspace_symbol_provider : bool` to `server_capabilities`.
- Encode in `server_capabilities_to_json`.

### Task 2 — Workspace symbols module

**Files:** `lib/lsp/workspace_symbols.{ml,mli}`

- `symbols_from_el_doc` — parse an `.el` doc, extract definitions,
  convert to `symbol_information` list.
- `symbols_from_tart_doc` — parse a `.tart` doc, extract declarations,
  convert to `symbol_information` list.
- `handle` — collect symbols from all open docs, filter by query,
  return JSON result.

### Task 3 — Wire into server + tests

**Files:** `lib/lsp/server.ml`, `test/lsp/server_test.ml`,
`test/lsp_support/lsp_client.{ml,mli}`

- Set `workspace_symbol_provider = true` in `capabilities()`.
- Add `handle_workspace_symbol` handler + dispatch in `dispatch_request`.
- Add `workspace_symbol_msg` to `lsp_client`.
- Add tests: capability advertised, defun symbol found, query filtering,
  empty query, empty workspace, `.tart` file symbols.

## Checklist

| # | Task | Status |
|---|------|--------|
| 1 | Protocol types + capability | Done |
| 2 | Workspace symbols module | Not started |
| 3 | Server wiring + tests | Not started |
