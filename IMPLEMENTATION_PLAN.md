# Implementation Plan — Type Definition

> Source: [Spec 71](./specs/71-lsp-server.md) §Type Definition, task 15
> from [specs/IMPLEMENTATION\_PLAN.md](./specs/IMPLEMENTATION_PLAN.md)

## Problem

The LSP server has no `textDocument/typeDefinition` handler. Editors
cannot navigate from a value to the source of its type — e.g., jumping
from a variable of type `person` to the `(import-struct person …)`
declaration in its `.tart` file.

The spec (§Type Definition) calls for:

- Infer the type at the cursor. If it resolves to a named type defined
  in a `.tart` file, return the location of that type's declaration.
- Return `null` for primitive types (`integer`, `string`, `symbol`,
  etc.) that have no user-visible definition.

## Current State

- `type_at_sexp` in `server.ml` infers the type at a cursor position
  and returns a `Core.Types.typ option`.
- Named types appear as `TCon "module/name"` (e.g., `TCon "mymod/person"`
  from `opaque_con_name` which formats as `module ^ "/" ^ type`).
- Prelude opaque types use `TCon "prelude.buffer"`, `TCon "prelude.window"`,
  etc. (dot-separated, built-in — no source location).
- Intrinsic primitives use `TCon "%tart-intrinsic%Int"` etc.
- `.tart` signature ASTs carry `DType { type_name; type_loc; … }`,
  `DData { data_name; data_loc; … }`, and
  `DImportStruct { struct_name; struct_loc; … }` — all with spans.
- `find_definition_in_signature` searches `DDefun`/`DDefvar` but not
  type declarations. `find_definition_in_signatures` resolves module
  prefixes to `.tart` files via the search path.
- `definition_params` / `definition_result` protocol types already
  exist and can be reused (same request/response shape).

## Design

### Extracting the type name

Given the inferred `typ`:
1. Follow `TVar` links via `repr`.
2. For `TCon name` — use `name` directly.
3. For `TApp (TCon name, _)` — use `name` (parameterised type).
4. For `TArrow`, `TForall`, `TUnion`, `TLiteral`, etc. — return null
   (no single named type to navigate to).

### Resolving to a source location

Given a TCon name like `"mymod/person"`:
1. If it starts with `%tart-intrinsic%` → null (primitive).
2. If it starts with `prelude.` → null (built-in opaque, no source).
3. Split on `/` to get `(module_name, type_name)`.
4. Search for the module's `.tart` file via `Sig.Search_path.find_signature`.
5. Parse the signature and find the `DType`, `DData`, or `DImportStruct`
   declaration matching `type_name`.
6. Return the location span.

### New helper: `find_type_definition_in_signature`

Like `find_definition_in_signature` but matches `DType`, `DData`, and
`DImportStruct` by their name fields instead of `DDefun`/`DDefvar`.

### Reuse of protocol types

`textDocument/typeDefinition` has the same params and result shape as
`textDocument/definition`. Reuse `definition_params` / `definition_result`
and their parsers/encoders. Add `type_definition_provider : bool` to
`server_capabilities`.

## Tasks

### Task 1 — Protocol type and capability

**Files:** `lib/lsp/protocol.ml`, `lib/lsp/protocol.mli`, `lib/lsp/server.ml`

- Add `type_definition_provider : bool` to `server_capabilities`.
- Encode it as `"typeDefinitionProvider": true` in
  `server_capabilities_to_json`.
- Set `type_definition_provider = true` in `server.ml` `capabilities()`.
- Add `parse_type_definition_params` (alias for `parse_definition_params`
  since the JSON shape is identical).

### Task 2 — Handler and type lookup

**Files:** `lib/lsp/server.ml`

- Add `find_type_definition_in_signature`: match `DType`, `DData`,
  `DImportStruct` by name in a `Sig_ast.signature`.
- Add `find_type_definition_in_signatures`: given a `TCon` name,
  split `module/type`, resolve the module via the search path, delegate
  to `find_type_definition_in_signature`.
- Add `handle_type_definition`:
  1. Parse params, get doc, parse sexps, find sexp at cursor.
  2. Type-check, call `type_at_sexp`.
  3. Extract `TCon` name from result type (follow `TVar`, handle `TApp`).
  4. Skip intrinsics and prelude types → null.
  5. Call `find_type_definition_in_signatures` → location or null.
- Add `"textDocument/typeDefinition"` to `dispatch_request`.

### Task 3 — Tests

**Files:** `test/lsp_support/lsp_client.{ml,mli}`,
`test/lsp/server_test.ml`

- Add `type_definition_msg` to `lsp_client.{ml,mli}` (same shape as
  `definition_msg`).
- Add tests in "type-definition" group:
  - `test_type_definition_capability_advertised`: check initialize
    response has `typeDefinitionProvider: true`.
  - `test_type_definition_returns_null_for_primitive`: cursor on `42`
    (type is int) → null.
  - `test_type_definition_returns_null_for_symbol`: cursor on a plain
    symbol with no named type → null.
  - `test_type_definition_empty_file`: empty doc → null.

## Checklist

| # | Task | Status |
|---|------|--------|
| 1 | Protocol type and capability | Done |
| 2 | Handler and type lookup | Not started |
| 3 | Tests | Not started |
