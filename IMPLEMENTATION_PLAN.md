# Implementation Plan — Spec 88: `let-type`

Replace the unused `(let ...)` block form with a top-level `(let-type ...)`
declaration. `let-type` has the same syntax and semantics as `(type ...)` except
it is **not exported** — invisible to `build_alias_context`/`build_opaque_context`
when other modules import the file.

## Status

| Task | Status |
|:-----|:-------|
| 1. AST: replace `DLet` with `DLetType of type_decl` | Done |
| 2. Parser: replace `parse_let` with `parse_let_type` | Done |
| 3. Loader: add `DLetType` handling (same as `DType` but no export) | Done |
| 4. Validation, convert, graph, LSP: update all `DLet` match arms | Done |
| 5. Tests: replace let tests with let-type tests | Done |
| 6. Font-lock: add `let-type` keyword to `tart-mode.el` | Done |

## Design

`let-type` reuses the existing `type_decl` record — it has the same fields
(name, params, body option, loc). The only difference is the variant tag
`DLetType` vs `DType`, which controls export visibility.

### Key insight: no body scoping

Unlike the old `DLet` (which had a `let_body` field for lexically-scoped
declarations), `DLetType` is a simple top-level declaration. It adds a type
alias/opaque to the file-local type context but is skipped by
`build_alias_context`/`build_opaque_context` (which only match `DType`).

### Removed types

- `DLet of let_decl` — the block form variant
- `let_decl` — the block record (bindings + body)
- `let_type_binding` — individual binding within a let block
- `parse_let` — block parser
- `parse_let_binding` — binding parser
- `load_let` — block loader

## Tasks

### Task 1 — AST: replace `DLet` with `DLetType of type_decl`

**Files:** `lib/sig/sig_ast.ml`, `lib/sig/sig_ast.mli`

Remove:
- `DLet of let_decl` variant from `decl`
- `let_decl` record type
- `let_type_binding` record type

Add:
- `DLetType of type_decl` variant to `decl`
  - Docstring: `[(let-type name ...)] — file-local type alias (not exported)`

Update:
- `decl_loc`: replace `DLet d -> d.let_loc` with `DLetType d -> d.type_loc`

### Task 2 — Parser: replace `parse_let` with `parse_let_type`

**File:** `lib/sig/sig_parser.ml`

Remove:
- `parse_let` function
- `parse_let_binding` function
- `"let"` arm in `parse_decl`

Add:
- `"let-type"` arm in `parse_decl` dispatching to `parse_let_type`
- `parse_let_type`: identical grammar to `parse_type_decl` but produces
  `DLetType` instead of `DType`. Reuse `parse_type_decl`'s logic by extracting a
  shared helper or duplicating the small function with a different constructor.

Update error message in catch-all arm to list `let-type` instead of `let`.

### Task 3 — Loader: add `DLetType` handling

**File:** `lib/sig/sig_loader.ml`

Remove:
- `load_let` function
- `DLet` arm in `load_scoped_decl`

Add `DLetType d ->` arm in `load_scoped_decl`, identical to `DType d ->` except:
- **No `mark_type_imported`** — `let-type` is never exported, even from include
- Shadowing check still runs

`build_alias_context` and `build_opaque_context` in `sig_convert.ml` already
only match `DType`, so `DLetType` is automatically excluded from exports.

### Task 4 — Validation, convert, graph, LSP: update all `DLet` match arms

**Files:**
- `lib/sig/sig_validation.ml` — `validate_decl` and `build_context`
- `lib/sig/sig_convert.ml` — `build_alias_context`, `build_opaque_context` (no
  change needed, `_ ->` already covers it; just verify)
- `lib/graph/graph_builder.ml` — `extract_edges_from_decl`
- `lib/lsp/workspace_symbols.ml` — `decl_symbol_kind`, `decl_name`, and
  `extract_tart_decl_symbols`
- `lib/typing/module_check.ml` — `check_decl_kinds_with_scope`

For each:
- Replace `DLet d -> ...` with `DLetType d -> ...`
- `validate_decl`: delegate to `validate_type_decl ctx d` (same as `DType`)
- `build_context`: add `DLetType d -> with_type ctx d.type_name` (let-type names
  ARE visible within the file for validation)
- `graph_builder`: `DLetType _ -> []` (no edges; it's file-local)
- `workspace_symbols`: `DLetType` returns `Some Protocol.SKTypeParameter` for
  kind and `Some d.type_name` for name (visible in file symbols)
- `module_check`: `DLetType _ -> []` (no kind errors from a simple type decl
  at top level)

### Task 5 — Tests: replace let tests with let-type tests

**File:** `test/sig/sig_parser_test.ml`

Remove:
- `test_let_simple`
- `test_let_parameterized`
- `test_let_multiple_bindings`
- `test_let_error_no_bindings`
- `test_let_error_no_body`
- `"let-declarations"` test group

Add `"let-type-declarations"` test group with:
- `test_let_type_simple`: `(let-type pair (cons int int))` → `DLetType` with
  name `"pair"`, no params, body = `STApp("cons", [int; int])`
- `test_let_type_parameterized`: `(let-type wrapper [a] (list a))` → one param
- `test_let_type_opaque`: `(let-type handle)` → body = `None`
- `test_let_type_error_no_name`: `(let-type)` → parse error

### Task 6 — Font-lock: add `let-type` keyword to `tart-mode.el`

**File:** `lisp/tart-mode.el`

Add `"let-type"` to the `declaration-keywords` list (line 455) so it gets
keyword highlighting. Add a font-lock rule for name highlighting after
`let-type`, similar to the `type` rule.
