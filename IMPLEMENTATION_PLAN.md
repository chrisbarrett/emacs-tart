# Implementation Plan — Spec 89: Mutually Recursive Types

Extend `(type ...)` and `(let-type ...)` to accept multiple bindings in a single
form, making all names in the group mutually visible. Single-binding forms remain
unchanged; multi-binding forms are currently parse errors, so this is purely
additive.

## Status

| Task | Status |
|:-----|:-------|
| 1. AST: introduce `type_binding` record, make `type_decl` a group | Not started |
| 2. Parser: greedy multi-binding parsing for type and let-type | Not started |
| 3. Loader: add all group names before resolving definitions | Not started |
| 4. Validation, convert, workspace symbols: iterate bindings | Not started |
| 5. Tests: multi-binding parser and loader tests | Not started |

## Design

### AST change

Replace the single-binding `type_decl` with a group representation:

```ocaml
(** A single type binding within a type/let-type group. *)
and type_binding = {
  tb_name : string;
  tb_params : tvar_binder list;
  tb_body : sig_type option;  (* None only for opaque, single-binding *)
  tb_loc : span;
}

(** A group of one or more type bindings.
    Multi-binding groups enable mutual recursion. *)
and type_decl = {
  type_bindings : type_binding list;  (* non-empty *)
  type_loc : span;
}
```

`DType of type_decl` and `DLetType of type_decl` both use the same group type.
Single-binding forms produce a one-element list, preserving backward compat.

### Opaque restriction

Opaque types (no body) are only valid in single-binding forms. A multi-binding
form where any binding lacks a body is a parse error. The parser enforces this.

### Scoping within a group

All names in a multi-binding group are in scope for all definitions. The loader
registers all names as aliases/opaques in a first pass before resolving any
bodies. The validation pass adds all group names to the context before
validating individual bodies.

### Export behaviour

`build_alias_context` and `build_opaque_context` in `sig_convert.ml` must
iterate `type_bindings` for `DType`. `DLetType` remains excluded (wildcard arm).

## Tasks

### Task 1 — AST: introduce `type_binding`, make `type_decl` a group

**Files:** `lib/sig/sig_ast.ml`, `lib/sig/sig_ast.mli`

Add:
- `type_binding` record: `tb_name`, `tb_params`, `tb_body` (option), `tb_loc`

Change `type_decl`:
- Replace `type_name/type_params/type_body/type_loc` fields with
  `type_bindings : type_binding list` and `type_loc : span`

Update `decl_loc`:
- Both `DType d` and `DLetType d` already use `d.type_loc` — no change needed.

### Task 2 — Parser: greedy multi-binding for type and let-type

**File:** `lib/sig/sig_parser.ml`

Extract a shared helper `parse_type_bindings` that consumes binding tokens
greedily after the keyword:

1. Read a bare symbol (type name)
2. If next is `[`, read quantifiers
3. Read a type expression (definition body)
4. If more tokens remain, go to 1

Special cases:
- `(type name)` — single opaque binding (body = None), no more tokens → OK
- `(type name [params])` — single opaque with phantom params → OK
- Multi-binding where a binding has no body → parse error

Both `parse_type_decl` and `parse_let_type` call `parse_type_bindings`, then
wrap the result in `DType` or `DLetType` respectively.

### Task 3 — Loader: add all group names before resolving definitions

**File:** `lib/sig/sig_loader.ml`

For `DType` and `DLetType` arms in both `load_decls_into_state` and
`load_scoped_decl`:

1. **First pass:** iterate `type_bindings`, run shadowing check for each name,
   register each name as an alias or opaque in the type context
2. **Second pass:** (no change — the type context already has all names when
   alias bodies are resolved during `sig_type_to_typ_with_ctx`)

Since aliases are stored as `sig_type` (surface AST) and resolved lazily during
`sig_type_to_typ_with_ctx`, simply adding all names before any body is resolved
provides mutual visibility automatically.

For export marking (`DType` only): mark each binding's name as imported when
`from_include` is true.

### Task 4 — Validation, convert, workspace symbols: iterate bindings

**Files:**
- `lib/sig/sig_validation.ml` — `validate_decl` and `build_context`
- `lib/sig/sig_convert.ml` — `build_alias_context` and `build_opaque_context`
- `lib/lsp/workspace_symbols.ml` — `kind_of_decl` and `name_of_decl`
- `lib/graph/graph_builder.ml` — no change (DType/DLetType → [])
- `lib/typing/module_check.ml` — iterate bindings for kind checking

**validate_decl:**
- For DType/DLetType, add all group names to ctx first, then validate each
  binding body with the extended ctx (mutual visibility)

**build_context:**
- Add all `tb_name`s from each DType/DLetType group

**build_alias_context / build_opaque_context:**
- Iterate `d.type_bindings` instead of accessing `d.type_name` directly

**workspace_symbols:**
- `kind_of_decl` and `name_of_decl` return the first binding's info
  (for multi-binding, each binding becomes a separate symbol via
  `extract_tart_decl_symbols`)

### Task 5 — Tests: multi-binding parser and loader tests

**Files:** `test/sig/sig_parser_test.ml`, `test/sig/sig_loader_test.ml`

Parser tests:
- Single binding (backward compat): `(type pair (cons int int))` → one binding
- Multi-binding: `(type tree (leaf int | node forest) forest (list tree))`
  → two bindings with correct names/bodies
- Multi-binding with params: `(type tree [a] (list a) forest [a] (list (tree a)))`
- Opaque single: `(type handle)` → one binding, body = None
- Error: opaque in multi-binding `(type handle wrapper (list handle))`

Loader tests:
- Mutual recursion resolves: both names visible in each other's bodies
- Multi-binding let-type not exported: names excluded from alias/opaque context
