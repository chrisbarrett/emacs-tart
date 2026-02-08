# Implementation Plan — Inlay Hints

> Source: [Spec 71](./specs/71-lsp-server.md) §Inlay Hints, task 13
> from [specs/IMPLEMENTATION_PLAN.md](./specs/IMPLEMENTATION_PLAN.md)

## Problem

The LSP server does not implement `textDocument/inlayHint`. Editors use
inlay hints to display inline type annotations and parameter names
without modifying the document text. Without this, users cannot see
inferred types for `let` bindings, function parameters at call sites,
or `defun` return types unless they hover over each expression
individually.

The spec (§Inlay Hints) calls for:

- **`let`/`let*` binding types** — shows inferred type after the
  variable name.
- **Parameter names at call sites** — shows parameter name when the
  argument is a literal or complex expression.
- **`defun` return types** — shows inferred return type when no `.tart`
  signature already provides the info.
- **Suppression** — hints are hidden when a `.tart` signature provides
  the type, or when the type is trivially obvious (e.g., a string
  literal bound to a variable).

## Current State

- No inlay hint types in `protocol.{ml,mli}`.
- No `inlay_hint_provider` in `server_capabilities`.
- No handler for `textDocument/inlayHint` in `server.ml`.
- `Typing.Check.check_program` returns `check_result` with an `env`
  (type environment) and `forms` list. `Typing.Infer.infer` can infer
  the type of any sexp. `Typing.Infer.infer_defun` returns the
  function type for defun forms.
- `Core.Types.to_string` pretty-prints types.
- The existing hover handler (`type_at_sexp`) demonstrates the pattern
  of inferring types for individual expressions.
- `range_of_span` in `server.ml` converts `Syntax.Location.span` to
  LSP ranges with UTF-16 positions.

## Design

Follow the delegation pattern used by `folding.ml`, `completion.ml`,
and `semantic_tokens.ml`: a standalone `inlay_hints.{ml,mli}` module
exposes a `handle` function that `server.ml` dispatches to.

### Hint Categories

1. **`defun` return types** — After the parameter list of each `defun`,
   show `: return-type`. Use `Typing.Infer.infer_defun` to get the
   function type, extract the return type from the `TArrow`. Skip if a
   sibling `.tart` signature declares the function (check
   `check_result.forms` for `TartDeclareForm`).

2. **`let`/`let*` binding types** — After each binding variable in
   `(let ((x EXPR)) ...)`, show `: type`. Use `Typing.Infer.infer` on
   the init expression. Skip when the type is trivially obvious: the
   init is a string/number/char literal whose type matches the variable
   type exactly.

3. **Parameter names at call sites** — At `(fn ARG1 ARG2)`, when `fn`
   resolves to a typed function, show parameter names before each
   argument. Only show when the argument is a literal or a complex
   expression (not a plain variable name that already acts as
   documentation). Deferred to a future iteration — the infrastructure
   for resolving parameter names from signatures is not yet in place,
   and this is the least impactful of the three categories.

### Trivial-Type Suppression

A type is "trivially obvious" when:

- A `let` binding's init expression is a string literal and the
  inferred type is `String`.
- A `let` binding's init expression is an integer literal and the
  inferred type is `Int`.
- A `let` binding's init expression is a float literal and the
  inferred type is `Float`.
- A `let` binding's init expression is `nil` and the type is `Nil`.
- A `let` binding's init expression is `t` and the type is `T`.

### Position Placement

- **`defun` return type hint:** Position at the end of the parameter
  list (closing paren of arglist). Kind = `Type`.
- **`let` binding type hint:** Position at the end of the variable
  symbol span. Kind = `Type`.

### UTF-16 Considerations

Hint positions use the negotiated position encoding (UTF-16 by
default). Use `Document.line_text_at` + `Document.utf16_offset_of_byte`
for the position character offset.

## Tasks

### Task 1 — Protocol types for inlay hints

**Files:** `lib/lsp/protocol.ml`, `lib/lsp/protocol.mli`

Add types and JSON encoders:

```ocaml
type inlay_hint_kind = IHType | IHParameter

type inlay_hint = {
  ih_position : position;
  ih_label : string;
  ih_kind : inlay_hint_kind option;
  ih_padding_left : bool;
  ih_padding_right : bool;
}

type inlay_hint_params = {
  ihp_text_document : string;
  ihp_range : range;
}

val parse_inlay_hint_params : Yojson.Safe.t -> inlay_hint_params
val inlay_hint_to_json : inlay_hint -> Yojson.Safe.t
val inlay_hint_result_to_json : inlay_hint list option -> Yojson.Safe.t
```

Add `inlay_hint_provider : bool` to `server_capabilities` and encode
in `server_capabilities_to_json`.

**Tests:** None needed — pure type definitions + JSON encoding.

### Task 2 — Inlay hints module (defun + let bindings)

**Files:** `lib/lsp/inlay_hints.ml`, `lib/lsp/inlay_hints.mli`

Create the module:

```ocaml
val handle :
  uri:string -> doc_text:string ->
  range:Protocol.range ->
  (Yojson.Safe.t, Rpc.response_error) result
```

`handle` parses the document, type-checks with `Typing.Check.check_program`,
then collects hints:

- Walk top-level forms for `defun` return types (via `infer_defun`).
- Walk all forms recursively for `let`/`let*` binding types (via
  `infer` on init expressions).
- Filter hints to the requested range.
- Convert positions to UTF-16.

`is_trivial_type` checks whether the init expression makes the type
obvious and suppresses the hint.

**Tests:** Add `test/lsp/inlay_hints_test.ml` with unit tests:

- `defun` produces return type hint after param list.
- `defvar` produces no hint (it's not a let binding).
- `let` binding produces type hint after variable name.
- `let*` binding produces type hints.
- String literal binding suppressed (trivially obvious).
- Number literal binding suppressed.
- Empty document returns no hints.
- Range filtering works (hints outside range excluded).

### Task 3 — Wire into server

**Files:** `lib/lsp/server.ml`

1. Set `inlay_hint_provider = true` in `capabilities()`.
2. Add `handle_inlay_hints` that extracts URI, gets doc text, and
   delegates to `Inlay_hints.handle`.
3. Add `"textDocument/inlayHint"` to `dispatch_request`.

**Tests:** Add to `test/lsp/server_test.ml`:

- `test_inlay_hint_capability_advertised`: verify the initialize
  response includes `inlayHintProvider: true`.
- `test_inlay_hint_defun_return_type`: open a file with a defun, send
  an inlay hint request, verify a type hint exists.
- `test_inlay_hint_let_binding`: open a file with a let form, verify
  binding type hints.
- `test_inlay_hint_empty_file`: verify empty result.

## Checklist

| # | Task | Status |
|---|------|--------|
| 1 | Protocol types for inlay hints | Not started |
| 2 | Inlay hints module | Not started |
| 3 | Wire into server | Not started |
