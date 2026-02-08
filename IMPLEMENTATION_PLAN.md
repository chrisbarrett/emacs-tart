# Implementation Plan — Semantic Tokens

> Source: [Spec 71](./specs/71-lsp-server.md) §Semantic Tokens, task 12
> from [specs/IMPLEMENTATION_PLAN.md](./specs/IMPLEMENTATION_PLAN.md)

## Problem

The LSP server does not implement `textDocument/semanticTokens/full`.
Editors use semantic tokens for type-aware syntax highlighting that goes
beyond regex-based grammars. Without this, users get no semantic coloring
for function calls, variables, macros, parameters, or special forms in
`.el` files opened via tart.

The spec (§Semantic Tokens) calls for:

- **Token types:** `function`, `variable`, `macro`, `parameter`,
  `keyword`, `string`, `number`, `comment`, `type`.
- **Token modifiers:** `definition`, `declaration`, `readonly`.
- **Encoding:** Delta-encoded flat array
  `[deltaLine, deltaStartChar, length, tokenType, tokenModifiers]`.
- Full tokens only (`semanticTokens/full`); delta updates deferred.

## Current State

- No semantic token types in `protocol.{ml,mli}`.
- No `semantic_tokens_provider` in `server_capabilities`.
- No handler for `textDocument/semanticTokens/full` in `server.ml`.
- The parser produces `Syntax.Sexp.t` with `Location.span` on every node —
  all position data needed.
- Comments are not represented in the AST (skipped by the lexer), so they
  require text scanning like `Folding.collect_comment_folds`.
- The type checker's `infer.ml` recognizes special forms (`if`, `let`,
  `progn`, `setq`, `cond`, `and`, `or`, `when`, `unless`, `let*`,
  `quote`, `lambda`) — these are the "keyword" tokens.
- Definition forms (`defun`, `defvar`, `defconst`, `defmacro`, `defsubst`,
  `defcustom`, `cl-defun`, `cl-defmacro`) carry the defined name as the
  second element, which gets the `definition` modifier.

## Design

Follow the delegation pattern used by `folding.ml`, `completion.ml`, and
`code_action.ml`: a standalone `semantic_tokens.{ml,mli}` module exposes a
`handle` function that `server.ml` dispatches to.

### Token Classification Strategy

Tokens come from two sources:

1. **AST-based** — walk `Syntax.Sexp.t list` to classify nodes:
   - `Int`, `Float` → `number`
   - `String` → `string`
   - `Char` → `string` (character literals display as strings)
   - `Keyword` → `keyword` (`:foo` keywords in Elisp)
   - `Symbol` in head of `(defun NAME ...)` → `function` + `definition`
   - `Symbol` in head of `(defvar NAME ...)` / `(defconst NAME ...)` →
     `variable` + `definition` (defconst also gets `readonly`)
   - `Symbol` in head of `(defmacro NAME ...)` → `macro` + `definition`
   - `Symbol` in head of `(defcustom NAME ...)` → `variable` + `definition`
   - Parameters in `(defun NAME (PARAMS...) ...)` and
     `(lambda (PARAMS...) ...)` → `parameter`
   - Special forms (`if`, `let`, `let*`, `progn`, `setq`, `cond`, `and`,
     `or`, `when`, `unless`, `while`, `quote`, `lambda`, `function`,
     `save-excursion`, `save-restriction`, `unwind-protect`,
     `condition-case`, `catch`, `throw`, `interactive`, `declare`,
     `require`, `provide`) in head position → `keyword`
   - All other symbols → `variable` (default; we don't have enough context
     without type checking to distinguish function references from variables
     in general — this is fine since the distinction is only cosmetic and
     the type-aware path can be added later)

2. **Text-based** — scan raw text for comment lines (`;`-prefixed) like
   folding does. Each comment line becomes a `comment` token.

### Delta Encoding

LSP semantic tokens use a flat `int[]` encoded as 5-tuples relative to the
previous token: `(deltaLine, deltaStartChar, length, tokenType,
tokenModifiers)`. Tokens must be sorted by position (line then character).

The module collects raw `(line, col, length, type, modifiers)` tuples,
sorts them, then delta-encodes into the flat array.

### UTF-16 Considerations

Semantic token positions use the negotiated position encoding (UTF-16 by
default). Start characters and lengths must be in UTF-16 code units for
multi-byte content. Use `Document.utf16_offset_of_byte` for the start
character and compute length by subtracting start from end UTF-16 offsets.

## Tasks

### Task 1 — Protocol types for semantic tokens

**Files:** `lib/lsp/protocol.ml`, `lib/lsp/protocol.mli`

Add types and JSON encoders:

```ocaml
type semantic_token_type =
  | STFunction | STVariable | STMacro | STParameter
  | STKeyword | STString | STNumber | STComment | STType

type semantic_token_modifier =
  | SMDefinition | SMDeclaration | SMReadonly

type semantic_tokens_legend = {
  stl_token_types : string list;
  stl_token_modifiers : string list;
}

type semantic_tokens_params = { stp_text_document : string }

type semantic_tokens_result = { str_data : int list }
(** The delta-encoded flat array. *)

val semantic_token_type_index : semantic_token_type -> int
val semantic_token_modifier_bit : semantic_token_modifier -> int
val semantic_tokens_legend : semantic_tokens_legend
val parse_semantic_tokens_params : Yojson.Safe.t -> semantic_tokens_params
val semantic_tokens_result_to_json :
  semantic_tokens_result option -> Yojson.Safe.t
val semantic_tokens_legend_to_json :
  semantic_tokens_legend -> Yojson.Safe.t
```

Add `semantic_tokens_provider` to `server_capabilities` (as a record with
`full: true` and `legend`), encode in `server_capabilities_to_json`.

**Tests:** None needed — pure type definitions + JSON encoding.

### Task 2 — Semantic tokens module with AST + comment scanning

**Files:** `lib/lsp/semantic_tokens.ml`, `lib/lsp/semantic_tokens.mli`

Create the module. The core produces a list of raw tokens, then
delta-encodes them:

```ocaml
type raw_token = {
  rt_line : int;        (** 0-based *)
  rt_col : int;         (** UTF-16 code units *)
  rt_length : int;      (** UTF-16 code units *)
  rt_type : Protocol.semantic_token_type;
  rt_modifiers : Protocol.semantic_token_modifier list;
}

val collect_sexp_tokens :
  text:string -> Syntax.Sexp.t list -> raw_token list

val collect_comment_tokens : string -> raw_token list

val delta_encode : raw_token list -> int list

val handle :
  uri:string -> doc_text:string ->
  (Yojson.Safe.t, Rpc.response_error) result
```

`collect_sexp_tokens` walks the AST recursively:

- For each `Symbol` in head position of a `List`, check if it's a
  definition form, special form, or plain call.
- For definition forms, emit tokens for the head keyword + defined name
  (with `definition` modifier) + parameters (with `parameter` type).
- Recurse into child nodes.
- For literals (`Int`, `Float`, `String`, `Char`), emit the appropriate
  token type.
- For `Keyword` nodes (`:foo`), emit `keyword`.

`collect_comment_tokens` scans text line-by-line for `;`-prefixed lines
and emits one `comment` token per line.

`delta_encode` sorts tokens by (line, col), then computes deltas.

`handle` parses the document, collects AST tokens + comment tokens,
delta-encodes, returns JSON.

**Tests:** Add `test/lsp/semantic_tokens_test.ml` with unit tests:

- `defun` emits `keyword` for "defun", `function`+`definition` for name,
  `parameter` for params.
- `defvar` emits `keyword` for "defvar", `variable`+`definition` for name.
- `defconst` emits `variable`+`definition`+`readonly` for name.
- Special forms (`if`, `let`, `progn`) emit `keyword`.
- String/number/char literals emit correct types.
- Comment lines emit `comment` tokens.
- Delta encoding produces correct flat array.
- Empty document returns empty data.
- Multi-line form tokens are sorted correctly.

### Task 3 — Wire into server

**Files:** `lib/lsp/server.ml`

1. Set `semantic_tokens_provider` in `capabilities()`.
2. Add `handle_semantic_tokens` that extracts URI, gets doc text, and
   delegates to `Semantic_tokens.handle`.
3. Add `"textDocument/semanticTokens/full"` to `dispatch_request`.

**Tests:** Add to `test/lsp/server_test.ml`:

- `test_semantic_tokens_capability_advertised`: verify the initialize
  response includes `semanticTokensProvider` with `full: true` and the
  correct legend.
- `test_semantic_tokens_defun`: open a file with a defun, send a semantic
  tokens request, verify tokens include function name and keyword.
- `test_semantic_tokens_empty_file`: verify empty data.
- `test_semantic_tokens_comments`: verify comment tokens are returned.

## Checklist

| # | Task | Status |
|---|------|--------|
| 1 | Protocol types for semantic tokens | Done |
| 2 | Semantic tokens module | Done |
| 3 | Wire into server | Not started |
