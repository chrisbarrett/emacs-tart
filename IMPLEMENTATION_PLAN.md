# Implementation Plan — Server Handler Boilerplate Extraction

> Source: Tidy pass after completing [Spec 71](./specs/71-lsp-server.md) tasks
> 12–16

## Problem

The 13 request handlers in `server.ml` repeat three identical patterns:

1. **Missing-params error** — 7 lines each × 13 handlers = ~91 lines of
   identical `Error { code = invalid_params; … }` blocks.
2. **Parse-params + get-document** — parse the JSON, extract URI, look up
   document, return a null/empty result when absent.
3. **Find-sexp-at-cursor** — parse the doc, check for empty sexps, call
   `find_with_context_in_forms`, return null when nothing found.

Patterns 1 and 2 appear in all 13 handlers. Pattern 3 appears in 7 of
them (hover, definition, type-definition, references, prepare-rename,
rename, and partially code-action).

## Current State

`server.ml` is 1734 lines. Each handler starts with:

```ocaml
let handle_X (server : t) (params : Yojson.Safe.t option) =
  match params with
  | None ->
      Error { Rpc.code = Rpc.invalid_params;
              message = "Missing X params"; data = None }
  | Some json ->
      let p = Protocol.parse_X_params json in
      let uri = p.X_text_document in
      …
      match Document.get_doc server.documents uri with
      | None -> Ok (null_result)
      | Some doc -> (* handler-specific logic *)
```

The handlers that additionally find a sexp at cursor then repeat:

```ocaml
let parse_result = Syntax.Read.parse_string ~filename doc.text in
if parse_result.sexps = [] then Ok null_result
else
  match Syntax.Sexp.find_with_context_in_forms ~line ~col
          parse_result.sexps with
  | None -> Ok null_result
  | Some ctx -> (* handler-specific logic *)
```

## Design

### `require_params` — eliminate pattern 1

A one-liner that wraps the `None → invalid_params` match:

```ocaml
let require_params (label : string)
    (params : Yojson.Safe.t option)
    (f : Yojson.Safe.t -> (Yojson.Safe.t, Rpc.response_error) result) =
  match params with
  | None ->
      Error { Rpc.code = Rpc.invalid_params;
              message = "Missing " ^ label ^ " params"; data = None }
  | Some json -> f json
```

Every handler becomes `require_params "hover" params (fun json -> …)`.

### `with_document` — eliminate pattern 2

Given a URI, look up the document and invoke the handler, or return
the provided null result:

```ocaml
let with_document (server : t) ~(uri : string)
    ~(not_found : Yojson.Safe.t)
    (f : Document.doc -> (Yojson.Safe.t, Rpc.response_error) result) =
  match Document.get_doc server.documents uri with
  | None ->
      Log.debug "Document not found: %s" uri;
      Ok not_found
  | Some doc -> f doc
```

### `with_sexp_at_cursor` — eliminate pattern 3

Given a doc, position, and null result, parse + find sexp:

```ocaml
let with_sexp_at_cursor ~(doc : Document.doc) ~(uri : string)
    ~(line : int) ~(col : int) ~(not_found : Yojson.Safe.t)
    (f : Syntax.Read.parse_result ->
         Syntax.Sexp.find_context ->
         (Yojson.Safe.t, Rpc.response_error) result) =
  let filename = Uri.to_filename uri in
  let parse_result = Syntax.Read.parse_string ~filename doc.text in
  if parse_result.sexps = [] then (
    Log.debug "No S-expressions parsed";
    Ok not_found)
  else
    match Syntax.Sexp.find_with_context_in_forms ~line ~col
            parse_result.sexps with
    | None ->
        Log.debug "No S-expression at position";
        Ok not_found
    | Some ctx -> f parse_result ctx
```

### Composition

A handler like `handle_hover` goes from 67 lines to roughly:

```ocaml
let handle_hover server params =
  require_params "hover" params @@ fun json ->
  let hp = Protocol.parse_hover_params json in
  let uri = hp.text_document in
  Log.debug "Hover request at %s:%d:%d" uri hp.position.line
    hp.position.character;
  with_document server ~uri ~not_found:`Null @@ fun doc ->
  let col = Document.utf16_col_to_byte … in
  with_sexp_at_cursor ~doc ~uri ~line ~col ~not_found:`Null
    @@ fun parse_result ctx ->
  (* handler-specific logic — unchanged *)
```

### What *not* to extract

Each handler's core logic (what it does after finding the sexp) is
different enough that further abstraction would hurt readability. The
three helpers above target only the mechanical scaffolding.

## Tasks

### Task 1 — Add `require_params` helper and adopt in all handlers

**Files:** `lib/lsp/server.ml`

- Add `require_params` helper (5 lines).
- Rewrite all 13 `match params with | None → Error …` blocks to use
  `require_params`.
- No .mli change needed (helper is internal).

### Task 2 — Add `with_document` helper and adopt in all handlers

**Files:** `lib/lsp/server.ml`

- Add `with_document` helper (7 lines).
- Rewrite all 13 `match Document.get_doc … with | None → …` blocks
  to use `with_document`.

### Task 3 — Add `with_sexp_at_cursor` helper and adopt in applicable handlers

**Files:** `lib/lsp/server.ml`

- Add `with_sexp_at_cursor` helper (~15 lines).
- Rewrite the 7 handlers that parse + find sexp (hover, definition,
  type-definition, references, prepare-rename, rename, and the sexp
  portion of code-action if structurally compatible) to use it.
- `handle_document_symbol`, `handle_completion`, `handle_signature_help`,
  `handle_folding_range`, `handle_semantic_tokens`, and
  `handle_inlay_hints` do not use sexp-at-cursor and are unaffected.

## Checklist

| # | Task | Status |
|---|------|--------|
| 1 | `require_params` helper | Done |
| 2 | `with_document` helper | Not started |
| 3 | `with_sexp_at_cursor` helper | Not started |
