# Implementation Plan — Document Save Events

> Source: Task 10 from [Spec 71](./specs/71-lsp-server.md)

## Problem

The server has no `textDocument/didSave` handler. The LSP
`TextDocumentSyncOptions` omits the `save` field, so clients never send
save notifications. This means:

1. Full re-checks never happen — only incremental cache-based checks run
   on `didChange`. If incremental state drifts, there is no corrective
   pass.
2. Non-open sibling `.tart` files that change on disk (e.g. after
   `git stash pop` or code generation) are not re-read until the next
   `didOpen`.
3. Dependents are not invalidated when a sibling file changes outside the
   editor.

## Current State

- `protocol.{ml,mli}`: `text_document_sync_options` has `open_close`
  and `change` but no `save` field.
  `text_document_sync_options_to_json` emits `openClose` and `change`
  only.
- `server.ml`: `dispatch_notification` has no `textDocument/didSave`
  arm. `capabilities()` sets `text_document_sync` with `open_close =
  true; change = Incremental`.
- `form_cache.mli`: exposes `invalidate_document` (clears cached form
  results) and `remove_document`.
- `server.ml`: `invalidate_dependents` already handles the cascade
  pattern (invalidate cache + re-publish diagnostics for each open
  dependent).
- `read_sibling_sig_content` re-reads `.tart` from disk when not in the
  signature tracker.

## Design

### Capability advertisement

Add `save : bool` to `text_document_sync_options`. When true, the JSON
encoder emits `"save": true` inside `textDocumentSync`. This tells the
client to send `textDocument/didSave` notifications.

The LSP spec also supports `"save": { "includeText": true }` for sending
the full text with the notification, but we don't need it — the server
already has the buffer contents from `didChange`.

### Handler behaviour

On `textDocument/didSave`:

1. **Invalidate form cache** for the saved document — forces a full
   (non-incremental) type-check.
2. **Re-publish diagnostics** — calls `publish_diagnostics`, which now
   runs a fresh check since the cache was cleared.
3. **Invalidate dependents** — calls `invalidate_dependents` to cascade
   re-checks to open files that depend on this module.

Step 1 is the key difference from `didChange`: on change, the form cache
is consulted and only changed forms are re-checked. On save, the entire
cache is cleared so every form is re-checked from scratch.

### Params parsing

`textDocument/didSave` sends `{ textDocument: { uri } }` (optionally
`text` if `includeText` is true, but we don't request that). We reuse
`Document.text_document_identifier_of_json` to extract the URI.

## Tasks

### Task 1 — Protocol: add `save` to sync options

**Files:** `lib/lsp/protocol.{ml,mli}`

- Add `save : bool` to `text_document_sync_options`.
- Emit `("save", \`Bool opts.save)` in
  `text_document_sync_options_to_json`.

### Task 2 — Handler + capability + tests

**Files:** `lib/lsp/server.ml`, `test/lsp/server_test.ml`,
`test/lsp_support/lsp_client.{ml,mli}`

- Set `save = true` in `capabilities()` sync options.
- Add `handle_did_save` notification handler:
  1. Parse URI from params.
  2. `Form_cache.invalidate_document` for the URI.
  3. `publish_diagnostics` with the document's current version.
  4. `invalidate_dependents` for the URI.
- Add `"textDocument/didSave"` to `dispatch_notification`.
- Add `did_save_msg` to `lsp_client.{ml,mli}`.
- Tests:
  - Save capability advertised (`save: true` in initialize response).
  - Save triggers fresh diagnostics (open doc → change → save → verify
    diagnostics republished).
  - Save invalidates dependents (open two files where A requires B,
    save B, verify A's diagnostics are updated).

## Checklist

| # | Task | Status |
|---|------|--------|
| 1 | Protocol: add `save` to sync options | Done |
| 2 | Handler + capability + tests | Done |
