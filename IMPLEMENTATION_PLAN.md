# Implementation Plan — File Watching

> Source: Task 8 from [Spec 71](./specs/71-lsp-server.md)

## Problem

When `.el` or `.tart` files change on disk outside the editor (branch
switches, `git stash pop`, code generation, external editors), the
server doesn't notice. Sibling `.tart` signatures may become stale,
causing incorrect diagnostics in open `.el` files until the user
manually closes and reopens them.

## Current State

- `server.ml`: `handle_did_save` invalidates the form cache and
  re-checks, but only for documents the editor explicitly saves.
  There is no handler for `workspace/didChangeWatchedFiles`.
- `rpc.{ml,mli}`: Only `write_response` and `write_notification`
  exist. There is no `write_request` for server→client requests
  (needed for `client/registerCapability`).
- `protocol.{ml,mli}`: No file event types, no registration types.
- `signature_tracker`: `get_by_path` checks the tracker first, then
  falls back to disk. Closed `.tart` files are read from disk at type
  check time via `read_sibling_sig_content`.
- `graph_tracker`: `update_document` re-extracts edges from document
  text. `dependent_uris` finds open dependents of a module.
- `invalidate_dependents` in `server.ml` handles the cascade pattern.

## Design

### Registration

After receiving `initialized`, the server sends a
`client/registerCapability` request to dynamically register a
`workspace/didChangeWatchedFiles` watcher with glob patterns
`**/*.el` and `**/*.tart`.

This requires adding `write_request` to `rpc.{ml,mli}` (a JSON-RPC
request with an `id` field that expects a response). The server tracks
a monotonic request ID counter for outgoing requests.

The server doesn't need to process the response — it's a
fire-and-forget registration. The client may not support dynamic
registration, in which case it simply ignores the request.

### Handler behaviour

On `workspace/didChangeWatchedFiles`:

1. Parse the `changes` array (each has `uri` and `type`: created=1,
   changed=2, deleted=3).
2. For each change, skip if the URI is currently open (buffer is
   source of truth).
3. For changed/created `.tart` files: invalidate dependents via
   `invalidate_dependents` (forces re-read from disk on next check).
4. For changed/created `.el` files (not open): no action needed (the
   server only type-checks open documents).
5. For deleted files: invalidate dependents (their require/autoload
   may now be broken).

The key insight: we don't need to load/track closed files. We just
need to invalidate caches so the next type-check re-reads from disk.

### Params types

```
file_change_type = Created | Changed | Deleted
file_event = { fe_uri : string; fe_type : file_change_type }
did_change_watched_files_params = { dcwf_changes : file_event list }
```

## Tasks

### Task 1 — Protocol types + write_request

**Files:** `lib/lsp/rpc.{ml,mli}`, `lib/lsp/protocol.{ml,mli}`

- Add `write_request` to `rpc.{ml,mli}`: sends a JSON-RPC message
  with `id`, `method`, and `params`. Takes an `id` parameter (server
  generates IDs).
- Add `file_change_type` (Created=1, Changed=2, Deleted=3),
  `file_event`, `did_change_watched_files_params` to
  `protocol.{ml,mli}`.
- Add `parse_did_change_watched_files_params` JSON parser.
- Add `register_file_watchers_json` helper that builds the
  `client/registerCapability` params JSON for
  `workspace/didChangeWatchedFiles` with `**/*.el` and `**/*.tart`
  glob patterns.

### Task 2 — Registration + handler + tests

**Files:** `lib/lsp/server.ml`, `test/lsp/server_test.ml`,
`test/lsp_support/lsp_client.{ml,mli}`

- Add a `mutable next_request_id : int` field to `server.t`.
- In `handle_initialized`, after logging, call `write_request` with
  `client/registerCapability` to register the file watchers.
- Add `handle_did_change_watched_files` notification handler:
  1. Parse params.
  2. For each `file_event`, skip if URI is open
     (`Document.get_doc` returns `Some`).
  3. For changed/created `.tart` files: invalidate dependents via
     `invalidate_dependents`.
  4. For changed/created `.el` files (not open): no action needed.
  5. For deleted files: invalidate dependents.
- Add `"workspace/didChangeWatchedFiles"` to `dispatch_notification`.
- Add `did_change_watched_files_msg` to `lsp_client.{ml,mli}`.
- Tests:
  - Registration request sent after initialized (verify
    `client/registerCapability` appears in messages).
  - Changed .tart file invalidates dependent (open .el, send watched
    files notification for sibling .tart, verify diagnostics
    republished).
  - Open file events ignored (open a file, send watched files
    notification for same URI, verify no extra diagnostics).

## Checklist

| # | Task | Status |
|---|------|--------|
| 1 | Protocol types + write_request | Done |
| 2 | Registration + handler + tests | Not started |
