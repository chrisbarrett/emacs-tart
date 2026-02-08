# Implementation Plan — Concurrent Processing & Responsiveness

> Source: Tasks 4–7 from [Spec 71](./specs/71-lsp-server.md)

## Problem

The server runs a blocking `read → process → respond → loop` cycle.
Every `didChange` immediately triggers `publish_diagnostics` →
`check_document`, blocking the main loop until the type check completes.
This means:

- Hover / completion / signature-help requests are blocked during type
  checks.
- Rapid keystrokes produce redundant type checks (no debouncing).
- `$/cancelRequest` is acknowledged but never acted on.
- Progress reporting is impossible because the loop cannot send
  notifications while work executes.

## Current State

- `server.ml` `run`: tight `loop () = Rpc.read_message ... process_message ... loop ()`.
- `publish_diagnostics` runs synchronously inside notification handlers
  (5 call sites: `apply_settings`, `handle_did_open`, `handle_did_change`,
  `handle_did_save`, `invalidate_dependents`).
- `rpc.ml` `read_message` blocks on `In_channel.input_line`.
- Diagnostic dedup + version staleness checks already exist.
- OCaml 5.4 available: `Domain`, `Mutex`, `Condition` are in stdlib.

## Design

### Architecture: single background domain

```
Main domain (IO)              Background domain (worker)
─────────────────             ──────────────────────────
read_message ──┐
               │  mutex-protected queue
schedule_check ─► enqueue(uri, text, version, config)
               │
poll_results ◄── dequeue(uri, version, diagnostics)
               │
write responses/
notifications
```

- **Main domain** owns all IO (stdin/stdout) and server state.
- **Background domain** runs type checks. It reads work items from a
  queue, executes `check_document`, and posts results back.
- A `Unix.pipe` signals the main loop that results are ready, so
  `Unix.select` can multiplex stdin + result-pipe without busy-waiting.

### Why not threads?

OCaml 5 domains provide true parallelism. A single background domain is
simpler than thread pools and avoids GC contention issues with multiple
domains. The type checker is CPU-bound, so one domain saturates one core
while the main domain handles IO.

### Coalescing

The work queue coalesces by URI: if a new check is enqueued for a URI
that already has a pending entry, the old entry is replaced. This handles
rapid `didChange` sequences without explicit timers.

### Debounce

After enqueuing a check, the main loop sets a per-URI debounce
timestamp. The worker skips items whose timestamp hasn't expired (the
item stays in the queue to be retried on the next poll). Default 200 ms,
configurable via `tart.diagnostics.debounceMs`.

### Cancellation

A `pending_requests` table maps request ID → `bool Atomic.t`. Long-running
request handlers (hover, completion, code actions) check the flag
periodically. `$/cancelRequest` sets the flag. The main loop catches
`Cancelled` and responds with error code −32800.

### Progress

During initial check and invalidation cascades, the server sends
`window/workDoneProgress/create` followed by `begin`/`report`/`end`
notifications. This requires checking `window.workDoneProgress` in
client capabilities.

## Tasks

### Task 1 — Extract `schedule_check` abstraction

**Files:** `lib/lsp/server.ml`

Replace the 5 inline `publish_diagnostics server uri (Some version)`
call sites with a `schedule_check server uri` helper. In this task the
helper still calls `publish_diagnostics` synchronously — the point is to
centralise the call site so task 2 can make it async.

- Add `schedule_check : t -> string -> unit` that looks up the document,
  extracts version, and calls `publish_diagnostics`.
- Replace all 5 call sites:
  - `apply_settings` (line 521)
  - `handle_did_open` (line 657)
  - `handle_did_change` (line 688)
  - `handle_did_save` (line 708)
  - `invalidate_dependents` (line 635)
- Verify all 122 tests still pass.

### Task 2 — Worker module + async check loop

**Files:** `lib/lsp/worker.{ml,mli}`, `lib/lsp/server.ml`, `lib/lsp/dune`

Create `worker.ml` with a background domain that processes type checks:

- Types: `work_item` (uri, text, version, config snapshot, sig_tracker
  snapshot, is_tart flag), `work_result` (uri, version,
  diagnostics, stats option).
- `create : unit -> t` — spawns background domain, creates work/result
  queues + Unix pipe for signalling.
- `enqueue : t -> work_item -> unit` — adds item to work queue,
  coalescing by URI. Writes a byte to the pipe to wake `select`.
- `poll_results : t -> work_result list` — drains the result queue
  (non-blocking).
- `signal_fd : t -> Unix.file_descr` — the read end of the pipe, for
  `select`.
- `shutdown : t -> unit` — signals the worker to stop and joins.

Modify `server.ml`:
- Add `worker : Worker.t` field to `server.t`.
- Change `schedule_check` to enqueue work items instead of calling
  `publish_diagnostics` directly.
- Change `run` to use `Unix.select` on `[stdin_fd; worker.signal_fd]`
  with a short timeout. On worker signal, call `poll_results` and
  publish diagnostics from results.
- `publish_diagnostics` becomes result-only: takes pre-computed
  diagnostics and writes the notification (no type checking).

Add `unix` to dune libraries.

### Task 3 — Debounce timer

**Files:** `lib/lsp/server.ml`

- Add `pending_checks : (string, float) Hashtbl.t` to `server.t`
  (URI → earliest-allowed-check timestamp).
- Add `debounce_ms : int` to `server.t` (default 200, from settings).
- In `schedule_check`, set `pending_checks.(uri) <- Unix.gettimeofday() + debounce_ms/1000`.
- In worker loop, before processing an item, check if
  `Unix.gettimeofday() >= pending_checks.(uri)`. If not, re-enqueue
  and sleep briefly.
- Wire `tart.diagnostics.debounceMs` setting into `apply_settings`.

### Task 4 — Request cancellation

**Files:** `lib/lsp/server.ml`, `lib/lsp/rpc.ml`

- Add `pending_requests : (Yojson.Safe.t, bool Atomic.t) Hashtbl.t` to
  `server.t`.
- Add `Cancelled` exception.
- In `process_message`, before dispatching a request: create an
  `Atomic.make false`, store in table. After response, remove from
  table.
- `$/cancelRequest` handler: parse id from params, look up in table,
  `Atomic.set flag true`.
- Catch `Cancelled` in `process_message`, respond with
  `Rpc.request_cancelled` (−32800).
- For now, only `check_document` checks the flag (between form-level
  cache lookups). Other handlers are fast enough not to need it.

### Task 5 — Progress reporting

**Files:** `lib/lsp/protocol.{ml,mli}`, `lib/lsp/server.ml`

- Parse `window.workDoneProgress` from client capabilities.
- Add protocol types: `work_done_progress_begin`,
  `work_done_progress_report`, `work_done_progress_end` with JSON
  encoders.
- Add `send_progress_begin`, `send_progress_report`,
  `send_progress_end` helpers to server.
- In `apply_settings` (full invalidation): send progress begin, report
  per-document, end.
- In `handle_did_open` with many dependents: send progress for
  cascade.

### Task 6 — Tests

**Files:** `test/lsp/server_test.ml`,
`test/lsp_support/lsp_client.{ml,mli}`

- Add `cancel_request_msg` to lsp_client.
- Test: schedule_check centralisation (open doc, verify diagnostics).
- Test: rapid didChange coalescing (3 changes, verify final diagnostics
  match final state — existing test may suffice).
- Test: `$/cancelRequest` returns −32800 for a pending request.
- Test: empty settings debounceMs parsed (verify no crash).
- Test: progress notifications appear during invalidation cascade (if
  client advertises `window.workDoneProgress`).

## Checklist

| # | Task | Status |
|---|------|--------|
| 1 | Extract `schedule_check` abstraction | Done |
| 2 | Worker module + async check loop | Not started |
| 3 | Debounce timer | Not started |
| 4 | Request cancellation | Not started |
| 5 | Progress reporting | Not started |
| 6 | Tests | Not started |
