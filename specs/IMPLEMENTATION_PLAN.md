# LSP Server — Outstanding Work

> Tracks all unfinished work from [Spec 71](./71-lsp-server.md), including
> deferred items. Replaces the previous phased plan.

## Audit Summary

The baseline LSP server is solid: lifecycle, document sync, diagnostics,
hover, goto-definition, find-references, code actions, completion, signature
help, document symbols, and rename are all implemented. Everything from the
original IMPLEMENTATION_PLAN Phases 1–4 and the deferred section of spec 71
remains outstanding.

### What's Done

| Feature | Key files |
|---------|-----------|
| initialize / initialized / shutdown / exit | `server.ml:409–477` |
| didOpen / didChange (incremental) / didClose | `server.ml:478–667`, `document.ml` |
| publishDiagnostics | `server.ml:330–406` |
| Hover (inferred types) | `server.ml:668–850` |
| Goto Definition | `server.ml:851–928` |
| Find References (current doc) | `server.ml:929–980` |
| Code Actions (missing sig, extract fn, version constraint) | `server.ml:981–1162`, `code_action.ml` |
| Completion | `server.ml:1197–1223`, `completion.ml` |
| Signature Help | `server.ml:1224–1254`, `signature_help.ml` |
| Document Symbols | `server.ml:1163–1196` |
| Rename (current doc) | `server.ml:1255–1324` |
| .tart file tracking & invalidation | `signature_tracker.ml`, `graph_tracker.ml` |
| Form-level caching | `form_cache.ml` |

### What's Not Done

Grouped into three tiers: correctness & testing, responsiveness
infrastructure, and new capabilities.

---

## Tier 1 — Correctness & Testing

### 1. Integration Test Harness

**Status:** Not implemented.

Build a test framework that spawns the LSP server as a subprocess,
communicates over JSON-RPC pipes, and asserts on responses. This is the
prerequisite for confidently landing every subsequent change.

**New files:**

| File | Purpose |
|------|---------|
| `test/lsp_integration/lsp_client.ml` | Spawn subprocess, send/receive JSON-RPC |
| `test/lsp_integration/lsp_client.mli` | Interface |
| `test/lsp_integration/integration_test.ml` | Test cases |
| `test/lsp_integration/dune` | Build target |

**Test cases:**

- Protocol lifecycle: initialize handshake, shutdown/exit, exit-without-
  shutdown, unknown method → -32601.
- Document sync: didOpen publishes diagnostics, didOpen with error, didChange
  incremental, didClose clears diagnostics.
- Edge cases: empty file, comment-only file, malformed JSON-RPC.

### 2. UTF-16 Position Encoding

**Status:** Not implemented. `server.ml:146` says "UTF-16 code units, but we
approximate with bytes." `document.ml:37–58` treats `character` as a byte
index.

**Changes:**

| File | Change |
|------|--------|
| `document.ml` | Add `utf16_offset_of_byte` and `byte_offset_of_utf16`; rewrite `position_to_offset` |
| `server.ml` | Convert byte columns → UTF-16 in `range_of_span` |
| `protocol.ml` | Parse `general.positionEncodings` in initialize params |
| `protocol.mli` | Extend `client_capabilities` with `position_encodings` |
| `test/lsp/document_test.ml` | Multi-byte test cases |

**Details:** Walk UTF-8 codepoints counting UTF-16 code units (BMP = 1,
supplementary = 2). Negotiate `utf-32` when the client advertises it. Create
`test/fixtures/typing/multibyte.el` with emoji + CJK content.

### 3. Incremental Sync Recovery

**Status:** Partially implemented. Bounds checking exists in
`document.ml:62–83` but returns an error instead of clamping. Version gap
detection is absent.

**Changes:**

| File | Change |
|------|--------|
| `document.ml` | Clamp out-of-range edits to document end; detect version gaps |
| `document.mli` | Expose `version` accessor if needed |
| `server.ml` | Log divergence at info level |
| `test/lsp/document_test.ml` | `test_edit_past_end_clamps`, `test_version_gap_logged` |

---

## Tier 2 — Responsiveness Infrastructure

### 4. Concurrent Request Processing

**Status:** Not implemented. `server.ml:1392–1411` is a blocking
`loop () = Rpc.read_message ... process_message ... loop ()`. No threading,
domains, or async.

**Architecture:** Single worker model. One background domain (or thread) runs
type checks. Mutex-protected work queue coalesces entries per URI; result
queue polled by the main loop via `Unix.select` with a short timeout.

**New files:** `lib/lsp/worker.ml`, `lib/lsp/worker.mli`.

**Changes to `server.ml`:** Replace blocking loop with select-based
multiplex. Extract `schedule_check` from inline `publish_diagnostics` calls.
Pass text + config snapshots to worker; update caches on main domain from
results.

**Phasing:** (a) Extract `schedule_check` abstraction, (b) wire it to a
background thread, (c) make main loop poll for results.

### 5. Request Cancellation

**Status:** Stub only. `server.ml:1365–1367`: `"$/cancelRequest" -> (* Ignore
cancellation for now *)`.

**Changes to `server.ml`:**

- Add `pending_requests` table mapping request ID → `Atomic.t bool`
  cancellation flag.
- Register long-running requests (hover, completion, code actions).
- On `$/cancelRequest`, set the flag.
- Handler checks flag periodically; raises `Cancelled`.
- Catch `Cancelled` in `process_message`, respond with error code -32800.

**Depends on:** Most useful after task 4 (concurrent processing).

### 6. Debounce & Coalescing

**Status:** Not implemented. Every `didChange` immediately triggers
`publish_diagnostics` → `check_document`.

**Changes to `server.ml`:**

- After `didChange`, delay type check by 200 ms (configurable). Reset on
  subsequent `didChange`.
- Before publishing diagnostics, compare against last published set per URI;
  suppress if identical. Add `last_diagnostics` cache to server state.
- Version staleness check: discard results if document version has advanced.

**Depends on:** Task 4 (timer-based debouncing requires non-blocking loop).
Without task 4, at minimum add version staleness checks and diagnostic
deduplication.

### 7. Progress Reporting

**Status:** Not implemented.

**Changes:**

| File | Change |
|------|--------|
| `protocol.ml` | Add `work_done_progress_begin/report/end` types, JSON encoders, `window/workDoneProgress/create` request |
| `protocol.mli` | Expose progress types |
| `server.ml` | Send progress during initial check and invalidation cascades |

**Details:** Check `window.workDoneProgress` in client capabilities. Use
unique tokens per operation. Operations are cancellable (ties into task 5).

**Depends on:** Task 4 (progress is only useful if the main loop can send
notifications while background work executes).

---

## Tier 3 — New Capabilities

### 8. File Watching

**Status:** Not implemented.

**Changes:**

| File | Change |
|------|--------|
| `protocol.ml` | Add `file_event`, `did_change_watched_files_params`, `file_change_type` types |
| `protocol.mli` | Expose new types |
| `server.ml` | Register `FileSystemWatcher` for `**/*.el` and `**/*.tart`; handle `workspace/didChangeWatchedFiles` |

**Behaviour:** Ignore events for open files (buffer is source of truth). For
closed files: re-read from disk, update dependency graph, invalidate caches,
re-publish diagnostics for open dependents.

### 9. Workspace Configuration

**Status:** Not implemented.

**Changes:**

| File | Change |
|------|--------|
| `protocol.ml` | Add settings types |
| `protocol.mli` | Expose settings types |
| `server.ml` | Handle `workspace/didChangeConfiguration`; store settings in server state |

**Settings:**

| Key | Type | Default | Effect |
|-----|------|---------|--------|
| `tart.emacsVersion` | `string` | auto-detect | Target Emacs version |
| `tart.searchPath` | `string[]` | `[]` | Additional signature lookup dirs |
| `tart.diagnostics.debounceMs` | `int` | `200` | Debounce interval |

On change: rebuild `module_config` if version changed, invalidate caches,
re-check open documents. Also check `initializationOptions` during
initialize.

### 10. Document Save Events

**Status:** Not implemented.

**Changes:**

| File | Change |
|------|--------|
| `protocol.ml` | Add `save : bool` to `text_document_sync_options` |
| `server.ml` | Handle `textDocument/didSave`; force full re-check, re-read non-open sibling files |

### 11. Workspace Symbols

**Status:** Not implemented.

**Changes:**

| File | Change |
|------|--------|
| `protocol.ml` | Add `workspace_symbol_params`, `symbol_information` types |
| `protocol.mli` | Expose new types |
| `server.ml` | Add `handle_workspace_symbol`, register `workspace_symbol_provider` capability |

**Details:** On `workspace/symbol`, collect symbols from all open documents
and loaded signatures. Case-insensitive prefix match on query. Return flat
`SymbolInformation[]` with location and container name.

### 12. Semantic Tokens

**Status:** Not implemented.

**New files:** `lib/lsp/semantic_tokens.ml`, `lib/lsp/semantic_tokens.mli`.

**Changes:**

| File | Change |
|------|--------|
| `protocol.ml` | Add semantic token legend, types |
| `protocol.mli` | Expose semantic token types |
| `server.ml` | Add handler, register `semanticTokensProvider` capability |

**Token types:** `function`, `variable`, `macro`, `parameter`, `keyword`,
`string`, `number`, `comment`, `type`.

**Token modifiers:** `definition`, `declaration`, `readonly`.

**Encoding:** Delta-encoded flat array
`[deltaLine, deltaStartChar, length, tokenType, tokenModifiers]`.

**Scope:** Full tokens only (`semanticTokens/full`). Delta updates
(`semanticTokens/full/delta`) deferred.

### 13. Inlay Hints

**Status:** Not implemented.

**New files:** `lib/lsp/inlay_hints.ml`, `lib/lsp/inlay_hints.mli`.

**Changes:**

| File | Change |
|------|--------|
| `protocol.ml` | Add `inlay_hint`, `inlay_hint_params` types |
| `protocol.mli` | Expose inlay hint types |
| `server.ml` | Add handler, register capability |

**Hints:** `let`/`let*` binding types, parameter names at call sites,
`defun` return types. Suppressed when a `.tart` signature already provides
the info, or when the type is trivially obvious.

### 14. Prepare Rename

**Status:** Not implemented.

**Changes:**

| File | Change |
|------|--------|
| `protocol.ml` | Add `prepare_rename_result` type |
| `protocol.mli` | Expose type |
| `server.ml` | Add `handle_prepare_rename`; change `rename_provider` from `bool` to `{ prepareProvider: true }` |

**Behaviour:** Return `{ range, placeholder }` for symbols; return null for
keywords, string literals, comments, numbers, builtins.

### 15. Type Definition

**Status:** Not implemented.

**Changes:**

| File | Change |
|------|--------|
| `protocol.ml` | Add params type (same as definition) |
| `protocol.mli` | Expose type |
| `server.ml` | Add `handle_type_definition`, register `type_definition_provider` capability |

**Behaviour:** Infer type at cursor (reuse `type_at_sexp`). If result is a
named type (`TCon`), look up its definition in loaded signatures. Return null
for primitives.

### 16. Folding Ranges

**Status:** Not implemented.

**New files:** `lib/lsp/folding.ml`, `lib/lsp/folding.mli`.

**Changes:**

| File | Change |
|------|--------|
| `protocol.ml` | Add `folding_range`, `folding_range_kind` types |
| `protocol.mli` | Expose folding range types |
| `server.ml` | Add handler, register capability |

**Folding rules:** Top-level forms spanning multiple lines, `let`/`let*`
blocks, consecutive comment lines (kind = Comment), multi-line strings. Walk
parsed sexps for forms; scan raw text for comment blocks.

---

## Deferred (from Spec 71)

These are explicitly out of scope until the above is complete.

| Feature | Reason |
|---------|--------|
| Call hierarchy (`callHierarchy/incomingCalls`, `outgoingCalls`) | Requires workspace-wide call graph |
| Type hierarchy (`typeHierarchy/supertypes`, `subtypes`) | Requires subtype relationship tracking |
| Code lens | No clear use case identified |
| Linked editing ranges | Not applicable to Elisp |
| On-type formatting | Better handled by `indent-region` |
| Pull-based diagnostics (`textDocument/diagnostic`) | Push model sufficient; migrate if protocol deprecates `publishDiagnostics` |
| Dynamic registration | Not needed while feature set is static |
| Workspace-wide find-references | References scoped to current document |
| Cross-file goto-definition for `.el` targets | Definition resolves to `.tart` signatures but not other `.el` files |
| Workspace-wide rename | Rename is single-document |
| Scope-aware completion | Completions not filtered by lexical scope |
| Semantic tokens delta updates (`semanticTokens/full/delta`) | Full tokens sufficient initially |

---

## Dependency Graph

```
1. Integration Test Harness
 |
 +---> 2. UTF-16 Positions
 +---> 3. Incremental Sync Recovery
 +---> 4. Concurrent Request Processing
         |
         +---> 5. Request Cancellation
         +---> 6. Debounce & Coalescing
         +---> 7. Progress Reporting
         |
         +---> 8. File Watching
         +---> 9. Workspace Configuration
         +---> 10. Document Save Events

11. Workspace Symbols       (independent)
12. Semantic Tokens          (independent)
13. Inlay Hints              (independent)
14. Prepare Rename           (independent)
15. Type Definition          (independent)
16. Folding Ranges           (independent)
```

Tier 3 tasks 11–16 are independent of each other and of tiers 1–2. They
depend only on the existing server infrastructure and can be implemented in
any order, interleaved with other work.

---

## Checklist

| # | Task | New files | Key changed files | Status |
|---|------|-----------|-------------------|--------|
| 1 | Integration test harness | `test/lsp_integration/{lsp_client,integration_test}.{ml,mli}`, `dune` | — | Done |
| 2 | UTF-16 positions | — | `document.ml`, `server.ml`, `protocol.ml` | Done |
| 3 | Incremental sync recovery | — | `document.ml`, `server.ml` | Done |
| 4 | Concurrent requests | `lib/lsp/worker.{ml,mli}` | `server.ml` | Not started |
| 5 | Request cancellation | — | `server.ml` | Stub only |
| 6 | Debounce & coalescing | — | `server.ml` | Partial (dedup + version staleness done; timer debounce needs task 4) |
| 7 | Progress reporting | — | `server.ml`, `protocol.ml` | Not started |
| 8 | File watching | — | `server.ml`, `protocol.ml` | Not started |
| 9 | Workspace configuration | — | `server.ml`, `protocol.ml` | Not started |
| 10 | Document save events | — | `server.ml`, `protocol.ml` | Not started |
| 11 | Workspace symbols | — | `server.ml`, `protocol.ml` | Not started |
| 12 | Semantic tokens | `lib/lsp/semantic_tokens.{ml,mli}` | `server.ml`, `protocol.ml` | Not started |
| 13 | Inlay hints | `lib/lsp/inlay_hints.{ml,mli}` | `server.ml`, `protocol.ml` | Not started |
| 14 | Prepare rename | — | `server.ml`, `protocol.ml` | Not started |
| 15 | Type definition | — | `server.ml`, `protocol.ml` | Not started |
| 16 | Folding ranges | `lib/lsp/folding.{ml,mli}` | `server.ml`, `protocol.ml` | Done |
