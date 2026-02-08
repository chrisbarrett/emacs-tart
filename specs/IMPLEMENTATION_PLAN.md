# LSP Server Improvement Plan

> Extends [Spec 71 -- LSP Server](./71-lsp-server.md) with concrete
> implementation tasks grouped into four phases.

## Current State

The LSP server (`lib/lsp/`) implements a synchronous JSON-RPC 2.0 server
over stdin/stdout. It supports:

- Full lifecycle: initialize, initialized, shutdown, exit
- Document sync: didOpen, didChange (incremental), didClose
- Diagnostics: publishDiagnostics on open/change
- Hover (inferred types), Go to Definition, Find References
- Code Actions (missing signature, extract function, version constraint fixes)
- Completion, Signature Help, Document Symbols, Rename

Key architectural facts:

| Aspect | Current state |
|--------|---------------|
| Concurrency | Fully synchronous; `Server.run` is a blocking `loop ()` |
| OCaml version | >= 5.0 (domains/effects available but unused) |
| Async libraries | None (no Lwt, Eio, or fiber) |
| Build system | Dune 3.0, Menhir |
| Test framework | Alcotest |
| LSP library | `tart.lsp` (public name); depends on `yojson`, `tart.syntax`, `tart.core`, `tart.typing`, `tart.graph`, `tart.sig`, `tart.log` |
| Position encoding | Byte offsets (comment in `server.ml:146` says "UTF-16 code units, but we approximate with bytes") |
| Cancel support | `$/cancelRequest` is received but ignored (`server.ml:1366`) |
| File watching | Not implemented |
| `didSave` | Not handled |

---

## Phase 1: Foundation (Testing & Correctness)

### 1.1 LSP Integration Test Harness

**Goal.** Build a test framework that spawns the server as a subprocess,
communicates over JSON-RPC pipes, and asserts on responses. This unlocks
confident refactoring for every later phase.

**Rationale.** The existing unit tests in `test/lsp/` exercise individual
modules (RPC framing in `rpc_test.ml`, document store in `document_test.ml`,
server lifecycle via temp-file I/O in `server_test.ml`). There is no test
that drives the server as a long-lived process exchanging messages over
pipes, which is the actual usage mode.

#### Files to create

| File | Purpose |
|------|---------|
| `test/lsp_integration/lsp_client.ml` | Helper module: spawn subprocess, send JSON-RPC, read responses |
| `test/lsp_integration/lsp_client.mli` | Interface for the helper |
| `test/lsp_integration/integration_test.ml` | Test cases |
| `test/lsp_integration/dune` | Build target |

#### `lsp_client.ml` design

```ocaml
type t
(** A running server subprocess with stdin/stdout pipes. *)

val start : ?args:string list -> unit -> t
(** Spawn `./tart lsp` as a child process. *)

val send_request : t -> id:int -> method_:string -> params:Yojson.Safe.t -> unit
(** Write a JSON-RPC request. *)

val send_notification : t -> method_:string -> params:Yojson.Safe.t -> unit
(** Write a JSON-RPC notification. *)

val read_response : t -> Yojson.Safe.t
(** Block until a response arrives; parse and return it. *)

val read_notification : t -> Yojson.Safe.t
(** Block until a server-initiated notification arrives. *)

val shutdown_and_exit : t -> int
(** Send shutdown + exit; return exit code. *)
```

Internally: use `Unix.open_process_full` (or `Unix.create_process` with
pipe pairs) to get `in_channel`/`out_channel` handles. Re-use `Rpc.write_*`
and a simplified `Rpc.read_message` loop for reading.

#### `dune` target

```sexp
(test
 (name integration_test)
 (modules lsp_client integration_test)
 (libraries tart unix alcotest)
 (deps
  (source_tree %{project_root}/typings)
  (package tart))               ; ensure binary is built
 (action
  (chdir %{project_root} (run %{test}))))
```

The `(package tart)` dep ensures the binary is built before the test runs.
Use `(chdir %{project_root} ...)` so the server can find typings relative
to the working directory.

#### Test cases (in `integration_test.ml`)

**Protocol lifecycle**

1. `test_initialize_handshake` -- send initialize, assert capabilities and
   serverInfo in response, send initialized notification, assert no error.
2. `test_shutdown_exit_clean` -- after initialize, send shutdown (expect
   `null` result), send exit, assert exit code 0.
3. `test_exit_without_shutdown` -- send exit without shutdown, assert exit
   code 1.
4. `test_unknown_method` -- send a request with an unrecognized method,
   assert error code -32601 (method not found).

**Document sync**

5. `test_did_open_publishes_diagnostics` -- open a well-typed `.el` file,
   read the publishDiagnostics notification, assert zero diagnostics.
6. `test_did_open_error_publishes_diagnostics` -- open a file with a type
   error, assert at least one diagnostic with severity 1 (Error).
7. `test_did_change_incremental` -- open a file, apply an incremental edit
   that introduces an error, read new diagnostics.
8. `test_did_close_clears_diagnostics` -- open then close, assert empty
   diagnostic array.

**Edge cases**

9. `test_empty_file` -- open an empty `.el` file, assert zero diagnostics
   and no crash.
10. `test_comment_only_file` -- open a file containing only `;; comment`,
    assert zero diagnostics.
11. `test_malformed_message` -- send raw bytes that are not valid JSON-RPC,
    assert the server does not crash (reads next message).

#### Changes to existing files

- `lib/lsp/rpc.mli` / `rpc.ml`: expose `write_json` (currently not in the
  `.mli`) so `lsp_client.ml` can send raw requests. Alternatively, the
  client module can re-implement the framing (it is only ~10 lines).

### 1.2 UTF-16 Position Correctness

**Goal.** Correctly convert between UTF-16 code-unit offsets (LSP default)
and byte offsets in document text. Add tests for multi-byte characters.

**Current bug.** `server.ml:146` explicitly notes: "UTF-16 code units, but
we approximate with bytes." `document.ml:position_to_offset` treats
`character` as a byte index via `String.split_on_char '\n'` and direct
indexing. Any character outside ASCII (emoji, CJK, accented letters) will
produce wrong ranges.

#### Files to change

| File | Change |
|------|--------|
| `lib/lsp/document.ml` | Rewrite `position_to_offset` to walk UTF-16 code units |
| `lib/lsp/document.mli` | No signature change needed |
| `lib/lsp/server.ml` | Update `range_of_span` to emit UTF-16 characters |
| `lib/lsp/protocol.ml` | Add `general.positionEncodings` parsing in `parse_initialize_params` |
| `lib/lsp/protocol.mli` | Extend `client_capabilities` with `position_encodings` |
| `test/lsp/document_test.ml` | Add multi-byte test cases |

#### Implementation details

1. **`document.ml` -- `utf16_offset_of_byte`**: Given a line string and a
   byte offset, walk the string decoding UTF-8 codepoints and counting
   UTF-16 code units (BMP = 1, supplementary = 2). This converts internal
   byte columns to LSP character offsets.

2. **`document.ml` -- `byte_offset_of_utf16`**: Inverse: given a line
   string and a UTF-16 code-unit count, walk and return the byte offset.
   This converts LSP character positions to byte indices for
   `position_to_offset`.

3. **`server.ml` -- `range_of_span`**: The `Syntax.Location.span` stores
   byte-offset columns. Convert them to UTF-16 using the line text from the
   document store. This requires either passing the document text through or
   looking it up from the store. A clean approach: add a helper
   `range_of_span_in_text text span` that extracts the relevant line and
   converts columns.

4. **Position encoding negotiation**: In the `initialize` response, if the
   client advertises `general.positionEncodings` containing `"utf-32"`,
   prefer it (byte offsets are equivalent to UTF-32 for ASCII-heavy Elisp).
   Store the negotiated encoding in `server.state`. Default remains UTF-16.

#### Test fixtures

Create `test/fixtures/typing/multibyte.el`:
```elisp
;; -*- coding: utf-8 -*-
(defvar emoji-var "Hello 🌍")
(defun greet (name)
  (concat "Hi " name " 你好"))
```

Test that hover at the `n` in `name` on the last line (after multi-byte
chars) returns the correct type and range.

### 1.3 Incremental Sync Recovery

**Goal.** Detect when incremental edits fall outside document bounds and
recover gracefully instead of crashing or silently corrupting state.

**Current behavior.** `document.ml:apply_single_change` returns
`Error "Start position out of range"` which `server.ml:handle_did_change`
logs but otherwise ignores (the document retains its old text). This is
correct for a single bad edit, but if the client and server diverge on
document state, every subsequent edit will also fail silently.

#### Files to change

| File | Change |
|------|--------|
| `lib/lsp/document.ml` | Add version tracking, gap detection |
| `lib/lsp/document.mli` | Expose `version` accessor if needed |
| `lib/lsp/server.ml` | Request full-document resync on divergence |

#### Implementation

1. **Version gap detection**: In `apply_changes`, if `version` is not
   exactly `current_version + 1`, log a warning. This can indicate dropped
   messages.

2. **Graceful recovery**: When `apply_single_change` returns an error, log
   the error at `info` level and request a full document resync. The server
   can do this by sending a `workspace/configuration` request or simply
   waiting for the next full sync (didClose + didOpen). For now, log the
   divergence and continue.

3. **Bounds clamping** (defense-in-depth): In `apply_single_change`, if the
   edit range extends past the document end, clamp to document length rather
   than returning an error. This matches VS Code's behavior for edits at
   EOF.

#### Tests

Add to `test/lsp/document_test.ml`:
- `test_edit_past_end_clamps` -- apply an edit whose end position is past
  EOF, assert it succeeds by clamping.
- `test_version_gap_logged` -- apply changes with a version gap, assert the
  change is applied but a warning is logged.

---

## Phase 2: Concurrency & Responsiveness

### 2.1 Concurrent Request Processing

**Goal.** Make the main loop non-blocking so that long-running operations
(type checking triggered by `didChange`) do not block interactive requests
(hover, completion, signature help).

**Constraints.** The project targets OCaml >= 5.0 but currently uses no
async runtime. Introducing Lwt or Eio is a large dependency change. The
lightest approach is to use OCaml 5 domains for CPU-bound work (type
checking) and keep the main message loop on the main domain.

#### Architecture

```
Main domain (message loop)
  |
  |-- read_message
  |-- dispatch_notification / dispatch_request
  |      |
  |      +-- type-check requests -> spawn on worker domain
  |      +-- hover/completion -> handle inline (fast path)
  |
  |-- write_response / write_notification

Worker domain (type checking)
  |-- runs check_document
  |-- posts result back via Domain.DLS or a shared queue
```

#### Files to change

| File | Change |
|------|--------|
| `lib/lsp/server.ml` | Introduce `worker` module with domain-based type checking |
| `lib/lsp/server.mli` | No public API change |
| `lib/lsp/worker.ml` (new) | Worker domain management |
| `lib/lsp/worker.mli` (new) | Worker interface |

#### Implementation

1. **`worker.ml`**: A single-worker model. One background domain runs type
   checks. A `Mutex`-protected queue of `(uri * version * text)` triples
   feeds work to it. The worker picks the newest entry per URI (coalescing).
   Results are posted to a `Mutex`-protected result queue that the main loop
   polls.

2. **Main loop change**: Replace the blocking `loop ()` in `Server.run`
   with:
   ```
   loop:
     if result_queue has results -> publish diagnostics
     if stdin has data -> read_message, dispatch
     else -> Thread.yield / small sleep
   ```
   Use `Unix.select` on `stdin` with a short timeout (50ms) to multiplex
   between reading messages and checking for results.

3. **Mutable state safety**: The `documents` hashtable and `form_cache` are
   currently accessed only from the main domain. Type checking reads
   document text (passed by value to the worker) and the module config
   (immutable). The worker should NOT touch `documents`, `form_cache`, or
   `dependency_graph` directly. Instead, the main domain:
   - Snapshots the text + config before dispatching to the worker.
   - Receives results and updates caches on the main domain.

4. **Fallback**: If OCaml 5 domains cause issues, fall back to
   `Thread.create` (which is sufficient since the GIL is released during
   I/O, and type checking is CPU-bound). For the initial implementation,
   `Thread` may be simpler and sufficient.

#### Phasing

This is the most complex change. Implement it in stages:
1. First, extract `publish_diagnostics` triggering into a separate
   `schedule_check` function (no concurrency yet, just the abstraction).
2. Then, wire `schedule_check` to a background thread/domain.
3. Finally, make the main loop poll for results.

### 2.2 Request Cancellation

**Goal.** Implement `$/cancelRequest` so the client can abort in-flight
requests.

**Current state.** `server.ml:1366` receives `$/cancelRequest` and ignores
it.

#### Files to change

| File | Change |
|------|--------|
| `lib/lsp/server.ml` | Track in-flight request IDs; handle cancel |
| `lib/lsp/rpc.ml` | No change (error code -32800 already defined as `request_cancelled`) |

#### Implementation

1. Add a `pending_requests : (Yojson.Safe.t, unit -> unit) Hashtbl.t` to
   the server state. Each entry maps a request ID to a cancellation
   callback.

2. When dispatching a request that may be long-running (hover, completion,
   code actions -- anything that type-checks), register it in
   `pending_requests` with a cancellation flag (an `Atomic.t bool`).

3. On `$/cancelRequest`, look up the ID in `pending_requests`. If found,
   set the cancellation flag. The handler periodically checks the flag and
   raises a `Cancelled` exception if set.

4. Catch `Cancelled` in `process_message` and respond with error code
   -32800.

5. **Priority.** The biggest win is cancelling type-check cycles triggered
   by `didChange`. If 2.1 (concurrent processing) is implemented, the
   worker checks the flag between forms. Without 2.1, cancellation has
   limited benefit since the main loop is blocked.

**Depends on:** Ideally 2.1, but can be stubbed independently.

### 2.3 Debounce & Coalescing

**Goal.** Reduce wasted type-checking work during rapid typing.

**Current behavior.** Every `didChange` immediately triggers
`publish_diagnostics`, which calls `check_document`, which runs a full
module check. Rapid typing causes N redundant checks.

#### Files to change

| File | Change |
|------|--------|
| `lib/lsp/server.ml` | Debounce didChange, coalesce diagnostics |

#### Implementation

1. **Version counter**: `Document.doc` already has a `version` field. After
   scheduling a check, record the version. When the check completes,
   compare against the current document version. If stale, discard.

2. **Debounce timer**: After receiving `didChange`, delay the type check by
   200ms (configurable). If another `didChange` arrives before the timer
   fires, reset the timer. Implementation options:
   - With 2.1 (worker domain): the main loop naturally debounces by only
     dispatching the latest queued work.
   - Without 2.1: use `Unix.gettimeofday` to track last-edit timestamps.
     In the main `loop`, before blocking on `Rpc.read_message`, check if
     any debounce timers have expired and run their checks. This requires
     the non-blocking main loop from 2.1.

3. **Diagnostic deduplication**: Before publishing, compare the new
   diagnostic set against the last published set for that URI. If identical,
   skip the publish. Store `last_diagnostics : (string, Protocol.diagnostic
   list) Hashtbl.t` in the server state.

**Depends on:** 2.1 (for timer-based debouncing; without it, at minimum do
version-based staleness checks).

### 2.4 Progress Reporting

**Goal.** Give users visual feedback during long operations.

#### Files to change

| File | Change |
|------|--------|
| `lib/lsp/protocol.ml` | Add `window/workDoneProgress/create` and progress notification types |
| `lib/lsp/protocol.mli` | Expose progress types |
| `lib/lsp/server.ml` | Send progress during initial check and invalidation cascades |

#### Protocol additions in `protocol.ml`

```ocaml
type work_done_progress_begin = {
  title : string;
  message : string option;
  percentage : int option;
  cancellable : bool;
}

type work_done_progress_report = {
  message : string option;
  percentage : int option;
  cancellable : bool;
}

type work_done_progress_end = {
  message : string option;
}
```

Plus JSON encoders and the `window/workDoneProgress/create` request.

#### Implementation

1. Before starting an invalidation cascade affecting N files, send a
   `workDoneProgress/create` request, then `$/progress` begin/report/end
   notifications with "Checking N files" and a percentage.

2. Use a unique token per progress operation (e.g., `"tart-check-<N>"`).

3. If the client supports `window.workDoneProgress` (check
   `client_capabilities`), enable progress. Otherwise skip.

4. Progress operations should be cancellable (ties into 2.2).

**Depends on:** 2.1 (progress is only useful if the main loop can send
progress notifications while work is happening in the background).

---

## Phase 3: File Watching & Configuration

### 3.1 File Watching

**Goal.** Detect when files change outside the editor and update
accordingly.

#### Files to change

| File | Change |
|------|--------|
| `lib/lsp/server.ml` | Handle `workspace/didChangeWatchedFiles` |
| `lib/lsp/protocol.ml` | Add file watcher registration types, parse `didChangeWatchedFiles` |
| `lib/lsp/protocol.mli` | Expose new types |

#### Implementation

1. **Registration**: During `initialize`, add
   `workspace.didChangeWatchedFiles` to `server_capabilities` with a
   `FileSystemWatcher` registration for `**/*.el` and `**/*.tart`.
   Alternatively, dynamically register via
   `client/registerCapability` after `initialized`.

2. **Handling `workspace/didChangeWatchedFiles`**: The notification carries
   an array of `FileEvent` objects, each with a URI and a change type
   (Created = 1, Changed = 2, Deleted = 3).

   For each event:
   - If the URI is open in the editor (exists in `documents`), **ignore**
     it (the editor buffer is the source of truth).
   - If closed:
     - **Created/Changed**: Re-read from disk, update the dependency graph,
       invalidate caches for dependents, re-publish diagnostics for open
       dependents.
     - **Deleted**: Remove from the dependency graph, invalidate dependents.

3. **Protocol types to add**:
   ```ocaml
   type file_change_type = Created | Changed | Deleted
   type file_event = { fe_uri : string; fe_type : file_change_type }
   type did_change_watched_files_params = { changes : file_event list }
   ```

#### Tests (integration)

- Modify a `.tart` file on disk while its dependent `.el` is open. Assert
  the `.el` file gets updated diagnostics.

### 3.2 Workspace Configuration

**Goal.** Support dynamic configuration via `workspace/didChangeConfiguration`.

#### Files to change

| File | Change |
|------|--------|
| `lib/lsp/server.ml` | Handle `workspace/didChangeConfiguration`, store settings |
| `lib/lsp/protocol.ml` | Add configuration types |
| `lib/lsp/protocol.mli` | Expose configuration types |

#### Settings

| Key | Type | Default | Effect |
|-----|------|---------|--------|
| `tart.emacsVersion` | `string` | auto-detect | Override detected Emacs version |
| `tart.searchPath` | `string[]` | `[]` | Additional directories to search for `.tart` files |
| `tart.diagnostics.debounceMs` | `int` | `200` | Debounce interval for type-check after edit |

#### Implementation

1. Add a `settings` record to server state:
   ```ocaml
   type settings = {
     emacs_version_override : Sig.Emacs_version.version option;
     extra_search_paths : string list;
     debounce_ms : int;
   }
   ```

2. On `workspace/didChangeConfiguration`:
   - Parse the new settings.
   - If `emacs_version` changed, rebuild `module_config` (calls
     `detect_emacs_and_build_config` with override).
   - Invalidate all caches.
   - Re-check all open documents.

3. On `initialize`, check `initializationOptions` for initial settings.

4. **No restart required**: the settings are applied dynamically.

### 3.3 Document Save Events

**Goal.** Handle `textDocument/didSave` for full re-checks.

#### Files to change

| File | Change |
|------|--------|
| `lib/lsp/server.ml` | Handle `textDocument/didSave` notification |
| `lib/lsp/protocol.ml` | Add `text_document_sync_options.save` |

#### Implementation

1. Add `save : bool` to `text_document_sync_options` and set it to `true`
   in `capabilities()`.

2. In `dispatch_notification`, add a `"textDocument/didSave"` case.

3. On save:
   - Invalidate the form cache for the document (force full re-check).
   - Re-read non-open files that might have changed (e.g., sibling `.tart`
     from disk).
   - Run `publish_diagnostics` with the current text.

4. This provides a safety net: even if incremental sync has diverged, save
   triggers a fresh check from the canonical editor state.

---

## Phase 4: Navigation & Intelligence

### 4.1 Workspace Symbols

**Goal.** Implement `workspace/symbol` for project-wide symbol search.

#### Files to change

| File | Change |
|------|--------|
| `lib/lsp/protocol.ml` | Add `workspace_symbol_params`, `symbol_information` types |
| `lib/lsp/protocol.mli` | Expose new types |
| `lib/lsp/server.ml` | Add `handle_workspace_symbol`, register capability |

#### Implementation

1. **Capability**: Add `workspace_symbol_provider = true` to
   `server_capabilities`.

2. **Handler**: On `workspace/symbol`, receive a query string. Collect
   symbols from:
   - All open documents (walk parsed sexps via `extract_symbol_from_def`).
   - All loaded signatures from the search path (iterate
     `sig_ast.sig_decls`).

3. **Filtering**: Case-insensitive substring match on the query.

4. **Return type**: `SymbolInformation[]` (flat list with location, not
   hierarchical `DocumentSymbol`).

5. **Protocol additions**:
   ```ocaml
   type workspace_symbol_params = { query : string }
   type symbol_information = {
     si_name : string;
     si_kind : symbol_kind;
     si_location : location;
     si_container_name : string option;
   }
   ```

### 4.2 Semantic Tokens

**Goal.** Implement `textDocument/semanticTokens/full` for rich syntax
highlighting.

#### Files to create

| File | Purpose |
|------|---------|
| `lib/lsp/semantic_tokens.ml` | Token classification and encoding |
| `lib/lsp/semantic_tokens.mli` | Interface |

#### Files to change

| File | Change |
|------|--------|
| `lib/lsp/protocol.ml` | Add semantic token types and legend |
| `lib/lsp/protocol.mli` | Expose semantic token types |
| `lib/lsp/server.ml` | Add handler, register capability |

#### Token types

| Token type | Applied to |
|------------|------------|
| `function` | `defun` names, function calls |
| `variable` | `defvar`/`defconst` names, variable references |
| `macro` | `defmacro` names |
| `parameter` | Function parameters in `defun` arg lists |
| `keyword` | Special forms: `if`, `let`, `cond`, `progn`, etc. |
| `string` | String literals |
| `number` | Integer and float literals |
| `comment` | `;` comments |
| `type` | Type names in `.tart` files |

#### Token modifiers

| Modifier | Applied to |
|----------|------------|
| `definition` | Defining occurrences (the name in `defun`, `defvar`) |
| `declaration` | Declarations in `.tart` files |
| `readonly` | `defconst` bindings |

#### Implementation

1. **`semantic_tokens.ml`**: Walk the parsed AST (`Syntax.Sexp.t list`).
   For each sexp, classify tokens by their syntactic context. The
   typed AST is not needed for basic classification; however, using the
   type environment can distinguish functions from variables.

2. **Encoding**: LSP semantic tokens use a delta-encoded flat array:
   `[deltaLine, deltaStartChar, length, tokenType, tokenModifiers]`.
   Build this array by walking sexps in document order.

3. **Capability registration**: Add `semanticTokensProvider` with the
   token legend to `server_capabilities`.

4. **Delta updates** (`semanticTokens/full/delta`): Defer to a later
   iteration. Full tokens on each request is sufficient initially.

### 4.3 Inlay Hints

**Goal.** Implement `textDocument/inlayHint` for inline type annotations.

#### Files to create

| File | Purpose |
|------|---------|
| `lib/lsp/inlay_hints.ml` | Inlay hint generation |
| `lib/lsp/inlay_hints.mli` | Interface |

#### Files to change

| File | Change |
|------|--------|
| `lib/lsp/protocol.ml` | Add inlay hint types |
| `lib/lsp/protocol.mli` | Expose inlay hint types |
| `lib/lsp/server.ml` | Add handler, register capability |

#### Implementation

1. **Protocol types**:
   ```ocaml
   type inlay_hint_kind = Type | Parameter
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
   ```

2. **Hint generation**: Type-check the document (or use cached results).
   Walk `let`/`let*` bindings and emit a `: <type>` hint after each
   variable name when:
   - The type is non-trivial (not just `'a` or `t`).
   - No sibling `.tart` signature already declares the type.

3. **Parameter name hints**: At call sites, if the function has named
   parameters (from its signature), show parameter names before literal or
   complex arguments.

4. **Suppression**: Skip hints when:
   - The type is `t` (uninformative generic).
   - The binding name already contains the type (e.g., `buffer-name` is
     obviously a string).
   - A `.tart` signature covers the function (explicit types are
     preferred).

### 4.4 Prepare Rename

**Goal.** Implement `textDocument/prepareRename` to validate rename
targets.

#### Files to change

| File | Change |
|------|--------|
| `lib/lsp/server.ml` | Add `handle_prepare_rename` |
| `lib/lsp/protocol.ml` | Add `prepare_rename_params`, `prepare_rename_result` |
| `lib/lsp/protocol.mli` | Expose new types |

#### Implementation

1. **Capability**: Change `rename_provider` from `bool` to the object form
   `{ prepareProvider: true }` in `server_capabilities_to_json`.

2. **Handler**: On `textDocument/prepareRename`:
   - Find the sexp at the cursor position.
   - If it is a `Symbol`, return `{ range, placeholder }` where `range`
     spans the symbol and `placeholder` is its current name.
   - If it is a keyword (`&optional`, `&rest`), string literal, comment,
     or number, return `null` (reject the rename).

3. **Protocol types**:
   ```ocaml
   type prepare_rename_result = {
     pr_range : range;
     pr_placeholder : string;
   }
   ```

### 4.5 Type Definition

**Goal.** Implement `textDocument/typeDefinition` to navigate from a value
to its type definition.

#### Files to change

| File | Change |
|------|--------|
| `lib/lsp/server.ml` | Add `handle_type_definition` |
| `lib/lsp/protocol.ml` | Add params (same structure as definition) |
| `lib/lsp/protocol.mli` | Expose new types |

#### Implementation

1. **Capability**: Add `type_definition_provider = true` to
   `server_capabilities`.

2. **Handler**: Infer the type at the cursor position (reuse
   `type_at_sexp`). If the type is a named type (`TCon name`), look up
   `name` in loaded signatures to find its definition span. Return the
   location, or `null` for primitive types.

3. This is most useful for user-defined types declared with `(tart-type
   ...)` in `.tart` files.

### 4.6 Folding Ranges

**Goal.** Implement `textDocument/foldingRange` for code folding.

#### Files to create

| File | Purpose |
|------|---------|
| `lib/lsp/folding.ml` | Folding range computation |
| `lib/lsp/folding.mli` | Interface |

#### Files to change

| File | Change |
|------|--------|
| `lib/lsp/protocol.ml` | Add folding range types |
| `lib/lsp/protocol.mli` | Expose folding range types |
| `lib/lsp/server.ml` | Add handler, register capability |

#### Implementation

1. **Protocol types**:
   ```ocaml
   type folding_range_kind = FRComment | FRImports | FRRegion
   type folding_range = {
     fr_start_line : int;
     fr_start_character : int option;
     fr_end_line : int;
     fr_end_character : int option;
     fr_kind : folding_range_kind option;
   }
   ```

2. **Folding rules**:
   - **Top-level forms**: Any `defun`, `defvar`, `defconst`, `defmacro`,
     `defcustom` that spans more than one line.
   - **`let`/`let*` bindings**: The binding block folds from `(let` to the
     matching `)`.
   - **Comment blocks**: Consecutive lines starting with `;;` fold together
     (kind = `FRComment`).
   - **Multi-line strings**: String literals spanning multiple lines.

3. **Implementation**: Walk the parsed sexp list. For each sexp, check its
   span. If `start_line < end_line`, emit a folding range. For comments,
   scan the raw text lines since comments are not in the AST.

---

## Dependency Graph

Tasks form a loose DAG. Arrows show hard prerequisites.

```
1.1 Integration Test Harness
 |
 +---> 1.2 UTF-16 Positions
 +---> 1.3 Incremental Sync Recovery
 +---> 2.1 Concurrent Request Processing
         |
         +---> 2.2 Request Cancellation
         +---> 2.3 Debounce & Coalescing
         +---> 2.4 Progress Reporting
         |
         +---> 3.1 File Watching
         +---> 3.2 Workspace Configuration
         +---> 3.3 Document Save Events

4.1 Workspace Symbols       (independent)
4.2 Semantic Tokens          (independent)
4.3 Inlay Hints              (independent)
4.4 Prepare Rename           (independent)
4.5 Type Definition          (independent)
4.6 Folding Ranges           (independent)
```

Phase 4 tasks are all independent of each other and of Phases 2-3. They
depend only on the existing server infrastructure (dispatch, protocol
types). They can be implemented in any order and interleaved with other
work.

## Checklist Summary

| ID | Task | New files | Key changed files | Protocol additions |
|----|------|-----------|-------------------|--------------------|
| 1.1 | Integration test harness | `test/lsp_integration/{lsp_client,integration_test}.{ml,mli}`, `dune` | -- | -- |
| 1.2 | UTF-16 positions | -- | `document.ml`, `server.ml`, `protocol.ml` | `general.positionEncodings` in capabilities |
| 1.3 | Incremental sync recovery | -- | `document.ml`, `server.ml` | -- |
| 2.1 | Concurrent requests | `lib/lsp/worker.{ml,mli}` | `server.ml` | -- |
| 2.2 | Request cancellation | -- | `server.ml` | -- |
| 2.3 | Debounce & coalescing | -- | `server.ml` | -- |
| 2.4 | Progress reporting | -- | `server.ml`, `protocol.ml` | `window/workDoneProgress/*` |
| 3.1 | File watching | -- | `server.ml`, `protocol.ml` | `workspace/didChangeWatchedFiles` |
| 3.2 | Workspace configuration | -- | `server.ml`, `protocol.ml` | `workspace/didChangeConfiguration` |
| 3.3 | Document save events | -- | `server.ml`, `protocol.ml` | `textDocument/didSave` |
| 4.1 | Workspace symbols | -- | `server.ml`, `protocol.ml` | `workspace/symbol` |
| 4.2 | Semantic tokens | `lib/lsp/semantic_tokens.{ml,mli}` | `server.ml`, `protocol.ml` | `textDocument/semanticTokens/full` |
| 4.3 | Inlay hints | `lib/lsp/inlay_hints.{ml,mli}` | `server.ml`, `protocol.ml` | `textDocument/inlayHint` |
| 4.4 | Prepare rename | -- | `server.ml`, `protocol.ml` | `textDocument/prepareRename` |
| 4.5 | Type definition | -- | `server.ml`, `protocol.ml` | `textDocument/typeDefinition` |
| 4.6 | Folding ranges | `lib/lsp/folding.{ml,mli}` | `server.ml`, `protocol.ml` | `textDocument/foldingRange` |
