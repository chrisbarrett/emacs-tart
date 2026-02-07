# Spec 08: LSP Server

Language Server Protocol implementation for IDE integration via eglot.

**Dependencies:** Specs 04-07 complete (parser, interpreter, inference,
signatures).

## Goal

Implement an LSP server that provides type-based IDE features: diagnostics,
hover, and document synchronization.

## Constraints

- **stdio transport**: Communicate via stdin/stdout JSON-RPC
- **Eglot compatible**: Test primarily with eglot in Emacs
- **Latency targets**: Hover <100ms, diagnostics <500ms after edit
- **Resilient**: Partial results on parse/type errors

## Scope

**Phase 1 (this spec):**
- `initialize`/`initialized`/`shutdown`
- `textDocument/didOpen`, `didChange`, `didClose`
- `textDocument/publishDiagnostics`
- `textDocument/hover`

**Phase 2 (future spec):**
- `textDocument/definition`
- `textDocument/references`
- `textDocument/codeAction`

## Output

```
tart/
├── bin/
│   └── main.ml            ; Entry point
├── lib/
│   └── lsp/
│       ├── rpc.ml         ; JSON-RPC protocol
│       ├── protocol.ml    ; LSP message types
│       ├── server.ml      ; Server state and main loop
│       ├── handlers.ml    ; Request/notification handlers
│       └── document.ml    ; Document manager
└── test/
    └── lsp/
        └── lsp_test.ml    ; Protocol tests
```

## Requirements

### R1: JSON-RPC transport

**Given** LSP messages over stdio
**When** the server runs
**Then** it reads `Content-Length` headers, parses JSON-RPC messages, and writes
responses with proper headers

**Verify:** Echo server test; send request, receive response with matching id

### R2: Initialize handshake

**Given** an `initialize` request with client capabilities
**When** processed
**Then** server responds with capabilities:
```json
{
  "capabilities": {
    "textDocumentSync": { "openClose": true, "change": 2 },
    "hoverProvider": true
  }
}
```

**Verify:** Eglot connects successfully; `M-x eglot` shows "Connected"

### R3: Document synchronization

**Given** `didOpen`, `didChange`, `didClose` notifications
**When** received
**Then** the document manager:
- Stores full text on `didOpen`
- Applies incremental changes on `didChange` (sync kind 2)
- Removes document on `didClose`

**Verify:** Edit document in Emacs; server sees updated content

### R4: Publish diagnostics

**Given** a document with type errors
**When** it changes (debounced, ~200ms)
**Then** server sends `textDocument/publishDiagnostics` with:
- URI of the document
- Array of diagnostics, each with range, severity, message

**Verify:** Type error in buffer shows in flymake; fixing it clears diagnostic

### R5: Diagnostic ranges

**Given** a type error at a specific location
**When** the diagnostic is generated
**Then** the range accurately spans the problematic expression

**Verify:** Diagnostic underline matches the error location precisely

### R6: Hover on expressions

**Given** a hover request at a position
**When** processed
**Then** server returns the inferred type at that position:
```json
{
  "contents": {
    "kind": "markdown",
    "value": "```elisp\n(-> (Int Int) Int)\n```"
  }
}
```

**Verify:** Hover over function shows its type; hover over variable shows type

### R7: Hover on function calls

**Given** a hover request on a function name in a call
**When** processed
**Then** server returns the function's type (instantiated if polymorphic)

**Verify:** Hover on `mapcar` in `(mapcar #'1+ xs)` shows instantiated type

### R8: Hover fallback for errors

**Given** a hover request in code with type errors
**When** processed
**Then** server returns best-effort type (partial inference) or "unknown"

**Verify:** Hover still works in files with type errors

### R9: Incremental type checking

**Given** edits to a function body
**When** diagnostics are recomputed
**Then** only affected functions are re-type-checked (query-based cache)

**Verify:** Edit one function; other functions' types not recomputed (log check)

### R10: Graceful degradation

**Given** a file that fails to parse
**When** LSP features are requested
**Then** server provides:
- Parse errors as diagnostics
- No hover (or "parse error" message)
- Does not crash

**Verify:** Malformed file shows parse errors; hover returns gracefully

### R11: Logging and debugging

**Given** the server is running
**When** `--log-level=debug` is passed
**Then** server logs: incoming messages, outgoing responses, timing info

**Verify:** Logs help diagnose issues; can be disabled for performance

## Tasks

- [x] [R1] Implement JSON-RPC reader/writer
- [x] [R2] Handle initialize/initialized/shutdown
- [x] [R3] Implement document manager with incremental sync
- [x] [R4] Implement publishDiagnostics on document change
- [x] [R5] Ensure diagnostic ranges are accurate
- [x] [R6] Implement textDocument/hover
- [x] [R7] Show instantiated types on hover
- [x] [R8] Handle hover gracefully on errors
- [x] [R9] Add query-based caching for incrementality
- [x] [R10] Handle parse failures gracefully
- [x] [R11] Add logging with configurable levels

Run review agent after hover works in eglot before tagging v1.0-alpha.
