# Spec 71 — LSP Server

> Consolidates specs: [08](./.archive/08-lsp-server.md), [26](./.archive/26-lsp-signature-sync.md), [27](./.archive/27-dependency-graph.md)

## Overview

The LSP server provides IDE integration via a JSON-RPC stdio transport, with
document synchronization, diagnostics, hover, signature file tracking, and a
dependency graph for incremental invalidation. It targets Eglot compatibility
and maintains sub-second feedback loops: hover responses arrive within 100 ms
and diagnostics publish within 500 ms of the last edit.

## Protocol & Transport

The server communicates over stdin/stdout using JSON-RPC 2.0 with
`Content-Length` headers. It implements the standard LSP lifecycle:

- **initialize** — returns server capabilities (incremental sync kind 2,
  hover provider)
- **initialized** — no-op acknowledgement
- **shutdown** / **exit** — orderly teardown

Logging is configurable via `--log-level`. At `debug` level the server records
incoming messages, outgoing responses, and timing information.

## Document Synchronization

The server tracks both `.el` and `.tart` files via three notifications:

| Notification | `.el` behaviour | `.tart` behaviour |
|--------------|-----------------|-------------------|
| `didOpen`    | Store full text, run initial type check | Store buffer contents, associate with dependents |
| `didChange`  | Apply incremental edits (sync kind 2), schedule re-check | Update stored contents, invalidate dependents |
| `didClose`   | Remove from open set | Fall back to disk contents, re-check dependents |

Buffer contents always take precedence over the on-disk file when a document is
open.

## Diagnostics

On each document change (debounced ~200 ms) the server sends
`textDocument/publishDiagnostics` containing an array of diagnostics, each with
a range, severity, and message. Ranges accurately span the problematic
expression.

Parse errors produce diagnostics without crashing the server. A `.tart` file
with parse errors retains its last valid state while the parse error itself
appears as a diagnostic.

## Hover

`textDocument/hover` returns the inferred type at the cursor position, rendered
as Markdown. Polymorphic types are instantiated at the call site. If the file
contains type errors, hover returns a best-effort partial type or an "unknown"
fallback. A query-based cache ensures only affected functions are
re-type-checked after edits.

## Signature File Tracking

`.tart` files are first-class LSP documents. When a `.tart` buffer changes the
server:

1. Updates its in-memory contents.
2. Invalidates the form cache for every dependent `.el` file (via
   `config_hash`).
3. Re-publishes diagnostics for each open dependent.

If the `.tart` file is open, its buffer contents are used for type checking; if
closed, the server reads from disk.

## Dependency Graph

A live graph tracks what depends on each module, enabling targeted
invalidation.

### Edge types

| Kind | Source | Example |
|------|--------|---------|
| Require | `.el` | `(require 'foo)` |
| Autoload | `.el` | `(autoload 'fn "bar")` |
| Open | `.tart` | `(open 'seq)` |
| Include | `.tart` | `(include 'dash)` |
| Sibling | implicit | `foo.el` implies `foo.tart` |

### Indexes

The graph maintains both a forward index (module to its dependencies) and a
reverse index (module to its dependents). Both indexes update incrementally on
`didOpen`, `didChange`, and `didClose`.

### Invalidation cascade

When module X changes:

1. Compute `direct_dependents(X)` from the reverse index.
2. Invalidate caches for each dependent.
3. Re-publish diagnostics for each open dependent.

A **core typings pseudo-module** (`typings/emacs/{version}/*`) is an implicit
dependency of every `.el` file. A change to any core typing invalidates all
open `.el` files.

### Cycle detection

Dependency cycles in `.tart` files are reported as errors. Cycles in `.el`
files are reported as warnings.

## Deferred

- **LSP Phase 2** (goto-definition, find-references, code actions): scoped
  out.
