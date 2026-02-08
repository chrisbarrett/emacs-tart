# Spec 71 — LSP Server

> Consolidates specs: [08](./.archive/08-lsp-server.md), [26](./.archive/26-lsp-signature-sync.md), [27](./.archive/27-dependency-graph.md)

## Overview

The LSP server provides IDE integration via a JSON-RPC stdio transport. It
covers document synchronization, diagnostics, hover, goto definition, find
references, code actions, completion, signature help, document symbols, rename,
signature file tracking, and a dependency graph for incremental invalidation. It
targets Eglot compatibility and maintains sub-second feedback loops: hover
responses arrive within 100 ms and diagnostics publish within 500 ms of the
last edit.

## Protocol & Transport

The server communicates over stdin/stdout using JSON-RPC 2.0 with
`Content-Length` headers. It implements the standard LSP lifecycle:

- **initialize** — returns server capabilities (incremental sync, hover,
  definition, references, code actions, completion, signature help, document
  symbols, rename)
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

## Goto Definition

`textDocument/definition` resolves symbols to their definition location. Two
resolution strategies apply in order:

1. **Local definitions** — `defun`, `defvar`, `defconst` forms in the current
   document.
2. **Signature lookup** — searches sibling `.tart` files, then the configured
   search path (typings, requires, autoloads). Uses prefix-based lookup for
   module-qualified names.

Returns a single location or null.

**Limitation:** does not scan `.el` files other than the current document. A
symbol defined in another `.el` file is only reachable if it has a `.tart`
signature.

## Find References

`textDocument/references` walks the AST of the current document and collects
all symbol occurrences matching the target name.

**Limitation:** current-document only. No workspace-wide search, no scoping
analysis — every textual occurrence of the name in the document is returned
regardless of binding context.

## Code Actions

`textDocument/codeAction` offers three actions:

| Action | Kind | Trigger |
|--------|------|---------|
| Add missing signature | QuickFix | Function lacks a `.tart` declaration |
| Extract function | RefactorExtract | User selects an expression |
| Version constraint fix | QuickFix | E0900/E0901 diagnostics |

**Add missing signature** infers the function's type and proposes inserting a
`defun` declaration into the sibling `.tart` file.

**Extract function** collects free variables in the selection (filtering out
builtins and accounting for binding forms like `let`, `lambda`, `defun`),
generates a new `defun`, and replaces the selection with a call.

**Version constraint fixes** offer three alternatives when a function is
unavailable at the declared minimum Emacs version:

- Bump the `Package-Requires` minimum version.
- Wrap the call in a `(when (fboundp 'fn) ...)` guard.
- Downgrade the minimum version (for removed functions).

All actions produce workspace edits, which may span multiple files.

## Completion

`textDocument/completion` returns prefix-matched candidates drawn from:

1. Local definitions in the current document.
2. Functions and variables from loaded signatures.
3. Bindings from the typed environment (builtins, type-checked names).

Candidates are deduplicated, preferring entries with type information.
Functions show their parameter list as detail text. Completion kinds
distinguish functions, variables, and constants.

**Limitation:** not scope-aware at the cursor — all typed bindings are offered
regardless of local visibility.

## Signature Help

`textDocument/signatureHelp` activates inside function calls. It locates the
enclosing call form, identifies the active argument position, and returns the
function's signature with the active parameter highlighted. Handles
`&optional`, `&rest`, and `&key` parameter markers. Polymorphic types are
unwrapped before display.

## Document Symbols

`textDocument/documentSymbol` returns a hierarchical list of definitions:

| Form | Symbol kind |
|------|-------------|
| `defun` | Function |
| `defvar` | Variable |
| `defconst` | Constant |
| `defmacro` | Method (to distinguish from functions) |

Nested definitions (e.g. a `defun` inside another `defun`) are included as
children. Each symbol carries the range of the whole form and a selection range
over just the name.

## Rename

`textDocument/rename` renames all occurrences of a symbol within the current
document.

**Limitation:** current-document only. Does not rename across the workspace or
in `.tart` signature files.

## Deferred

- **Workspace-wide find-references**: references are currently scoped to the
  open document.
- **Cross-file goto-definition for `.el` targets**: definition resolution
  reaches `.tart` signatures but not other `.el` files.
- **Workspace-wide rename**: rename is currently single-document.
- **Scope-aware completion**: completions are not filtered by lexical scope at
  the cursor.
