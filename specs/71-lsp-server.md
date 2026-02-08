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

`textDocument/definition` resolves symbols to their definition location. Three
resolution strategies apply in order:

1. **Local definitions** — `defun`, `defvar`, `defconst` forms in the current
   document.
2. **Signature lookup** — searches sibling `.tart` files, then the configured
   search path (typings, requires, autoloads). Uses prefix-based lookup for
   module-qualified names.
3. **Cross-file `.el` lookup** — searches all other open `.el` documents for
   `defun`, `defvar`, `defconst` definitions matching the target name.

Returns a single location or null. Local definitions take priority over
signature results, which take priority over cross-file `.el` results.

## Find References

`textDocument/references` walks the AST of all open `.el` documents and collects
symbol occurrences matching the target name. Results from the origin document
appear first.

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

Completions are scope-aware: bindings from `let`, `let*`, `lambda`, `defun`,
`if-let`, `dolist`, and `dotimes` are only offered when the cursor is within
their lexical scope. `let*` respects sequential visibility.

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

`textDocument/rename` renames all occurrences of a symbol across the workspace.
References are found via workspace-wide search across all open `.el` documents.
Results are grouped by URI into separate text document edits.

## Concurrency & Cancellation

The server processes requests concurrently. Long-running operations (type
checking triggered by `didChange`, workspace-wide symbol search) run on
background fibers. The main loop remains responsive to new requests while
background work executes.

`$/cancelRequest` cancels in-flight requests. When a request is cancelled, any
in-progress computation is abandoned and the server responds with error code
`-32800`. Rapid successive `didChange` notifications coalesce: only the latest
document version triggers a type-check cycle. Intermediate versions are skipped.

A version counter on each document tracks edit sequence. Before publishing
diagnostics, the server verifies the document version hasn't advanced; stale
results are discarded silently.

## Progress Reporting

Long-running operations emit `$/progress` notifications using work-done progress
tokens. Two categories:

| Operation           | Token pattern         | Granularity                  |
| ------------------- | --------------------- | ---------------------------- |
| Initial type-check  | `tart/initialCheck`   | Per-file progress (N of M files) |
| Invalidation cascade| `tart/recheck`        | Per-dependent progress       |

The server sends `window/workDoneProgress/create` before the first progress
notification. Each sequence includes `begin`, zero or more `report`, and `end`
messages. Cancellable operations set `cancellable = true`.

## File Watching

The server registers for `workspace/didChangeWatchedFiles` notifications
covering `**/*.el` and `**/*.tart` patterns. When an external change is
detected:

1. If the file is open in the editor, the notification is ignored (buffer
   contents take precedence).
2. If the file is closed, the server re-reads from disk, updates the dependency
   graph, and re-publishes diagnostics for affected dependents.

This handles branch switches, code generation, `git stash pop`, and other
out-of-editor file mutations.

## Workspace Configuration

The server supports `workspace/didChangeConfiguration` notifications.
Configurable settings:

| Setting                             | Type       | Default       | Effect                                        |
| ----------------------------------- | ---------- | ------------- | --------------------------------------------- |
| `tart.emacsVersion`                 | `string`   | Auto-detected | Target Emacs version for type checking        |
| `tart.searchPath`                   | `string[]` | `[]`          | Additional directories for signature lookup   |
| `tart.diagnostics.debounceMs`       | `number`   | `200`         | Debounce interval for diagnostic publishing   |

When configuration changes, the server invalidates affected caches and
re-checks open documents against the new settings without requiring a restart.

## Workspace Symbols

`workspace/symbol` returns symbols matching a query string across all open
documents and loaded signatures. Results include:

- `defun`, `defvar`, `defconst`, `defmacro` forms from open `.el` files.
- Declarations from loaded `.tart` signature files.

Matching is case-insensitive prefix-based. Each result includes the symbol's
kind, location, and containing module name.

## Semantic Tokens

`textDocument/semanticTokens/full` returns token classifications for the entire
document. The server advertises the following token types:

| Token type  | Applied to                                                   |
| ----------- | ------------------------------------------------------------ |
| `function`  | Function names in `defun` forms and call positions          |
| `variable`  | Variable names in `defvar`/`defconst` and reference positions |
| `macro`     | Macro names in `defmacro` forms and call positions          |
| `parameter` | Lambda and function parameters                               |
| `keyword`   | Special forms (`if`, `let`, `progn`, `setq`, etc.)           |
| `string`    | String literals                                              |
| `number`    | Numeric literals                                             |
| `comment`   | Comments                                                     |
| `type`      | Type names in `.tart` files                                  |

Token modifiers: `definition`, `declaration`, `readonly`.

Delta updates via `textDocument/semanticTokens/full/delta` reduce payload size
after incremental edits. The server caches the previous token array and result
ID per document; delta responses use prefix/suffix matching to produce a minimal
edit list. On cache miss the server falls back to a full response.

## Inlay Hints

`textDocument/inlayHint` returns inline type annotations for:

- `let`/`let*` bindings — shows inferred type after the variable name.
- Function parameters at call sites — shows parameter name when the argument is
  a literal or complex expression.
- `defun` return types — shows inferred return type when no `.tart` signature
  exists.

Hints are non-intrusive: they are suppressed when an explicit `.tart` signature
already provides the type information, and when the type is trivially obvious
(e.g., a string literal bound to a variable).

## Prepare Rename

`textDocument/prepareRename` validates that the cursor is on a renameable symbol
before the client opens the rename dialog. Returns the range of the symbol and
its current name, or an error if the position is not renameable (e.g., on a
keyword, inside a string literal, or on a built-in).

## Type Definition

`textDocument/typeDefinition` navigates from a value to the definition of its
type. If the type checker resolves a symbol to a named type (e.g., a struct or
an opaque type defined in a `.tart` file), the server returns the location of
that type's declaration.

Returns null for primitive types (`integer`, `string`, `symbol`, etc.) that
have no user-visible definition.

## Folding Ranges

`textDocument/foldingRange` returns fold regions for:

- Top-level forms (`defun`, `defvar`, `defconst`, `defmacro`, `defcustom`).
- `let`/`let*` binding blocks.
- Comment blocks (consecutive `;`-prefixed lines).
- String literals spanning multiple lines.

## Document Save Events

The server handles `textDocument/didSave` notifications. On save:

1. Runs a full (non-incremental) type-check pass for the saved document.
2. Re-reads any non-open files that may have changed on disk (e.g., generated
   files).
3. Publishes updated diagnostics.

The full check on save catches issues that incremental checking may miss due to
stale partial state.

## Robustness

**UTF-16 position encoding.** Position arithmetic correctly handles multi-byte
characters (emoji, CJK, combining characters) by converting between UTF-8 byte
offsets and UTF-16 code units at the protocol boundary. The server advertises
`utf-16` as its position encoding (or negotiates `utf-32` if the client supports
`positionEncoding`).

**Incremental sync recovery.** If the server detects that its document state is
inconsistent (e.g., an edit range falls outside the document bounds), it logs a
warning and requests the client to re-send the full document by closing and
reopening. This prevents silent state divergence.

**Debounce and coalescing.** Rapid `didChange` notifications within the
debounce window are coalesced. The server tracks a pending-check flag per
document and only starts a new type-check cycle when the debounce timer expires.
If a check is already running for a document, it is cancelled before starting a
new one.

**Identical diagnostics suppression.** Before publishing diagnostics, the server
compares the new diagnostic set against the last published set for that
document. If they are identical, the publish is suppressed to avoid unnecessary
client-side rendering.

## Testing

The LSP server has a dedicated integration test suite that exercises the
protocol layer end-to-end:

- **Protocol tests** send raw JSON-RPC messages over a pipe and assert response
  structure, error codes, and notification content.
- **Document sync tests** verify that incremental edits produce correct document
  state, including multi-byte character handling.
- **Diagnostic tests** confirm that editing a file produces expected diagnostics
  and that fixes clear them.
- **Navigation tests** verify hover, goto-definition, references, and workspace
  symbols return correct locations.
- **Cancellation tests** confirm that cancelled requests return error code
  `-32800` and that in-progress work is abandoned.
- **Regression tests** cover known edge cases: empty files, files with only
  comments, malformed JSON-RPC, oversized messages, and rapid open/close cycles.

## Call Hierarchy

`textDocument/prepareCallHierarchy` finds the `defun` at the cursor position.
`callHierarchy/incomingCalls` searches all open `.el` documents for functions
that call the target. `callHierarchy/outgoingCalls` extracts callee names from
the target function's body and resolves them to `defun` definitions across the
workspace.

## Type Hierarchy

`textDocument/prepareTypeHierarchy` infers the type at the cursor and returns a
type hierarchy item. `typeHierarchy/supertypes` and `typeHierarchy/subtypes`
implement the numeric tower (`Int <: Num`, `Float <: Num`). Source locations
from `.tart` signatures are included when available.

## Code Lens

`textDocument/codeLens` produces two lenses per top-level definition (`defun`,
`defvar`, `defconst`):

1. **Reference count** — workspace-wide count of symbol references.
2. **Signature status** — whether a `.tart` signature exists for the definition.

## Linked Editing Ranges

`textDocument/linkedEditingRange` returns linked ranges for variables bound by
`let`, `let*`, `defun`, `defmacro`, `cl-defun`, `lambda`, and `closure` forms.
The binding site and all body references are linked, enabling simultaneous
rename. Markers (`&rest`, `&optional`) are excluded.

## On-Type Formatting

`textDocument/onTypeFormatting` triggers on `)` and newline:

- **Newline**: inserts indentation proportional to paren nesting depth (2 spaces
  per level). No edit at top level.
- **Close paren**: inserts a trailing newline when a top-level form closes. No
  edit for nested close parens.

## Pull-Based Diagnostics

`textDocument/diagnostic` provides pull-based diagnostics alongside the existing
push model. Unchanged reports are returned when the `previousResultId` matches
the cached result, avoiding redundant type-checking. Full reports include a
unique `resultId` for subsequent delta requests.

## Dynamic Registration

The server dynamically registers capabilities when the client advertises
`dynamicRegistration` support. Static capabilities are omitted from the
`initialize` response for features the client will register dynamically. A
`client/registerCapability` request is sent during `initialized` with
appropriate `registerOptions` (trigger characters, token legends, etc.).
