# LSP Patterns for Type Checkers

This document analyses Language Server Protocol patterns relevant to building a
type checker for Emacs Lisp, with particular focus on Eglot integration.

## 1. LSP Fundamentals

### Protocol Overview

The Language Server Protocol (LSP) is a JSON-RPC based protocol for
communication between editors (clients) and language servers. The protocol
standardizes IDE functionality like completions, hover information, diagnostics,
and navigation.

Key characteristics:
- **Transport**: JSON-RPC 2.0 over stdio or TCP
- **Asynchronous**: Clients can continue editing while requests are pending
- **Capability-based**: Servers advertise supported features during initialization

### Core Message Types

```
┌─────────────────────────────────────────────────────────────────┐
│                        LSP Messages                              │
├──────────────────┬──────────────────┬───────────────────────────┤
│ Requests         │ Responses        │ Notifications             │
│ (client→server)  │ (server→client)  │ (either direction)        │
├──────────────────┼──────────────────┼───────────────────────────┤
│ textDocument/    │ Result or Error  │ textDocument/             │
│   hover          │                  │   publishDiagnostics      │
│   definition     │                  │   didOpen                 │
│   references     │                  │   didChange               │
│   codeAction     │                  │   didSave                 │
│   completion     │                  │   didClose                │
│   inlayHint      │                  │ window/showMessage        │
└──────────────────┴──────────────────┴───────────────────────────┘
```

### Capability Matrix for Type Checkers

| Capability | Priority | Purpose for Type Checker |
|------------|----------|--------------------------|
| `textDocument/publishDiagnostics` | Essential | Report type errors |
| `textDocument/hover` | Essential | Display inferred types |
| `textDocument/definition` | High | Jump to type definitions |
| `textDocument/references` | High | Find usages for refactoring |
| `textDocument/codeAction` | High | Provide quick fixes |
| `textDocument/inlayHint` | Medium | Show inline type annotations |
| `textDocument/completion` | Medium | Type-aware completions |
| `textDocument/signatureHelp` | Medium | Function signature display |
| `textDocument/semanticTokens` | Low | Type-aware highlighting |

### JSON-RPC Message Format

Request example:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "textDocument/hover",
  "params": {
    "textDocument": { "uri": "file:///path/to/file.el" },
    "position": { "line": 10, "character": 5 }
  }
}
```

Response example:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "contents": {
      "kind": "markdown",
      "value": "```elisp\n(-> Integer Integer Integer)\n```\nAdd two integers."
    },
    "range": {
      "start": { "line": 10, "character": 1 },
      "end": { "line": 10, "character": 2 }
    }
  }
}
```

## 2. Incremental Analysis

### The Incremental Challenge

Type checking an entire codebase on every keystroke is infeasible. LSP servers
must answer queries quickly (ideally <100ms) while maintaining correctness.

### Strategies for Incremental Type Checking

#### Query-Based Architecture (Recommended)

The query-based model, pioneered by rust-analyzer using the Salsa framework,
treats compilation as a set of memoized pure functions:

```
┌──────────────────────────────────────────────────────────────────┐
│                    Query Graph Example                            │
├──────────────────────────────────────────────────────────────────┤
│                                                                   │
│    [source_text]──────►[parse]──────►[resolve]──────►[typecheck] │
│         │                  │             │                │       │
│    (input)            (cached)       (cached)         (cached)    │
│                                                                   │
│  Changing source_text invalidates parse and downstream queries.  │
│  But if parse result is unchanged (e.g., whitespace-only edit),  │
│  the early cutoff optimization preserves resolve/typecheck.      │
│                                                                   │
└──────────────────────────────────────────────────────────────────┘
```

Key principles:
1. **Queries are pure functions**: `K -> V` with no side effects
2. **Results are memoized**: Avoid recomputation when inputs unchanged
3. **Early cutoff**: If a query produces the same result as before, don't
   invalidate downstream queries
4. **Lazy invalidation**: Mark queries as "maybe stale" rather than eagerly
   recomputing

#### Durability System

Different inputs change at different frequencies:

| Durability Level | Example | Update Frequency |
|------------------|---------|------------------|
| Low | User's current buffer | Every keystroke |
| Medium | Other project files | On save |
| High | Standard library | Never during session |

rust-analyzer uses this to avoid checking stdlib queries when user code changes.

#### Practical Implementation for Elisp

For an Elisp type checker, the query structure might be:

```
source_text(file) → String                    ; Input
parsed_forms(file) → List<Form>               ; Memoized
resolved_symbols(file) → SymbolTable          ; Memoized
inferred_types(file) → TypeMap                ; Memoized
diagnostics(file) → List<Diagnostic>          ; Memoized
```

The key invariant to maintain: **typing inside a function body should not
invalidate type information for other functions**.

### Document Synchronization

LSP provides two synchronization modes:

1. **Full sync**: Client sends entire document on every change
2. **Incremental sync**: Client sends only changed ranges

For a type checker, incremental sync is preferred but requires maintaining
document state server-side.

```
┌────────────────────────────────────────────────────────────────┐
│              Document Lifecycle                                 │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  didOpen ──► didChange* ──► didSave ──► didClose               │
│     │            │             │                                │
│     ▼            ▼             ▼                                │
│  Parse &     Reparse      Trigger full                          │
│  typecheck   incrementally analysis if                          │
│  file        (debounced)  needed                                │
│                                                                 │
└────────────────────────────────────────────────────────────────┘
```

### Debouncing and Batching

Recommendations:
- Debounce `didChange` notifications (100-200ms typical)
- Batch diagnostic updates to avoid flickering
- Cancel in-progress analysis when new changes arrive

### Cancellation

LSP supports request cancellation via `$/cancelRequest`. Essential for:
- Cancelling slow hover requests when user moves cursor
- Abandoning analysis when document changes mid-computation

## 3. Type Information Display

### Hover Formatting

The hover response supports markdown content:

```json
{
  "contents": {
    "kind": "markdown",
    "value": "```elisp\n(defun add (x y)\n  \"Add two numbers.\"\n  ...)\n```\n\n**Type**: `(-> Number Number Number)`\n\n---\nDefined in `my-package.el:42`"
  }
}
```

Recommended hover structure for type information:

```markdown
```elisp
(signature-preview)
```

**Type**: `type-signature`

Documentation string if available.

---
Defined in `location`
```

### Inlay Hints

Inlay hints display inline annotations. For a type checker, useful for:
- Parameter names at call sites
- Inferred types for `let` bindings
- Return type annotations

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "textDocument/inlayHint",
  "params": {
    "textDocument": { "uri": "file:///path.el" },
    "range": {
      "start": { "line": 0, "character": 0 },
      "end": { "line": 50, "character": 0 }
    }
  }
}
```

Response:
```json
{
  "result": [
    {
      "position": { "line": 5, "character": 12 },
      "label": ": Integer",
      "kind": 1,
      "paddingLeft": true
    }
  ]
}
```

Inlay hint kinds:
- `1` = Type
- `2` = Parameter

### Signature Help

For function calls, provide parameter information:

```json
{
  "signatures": [
    {
      "label": "(mapcar FUNCTION SEQUENCE)",
      "documentation": "Apply FUNCTION to each element of SEQUENCE.",
      "parameters": [
        {
          "label": "FUNCTION",
          "documentation": "(-> A B) - Function to apply"
        },
        {
          "label": "SEQUENCE",
          "documentation": "(List A) - Input sequence"
        }
      ]
    }
  ],
  "activeSignature": 0,
  "activeParameter": 0
}
```

## 4. Error Reporting via LSP

### Diagnostic Structure

```json
{
  "method": "textDocument/publishDiagnostics",
  "params": {
    "uri": "file:///path/to/file.el",
    "version": 5,
    "diagnostics": [
      {
        "range": {
          "start": { "line": 10, "character": 5 },
          "end": { "line": 10, "character": 15 }
        },
        "severity": 1,
        "code": "E001",
        "codeDescription": {
          "href": "https://docs.example.com/errors/E001"
        },
        "source": "tart",
        "message": "Type mismatch: expected Integer, got String",
        "relatedInformation": [
          {
            "location": {
              "uri": "file:///path/to/file.el",
              "range": {
                "start": { "line": 5, "character": 0 },
                "end": { "line": 5, "character": 20 }
              }
            },
            "message": "Variable declared as Integer here"
          }
        ],
        "data": {
          "fixId": "type-cast-001"
        }
      }
    ]
  }
}
```

### Diagnostic Severity Levels

| Value | Severity | Use Case |
|-------|----------|----------|
| 1 | Error | Type mismatches, undefined symbols |
| 2 | Warning | Unused variables, deprecated functions |
| 3 | Information | Suggestions, style hints |
| 4 | Hint | Subtle improvements |

### Related Information

Use `relatedInformation` to show:
- Where a type was declared
- Where conflicting types originated
- Other locations involved in the error

This is particularly valuable for type errors where the mismatch involves
multiple source locations.

### Code Actions and Quick Fixes

Code actions provide automated fixes for diagnostics:

```json
{
  "method": "textDocument/codeAction",
  "params": {
    "textDocument": { "uri": "file:///path.el" },
    "range": { ... },
    "context": {
      "diagnostics": [ ... ],
      "only": ["quickfix"]
    }
  }
}
```

Response:
```json
{
  "result": [
    {
      "title": "Add type annotation",
      "kind": "quickfix",
      "diagnostics": [ ... ],
      "edit": {
        "changes": {
          "file:///path.el": [
            {
              "range": { ... },
              "newText": "(declare (type Integer x))"
            }
          ]
        }
      }
    }
  ]
}
```

Code action kinds for type checkers:
- `quickfix` - Fix type errors
- `refactor.rewrite` - Transform code structure
- `source.fixAll` - Fix all auto-fixable issues

### Diagnostic Formatting Recommendations

1. **Be specific**: "Expected Integer but got String" not "Type error"
2. **Show both types**: In mismatches, show expected AND actual
3. **Use related information**: Link to declaration sites
4. **Provide codes**: Enable documentation lookup and filtering
5. **Include fix data**: Preserve info needed for code actions in `data` field

## 5. Eglot-Specific Considerations

### Eglot Philosophy

Eglot differs from lsp-mode in key ways:

| Aspect | Eglot | lsp-mode |
|--------|-------|----------|
| Design | Minimal, built-in | Feature-rich, external |
| Dependencies | None (uses Emacs builtins) | Many external packages |
| UI | Flymake, ElDoc, Xref | Custom UI components |
| Configuration | Minimal | Extensive |
| Multi-server | Via multiplexer (rass) | Native support |

### Integration Points

Eglot delegates to built-in Emacs systems:

| Feature | Eglot Delegates To |
|---------|-------------------|
| Diagnostics | Flymake |
| Documentation | ElDoc |
| Navigation | Xref |
| Completions | completion-at-point |
| Project management | project.el |

### Capabilities to Prioritize

For Eglot integration, prioritize:

1. **publishDiagnostics** - Primary error display via Flymake
2. **textDocument/hover** - ElDoc integration for type display
3. **textDocument/definition** - Xref for navigation
4. **textDocument/references** - Xref for find-references
5. **textDocument/codeAction** - Quick fixes appear in Flymake annotations

### Eglot-Specific Features (1.19+)

Recent Eglot versions add:
- **Inlay hints**: `eglot-inlay-hints-mode` (off by default)
- **Semantic tokens**: `eglot-semantic-tokens-mode` (on by default in 1.19)
- **Type/call hierarchies**: `eglot-show-type-hierarchy`

Enable inlay hints by default:
```elisp
(add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)
```

### Workspace Configuration

Servers can receive configuration via `workspace/configuration`:

```elisp
(setq-default eglot-workspace-configuration
  '(:tart (:strictMode t
           :inferTypes t
           :checkOnSave t)))
```

### Server Registration

Register the type checker with Eglot:

```elisp
(add-to-list 'eglot-server-programs
             '(emacs-lisp-mode . ("tart-lsp" "--stdio")))
```

### Performance Considerations

Eglot users expect:
- Fast startup (server should initialize quickly)
- Responsive hover (<100ms)
- Non-blocking diagnostics
- Low memory footprint

### Handling Elisp-Specific Challenges

The Elisp environment presents unique challenges:

1. **Dynamic scope**: Variables can be rebound; types may vary by call site
2. **Buffer-local state**: Same variable, different types per buffer
3. **Macros**: Expand before type checking
4. **Load-time evaluation**: Code runs when loaded

Recommendations:
- Start with open files, expand to project scope
- Cache macro expansions
- Treat dynamic variables as having declared types (accept unsoundness)
- Provide clear diagnostics for dynamic-scope-related issues

## Architecture Recommendations

### Recommended Server Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Type Checker LSP Server                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐       │
│  │   JSON-RPC   │────│   Dispatch   │────│   Handlers   │       │
│  │   Transport  │    │   Router     │    │              │       │
│  └──────────────┘    └──────────────┘    └──────────────┘       │
│                                                 │                │
│                           ┌─────────────────────┴──────┐        │
│                           ▼                            ▼        │
│                    ┌──────────────┐          ┌──────────────┐   │
│                    │   Document   │          │    Query     │   │
│                    │   Manager    │          │   Database   │   │
│                    └──────────────┘          └──────────────┘   │
│                           │                         │           │
│                           ▼                         ▼           │
│                    ┌──────────────┐          ┌──────────────┐   │
│                    │   Virtual    │◄────────►│    Type      │   │
│                    │   File Sys   │          │   Checker    │   │
│                    └──────────────┘          └──────────────┘   │
│                                                     │           │
│                                              ┌──────┴──────┐    │
│                                              ▼             ▼    │
│                                        ┌─────────┐  ┌─────────┐ │
│                                        │ Parser  │  │ Stdlib  │ │
│                                        │         │  │  Types  │ │
│                                        └─────────┘  └─────────┘ │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Implementation Phases

**Phase 1: Minimal Viable LSP**
- Initialize/shutdown handshake
- Document synchronization (didOpen, didChange, didClose)
- Publish diagnostics for type errors
- Basic hover for type display

**Phase 2: Navigation**
- Go to definition (xref)
- Find references
- Symbol search

**Phase 3: Rich Features**
- Code actions and quick fixes
- Inlay hints for type annotations
- Signature help
- Semantic tokens

**Phase 4: Advanced**
- Workspace-wide analysis
- Rename refactoring
- Code lens
- Call/type hierarchies

### Technology Choices

For Elisp implementation:
- Consider Rust with `tower-lsp` or `lsp-server` crates
- Alternatively, implement in Elisp itself (dogfooding)
- Use Salsa or similar for incremental computation

For Elisp-native implementation:
- Batch processing via Emacs subprocess
- Communication via JSONRPC (Emacs has `jsonrpc.el`)
- Leverage existing Elisp tooling (byte-compiler, macroexpand)

## References

- [LSP Specification 3.17](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
- [Eglot Manual](https://joaotavora.github.io/eglot/)
- [rust-analyzer Architecture](https://rust-analyzer.github.io/blog/2023/07/24/durable-incrementality.html)
- [Salsa Incremental Computation](https://docs.rs/rust-analyzer-salsa/latest/salsa/)
- [ty: Fast Python Type Checker](https://astral.sh/blog/ty)
