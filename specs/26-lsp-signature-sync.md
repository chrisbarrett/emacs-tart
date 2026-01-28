# Spec 26: LSP Signature File Synchronization

**Priority: High**

Keep type checking in sync when editing `.tart` signature files alongside `.el` files.

**Deps:** Spec 08 (LSP server), Spec 27 (dependency graph).

## Problem

Currently, editing a `.tart` file does not trigger re-type-checking of dependent `.el` files. The LSP only re-reads `.tart` files when the `.el` file itself changes.

Workflow that breaks:
1. Open `foo.el` and `foo.tart`
2. Edit `foo.tart` to add/change a type signature
3. `foo.el` still shows stale diagnostics until you touch it

## Goal

Type checking stays current when editing `.tart` files. Changes to signatures immediately reflect in diagnostics for dependent `.el` files.

## Constraints

| Constraint | Detail |
|------------|--------|
| Real-time | Updates within normal LSP latency (~100ms) |
| Buffer-aware | Use buffer contents if `.tart` is open, else disk |
| Minimal overhead | Don't re-check files unnecessarily |

## Approach

Track open `.tart` files via `didOpen`/`didChange`/`didClose`. When a `.tart` file changes, invalidate and re-check all dependent `.el` files.

## Output

```
lib/lsp/
├── server.ml           ; (modify) Handle .tart didOpen/didChange
├── signature_tracker.ml  ; (new) Track open .tart buffers
└── signature_tracker.mli
```

## Requirements

### R1: Track open .tart files

LSP handles `textDocument/didOpen` for `.tart` files:
- Store buffer contents in memory
- Associate with dependent `.el` files (by filename convention: `foo.tart` → `foo.el`)

### R2: Handle .tart didChange

When `.tart` buffer changes:
1. Update stored contents
2. Query dependency graph for dependents (Spec 27)
3. Re-publish diagnostics for each dependent file

### R3: Prefer buffer over disk

When loading a signature:
- If `.tart` is open in LSP → use buffer contents
- Else → read from disk

### R4: Handle .tart didClose

When `.tart` buffer closes:
- Remove from tracked buffers
- Re-check dependents (they'll now read from disk)

### R5: Invalidate form cache

When `.tart` changes, invalidate the form cache for dependent `.el` files (existing `config_hash` mechanism).

### R6: Integrate with dependency graph

Use Spec 27 dependency graph for:
- Finding all dependents of a changed `.tart`
- Handling transitive dependencies
- Core typings invalidation (all `.el` depend on active version)

## Edge Cases

| Case | Behavior |
|------|----------|
| `.tart` open but `.el` not open | No action needed |
| `.tart` saved, buffer differs | Use buffer contents |
| Multiple `.el` depend on one `.tart` | Re-check all |
| `.tart` has parse errors | Keep last valid state, show parse error |

## Tasks

- [ ] Create signature_tracker module
- [ ] Handle `.tart` in didOpen
- [ ] Handle `.tart` in didChange → trigger dependent re-check
- [ ] Handle `.tart` in didClose
- [ ] Modify sig loading to check tracker first
- [ ] Wire up form cache invalidation
- [ ] Test: edit .tart, verify .el diagnostics update
