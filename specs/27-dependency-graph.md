# Spec 27: Module Dependency Graph

**Priority: High**

Track module dependencies for incremental re-checking.

**Deps:** Spec 07 (signatures), Spec 12 (modules). **Used by:** Spec 26.

## Goal

Live graph answering: what depends on module X? (for invalidation on change)

## Edge Types

| Kind | Source | Example |
|------|--------|---------|
| Require | `.el` | `(require 'foo)` |
| Autoload | `.el` | `(autoload 'fn "bar")` |
| Open | `.tart` | `(open 'seq)` |
| Include | `.tart` | `(include 'dash)` |
| Sibling | implicit | `foo.el` → `foo.tart` |

## Data Model

```ocaml
type module_id = string  (* "foo" or "typings/emacs/31.0/data" *)
type edge_kind = Require | Autoload | Open | Include | Sibling

type t = {
  forward: (module_id, module_id list) Hashtbl.t;
  reverse: (module_id, module_id list) Hashtbl.t;  (* dependents *)
}

val direct_dependents : t -> module_id -> module_id list
val dependents : t -> module_id -> module_id list  (* transitive *)
```

## Output

```
lib/graph/
├── dependency_graph.ml
├── dependency_graph.mli
├── graph_builder.ml      ; Extract deps from parsed files
└── graph_builder.mli
```

## Requirements

### R1: Extract from .el

`(require 'foo)` → Require edge. `(autoload 'fn "bar")` → Autoload edge.

### R2: Extract from .tart

`(open 'seq)` → Open edge. `(include 'dash)` → Include edge.

### R3: Sibling edge

`foo.el` → implicit edge to `foo.tart` if exists.

### R4: Incremental updates

| Event | Action |
|-------|--------|
| didOpen | Parse, add edges, update reverse index |
| didChange | Re-extract, diff edges, update index |
| didClose | Keep in graph (file exists on disk) |

### R5: Invalidation cascade

Module X changes → `direct_dependents(X)` → invalidate caches → re-publish diagnostics.

### R6: Core typings pseudo-module

All `.el` implicitly depend on `typings/emacs/{version}/*`. Change to any → invalidate all.

### R7: Cycle detection

Report cycles as errors (`.tart`) or warnings (`.el`).

## Tasks

- [ ] Graph data structures
- [ ] Dependency extraction (.el)
- [ ] Dependency extraction (.tart)
- [ ] Reverse index for dependents
- [ ] LSP integration (didOpen/didChange)
- [ ] Invalidation cascade
- [ ] Core typings as pseudo-module
- [ ] Cycle detection
