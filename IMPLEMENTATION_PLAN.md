# Row Polymorphism Implementation Plan

Complete the row polymorphism feature (Spec 11, R4-R13). The foundation is
already in place: `TRow` representation, row parsing in signatures, and row
unification rules. This plan covers the remaining work broken into small,
self-contained iterations.

## Current State

**Done:**
- `TRow` variant in `types.ml` with `row_fields` + `row_var`
- `closed_row`, `open_row` constructors; `row_lookup`, `row_has_field` utilities
- Row unification in `unify.ml` (open vs closed, row variable threading)
- `{name string & r}` parsing in `sig_parser.ml`
- `STRow` → `TRow` conversion in `sig_loader.ml`
- `alist`, `plist`, `hash-table` aliases in prelude
- `alist-get` polymorphic signature in `subr.tart`
- 5 parser tests + 7 unification tests passing

**Remaining (Spec 11 R4-R13):**
- R4: Row-polymorphic alist types (signature-driven)
- R5: Row-polymorphic plist types (signature-driven)
- R6: Map pattern exhaustiveness
- R7: Closed unions vs open row types
- R8: Row type inference from field access
- R9: Literal types with deferred widening
- R12: Generic `map` supertype
- R13: All map type forms

## Iterations

### Iteration 1: Row-typed alist signatures work end-to-end

**Goal:** A function annotated with `(alist {name string & r})` type-checks
against callers providing alists with extra fields.

**What to build:**

1. Wire row types through `alist` type application in `sig_loader.ml`
   - When `alist` is applied to a single row argument instead of `k v`, produce
     `TApp(List, [TApp(Pair, [Symbol, TRow {...}])])` or a dedicated
     representation
   - Decision: treat `(alist {name string & r})` as sugar for
     `(list (pair symbol (alist-row {name string & r})))`, OR introduce a new
     internal type constructor for record-style alists that carries the row
     directly. The simpler path: `alist` with a row arg is a distinct type
     form — add a `TMap` or handle `TApp(alist, [TRow ...])` as a special case
     in unification
   - Start with the simplest approach: `(alist {name string & r})` parses as
     `TApp(TCon "alist", [TRow {name:string & r}])` and unification matches this
     against concrete alist values

2. Add an `alist-get` overload that works with row-typed alists
   - When calling `(alist-get 'name person)` where `person` has type
     `(alist {name string & r})`, the return type should be `(string | nil)`
   - This requires special-case logic in `infer.ml` for `alist-get` calls where
     the first arg is a quoted symbol and the second arg has a row-typed alist
     type

3. Add fixture tests:
   - `test/fixtures/typing/rows/alist_row_basic.el` — function with row-typed
     alist param, called with extra fields
   - `test/fixtures/typing/rows/alist_row_closed.el` — closed row rejects extra
     fields

**Files to change:**
- `lib/sig/sig_loader.ml` — handle alist with row arg
- `lib/typing/infer.ml` — special-case `alist-get` with literal key + row type
- `lib/typing/unify.ml` — potentially handle `TApp(alist, [TRow])` matching
- `test/fixtures/typing/rows/` — new fixture tests

**Verify:** `dune test` passes; fixture tests demonstrate row-typed alist
signatures accepting extra fields

---

### Iteration 2: Row-typed plist signatures

**Goal:** Same as Iteration 1 but for plist types with keyword fields.

**What to build:**

1. Wire row types through `plist` type application
   - `(plist {:name string & r})` should work analogously to alist rows
   - Plist rows use `:keyword` prefixed field names (already parsed by
     `sig_parser.ml`)

2. Add `plist-get` overload for row-typed plists
   - When calling `(plist-get person :name)` where `person` has type
     `(plist {:name string & r})`, return type should be `(string | nil)`
   - Keyword argument detection: second arg is a `Keyword` literal

3. Add fixture tests:
   - `test/fixtures/typing/rows/plist_row_basic.el`
   - `test/fixtures/typing/rows/plist_row_closed.el`

**Files to change:**
- `lib/sig/sig_loader.ml` — handle plist with row arg
- `lib/typing/infer.ml` — special-case `plist-get` with keyword key + row type
- `test/fixtures/typing/rows/` — new fixture tests

**Verify:** `dune test` passes

---

### Iteration 3: Row type inference from field access (R8)

**Goal:** When `alist-get` or `plist-get` is called with a literal key, infer a
row type for the map argument without requiring an explicit annotation.

**What to build:**

1. In `infer.ml`, detect `alist-get` calls with literal first arg:
   - `(alist-get 'name x)` → constrain `x` to have type
     `(alist {name α & r})` where `α` is a fresh type variable
   - Multiple accesses accumulate fields:
     `(alist-get 'name x)` + `(alist-get 'age x)` → `x : (alist {name α age β & r})`

2. For variable keys, fall back to homogeneous inference:
   - `(alist-get key m)` → `m : (alist k v)`, `key : k`, return `(v | nil)`
   - This is the current behavior; no changes needed

3. Add fixture tests:
   - `test/fixtures/typing/rows/alist_infer_row.el` — row inferred from literal
     access
   - `test/fixtures/typing/rows/alist_infer_multi.el` — multiple accesses infer
     multi-field row

**Files to change:**
- `lib/typing/infer.ml` — row inference for `alist-get` / `plist-get` with
  literal keys
- `test/fixtures/typing/rows/` — new fixture tests

**Verify:** `dune test` passes

---

### Iteration 4: Generic `map` supertype (R12)

**Goal:** A `map` type that is a supertype of `alist`, `plist`, and
`hash-table`, enabling functions that accept any map-like structure.

**What to build:**

1. Add `(type map [r] ...)` to the prelude
   - `map` with a row argument represents "any map-like structure with at least
     these fields"
   - Subtyping: `(alist {name string & r}) <: (map {name string & r})`
   - Similarly for plist and hash-table

2. Add `map-elt` function signature
   - `(defun map-elt [k v] ((map k v) k) -> (v | nil))`
   - Or with row types: `(defun map-elt (((m (map {f t & r}))) symbol) -> ...)`
   - Start simple: `map-elt` with homogeneous map type, add row overload later

3. Add subtyping rules in `unify.ml`
   - `TApp(alist, [TRow r]) ~ TApp(map, [TRow r])` should unify
   - Same for plist, hash-table

4. Add fixture tests:
   - `test/fixtures/typing/rows/map_supertype.el` — function typed with `map`
     accepts alist, plist, hash-table

**Files to change:**
- `typings/tart-prelude.tart` — add `map` type
- `typings/emacs/31.0/lisp-core/map.tart` — add `map-elt` signature (or
  whichever file is appropriate)
- `lib/typing/unify.ml` — subtyping for map/alist/plist/hash-table
- `test/fixtures/typing/rows/` — new fixture tests

**Verify:** `dune test` passes

---

### Iteration 5: Closed unions vs open row types (R7)

**Goal:** Ensure unions are always exhaustive while row types permit extra
fields. Document and test the boundary.

**What to build:**

1. Verify exhaustiveness checker handles row-typed map patterns correctly
   - `pcase` on a row-typed value with `map` patterns should not warn about
     extra fields
   - `pcase` on a union should still require exhaustive coverage

2. Add fixture tests demonstrating the distinction:
   - `test/fixtures/typing/rows/union_closed.el` — union exhaustiveness still
     works
   - `test/fixtures/typing/rows/row_open_no_warning.el` — row-typed map doesn't
     warn about unmatched extra fields

**Files to change:**
- `lib/typing/exhaustiveness.ml` — potentially adjust for row types
- `test/fixtures/typing/rows/` — new fixture tests

**Verify:** `dune test` passes

---

### Iteration 6: Map pattern integration (R6)

**Goal:** `(pcase-let (((map :name :age) person)) ...)` integrates with row
types for field extraction.

**What to build:**

1. In `infer.ml`, handle `map` pcase patterns:
   - Extract field names from the pattern
   - Generate row type constraints for the matched expression
   - Bind each extracted field to its row field type in the branch environment

2. Error on accessing a field not present in the row type

3. Add fixture tests:
   - `test/fixtures/typing/rows/map_pattern.el` — map pattern extracts typed
     fields
   - `test/fixtures/typing/rows/map_pattern_error.el` — accessing absent field
     produces type error

**Files to change:**
- `lib/typing/infer.ml` — handle map patterns in pcase
- `test/fixtures/typing/rows/` — new fixture tests

**Verify:** `dune test` passes

---

### Iteration 7: All map type forms (R13)

**Goal:** Support bare `(alist)`, homogeneous `(alist k v)`, and record
`(alist {fields...})` forms, and ensure they interact correctly.

**What to build:**

1. Support bare `(alist)` in signatures — inferred from body
2. Ensure homogeneous and record forms both work and don't interfere
3. Ensure `hash-table` gets the same row support as alist/plist

4. Add fixture tests for each form:
   - `test/fixtures/typing/rows/alist_forms.el`
   - `test/fixtures/typing/rows/hashtable_row.el`

**Files to change:**
- `lib/sig/sig_loader.ml` — handle bare alist form
- `lib/typing/infer.ml` — row inference for hash-table access functions
- `test/fixtures/typing/rows/` — new fixture tests

**Verify:** `dune test` passes

---

## Deferred

**R9 (Literal types with deferred widening)** is listed under Spec 11 but is a
cross-cutting concern that affects more than row polymorphism. It should be
planned separately. The row polymorphism iterations above do not depend on it.

## Ordering

Iterations are ordered by dependency:

```
1 (alist rows) ──► 2 (plist rows) ──► 3 (row inference)
                                           │
                        4 (map supertype) ◄─┘
                             │
              5 (unions vs rows) ──► 6 (map patterns) ──► 7 (all forms)
```

Iterations 1-3 form the critical path. Iterations 4-7 can be parallelized to
some degree after 3 is complete.
