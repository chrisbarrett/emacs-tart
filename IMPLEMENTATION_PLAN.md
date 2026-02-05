# Row Polymorphism Implementation Plan

Complete the row polymorphism feature (Spec 11, R4-R15). The foundation is
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
- Basic row-typed `alist-get` inference for literal symbol keys (b8942b6):
  constrains alist to `TApp(TCon "alist", [TRow {key α & r}])`, returns
  `(α | nil)`. This is a stepping stone—Iteration 1 changes the internal
  representation to Design B.
- 5 parser tests + 7 unification tests passing

**Remaining (Spec 11 R4-R15):**
- R4: Design B alist expansion + refined `alist-get` return types
- R5: Row-polymorphic plist types (signature-driven)
- R6: Map pattern exhaustiveness
- R7: Closed unions vs open row types
- R8: Row type inference from field access
- R9: Literal types with deferred widening
- R12: Generic `map` supertype
- R13: All map type forms
- R14: Equality predicate bounds for `alist-get` testfn
- R15: Row-to-homogeneous unification rule

## Iterations

### Iteration 1: Design B alist expansion

**Goal:** `(alist {name string & r})` expands to
`(list (cons symbol {name string & r}))` in `sig_loader.ml`. The row occupies
the value position of the cons pair, preserving field names for static key
lookup while maintaining structural compatibility with homogeneous alists.

**What to build:**

1. In `sig_loader.ml`, add a special case for the `alist` alias when applied to
   a single row argument. Currently the arity mismatch (1 arg vs 2 params)
   causes a fall-through to `TApp(TCon "alist", [TRow ...])`. Change this to
   expand as `TApp(List, [TApp(Pair, [Symbol, TRow {...}])])`:
   - Detect: name is `"alist"`, args has length 1, and the single arg is a row
     type
   - Expand: `list_of (pair_of Prim.symbol row_type)`
   - Apply the same pattern for `plist` (key type: `keyword`) and `hash-table`

2. Update `infer_alist_get_row` in `infer.ml` (from b8942b6) to use the
   expanded form:
   - Instead of `TApp(TCon "alist", [TRow ...])`, constrain the alist expression
     to `TApp(List, [TApp(Pair, [Symbol, TRow {KEY α & r}])])` where `α` is a
     fresh type variable for the field's value type and `r` is a fresh row
     variable
   - The `row_lookup` on the extracted `TRow` is what enables static key
     resolution

3. Add the `TRow ~ T` unification rule in `unify.ml` (Spec 11 R15):
   - When unifying `TRow {f1:t1, f2:t2, ...}` with a plain type `T`, succeed
     when every `ti ~ T`
   - Open rows: the row variable must also satisfy the constraint (bind it to a
     row where all field types are `T`)
   - This enables compatibility between row-typed and homogeneous alists

4. Existing fixture tests should continue passing (may need updated expectations
   if the type printer output changes due to the expanded representation)

**Files to change:**
- `lib/sig/sig_loader.ml` — 1-arg alist/plist/hash-table expansion
- `lib/typing/infer.ml` — update `infer_alist_get_row` to use expanded form
- `lib/typing/unify.ml` — add `TRow ~ T` compatibility rule
- `test/fixtures/typing/rows/` — update expectations if needed

**Verify:** `dune test` passes; existing row fixtures still pass

---

### Iteration 2: Refined alist-get return types

**Goal:** `alist-get` returns precise types based on the 7-case decision table
from Spec 11 R4. Currently always returns `(field_type | nil)`.

**What to build:**

1. Expand `infer_alist_get_row` in `infer.ml` to handle all cases:
   - **Cases 1–2** (literal key, present in row): return `field_type` directly,
     not `field_type | nil`. The key is provably present, so nil is impossible
     (assuming the value type itself doesn't include nil).
   - **Cases 3–4** (literal key, absent from closed row): return `nil` (or the
     DEFAULT type if provided). Emit a note diagnostic—the key is statically
     known absent.
   - **Case 5** (literal key, absent from open row): current behavior—return
     `(α | nil)` since the key might exist in the unknown row tail.
   - **Case 6** (variable key): return the join of all field value types `| nil`.
     Cannot determine which field, so use the union of all possibilities.
   - **Case 7** (incompatible testfn): defer to Iteration 3.

2. This requires two-phase inference for literal keys:
   - First, try to resolve the alist expression's type to see if it already has
     a known row (from an annotation or prior constraint)
   - If the row is known, use `row_lookup` to check field membership
   - If the row is not yet known (fresh type variable), fall back to current
     behavior (constrain with fresh row, return `α | nil`)

3. Add fixture tests for new cases:
   - `test/fixtures/typing/rows/alist_get_precise.el` — literal key in row
     returns exact type (no nil)
   - `test/fixtures/typing/rows/alist_get_absent.el` — literal key absent from
     closed row returns nil with note
   - `test/fixtures/typing/rows/alist_get_default.el` — absent key with default
     returns default type
   - `test/fixtures/typing/rows/alist_get_variable.el` — variable key returns
     join of field types

**Files to change:**
- `lib/typing/infer.ml` — expand `infer_alist_get_row` for all cases
- `lib/typing/diagnostic.ml` — add note-level diagnostic for absent key
- `test/fixtures/typing/rows/` — new fixture tests

**Verify:** `dune test` passes

---

### Iteration 3: Equality predicate disjointness checking

**Goal:** `eq` and `eql` reject comparisons between statically disjoint types.
Instead of encoding bounds in the signature language, use a special-case
disjointness analysis in the type checker. Implements Spec 11 R14.

**Design rationale:** A signature-level approach (`[a : eq-safe] (a a) -> bool`)
doesn't capture disjointness — with union widening, `join(int, symbol)` passes
the bound even though the values can never be `eq`. The constraint we need
("types must have non-empty intersection") is relational between two type
variables, which would require a new constraint form (`overlaps`) in the
signature language just for this family of functions. A special-case check in
`infer.ml` achieves the same result with no language extensions.

**The rule:** `(eq a b)` is well-typed iff the types of `a` and `b` are not
provably disjoint. "Provably disjoint" means their intersection is empty given
what's statically known:
- Two different base types (int vs symbol) → disjoint
- A union overlaps with T if any member overlaps with T
- Type variables are conservatively non-disjoint (no evidence to reject)
- `any` overlaps with everything

Examples:
```
(eq 1 'foo)        ;; int ∩ symbol = ∅ → error
(eq num_val 1.0)   ;; num ∩ float ≠ ∅ → ok
(eq num_val 'foo)  ;; num ∩ symbol = ∅ → error
(eq poly_a poly_b) ;; both polymorphic → ok (conservative)
```

**What to build:**

1. Keep `eq` and `eql` signatures simple — two independent type params, no
   bounds:
   - `typings/emacs/31.0/c-core/data.tart`: `(defun eq [a b] (a b) -> bool)`
   - `typings/emacs/31.0/c-core/fns.tart`: `(defun eql [a b] (a b) -> bool)`
   - `equal` stays as `(defun equal (any any) -> bool)` — no check needed

2. Add `types_disjoint : Type.t -> Type.t -> bool` in `lib/typing/unify.ml`
   (or a new `lib/typing/disjointness.ml`):
   - Walk both types after `repr` resolution
   - Base types: disjoint if different type constructors with no subtype
     relationship (e.g. `int` vs `symbol`; `int` vs `num` are NOT disjoint
     because `int <: num`)
   - Unions: `(A | B)` is disjoint from `T` iff both `A` disjoint from `T` and
     `B` disjoint from `T`
   - Type variables (unresolved): conservatively return `false` (not disjoint)
   - `any`, `nil`: `any` overlaps everything; `nil` overlaps only `nil` and
     unions/supertypes containing it

3. Add a special case in `infer.ml` for `eq` and `eql` calls:
   - After inferring both argument types, run `types_disjoint` on them
   - If disjoint, emit a type error ("values of type T1 and T2 can never be eq")
   - Apply the same check for `memq`, `assq`, `remq` and other eq-family
     functions (the first arg's type vs the element type of the list arg)

4. Wire testfn checking into `infer_alist_get_row`:
   - When no TESTFN argument is provided: run `types_disjoint` on the key type
     and `symbol` (the default key type for alists using `eq`). If the key type
     is e.g. `string`, the check fires because `string ∩ symbol = ∅` for `eq`
     purposes
   - When TESTFN is provided: type-check it as `(K K) -> any` where K is the
     key type. The TESTFN's own parameter types enforce compatibility
   - This implements Case 7 from the decision table

5. Add fixture tests:
   - `test/fixtures/typing/eq_disjoint_base.el` — `(eq 1 'foo)` errors
   - `test/fixtures/typing/eq_disjoint_union.el` — `(eq num_val 1)` ok,
     `(eq num_val 'foo)` errors
   - `test/fixtures/typing/eq_polymorphic.el` — polymorphic args ok
   - `test/fixtures/typing/rows/alist_get_string_key.el` — string keys with
     default eq: error
   - `test/fixtures/typing/rows/alist_get_equal.el` — string keys with explicit
     `#'equal`: ok

**Files to change:**
- `typings/emacs/31.0/c-core/data.tart` — update `eq` signature (two params)
- `typings/emacs/31.0/c-core/fns.tart` — update `eql` signature (two params)
- `lib/typing/unify.ml` (or new `lib/typing/disjointness.ml`) — `types_disjoint`
- `lib/typing/infer.ml` — special-case eq/eql + testfn checking
- `test/fixtures/typing/` — new fixture tests

**Verify:** `dune test` passes

---

### Iteration 4: Row-typed plist signatures

**Goal:** Same as Iteration 1 but for plist types with keyword fields.

**What to build:**

1. Wire row types through `plist` type application
   - `(plist {:name string & r})` should work analogously to alist rows
   - Plist rows use `:keyword` prefixed field names (already parsed by
     `sig_parser.ml`)
   - If not already handled in Iteration 1 step 1, add the plist expansion:
     `(plist {:name string & r})` → `(list (keyword | TRow {...}))`

2. Add `plist-get` overload for row-typed plists
   - When calling `(plist-get person :name)` where `person` has type
     `(plist {:name string & r})`, return type should be `(string | nil)`
   - Keyword argument detection: second arg is a `Keyword` literal

3. Add fixture tests:
   - `test/fixtures/typing/rows/plist_row_basic.el`
   - `test/fixtures/typing/rows/plist_row_closed.el`

**Files to change:**
- `lib/sig/sig_loader.ml` — handle plist with row arg (if not done in Iter 1)
- `lib/typing/infer.ml` — special-case `plist-get` with keyword key + row type
- `test/fixtures/typing/rows/` — new fixture tests

**Verify:** `dune test` passes

---

### Iteration 5: Row type inference from field access (R8)

**Goal:** When `alist-get` or `plist-get` is called with a literal key, infer a
row type for the map argument without requiring an explicit annotation.

**What to build:**

1. In `infer.ml`, detect `alist-get` calls with literal first arg:
   - `(alist-get 'name x)` → constrain `x` to have type
     `(list (cons symbol {name α & r}))` where `α` is a fresh type variable
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

### Iteration 6: Generic `map` supertype (R12)

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

### Iteration 7: Closed unions vs open row types (R7)

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

### Iteration 8: Map pattern integration (R6)

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

### Iteration 9: All map type forms (R13)

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
1 (Design B expansion) ──► 2 (refined alist-get) ──► 3 (eq bounds)
                                                          │
                                   4 (plist rows) ◄───────┘
                                        │
                                   5 (row inference) ──► 6 (map supertype)
                                                              │
                            7 (unions vs rows) ──► 8 (map patterns) ──► 9 (all forms)
```

Iterations 1–3 form the critical path: Design B expansion, precise return
types, then equality bounds. Iteration 4 (plist rows) follows the established
alist pattern. Iterations 5–9 can be parallelized to some degree after 5 is
complete.
