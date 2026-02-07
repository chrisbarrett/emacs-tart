# Implementation Plan: Spec 48 R7 — Equality Safety Bounds

## Background

Spec 48 (Prelude) R1–R6 are fully implemented. The one remaining task is
R7: make `eq` and `eql` type-safe by detecting identity comparison on
non-identity-safe types (strings, lists, floats for `eq`).

**What's already done:**

- `eq-safe` and `eql-safe` types exist in `typings/tart-prelude.tart`
- `satisfies_bound` in `sig_loader.ml` supports union bounds
- Multi-clause dispatch works (Spec 54/56)
- Clause diagnostics work (Spec 57)
- `infer_eq_with_disjointness` in `infer.ml` detects provably disjoint
  types (e.g., `(eq 1 "a")` → error E0008)
- `types_disjoint` in `unify.ml` implements the disjointness check

**Current eq/eql signatures:**

```lisp
;; data.tart
(defun eq [a b] (a b) -> bool)
;; fns.tart
(defun eql [a b] (a b) -> bool)
```

**Target signatures (per Spec 57 design notes):**

```lisp
;; data.tart
(defun eq
  ((eq-safe eq-safe) -> bool)
  ((_ _) -> bool
    (warn "eq compares by identity; use equal for structural comparison")))

;; fns.tart
(defun eql
  ((eql-safe eql-safe) -> bool)
  ((_ _) -> bool
    (warn "eql compares by identity for non-numbers; use equal for structural comparison")))
```

This uses multi-clause signatures with clause diagnostics rather than
bounded type parameters. The first clause matches identity-safe types
silently; the second clause matches anything else with a warning.

**Interaction with disjointness checking:**

The existing `infer_eq_with_disjointness` special case in `infer.ml`
provides a more precise check: it detects when the two argument types are
provably disjoint (can never be `eq`). This is a separate, complementary
concern—two strings can be `eq` (identity) but shouldn't be compared that
way. The disjointness check catches `(eq "a" 42)` as an error; the
clause diagnostic catches `(eq "a" "b")` as a warning.

Both checks should coexist: the multi-clause dispatch runs through
`infer_application`, while the disjointness check runs in the special
`infer_eq_with_disjointness` path. However, since multi-clause dispatch
now handles the identity-safety concern, the special-case path should
still run for disjointness but the clause dispatch should also fire.

**Key issue:** Currently `infer_eq_with_disjointness` intercepts eq/eql
calls *before* they reach `infer_application`. This bypasses clause
dispatch entirely. To get both behaviors, we need to either:

(a) Remove the special-case intercept and let calls flow through
    `infer_application` (which already does clause dispatch), then add
    disjointness checking as a post-step, OR
(b) Keep the special case but also invoke clause dispatch within it.

Option (a) is cleaner: route eq/eql through `infer_application` and add
disjointness checking there.

---

## Iteration 1: Migrate eq/eql to multi-clause signatures

Change `eq` and `eql` signatures to multi-clause with clause diagnostics,
and route their inference through `infer_application` so clause dispatch
fires.

**What to build:**

1. Update `typings/emacs/31.0/c-core/data.tart`: change `eq` from
   `[a b] (a b) -> bool` to multi-clause with `eq-safe` first clause
   and wildcard second clause with `warn` diagnostic.

2. Update `typings/emacs/31.0/c-core/fns.tart`: same for `eql` with
   `eql-safe`.

3. Modify `infer.ml`: remove the special-case pattern match for eq/eql
   (lines ~287-289) that dispatches to `infer_eq_with_disjointness`.
   Instead, let eq/eql calls fall through to `infer_application` like
   any other function.

4. Move disjointness checking into `infer_application`: after clause
   dispatch or standard constraint generation for eq/eql, check if
   the two argument types are provably disjoint and inject the E0008
   constraint if so.

**Files:** `typings/emacs/31.0/c-core/data.tart`,
`typings/emacs/31.0/c-core/fns.tart`, `lib/typing/infer.ml`

**Verify:** `dune test`; existing disjoint-eq fixtures still pass;
identity-unsafe calls now produce warnings

---

## Iteration 2: Test fixtures and spec cleanup

Add test fixtures for the new behavior and update spec task checkboxes.

**What to build:**

1. Add fixture `test/fixtures/typing/core/eq_identity_safe.{el,expected}`:
   eq with identity-safe types (symbol, keyword, int, t, nil) → PASS

2. Add fixture `test/fixtures/typing/errors/eq-identity-unsafe.{el,expected}`:
   eq with string args → FAIL with identity warning

3. Add fixture `test/fixtures/typing/core/eql_float.{el,expected}`:
   eql with float args → PASS (float is eql-safe)

4. Update existing `eq_compatible.el` expected output if needed (symbols
   are eq-safe, so no warning expected)

5. Verify existing `disjoint-eq/` fixtures still pass (disjointness
   check is complementary to identity-safety check)

6. Update spec task checkboxes:
   - `specs/48-prelude.md`: check R7 task, update status to "Complete"
   - `specs/35-structured-errors.md`: check all tasks (already
     implemented), update status to "Complete"

**Files:** test fixtures, spec files

**Verify:** `dune test`
