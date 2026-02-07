# Implementation Plan: Spec 33 — Typing Test Fixtures

## Background

Spec 33 calls for comprehensive fixture coverage organized by error
category, plus regression fixtures for previously-encountered bugs.

**What's already done (prior iterations):**

- `errors/` directory with 8 subdirectories: type-mismatch (6 fixtures),
  arity (4), unbound (5), occurs-check (2), kind (3), exhaustiveness (3),
  disjoint-eq (4 — bonus), narrowing (3 — bonus)
- R8 realistic user scenarios in type-mismatch (user-config), arity
  (hook-function), unbound (require-missing)
- R9 fixture_test.ml already discovers errors/ subdirectories dynamically
- 93 total fixtures, all passing

**What remains:**

1. **R7: Regression fixtures** — regression/ directory exists but is empty
2. **Spec tasks** — all 12 task checkboxes are unchecked
3. **Review** — verify all .expected files are correct

---

## Iteration 1: Regression fixtures and spec completion

Create regression fixtures documenting previously-fixed bugs, then mark
all spec tasks complete.

**What to build:**

1. Create regression fixtures (minimum 3) based on bugs found during
   development:

   a. `regression/single-clause-diagnostic.{el,expected}` — Regression
      for compute_defun_clauses bug where single-clause with diagnostic
      was returning None (fixed iteration 26). Test that a single-clause
      defun with a deprecation warning still emits the diagnostic.

   b. `regression/plist-list-subsume.{el,expected}` — Regression for
      plist-to-list subsumption (iteration 13). Verify that a plist value
      can be passed where a list is expected.

   c. `regression/union-subtract-multi.{el,expected}` — Regression for
      subtract_type with union subtrahend (iteration 30). Verify that
      subtracting a union from a union works correctly (e.g., sequencep
      else-branch).

2. Update `specs/33-typing-fixtures.md`:
   - Check all completed task boxes
   - Update status

3. Run full test suite to verify.

**Files:** regression fixtures, `specs/33-typing-fixtures.md`

**Verify:** `dune test`; all fixtures pass including new regression tests
