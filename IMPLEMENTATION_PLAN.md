# Implementation Plan: Spec 46 R7 / Spec 13 R2 — Branch-Specific Error Messages

## Background

Spec 46 R7 and Spec 13 R2 describe the same feature: when a branch
type violates a declared return type, the error should point to both
the offending branch AND the function's return type declaration.

Currently `branch_mismatch` in `diagnostic.ml` produces a basic error
("if branches have incompatible types") without cross-referencing the
function declaration. The spec expects:

```
error[E0308]: branch type incompatible with return type
  --> utils.el:5:7
   |
 5 |       "negative"
   |       ^^^^^^^^^^ this branch has type: String
   |
note: function declared to return Int
  --> utils.el:1:1
   |
 1 | ;; ((Int) -> Int)
   |              ^^^ expected return type
```

**Key insight:** The type checker already detects the mismatch as a
`ReturnMismatch` or `BranchMismatch`. What's missing is:
1. Capturing the declaration span (the `-> ReturnType` in the signature)
2. Adding it as a `related_location` in the diagnostic
3. Formatting with `note:` pointing to the declaration

---

## Iteration 1: Capture declaration span and enhance diagnostics

**What to build:**

1. In `lib/typing/infer.ml`, find where branch types are checked
   against declared return types (in `infer_defun_with_declaration`,
   `infer_if`, `infer_cond`, etc.). When a constraint is created
   between a branch result and the expected return type, ensure the
   return type's span is available.

2. In `lib/typing/diagnostic.ml`:
   - Extend `type_mismatch_with_context` or `branch_mismatch` to
     accept an optional `declared_return_span` parameter
   - When present, add a `related_location` with message "function
     declared to return TYPE"
   - Format in `to_string_human` using the existing `note:` section

3. Thread the declaration span from `check.ml` / `infer.ml` through
   to the diagnostic constructor. The return type span may come from:
   - The `.tart` signature's return type span
   - The `(declare (tart ...))` form's return type position
   - The signature annotation span stored in the type environment

4. Create test fixture `test/fixtures/typing/errors/type-mismatch/branch-return.{el,expected}`:
   - Function with `(declare (tart ...))` returning wrong type in
     one branch
   - Expected output shows both the branch and the declaration

**Files:**
- `lib/typing/diagnostic.ml` / `diagnostic.mli` — enhanced branch error
- `lib/typing/infer.ml` — thread declaration span
- `lib/typing/check.ml` — thread declaration span
- `test/fixtures/typing/errors/type-mismatch/branch-return.{el,expected}` (new)

**Verify:** `dune build`; `dune test`

---

## Iteration 2: Spec status updates

**What to build:**

1. Check Spec 46 R7 task box; update status to "Complete"
2. Check Spec 13 R2 task box; update status to "Complete"
3. Verify all tests pass

**Files:**
- `specs/46-truthiness-unions.md`
- `specs/13-error-reporting.md`

**Verify:** `dune test`; all tests pass
