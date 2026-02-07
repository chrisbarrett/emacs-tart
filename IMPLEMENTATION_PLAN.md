# Implementation Plan: Spec 47 — Error Code Registry

## Background

Spec 47 defines a canonical error code registry. All diagnostics must
reference codes from this spec. Codes are sequential, stable, and
categorized.

**What's already done:**

- `diagnostic.ml(i)` has `error_code` type with 19 constructors
  covering all currently-used error conditions
- `error_code_to_string` maps each to E-prefixed string
- `diagnostic_test.ml` has 73 tests covering all code paths
- `to_string`, `to_string_compact`, `to_string_human`, `to_json` all
  include codes when present

**Gap analysis:**

1. **Spec registry missing E0008 (DisjointEquality)**: added after
   spec was written (Spec 48 R7). The spec table needs updating.

2. **Clause diagnostics lack error codes**: `clause_diagnostic_to_diagnostic`
   in `module_check.ml` emits `code = None`. These are intentional
   (advisory messages from `.tart` signatures, not error conditions).
   Acceptable — they're user-authored messages, not compiler errors.

3. **No test enforcing code↔string stability**: R3 requires codes never
   be reassigned. A test mapping every constructor to its string would
   catch accidental changes.

4. **Spec task checkboxes unchecked**: all 4 tasks need auditing.

5. **Future codes (E0103, E0105, E0202, E0203, E0401, E0402,
   E0500–E0503, E0600–E0602, E0700, E0701, E0800–E0802)** exist in
   the spec registry but have no implementation because the features
   don't exist yet. These are reservations, not gaps.

---

## Iteration 1: Add stability test, update spec, check boxes

**What to build:**

1. Add E0008 (DisjointEquality) row to the spec 47 Type Errors table.

2. Add an error code stability test to `diagnostic_test.ml`:
   - Exhaustive test mapping every `error_code` constructor to its
     expected string, catching regressions if codes drift.

3. Check all 4 task boxes in the spec:
   - R1: `error_code` type matches spec (modulo future reservations)
   - R2: 1:1 mapping verified by exhaustive match
   - R3: stability test added
   - R4: all diagnostic constructors emit codes (clause diagnostics
     are intentionally code-less — they're advisory, not errors)

4. Update spec status.

**Files:**
- `specs/47-error-codes.md`
- `test/typing/diagnostic_test.ml`

**Verify:** `dune test`; all tests pass
