# Implementation Plan: Spec 52 — Remaining Type Predicate Tasks

## Background

Spec 52 (Type Predicates) is mostly implemented. R1–R4 (basic narrowing,
else-branch subtraction, cumulative cond narrowing, predicates in `and`)
all work, plus standard library declarations are migrated to multi-clause
syntax (Spec 54). Three tasks remain:

1. **R5: Predicates in `or` early-exit** — `(or (stringp x) (error "..."))`
   should narrow `x` to `string` after the `or` expression
2. **Union intersection for narrowing** — `narrow_type` needs to handle
   semantic intersection correctly for overlapping types
3. **Inline-only restriction (R12)** — Stored predicate results like
   `(let ((result (stringp x))) (when result ...))` must NOT narrow `x`

**What's already done:**

- `narrow.ml`: `analyze_condition`, `narrow_type`, `predicate_info`
- `infer.ml`: `narrow_env_from_analysis`, `narrow_then_from_analysis`,
  `narrow_else_from_analysis`, plus narrowing in `infer_if`,
  `infer_if_no_else`, `infer_when`, `infer_unless`, `infer_cond`,
  `infer_and`
- `infer_or` exists but does NOT propagate narrowing to code after the
  `or` expression

**Key insight for R5:** The `or` early-exit pattern works differently from
`and`. In `(or (stringp x) (error "..."))`, if execution continues past
the `or`, one of the args was truthy. If the first arg is a predicate
and the second arg diverges (never returns), then the predicate must have
been true. However, implementing full divergence analysis is complex.

The practical approach: when `or` is used as a condition in `if`/`when`
etc., the narrowing already happens because `analyze_condition` already
handles the outer `if`. The missing case is when `or` appears as a
standalone statement and execution continues afterward. This requires
`infer_or` to propagate the then-narrowing from predicates in its args
into the subsequent environment.

Actually, re-reading the spec more carefully: R5 says:

```elisp
(or (stringp x) (error "Expected string"))
(upcase x)   ; x : string
```

This means the narrowing must escape the `or` into the enclosing `progn`.
This is a **progn-level** narrowing concern: after an `or` expression
that contains a predicate, subsequent forms see the narrowed env.

This is analogous to how some type systems handle assertion-like
patterns. The simplest correct approach: when `or` has a predicate in
its first position and a divergent (error/signal) form in subsequent
positions, narrow the environment after the `or`.

For now, the pragmatic approach is to handle `or` when used as a
condition (already works via `analyze_condition`) and defer the
progn-level escape. The spec example is an aspirational pattern;
checking whether the second arg diverges requires analyzing `error`
as `never`-returning, which the type system may not track yet.

**Revised scope:** Focus on the inline-only restriction (R12) which is a
correctness fix, and union intersection improvement which completes the
narrowing semantics. R5 (or-expression narrowing in progn context)
requires `never`-return tracking and is deferred.

---

## Iteration 1: Inline-only restriction for predicate narrowing

Ensure that storing a predicate result in a variable does NOT enable
narrowing. Currently `analyze_condition` only recognizes inline
`(predicate-fn var)` calls and `(and ...)` forms, so the restriction
is partially enforced by the pattern-matching structure. Verify this
is correct and add a test fixture.

**What to build:**

1. Verify `analyze_condition` in `narrow.ml` does NOT match a bare
   symbol reference like `result` in `(when result ...)` — it should
   return `NoPredicate` because the condition is a `Symbol`, not a
   `List` with a function call. Read the code to confirm.

2. Create test fixture `test/fixtures/typing/core/predicate_no_stored.el`:
   ```elisp
   ;; Stored predicate result does NOT narrow (R12)
   (defun stored-predicate (x)
     (declare (tart ((string | int)) -> int))
     (let ((is-str (stringp x)))
       (if is-str
           (string-to-char x)   ;; x is NOT narrowed here
         (+ x 1))))             ;; nor here
   ```
   This should FAIL because `x` is not narrowed to `string` in the
   then-branch (it's still `(string | int)` and `string-to-char`
   expects `string`).

3. Create the `.expected` file with `FAIL` and the expected error
   substring.

**Files:** `narrow.ml` (read/verify), test fixtures

**Verify:** `dune test`; fixture confirms stored predicate doesn't narrow

---

## Iteration 2: Improve narrow_type for union intersection

The current `narrow_type` implementation uses `types_disjoint` to filter
union members, which works for simple cases but fails for overlapping
types. Improve it to handle the key cases:

**What to build:**

1. In `narrow.ml`, improve `narrow_type`:
   - When original is a union and target is also a union, compute the
     intersection by filtering original members that overlap with ANY
     target member
   - When original is `any` and target is a union, return target (already
     works)
   - When original is a single type that's a subtype of target, keep it
     (already partially works via disjointness check)

2. Handle the case where target is a multi-type predicate union like
   `(list any) | (vector any) | string` (from `sequencep`):
   - `narrow_type ((string | int | (list any))) ((list any) | (vector any) | string)`
     should produce `(string | (list any))` — the members of original
     that overlap with any member of target

3. Add unit tests for `narrow_type` edge cases in a test file.

4. Create test fixture `test/fixtures/typing/core/predicate_union_intersect.el`:
   Tests narrowing with multi-type predicates where the input union
   partially overlaps with the predicate's narrowed type.

**Files:** `lib/typing/narrow.ml`, test fixtures

**Verify:** `dune test`; multi-type predicate narrowing correct

---

## Iteration 3: Update spec checkboxes and test

Mark completed tasks in spec, run full test suite.

**What to build:**

1. Update `specs/52-type-predicates.md`:
   - Check "Inline-only restriction" task
   - Check "Union intersection for narrowing" task
   - Add note that R5 (or-expression progn narrowing) requires
     never-return tracking and is deferred
   - Update status

2. Run full test suite and verify all fixtures pass.

**Files:** `specs/52-type-predicates.md`

**Verify:** `dune test`
