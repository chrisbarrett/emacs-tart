# Implementation Plan: Spec 34 — Funcall/Apply Remaining Tasks

## Background

Core funcall/apply typing is implemented (R1-R4, R6-R8, R14). The
remaining tasks are:

- **R5**: Style warning for `'name` (regular quote) in funcall/apply
  position (recommend `#'name`)
- **R9**: Tuple-to-list subtyping in unification (`TTuple <: TApp(List,_)`)
- **R10**: Infer quoted lists as tuples in apply context
- **R11**: Handle union function types in funcall
- **R12/R13**: Already implemented via Spec 52 predicate narrowing +
  functionp multi-clause
- **Cleanup**: Remove funcall/apply `(any &rest any) -> any` from eval.tart
- **Test fixtures**: Add coverage for remaining requirements

---

## Iteration 1: R5 — Style warning for quoted symbols in funcall/apply

**What to build:**

1. In `lib/typing/infer.ml`, in `infer_funcall` and `infer_apply`:
   detect when the function expression is a regular-quoted symbol
   `(quote name)` (not `(function name)` / `#'name`). When detected,
   emit a clause diagnostic (hint severity) recommending `#'name`.

2. Use the existing clause diagnostic infrastructure (Spec 57):
   create a `resolved_clause_diagnostic` with DiagHint severity and
   message like "use #'name instead of 'name for function references".

3. Create test fixture
   `test/fixtures/typing/funcall/quote-style-warning.{el,expected}`:
   - `(funcall 'add-one 5)` → PASS with hint about #'
   - `(apply '+ '(1 2 3))` → PASS with hint about #'
   - `(funcall #'add-one 5)` → PASS (no hint)

**Files:**
- `lib/typing/infer.ml` — detect quoted symbol, emit hint
- `test/fixtures/typing/funcall/quote-style-warning.{el,expected}` (new)

**Verify:** `dune build`; `dune test`

---

## Iteration 2: R9 — Tuple-to-list subtyping in unification

**What to build:**

1. In `lib/typing/unify.ml`, add a unification rule:
   `TTuple [t1; t2; ...; tn]` unifies with `TApp("List", [elem])`
   when each `ti` unifies with `elem`. This is one-directional:
   tuple <: list, but not list <: tuple.

2. Handle the symmetric case: `TApp("List", [elem])` on left,
   `TTuple` on right should fail (lists don't narrow to tuples).

3. Create test fixture
   `test/fixtures/typing/funcall/tuple-list-subtype.{el,expected}`:
   - Pass a quoted list `'(1 2 3)` where `(list int)` expected → PASS
   - Verify tuple-to-list widening works with apply

**Files:**
- `lib/typing/unify.ml` — TTuple/TApp(List) subtyping rule
- `test/fixtures/typing/funcall/tuple-list-subtype.{el,expected}` (new)

**Verify:** `dune build`; `dune test`

---

## Iteration 3: R10 — Tuple inference for quoted lists in apply

**What to build:**

1. In `lib/typing/infer.ml`, modify `infer_quoted` (or add a variant)
   so that quoted list literals `'(1 "hello" nil)` infer as
   `TTuple [int; string; nil]` instead of `(List Any)`.

2. This change affects ALL quoted lists, not just apply context. The
   tuple-to-list subtyping from R9 ensures backward compatibility:
   where `(List T)` is expected, a tuple unifies via the R9 rule.

3. Update `infer_apply` to remove the NOTE at lines ~1405 about
   not constraining list args (now safe because tuples widen to lists).

4. Create test fixtures:
   - `test/fixtures/typing/funcall/apply-tuple-infer.{el,expected}`:
     `(apply #'cons '(1 (2 3)))` with proper tuple inference
   - Verify existing apply tests still pass

**Files:**
- `lib/typing/infer.ml` — quoted list → tuple, apply constraint
- `test/fixtures/typing/funcall/apply-tuple-infer.{el,expected}` (new)

**Verify:** `dune build`; `dune test`; all 97+ fixtures pass

---

## Iteration 4: R11 — Union function types in funcall

**What to build:**

1. In `lib/typing/infer.ml`, in `infer_funcall`: when the inferred
   function type is `TUnion [TArrow(...); TArrow(...); ...]`, check
   args against each variant. Result is union of return types.

2. If arg types are incompatible across variants (e.g., one expects
   String, another Symbol), produce type error.

3. Create test fixture
   `test/fixtures/typing/funcall/union-function.{el,expected}`:
   - Compatible union: `(if cond #'1+ #'1-)` → funcall works
   - Incompatible union: `(if cond #'upcase #'symbol-name)` → error

**Files:**
- `lib/typing/infer.ml` — union function type handling in funcall
- `test/fixtures/typing/funcall/union-function.{el,expected}` (new)

**Verify:** `dune build`; `dune test`

---

## Iteration 5: Cleanup + R12/R13 verification + spec status

**What to build:**

1. Remove `funcall` and `apply` signatures from
   `typings/emacs/31.0/c-core/eval.tart` (they're handled by
   special-casing in infer.ml; the `(any &rest any) -> any` signatures
   are dead code that could mask type errors if the special-casing
   is bypassed).

2. Create test fixture verifying R12/R13 already work:
   `test/fixtures/typing/funcall/narrowed-funcall.{el,expected}`:
   - `(if (functionp f) (funcall f x) ...)` → f narrowed to function

3. Check all task boxes in spec 34; update status to "Complete".

**Files:**
- `typings/emacs/31.0/c-core/eval.tart` — remove funcall/apply sigs
- `test/fixtures/typing/funcall/narrowed-funcall.{el,expected}` (new)
- `specs/34-funcall-apply-typing.md` — check boxes, update status

**Verify:** `dune build`; `dune test`; all fixtures pass
