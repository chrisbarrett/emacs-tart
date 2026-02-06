# Spec 52: Type Predicate Narrowing

Implement flow-sensitive type narrowing for predicate functions in
conditional branches. The parsing and registration infrastructure is
complete; this plan covers the inference-time narrowing.

## Iteration 1: Analyze conditions for predicate calls

**Problem:** No function examines a condition expression to detect
predicate calls (e.g. `(stringp x)`) and extract the predicate info
(function name, argument variable name, narrowed type).

**What to build:**

1. Add `analyze_condition` to `narrow.ml`:
   - Input: a `Sexp.t` condition and the `Type_env.t`
   - Detect pattern `(predicate_fn arg)` where `predicate_fn` has a
     registered predicate in `Type_env.predicates`
   - `arg` must be a plain `Symbol` (variable reference) per R12
     (inline-only restriction)
   - Return `Predicate { var_name; narrowed_type }` or `NoPredicate`
2. Export in `narrow.mli`:
   `val analyze_condition : Syntax.Sexp.t -> Core.Type_env.t -> condition_analysis`

**Files to change:**
- `lib/typing/narrow.ml` / `narrow.mli`

**Verify:** `dune build && dune test`

---

## Iteration 2: Narrow types in `infer_if` then/else branches

**Problem:** `infer_if` infers both branches with the same env. When
the condition is a predicate call like `(stringp x)`, the then-branch
should see `x : (original ∩ string)` and the else-branch should see
`x : (original − string)`.

**What to build:**

1. In `infer_if`, after inferring the condition, call
   `Narrow.analyze_condition` on the condition sexp
2. If a predicate is detected:
   - Look up the variable's current type in the env
   - Create narrowed env for then-branch: override the variable
     binding with `narrow_type original narrowed_type`
   - Create subtracted env for else-branch: override the variable
     binding with `Types.subtract_type original narrowed_type`
3. Infer then-branch with narrowed env, else-branch with subtracted env
4. Also update `infer_if_no_else` similarly (then-branch only)
5. Add a helper to `env.ml` (or `type_env.ml`) for overriding a
   variable's type:
   `val with_narrowed_var : string -> Types.typ -> t -> t`

**Files to change:**
- `lib/typing/infer.ml`
- `lib/typing/env.ml` / `env.mli` (or `lib/core/type_env.ml`)

**Verify:** `dune build && dune test`

---

## Iteration 3: Add test fixtures for basic predicate narrowing

**Problem:** No test fixtures exercise predicate narrowing in
if/when/unless branches.

**What to build:**

1. Create `test/fixtures/typing/core/predicate_narrowing.el` with
   `.expected` PASS file, testing:
   - `(if (stringp x) (upcase x) ...)` — x : string in then-branch
   - `(if (integerp x) (+ x 1) ...)` — x : int in then-branch
   - `(if (listp x) (car x) ...)` — x : list in then-branch
2. Create `test/fixtures/typing/errors/predicate_narrowing_fail.el`
   with `.expected` FAIL file, testing:
   - Calling string-only fn in else-branch of stringp — should fail
3. Add predicate signatures to test typings fixture if needed (or use
   existing data.tart predicates)

**Files to change:**
- `test/fixtures/typing/core/predicate_narrowing.el` (new)
- `test/fixtures/typing/core/predicate_narrowing.expected` (new)
- `test/fixtures/typing/errors/narrowing/` (new directory + fixtures)

**Verify:** `dune test`

---

## Iteration 4: Handle `when` and `unless` as special forms

**Problem:** `when` and `unless` are treated as regular function calls
via generic application. They need special treatment to support
narrowing: `(when (stringp x) body)` should narrow x to string in
body.

**What to build:**

1. Add pattern matches for `when` and `unless` in the main `infer`
   dispatch (before the catch-all application handler)
2. `infer_when`: analyze condition, narrow env for body, result type
   is `(body_type | nil)` since when returns nil if condition is false
3. `infer_unless`: analyze condition, subtract env for body, result
   type is `(body_type | nil)`
4. Both: infer condition normally, apply narrowing/subtraction to body

**Files to change:**
- `lib/typing/infer.ml`

**Verify:** `dune build && dune test`

---

## Iteration 5: Cumulative narrowing in `cond`

**Problem:** In `infer_cond`, each clause body is inferred with the
same env. For cumulative narrowing per R3, each clause's body should
see the narrowing from its own test, and subsequent clauses should
see the accumulated subtractions from earlier predicate tests.

**What to build:**

1. Thread an accumulated env through `process_clauses` in `infer_cond`
2. For each clause: analyze the test condition for predicates
3. Narrow env for the body of that clause (intersection)
4. Subtract from the accumulated env for subsequent clauses
5. Example: `(cond ((stringp x) ...) ((listp x) ...) ...)` —
   second clause sees x with string subtracted

**Files to change:**
- `lib/typing/infer.ml`

**Verify:** `dune build && dune test`

---

## Iteration 6: Predicates in `and` expressions

**Problem:** `(and (stringp x) (upcase x))` should narrow x to string
for the second operand. Currently `infer_and` infers all args with the
same env.

**What to build:**

1. Change `infer_and` to process args sequentially instead of
   `List.map`
2. After each arg, analyze it for predicates
3. Narrow the env for subsequent args
4. Truthiness semantics already handled (Spec 46); this adds the
   narrowing dimension

**Files to change:**
- `lib/typing/infer.ml`

**Verify:** `dune build && dune test`

---

## Iteration 7: Update standard library predicate signatures

**Problem:** The standard library `.tart` files declare predicates
with `-> bool` and without named parameters. They need `(x is T)`
return types and named parameters `((x type))` for the narrowing
infrastructure to recognize them.

**What to build:**

1. Update `typings/emacs/31.0/c-core/data.tart` with predicate
   signatures per Spec 52's standard library section:
   - `(defun stringp ((x any)) -> (x is string))`
   - `(defun symbolp ((x any)) -> (x is symbol))`
   - `(defun integerp ((x any)) -> (x is int))`
   - `(defun floatp ((x any)) -> (x is float))`
   - `(defun numberp ((x any)) -> (x is num))`
   - `(defun consp ((x any)) -> (x is (cons any any)))`
   - `(defun listp ((x any)) -> (x is (list any)))`
   - `(defun null ((x any)) -> (x is nil))`
   - etc.
2. Update any `include`d files in version 30 if they reference data
3. Add corresponding test fixtures

**Files to change:**
- `typings/emacs/31.0/c-core/data.tart`
- Possibly `typings/emacs/30/c-core/data.tart` or includes

**Verify:** `dune build && dune test`
