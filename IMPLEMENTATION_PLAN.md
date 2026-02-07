# Implementation Plan: Spec 32 — Emacs Core Typings Audit

## Background

The four core `.tart` files (data, fns, eval, alloc) already exist with
substantial coverage (~300 defuns total). BUGS.md skeleton files exist
at both package and version levels. The remaining work is:

- Audit `-> any` return types (19 in data, 2 in fns, 22 in eval, 5 in
  alloc) and improve where semantics are actually known
- Populate BUGS.md with genuinely untypeable items (category + reason)
- Validate files parse and load correctly (already tested in
  search_path_test.ml bundled-c-core suite)
- Check all spec task boxes

Many `-> any` returns are legitimately dynamic (symbol-value,
condition-case, eval). The goal is NOT to eliminate all `-> any` but to
ensure each is justified, and those that can be improved are improved.

---

## Iteration 1: Audit data.tart `-> any` returns

**What to do:**

1. Read `typings/emacs/31.0/c-core/data.tart` and evaluate each of the
   19 `-> any` return types:

   **Can improve:**
   - `car-safe` → `(any) -> any` is correct (accepts non-cons)
   - `cdr-safe` → same, correct
   - `fset` → returns the function object; `(symbol any) -> any` is
     correct (genuinely dynamic)
   - `set` → returns value; same pattern as fset
   - `subr-type` → returns symbol describing subr type
   - `bool-vector-*` → operate on bool-vectors; tighten params/returns

   **Document as untypeable:**
   - `symbol-value` → dynamic by nature (returns stored value)
   - `symbol-function` → returns whatever was fset'd
   - `default-value` → dynamic
   - `indirect-function` → dynamic chain resolution
   - `interactive-form` → returns arbitrary form or nil

2. For each improvable signature, update the .tart file.

3. For each genuinely untypeable case, add entry to
   `typings/emacs/BUGS.md` using the spec's format.

**Files:**
- `typings/emacs/31.0/c-core/data.tart` — improve signatures
- `typings/emacs/BUGS.md` — add untypeable entries

**Verify:** `nix develop --command dune build`; `nix develop --command dune test`

---

## Iteration 2: Audit fns.tart + alloc.tart `-> any` returns

**What to do:**

1. **fns.tart** (2 cases): `get` and `put` (symbol plist access).
   - `get` returns plist value for symbol — genuinely dynamic
   - `put` stores value in symbol plist — returns value, dynamic
   - Document in BUGS.md if no improvement possible

2. **alloc.tart** (5 cases): `record`, `make-record`,
   `make-byte-code`, `make-closure`, `make-finalizer`.
   - Records are opaque; `make-record` returns record type
   - `make-byte-code` / `make-closure` return compiled function
   - `make-finalizer` returns finalizer object
   - Improve where possible (e.g., make-finalizer → truthy)

3. Also scan both files for `any` in input positions that could be
   union types (R10 guideline).

**Files:**
- `typings/emacs/31.0/c-core/fns.tart` — improve signatures
- `typings/emacs/31.0/c-core/alloc.tart` — improve signatures
- `typings/emacs/BUGS.md` — add entries

**Verify:** `nix develop --command dune build`; `nix develop --command dune test`

---

## Iteration 3: Audit eval.tart `-> any` returns

**What to do:**

1. **eval.tart** has 22 `-> any` returns — the most of any core file.
   Many are legitimately dynamic (special forms, evaluation primitives):

   **Legitimately dynamic (document):**
   - `eval`, `cond`, `and`, `or`, `setq` — special forms with
     dynamic return types
   - `condition-case`, `catch` — exception handling
   - `macroexpand` — returns expanded form
   - `run-hook-with-args*` — hook dispatch
   - `make-interpreted-closure` — closure construction
   - `backtrace-*` — debugging internals

   **Can improve:**
   - `funcall-with-delayed-message` → same status as funcall
     (special-cased or legitimately any)
   - `handler-bind-1` → internal, ok as any
   - `default-toplevel-value` → returns stored value
   - `buffer-local-toplevel-value` → returns stored value

2. Document the legitimately dynamic cases in BUGS.md.

**Files:**
- `typings/emacs/31.0/c-core/eval.tart` — improve where possible
- `typings/emacs/BUGS.md` — add entries

**Verify:** `nix develop --command dune build`; `nix develop --command dune test`

---

## Iteration 4: Spec status update

**What to do:**

1. Check the [R5,R6] task box — BUGS.md structure already exists
2. Check [R1-R4] boxes for data.tart, fns.tart, eval.tart, alloc.tart
3. Check [R7,R8] box — BUGS.md entries use correct format/categories
4. Add Status section to spec

**Files:**
- `specs/32-emacs-core-typings.md` — check boxes, update status

**Verify:** All tests pass
