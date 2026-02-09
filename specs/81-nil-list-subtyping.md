# Spec 81 — Nil-List Subtyping

## Overview

Emacs Lisp treats `nil` as the empty list: `(listp nil)` returns `t`. Tart's
type system models `nil` and `(list a)` as distinct types, causing false
positives wherever code passes `nil` as a list argument or applies list
operations to values that might be `nil`. This spec adds a subtyping rule
`nil <: (list a)` for all `a`, and broadens `car`/`cdr` to accept values that
include `nil` in their union type without requiring the caller to narrow first.

## Nil-List Subtyping Rule

### Rule

Add the subtyping judgement:

```
nil <: (list a)   for all a
```

This reflects the Emacs Lisp semantics where `nil` is the empty list. The rule
is unconditional — `nil` is a valid `(list a)` regardless of the element type
`a`, because the empty list contains no elements whose type could conflict.

### Implementation

In `unify.ml`, the subtyping check currently treats `Nil` and `TList _` as
incompatible. Add a case to the subtyping relation:

```
| (TNil, TList _) -> success
```

This case fires before the structural unification fallback, so `nil` unifies
with any `(list a)` without constraining `a`.

### Effects

Patterns that currently produce false positives will type-check:

```lisp
(append sequence nil)       ;; nil as (list a) argument
(nreverse result)           ;; result initialized to nil
(let ((xs nil)) (push x xs) xs)  ;; nil as initial list value
```

The rule also means `(list a)` remains equivalent to
`((cons a (list a)) | nil)` — `nil` is already the base case of the recursive
list type. The new subtyping rule makes this equivalence usable in practice.

## Broadening car/cdr

### Problem

`car` and `cdr` are typed via [multi-clause dispatch](./70-multi-clause-dispatch.md):

```lisp
(defun car [a b]
  (((cons a b)) -> a)
  ((nil) -> nil))
```

This rejects calls where the argument has a union type that includes non-list
members, e.g. `(Or symbol keyword int t nil)`. In `minibuffer.el` this accounts
for 50 `car` and 22 `cdr` arity errors; in `window.el`, 4 `car` errors. The
root cause is that variables bound via `let` with complex control flow acquire
wide union types rather than the narrower type the programmer intends.

### Solution

Add a fallback clause accepting `any` with `any` return:

```lisp
(defun car [a b]
  (((cons a b)) -> a)
  ((nil) -> nil)
  ((any) -> any
    (note "car applied to non-list type; result is any")))
```

The first two clauses preserve precise typing for `(cons a b)` and `nil`
arguments. The third clause handles union-typed values where `car` is applied
dynamically. The `note` diagnostic alerts users that type precision is lost
without blocking type checking.

The same pattern applies to `cdr`:

```lisp
(defun cdr [a b]
  (((cons a b)) -> b)
  ((nil) -> nil)
  ((any) -> any
    (note "cdr applied to non-list type; result is any")))
```

### Signature Files

Update the `car`/`cdr` signatures in:

- `typings/emacs/31.0/c-core/data.tart`
- Any version-specific overrides that define `car`/`cdr`

## Key Files

| File | Role |
|:-----|:-----|
| `lib/typing/unify.ml` | Add `nil <: (list a)` subtyping rule |
| `lib/core/types.mli` | Type definitions (`TNil`, `TList`) |
| `typings/emacs/31.0/c-core/data.tart` | `car`/`cdr` signature updates |
| `typings/tart-prelude.tart` | `list` type definition (unchanged) |

## Deferred

- **Non-nil list refinement.** A `(nonempty a)` type already exists in the
  prelude as `(cons a (list a))`. Full flow-sensitive narrowing from `(list a)`
  to `(nonempty a)` after a nil check is not part of this spec.
- **setcar/setcdr narrowing.** Patterns like `(setcar (cdr xs) val)` fail
  because `cdr` returns `(list a)` which might be `nil`. Narrowing `(list a)`
  to `(cons a (list a))` after a non-nil check would resolve this, but requires
  flow-sensitive occurrence typing beyond what this spec addresses.
