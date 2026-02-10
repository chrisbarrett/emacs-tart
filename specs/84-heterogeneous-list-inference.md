# Spec 84 — Heterogeneous List Inference

## Overview

The `list` function is polymorphic `[a] (&rest a) -> (list a)`, but Emacs Lisp
frequently builds heterogeneous lists for code-as-data patterns:
`(list 'setq place value)`. The first argument `'setq` (a symbol) fixes
`a = symbol`, then subsequent non-symbol arguments fail. This spec makes `list`
a type-checker intrinsic that infers `TTuple [t₁; ...; tₙ]` when arguments have
different types, leveraging the existing `TTuple`-to-`TList` subtyping in
`unify.ml`.

## Intrinsic Handling

### Current Behaviour

`list` is currently a regular polymorphic function resolved through the
signature environment. The single type variable `a` is unified across all
arguments, forcing them to share a common type. When arguments have different
types, unification either fails or produces a union type that is too wide.

### New Behaviour

Make `list` a type-checker intrinsic in `infer.ml`, handled alongside existing
intrinsics like `funcall` and `apply`. When the checker encounters a `list`
call:

1. Infer the type of each argument independently: `t₁, t₂, ..., tₙ`
2. If all types unify to a single type `a`, return `(list a)` (existing
   behaviour preserved)
3. If types are heterogeneous, return `TTuple [t₁; t₂; ...; tₙ]`

### Homogeneity Check

The check for whether arguments are homogeneous uses unification, not equality.
Types that unify (e.g. `int` and `int`, or `'foo` and `symbol`) produce a
homogeneous list. Types that do not unify (e.g. `symbol` and `int`) produce
a tuple.

The unification attempt must not produce side effects on the type environment.
Use a trial unification (snapshot and rollback) to test homogeneity without
committing constraints.

## Tuple-List Subtyping

The existing subtyping rule in `unify.ml` already handles `TTuple`-to-`TList`
coercion:

```
TTuple [t₁; ...; tₙ]  <:  (list a)   when  ∀i. tᵢ <: a
```

This means a `TTuple [symbol; int; string]` can be passed where `(list any)`
is expected, because `symbol <: any`, `int <: any`, and `string <: any`.

No changes to the subtyping relation are needed.

## Examples

### Code-as-data pattern

```lisp
(list 'setq place value)
;; Currently: fails (symbol ≠ type-of-place ≠ type-of-value)
;; After: TTuple [symbol, type-of-place, type-of-value]
```

### Homogeneous case (unchanged)

```lisp
(list 1 2 3)
;; Still inferred as: (list int)
```

### Mixed numeric types

```lisp
(list 1 2.0 3)
;; int and float unify to num
;; Inferred as: (list num)
```

### Macro-building patterns

```lisp
(list 'progn
      (list 'setq x 1)
      (list 'message "done"))
;; Outer: TTuple [symbol, TTuple[...], TTuple[...]]
;; Inner lists also get tuple types
```

## Implementation Notes

### Intrinsic Registration

Add `list` to the set of intrinsic functions checked in `infer.ml`'s function
application handler. The intrinsic bypasses normal clause dispatch and directly
applies the homogeneity check and tuple inference described above.

### Empty List

`(list)` with no arguments returns `nil`. This is already correct and should
remain unchanged by the intrinsic handling.

### Single Argument

`(list x)` returns `(list t)` where `t` is the type of `x`. A single-element
tuple degenerates to a regular list since there is no heterogeneity to capture.

## Key Files

| File | Role |
|:-----|:-----|
| `lib/typing/infer.ml` | Add `list` intrinsic handling |
| `lib/typing/unify.ml` | Existing `TTuple <: TList` subtyping (no changes needed) |
| `lib/core/types.mli` | `TTuple` type constructor definition |
| `typings/emacs/31.0/c-core/alloc.tart` | Current `list` signature (becomes fallback) |

## Future Work

- **Tuple element access.**
  [Spec 91](91-tuple-element-access.md) — accessing individual elements
  of a tuple by index (e.g. `(nth 0 tuple)`) with precise per-element
  typing. Tuples accessed via `car`/`cdr` use the existing list-typed
  fallback.
- **Tuple pattern matching.** Destructuring tuples via `pcase` with
  per-element type narrowing.
