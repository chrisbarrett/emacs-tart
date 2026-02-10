# Spec 90 — Contravariant Function Subtyping

## Overview

Standard function subtyping is contravariant in parameters and covariant in
return types: `(A -> R)` is a subtype of `(B -> R)` when `B <: A` and
`R <: R'`. Tart does not implement this rule — parameter unification is
invariant (equality). This causes false positives when callback parameters
have wider types than the function being passed.

[Spec 83](83-function-subtype-widening.md) added rest-parameter widening
(`(a1...an) -> r <: (&rest T) -> R`), but the general contravariant rule
for same-arity functions is missing.

## Subtyping Rule

### Formal Rule

```
  ∀i. Bᵢ <: Aᵢ      R_a <: R_b
──────────────────────────────────
(A₁...Aₙ) -> R_a  <:  (B₁...Bₙ) -> R_b
```

A function that accepts wider parameter types is substitutable for one that
accepts narrower types. The return type is covariant: a function returning a
more specific type is substitutable for one returning a wider type.

### Extended Rule with Optional and Rest

```
  ∀i. Bᵢ <: Aᵢ    ∀j. Dⱼ <: Cⱼ    U <: S    R_a <: R_b
───────────────────────────────────────────────────────────────
(A₁...Aₙ &optional C₁...Cₘ &rest S) -> R_a
    <:  (B₁...Bₙ &optional D₁...Dₘ &rest U) -> R_b
```

The rule requires the same parameter structure on both sides: same number of
positional, optional, and rest parameters. Mismatched structure (different
arities, rest on one side but not the other) is not eligible for
contravariant subtyping — those cases are handled by existing
rest-parameter widening ([Spec 83](83-function-subtype-widening.md)) or
rejected as arity mismatches.

## Motivating Examples

### Callback with wider parameter

```lisp
(defun process-item (handler)
  (declare (tart ((string -> nil)) -> nil))
  (funcall handler "hello"))

;; any is a supertype of string, so (any -> nil) <: (string -> nil)
(process-item (lambda (x) (message "got: %s" x)))
;; Currently: PASS (x inferred as any, which unifies with string)

;; But with an explicit wider type:
(defun my-handler (x)
  (declare (tart (any) -> nil))
  (message "got: %s" x))

(process-item #'my-handler)
;; Currently: TYPE MISMATCH (any ≠ string in parameter position)
;; After this spec: PASS (string <: any, so (any -> nil) <: (string -> nil))
```

### Narrower return type

```lisp
(defun call-producer (f)
  (declare (tart ((nil -> any)) -> any))
  (funcall f))

(defun make-greeting ()
  (declare (tart (nil) -> string))
  "hello")

(call-producer #'make-greeting)
;; Currently: TYPE MISMATCH (string ≠ any in return position)
;; After this spec: PASS (string <: any, covariant return)
```

## Implementation

### Fallback in `unify_param`

The function `unify_param` in `lib/typing/unify.ml` (line 846) currently
unifies parameter types with equality semantics. When invariant unification
of two concrete parameter types fails with a type mismatch, retry with
the expected parameter type checked as a subtype of the actual:

```
unify_param p_actual p_expected  →  fails
retry: check p_expected_type <: p_actual_type  (contravariant)
```

The retry applies only when:

1. Both parameters have the **same kind** (both positional, both optional,
   both rest, or both keyword with the same name).
2. The initial failure is a **type mismatch**, not an arity mismatch.
3. Neither type is a bare type variable — contravariant retry on tvars would
   produce surprising inference results. At least one side must be concrete
   (a `TCon`, `TApp`, `TArrow`, `TUnion`, `TTuple`, or `TLiteral`).

### Return Type

Return types are already unified covariantly by the `TArrow` case in
`unify` (line 421): `unify r1 r2`. No change needed — the existing
subtyping rules (`Int <: Num`, `T <: T|U`, `nil <: list a`, etc.) already
handle covariant return checking.

### Guard Against Inference Damage

Contravariant parameter retry must not corrupt type inference when type
variables are involved. The guard requiring at least one concrete type
prevents the unifier from flipping a tvar link direction, which would
produce unsound types.

An alternative guard: only attempt contravariant retry in the
`Subtype_check` context, not during `Constraint_solving`. This would
require extending the `unification_context` type with a `Subtype_check`
variant and threading it through function argument checking.

## Key Files

| File | Role |
|:-----|:-----|
| `lib/typing/unify.ml` | `unify_param` — add contravariant retry |
| `lib/core/types.mli` | Param type definitions (unchanged) |

## Future Work

- **Keyword parameter contravariance.** Keyword parameters with the same
  name follow the same contravariant rule, but keyword reordering between
  expected and actual types is not handled.
- **Higher-rank contravariance.** `TForall` types in parameter positions
  require impredicative instantiation for proper contravariant checking.
