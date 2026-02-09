# Spec 83 — Function Subtype Widening

## Overview

A function type `(a₁ ... aₙ) -> r` is not currently recognised as a subtype of
`(&rest T) -> R`, even when each `aᵢ <: T` and `r <: R`. This causes
`add-hook` and `remove-hook` — whose FN parameter is typed as
`(symbol | ((&rest any) -> any))` — to reject lambda expressions with fixed
arities. In `minibuffer.el` alone, 6 `add-hook` and 7 `remove-hook` calls fail
because the lambda has 0–3 fixed parameters. This spec adds the missing
subtyping rule.

## Subtyping Rule

### Formal Rule

```
  ∀i. aᵢ <: T      r <: R
────────────────────────────────
(a₁ ... aₙ) -> r  <:  (&rest T) -> R
```

A fixed-arity function is a subtype of a rest-parameter function when:

1. Every fixed parameter type `aᵢ` is a subtype of the rest parameter type `T`
2. The return type `r` is a subtype of the expected return type `R`

The rule also covers functions with mixed fixed and rest parameters:

```
  ∀i. aᵢ <: T    S <: T    r <: R
──────────────────────────────────────
(a₁ ... aₙ &rest S) -> r  <:  (&rest T) -> R
```

### Zero-Arity Case

A nullary function `() -> r` is a subtype of `(&rest T) -> R` when `r <: R`.
The fixed-parameter constraint is vacuously satisfied since there are no `aᵢ`.

### Optional Parameters

Functions with `&optional` parameters also satisfy the rule. Each optional
parameter type is checked against `T` in the same way as fixed parameters:

```
  ∀i. aᵢ <: T    ∀j. oⱼ <: T    r <: R
────────────────────────────────────────────
(a₁ ... aₙ &optional o₁ ... oₘ) -> r  <:  (&rest T) -> R
```

## Implementation

### unify_param_lists in unify.ml

The function `unify_param_lists` in `lib/typing/unify.ml` handles unification
of function parameter lists. Add a case for when the expected parameter list
has a single rest parameter and the actual parameter list has fixed (and
optionally rest/optional) parameters:

1. For each fixed parameter `aᵢ` in the actual type, check `aᵢ <: T`
2. For each optional parameter `oⱼ` in the actual type, check `oⱼ <: T`
3. If the actual type has a rest parameter `S`, check `S <: T`
4. Check the return type `r <: R`

If all checks pass, the function types unify. If any check fails, report a
type mismatch with the specific parameter that failed.

### Contravariance Note

Standard function subtyping is contravariant in parameter types: `(A -> R)` is
a subtype of `(B -> R)` when `B <: A` (parameters flip). However, for the
rest-parameter widening rule, the check is `aᵢ <: T` (covariant), not
`T <: aᵢ`. This is because the rest parameter `T` represents the type of
*each element* in the rest list, and the fixed-arity function promises to
accept specific types — callers passing values of type `T` must be able to
rely on the function accepting them. Since `aᵢ <: T`, any value of type `aᵢ`
is also of type `T`, so the function is safely callable.

In practice, `T` is almost always `any` in Emacs Lisp hook signatures, making
the direction moot: every type is a subtype of `any`.

## Motivating Examples

### add-hook with lambda

```lisp
;; Currently fails: (lambda () ...) is not <: (&rest any) -> any
(add-hook 'after-init-hook (lambda () (load-theme 'modus-vivendi)))

;; After this spec: () -> any <: (&rest any) -> any ✓
```

### remove-hook with named function

```lisp
;; defun with fixed arity
(defun my-hook-fn (arg)
  (message "got %s" arg))

;; Currently fails: (any) -> string is not <: (&rest any) -> any
(remove-hook 'some-hook #'my-hook-fn)

;; After this spec: (any) -> string <: (&rest any) -> any ✓
```

## Key Files

| File | Role |
|:-----|:-----|
| `lib/typing/unify.ml` | `unify_param_lists` — add rest-parameter subtyping case |
| `lib/core/types.mli` | Function type representation (`TArrow`, param list types) |
| `typings/emacs/31.0/c-core/eval.tart` | `add-hook`/`remove-hook` signatures |

## Deferred

- **Full contravariant function subtyping.** This spec adds only the
  rest-parameter widening rule, not general function subtyping. Full
  contravariant parameter / covariant return subtyping for all function types
  is a separate, larger change.
- **Arity checking at hook call sites.** When a hook is invoked via
  `run-hook-with-args`, the arguments passed should match the arity of each
  registered function. This requires tracking hook function types, which is
  out of scope.
