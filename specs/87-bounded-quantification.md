# Spec 87 — Bounded Quantification

## Overview

When a polymorphic function has a rest parameter typed as a union (e.g. `concat`
accepting `(&rest (string | symbol | (list int) | (vector int)))`), the
constraint solver currently infers equality constraints (`a = T`) for caller
parameters, poisoning them with the full union type. This spec replaces equality
constraints with upper-bound constraints (`a <: T`) during parameter inference
from rest-parameter union types, enabling callers to pass any subtype without
acquiring the full union.

## Problem

### concat Example

The ideal signature for `concat` is:

```lisp
(defun concat (&rest (string | symbol | (list int) | (vector int))) -> string)
```

But when a function calls `concat` with a parameter:

```lisp
(defun f (x)
  (concat x))
```

The constraint solver infers `x = (string | symbol | (list int) | (vector int))`
as an equality constraint. This means callers of `f` must pass exactly the union
type — passing a plain `string` fails because `string` is not equal to the full
union. The workaround is to keep `concat` as `(&rest any) -> string`, losing the
ability to detect invalid argument types.

### General Pattern

This problem affects any polymorphic function with a rest parameter typed as a
union:

```lisp
(defun some-fn (&rest (int | string)) -> truthy)

(defun g (x)
  (some-fn x))
;; x should be inferred as [a <: (int | string)] a
;; not as x = (int | string)
```

## Bounded Type Variables

### Concept

Instead of `a = T`, the constraint solver should produce `a <: T` (an upper
bound). The inferred type of a function parameter becomes:

```
[a <: (string | symbol | (list int) | (vector int))] a
```

This means `a` can be instantiated to any subtype of the bound:

- `string` — valid, `string <: (string | symbol | ...)`
- `symbol` — valid
- `int` — invalid, `int` is not a member of the union

### Formal Notation

A bounded type scheme extends the standard `∀a. T` with upper bounds:

```
∀(a <: U). T
```

where `U` is the upper bound on `a`. During instantiation, `a` is replaced with
a fresh type variable constrained by `a <: U`. Unification of this fresh
variable with a concrete type `S` succeeds when `S <: U`.

## Implementation

### Constraint Representation

Extend the constraint type in `lib/typing/unify.ml` to distinguish between:

1. **Equality constraints** (`a = T`) — the existing behaviour, used for
   non-union unification
2. **Upper-bound constraints** (`a <: T`) — new, used when a type variable is
   unified with a union type from a rest parameter position

The constraint representation could use a variant type:

```ocaml
type constraint =
  | Eq of tvar * ty
  | Upper_bound of tvar * ty
```

### Constraint Generation

In `unify.ml`, when unifying a type variable `a` with a union type `T` that
originates from a rest parameter position:

- Instead of adding `a = T`, add `a <: T`
- The "rest parameter origin" information must be threaded through the
  unification context, so that non-rest-parameter unions still produce equality
  constraints

### Generalization

In `lib/typing/generalize.ml`, when generalizing a type variable `a` that has an
upper-bound constraint `a <: U`:

1. Include the bound in the type scheme: `∀(a <: U). T`
2. During instantiation, create a fresh variable `a'` with bound `U`
3. When `a'` is later unified with a concrete type `S`, check `S <: U`

### Instantiation

When instantiating a bounded type scheme at a call site:

1. Replace each `∀(a <: U). ...` with a fresh type variable `a'`
2. Record the constraint `a' <: U`
3. When `a'` is unified with a concrete type `S`, verify `S <: U`
4. If `S` is not a subtype of `U`, report a type mismatch

## concat Signature

Once bounded quantification is implemented, tighten the `concat` signature in
`typings/emacs/31.0/c-core/fns.tart`:

```lisp
(let-type concatable (string | symbol | (list int) | (vector int)))

(defun concat (&rest concatable) -> string)
```

This replaces the current `(&rest any) -> string`, enabling the checker to
reject invalid arguments (e.g. passing a float to `concat`) while allowing
callers with string parameters to pass them without type errors.

## Examples

### Basic usage

```lisp
(defun greet (name)
  (concat "Hello, " name))
;; Inferred: name : [a <: (string | symbol | ...)] a
;; Callers can pass: (greet "world") ✓  (greet 'world) ✓  (greet 42) ✗
```

### Chained calls

```lisp
(defun wrap (s)
  (concat "[" s "]"))

(defun wrap-name (name)
  (wrap name))
;; wrap: name : [a <: (string | ...)] a
;; wrap-name: name : [a <: (string | ...)] a (bound propagates)
```

### Non-rest parameters (unchanged)

```lisp
(defun f (x)
  (+ x 1))
;; x : int (equality constraint, not bounded — + has fixed params)
```

## Key Files

| File                                 | Role                                                                |
| :----------------------------------- | :------------------------------------------------------------------ |
| `lib/typing/unify.ml`                | Extend constraint representation; upper-bound constraint generation |
| `lib/typing/generalize.ml`           | Bounded type scheme generalization and instantiation                |
| `lib/typing/infer.ml`                | Thread rest-parameter origin through unification                    |
| `lib/core/types.mli`                 | Extend type scheme representation with bounds                       |
| `typings/emacs/31.0/c-core/fns.tart` | Tighten `concat` signature                                          |

## Deferred

- **General bounded polymorphism syntax.** A user-facing syntax for declaring
  bounded type variables in `.tart` signatures (e.g.
  `(defun f [(a : (int | string))] (a) -> a)`) is not part of this spec. Bounded
  quantification here is inferred, not declared.
- **Multiple bounds.** A type variable with multiple upper bounds (`a <: T₁` and
  `a <: T₂`) would require intersection types. Deferred.
- **Lower bounds.** Constraints of the form `T <: a` (lower bounds) are not
  addressed. These would be needed for contravariant positions.
