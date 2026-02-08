# Spec 52: Type Predicates

Declare functions as type predicates that narrow types in conditional branches.

- **Deps:**
  - [Spec 34](./34-funcall-apply-typing.md)
  - [Spec 46](./46-truthiness-unions.md)
  - [Spec 11](./11-adt-system.md)

## Goal

Enable `stringp`, `listp`, and user-defined predicates to narrow types in conditionals via signature declarations (not hardcoded).

## Syntax (superseded)

See [Spec 54](./54-multi-clause-signatures.md) for current syntax. Original:

```lisp
(defun stringp (x) -> (x is string))
(defun listp (x) -> (x is (list any)))
```

Parameter name before `is` must match signature. Type after `is` is narrowed type when truthy.

## Constraints

| Constraint    | Detail                                    |
| ------------- | ----------------------------------------- |
| Declarative   | Declared in signatures, not hardcoded     |
| Composable    | Works with unions, negation               |
| Extensible    | User predicates supported                 |
| Compatible    | Non-predicate functions unchanged         |

## Requirements

### R1-R2: Basic narrowing

```elisp
(if (stringp x)
    (upcase x)    ; x : string
  (other x))      ; x : (original - string)
```

Then-branch: type intersection. Else-branch: type subtraction.

### R3: Cumulative narrowing in cond

```elisp
(cond
  ((stringp x) ...)   ; x : string
  ((listp x) ...)     ; x : list, already ¬string
  ((symbolp x) ...))  ; x : symbol, already ¬string ¬list
```

### R4: Predicates in `and`

```elisp
(when (and (listp x) (not (null x)))
  (car x))   ; x : nonempty list
```

All narrowings combine.

### R5: Predicates in `or` early-exit

Requires R13 (`never`-return tracking).

```elisp
(or (stringp x) (error "Expected string"))
(upcase x)   ; x : string
```

When `error` is known to return `never`, the `or` form can only continue past
the second branch if the first branch was truthy. This narrows `x` to `string`
in subsequent code.

### R6-R7: User predicates, specific parameter

```lisp
(defun my-thing-p (x) -> (x is my-thing))
(defun string-or-null-p (x default) -> (x is (string | nil)))
```

Only named parameter narrowed.

### R8: Union narrowing

```lisp
(defun number-or-marker-p (x) -> (x is (num | marker)))
```

Input `(string | int | float | marker)` → narrowed `(int | float | marker)`.

### R9: Nested checks accumulate

### R10: `(x is T)` is subtype of `bool`

### R11: Invalid parameter error

```lisp
(defun bad (x) -> (y is string))  ; Error: 'y' not in params
```

### R12: Inline-only narrowing

```elisp
(let ((result (stringp x)))
  (when result ...))   ; x NOT narrowed
```

Stored results don't narrow (same as [Spec 49](./49-feature-guards.md) R17).

## Standard Library Predicates

Declare in `typings/emacs/*/c-core/data.tart` using multi-clause syntax
([Spec 54](./54-multi-clause-signatures.md)):

```lisp
(defun stringp ((string) -> t) ((_) -> nil))
(defun symbolp ((symbol) -> t) ((_) -> nil))
(defun integerp ((int) -> t) ((_) -> nil))
(defun floatp ((float) -> t) ((_) -> nil))
(defun numberp ((num) -> t) ((_) -> nil))
(defun consp (((cons any any)) -> t) ((_) -> nil))
(defun listp (((list any)) -> t) ((_) -> nil))
(defun vectorp (((vector any)) -> t) ((_) -> nil))
(defun keywordp ((keyword) -> t) ((_) -> nil))
(defun bufferp ((buffer) -> t) ((_) -> nil))
(defun markerp ((marker) -> t) ((_) -> nil))

;; Multi-type predicates
(defun sequencep
  (((list any)) -> t) (((vector any)) -> t) ((string) -> t)
  ((_) -> nil))

;; Inverted predicate
(defun atom (((cons any any)) -> nil) ((_) -> t))

;; Nil/null
(defun null ((nil) -> t) ((_) -> nil))
(defun booleanp ((bool) -> t) ((_) -> nil))
```

### R13: `never`-return tracking

Functions that unconditionally signal (e.g., `error`, `signal`, `throw`) return
the bottom type `never`. This enables R5: when the alternative branch of an `or`
returns `never`, execution can only reach subsequent code if the predicate branch
was truthy.

```lisp
;; In typings:
(defun error (string &rest any) -> never)
(defun signal (symbol any) -> never)
(defun throw (symbol any) -> never)
```

**Requirements:**

1. `never` is a bottom type: subtype of every type (`never <: T` for all `T`)
2. `(T | never)` simplifies to `T`
3. Functions declared with return type `never` propagate non-return through
   `or`, `cond`, and `if` branches
4. When all branches of an `or` except one return `never`, the surviving
   branch's narrowing effects apply to subsequent code

**Verify:** `dune test`; `(or (stringp x) (error "bad"))` narrows `x` to
`string` after the `or`

## Non-Requirements

- Multi-parameter narrowing
- Conditional narrowing (A if arg1, B if arg2)
- Automatic inference from body
- Composition syntax

## Design Notes

### Implementation

Predicate info is now derived from multi-clause structure ([Spec 54](./54-multi-clause-signatures.md) R5)
instead of `STPredicate` AST nodes.

Narrowing applies in `infer.ml` for `if`/`cond`/`when`/`unless`:
- Detect predicate calls in condition
- Apply intersection in then-branch
- Apply subtraction in else-branch
- Narrowing is lexical scope

### Type Operations

```
narrow(T, P→S) = T ∩ S
negate(T, P→S) = T - S
```

### Integration with Feature Guards ([Spec 49](./49-feature-guards.md))

Same narrowing infrastructure, different tracking:
- Feature guards: `feature_env` → available symbols
- Type predicates: `type_env` → narrowed types per variable

## Tasks

- [x] ~~Parse `(x is T)` return syntax~~ → superseded by [Spec 54](./54-multi-clause-signatures.md)
- [x] ~~Named parameter syntax `((name type))`~~ → superseded by [Spec 54](./54-multi-clause-signatures.md)
- [x] Register predicate info in sig_loader
- [x] Predicate narrowing in if/when/unless
- [x] Type subtraction for else branches
- [x] Cumulative narrowing in cond
- [x] Predicates in and expressions
- [x] [R13] Add `never` bottom type to type system
- [x] [R13] Declare `error`, `signal`, `throw` as returning `never` in typings
- [x] [R5, R13] Predicates in or expressions — narrowing via `never`-return tracking
- [x] Union intersection for narrowing
- [x] Inline-only restriction
- [x] Standard library declarations

**Status:** Complete. All requirements implemented. R13 (`never` bottom type)
adds `never <: T` subtyping and `(T | never)` simplification. R5
(or-expression narrowing) detects `(or (pred x) (error ...))` patterns where
the alternative returns `never`, applying predicate narrowing to subsequent
code via `infer_progn` environment threading.
