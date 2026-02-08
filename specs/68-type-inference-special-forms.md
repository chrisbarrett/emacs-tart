# Spec 68 — Type Inference & Special Forms

> Consolidates specs: [34](./.archive/34-funcall-apply-typing.md), [52](./.archive/52-type-predicates.md), [49](./.archive/49-feature-guards.md), [50](./.archive/50-version-constraints.md), [55](./.archive/55-plist-intrinsic.md)

## Overview

Tart's type inference extends beyond standard Hindley-Milner to handle Emacs
Lisp's dynamic idioms: `funcall` and `apply` preserve function return types
through special-case type-checker logic; occurrence typing narrows variable
types in conditional branches via type predicates declared through
[multi-clause signatures](./70-multi-clause-dispatch.md); feature
guards (`featurep`, `fboundp`, `boundp`, `require`) prove symbol availability
at compile time; version constraints from typings directories and
`Package-Requires` headers catch API misuse across Emacs releases; and the
`plist` intrinsic provides directional subsumption with structural promotion
for property lists.

## Funcall & Apply

Tart treats `funcall` and `apply` as type-checker intrinsics, not as functions
with signatures. This preserves the return type from the function argument
rather than collapsing to `any`.

### Dual namespace environment

Emacs Lisp is a Lisp-2: function bindings (`defun`, `defalias`) and variable
bindings (`let`, `setq`, `defvar`) occupy separate namespaces. The type
environment tracks both. `#'name` and `'name` look up the function namespace;
a bare `name` in expression position looks up the variable namespace.

Sharp-quote (`#'`) and regular quote (`'`) are semantically equivalent for
type-checking. Regular quote in funcall/apply position emits a style warning
recommending sharp-quote.

### Funcall inference

`(funcall f arg1 arg2 ...)` infers the type of `f`, verifies it is a function
type `(-> (T1 T2 ...) R)` matching the arguments, and returns `R`.

```elisp
(funcall #'+ 1 2 3)       ; + : (-> (&rest Int) Int), result: Int
(funcall #'cons 1 '(2))   ; cons : (-> (a (List a)) (List a)), result: (List Int)
```

When `f` is not a function type or arguments do not match, a type error is
produced. Unification never silently widens to a top type.

### Apply inference

`(apply f args... list)` checks fixed arguments against the function type,
then checks the trailing list argument against the rest parameter or, for
fixed-arity functions, destructures it as a tuple.

- **Rest-arg functions:** fixed args and list elements unify with the rest
  element type.
- **Fixed-arity functions with tuple argument:** combined arity of fixed args
  plus tuple length must match; positional types must align.

### Tuple-list subtyping

`(Tuple T1 T2 ... Tn)` is a subtype of `(List T)` when all `Ti` unify with
`T`. List literals in apply position are inferred as tuple types with
per-element types when the function has fixed arity.

### Union function types

When `f` has a union function type (e.g. from an `if` selecting between two
functions), arguments must satisfy all variants. The result type is the union
of return types.

```elisp
(let ((f (if condition #'1+ #'1-)))
  ;; f : (Or (-> (Int) Int) (-> (Int) Int)) = (-> (Int) Int)
  (funcall f 5))              ; result: Int
```

## Occurrence Typing

Type predicates narrow variable types in conditional branches. Predicates are
declared via [multi-clause signatures](./70-multi-clause-dispatch.md)
rather than a dedicated syntax. The type checker derives predicate information
from the return type pattern: a clause returning `t` for a specific input type
alongside a catch-all clause returning `nil` establishes a predicate.

```lisp
(defun stringp ((string) -> t) ((_) -> nil))
```

### Narrowing rules

| Branch | Operation | Effect |
|:-------|:----------|:-------|
| Then   | Intersection | `T & S` where `S` is the predicate's target type |
| Else   | Subtraction  | `T - S` |

```elisp
(if (stringp x)
    (upcase x)    ; x : String
  (other x))      ; x : (T - String)
```

### Control flow integration

- **cond:** each clause cumulatively subtracts previous predicates.
- **and:** all narrowings combine for later operands.
- **or with `never`:** `(or (stringp x) (error "bad"))` narrows `x` to
  `String` in subsequent code because `error` returns the bottom type `never`.
- **Inline only:** storing a predicate result in a variable does not propagate
  narrowing.

```elisp
(let ((result (stringp x)))
  (when result ...))   ; x NOT narrowed—inline guards only
```

### The `never` bottom type

Functions that unconditionally signal (`error`, `signal`, `throw`) return the
bottom type `never`. `never` is a subtype of every type (`never <: T`), and
`(T | never)` simplifies to `T`. This enables or-expression narrowing: when
the alternative branch returns `never`, the predicate branch's narrowing
applies to subsequent code.

### Standard library predicates

Declared in `typings/emacs/*/c-core/data.tart` using multi-clause syntax:

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
(defun null ((nil) -> t) ((_) -> nil))
(defun booleanp ((bool) -> t) ((_) -> nil))
(defun atom (((cons any any)) -> nil) ((_) -> t))

;; Multi-type predicates
(defun sequencep
  (((list any)) -> t) (((vector any)) -> t) ((string) -> t)
  ((_) -> nil))
```

## Feature Guards

Flow-sensitive narrowing for runtime feature detection. Guards are static-only,
macro-transparent, and affect types only within their lexical scope.

### Guard patterns

| Guard                     | Proves                    | Unlocks                   |
|:--------------------------|:--------------------------|:--------------------------|
| `(featurep 'X)`           | Feature X loaded          | All types from `X.tart`   |
| `(fboundp 'f)`            | Function f bound          | Only function f           |
| `(boundp 'v)`             | Variable v bound          | Only variable v           |
| `(bound-and-true-p v)`    | Variable bound and non-nil| Variable v with type `t`  |
| `(require 'X)`            | Feature X loaded          | All types from `X.tart`   |
| `(require 'X nil t)`      | Maybe loaded              | Nothing (needs guard)     |

### Filename convention

`(featurep 'json)` resolves to `json.tart`. No registry is needed.

### Control flow propagation

Guards propagate through truthiness-aware forms:

- **if:** then-branch only.
- **cond:** each clause independent.
- **and:** propagates to later operands.
- **or/unless early-return:** proves for rest of scope
  (e.g. `(or (featurep 'json) (error "required"))` unlocks JSON types for
  subsequent code).

### Negated guards

The else branch of a feature guard does _not_ unlock the guarded types:

```elisp
(if (featurep 'json) (use-json)
  (json-parse-string "{}"))  ; Error: not available in else branch
```

### Inline guards only

Storing a guard result in a variable does not propagate:

```elisp
(let ((avail (featurep 'json)))
  (when avail (json-parse-string "{}")))  ; Error: not recognized
```

### Redundant guard warning

When a feature is built-in since the target Emacs version, guarding on it
emits a warning. Types remain available; the warning is informational only.

## Version Constraints

Typings directory paths and package headers establish the Emacs version range
in which each symbol is available. The type checker warns when code uses
symbols outside the declared range.

### Version sources

| Source | Determines |
|:-------|:-----------|
| Typings directory path (e.g. `typings/emacs/29.1/...`) | Minimum version |
| `@max-version` annotation in typings | Maximum version |
| `Package-Requires: ((emacs "29.1"))` header | Package's declared floor |
| `(include ...)` in typings | Preserves source version, not includer |

When no `Package-Requires` header exists, the detected Emacs version is used
as a fallback (warnings only).

### Constraint propagation

The effective minimum version is the maximum of all individual requirements.
If any symbol's minimum version exceeds the package's declared floor, a
warning is emitted. If a symbol's maximum version is below the package's
declared floor, a removal warning is emitted.

### Error codes

| Code  | Name               | Description                       |
|:------|:-------------------|:----------------------------------|
| E0900 | VersionTooLow      | Feature requires newer Emacs      |
| E0901 | VersionTooHigh     | Feature removed in declared Emacs |
| E0902 | VersionParseFailed | `Package-Requires` parse error    |

### Feature guard exemption

Calls wrapped in a feature guard (e.g. `(when (fboundp 'json-parse-string) ...)`)
are exempt from version warnings.

### LSP code actions

On version warnings, the LSP server offers:

- "Bump minimum Emacs version to X.Y"
- "Wrap in feature guard"
- "Downgrade minimum version to X.Y" (when `Package-Requires` could be lower)

## Plist Intrinsic

`plist` is a compiler intrinsic (like `hash-table`), not a type alias. The
prelude defines it via `(type plist [k v] (%tart-intrinsic%Plist k v))`.

### Directional subsumption

| Direction | Rule | Rationale |
|:----------|:-----|:----------|
| plist -> list | Always succeeds | A plist IS a list: `(plist k v)` widens to `(list (k \| v))` |
| list -> plist | Always fails | A bare `(list (k \| v))` provides no alternation evidence |
| cons chain -> plist | When alternating | N-tuple structure proves key-value layout |

### Structural promotion

Cons chains with alternating key-value structure promote to plist:

```elisp
'(:name "Alice" :age 30)
;; (cons keyword (cons string (cons keyword (cons int nil))))
;; Promotes to: (plist keyword (string | int))
```

Even positions (0, 2, ...) must unify with the key type; odd positions
(1, 3, ...) must unify with the value type. The chain must have even length.

### Row expansion

Row-typed plists expand to the intrinsic form:

```
(plist {:name string & r}) -> (Plist keyword {row})
```

### Truthiness

Plist values are always truthy (like hash-tables).

## Deferred

- Version constraint LSP code actions (auto-bump, add-guard, downgrade):
  specified but may not be fully implemented.
