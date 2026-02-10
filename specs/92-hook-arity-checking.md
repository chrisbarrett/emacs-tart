# Spec 92 — Hook Arity Checking

## Overview

Emacs hooks have implicit arity contracts: `after-save-hook` calls its
functions with zero arguments, `after-change-functions` calls with three.
Tart cannot enforce these contracts because hook variables are typed as
`(list any)` or `(list ((&rest any) -> any))`, erasing the expected
function arity. When `run-hook-with-args` is called, the checker cannot
verify that the passed arguments match what registered functions expect.

This spec introduces **typed hook variables** in `.tart` signature files
and validates `add-hook` calls against the hook's declared function type.

## Typed Hook Variables

### Signature Convention

Hook variables declare their expected function type as the list element
type:

```lisp
;; Normal hook (zero arguments)
(defvar after-save-hook (list ((nil) -> any)))

;; Abnormal hook (specific arguments)
(defvar after-change-functions (list ((int int int) -> any)))

;; Hook with &optional
(defvar window-size-change-functions (list (((frame | nil)) -> any)))
```

The function type in the list element position is the **hook contract**: any
function added to this hook must be compatible with this signature.

### Existing State

Several hook variables already use typed function elements in the typings
files:

- `flymake-mode-hook`: `(list ((nil) -> any))`
- `compilation-start-hook`: `(list ((process) -> any))`
- `eglot-connect-hook`: `(list ((any) -> any))`

Most hooks are typed as `(list any)`, which opts out of arity checking.

## Validation at `add-hook`

### Current Signature

```lisp
(defun add-hook (symbol (symbol | ((&rest any) -> any)) &optional (int | nil) bool) -> nil)
```

The HOOK parameter is `symbol` — the checker resolves it to a variable name
but does not inspect the variable's type. The FN parameter accepts any
function via `(&rest any) -> any`.

### New Behaviour

When the checker encounters `(add-hook 'hook-name fn)`:

1. Look up `hook-name` in the type environment.
2. If the hook variable's type is `(list F)` where `F` is a function type
   (`TArrow`), extract `F` as the **hook contract**.
3. Check that `fn`'s type is a subtype of `F` using the existing subtyping
   rules ([Spec 83](83-function-subtype-widening.md) rest-parameter
   widening, [Spec 90](90-contravariant-function-subtyping.md)
   contravariant params).
4. If the hook variable's type is `(list any)` or is not a `(list F)` form,
   skip validation (opt-out).

### Constraint

The first argument to `add-hook` must be a **quoted symbol** for the
checker to resolve the hook variable. Dynamic hook names
(`(add-hook hook-var fn)` where `hook-var` is a variable) skip validation.

## Validation at `run-hook-with-args`

### Current Signature

```lisp
(defun run-hook-with-args (symbol &rest any) -> any)
```

The arguments after the hook name are untyped.

### New Behaviour

When the checker encounters `(run-hook-with-args 'hook-name arg1 arg2 ...)`:

1. Look up `hook-name` in the type environment.
2. If the hook variable's type is `(list (P1 P2 ... -> R))`, extract the
   parameter types.
3. Check each argument `argi` against the corresponding parameter type `Pi`.
4. If the hook type is `(list any)`, skip validation.

This catches cases where `run-hook-with-args` is called with the wrong
number or type of arguments.

## Examples

### Arity mismatch at `add-hook`

```lisp
;; Hook expects (int int int) -> any
;; (defvar after-change-functions (list ((int int int) -> any)))

(add-hook 'after-change-functions (lambda () (message "changed")))
;; After this spec: TYPE MISMATCH
;; lambda () has 0 params, hook expects 3
```

### Correct usage

```lisp
(add-hook 'after-change-functions
          (lambda (beg end len) (message "changed %d" len)))
;; PASS: (int int int) -> any matches hook contract
```

### Untyped hook (opt-out)

```lisp
;; (defvar my-hook (list any))
(add-hook 'my-hook (lambda () (do-something)))
;; PASS: no validation (hook type is (list any))
```

## Implementation

### Intrinsic for `add-hook`

Make `add-hook` an intrinsic in `infer.ml` (or a post-inference validation
pass). When the first argument is a quoted symbol:

1. Resolve the symbol in the type environment to get the hook variable type.
2. Extract the function type from `(list F)` via pattern matching on the
   resolved type.
3. Generate an additional subtype constraint between the FN argument and `F`.

The standard `add-hook` signature remains for cases where the hook name
is not a quoted symbol.

### Intrinsic for `run-hook-with-args`

Similarly, make `run-hook-with-args`, `run-hook-with-args-until-success`,
and `run-hook-with-args-until-failure` intrinsics that extract the hook
contract and validate arguments.

### Gradual Adoption

Since most hook variables are currently `(list any)`, the validation is
opt-in per hook. As typings files are updated with precise hook function
types, more hooks gain arity checking without code changes.

## Key Files

| File | Role |
|:-----|:-----|
| `lib/typing/infer.ml` | `add-hook` and `run-hook-with-args` intrinsics |
| `lib/typing/type_env.ml` | Variable type lookup for hook resolution |
| `typings/emacs/31.0/lisp-core/subr.tart` | `add-hook`/`remove-hook` signatures |
| `typings/emacs/31.0/c-core/eval.tart` | `run-hook-with-args*` signatures |
| `typings/emacs/31.0/lisp-core/*.tart` | Hook variable declarations |

## Future Work

- **Hook variable inference.** When `defvar` initialises a hook with a
  list of functions, infer the hook contract from the function types.
- **`define-minor-mode` hook typing.** `define-minor-mode` generates a
  mode hook variable; the generated signature could declare the hook
  function type automatically.
