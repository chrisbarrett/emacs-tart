# Spec 85 — Condition-Case Return Typing

## Overview

`condition-case` and `catch` produce values that are unions of their body
expression and handler/thrown values, but tart currently types them imprecisely.
This spec adds dedicated inference for `condition-case` (and
`condition-case-unless-debug`) to compute the return type as the union of the
body type and all handler body types, with the error variable correctly bound
in handler scopes.

## condition-case Inference

### Elisp Semantics

```lisp
(condition-case VAR
    BODYFORM
  (CONDITION-NAME-1 HANDLER-BODY-1...)
  (CONDITION-NAME-2 HANDLER-BODY-2...)
  ...)
```

The return value is either the result of `BODYFORM` (if no error is signalled)
or the result of one of the `HANDLER-BODY` sequences (if a matching error is
caught). The overall type is therefore the union of all these possibilities.

### Inference Rule

```
  Γ ⊢ body : T_body
  Γ, var : (cons symbol any) ⊢ handler₁ : T₁
  ...
  Γ, var : (cons symbol any) ⊢ handlerₙ : Tₙ
────────────────────────────────────────────────
  Γ ⊢ (condition-case var body h₁ ... hₙ) : (T_body | T₁ | ... | Tₙ)
```

### Handler Variable Binding

When `VAR` is non-nil, it is bound in each handler body to the error data.
Emacs Lisp error data is a cons cell of the error symbol and associated data:

```lisp
(condition-case err
    (/ 1 0)
  (arith-error
    (message "Error: %s" (car err))  ;; err : (cons symbol any)
    nil))
```

The type of `VAR` is `(cons symbol any)`:
- `car` of the error data is the error symbol (a symbol)
- `cdr` is the associated data (any type, typically a list)

When `VAR` is `nil` (omitted), no variable is bound in handler bodies.

### condition-case-unless-debug

`condition-case-unless-debug` has identical syntax and semantics to
`condition-case` for type-checking purposes. The only difference is runtime
behaviour when `debug-on-error` is set. The same inference rule applies.

## Implementation

### infer_condition_case in infer.ml

Add a function `infer_condition_case` in `lib/typing/infer.ml` that:

1. Infers the body expression type `T_body`
2. For each handler clause `(CONDITION BODY...)`:
   a. If `VAR` is non-nil, extend the environment with
      `VAR : (cons symbol any)`
   b. Infer the type of the handler body (last expression in `BODY...`)
      as `Tᵢ`
3. Returns the union `T_body | T₁ | ... | Tₙ`

### Form Recognition

Register `condition-case` and `condition-case-unless-debug` in the form
recogniser in `check.ml` to dispatch to `infer_condition_case` instead of
treating them as regular function calls.

### Error Condition Names

The `CONDITION-NAME` in each handler is a symbol or list of symbols naming
error conditions. For type-checking purposes, the condition names are not
used — they affect which errors are caught at runtime, not the types involved.
The checker does not validate that condition names correspond to defined error
conditions.

## catch/throw

### Elisp Semantics

```lisp
(catch TAG BODY...)
```

Returns the value of the last form in `BODY`, or the value passed to `throw`
if a matching tag is thrown. The return type is the union of the body type and
all possible thrown values — but since `throw` can occur anywhere (including
in called functions), the thrown value type is not statically knowable in
general.

### Typing Approach

Type `catch` as returning `any`. Unlike `condition-case`, where handlers are
lexically visible and their types can be inferred, `throw` calls may occur in
any function called from the body. Tracking throw types across function
boundaries would require effect typing, which is out of scope.

The `catch` signature in the typings files should be:

```lisp
(defun catch (symbol &rest any) -> any)
```

## Examples

### condition-case with recovery

```lisp
(condition-case nil
    (read-from-string input)     ;; returns (cons any int)
  (invalid-read-syntax nil)      ;; returns nil
  (end-of-file ""))              ;; returns string
;; Type: ((cons any int) | nil | string)
```

### condition-case with variable

```lisp
(condition-case err
    (json-parse-string s)        ;; returns any
  (json-parse-error
    (error "Parse failed: %s" (cdr err))))  ;; returns never (error signals)
;; Type: (any | never) = any
```

## Key Files

| File | Role |
|:-----|:-----|
| `lib/typing/infer.ml` | Add `infer_condition_case` function |
| `lib/typing/check.ml` | Register `condition-case` and `condition-case-unless-debug` forms |
| `lib/core/types.mli` | Type definitions (no changes expected) |
| `typings/emacs/31.0/c-core/eval.tart` | `catch` signature |

## Deferred

- **catch/throw tracking.** Tracking which values are thrown to which tags
  across function boundaries would require an effect system. Out of scope.
- **Exception exhaustiveness.** Verifying that all possible error conditions
  from a body expression are handled would require declaring which functions
  signal which errors. Out of scope.
- **Error condition hierarchy.** Emacs error conditions form a hierarchy
  (e.g. `arith-error` is a subtype of `error`). Using this hierarchy for
  narrowing the error variable type is deferred.
