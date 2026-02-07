# Known Issues and Gaps

This document tracks known limitations and bugs in the tart type checker.

## Type System Gaps

### BUG-001: No union subtyping

**Status:** FIXED
**Severity:** High
**Impact:** Many Emacs C-core signatures expect `(T | nil)` but callers often
pass just `T`. This caused false positive type errors.

**Example:**
```elisp
(message "Hello")  ; Previously: Error: expected (string | nil), found string
                   ; Now: OK (string matches (string | nil))
```

**Fix implemented:** Added asymmetric union handling in `lib/typing/unify.ml`:
1. `TUnion ts, t` (union expected, non-union provided): succeeds if t matches
   any member of the union. This is the safe direction.
2. `t, TUnion ts` (non-union expected, union provided): fails, preserving
   soundness (nil might be passed at runtime).

Added `is_union : typ -> bool` helper to `lib/core/types.ml`

---

## C-Core Typing Gaps

### GAP-001: Functions not in c-core yet

The following commonly used functions are defined in Emacs C source files not
yet typed:

- `start-process`, `call-process` — in callproc.c (not process.c)
- `sit-for` — in dispnew.c (not keyboard.c)
- `yes-or-no-p`, `y-or-n-p` — in fns.c (not minibuf.c)
- `propertize` — in fns.c (not textprop.c)
- `message`, `format` — in editfns.c (covered, but may need attention)

---

### GAP-002: `nil` not recognized as literal

**Status:** FIXED
**Severity:** Medium

The symbol `nil` is now recognized as a literal with type `Nil`.
The symbol `t` is also recognized as a literal with type `T`.

**Fix implemented:** Added special cases in `lib/typing/infer.ml` for `nil` and
`t` symbols to return `Prim.nil` and `Prim.t` respectively.

**Note:** `nil` as empty list (`(cons 1 nil)`) needs separate handling since
`Nil` is not the same as `(List a)`. This is expected behavior for now.

---

## Self-Typing Gaps

Running `tart check` on the checker's own Elisp (`lisp/tart.el`,
`lisp/tart-mode.el`) reveals several categories of unsupported
constructs. Signatures in `lisp/tart.tart` and `lisp/tart-mode.tart`
are correct; the errors below are type-checker limitations.

### GAP-003: `defmacro` not supported

**Severity:** Medium
**Files affected:** `lisp/tart.el`

The checker does not recognise `defmacro` as a defining form. Each
`defmacro` body generates UNDEFINED VARIABLE errors for the macro
name, parameters, and `&rest`.

```elisp
(defmacro tart (type-or-types &rest args) ...)
;; 4 UNDEFINED VARIABLE errors per defmacro
```

**Workaround:** Declare as `defun` in the `.tart` file, which
suppresses "unknown function" errors from call sites.

### GAP-004: `defconst` not supported

**Severity:** Low
**Files affected:** `lisp/tart-mode.el`

`defconst` is not recognised as a defining form, producing
UNDEFINED VARIABLE errors for the constant name.

```elisp
(defconst tart--prompt-regexp (rx ...) "...")
;; UNDEFINED VARIABLE: tart--prompt-regexp
```

### GAP-005: `define-derived-mode` not supported

**Severity:** Medium
**Files affected:** `lisp/tart-mode.el`

`define-derived-mode` is not recognised. The checker sees the mode
name, parent mode, and docstring as UNDEFINED VARIABLE references.

```elisp
(define-derived-mode tart-signature-mode lisp-mode "Tart" ...)
(define-derived-mode inferior-tart-mode comint-mode "Tart REPL" ...)
```

### GAP-006: `rx` macro not supported

**Severity:** Low
**Files affected:** `lisp/tart-mode.el`

The `rx` macro uses its own DSL (`bol`, `group`, `+`, `not`, `any`,
`digit`, `or`). The checker tries to resolve these as Elisp variables,
producing many UNDEFINED VARIABLE errors.

```elisp
(rx bol (group (+ (not (any " \n")))) ":" (group (+ digit)) ...)
;; ~10 UNDEFINED VARIABLE errors per rx form
```

### GAP-007: `defcustom` name/type conflict

**Severity:** Medium
**Files affected:** `lisp/tart-mode.el`

When a `defvar` signature gives a variable a type richer than
`symbol` (e.g. `(symbol | string)`), the checker reports a TYPE
MISMATCH because `defcustom`'s built-in signature expects argument
1 to be `symbol`.

```elisp
;; tart-mode.tart: (defvar tart-executable (symbol | string))
;; tart-mode.el:   (defcustom tart-executable 'managed ...)
;; Error: defcustom expects argument 1 to be symbol, found (symbol | string)
```

This affects all 8 `defcustom` variables in `tart-mode.el`.

### GAP-008: `apply` loses arity information

**Severity:** Low
**Files affected:** `lisp/tart-mode.el`

`(apply #'make-comint ...)` reports WRONG ARITY because `apply`
erases the callee's arity information.

### GAP-009: Missing library signatures

**Severity:** Low
**Files affected:** `lisp/tart-mode.el`

The following libraries used by `tart-mode.el` lack `.tart` files:

- `url` — `url-retrieve`, `url-request-extra-headers`,
  `url-http-end-of-headers`
- `xdg` — `xdg-data-home`, `xdg-state-home`
- `json` — `json-parse-buffer` (C-core)
- `comint` — `comint-mode-map` variable (missing from
  existing `comint.tart`)

### GAP-010: Branch mismatch with `error`/`user-error`

**Severity:** Low
**Files affected:** `lisp/tart-mode.el`

`error` and `user-error` have return type `never`, which causes
BRANCH MISMATCH when used in `if` or `cond` branches alongside
non-never types. This is technically correct (the branch diverges)
but noisy.

```elisp
(if condition
    (user-error "...")   ;; type: never
  some-value)            ;; type: string
;; BRANCH MISMATCH: never vs string
```

---

## Parser Gaps

(None documented yet)

---

## LSP Gaps

(None documented yet)
