# Spec 82 — Special Form Parser Extensions

## Overview

Tart's type checker does not recognise many common Emacs Lisp defining forms,
causing all bindings inside their bodies to appear as undefined variables. These
unsupported forms account for approximately 49% of all validation errors
(7,171/14,508 UNDEFINED VARIABLE errors) across the Emacs core files tested.

Rather than hardcoding each form in OCaml, this spec adds a **macro expansion
pre-pass** that rewrites unknown defining forms into forms the checker already
understands (`defun`, `defvar`, `progn`). A curated `.el` file provides
simplified macro definitions. The existing pure Elisp interpreter
([`lib/interp/`](../lib/interp/)) expands these before the type checker runs,
keeping form knowledge in user-land and making new forms a one-line Elisp
change.

This spec operates at the type-checker's form-recognition level — it
determines which Elisp forms the checker understands and how it binds variables
within them. The S-expression parser itself is covered by
[Spec 78](./78-parser-fidelity.md).

## Design

### Where

In [`module_check.ml`](../lib/typing/module_check.ml), between environment
setup and `Check.check_program` (around line 799). The expansion is a
pre-processing step — the type checker receives already-expanded S-expressions
and needs no changes to its dispatch.

### How

Create an interpreter, load curated macros from `typings/tart-macros.el`,
single-step expand each top-level form, flatten any resulting `progn` into
multiple top-level forms.

### Why not inside `check_form_with_state`?

Threading the interpreter through `check_state` would be needed to support
user-defined macros in the same file. For this spec, all target macros are
pre-defined. Pre-processing is simpler and keeps the interpreter out of the
type checker.

## Form Classification

### Defun-like forms

Forms that define a named function. The pre-pass rewrites them to `defun`.

| Form | Expansion | Notes |
|:-----|:----------|:------|
| `defsubst` | `(defun ...)` | Identical structure |
| `cl-defgeneric` | `(defun ...)` | Drop options after arglist |
| `cl-defmethod` | `(defun ...)` | Skip qualifiers, strip specializers |
| `defmacro` | `(defun ...)` | Treat as function for type checking |
| `pcase-defmacro` | `(defun ...)` | Same as defmacro |
| `gv-define-setter` | `(defun ...)` | Bind name as function |
| `gv-define-expander` | `(defun ...)` | Bind name as function |

### Variable-binding forms

Forms that introduce a named variable. The pre-pass rewrites them to `defvar`.

| Form | Expansion | Notes |
|:-----|:----------|:------|
| `defcustom` | `(defvar ...)` | Variable binding |
| `defgroup` | `(defvar ...)` | Variable binding |
| `defface` | `(defvar ...)` | Variable binding |
| `declare-function` | `(defvar ...)` | Just makes name known |
| `cl--defalias` | `(defvar ...)` | Bind name |

### Dual-binding forms

Forms that define both a function and a variable.

| Form | Expansion | Notes |
|:-----|:----------|:------|
| `define-minor-mode` | `(progn (defvar ...) (defun ...))` | Mode variable + toggle function |

### Local binding forms

Forms that introduce local variable bindings in a body, similar to `let`.

| Form | Expansion | Notes |
|:-----|:----------|:------|
| `gv-letplace` | `(let ...)` | Binds getter/setter pair in body |
| `macroexp-let2` | `(let ...)` | Binds variable to expression in body |

### No-op forms

Forms that need no bindings and can be discarded.

| Form | Expansion | Notes |
|:-----|:----------|:------|
| `set-advertised-calling-convention` | `nil` | Metadata only |

## Edge Cases

### `cl-defmethod` qualifier skipping

`cl-defmethod` accepts optional qualifiers between the name and arglist:
`:before`, `:after`, `:around`, and `:extra "string"`. The macro must skip
these to find the actual parameter list. The interpreter lacks `keywordp` —
add it as a one-line builtin in
[`builtin.ml`](../lib/interp/builtin.ml).

### `defmacro` macro ordering

The `defmacro` macro must be defined *last* in `tart-macros.el`.
`load_macros` uses the `defmacro` special form to register all other macros.
Once a macro named `defmacro` is registered, the expander would try to expand
subsequent `(defmacro ...)` forms. In practice `load_macros` uses
`eval_toplevel` which hits the special form first, and we only use `expand_1`
not `macroexpand_all`, but defining it last is defensive.

## Key Files

| File | Role |
|:-----|:-----|
| [`lib/typing/dune`](../lib/typing/dune) | Add `tart.interp` dependency |
| [`lib/interp/builtin.ml`](../lib/interp/builtin.ml) | Add `keywordp` predicate |
| `typings/tart-macros.el` | Curated macro definitions (new file) |
| [`lib/typing/module_check.ml`](../lib/typing/module_check.ml) | Expansion pre-pass |
| `test/fixtures/typing/special-forms/` | Test fixtures (new directory) |

## Test Plan

Fixture pairs in `test/fixtures/typing/special-forms/`:

| Fixture | Tests |
|:--------|:------|
| `defsubst.el` / `.expected` | Define via defsubst, call it → PASS |
| `cl-defmethod.el` / `.expected` | With qualifier and specializer → PASS |
| `define-minor-mode.el` / `.expected` | Use mode variable and function → PASS |
| `defcustom.el` / `.expected` | Reference the variable → PASS |
| `binding-forms.el` / `.expected` | gv-letplace, macroexp-let2 → PASS |
| `declaration-only.el` / `.expected` | declare-function etc. → PASS |

Verification:

1. `nix develop --command dune build 2>&1` — compiles cleanly
2. `nix develop --command dune test --force 2>&1` — all tests pass
3. `./tart check --emacs-version 31.0 <file-with-defsubst>` — no UNDEFINED
   VARIABLE for the defined name

## Impact Estimate

Based on current validation counts, addressing unsupported special forms
would eliminate approximately 7,171 UNDEFINED VARIABLE errors — 49% of all
errors across the tested files. The per-file impact:

| File | UNDEFINED VARIABLE from forms | Total UNDEFINED VARIABLE |
|:-----|------------------------------:|-------------------------:|
| `seq.el` | 422 | 422 |
| `cl-lib.el` | 155 | 155 |
| `subr.el` | ~1,466 | 1,466 |
| `simple.el` | ~1,217 | 1,217 |
| `files.el` | ~1,172 | 1,172 |
| `startup.el` | ~606 | 606 |
| `minibuffer.el` | ~614 | 614 |
| `window.el` | ~1,519 | 1,519 |

## Future: Upgrade Path

The curated macro file is the natural place to replace with type-level effect
bodies later. Each macro definition documents "this form has these binding
effects," which is exactly the information a type-level effect system would
formalize. The pre-processing architecture can be swapped for an in-line
expansion step (threading the interpreter through `check_state`) when needed
for user-defined macros.

## Deferred

- **User-defined macros.** Only pre-defined macros from `tart-macros.el` are
  expanded. Macros defined in the file being checked are not expanded.
- **Type specializers.** `cl-defmethod` parameters can include type
  specializers `((arg type) ...)`. Using these for type narrowing in the
  method body is deferred.
- **define-derived-mode.** Mode definition with parent mode inheritance
  has additional complexity and is not included in this spec.
