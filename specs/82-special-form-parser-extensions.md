# Spec 82 — Special Form Parser Extensions

## Overview

Tart's parser does not recognise many common Emacs Lisp defining forms, causing
all bindings inside their bodies to appear as undefined variables. These
unsupported forms account for approximately 49% of all validation errors
(7,171/14,508 UNDEFINED VARIABLE errors) across the Emacs core files tested.
This spec classifies the unsupported forms, defines the parsing behaviour for
each category, fixes `defcustom`/`defgroup`/`defface` first-argument
interpretation, and organises the work into five phases.

This spec operates at the type-checker's form-recognition level — it
determines which Elisp forms the checker understands and how it binds variables
within them. The S-expression parser itself is covered by
[Spec 78](./78-parser-fidelity.md).

## Form Classification

### Defun-like forms

Forms that define a named function with a parameter list and body.
The checker should treat these identically to `defun`: bind the function name,
bind parameters in the body scope, and infer the body type.

| Form | Notes |
|:-----|:------|
| `defsubst` | Inline function; semantically identical to `defun` |
| `cl-defgeneric` | CLOS-style generic; treat name + arglist as function binding |
| `cl-defmethod` | CLOS-style method; bind name + arglist, ignore qualifiers/specializers |

### Defmacro-like forms

Forms that define a named macro with a parameter list and body.
The checker should bind the macro name and parameters in the body scope.
Macro parameters use destructuring `&rest`/`&body` and may include
`&environment` — these are bound as variables but not typed.

| Form | Notes |
|:-----|:------|
| `defmacro` | Currently parsed but body bindings are not fully scoped |
| `pcase-defmacro` | Defines a pcase pattern macro; same structure as `defmacro` |

### Define-mode-like forms

Forms that define a minor or major mode. These generate a function (the mode
toggle) and a variable (the mode state). The checker should bind both.

| Form | Notes |
|:-----|:------|
| `define-minor-mode` | Generates `MODE` function and `MODE` variable (bool) |

### Binding forms

Forms that introduce local variable bindings in a body, similar to `let`.
The checker should scope the bound variables to the body expressions.

| Form | Notes |
|:-----|:------|
| `gv-letplace` | Binds a getter/setter pair in body |
| `macroexp-let2` | Binds a variable to an expression in body |

### Declaration-only forms

Forms that declare a name without defining a body. The checker should record
the binding but has no body to type-check.

| Form | Notes |
|:-----|:------|
| `declare-function` | Declares external function exists; bind name |
| `set-advertised-calling-convention` | Metadata; no bindings needed |
| `gv-define-setter` | Declares a setter; bind name |
| `gv-define-expander` | Declares a gv expander; bind name |
| `cl--defalias` | Internal alias; bind target name |

## Defcustom First-Argument Fix

### Problem

`defcustom` is typed as `(symbol any string &rest any) -> symbol`, but the
checker currently interprets `(defcustom foo 42 "doc" ...)` with `foo` as a
variable reference rather than a symbol literal. This causes 12 spurious
"expects argument 1 to be symbol" errors in `simple.el` alone.

The same issue affects `defgroup` and `defface`, which also take a symbol
name as their first argument.

### Solution

In `check.ml`, add special-case handling for `defcustom`, `defgroup`, and
`defface` to treat the first argument as a quoted symbol literal rather than
a variable reference. This matches the existing handling of `defvar` and
`defconst`.

## Phased Implementation

### Phase 1 — Defmacro/Defsubst

Extend the form recogniser for `defmacro` (full body scoping) and `defsubst`.
These are the most common forms and have straightforward defun-like structure.

### Phase 2 — CL Forms

Add support for `cl-defgeneric`, `cl-defmethod`, and `cl--defalias`.
`cl-defmethod` requires skipping optional qualifiers (`:before`, `:after`,
`:around`) and type specializers in the parameter list.

### Phase 3 — Mode/GV/Pcase

Add `define-minor-mode`, `gv-letplace`, `gv-define-setter`,
`gv-define-expander`, and `pcase-defmacro`. These are less frequent but still
contribute to UNDEFINED VARIABLE counts.

### Phase 4 — Declaration-Only

Add `declare-function`, `set-advertised-calling-convention`, and
`macroexp-let2`. These are low-frequency but easy to implement since they
mostly just record a binding.

### Phase 5 — Defcustom/Defgroup/Defface

Fix the first-argument interpretation for `defcustom`, `defgroup`, and
`defface`. This is separated because it changes argument evaluation semantics
rather than adding new form recognition.

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

## Key Files

| File | Role |
|:-----|:-----|
| `lib/typing/check.ml` | Form recognition, defcustom first-arg fix |
| `lib/typing/infer.ml` | Body inference for new form types |
| `lib/typing/defun_infer.ml` | Defun-like inference (template for new forms) |
| `lib/syntax/sexp.ml` | S-expression parser (unchanged; see [Spec 78](./78-parser-fidelity.md)) |

## Deferred

- **Macro expansion.** Tart does not expand macros before type checking.
  Forms like `cl-defmethod` may expand into more complex code that the
  checker cannot follow. Full macro expansion is out of scope.
- **Type specializers.** `cl-defmethod` parameters can include type
  specializers `((arg type) ...)`. Using these for type narrowing in the
  method body is deferred.
- **define-derived-mode.** Mode definition with parent mode inheritance
  has additional complexity and is not included in this spec.
