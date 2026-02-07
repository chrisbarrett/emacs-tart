# Spec 59: Signature Font Lock

Keyword highlighting for `.tart` signature files.

- **Dependencies:**
  - [Spec 58](./58-signature-syntax-table.md) syntax table correct

## Goal

Provide `tart-signature-mode` with font-lock rules that highlight the
syntactic structure of `.tart` files, replacing the inherited `lisp-mode`
highlighting with tart-specific rules.

## Requirements

### R1: Declaration keywords

**Given** a `.tart` buffer
**When** font-lock is active
**Then** top-level keywords `defun`, `defvar`, `type`, `open`, `include`
are highlighted with `font-lock-keyword-face`

**Verify:** Open `tart-prelude.tart`; `type`, `defun` are highlighted as
keywords

### R2: Declaration names

**Given** a `(defun foo ...)`, `(defvar bar ...)`, or `(type baz ...)`
form
**When** font-lock is active
**Then** the declared name (`foo`, `bar`, `baz`) is highlighted with
`font-lock-function-name-face` (for `defun`) or
`font-lock-variable-name-face` (for `defvar`) or
`font-lock-type-face` (for `type`)

**Verify:** In `buffer.tart`, function names like `buffer-live-p` are
highlighted as function names; in `tart-prelude.tart`, type names like
`int` and `list` are highlighted as types

### R3: Arrow operator

**Given** a return type arrow `->`
**When** font-lock is active
**Then** `->` is highlighted with `font-lock-keyword-face`

**Verify:** `->` tokens are visually distinct from surrounding type names

### R4: Type variable quantifiers

**Given** a type variable list like `[a b]` or `[(a : truthy)]`
**When** font-lock is active
**Then** the brackets and their contents are highlighted to indicate they
are type-level variables, not value-level code; variables within quantifier
brackets are highlighted with `font-lock-variable-name-face`

**Verify:** `[a b]` in `(type list [a] ...)` is visually distinct from
parameter lists

### R5: Module names

**Given** an `(open foo)` or `(include foo)` form
**When** font-lock is active
**Then** the module name is highlighted with `font-lock-constant-face`

**Verify:** Open a `.tart` file with `(include ...)` directives; module
names are highlighted

## Tasks

- [ ] [R1–R5] Define `tart-signature-mode-font-lock-keywords`
- [ ] [R1–R5] Set `font-lock-defaults` in `tart-signature-mode`
- [ ] [R1–R5] Manual smoke test on prelude and c-core files
