# Spec 61: Signature Imenu

Imenu index support for `.tart` signature files.

- **Dependencies:**
  - [Spec 58](./58-signature-syntax-table.md) syntax table correct

## Goal

Provide `imenu` index definitions for `tart-signature-mode` so that
`M-x imenu` (and packages that use the imenu index, like
`which-function-mode` and consult) can navigate to `defun`, `defvar`, and
`type` declarations.

## Requirements

### R1: Function index

**Given** a `.tart` buffer with `(defun name ...)` declarations
**When** `M-x imenu` is invoked
**Then** function names appear under a "Functions" category

**Verify:** Open `buffer.tart`; imenu lists `buffer-live-p`, `buffer-list`,
`current-buffer`, etc.

### R2: Variable index

**Given** a `.tart` buffer with `(defvar name ...)` declarations
**When** `M-x imenu` is invoked
**Then** variable names appear under a "Variables" category

**Verify:** Open a `.tart` file with `defvar` forms; imenu lists them

### R3: Type index

**Given** a `.tart` buffer with `(type name ...)` declarations
**When** `M-x imenu` is invoked
**Then** type names appear under a "Types" category

**Verify:** Open `tart-prelude.tart`; imenu lists `int`, `float`, `list`,
`option`, `buffer`, etc.

### R4: Flat fallback when only one kind

**Given** a `.tart` buffer with only one kind of declaration (e.g., only
`defun` forms, as in most c-core files)
**When** `M-x imenu` is invoked
**Then** names appear as a flat list (no unnecessary single-category nesting)

**Verify:** Open `buffer.tart` (all defuns); imenu shows flat list of names

## Tasks

- [ ] [R1–R4] Set `imenu-generic-expression` in `tart-signature-mode`
- [ ] [R4] Set `imenu-create-index-function` if flat fallback needs custom logic
- [ ] [R1–R3] Manual test: imenu on prelude (mixed types) and buffer.tart
  (all functions)
