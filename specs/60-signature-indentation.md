# Spec 60: Signature Indentation

Correct automatic indentation for `.tart` signature files.

- **Dependencies:**
  - [Spec 58](./58-signature-syntax-table.md) syntax table correct

## Goal

Configure `tart-signature-mode` indentation so that tart forms indent
correctly with TAB / `indent-region`, following Lisp conventions with
tart-specific form rules.

## Context

Tart signature files use a small set of top-level forms.  Most are
single-line, but `defun` with multi-clause signatures spans multiple lines:

```lisp
;; Single-line (most common)
(defun buffer-live-p (any) -> bool)
(defvar buffer-alist (list any))
(type option [(a : truthy)] (a | nil))
(open some-module)
(include other-module)

;; Multi-clause defun
(defun overlayp
  ((overlay) -> t)
  ((_) -> nil))

;; Multi-line type alias
(type eq-safe
  (symbol | keyword | int | t | nil))
```

## Requirements

### R1: Lisp-style form indentation

**Given** standard tart forms
**When** TAB or `indent-region` is invoked
**Then** `defun`, `defvar`, `type` indent their body with a 2-space offset
from the opening paren (Lisp `defun`-style indentation, not function-call
alignment)

**Verify:** In a multi-clause `defun`, each clause is indented 2 spaces
from `(defun`:

```lisp
(defun overlayp
  ((overlay) -> t)
  ((_) -> nil))
```

### R2: open and include indentation

**Given** `open` and `include` forms
**When** TAB is invoked
**Then** they indent with standard 1-body-form offset (contents align
after the keyword, as with Lisp special forms)

**Verify:** `(open foo)` and `(include bar)` indent as normal Lisp forms

### R3: Nested type expressions

**Given** a multi-line type expression within parentheses
**When** TAB is invoked
**Then** nested lines align with standard Lisp indentation (first
argument alignment or 1/2-space body offset as appropriate)

**Verify:**

```lisp
(type eq-safe
  (symbol | keyword | int | t | nil))
```

## Tasks

- [x] [R1–R3] Register Lisp indent properties for tart forms (`defun`,
  `defvar`, `type`, `open`, `include`)
- [x] [R1–R3] Verify with `indent-region` on prelude and c-core files
