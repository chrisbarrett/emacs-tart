# Spec 75 — Signature Major Mode

> Consolidates specs: [58](./.archive/58-signature-syntax-table.md), [59](./.archive/59-signature-font-lock.md), [60](./.archive/60-signature-indentation.md), [61](./.archive/61-signature-imenu.md), [62](./.archive/62-signature-eglot.md)

## Overview

`tart-signature-mode` is a major mode for editing `.tart` signature files,
derived from `lisp-mode`. It provides a tart-specific syntax table for correct
S-expression navigation, font-lock rules for declaration keywords and type
syntax, Lisp-style indentation with form-aware overrides, imenu navigation for
`defun`/`defvar`/`type` declarations, and eglot integration that connects
`.tart` buffers to the tart LSP server for diagnostics and hover.

## Syntax Table

`tart-signature-mode-syntax-table` overrides `lisp-mode`'s defaults to match
tart's type syntax. The table is modelled on `inferior-tart-mode-syntax-table`
with additional symbol constituents.

### Character Classes

| Character(s)                             | Syntax class      | Rationale                                                             |
|:-----------------------------------------|:------------------|:----------------------------------------------------------------------|
| `(` `)`                                  | Paired delimiters | S-expression grouping                                                 |
| `[` `]`                                  | Paired delimiters | Type variable quantifier brackets (`[a b]`)                          |
| `{` `}`                                  | Paired delimiters | Row type literals (`{name string age int}`)                          |
| `;`                                      | Comment start     | Line comments                                                         |
| `\n`                                     | Comment end       | Terminates `;` comments                                               |
| `"`                                      | String delimiter  | String literals                                                       |
| `\`                                      | Escape            | Backslash escapes in strings                                          |
| `\|`                                     | Symbol constituent| Union operator---`lisp-mode` treats `\|` as a string-fence character, causing false `check-parens` errors in files with odd `\|` counts |
| `&`                                      | Symbol constituent| `&optional`, `&rest`, `&key` parse as single symbols                 |
| `:`                                      | Symbol constituent| Bounded quantifiers `(a : truthy)` and keyword arguments             |
| `-` `_` `*` `+` `/` `<` `>` `=` `?` `!` | Symbol constituent| Standard Lisp symbol characters                                       |

With this table, `forward-sexp`, `show-paren-mode`, `electric-pair-mode`, and
`check-parens` all operate correctly on tart syntax.

## Font-Lock

Font-lock rules replace the inherited `lisp-mode` highlighting with
tart-specific keywords. The variable `tart-signature-mode-font-lock-keywords`
defines all rules.

### Highlighted Elements

| Element                     | Face                            | Examples                                    |
|:----------------------------|:--------------------------------|:--------------------------------------------|
| Declaration keywords        | `font-lock-keyword-face`        | `defun`, `defvar`, `type`, `open`, `include`|
| Function names after `defun`| `font-lock-function-name-face`  | `buffer-live-p`, `car`                      |
| Variable names after `defvar`| `font-lock-variable-name-face` | `load-path`, `buffer-alist`                 |
| Type names after `type`     | `font-lock-type-face`           | `int`, `list`, `option`                     |
| Module names after `open`/`include` | `font-lock-constant-face` | `seq`, `dash`                             |
| Arrow operator `->` | `font-lock-keyword-face` | `(int int) -> int` |
| Type variables in quantifier brackets | `font-lock-variable-name-face` | `[a b]`, `[(a : truthy)]` |

Comments and string literals are handled by the syntax table and do not require
explicit font-lock rules.

## Indentation

Indentation follows Emacs Lisp conventions via a custom indent function
(`tart-signature-indent-function`) that consults
`tart-signature-mode--indent-overrides` before falling back to standard
`lisp-indent-function` symbol properties.

### Form Indentation

All tart top-level forms use `defun`-style indentation (2-space body offset
from the opening parenthesis):

| Form      | Indent method | Example                                       |
|:----------|:--------------|:----------------------------------------------|
| `defun`   | `defun`       | Multi-clause bodies indent 2 spaces            |
| `defvar`  | `defun`       | Body aligns under keyword                      |
| `type`    | `defun`       | Multi-line type bodies indent 2 spaces         |
| `open`    | `defun`       | Contents align after keyword                   |
| `include` | `defun`       | Contents align after keyword                   |

Multi-clause `defun` signatures indent each clause consistently:

```lisp
(defun overlayp
  ((overlay) -> t)
  ((_) -> nil))
```

Multi-line type aliases indent their bodies:

```lisp
(type eq-safe
  (symbol | keyword | int | t | nil))
```

Nested type expressions within parentheses follow standard Lisp indentation
(first-argument alignment or body offset as appropriate).

## Imenu

`tart-signature-mode` sets `imenu-generic-expression` to index three
declaration kinds:

| Category    | Pattern              | Example matches                          |
|:------------|:---------------------|:-----------------------------------------|
| Functions   | `(defun <name> ...)`  | `buffer-live-p`, `car`, `stringp`       |
| Variables   | `(defvar <name> ...)` | `load-path`, `buffer-alist`             |
| Types       | `(type <name> ...)`   | `int`, `list`, `option`, `buffer`       |

A custom `imenu-create-index-function`
(`tart-signature-mode--imenu-create-index`) flattens the index when all entries
fall under a single category. Most c-core files contain only `defun`
declarations, so their imenu lists appear flat rather than nested under a
redundant "Functions" heading.

Packages that consume the imenu index (`which-function-mode`, consult,
`helm-imenu`) work without additional configuration.

## Eglot Integration

When `tart-setup-eglot` is non-nil (the default), `tart-mode.el` registers
`tart-signature-mode` alongside `emacs-lisp-mode` in `eglot-server-programs`.
Both modes share a single multi-mode entry pointing to
`tart--eglot-server-program` (which resolves to `tart lsp`).

### Capabilities in `.tart` Buffers

- **Diagnostics**: Parse errors and unbound type references appear as
  underlined errors/warnings in the buffer.
- **Hover**: `textDocument/hover` returns type information for expressions
  under the cursor.
- **Real-time feedback**: Edits to `.tart` files trigger re-checking of
  dependent `.el` buffers (see [Spec 71](./71-lsp-server.md) for the
  invalidation mechanism).

### Shared Workspace

The multi-mode `eglot-server-programs` entry ensures that `.el` and `.tart`
buffers in the same project share a single LSP server connection. Opening a
`.tart` file when eglot is already running for a sibling `.el` file reuses the
existing server rather than spawning a duplicate.

## Key Files

| File | Role |
|:-----|:-----|
| `lisp/tart-mode.el` | `tart-signature-mode` definition, syntax table, font-lock, indentation, imenu, eglot registration |

## Deferred

No items currently deferred.
