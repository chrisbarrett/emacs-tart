# Implementation Plan: Emacs Integration — Signature Mode & Minor Mode

Six specs enhance the Emacs tooling: a proper major mode for `.tart`
signature files (syntax table, font-lock, indentation, imenu), eglot
registration for `.tart` buffers, and clean minor-mode lifecycle for
`tart-mode`.

All work targets `lisp/tart-mode.el`.

---

## Dependency Graph

```
Task 1 (syntax table) ── Task 2 (font-lock)
                      ├── Task 3 (indentation)
                      ├── Task 4 (imenu)
                      └── Task 5 (eglot registration)

Task 6 (minor-mode lifecycle) — independent
```

Spec 58 (syntax table) is the foundation — specs 59–62 all depend on it.
Spec 63 (lifecycle) is independent and can be done in any order.

---

## Task 1 — Signature syntax table ([Spec 58][s58])

**Problem:** `tart-signature-mode` derives from `lisp-mode`, which treats
`|` as a string-fence character (Common Lisp `|symbol|` escapes). This
breaks `check-parens` on 60+ `.tart` files. Square brackets `[]` and curly
braces `{}` are not paired delimiters.

**Approach:** Define `tart-signature-mode-syntax-table` modelled on the
existing `inferior-tart-mode-syntax-table` (which already solves these
problems). Set it via `:syntax-table` in the mode definition.

The existing `inferior-tart-mode-syntax-table` defines:
- `()` as matched parens ✓
- `[]` as matched brackets ✓
- `{}` as matched braces ✓
- `;` as comment-start, `\n` as comment-end ✓
- `"` as string delimiter, `\` as escape ✓
- `-_*+/<>=?!` as symbol constituents ✓

Additionally needed for `.tart` (per R5):
- `|` as symbol constituent (not string-fence)
- `&` as symbol constituent (for `&optional`, `&rest`)
- `:` as symbol constituent (for `(a : truthy)` bounds)

Files:
- `lisp/tart-mode.el` — add `tart-signature-mode-syntax-table`; update
  `tart-signature-mode` definition to use `:syntax-table`

Scope: ~25 lines added.

---

## Task 2 — Font-lock keywords ([Spec 59][s59])

**Problem:** `tart-signature-mode` inherits `lisp-mode` font-lock, which
highlights Lisp keywords (`defun`, `defvar`, etc.) but not tart-specific
constructs (`type`, `open`, `include`, `->`, `warn`).

**Approach:** Define `tart-signature-mode-font-lock-keywords` with matchers
for:
- **R1:** Declaration keywords: `defun`, `defvar`, `type`, `open`,
  `include` → `font-lock-keyword-face`
- **R2:** Declaration names: the symbol after `defun`/`defvar`/`type` →
  `font-lock-function-name-face` / `font-lock-variable-name-face` /
  `font-lock-type-face`
- **R3:** Arrow operator `->` → `font-lock-keyword-face`
- **R4:** Type variable quantifier brackets `[a b]` — variables inside →
  `font-lock-variable-name-face`
- **R5:** Module names after `open`/`include` →
  `font-lock-constant-face`

Set `font-lock-defaults` in the mode definition to use these keywords
instead of `lisp-mode`'s defaults.

Files:
- `lisp/tart-mode.el` — add font-lock keywords variable; update mode
  definition

Scope: ~40 lines added.

---

## Task 3 — Indentation ([Spec 60][s60])

**Problem:** `lisp-mode` indentation works for most forms but needs
tart-specific `lisp-indent-function` properties for `defun`, `defvar`,
`type`, `open`, and `include`.

**Approach:** Register indent properties in the mode body:
- `defun` → `defun` (2-space body indent, Lisp special-form style)
- `defvar` → `defun`
- `type` → `defun`
- `open` → 1
- `include` → 1

These use `put ... 'lisp-indent-function ...` which is the standard
mechanism for `lisp-mode`-derived modes. Since `tart-signature-mode`
inherits `lisp-mode`'s indentation engine, this is sufficient.

Files:
- `lisp/tart-mode.el` — add `put` forms in mode body

Scope: ~10 lines added.

---

## Task 4 — Imenu integration ([Spec 61][s61])

**Problem:** `tart-signature-mode` has no imenu support, so `M-x imenu`
doesn't list declarations.

**Approach:** Set `imenu-generic-expression` with three categories:
- `"Functions"` matching `(defun NAME`
- `"Variables"` matching `(defvar NAME`
- `"Types"` matching `(type NAME`

For R4 (flat fallback when only one kind), use a custom
`imenu-create-index-function` that flattens when all entries fall under a
single category (most c-core files are all `defun`).

Files:
- `lisp/tart-mode.el` — add imenu configuration in mode body

Scope: ~25 lines added.

---

## Task 5 — Eglot registration for `.tart` buffers ([Spec 62][s62])

**Problem:** Eglot is registered only for `emacs-lisp-mode`. Opening a
`.tart` file and running `M-x eglot` doesn't know which server to use.

**Approach:** Add `tart-signature-mode` to `eglot-server-programs`
alongside the existing `emacs-lisp-mode` entry. Both modes share the same
server command (`tart lsp`).

For R3 (shared workspace), eglot groups buffers by project root — if
both `.el` and `.tart` files are in the same project, they'll share one
server connection automatically (no code needed beyond registration).

Files:
- `lisp/tart-mode.el` — update the `when tart-setup-eglot` block to
  register both modes

Scope: ~5 lines changed.

---

## Task 6 — Minor-mode lifecycle ([Spec 63][s63])

**Problem:** `tart-mode`'s `define-minor-mode` body is empty — it only
sets up a keymap and lighter. As features are added (custom font-lock,
syntax modifications), the enable/disable lifecycle must properly
install and remove buffer-local modifications.

**Approach:** Currently `tart-mode` makes no buffer-local modifications
beyond its keymap (which `define-minor-mode` handles automatically). The
lifecycle concern is about *future* modifications. Audit the current
body and add a conditional branch:

```elisp
(if tart-mode
    ;; Enable: install buffer-local modifications
    (progn ...)
  ;; Disable: remove buffer-local modifications
  (progn ...))
```

For now this is a no-op scaffold, but it establishes the pattern for
future additions (e.g., custom `check-parens` advice, additional
font-lock rules for `.el` buffers).

Files:
- `lisp/tart-mode.el` — update `tart-mode` body with lifecycle
  branching

Scope: ~10 lines added.

---

## Task 7 — ERT tests for new features

Add tests for the new functionality to `lisp/tart-mode-tests.el`:

- **Syntax table:** `|` is symbol constituent (not string-fence);
  `[]` are matched brackets; `{}` are matched braces
- **Font-lock:** keyword faces applied to `defun`, `type`, `->`;
  function names get `font-lock-function-name-face`
- **Imenu:** imenu index contains expected entries from sample content
- **Indentation:** multi-clause `defun` indents body at 2-space offset
- **Minor-mode lifecycle:** enable/disable is idempotent

Files:
- `lisp/tart-mode-tests.el` — add test cases

Scope: ~60 lines added.

---

## Risks

- **`lisp-mode` indentation internals:** The `lisp-indent-function`
  property may not cover all multi-clause `defun` forms if `lisp-mode`'s
  indentation engine treats the first argument specially. Test with real
  `.tart` files.
- **Font-lock anchored matchers:** Highlighting type variables inside
  `[...]` brackets requires anchored font-lock matchers, which can be
  tricky. Simpler approaches (e.g., highlighting the whole bracket form)
  may be more robust.
- **Imenu flat fallback:** Custom `imenu-create-index-function` must
  handle edge cases (empty files, mixed categories) correctly.

[s58]: ./specs/58-signature-syntax-table.md
[s59]: ./specs/59-signature-font-lock.md
[s60]: ./specs/60-signature-indentation.md
[s61]: ./specs/61-signature-imenu.md
[s62]: ./specs/62-signature-eglot.md
[s63]: ./specs/63-tart-minor-mode-lifecycle.md
