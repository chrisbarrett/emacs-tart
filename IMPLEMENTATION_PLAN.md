# Implementation Plan: Self-Typing ([Spec 64][s64])

Write `.tart` signature files for `tart.el` and `tart-mode.el`,
dogfooding the type checker on its own tooling.

All work targets `lisp/tart.tart`, `lisp/tart-mode.tart`, and
`BUGS.md`.

---

## Dependency Graph

```
Task 1 (tart.tart) ─── Task 3 (tart check + iterate)
Task 2 (tart-mode.tart) ─┘            │
                                       └── Task 4 (BUGS.md)
```

Tasks 1 and 2 are independent. Task 3 depends on both. Task 4
depends on Task 3 (gaps surface during type-checking).

---

## Task 1 — `lisp/tart.tart` ([Spec 64, R1][s64])

**Problem:** `tart.el` has no `.tart` sibling, so running
`tart check lisp/tart.el` cannot type-check the file.

**Approach:** Write `lisp/tart.tart` declaring signatures for
the three public macros. Since the signature parser does not
support `defmacro`, macros are declared as `defun` (the
established convention — see `pcase.tart`, `cl-lib.tart`,
`custom.tart`).

The macros are:
- `tart` — Two forms: type assertion `(tart TYPE FORM)` and
  explicit instantiation `(tart [TYPES...] FN ARGS...)`.
  Both reduce to `any -> any` at the signature level since the
  type checker handles `tart` forms specially (Spec 14).
- `tart-type` — `(tart-type NAME DEF)` → nil. Again handled
  specially by the checker.
- `tart-declare` — `(tart-declare NAME TYPE)` → nil.

Since these are compile-time forms recognised by the type checker
itself, the signatures serve to suppress "unknown function" errors
and document intent. The type checker's own special-form handling
takes precedence over the declared signatures.

**File:** `lisp/tart.tart` — new file, ~20 lines.

---

## Task 2 — `lisp/tart-mode.tart` ([Spec 64, R2][s64])

**Problem:** `tart-mode.el` has no `.tart` sibling and so cannot
be type-checked.

**Approach:** Write `lisp/tart-mode.tart` declaring signatures
for public functions and variables. Per the spec constraints,
only the **public API** is typed — private helpers (`tart--*`)
may be omitted unless needed.

### Public interactive commands

| function              | signature                          |
| --------------------- | ---------------------------------- |
| `run-tart`            | `() -> nil`                        |
| `tart-send-region`    | `(int int) -> nil`                 |
| `tart-send-defun`     | `() -> nil`                        |
| `tart-send-last-sexp` | `() -> nil`                        |
| `tart-send-buffer`    | `() -> nil`                        |
| `tart-switch-to-repl` | `() -> nil`                        |
| `tart-type-at-point`  | `() -> nil`                        |
| `tart-expand-at-point`| `() -> nil`                        |
| `tart-eglot`          | `() -> nil`                        |
| `tart-eglot-ensure`   | `() -> nil`                        |
| `tart-install-binary` | `() -> nil`                        |

### Major/minor mode macros (declared as defun)

| form                        | signature              |
| --------------------------- | ---------------------- |
| `inferior-tart-mode`        | `() -> nil`            |
| `tart-signature-mode`       | `() -> nil`            |
| `tart-mode`                 | `(&optional any) -> nil` |

### Customization variables (defvar)

| variable                      | type                         |
| ----------------------------- | ---------------------------- |
| `tart-executable`             | `(symbol \| string)`         |
| `tart-version`                | `(symbol \| string)`         |
| `tart-lsp-args`               | `(list string)`              |
| `tart-repl-args`              | `(list string)`              |
| `tart-directory-style`        | `symbol`                     |
| `tart-repl-history-file`      | `(symbol \| string \| nil)`  |
| `tart-install-directory`      | `(symbol \| string)`         |
| `tart-setup-find-sibling-rules` | `bool`                    |
| `tart-setup-eglot`            | `bool`                       |
| `tart-error-regexp`           | `(list any)`                 |

### Other public variables/constants

| variable                       | type           |
| ------------------------------ | -------------- |
| `inferior-tart-mode-map`       | `any`          |
| `tart-mode-map`                | `any`          |

Keymaps and syntax tables have no precise type in tart's type
system, so they use `any`.

**File:** `lisp/tart-mode.tart` — new file, ~80 lines.

---

## Task 3 — Type-check and iterate ([Spec 64, R1–R2][s64])

**Problem:** The signatures from Tasks 1–2 may contain errors
or reveal type-checker gaps.

**Approach:** Run `./tart check --emacs-version 31.0 lisp/tart.el`
and `./tart check --emacs-version 31.0 lisp/tart-mode.el`. Fix
signature errors iteratively. Classify remaining diagnostics as
either:
- **Fixable:** adjust the `.tart` signature
- **Gap:** a genuine type-checker limitation (→ Task 4)

Expect common issues:
- `defmacro` not directly supported (use `defun`)
- `defcustom` forms with `:type` plist may confuse the checker
- `define-minor-mode` / `define-derived-mode` are macros that
  expand into multiple definitions
- `defalias` may not be recognized
- Backquoted forms / `rx` macros may produce unknown types
- `url-retrieve` callbacks with closures

Goal: reduce to zero errors or document each remaining error in
BUGS.md.

**Files:** `lisp/tart.tart`, `lisp/tart-mode.tart` (iterations).

---

## Task 4 — Gap documentation ([Spec 64, R3][s64])

**Problem:** Some constructs in `tart.el` / `tart-mode.el` may
be untypeable by the current type checker.

**Approach:** For each remaining diagnostic after Task 3 that
cannot be resolved by fixing signatures, add an entry to
`BUGS.md` under a new "## Self-Typing Gaps" section with:
- The construct that fails
- The error or unsound behaviour
- A minimal reproduction

**File:** `BUGS.md` — append section.

---

## Risks

- **Macro expansion opacity:** `define-minor-mode`,
  `define-derived-mode`, `defcustom`, `defalias` expand into
  code that may not match simple `defun`/`defvar` patterns the
  type checker expects. The checker may not see the variables
  and functions these macros define.
- **Backquote/splicing:** `tart-mode.el` uses backquote in
  several places (eglot registration, auto-mode-alist). The
  checker may not handle these correctly.
- **Callback-heavy code:** `tart--github-request`,
  `tart--download-binary`, `url-retrieve` all use closures
  as callbacks. These are private helpers and not in the public
  API, so they can be omitted.
- **`require` dependencies:** `tart-mode.el` requires `eglot`,
  `comint`, `compile`, `url`, `xdg`, `cl-lib`. If any lack
  `.tart` signatures, the checker will report unknown functions.

[s64]: ./specs/64-self-typing.md
