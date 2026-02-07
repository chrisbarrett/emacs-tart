# Spec 64: Self-Typing

Write `.tart` signature files for tart's own Emacs Lisp libraries,
dogfooding the type checker on its own tooling.

- **Dependencies:**
  - [Spec 14](./14-tart-el.md) tart.el runtime macros
  - [Spec 10](./10-emacs-integration.md) tart-mode.el dev tooling
  - [Spec 07](./07-signature-files.md) signature file format

## Goal

Create `.tart` files for `tart.el` and `tart-mode.el` that declare the
public API types.  Running `tart check` against these files exercises the
type checker on real-world Emacs Lisp and surfaces gaps or bugs.

## Constraints

- Only type the **public API** (autoloaded, interactive, and user-facing
  functions/variables); private helpers prefixed `tart--` may be omitted
  unless needed for type-checking accuracy
- Document genuinely untypeable cases in `BUGS.md` rather than forcing
  incorrect types
- If the type checker lacks support for a construct (e.g., macros that
  generate defuns, complex CL patterns), note the gap; do not invent types
  that would be unsound

## Output

```
lisp/
├── tart.el
├── tart.tart           ; Signatures for tart.el
├── tart-mode.el
└── tart-mode.tart      ; Signatures for tart-mode.el
```

## Requirements

### R1: tart.el signatures

**Given** `lisp/tart.tart`
**When** `tart check lisp/tart.el` is run
**Then** the public macros and their types are declared:
- `tart` macro (type assertion and explicit instantiation forms)
- `tart-type` macro
- `tart-declare` macro

**Verify:** `./tart check --emacs-version 31.0 lisp/tart.el` succeeds
(or produces only known-gap warnings)

### R2: tart-mode.el signatures

**Given** `lisp/tart-mode.tart`
**When** `tart check lisp/tart-mode.el` is run
**Then** public functions and variables are declared, including:
- Interactive commands: `run-tart`, `tart-send-region`,
  `tart-send-defun`, `tart-send-last-sexp`, `tart-send-buffer`,
  `tart-switch-to-repl`, `tart-type-at-point`, `tart-expand-at-point`,
  `tart-eglot`, `tart-eglot-ensure`, `tart-install-binary`
- Customization variables: `tart-executable`, `tart-version`,
  `tart-lsp-args`, `tart-repl-args`, etc.

**Verify:** `./tart check --emacs-version 31.0 lisp/tart-mode.el` succeeds
(or produces only known-gap warnings)

### R3: Gap documentation

**Given** constructs in `tart.el` or `tart-mode.el` that the type checker
cannot handle
**When** identified during typing
**Then** each gap is documented in `BUGS.md` with:
- The construct that fails
- The error or unsound behaviour observed
- A minimal reproduction if possible

**Verify:** `BUGS.md` exists and documents any gaps found

## Tasks

- [ ] [R1] Write `lisp/tart.tart`
- [ ] [R2] Write `lisp/tart-mode.tart`
- [ ] [R1–R2] Run `tart check` against both files; iterate on signatures
- [ ] [R3] Document gaps in BUGS.md
