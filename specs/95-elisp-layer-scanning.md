# Spec 95 — Elisp Layer Scanning for Emacs Coverage

> Extends: [Spec 79 — Coverage Reporting](./79-coverage-reporting.md)

## Overview

`tart emacs-coverage` only scans C source (`src/*.c`) against `c-core/*.tart`. The typings directory also contains `lisp-core/*.tart` for Emacs Lisp core libraries. Scanning the Elisp layer completes the coverage picture.

## Requirements

### R1: Scan Emacs Lisp core files

Scan `lisp/*.el` using the existing definition extractor (same forms as `tart coverage`). Output includes rows for `.el` files alongside `.c` files.

### R2: Match against lisp-core typings

A definition `foo` from `lisp/bar.el` is checked against signatures in `typings/emacs/{version}/lisp-core/bar.tart`.

### R3: Private identifier handling

Identifiers containing `--` are classified as private and excluded from coverage percentages, consistent with the C layer behaviour.

### R4: Files without typings show zero coverage

If `lisp/baz.el` exists but `lisp-core/baz.tart` does not, `baz.el` appears with `0/N` public coverage.

## Key Files

| File | Role |
|:-----|:-----|
| `lib/coverage/definition_extractor.ml` | Extracts definitions from `.el` files |
| `lib/coverage/emacs_coverage.ml` | Coverage calculation (needs Elisp layer) |
| `typings/emacs/31.0/lisp-core/` | Elisp core typings |
