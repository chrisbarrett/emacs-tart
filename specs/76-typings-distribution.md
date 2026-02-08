# Spec 76 — Typings Distribution

> Consolidates specs: [24](./.archive/24-versioned-typings.md), [32](./.archive/32-emacs-core-typings.md)

## Overview

Tart ships versioned type signatures for Emacs C primitives and core Lisp
libraries, organized under `typings/emacs/{version}/`. The type checker
auto-detects the installed Emacs version, resolves the best-matching typings
directory through a fallback chain, and loads all `.tart` files from `c-core/`
and `lisp-core/` subdirectories. An implicit prelude provides foundational type
aliases shared across all versions. Authors create and maintain typings by
running `./tart check` against real Elisp, fixing signatures from errors, and
documenting untypeable cases in `BUGS.md`.

## Directory Structure

```
typings/
├── tart-prelude.tart              ; implicit prelude (utility types)
└── emacs/
    ├── BUGS.md                    ; cross-version issues
    ├── 30/
    │   ├── c-core/*.tart
    │   └── lisp-core/*.tart
    └── 31.0/
        ├── BUGS.md                ; version-specific issues
        ├── c-core/*.tart
        └── lisp-core/*.tart
```

C-core files map 1:1 to Emacs C source files (e.g., `data.tart` for `data.c`).
Lisp-core files map 1:1 to Emacs Lisp core libraries (e.g., `subr.tart` for
`subr.el`). Each file contains `defun`, `defvar`, `type`, and directive
declarations as described in [Spec 69](./69-signature-files-modules.md).

Typings are bundled with the tart binary.

## Version Resolution

The `--emacs-version` CLI flag explicitly selects a version. Without it, tart
runs `emacs --version` and parses the output. If Emacs is not on `PATH` and no
flag is given, tart falls back to the `latest` version (currently 31.0) with a
warning.

The resolved version feeds a fallback chain that searches for an existing
typings directory:

```
31.0.50 -> 31.0 -> 31 -> latest
```

The first directory that exists wins. Within that directory, all `c-core/*.tart`
and `lisp-core/*.tart` files are loaded into the type environment.

Implementation files:

| file                        | role                                     |
| --------------------------- | ---------------------------------------- |
| `lib/sig/emacs_version.ml`  | Version detection and parsing            |
| `lib/sig/emacs_version.mli` | Public interface for version types       |
| `lib/sig/search_path.ml`    | Fallback chain and typings directory load |

## Prelude

The prelude (`typings/tart-prelude.tart`) loads before any versioned typings. It
defines userspace names for compiler intrinsics (`list`, `option`, `is`,
`nonempty`, etc.) that all `.tart` files can reference without an explicit
import. See [Spec 69](./69-signature-files-modules.md) for the full loading
sequence.

## Authoring Workflow

1. **Select target** -- Pick a C source file or Lisp library to cover.
2. **Extract symbols** -- Run `tart emacs-coverage -v` to list uncovered
   DEFUNs/DEFVARs.
3. **Write signatures** -- Create the corresponding `.tart` file under
   `c-core/` or `lisp-core/`.
4. **Validate** -- Run `./tart check` against `.el` files from Emacs's `lisp/`
   directory.
5. **Iterate** -- Fix type errors until the file type-checks cleanly.
6. **Document gaps** -- Log untypeable items to the appropriate `BUGS.md`.

### Signature guidelines

- Avoid `any` in output position. Prefer unions of specific types. If `any` is
  unavoidable, document the reason in `BUGS.md`.
- Primitives without special type-checker support are represented as opaque
  types.
- The `(include)` directive de-duplicates identical definitions across Emacs
  versions, so shared signatures need only be written once.

### BUGS.md categories

Each entry records the symbol name, source location, category, description, and
(where applicable) a suggested type system feature.

| category           | meaning                                                   |
| ------------------ | --------------------------------------------------------- |
| `type-system-gap`  | Needs features tart lacks (dependent types, row poly)     |
| `untypeable`       | Behavior cannot be captured soundly (dynamic dispatch)    |
| `ergonomic`        | Typeable but awkward (excessive annotations at call sites) |
| `version-specific` | Signature changed between Emacs versions                  |

### Coverage tracking

`tart emacs-coverage` measures C-layer type coverage for a given Emacs version.
The `--verbose` flag shows per-file signature counts and sample matches for
debugging.

## Deferred

- Backfill of 29.x typings.
- Remaining C source files beyond the initial core set are ongoing.
- GUI backend typings (ns/w32/pgtk/x11).
- Third-party package typings (dash, s, etc.) in a separate repository.
