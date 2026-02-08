# Spec 79 — Coverage Reporting

> Consolidates specs: [28](./.archive/28-coverage-report.md), [29](./.archive/29-emacs-coverage.md), [30](./.archive/30-verbose-coverage.md)

## Overview

Two coverage commands measure type signature coverage: `tart coverage` for user packages and `tart emacs-coverage` for Emacs core. Both compare extracted definitions against `.tart` signature files and report counts, percentages, and uncovered identifiers. A shared `--verbose` flag exposes path resolution, file loading, scanning progress, and match details on stderr for debugging.

## Package Coverage

`tart coverage [--format=human|json] [--verbose] [--fail-under=N] [--exclude=PATTERN] [PATH...]`

Alias: `tart cov`.

### Scanning

`tart coverage` scans `.el` files for definitions. With no arguments it scans the current directory recursively; explicit paths may be files or directories. The `--exclude=PATTERN` flag skips files matching the given glob.

### Extracted forms

The scanner recognises the following defining forms:

| Category | Forms |
|----------|-------|
| Functions | `defun`, `defsubst`, `cl-defun`, `defmacro`, `cl-defmacro`, `defadvice`, `cl-defmethod`, `cl-defgeneric` |
| Variables | `defvar`, `defcustom`, `defconst`, `defvar-local` |
| Structs | `cl-defstruct` (generates constructor `make-X`, predicate `X-p`, copier `copy-X`, and per-slot accessors) |
| Classes | `defclass` (EIEIO; generates predicate `X-p`, constructor, and declared accessors) |
| Faces | `defface` (extracted as a variable-slot identifier) |

### Private identifiers

Identifiers containing `--` are classified as private. They appear in a separate section of the report and are excluded from coverage percentages.

### Signature matching

A definition is covered if a matching signature exists in a sibling `.tart` file (e.g., `foo.tart` for `foo.el`) or in any `.tart` file on the signature search path.

### Report format

The human-readable report contains:

- **Summary**: files scanned, total/public/private definition counts, public coverage percentage.
- **Uncovered list**: each uncovered public identifier with its source file and line number.
- **Private list**: private identifiers listed separately.

### Machine-readable output

`--format=json` produces a JSON object with `files_scanned`, `public` (total, covered, uncovered list), and `private` (total, identifiers list).

### Threshold

`--fail-under=N` (0--100) causes exit code 1 when public coverage falls below the threshold.

### Exit codes

| Code | Meaning |
|------|---------|
| 0 | Success (regardless of coverage percentage, unless `--fail-under` triggers) |
| 1 | Error (parse error, file not found, or coverage below `--fail-under` threshold) |
| 2 | Usage error (bad arguments) |

## Emacs Core Coverage

`tart emacs-coverage [--emacs-source PATH] [--emacs-version VER] [--verbose]`

### Source discovery

The Emacs source directory is auto-detected by querying a running Emacs for `find-function-C-source-directory`. The `--emacs-source PATH` flag overrides auto-detection. If the path does not contain `src/*.c`, an error is reported. When auto-detection fails, the error message suggests `--emacs-source` or installing Emacs source.

### C layer scanning

The scanner parses Emacs C source files (`src/*.c`) for macros that define Lisp-accessible symbols:

| Macro | Defines |
|-------|---------|
| `DEFUN` | Function (subr) |
| `DEFVAR_LISP` | Lisp variable |
| `DEFVAR_INT` | Integer variable |
| `DEFVAR_BOOL` | Boolean variable |
| `DEFSYM` | Interned symbol |

### Signature comparison

Each scanned symbol is checked against signatures in `typings/emacs/{version}/`. The `--emacs-version` flag selects the typings version (auto-detected if omitted).

### Report format

The report has two sections:

- **C Layer Coverage**: source path, files scanned, total public symbols, covered count and percentage, private symbols excluded, uncovered public identifiers (sorted alphabetically).
- **Elisp Layer Coverage**: delegates to the existing package coverage implementation for the Elisp core libraries.

### Exit codes

| Code | Meaning |
|------|---------|
| 0 | Report generated successfully |
| 1 | Emacs source not found or inaccessible |
| 2 | Usage error (bad arguments) |

## Verbose Output

Both commands accept `-v` / `--verbose`. Diagnostic messages go to stderr; the report goes to stdout.

### Path resolution

Shows all typings root candidates with found/not-found status and the final selected path.

### Version detection

Shows the Emacs binary path, detected version, fallback chain (e.g., `31.0.50 -> 31.0 -> 31 -> latest`), and the selected typings version.

### Typings loading

Shows each loaded `.tart` file with its signature count and a total.

### C source scanning (emacs-coverage only)

Shows per-file counts of `DEFUN`, `DEFVAR`, and `DEFSYM` extractions, plus category totals.

### Match summary

Shows a sample of the first 5 covered and first 5 uncovered symbols with source locations.

## Key Files

| File | Role |
|------|------|
| `lib/cli/coverage.ml` | Package coverage command |
| `lib/cli/coverage.mli` | Coverage command interface |
| `lib/coverage/definition_extractor.ml` | Extract definitions from `.el` files |
| `lib/coverage/definition_extractor.mli` | Extractor interface |
| `lib/coverage/coverage_report.ml` | Compare definitions to signatures, generate report |
| `lib/coverage/coverage_report.mli` | Report interface |
| `lib/coverage/c_scanner.ml` | Parse Emacs C source for DEFUNs etc. |
| `lib/coverage/c_scanner.mli` | C scanner interface |
| `lib/coverage/emacs_source.ml` | Discover Emacs source directory |
| `lib/coverage/emacs_source.mli` | Emacs source interface |

## Deferred

No items currently deferred.
