# Spec 29: Emacs Coverage Report

Development tool for tracking Tart's type coverage of Emacs itself.

**Deps:** Spec 24 (versioned typings).

## Goal

Know how complete type coverage is for Emacs core (C layer + elisp layer), to track progress and inform Tart development priorities.

## Constraints

| Constraint | Detail |
|------------|--------|
| C layer first | Parse Emacs C source files for Lisp-accessible definitions |
| Auto-discovery | Detect Emacs source directory from running Emacs |
| Override | Accept explicit path for custom builds |
| Private exclusion | Identifiers containing `--` excluded from percentages |

## Emacs Source Discovery

Query running Emacs for source location:

```
emacs --batch --eval '(message "%S" (list :source-directory find-function-C-source-directory :version emacs-version))'
```

Fallback to explicit path via `--emacs-source`.

## C Layer Scanning

Parse Emacs C source files for macros defining Lisp-accessible symbols:

| Macro | Defines |
|-------|---------|
| DEFUN | Function (subr) |
| DEFVAR_LISP | Lisp variable |
| DEFVAR_INT | Integer variable |
| DEFVAR_BOOL | Boolean variable |
| DEFSYM | Interned symbol |

## Output Format

Human-readable report:

```
=== C Layer Coverage ===
Emacs source: /path/to/emacs/src
Files scanned: 142
Total public symbols: 1847
Covered: 234 (12.7%)
Uncovered: 1613

Private symbols excluded: 42
  internal--foo, internal--bar, ...

Uncovered public:
  abort-recursive-edit
  accept-process-output
  ...

=== Elisp Layer Coverage ===
[delegates to existing coverage-report implementation]
```

## Directory Structure

```
lib/coverage/
├── c_scanner.ml        ; Parse Emacs C source for DEFUNs etc.
├── c_scanner.mli
├── emacs_source.ml     ; Discover Emacs source directory
├── emacs_source.mli
├── coverage_report.ml  ; Generate combined report
└── coverage_report.mli
bin/main.ml             ; (modify) Add emacs-coverage subcommand
```

## Requirements

### R1: Auto-detect Emacs source directory

**Given** Emacs is installed and `find-function-C-source-directory` points to source
**When** `tart emacs-coverage` is invoked with no path argument
**Then** the Emacs source directory is detected via batch eval
**And** the detected path is printed in the report header

**Verify:** `tart emacs-coverage | head -3` shows detected source path

### R2: Explicit source path override

**Given** `--emacs-source /custom/path` is provided
**When** `tart emacs-coverage --emacs-source /custom/path` is invoked
**Then** the specified path is used instead of auto-detection
**And** an error is reported if the path does not contain `src/*.c`

**Verify:** `tart emacs-coverage --emacs-source /tmp/empty` exits 1 with error

### R3: Missing source gracefully handled

**Given** auto-detection fails (no Emacs, or source not installed)
**When** `tart emacs-coverage` is invoked
**Then** a clear error message explains the issue
**And** suggests `--emacs-source` or installing Emacs source

**Verify:** `PATH= tart emacs-coverage 2>&1` shows helpful error

### R4: Scan C files for DEFUNs

**Given** a valid Emacs source directory
**When** `tart emacs-coverage` runs
**Then** all `.c` files in `src/` are scanned
**And** `DEFUN` macro invocations are parsed to extract function names

**Verify:** Output includes known C primitives like `car`, `cdr`, `cons`

### R5: Scan C files for DEFVARs

**Given** a valid Emacs source directory
**When** `tart emacs-coverage` runs
**Then** `DEFVAR_LISP`, `DEFVAR_INT`, `DEFVAR_BOOL` are parsed
**And** variable names are extracted (with leading `V` stripped)

**Verify:** Output includes known variables like `load-path`, `buffer-file-name`

### R6: Scan C files for DEFSYMs

**Given** a valid Emacs source directory
**When** `tart emacs-coverage` runs
**Then** `DEFSYM` macro invocations are parsed
**And** symbol names are extracted

**Verify:** Output includes known symbols like `nil`, `t`, `lambda`

### R7: Private identifiers excluded from percentages

**Given** identifiers containing `--` exist in the scan results
**When** coverage percentages are calculated
**Then** private identifiers are excluded from the denominator
**And** private identifiers are listed separately in the report

**Verify:** Report shows "Private symbols excluded:" section

### R8: Compare against typings

**Given** C layer symbols are scanned
**When** coverage is calculated
**Then** each symbol is checked against loaded typings
**And** a symbol is "covered" if it has a type signature in `typings/emacs/{version}/`

**Verify:** Symbols in typings show as covered

### R9: Elisp layer delegation

**Given** `tart emacs-coverage` is invoked
**When** the report is generated
**Then** elisp layer coverage is included via the existing coverage-report mechanism
**And** elisp section follows C layer section

**Verify:** Report contains "Elisp Layer Coverage" section

### R10: Summary statistics

**Given** `tart emacs-coverage` completes
**When** the report is printed
**Then** summary shows: total symbols, covered count, percentage, uncovered count
**And** separate summaries for C layer and elisp layer

**Verify:** Report contains percentage like "Covered: 234 (12.7%)"

### R11: List uncovered public identifiers

**Given** `tart emacs-coverage` completes
**When** the report is printed
**Then** all uncovered public identifiers are listed alphabetically

**Verify:** Report contains "Uncovered public:" section with sorted list

### R12: Emacs version alignment

**Given** `--emacs-version` is provided (or auto-detected)
**When** coverage is calculated
**Then** the typings for that version are used for comparison
**And** the report header shows the Emacs version

**Verify:** `tart emacs-coverage --emacs-version 31.0` uses 31.0 typings

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Report generated successfully |
| 1 | Emacs source not found or inaccessible |
| 2 | Usage error (bad arguments) |

## Tasks

- [ ] Emacs source discovery module
- [ ] C scanner for DEFUN extraction
- [ ] C scanner for DEFVAR extraction
- [ ] C scanner for DEFSYM extraction
- [ ] Private identifier filtering
- [ ] Typings comparison logic
- [ ] Elisp layer delegation
- [ ] Report formatting
- [ ] CLI subcommand wiring
- [ ] Integration tests
