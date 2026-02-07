# Spec 28: Coverage Report

Command to measure type signature coverage for Emacs packages.

**Dependencies:** Spec 07 (signatures), Spec 09 (CLI).

## Goal

Know how complete type coverage is for Emacs packages, to track progress and
inform Tart development priorities.

## Constraints

- **Actionable**: Show exactly what needs signatures
- **Accurate**: Exclude private identifiers from coverage metrics
- **Fast**: Scan directories quickly, suitable for CI
- **Scriptable**: Exit codes and parseable output for automation

## Output

```
lib/cli/
├── coverage.ml     ; Coverage command implementation
└── coverage.mli
lib/coverage/
├── definition_extractor.ml   ; Extract definitions from .el files
├── definition_extractor.mli
├── coverage_report.ml        ; Compare definitions to signatures
└── coverage_report.mli
test/cli/
└── coverage_test.ml
```

## Requirements

### R1: Default directory scan

**Given** a directory containing `.el` files
**When** `tart coverage` is invoked with no arguments
**Then** the current directory is scanned recursively for `.el` files
**And** each file is analyzed for definitions

**Verify:** `cd some-package && tart coverage` reports on all `.el` files

### R2: Explicit path arguments

**Given** `tart coverage path/to/file.el path/to/dir/`
**When** executed
**Then** files are analyzed directly, directories scanned recursively
**And** only `.el` files are considered

**Verify:** `tart coverage src/` analyzes all `.el` files under `src/`

### R3: Alias support

**Given** `tart cov`
**When** executed
**Then** it behaves identically to `tart coverage`

**Verify:** `tart cov --help` shows coverage command help

### R4: Extract function definitions

**Given** an `.el` file containing:
```elisp
(defun foo () ...)
(defsubst bar () ...)
(cl-defun baz () ...)
(defmacro qux () ...)
(cl-defmacro quux () ...)
(defadvice old-fn ...)
(cl-defmethod generic (obj) ...)
(cl-defgeneric generic (obj))
```
**When** analyzed
**Then** all defining forms are extracted: `foo`, `bar`, `baz`, `qux`, `quux`,
`old-fn`, `generic`

**Verify:** Definition extractor captures all function-slot forms

### R5: Extract variable definitions

**Given** an `.el` file containing:
```elisp
(defvar my-var ...)
(defcustom my-custom ...)
(defconst my-const ...)
(defvar-local my-local ...)
```
**When** analyzed
**Then** all variable definitions are extracted: `my-var`, `my-custom`,
`my-const`, `my-local`

**Verify:** Definition extractor captures all variable-slot forms

### R6: Extract struct definitions

**Given** an `.el` file containing:
```elisp
(cl-defstruct person name age)
```
**When** analyzed
**Then** the following identifiers are extracted:
- Type/constructor: `person`, `make-person`
- Predicate: `person-p`
- Accessors: `person-name`, `person-age`
- Copier: `copy-person`

**Verify:** All struct-generated identifiers are captured

### R7: Extract EIEIO class definitions

**Given** an `.el` file containing:
```elisp
(defclass widget ()
  ((name :initarg :name :accessor widget-name)
   (value :initarg :value)))
```
**When** analyzed
**Then** the following identifiers are extracted:
- Class name: `widget`
- Predicate: `widget-p`
- Constructor: `widget` (as function)
- Accessors: `widget-name`

**Verify:** EIEIO class and slot accessors captured

### R8: Extract face definitions

**Given** an `.el` file containing:
```elisp
(defface my-face ...)
```
**When** analyzed
**Then** `my-face` is extracted as a variable-slot identifier

**Verify:** Face definitions are captured

### R9: Private identifier detection

**Given** definitions including private identifiers (containing `--`):
```elisp
(defun my-pkg-public () ...)
(defun my-pkg--private () ...)
(defvar my-pkg--internal ...)
```
**When** analyzed
**Then** `my-pkg--private` and `my-pkg--internal` are marked as private
**And** `my-pkg-public` is marked as public

**Verify:** `--` in identifier name indicates private

### R10: Compare against signatures

**Given** definitions extracted from `.el` files
**And** corresponding `.tart` signature files
**When** coverage is computed
**Then** a definition is "covered" if a matching signature exists in:
- Sibling `.tart` file (e.g., `foo.tart` for `foo.el`)
- Any `.tart` file in the signature search path

**Verify:** Definition with matching `defun`/`defvar` in `.tart` is covered

### R11: Report format - summary

**Given** analysis is complete
**When** report is printed
**Then** it shows:
```
Coverage Report
===============

Files scanned: 12
Total definitions: 156
  Public: 120
  Private: 36

Public coverage: 85/120 (70.8%)
```

**Verify:** Summary shows counts and percentage for public identifiers only

### R12: Report format - uncovered list

**Given** analysis is complete
**When** report is printed
**Then** uncovered public identifiers are listed:
```
Uncovered public identifiers (35):
  my-pkg-foo          (my-pkg.el:42)
  my-pkg-bar          (my-pkg.el:58)
  my-other-fn         (other.el:12)
```

**Verify:** Each uncovered identifier shows file and line number

### R13: Report format - private list

**Given** analysis is complete
**When** report is printed
**Then** private identifiers are listed separately:
```
Private identifiers excluded (36):
  my-pkg--internal    (my-pkg.el:15)
  my-pkg--helper      (my-pkg.el:89)
```

**Verify:** Private identifiers listed but not counted in coverage percentage

### R14: Exit codes

**Given** any `tart coverage` invocation
**When** it completes
**Then** exit codes are:
- 0: success (regardless of coverage percentage)
- 1: error (parse error, file not found)
- 2: usage error (bad arguments)

**Verify:** `tart coverage nonexistent.el` exits 1

### R15: Machine-readable output

**Given** `tart coverage --format=json`
**When** executed
**Then** output is JSON:
```json
{
  "files_scanned": 12,
  "public": {
    "total": 120,
    "covered": 85,
    "uncovered": ["my-pkg-foo", "my-pkg-bar"]
  },
  "private": {
    "total": 36,
    "identifiers": ["my-pkg--internal", "my-pkg--helper"]
  }
}
```

**Verify:** JSON output is valid and parseable

### R16: Threshold flag

**Given** `tart coverage --fail-under=80`
**When** public coverage is below 80%
**Then** exit code is 1

**Verify:** `tart coverage --fail-under=100` fails on any uncovered identifiers

### R17: Exclude patterns

**Given** `tart coverage --exclude='*-test.el'`
**When** executed
**Then** files matching the pattern are skipped

**Verify:** Test files can be excluded from coverage analysis

### R18: Verbose mode

**Given** `tart coverage --verbose`
**When** executed
**Then** additional details are shown:
- Each file as it's scanned
- Signature files consulted
- Covered identifiers (not just uncovered)

**Verify:** Verbose output helps debug coverage issues

## Tasks

- [x] [R4-R8] Definition extractor for all Elisp forms
- [x] [R9] Private identifier detection
- [x] [R1,R2] Directory/file scanning
- [x] [R3] CLI command with alias
- [x] [R10] Signature matching logic
- [x] [R11-R13] Human-readable report format
- [x] [R14] Exit codes
- [x] [R15] JSON output format
- [x] [R16] Threshold flag
- [x] [R17] Exclude patterns
- [x] [R18] Verbose mode
- [x] Integration tests

## Status

Complete
