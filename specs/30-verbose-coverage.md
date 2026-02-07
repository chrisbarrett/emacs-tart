# Spec 30: Verbose Coverage Output

Add `--verbose|-v` flag to coverage commands for debugging and transparency.

**Deps:** [Spec 28][] (coverage report), [Spec 29][] (emacs-coverage).

## Links

### Deps
[Spec 28]: ./28-coverage-report.md
[Spec 29]: ./29-emacs-coverage.md

### Blocks
[Spec 32]: ./32-emacs-core-typings.md

## Goal

When coverage reports show unexpected results (0% when signatures exist, missing matches), users need visibility into what paths are being searched, what files are being loaded, and why matches succeed or fail.

## Constraints

| Constraint | Detail |
|------------|--------|
| Opt-in | Verbose output only when `-v` or `--verbose` is specified |
| Stderr | Diagnostic messages go to stderr, report to stdout |
| Incremental | Show progress as files are processed |
| Actionable | Messages should help diagnose path/loading issues |

## Verbose Output Categories

### Path Resolution

Show where tart is looking for typings:

```
[verbose] Executable: /path/to/tart
[verbose] Typings root candidates:
[verbose]   typings/emacs (not found)
[verbose]   ../typings/emacs (not found)
[verbose]   ../share/tart/typings/emacs (found)
[verbose] Using typings root: /path/to/share/tart/typings/emacs
```

### Version Detection

Show Emacs version detection:

```
[verbose] Detecting Emacs version...
[verbose] Emacs binary: /usr/bin/emacs
[verbose] Detected version: 31.0.50
[verbose] Version fallback chain: 31.0.50 -> 31.0 -> 31 -> latest
[verbose] Using typings version: 31.0
```

### Typings Loading

Show which typing files are loaded:

```
[verbose] Loading c-core typings from /path/to/typings/emacs/31.0/c-core
[verbose]   data.tart: 80 signatures
[verbose]   fns.tart: 95 signatures
[verbose]   eval.tart: 25 signatures
[verbose]   ... (13 more files)
[verbose] Total signatures loaded: 900
```

### C Source Scanning (emacs-coverage only)

Show scanning progress:

```
[verbose] Scanning C source: /path/to/emacs/src
[verbose]   alloc.c: 45 DEFUNs, 12 DEFVARs, 8 DEFSYMs
[verbose]   buffer.c: 89 DEFUNs, 34 DEFVARs, 15 DEFSYMs
[verbose]   ... (115 more files)
[verbose] Total: 1823 DEFUNs, 456 DEFVARs, 2594 DEFSYMs
```

### Match Summary

Show coverage matching details:

```
[verbose] Matching symbols against typings...
[verbose] Sample matches:
[verbose]   car: COVERED (data.tart:42)
[verbose]   cdr: COVERED (data.tart:43)
[verbose]   some-obscure-fn: UNCOVERED
[verbose] Match complete: 900/1823 DEFUNs covered (49.4%)
```

## Requirements

### R1: Add --verbose/-v flag to emacs-coverage

**Given** the user runs `tart emacs-coverage -v`
**When** the command executes
**Then** diagnostic messages are printed to stderr
**And** the normal report is printed to stdout

**Verify:** `tart emacs-coverage -v 2>&1 | grep "\[verbose\]"` shows messages

### R2: Add --verbose/-v flag to coverage command

**Given** the user runs `tart coverage -v .`
**When** the command executes
**Then** diagnostic messages about path resolution and loading are printed
**And** the normal report is printed to stdout

**Verify:** `tart coverage -v . 2>&1 | grep "\[verbose\]"` shows messages

### R3: Show typings path resolution

**Given** verbose mode is enabled
**When** typings are being located
**Then** all candidate paths are shown with found/not found status
**And** the final selected path is clearly indicated

**Verify:** Output shows "Typings root candidates" with status for each

### R4: Show version detection and fallback

**Given** verbose mode is enabled
**When** Emacs version is detected
**Then** the detection method and result are shown
**And** the fallback chain is displayed
**And** the selected typings version is shown

**Verify:** Output shows "Version fallback chain" and selected version

### R5: Show typings file loading

**Given** verbose mode is enabled
**When** typings files are loaded
**Then** each file name and signature count is shown
**And** a total signature count is displayed

**Verify:** Output shows individual .tart files with counts

### R6: Show C source scanning progress (emacs-coverage)

**Given** verbose mode is enabled for emacs-coverage
**When** C source files are scanned
**Then** each file's DEFUN/DEFVAR/DEFSYM counts are shown
**And** totals by category are displayed

**Verify:** Output shows per-file counts for C source scanning

### R7: Show sample matches

**Given** verbose mode is enabled
**When** coverage matching completes
**Then** a sample of matched symbols is shown (first 5 covered, first 5 uncovered)
**And** covered symbols show which typings file contains them

**Verify:** Output shows "Sample matches" with file locations

### R8: Verbose output goes to stderr

**Given** verbose mode is enabled
**When** diagnostic messages are printed
**Then** they go to stderr, not stdout
**And** the report can still be piped/redirected cleanly

**Verify:** `tart emacs-coverage -v > report.txt 2> debug.txt` separates output

## Implementation Notes

Add a `verbose : bool` parameter to:
- `Emacs_coverage.calculate_coverage`
- `Coverage_report.analyze_files`
- `Search_path.load_c_core` (or add logging callback)

Consider a simple logging approach:
```ocaml
let verbose_log enabled fmt =
  if enabled then Printf.eprintf ("[verbose] " ^^ fmt ^^ "\n")
  else Printf.ifprintf stderr fmt
```

## Tasks

- [x] Add --verbose/-v flag to emacs-coverage CLI parsing
- [x] Add --verbose/-v flag to coverage CLI parsing
- [x] Implement path resolution logging
- [x] Implement version detection logging
- [x] Implement typings loading logging
- [x] Implement C scanning progress logging
- [x] Implement match sample logging
- [x] Update help text for both commands

## Status

Complete
