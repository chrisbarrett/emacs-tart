# Implementation Plan

Priority: Agent-focused development - fast feedback loops and debugging.

---

## Active Work

### Phase 30: Structured Errors (Spec 35)

**Status:** Not started
**Priority:** High - foundation for CLI/IO error handling
**Depends on:** Spec 13 (error reporting) ✓

Unified error type system for composable, machine-readable errors.

#### 30.1 Error Type Definition

- [ ] [R1] Create `lib/core/error.ml` with variant type:
  - `Type of Diagnostic.t`
  - `Parse of { message; span }`
  - `Eval of { message; span }`
  - `Io of { path; message }`
  - `Cli of { message; hint option }`
- [ ] [R2] Implement `is_fatal : t -> bool` (Io/Cli fatal; Type/Parse recoverable)
- [ ] [R3] Implement `location : t -> Location.span option`

**Files:** `lib/core/error.ml`, `lib/core/error.mli`

#### 30.2 Formatting

- [ ] [R4] Implement `to_string` for all variants (compiler-style format)
- [ ] [R7] Add `to_json : t -> Yojson.Safe.t` to `Diagnostic.t`
- [ ] [R5] Implement `to_json` for `Error.t`

**Files:** `lib/core/error.ml`, `lib/typing/diagnostic.ml`

#### 30.3 Accumulator and Reporting

- [ ] [R10] Implement `Error.Acc` module (empty, add, add_list, to_list, has_errors)
- [ ] [R11] Add `of_diagnostics : Diagnostic.t list -> t list`
- [ ] [R6] Implement `Error.report` with summary count

**Files:** `lib/core/error.ml`

#### 30.4 CLI Integration

- [ ] [R9] Add `io_error : path:string -> exn:exn -> t`
- [ ] [R8] Migrate CLI errors in `bin/main.ml` to use `Error.t`
- [ ] Add `--format=json` flag to `tart check`

**Files:** `lib/core/error.ml`, `bin/main.ml`

**Verification:**
```bash
dune build
./tart check --format=json bad.el | jq .
./tart check file-with-errors.el  # Shows count at end
```

---

### Phase 31: Cmdliner CLI (Spec 36)

**Status:** Not started
**Priority:** High - better UX for users and agents
**Depends on:** Phase 30 (structured errors)

Migrate manual argument parsing to Cmdliner for declarative CLI.

#### 31.1 Setup

- [ ] [R1] Add cmdliner dependency to `bin/dune`

#### 31.2 Subcommands

- [ ] [R2] Implement `check` subcommand (default) with `--emacs-version`
- [ ] [R3] Implement `eval EXPR` subcommand
- [ ] [R4] Implement `expand [--load FILE]... FILE` subcommand
- [ ] [R5] Implement `repl` subcommand
- [ ] [R6] Implement `lsp [--port PORT] [--log-level LEVEL]` subcommand
- [ ] [R7] Implement `coverage [--format] [--verbose] [--fail-under] [--exclude]`
- [ ] [R8] Implement `emacs-coverage [--emacs-source] [--emacs-version] [--verbose]`

#### 31.3 Validation and Errors

- [ ] [R9] Invalid port error with structured type
- [ ] [R10] Missing required argument errors
- [ ] [R11] Unknown option suggestions (Levenshtein)
- [ ] [R12] Unknown subcommand suggestions
- [ ] [R17] Enum validation (lists valid values)
- [ ] [R18] Range validation (0-100 for --fail-under)
- [ ] [R16] Use structured error type from Spec 35

#### 31.4 Help and Version

- [ ] [R13] Auto-generated `--help` from Cmdliner specs
- [ ] [R14] `--version` flag
- [ ] [R15] Exit codes: 0=success, 1=input error, 2=usage error

#### 31.5 Cleanup

- [ ] Remove manual argument parsing code
- [ ] Update `main.mli`

**Files:** `bin/dune`, `bin/main.ml`, `bin/main.mli`

**Verification:**
```bash
./tart --help
./tart check --help
./tart lsp --prot 8080  # Should suggest --port
./tart coverage --format=xml  # Should list valid values
```

---

### Phase 32: File I/O Errors (Spec 37)

**Status:** Not started
**Priority:** High - clear, actionable file error messages
**Depends on:** Phase 30 (structured errors)

Structured file error handling with suggestions.

#### 32.1 Error Type

- [ ] [R9] Create `lib/errors/file_error.ml` with:
  - `File_not_found of { path; suggestions }`
  - `Permission_denied of { path }`
  - `Is_directory of { path }`
  - `Read_error of { path; message }`
  - `Signature_not_found of { module_name; search_paths; span option }`
- [ ] [R10] Error code mapping (E0001-E0005)

#### 32.2 Suggestions

- [ ] [R7] Levenshtein filename suggestions (distance <= 2)
- [ ] [R8] Missing `.el` extension suggestion
- [ ] [R1] File not found with similar file suggestions
- [ ] [R3] Directory detection with `*.el` glob suggestion

#### 32.3 Error Handling

- [ ] [R2] Permission denied formatting
- [ ] [R6] Read error wrapping with context
- [ ] [R4] Signature not found with search paths tried
- [ ] [R5] `--load` file not found in expand command

#### 32.4 Integration

- [ ] Update `Search_path` to return structured errors
- [ ] Update `lib/syntax/read.ml` to wrap I/O exceptions
- [ ] Update `bin/main.ml` expand command

**Files:** `lib/errors/file_error.ml`, `lib/errors/file_error.mli`, `lib/sig/search_path.ml`, `lib/syntax/read.ml`, `bin/main.ml`

**Verification:**
```bash
./tart check nonexistent.el  # Shows suggestions
./tart check lib/  # Shows directory error
./tart check init.l  # Suggests init.el
./tart expand --load missing.el test.el  # Shows --load context
```

---

### Phase 33: C-Core Typings Validation (Spec 32)

**Status:** Partial (BUGS.md structure done)
**Priority:** Medium - systematic type coverage expansion
**Depends on:** Phase 0 (./tart script) ✓, Phase 0.5 (verbose coverage) ✓

Validate existing c-core typings against Emacs lisp/ directory.

#### 33.1 BUGS.md Structure ✓

- [x] Create `typings/emacs/BUGS.md` for cross-version issues
- [x] Create `typings/emacs/31.0/BUGS.md` for version-specific issues
- [x] Document format: function name, source location, category, description

Categories: type-system-gap, untypeable, ergonomic, version-specific

#### 33.2 Validation Workflow

For each C-core file:

1. Run `./tart emacs-coverage -v` to list DEFUNs/DEFVARs
2. Run `./tart check` against Emacs lisp/ directory
3. Fix type errors in `.tart` files
4. Document gaps in BUGS.md
5. Iterate until 95%+ pass rate

#### 33.3 C-Core Files to Validate

| File | Status |
|------|--------|
| data.tart | Validate |
| fns.tart | Validate |
| eval.tart | Validate |
| alloc.tart | Validate |
| buffer.tart | Validate |
| window.tart | Validate |
| frame.tart | Validate |
| fileio.tart | Validate |
| editfns.tart | Validate |
| search.tart | Validate |
| process.tart | Validate |
| keyboard.tart | Validate |
| keymap.tart | Validate |
| minibuf.tart | Validate |
| textprop.tart | Validate |
| print.tart | Validate |

Acceptance: 100% symbol coverage, 95%+ validation success, gaps documented.

---

### Phase 34: Funcall/Apply Enhancements (Spec 34 deferred items)

**Status:** Core complete (R1-R5, R7); enhancements pending
**Priority:** Medium - advanced type checking features

#### 34.1 Funcall Error Messages

- [ ] [R6] Funcall-specific context in `constraint.ml`
- [ ] [R6] "expected function type, got X" error
- [ ] [R6] "in funcall argument N" context

#### 34.2 Apply with Fixed-Arity

- [ ] [R8] Fixed-arity apply with tuple argument
- [ ] [R9] Tuple-to-list subtyping in `unify.ml`
- [ ] [R10] Context-sensitive tuple inference in apply

#### 34.3 Advanced Features

- [ ] [R11] Union function types in funcall
- [ ] [R12-R13] Occurrence typing for predicates
- [ ] [R14] Audit unification for implicit `Any` widening

#### 34.4 Cleanup

- [ ] Remove weak `funcall`/`apply` from `builtin_types.ml`
- [ ] Add fixtures for R9-R14

**Files:** `lib/typing/constraint.ml`, `lib/typing/unify.ml`, `lib/typing/infer.ml`, `lib/typing/builtin_types.ml`

---

### Phase 35: AST Printer (Spec 38)

**Status:** Not started
**Priority:** High - foundation for round-trip testing
**Depends on:** None

Print `Sexp.t` to valid Emacs Lisp for round-trip testing.

#### 35.1 Module Setup

- [ ] [R17] Create `lib/syntax/print.ml` and `print.mli`
- [ ] Implement `val to_string : Sexp.t -> string`

#### 35.2 Primitive Types

- [ ] [R1] Integer printing
- [ ] [R2] Float printing
- [ ] [R3] Simple string printing (double-quoted)
- [ ] [R4] String escapes (newline, tab, CR, backslash, quote, etc.)
- [ ] [R5] Non-ASCII strings (UTF-8 or `\xNN`/`\uNNNN`)

#### 35.3 Symbols and Keywords

- [ ] [R6] Symbol printing
- [ ] [R7] Keyword printing (leading colon)

#### 35.4 Characters

- [ ] [R8] Simple character literals (`?c` syntax)
- [ ] [R9] Escaped character literals (`?\n`, `?\t`, `?\\`)
- [ ] [R10] Character modifiers (Control, Meta, Shift, Hyper, Alt, Super)

#### 35.5 Compound Types

- [ ] [R11] List printing (parenthesized, space-separated)
- [ ] [R12] Quote forms as reader macros (`'x`, `` `x ``, `,x`, `,@x`, `#'f`)
- [ ] [R13] Vector printing (`#(...)`)
- [ ] [R14] Dotted pair printing (`(a . b)`)
- [ ] [R15] Improper list printing (`(1 2 . 3)`)

#### 35.6 Error Handling

- [ ] [R16] Error node printing (`#<error: ...>`)
- [ ] [R18] Round-trip tests (parse → print → parse = original)

**Files:** `lib/syntax/print.ml`, `lib/syntax/print.mli`

**Verification:**
```bash
dune build
dune test  # Round-trip tests
```

---

### Phase 36: Emacs Reader Oracle (Spec 39)

**Status:** Not started
**Priority:** High - authoritative parsing verification
**Depends on:** Phase 30 (structured errors), Phase 32 (file I/O errors)

Invoke Emacs as gold-standard oracle to verify tart parses correctly.

#### 36.1 Core Oracle

- [ ] [R1, R5] `read_string` with `--batch --quick` mode
- [ ] [R2, R12] `read_file` for multi-form files
- [ ] [R4] PATH-based Emacs lookup

#### 36.2 Error Handling

- [ ] [R3, R9, R11] `emacs_error` type: `Read_error`, `Emacs_not_found`, `Emacs_failed`
- [ ] [R10] Timeout handling (default 5000ms)

#### 36.3 Comparison

- [ ] [R6] `compare_string`/`compare_file` functions
- [ ] [R7] Canonical printing matching `prin1-to-string` conventions
- [ ] [R8] Special read syntax tests (quote, backquote, function, reader macros)
- [ ] [R13] Comment stripping verification

**Files:** `lib/oracle/emacs_reader.ml`, `lib/oracle/emacs_reader.mli`, `lib/oracle/oracle.ml`, `lib/oracle/oracle.mli`, `test/oracle/oracle_test.ml`

**Verification:**
```bash
dune build
dune test
./tart oracle test.el  # Compare tart vs Emacs
```

---

### Phase 37: Content-Addressable Cache (Spec 40)

**Status:** Not started
**Priority:** Medium - performance optimization
**Depends on:** None

XDG-compliant content-addressable caching for incremental type-checking.

#### 37.1 XDG Paths

- [ ] [R1] `cache_dir` using `$XDG_CACHE_HOME/tart/` or `~/.cache/tart/`
- [ ] [R10] `v1/` subdirectory for version evolution
- [ ] [R7] Create directory structure on demand

#### 37.2 Key Computation

- [ ] [R9] `binary_path` using `Sys.executable_name`
- [ ] [R2] `compute_key` with SHA256 of binary + input content

#### 37.3 Store/Retrieve

- [ ] [R3, R8] `store` with atomic writes (temp file + rename)
- [ ] [R4] `retrieve` returning `Some data` or `None`
- [ ] [R5] Graceful miss handling (corrupted file = None)
- [ ] [R6] JSON format with version and timestamp

**Files:** `lib/cache/content_cache.ml`, `lib/cache/content_cache.mli`

**Verification:**
```bash
dune build
dune test
XDG_CACHE_HOME=/tmp/test ./tart check file.el && ls /tmp/test/tart/v1/
```

---

### Phase 38: Emacs Source Corpus (Spec 41)

**Status:** Not started
**Priority:** Medium - testing infrastructure
**Depends on:** Spec 29 (emacs-coverage) ✓

Clone Emacs source in XDG cache for testing against real Elisp.

#### 38.1 Cache Location

- [ ] [R3-4] XDG path resolution (`$XDG_CACHE_HOME/tart/emacs-src/` or `~/.cache/tart/emacs-src/`)

#### 38.2 Clone Management

- [ ] [R1] Clone from `https://git.savannah.gnu.org/git/emacs.git` if not present
- [ ] [R2] Reuse existing clone (no network activity)
- [ ] [R15] `corpus clean` command to remove and report freed bytes

#### 38.3 Version Control

- [ ] [R5] `corpus checkout <tag>` (e.g., `emacs-29.1`)
- [ ] [R6] `corpus checkout <sha>` for specific commits
- [ ] [R7-8] Auto-detect system Emacs version via `emacs --version`
- [ ] [R9] `--emacs-version` option for CI

#### 38.4 File Operations

- [ ] [R10] `corpus list` - all `.el` files, absolute paths
- [ ] [R11] Include generated files (`loaddefs.el`, etc.)
- [ ] [R14] `corpus path` - output absolute path

#### 38.5 Error Handling

- [ ] [R12] Network failure on fresh clone
- [ ] [R13] Network failure on fetch (suggest local tags)

**Files:** `lib/corpus/emacs_corpus.ml`, `lib/corpus/emacs_corpus.mli`, `bin/main.ml`

**Verification:**
```bash
./tart corpus list | wc -l  # 3000+
./tart corpus checkout emacs-29.1
./tart corpus path
./tart corpus clean
```

---

### Phase 39: Round-Trip Test Harness (Spec 42)

**Status:** Not started
**Priority:** High - parsing accuracy verification
**Depends on:** Phase 35 (AST printer), Phase 36 (Emacs oracle), Phase 38 (corpus)

Parse-print-parse testing for 100% parsing accuracy.

#### 39.1 Core Checks

- [ ] [R1] `check_file` - structural equality (parse → print → parse)
- [ ] [R2] `check_file_with_emacs` - Emacs oracle verification

#### 39.2 Failure Reporting

- [ ] [R3] Failure shows path + error with location
- [ ] [R4] Failure shows unified diff of expected vs actual

#### 39.3 Caching

- [ ] [R5] Cache hit skips (content hash matches)
- [ ] [R6] Cache miss triggers full check, update on success

#### 39.4 Corpus Integration

- [ ] [R7] Discover all `.el` files recursively
- [ ] [R12] Parallel execution

#### 39.5 Results

- [ ] [R8-9] Exit non-zero on parse error or mismatch
- [ ] [R10] `dune test` integration
- [ ] [R11] Summary statistics (total, passed, failed, cached)

**Files:** `lib/roundtrip/roundtrip.ml`, `lib/roundtrip/roundtrip.mli`, `lib/roundtrip/cache.ml`, `lib/roundtrip/cache.mli`, `test/roundtrip/roundtrip_test.ml`, `scripts/run-roundtrip.sh`

**Verification:**
```bash
dune test
./scripts/run-roundtrip.sh
./scripts/run-roundtrip.sh --with-emacs
```

---

### Phase 40: CI Version Matrix (Spec 43)

**Status:** Not started
**Priority:** Medium - CI quality
**Depends on:** Phase 39 (round-trip harness)

Multi-version Emacs testing with blocking/advisory tiers.

#### 40.1 Nix Dev Shells

- [ ] [R6] Add `devShells.emacs28`, `devShells.emacs29`, `devShells.emacs30`, `devShells.emacsMain` to flake.nix

#### 40.2 CI Matrix

- [ ] [R1, R9] Version matrix with tier labels (28/29/30 blocking, main advisory)
- [ ] [R7] Parallel execution (`fail-fast: false`)
- [ ] [R10] Nix caching with magic-nix-cache-action

#### 40.3 Failure Handling

- [ ] [R2] Blocking failures fail the build
- [ ] [R3] Advisory failures don't fail build (`continue-on-error`)
- [ ] [R4] Advisory failure warning annotations

#### 40.4 Testing

- [ ] [R5] E2E runs with correct Emacs version per matrix job
- [ ] [R8] Version-specific test skip helpers

**Files:** `.github/workflows/ci.yml`, `flake.nix`

**Verification:**
```bash
nix develop .#emacs29 --command emacs --version
gh workflow run ci.yml
```

---

## Task Summary

| Phase | Spec | Priority | Status |
|-------|------|----------|--------|
| 30 | 35 | 1 | Not started |
| 31 | 36 | 2 | Not started |
| 32 | 37 | 2 | Not started |
| 33 | 32 | 3 | Partial |
| 34 | 34 | 4 | Partial |
| 35 | 38 | 1 | Not started |
| 36 | 39 | 2 | Not started |
| 37 | 40 | 3 | Not started |
| 38 | 41 | 3 | Not started |
| 39 | 42 | 2 | Not started |
| 40 | 43 | 3 | Not started |

Dependencies:
```
Spec 35 (Structured Errors)
    ├─> Spec 36 (Cmdliner CLI)
    ├─> Spec 37 (File I/O Errors)
    └─> Spec 39 (Emacs Reader Oracle)

Spec 38 (AST Printer)
    └─> Spec 42 (Round-Trip Harness)

Spec 39 (Emacs Reader Oracle)
    └─> Spec 42 (Round-Trip Harness)

Spec 41 (Emacs Source Corpus)
    └─> Spec 42 (Round-Trip Harness)

Spec 42 (Round-Trip Harness)
    └─> Spec 43 (CI Version Matrix)

Spec 40 (Content Cache) - standalone
```

---

## Completed Phases

### Infrastructure (Done)
- Phase 0: Fast Feedback - `./tart` wrapper (Spec 31) ✓
- Phase 0.5: Verbose Coverage Output (Spec 30) ✓
- Phase 0.7: Typing Test Fixtures (Spec 33) ✓

### Core Type System (Done)
- Phase 1: Signature System (Spec 07) ✓
- Phase 4: Forall Inference (Spec 15) ✓
- Phase 5: Module Boundaries (Spec 12) ✓
- Phase 9: ADT System (Spec 11) ✓
- Phase 14: Higher-Kinded Types (Spec 17) ✓
- Phase 15: Explicit Instantiation (Spec 18) ✓
- Phase 16: Scoped Type Variables (Spec 19) ✓
- Phase 18: Remove Type Classes ✓
- Phase 29: Funcall/Apply Core (Spec 34 R1-R5, R7) ✓

### Tooling (Done)
- Phase 2: Prek Migration (Spec 16) ✓
- Phase 3: Emacs LSP Integration (Specs 09, 10) ✓
- Phase 6: Error Reporting (Spec 13) ✓
- Phase 7: Emacs REPL (Spec 10) ✓
- Phase 8: tart.el Runtime (Spec 14) ✓
- Phase 10-12: LSP Features (Spec 08) ✓
- Phase 20: Documentation ✓
- Phase 21: E2E Test Harness (Spec 21) ✓
- Phase 22-23: CI Releases & Binary Installation (Specs 22, 23) ✓

### Type Coverage (Done)
- Phase 13, 17: Expanded Stdlib (Spec 20) ✓
- Phase 24-25: Dependency Graph & Signature Sync (Specs 26, 27) ✓
- Phase 26: Versioned Typings (Spec 24) ✓
- Phase 27: Typechecker Test Harness (Spec 25) ✓
- Phase 28-29: Coverage Reports (Specs 28, 29) ✓

### Deferred
- Phase 19: Dogfood tart.el - needs comint/eglot/compile signatures

---

## Future Work (Requires New Specs)

- Additional stdlib coverage (org-mode, magit, etc.)
- Feature availability detection (JSON, tree-sitter, SQLite)
- Separate typings repository
- GUI backend typings (ns.c, w32.c, pgtk.c, x11.c)
- Lisp-core typings (subr.el, simple.el)
- Cache size limits/eviction policies
- Remote/shared caching
- Windows/macOS CI testing
