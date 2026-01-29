# Implementation Plan

Priority: Agent-focused development - fast feedback loops and debugging.

---

## Phase 0: Fast Feedback (Spec 31)

**Status:** ✅ Complete
**Priority:** Highest - enables rapid agent iteration

The `./tart` wrapper script enables rapid iteration for agents and developers by skipping redundant nix shell invocations and using incremental builds.

### 0.1 Create `./tart` Script

- [ ] [R1-R8] Create executable `./tart` script at repo root
  - Bash script with `set -euo pipefail`
  - Find repo root via `SCRIPT_DIR`
  - Detect nix shell via `IN_NIX_SHELL` environment variable
  - Run `dune exec tart -- "$@"` directly when in nix shell
  - Wrap with `nix develop --command` when outside
  - Make executable with `chmod +x`

**Files:** `./tart` (new)

**Verification:**
```bash
# Script exists and is executable
test -x ./tart

# Works inside nix shell (fast path)
nix develop --command ./tart --version

# Works outside nix shell (wrapped path)
env -u IN_NIX_SHELL ./tart --version

# Passes arguments correctly
./tart eval '(+ 1 2)'

# Works from subdirectories
cd lib && ../tart --version

# Propagates exit codes
./tart check nonexistent.el; echo $?  # Should be 1
```

---

## Phase 0.5: Verbose Coverage Output (Spec 30)

**Status:** ✅ Complete
**Priority:** High - enables debugging coverage issues

The current `--verbose` implementation just shows covered identifiers. Spec 30 requires detailed diagnostic output about path resolution, version detection, typings loading, and match details.

### 0.5.1 Add Verbose Logging Utility

- [ ] Create `lib/coverage/verbose_log.ml` and `.mli`
- [ ] Implement `verbose_log : bool -> ('a, unit, string, unit) format4 -> 'a`
- [ ] Output to stderr with `[verbose]` prefix

### 0.5.2 Add Verbose Path Resolution Logging

- [ ] Log executable location
- [ ] Log typings root candidates with found/not found status
- [ ] Log final selected typings path

### 0.5.3 Add Verbose Version Detection Logging

- [ ] Log Emacs binary location
- [ ] Log detected version string
- [ ] Log version fallback chain
- [ ] Log selected typings version

### 0.5.4 Add Verbose Typings Loading Logging

- [ ] Log each `.tart` file loaded
- [ ] Log signature count per file
- [ ] Log total signatures loaded

### 0.5.5 Add Verbose C Source Scanning Logging (emacs-coverage)

- [ ] Log source directory being scanned
- [ ] Log per-file DEFUN/DEFVAR/DEFSYM counts
- [ ] Log totals by category

### 0.5.6 Add Verbose Match Summary Logging

- [ ] Log sample covered symbols (first 5) with source file location
- [ ] Log sample uncovered symbols (first 5)
- [ ] Log final match statistics

### 0.5.7 Thread Verbose Flag Through Functions

- [ ] Add `~verbose:bool` parameter to `Emacs_coverage.calculate_coverage`
- [ ] Add `~verbose:bool` parameter to `Coverage_report.analyze_files`
- [ ] Add `~verbose:bool` parameter to `Search_path.load_c_core` (or use callback)

### 0.5.8 Add `-v` Short Form to emacs-coverage Command

- [ ] Parse `-v` as alias for `--verbose`
- [ ] Update help text

**Files:**
- `lib/coverage/verbose_log.ml` (new)
- `lib/coverage/verbose_log.mli` (new)
- `lib/coverage/emacs_coverage.ml`
- `lib/coverage/emacs_coverage.mli`
- `lib/coverage/coverage_report.ml`
- `lib/coverage/coverage_report.mli`
- `lib/sig/search_path.ml`
- `lib/sig/search_path.mli`
- `bin/main.ml`

**Verification:**
```bash
# Verbose output appears on stderr
tart coverage -v . 2>&1 | grep "\[verbose\]"
tart emacs-coverage -v 2>&1 | grep "\[verbose\]"

# Shows path resolution
tart emacs-coverage -v 2>&1 | grep "Typings root"

# Shows version detection
tart emacs-coverage -v 2>&1 | grep "Detected version"

# Shows typings loading
tart emacs-coverage -v 2>&1 | grep ".tart:"

# Report goes to stdout, verbose to stderr
tart emacs-coverage -v > report.txt 2> debug.txt
```

---

## Phase 0.6: Emacs Core Typings Workflow (Spec 32)

**Status:** Partial - BUGS.md structure complete, validation workflow pending
**Priority:** High - systematic expansion of type coverage
**Depends on:** Phase 0 (./tart script), Phase 0.5 (verbose coverage)

This phase establishes the workflow for systematically creating complete, verified type signatures for Emacs C core primitives.

### 0.6.1 Create BUGS.md Structure

- [ ] [R5] Create `typings/emacs/BUGS.md` for cross-version issues
- [ ] [R6] Create `typings/emacs/31.0/BUGS.md` for version-specific issues
- [ ] [R7,R8] Document format: function name, source location, category, description

Categories:
- **type-system-gap**: Needs features tart doesn't have (dependent types, row polymorphism)
- **untypeable**: Behavior can't be captured soundly (dynamic dispatch, eval-based)
- **ergonomic**: Typeable but awkward (excessive annotations at call sites)
- **version-specific**: Signature changed between Emacs versions

### 0.6.2 Workflow for Each C Source File

For each C file (data.c, fns.c, eval.c, alloc.c, etc.):

1. **Extract symbols** - Run `./tart emacs-coverage -v` to list DEFUNs/DEFVARs
2. **Write signatures** - Create/update corresponding `.tart` file
3. **Validate** - Run `./tart check` against Emacs lisp/ directory
4. **Debug** - Use `./tart emacs-coverage -v` to confirm signatures loaded
5. **Document gaps** - Log untypeable items to appropriate BUGS.md
6. **Iterate** - Fix type errors until 95%+ pass rate

### 0.6.3 Acceptance Criteria Per File

- [ ] 100% symbol coverage (every public DEFUN/DEFVAR has a type signature)
- [ ] 95%+ validation success against Emacs lisp/ usages
- [ ] All untypeable items documented in BUGS.md with category

### 0.6.4 Current C-Core Files Status

The following files exist in `typings/emacs/31.0/c-core/`:

| File | Source | Status | Needs Work |
|------|--------|--------|------------|
| data.tart | data.c | Exists | Validate against lisp/, document gaps |
| fns.tart | fns.c | Exists | Validate against lisp/, document gaps |
| eval.tart | eval.c | Exists | Validate against lisp/, document gaps |
| alloc.tart | alloc.c | Exists | Validate against lisp/, document gaps |
| buffer.tart | buffer.c | Exists | Validate against lisp/, document gaps |
| window.tart | window.c | Exists | Validate against lisp/, document gaps |
| frame.tart | frame.c | Exists | Validate against lisp/, document gaps |
| fileio.tart | fileio.c | Exists | Validate against lisp/, document gaps |
| editfns.tart | editfns.c | Exists | Validate against lisp/, document gaps |
| search.tart | search.c | Exists | Validate against lisp/, document gaps |
| process.tart | process.c | Exists | Validate against lisp/, document gaps |
| keyboard.tart | keyboard.c | Exists | Validate against lisp/, document gaps |
| keymap.tart | keymap.c | Exists | Validate against lisp/, document gaps |
| minibuf.tart | minibuf.c | Exists | Validate against lisp/, document gaps |
| textprop.tart | textprop.c | Exists | Validate against lisp/, document gaps |
| print.tart | print.c | Exists | Validate against lisp/, document gaps |

Note: 30.1 and 29.1 directories are backfills from 31.0; version-specific differences need to be audited.

---

## Phase 0.7: Typing Test Fixtures (Spec 33)

**Status:** ✅ Complete
**Priority:** High - documents type checker behavior, enables systematic testing
**Depends on:** Phase 27 (test harness), Spec 25 completed

Comprehensive fixture suite covering pass/fail cases for all error categories.

### 0.7.1 Create Error Category Directories

- [ ] Create `test/fixtures/typing/errors/type-mismatch/`
- [ ] Create `test/fixtures/typing/errors/arity/`
- [ ] Create `test/fixtures/typing/errors/unbound/`
- [ ] Create `test/fixtures/typing/errors/occurs-check/`
- [ ] Create `test/fixtures/typing/errors/kind/`
- [ ] Create `test/fixtures/typing/errors/exhaustiveness/`

**Files:** 6 new directories under `test/fixtures/typing/errors/`

**Verification:**
```bash
ls -la test/fixtures/typing/errors/
# Should show all 6 directories
```

### 0.7.2 Type Mismatch Fixtures (R1)

- [ ] Create `errors/type-mismatch/int-for-string.el` - passing int where string expected
- [ ] Create `errors/type-mismatch/string-for-int.el` - passing string where int expected
- [ ] Create `errors/type-mismatch/list-for-atom.el` - passing list where atom expected
- [ ] Create `errors/type-mismatch/function-arity.el` - function type with wrong arity
- [ ] Create `errors/type-mismatch/polymorphic.el` - incompatible instantiations of type variable
- [ ] Generate matching `.expected` files via `./tart check`

**Files:**
- `test/fixtures/typing/errors/type-mismatch/int-for-string.el` + `.expected`
- `test/fixtures/typing/errors/type-mismatch/string-for-int.el` + `.expected`
- `test/fixtures/typing/errors/type-mismatch/list-for-atom.el` + `.expected`
- `test/fixtures/typing/errors/type-mismatch/function-arity.el` + `.expected`
- `test/fixtures/typing/errors/type-mismatch/polymorphic.el` + `.expected`

**Verification:**
```bash
dune test
# All type-mismatch fixtures should pass
./tart check test/fixtures/typing/errors/type-mismatch/int-for-string.el
# Should output type mismatch diagnostic
```

### 0.7.3 Arity Error Fixtures (R2)

- [ ] Create `errors/arity/too-few-args.el` - missing required arguments
- [ ] Create `errors/arity/too-many-args.el` - excess arguments to fixed-arity function
- [ ] Create `errors/arity/optional-required.el` - omitting required arg when optional exists
- [ ] Generate matching `.expected` files

**Files:**
- `test/fixtures/typing/errors/arity/too-few-args.el` + `.expected`
- `test/fixtures/typing/errors/arity/too-many-args.el` + `.expected`
- `test/fixtures/typing/errors/arity/optional-required.el` + `.expected`

**Verification:**
```bash
dune test
# All arity fixtures should pass
./tart check test/fixtures/typing/errors/arity/too-few-args.el
# Should output arity diagnostic
```

### 0.7.4 Unbound Identifier Fixtures (R3)

- [ ] Create `errors/unbound/unbound-var.el` - reference to undefined variable
- [ ] Create `errors/unbound/unbound-fn.el` - call to undefined function
- [ ] Create `errors/unbound/typo.el` - realistic typo in common function name
- [ ] Create `errors/unbound/scoping.el` - variable used outside its let scope
- [ ] Generate matching `.expected` files

**Files:**
- `test/fixtures/typing/errors/unbound/unbound-var.el` + `.expected`
- `test/fixtures/typing/errors/unbound/unbound-fn.el` + `.expected`
- `test/fixtures/typing/errors/unbound/typo.el` + `.expected`
- `test/fixtures/typing/errors/unbound/scoping.el` + `.expected`

**Verification:**
```bash
dune test
./tart check test/fixtures/typing/errors/unbound/unbound-var.el
# Should output unbound variable diagnostic
```

### 0.7.5 Occurs Check Fixtures (R4)

- [ ] Create `errors/occurs-check/self-reference.el` - `(setq x (cons x nil))`
- [ ] Create `errors/occurs-check/mutual-recursion.el` - mutually recursive definitions creating cycle
- [ ] Generate matching `.expected` files

**Files:**
- `test/fixtures/typing/errors/occurs-check/self-reference.el` + `.expected`
- `test/fixtures/typing/errors/occurs-check/mutual-recursion.el` + `.expected`

**Verification:**
```bash
dune test
./tart check test/fixtures/typing/errors/occurs-check/self-reference.el
# Should output infinite type / occurs check diagnostic
```

### 0.7.6 Kind Error Fixtures (R5)

- [ ] Create `errors/kind/type-as-value.el` - using type constructor as value
- [ ] Create `errors/kind/value-as-type.el` - using value where type expected (in annotations)
- [ ] Create `errors/kind/wrong-arity-tycon.el` - type constructor with wrong number of args
- [ ] Generate matching `.expected` files

**Files:**
- `test/fixtures/typing/errors/kind/type-as-value.el` + `.expected`
- `test/fixtures/typing/errors/kind/value-as-type.el` + `.expected`
- `test/fixtures/typing/errors/kind/wrong-arity-tycon.el` + `.expected`

**Verification:**
```bash
dune test
./tart check test/fixtures/typing/errors/kind/wrong-arity-tycon.el
# Should output kind mismatch diagnostic
```

### 0.7.7 Exhaustiveness Fixtures (R6)

- [ ] Create `errors/exhaustiveness/missing-case.el` - pcase missing constructor
- [ ] Create `errors/exhaustiveness/missing-nil.el` - list match missing nil case
- [ ] Create `errors/exhaustiveness/missing-default.el` - cond without catch-all
- [ ] Generate matching `.expected` files

**Files:**
- `test/fixtures/typing/errors/exhaustiveness/missing-case.el` + `.expected`
- `test/fixtures/typing/errors/exhaustiveness/missing-nil.el` + `.expected`
- `test/fixtures/typing/errors/exhaustiveness/missing-default.el` + `.expected`

**Verification:**
```bash
dune test
./tart check test/fixtures/typing/errors/exhaustiveness/missing-case.el
# Should output exhaustiveness warning
```

### 0.7.8 Regression Fixtures (R7)

- [ ] Ensure `test/fixtures/typing/regression/` directory exists
- [ ] Create template header for regression fixtures
- [ ] Add initial regression fixtures as bugs are discovered

**Fixture template:**
```elisp
;; Regression: [brief description of the bug]
;; Fixed: [date or commit]
;; test: expect-error "relevant error substring"

;; Code that triggered the bug
```

**Files:**
- `test/fixtures/typing/regression/` (directory already exists)
- Fixtures added as bugs are found and fixed

**Verification:**
```bash
ls test/fixtures/typing/regression/
# Directory should exist, may be empty initially
```

### 0.7.9 Realistic User Scenario Fixtures (R8)

- [ ] Create `errors/type-mismatch/user-config.el` - wrong type in defcustom
- [ ] Create `errors/arity/hook-function.el` - hook function with wrong signature
- [ ] Create `errors/unbound/require-missing.el` - using function without require
- [ ] Generate matching `.expected` files

**Files:**
- `test/fixtures/typing/errors/type-mismatch/user-config.el` + `.expected`
- `test/fixtures/typing/errors/arity/hook-function.el` + `.expected`
- `test/fixtures/typing/errors/unbound/require-missing.el` + `.expected`

**Verification:**
```bash
# Review fixtures manually - should read as plausible user code
cat test/fixtures/typing/errors/type-mismatch/user-config.el
# Should look like realistic init.el snippet, not synthetic test case
```

### 0.7.10 Update Test Harness Discovery (R9)

- [ ] Verify `fixture_test.ml` discovers `errors/` subdirectories automatically
- [ ] No code changes expected - harness already discovers recursively
- [ ] Verify test count increases with new fixtures

**Files:** No changes expected to `test/test_harness/fixture_test.ml`

**Verification:**
```bash
# Before adding fixtures
dune test 2>&1 | grep -E "test|fixture"

# After adding fixtures - count should increase
dune test 2>&1 | grep -E "test|fixture"
```

### 0.7.11 Generate and Review Expected Files

- [ ] Run `./tart check` on each fixture to generate actual output
- [ ] Create `.expected` files with PASS/FAIL status and diagnostic substrings
- [ ] Review all `.expected` files for correctness
- [ ] Ensure diagnostic messages are user-friendly and accurate

**Verification:**
```bash
# All tests pass
dune test

# Review a sample expected file
cat test/fixtures/typing/errors/type-mismatch/int-for-string.expected
# Should show FAIL with type mismatch diagnostic substring
```

---

## Task Summary (New Priority Work)

| Task | Spec | Priority | Complexity | Status |
|------|------|----------|------------|--------|
| Create `./tart` script | 31 | 1 | Low | **Done** |
| Add verbose logging utility | 30 | 2 | Low | **Done** |
| Add verbose path resolution | 30 | 2 | Medium | **Done** |
| Add verbose version detection | 30 | 2 | Medium | **Done** |
| Add verbose typings loading | 30 | 2 | Medium | **Done** |
| Add verbose C scanning | 30 | 2 | Medium | **Done** |
| Add verbose match summary | 30 | 2 | Medium | **Done** |
| Thread verbose through functions | 30 | 2 | Medium | **Done** (via main.ml) |
| Add `-v` short form | 30 | 2 | Low | **Done** |
| Create BUGS.md structure | 32 | 3 | Low | **Done** |
| Version-specific test directive | 25 | 3 | Medium | **Done** |
| Create error category directories | 33 | 4 | Low | **Done** |
| Type mismatch fixtures | 33 | 4 | Medium | **Done** |
| Arity error fixtures | 33 | 4 | Medium | **Done** |
| Unbound identifier fixtures | 33 | 4 | Medium | **Done** |
| Occurs check fixtures | 33 | 4 | Medium | **Done** |
| Kind error fixtures | 33 | 4 | Medium | **Done** |
| Exhaustiveness fixtures | 33 | 4 | Medium | **Done** |
| Regression fixtures | 33 | 4 | Low | **Done** |
| Realistic user scenarios | 33 | 4 | Medium | **Done** |
| Update test harness discovery | 33 | 4 | Low | **Done** |
| Generate/review expected files | 33 | 4 | Medium | **Done** |
| Validate data.tart | 32 | 3 | Medium | Not started |
| Validate fns.tart | 32 | 3 | Medium | Not started |
| Validate eval.tart | 32 | 3 | Medium | Not started |
| Validate remaining c-core files | 32 | 3 | Medium | Not started |

**Implementation complete:**
- Phase 0 (Spec 31) - `./tart` wrapper script for fast iteration ✅
- Phase 0.5 (Spec 30) - Verbose output for coverage debugging ✅
- Phase 0.6.1 (Spec 32) - BUGS.md structure for gap documentation ✅
- Phase 27.5 (Spec 25) - Version-specific test directive ✅
- Phase 0.7 (Spec 33) - Typing test fixtures ✅ (29 fixtures across 6 error categories)

**Remaining work:**
- Phase 0.6.2-0.6.4 (Spec 32) - C-core typings validation workflow (iterative human/agent work)
- Phase 29 (Spec 34) - Funcall and Apply Typing

---

## Completed Phases (Historical)

The following phases document completed work from specs 07-29.

## Phase 1: Complete Signature System (Spec 07)

Foundation for module boundaries and type-checked code.

### 1.1 Function and Variable Loading

- [x] [R5] Load `defun` signatures into type environment
- [x] [R6] Load `defvar` declarations into type environment
- [x] Verify: Type checker uses loaded signatures for calls

### 1.2 Type Aliases

- [x] [R7] Load type aliases (with definition) into type context
- [x] [R8] Load parameterized type aliases with instantiation
- [x] Verify: Aliases expand correctly in type expressions

### 1.3 Opaque Types

- [x] [R9] Load opaque types (no definition) as distinct abstract types
- [x] [R10] Handle opaque types with phantom type parameters
- [x] Verify: Opaque types not unifiable with other types

### 1.4 Module Directives

- [x] [R12] Implement `open` directive (import types, not re-export)
- [x] [R13] Implement `include` directive (inline and re-export)
- [x] Verify: Opened types available but not exported

### 1.5 Struct Imports

- [x] [R11] Implement `import-struct` to generate type, constructor, predicate,
      accessors
- [x] Verify: Struct accessor calls type-check based on slot types

### 1.6 Signature Search Path

- [x] [R15] Implement `tart-type-path` search path configuration
- [x] [R16] Implement module discovery order (sibling, search path, stdlib)
- [x] Verify: `(require 'cl-lib)` loads signatures from search path

### 1.7 Stdlib Signatures

- [x] [R17] Create `stdlib/builtins.tart` with arithmetic, list, string,
      predicate signatures
- [x] [R17] Create `stdlib/cl-lib.tart` with basic cl-lib signatures
- [x] [R17] Create `stdlib/seq.tart` with seq.el signatures
- [x] Verify: Built-in calls type-check correctly

## Phase 2: Migrate to Prek (Spec 16)

Faster git hooks improve every development iteration.

- [x] [R1] Add prek to flake.nix devShell packages
- [x] [R2] Update any docs referencing pre-commit
- [x] [R3] Run `prek install -f` to reinstall hooks
- [x] [R4] Verify all hooks pass with prek

## Phase 3: Emacs LSP Integration (Specs 09, 10)

Enable hover and diagnostics in Emacs via eglot.

### 3.1 CLI LSP Command

- [x] [R9] Wire `tart lsp` to start LSP server on stdio
- [x] [R10] Add `--port` option for TCP mode
- [x] Verify: LSP client connects and receives diagnostics

### 3.2 Eglot Integration

- [x] [R5] Add eglot server configuration for tart
- [x] [R8] Add customization options (`tart-executable`, etc.)
- [x] Verify: `M-x eglot` connects and hover shows types

### 3.3 Basic Documentation

- [x] [R11] Add setup instructions to README
- [x] Verify: README contains installation and usage examples

## Phase 4: Forall Inference (Spec 15)

Reduces boilerplate in signature files.

### 4.1 Implicit Quantification

- [x] [R1] Collect type variables in left-to-right first-occurrence order
- [x] [R2] Explicit quantifiers disable inference; error on unbound vars
- [x] Verify: `(defun seq-map (((a -> b)) (seq a)) -> (list b))` infers `[a b]`

### 4.2 Edge Cases

- [x] [R3] Handle phantom type variables (only in return type)
- [x] [R4] Distinguish quoted literals from type variables
- [x] [R5] Traverse nested arrow types for variable collection
- [x] [R6] Deduplicate quantifier list
- [x] Verify: Inference handles all grammar productions

## Phase 5: Module Boundaries (Spec 12)

Enables type-checked module interactions.

### 5.1 Basic Module Loading

- [x] [R1] Enable type checking for any `.el` file via LSP
- [x] [R2] Verify implementations match `.tart` signatures
- [x] [R3] Load signatures from search path for required modules
- [x] Verify: Mismatched implementations produce errors

### 5.2 Public/Private Distinction

- [x] [R5] Distinguish public (in `.tart`) vs internal (not listed) functions
- [x] [R8] Warn on functions defined but not in signature file
- [x] Verify: Internal functions inferred but not exported

### 5.3 Module Dependencies

- [x] [R6] Load signatures when `(require 'module)` is encountered
- [x] [R7] Handle autoloaded function lookup via prefix-based search
- [x] [R9] Handle circular module dependencies with lazy loading
- [x] Verify: Required module signatures available for type checking

## Phase 6: Error Reporting (Spec 13)

Improve error message quality.

### 6.1 Type Mismatch Formatting

- [x] [R1] Format type mismatch errors with expected/found
- [x] [R2] Format branch type mismatch errors
- [x] [R3] Format Option/nil errors with suggestions
- [x] Verify: Errors show context and suggestions

### 6.2 Name Errors

- [x] [R4] Implement Levenshtein-based typo suggestions
- [x] [R5] Format arity mismatch errors with signature
- [x] [R6] Format signature mismatch errors with both locations
- [x] Verify: Typos suggest similar names

### 6.3 LSP Integration

- [x] [R7] Map rich diagnostics to LSP format with related info
- [x] Verify: LSP clients show related information

## Phase 7: Emacs REPL Integration (Spec 10)

Interactive development workflow. All tooling lives in `tart-mode.el`, separate
from the runtime macros in `tart.el`.

### 7.0 File Split

- [x] Split current `tart.el` into `tart.el` (macros only) and `tart-mode.el`
      (dev tooling)
- [x] Verify: `(require 'tart)` loads only macros, no eglot/comint deps

### 7.1 Inferior Mode

- [x] [R1,R2,R9] Implement `inferior-tart-mode` with comint
- [x] [R3] Implement send-to-REPL commands
- [x] [R4] Implement type/expand inspection commands
- [x] Verify: REPL interaction works from elisp buffers

### 7.2 Minor Mode

- [x] [R7] Implement `tart-mode` minor mode with keymap
- [x] [R10] Add compilation-mode error parsing
- [x] Verify: Keybindings work, errors are clickable

## Phase 8: tart.el Runtime (Spec 14)

Inline type annotations in `.el` files. The runtime macros live in `tart.el`
(minimal, no dependencies). Development tooling is in `tart-mode.el` (see
Phase 7).

### 8.1 Macro Definitions (in tart.el)

- [x] [R8] Implement `tart` macro (expands to form)
- [x] [R8] Implement `tart-type` macro (expands to nil)
- [x] [R8] Implement `tart-declare` macro (expands to nil)
- [x] Verify: Macros expand correctly at runtime

### 8.2 Type Checker Recognition

- [x] [R1] Recognize `(declare (tart ...))` in function definitions
- [x] [R2] Add expression annotation checking (`(tart TYPE FORM)`)
- [x] [R3,R4] Track variable types from annotations
- [x] [R3] Check `setq`/`setf` against declared variable types
- [x] Verify: Type errors on annotation mismatches

### 8.3 Type Aliases

- [x] [R5,R6] Implement file-local type alias scope
- [x] [R7] Enforce invariance for parameterized types
- [x] Verify: Aliases usable in same file, not exported

### 8.4 Integration

- [x] [R10] Verify inline annotations match `.tart` declarations
- [x] [R9] Format error messages for annotation mismatches
- [x] Verify: Mismatched inline/`.tart` produces error

## Phase 9: ADT System (Spec 11)

Runtime representation and pattern matching.

### 9.1 Code Generation

- [x] [R1] Generate constructor functions from ADT definitions
- [x] [R2] Generate predicate functions
- [x] [R3] Generate accessor functions
- [x] [R6] Handle multi-field constructors (vectors)
- [x] [R7] Test recursive type handling
- [x] Verify: ADT construction/access works at runtime

### 9.2 Type Checking

- [x] [R4] Implement pcase type narrowing in branches
- [x] [R5] Implement exhaustiveness checking with warnings
- [x] Verify: Non-exhaustive matches produce warnings

## Phase 10: LSP Incremental (Spec 08)

Performance optimization for large codebases.

### 10.1 Query-Based Caching

- [x] [R9] Implement query-based caching for incremental type checking
- [x] Verify: Edit one function; others not recomputed (log check)

## Dependencies

```
Spec 07 (Signatures) ──┬─> Spec 15 (Forall Inference)
                       ├─> Spec 12 (Module Boundaries)
                       ├─> Spec 14 (tart.el Runtime)
                       └─> Spec 11 (ADT System)

Spec 08 (LSP) ─────────┬─> Spec 09 (CLI) ──> Spec 10 (Emacs)
                       └─> Spec 13 (Error Reporting)

Spec 22 (CI Releases) ──> Spec 23 (Binary Installation)

Spec 27 (Dependency Graph) ──> Spec 26 (LSP Signature Sync)

Spec 24 (Versioned Typings) ──> Spec 25 (Typechecker Test Harness) ──> Spec 33 (Typing Fixtures)
```

## Phase 11: LSP Navigation Features (Spec 08 Phase 2)

Navigation and code intelligence features for IDE productivity.

### 11.1 Go to Definition

- [x] [R12] Implement `textDocument/definition` for function calls
- [x] [R13] Return definition location from defun spans
- [x] [R14] Handle cross-file definitions via signature lookup
- [x] Verify: Clicking on function name jumps to definition

### 11.2 Find References

- [x] [R15] Implement `textDocument/references` for symbols
- [x] [R16] Collect all references to a symbol across the document
- [x] Verify: Shows all usages of a function/variable

### 11.3 Code Actions

- [x] [R17] Implement `textDocument/codeAction` framework
- [x] [R18] Add "Extract function" refactoring
- [x] [R19] Add "Add type annotation" quickfix
- [x] Verify: Code actions appear on type errors

## Phase 12: LSP Completion and Symbols (Future)

Auto-completion and document structure for improved IDE experience.

### 12.1 Document Symbols

- [x] Implement `textDocument/documentSymbol` for outline view
- [x] Return defun, defvar, defconst declarations with their types
- [x] Include nested defuns (inner functions)
- [x] Verify: Emacs imenu/outline shows document structure (manual)

### 12.2 Auto-Completion

- [x] Implement `textDocument/completion` for symbol names
- [x] Complete local variables in scope
- [x] Complete functions from loaded signatures (stdlib, requires)
- [x] Include type information in completion items
- [x] Verify: Typing prefix shows completion candidates with types (manual)

### 12.3 Signature Help

- [x] Implement `textDocument/signatureHelp` for function calls
- [x] Show function signature when cursor is in argument list
- [x] Highlight current parameter position
- [x] Verify: Typing `(mapcar |` shows signature with first param highlighted (manual)

### 12.4 Symbol Rename

- [x] Implement `textDocument/rename` for local symbols
- [x] Rename variables and function definitions within file
- [x] Verify: Renaming updates all references consistently

## Phase 13: Expanded Stdlib

Broader coverage of common Emacs packages.

### 13.1 Buffer and Window Operations

- [x] Add `stdlib/buffers.tart` for buffer manipulation functions
- [x] Add `stdlib/windows.tart` for window management
- [x] Add `stdlib/frames.tart` for frame operations
- [x] Verify: Buffer/window/frame code type-checks correctly

### 13.2 File Operations

- [x] Add `stdlib/files.tart` for file I/O (includes directory operations)
- [x] Verify: File manipulation code type-checks

### 13.3 Text Properties and Overlays

- [x] Add `stdlib/text-properties.tart`
- [x] Add `stdlib/overlays.tart`
- [x] Verify: Text property code type-checks

### 13.4 Common Packages

- [x] Add `stdlib/dash.tart` for dash.el
- [x] Add `stdlib/s.tart` for s.el
- [x] Add `stdlib/f.tart` for f.el
- [x] Verify: Code using these packages type-checks

## Phase 14: Higher-Kinded Types (Spec 17)

Enable polymorphism over type constructors via kind inference.

### 14.1 Kind Representation

- [x] [R1] Add `kind` type to lib/typing/kind.ml
- [x] [R1] Add kind comparison and pretty-printing
- [x] Verify: `dune test`; kinds construct and compare

### 14.2 Kind Defaulting

- [x] [R5] Default existing type variables to kind `*`
- [x] Verify: Existing signatures unchanged

### 14.3 Kind Inference

- [x] [R2] Implement kind inference algorithm in kind_infer.ml
- [x] [R3] Add kind checking to type applications
- [x] Verify: HK type variable `f` inferred as `* -> *`

### 14.4 Explicit Annotations and Errors

- [x] [R4] Parse explicit kind annotations `(f : (* -> *))`
- [x] [R6] Implement kind error formatting
- [x] Verify: Kind mismatch shows expected/found

### 14.5 HK Instantiation

- [x] [R7] Update unification for HK instantiation
- [x] [R8] Test nested/partial type constructors
- [x] Verify: `fmap` instantiation preserves type safety

## Phase 15: Explicit Type Instantiation (Spec 18)

Enable explicit instantiation of polymorphic types at call sites.

### 15.1 Runtime Macro

- [x] [R5] Add `@type` macro to tart.el (expands to function call)
- [x] Verify: ERT tests; `(@type [int] identity 42)` expands to `(identity 42)`

### 15.2 Basic Instantiation

- [x] [R1] Recognize `@type` forms in infer.ml
- [x] [R1] Parse type arguments from vector using sig_parser
- [x] [R1] Apply explicit type arguments during inference
- [x] Verify: `(@type [int] identity 42)` type-checks

### 15.3 Partial Instantiation

- [x] [R3] Handle `_` placeholder for partial instantiation
- [x] Verify: `(@type [_ string] pair 1 "hi")` infers first arg

### 15.4 HK Instantiation

- [x] [R2] Test HK type constructor instantiation
- [x] Verify: `(@type [list int string] fmap ...)` works

### 15.5 Error Handling

- [x] [R4] Validate type argument arity
- [x] [R6] Format error messages with annotation context
- [x] Verify: Wrong arity and type mismatch errors show context

---

## Phase 16: Scoped Type Variables (Spec 19)

Enable type variables to be shared across multiple signatures within a scope.

### 16.1 Parsing and AST

- [x] [R1] Add TypeScope variant to sig_ast.ml
- [x] [R1] Parse type-scope blocks in sig_parser.ml
- [x] Verify: `dune test`; scoped blocks parse correctly

### 16.2 Scope Variable Binding

- [x] [R2] Implement scope variable binding during loading
- [x] [R3] Handle explicit forall inside scope (merges with scope vars)
- [x] [R8] Validate variable binding in scopes (error on unbound)
- [x] Verify: Scoped variables shared; external independent

### 16.3 Advanced Features

- [x] [R4] Integrate with kind inference for HK scoped variables
- [x] [R5] Implement nested scope shadowing
- [x] [R6] Handle opaque types in scopes
- [x] Verify: HK scopes and nesting work correctly

### 16.4 Integration

- [x] [R7] Export scoped declarations with correct polymorphic types
- [x] Verify: Exported functions usable from other modules

---

## Phase 17: Expanded Stdlib Phase 2 (Spec 20)

Additional commonly-used Emacs libraries for broader type checking coverage.

### 17.1 Hash Tables and Maps

- [x] [R1] Create stdlib/ht.tart with ht.el hash table library signatures
- [x] [R2] Create stdlib/map.tart with map.el generic map operations
- [x] Verify: ht.tart and map.tart parse and load

### 17.2 String and Utility Functions

- [x] [R3] Create stdlib/subr-x.tart with subr-x.el signatures
- [x] [R5] Create stdlib/rx.tart with rx-to-string signature
- [x] Verify: subr-x.tart and rx.tart parse and load

### 17.3 Process Management

- [x] [R4] Create stdlib/process.tart with process function signatures
- [x] Verify: process.tart parses and loads

### 17.4 Expanded cl-lib

- [x] [R6] Expand stdlib/cl-lib.tart with additional functions
- [x] Verify: Expanded cl-lib.tart loads

---

## Phase 18: Remove Type Classes

Type classes were implemented per Spec 21, but after evaluation the feature was
deemed unnecessary for Elisp's idioms. Elisp's polymorphism is either fully
parametric (works on any type) or uses explicit higher-order functions (passing
comparators). This phase removes the feature.

### 18.1 Delete Spec and Stdlib

- [x] Delete `specs/21-type-classes.md`
- [x] Delete `stdlib/classes/` directory (eq.tart, ord.tart, show.tart, functor.tart)
- [x] Verify: No `.tart` files reference class/instance syntax

### 18.2 Remove Core Implementation

- [x] Delete `lib/typing/instance.ml` and `lib/typing/instance.mli`
- [x] Delete `test/typing/instance_test.ml`
- [x] Update `lib/tart.ml` to remove `Instance` module export
- [x] Verify: `dune build` succeeds after removal

### 18.3 Remove AST and Parsing

- [x] Remove `DClass` and `DInstance` variants from `lib/sig/sig_ast.ml`
- [x] Remove `class_decl` and `instance_decl` type definitions from sig_ast.ml/mli
- [x] Remove class/instance parsing from `lib/sig/sig_parser.ml`
- [x] Remove constraint syntax (`=>`) parsing from defun signatures
- [x] Verify: `dune test` passes; existing .tart files still parse

### 18.4 Remove Signature Loading

- [x] Remove `DClass`/`DInstance` handling from `lib/sig/sig_loader.ml`
- [x] Remove constraint inference from `lib/sig/forall_infer.ml`
- [x] Verify: Signature loading works without class/instance support

### 18.5 Remove Type System Integration

- [x] Remove `type_constraint` from `lib/core/types.ml`
- [x] Remove constraint field from `scheme` type in `lib/core/type_env.ml`
- [x] Remove `class_constraint_with_span` from `lib/typing/infer.ml`
- [x] Remove constraint tracking from inference result types
- [x] Verify: Type inference works without constraints

### 18.6 Remove Module Check Integration

- [x] Remove `instance_error` type from `lib/typing/module_check.ml`
- [x] Remove instance registry building (Step 10)
- [x] Remove `instance_errors` from check result type
- [x] Remove instance resolution calls
- [x] Verify: Module checking works without instance resolution

### 18.7 Remove Diagnostics

- [x] Remove `E0601` error code from `lib/typing/diagnostic.ml` (already removed)
- [x] Remove `missing_instance` diagnostic function (already removed)
- [x] Verify: All remaining diagnostics work correctly

### 18.8 Update Documentation

- [x] Review `docs/research/*.md` for type class references (research docs - low priority, no changes needed)
- [x] Update README.md to remove type class example
- [x] Update docs/library-authors.adoc to remove type class section
- [x] Update DESIGN.md if needed (no changes needed)
- [x] Verify: Documentation accurate after removal

---

## Phase 19: Dogfood tart.el

Type-check tart's own Emacs Lisp code.

Note: Phase 19 is DEFERRED. Rationale:
- tart.el contains only macros which are recognized patterns in the type checker
  (not via signatures), so a .tart file adds no value
- tart-mode.el uses comint, eglot, compile, and cl-lib extensively; full coverage
  would require first creating signatures for these Emacs libraries
- The return on investment is low until more Emacs libraries have signatures

### 19.1 tart.el Signatures (N/A)

- [x] Analysis: tart.el contains only macros (tart, tart-type, tart-declare, @type)
      which are built-in recognized patterns - signatures not applicable
- [x] Verify: Macros work correctly (tested via tart-tests.el)

### 19.2 tart-mode.el Signatures (Deferred)

Prerequisites needed:
- [ ] stdlib/comint.tart - comint-mode, comint-send-string, etc.
- [ ] stdlib/eglot.tart - eglot-server-programs, eglot-ensure, etc.
- [ ] stdlib/compile.tart - compilation-error-regexp-alist, etc.

After prerequisites:
- [ ] Create `lisp/tart-mode.tart` with signatures for dev tooling
- [ ] Verify: `tart check lisp/tart-mode.el` passes

### 19.3 Fix Any Issues Found (Deferred)

- [ ] Address type errors discovered during dogfooding
- [ ] Document any patterns that are hard to type
- [ ] Verify: Both files type-check cleanly

---

## Phase 20: Documentation

Create comprehensive documentation using AsciiDoc, plus refresh the README.

### 20.1 Documentation Structure

- [x] Create `docs/getting-started.adoc` - Quick start guide
- [x] Create `docs/library-authors.adoc` - Guide for library authors writing `.tart` files
- [x] Create `docs/tooling-setup.adoc` - LSP setup guide for Emacs users
- [x] Create `docs/cli-reference.adoc` - CLI reference (manpage source)

### 20.2 README Refresh

- [x] Rewrite `README.md` with elevator pitch and friendly introduction
  - Lead with "what problem does this solve"
  - Show compelling before/after examples
  - Quick installation instructions
  - Link to full documentation
  - Make development status less prominent

### Documentation Content Outline

**`getting-started.adoc`**
1. What is Tart? (2 paragraphs)
2. Installation (Nix, building from source)
3. Your first `.tart` file
4. Running the type checker
5. Seeing errors in your editor
6. Next steps (links to other guides)

**`library-authors.adoc`**
1. The `.tart` file format
2. Declaring functions (`defun`)
3. Declaring variables (`defvar`)
4. Type aliases (`type`)
5. Opaque types (`opaque`)
6. Generic functions (type parameters, bounds)
7. Module organization (`open`, `include`)
8. Publishing type definitions
9. Stdlib coverage and contributing signatures

**`tooling-setup.adoc`**
1. Prerequisites (Emacs 29+, eglot)
2. Installing tart-mode
3. Configuring eglot for tart
4. Features: hover, diagnostics, go-to-definition
5. The REPL (inferior-tart-mode)
6. Troubleshooting common issues
7. For init.el users (type checking your config)

**`cli-reference.adoc`**
1. Synopsis
2. Description
3. Commands
   - `tart [check]` - Type check files
   - `tart eval` - Evaluate expression
   - `tart expand` - Macro expansion
   - `tart repl` - Interactive REPL
   - `tart lsp` - Language server
4. Options
5. Exit codes
6. Environment variables
7. Files (search paths, stdlib location)
8. Examples
9. See also

---

## Phase 21: E2E Test Harness (Spec 21)

ERT tests for Emacs integration against live tart processes.

### 21.1 Test Infrastructure

- [x] [R1] Create `scripts/run-emacs-tests.sh` test runner
- [x] [R2] Create `lisp/tart-test-helpers.el` with test utilities
- [x] [R14] Create `test/fixtures/e2e/valid.el` and `valid.tart`
- [x] [R14] Create `test/fixtures/e2e/error.el` and `error.tart`
- [x] Verify: `./scripts/run-emacs-tests.sh` runs all tests

### 21.2 LSP Tests

- [x] [R3] Test eglot connects to tart LSP server
- [x] [R4] Test LSP diagnostics for type errors
- [x] [R5] Test LSP hover shows type information
- [x] Verify: All LSP tests pass

### 21.3 REPL Tests

- [x] [R6] Test REPL starts and shows prompt
- [x] [R7] Test REPL evaluates expressions
- [x] [R8] Test REPL ,type command
- [x] Verify: All REPL tests pass

### 21.4 Integration Tests

- [x] [R9] Test send-defun to REPL
- [x] [R10] Test type-at-point
- [x] [R11] Test keybindings
- [x] [R12] Test cleanup removes processes
- [x] [R13] Test timeout behavior
- [x] Verify: All integration tests pass

---

## Phase 22: CI Release Builds (Spec 22)

GitHub Actions workflow for building and releasing tart binaries.

### 22.1 Workflow Setup

- [x] Create `.github/workflows/release.yml`
- [x] Configure v* tag trigger
- [x] Set up build matrix (darwin arm64/x86_64, linux arm64/x86_64)

### 22.2 Build Steps

- [x] Install Nix with DeterminateSystems/determinate-nix-action@v3
- [x] Configure Nix cache with DeterminateSystems/magic-nix-cache-action@v8
- [x] Build with `nix build .#default`
- [x] Rename to `tart-{os}-{arch}` format

### 22.3 Release Job

- [x] Collect artifacts from all matrix builds
- [x] Detect prerelease tags (rc, alpha, beta)
- [x] Create release with softprops/action-gh-release
- [x] Attach all 4 binaries as assets

---

## Phase 23: Binary Installation (Spec 23)

Download prebuilt tart binaries from GitHub releases.

**Prerequisite:** Phase 22 (CI releases) must be complete to publish binaries.

### 23.1 Version Configuration

- [x] [R1] Add `tart-version` defcustom (nil = latest, string = specific version)
- [x] Verify: `.dir-locals.el` with `((emacs-lisp-mode . ((tart-version . "0.2.0"))))` respected

### 23.2 Platform Detection

- [x] [R3] Implement `tart--platform-asset` using `system-type` and `system-configuration`
- [x] Handle darwin/arm64, darwin/x86_64, linux/arm64, linux/x86_64
- [x] Verify: `(tart--platform-asset)` returns correct asset name for current system

### 23.3 Executable Resolution

- [x] [R5] Change `tart-executable` default to `'managed`
- [x] [R5] Implement `tart--resolve-executable` (managed → downloaded binary, string → direct)
- [x] Update existing callers to use `tart--resolve-executable`
- [x] Verify: Default `'managed` uses downloaded binary; string overrides

### 23.4 Binary Download

- [x] [R2] Implement `tart-install-binary` command
- [x] [R2] Query GitHub API for release (latest or `tart-version`)
- [x] [R2] Download to `~/.emacs.d/tart/bin/tart-VERSION`
- [x] [R6] Show progress in echo area during download
- [x] Verify: `M-x tart-install-binary` → binary in `~/.emacs.d/tart/bin/`, executable

### 23.5 Hook-Friendly Eglot

- [x] [R4] Implement `tart--binary-available-p` predicate
- [x] [R4] Add `tart-eglot` with install prompt (kept `tart-eglot-ensure` for compatibility)
- [x] [R4] Prompt to install if binary missing before starting eglot
- [x] Verify: Without binary, `tart-eglot` prompts; after install, eglot connects

### 23.6 Error Handling

- [x] [R7] Handle no network with clear error message
- [x] [R7] Handle asset not found for platform with supported platforms list
- [x] [R7] Handle GitHub rate limit with suggestion to set `GITHUB_TOKEN`
- [x] Verify: Airplane mode → "Network error" message, not hang

---

## Phase 24: Module Dependency Graph (Spec 27)

Track module dependencies for incremental re-checking. Foundation for LSP
signature file synchronization.

### 24.1 Graph Data Structures

- [x] Create `lib/graph/dependency_graph.ml` with forward/reverse index
- [x] Define `module_id` type and `edge_kind` enum
- [x] Implement `direct_dependents` and transitive `dependents` queries
- [x] Verify: `dune test`; graph construction and queries work

### 24.2 Dependency Extraction from .el Files

- [x] [R1] Extract `(require 'foo)` → Require edge
- [x] [R1] Extract `(autoload 'fn "bar")` → Autoload edge
- [x] Verify: Parser extracts both require and autoload edges

### 24.3 Dependency Extraction from .tart Files

- [x] [R2] Extract `(open 'seq)` → Open edge
- [x] [R2] Extract `(include 'dash)` → Include edge
- [x] Verify: Signature parser extracts both open and include edges

### 24.4 Sibling Edge and Core Typings

- [x] [R3] Add implicit sibling edge: `foo.el` → `foo.tart`
- [x] [R6] Add pseudo-module for core typings that all files depend on
- [x] Verify: Sibling and core typing edges present in graph

### 24.5 Incremental Updates

- [x] [R4] Update graph on `didOpen` (parse, add edges)
- [x] [R4] Update graph on `didChange` (re-extract, diff edges)
- [x] [R4] Keep graph entry on `didClose` (file still exists on disk)
- [x] Verify: Graph updates correctly as documents open/change/close

### 24.6 Invalidation Cascade

- [x] [R5] Implement invalidation: module X changes → get dependents → invalidate caches
- [x] Wire to form_cache.ml for cache invalidation
- [x] Verify: Changing a module invalidates dependent module caches

### 24.7 Cycle Detection

- [x] [R7] Detect cycles during graph construction
- [x] Report cycles as errors in `.tart` files, warnings in `.el` files
- [x] Verify: Circular requires produce appropriate diagnostics

---

## Phase 25: LSP Signature File Synchronization (Spec 26)

Keep type checking in sync when editing `.tart` signature files alongside `.el` files.

**Prerequisite:** Phase 24 (dependency graph) provides the dependents lookup.

### 25.1 Signature Tracker Module

- [x] Create `lib/lsp/signature_tracker.ml` to track open `.tart` buffers
- [x] Store buffer contents keyed by URI
- [x] Verify: Module compiles and exports required functions

### 25.2 Handle .tart in didOpen

- [x] [R1] Recognize `.tart` files in `textDocument/didOpen`
- [x] Store buffer contents in signature tracker
- [x] Associate with dependent `.el` files via filename convention
- [x] Verify: Opening `.tart` file registers it in tracker

### 25.3 Handle .tart in didChange

- [x] [R2] Update signature tracker on `textDocument/didChange` for `.tart` files
- [x] Query dependency graph for dependents
- [x] Re-publish diagnostics for each dependent `.el` file
- [x] Verify: Changing `.tart` triggers re-check of dependent `.el`

### 25.4 Handle .tart in didClose

- [x] [R4] Remove `.tart` from signature tracker on `didClose`
- [x] Re-check dependents (they'll now read from disk)
- [x] Verify: Closing `.tart` falls back to disk version

### 25.5 Signature Loading Integration

- [x] [R3] Modify sig_loader to check signature tracker first
- [x] If `.tart` is open in LSP → use buffer contents
- [x] Else → read from disk (existing behavior)
- [x] Verify: Open `.tart` buffer contents used for type checking

### 25.6 Form Cache Invalidation

- [x] [R5] When `.tart` changes, invalidate form cache for dependent `.el` files
- [x] Use existing `config_hash` mechanism
- [x] Verify: Cached forms invalidated on signature change

### 25.7 Edge Cases

- [x] Handle `.tart` open but dependent `.el` not open (no action needed)
- [x] Handle `.tart` saved but buffer differs from disk (use buffer)
- [x] Handle multiple `.el` files depending on one `.tart`
- [x] Handle `.tart` parse errors (keep last valid state, show parse error)
- [x] Verify: All edge cases handled gracefully

---

## Phase 26: Versioned Typings Distribution (Spec 24)

Versioned Emacs core typings with auto-detection.

### 26.1 Emacs Version Detection

- [x] Create `lib/sig/emacs_version.ml` module
- [x] Run `emacs --version` and parse major.minor
- [x] Handle missing Emacs gracefully (use `latest/` with warning)
- [x] Verify: Version detection works for Emacs 29, 30, 31

### 26.2 CLI Override Flag

- [x] [R2] Add `--emacs-version VERSION` flag to CLI
- [x] Error if specified directory doesn't exist
- [x] Verify: Override flag takes precedence over detection

### 26.3 Version Fallback in Search Path

- [x] [R3] Implement fallback chain: exact → minor → major → latest
- [x] Example: `31.0.50` → `31.0` → `31` → `latest`
- [x] Verify: Fallback chain finds closest available version

### 26.4 Directory Structure Migration

- [x] Delete `stdlib/` directory entirely
- [x] Create `typings/emacs/31.0/c-core/` directory
- [x] Create `typings/emacs/latest` symlink to `31.0`
- [x] Verify: Search path resolves to new typings location

### 26.5 C-Core Typings for Emacs 31.0

Create signature files mapping 1:1 to Emacs source:

- [x] [R5] Create `typings/emacs/31.0/c-core/data.tart` (eq, null, +, -, car, cdr, predicates)
- [x] [R5] Create `typings/emacs/31.0/c-core/fns.tart` (length, concat, mapcar, assoc)
- [x] [R5] Create `typings/emacs/31.0/c-core/eval.tart` (funcall, apply, signal, catch)
- [x] [R5] Create `typings/emacs/31.0/c-core/alloc.tart` (cons, list, make-vector)
- [x] [R5] Create `typings/emacs/31.0/c-core/buffer.tart` (current-buffer, set-buffer)
- [x] [R5] Create `typings/emacs/31.0/c-core/window.tart` (selected-window, window-buffer)
- [x] [R5] Create `typings/emacs/31.0/c-core/frame.tart` (selected-frame, frame-parameters)
- [x] [R5] Create `typings/emacs/31.0/c-core/fileio.tart` (find-file-noselect, write-region)
- [x] [R5] Create `typings/emacs/31.0/c-core/editfns.tart` (point, goto-char, insert)
- [x] [R5] Create `typings/emacs/31.0/c-core/search.tart` (re-search-forward, match-string)
- [x] [R5] Create `typings/emacs/31.0/c-core/process.tart` (start-process, process-send-string)
- [x] [R5] Create `typings/emacs/31.0/c-core/keyboard.tart` (read-key-sequence)
- [x] [R5] Create `typings/emacs/31.0/c-core/keymap.tart` (define-key, lookup-key)
- [x] [R5] Create `typings/emacs/31.0/c-core/minibuf.tart` (read-string, completing-read)
- [x] [R5] Create `typings/emacs/31.0/c-core/textprop.tart` (get-text-property, put-text-property)
- [x] [R5] Create `typings/emacs/31.0/c-core/print.tart` (prin1, princ, message)
- [x] Verify: All c-core files parse and load without conflicts

### 26.6 Multi-File C-Core Loading

- [x] Update search_path.ml to load all `.tart` files in c-core directory
- [x] Merge signatures; error on conflicts at load time
- [x] Verify: Type checking works with split c-core files

### 26.7 LSP Version Detection

- [x] [R7] Detect Emacs version once at LSP startup
- [x] Use detected version for entire session
- [x] Log detected version at debug level
- [x] Verify: LSP logs which typings version is being used

### 26.8 Backfill Older Versions

- [x] Copy `31.0/` to `30.1/` and `29.1/`
- [ ] Diff against Emacs source for version-specific changes (future work)
- [ ] Add JSON, tree-sitter, SQLite, etc. signatures where appropriate (future work)
- [x] Verify: Typings work for Emacs 29.1 and 30.1

Note: Version-specific differences between 29.1, 30.1, and 31.0 are tracked in Phase 0.6 (Spec 32)

---

## Phase 27: Typechecker Test Harness (Spec 25)

Fixture-based acceptance tests for type checker output.

**Prerequisite:** Phase 26 (versioned typings) provides version-aware type loading.

### 27.1 Expected File Parser

- [x] Define `.expected` file format (PASS/FAIL on line 1, diagnostics following)
- [x] Implement parser in `lib/test_harness/acceptance.ml`
- [x] Verify: Parser handles both PASS and FAIL expectations

### 27.2 Acceptance Harness Core

- [x] Implement `check_fixture : path:string -> Fixture_result.t`
- [x] Implement `run_all : dir:string -> Summary.t`
- [x] Support parallel execution for speed
- [x] Verify: Single fixture and batch execution work

### 27.3 Core Typings Fixtures

Create fixtures exercising each C primitive category:

- [x] [R5] Create `test/fixtures/typing/core/arithmetic.el` (+, -, *, /, mod)
- [x] [R5] Create `test/fixtures/typing/core/lists.el` (car, cdr, cons, nth, mapcar)
- [x] [R5] Create `test/fixtures/typing/core/strings.el` (concat, substring, upcase)
- [x] [R5] Create `test/fixtures/typing/core/predicates.el` (null, listp, stringp)
- [x] [R5] Create `test/fixtures/typing/core/control.el` (funcall, apply, signal)
- [x] Include both passing and failing cases in each fixture
- [x] Verify: All core fixtures have matching `.expected` files

### 27.4 Diagnostic Assertions

- [x] [R2] Support `test: expect-error "substring"` comment syntax
- [x] [R3] Support `test: expect-error-at 6:1` location assertions
- [x] Verify: Diagnostic content and location assertions work

### 27.5 Version-Specific Tests

- [x] [R4] Support `test: emacs-version 31.0` directive
- [x] Create `test/fixtures/typing/version/` directory
- [x] Add version-specific fixtures (e.g., version-30.el example)
- [x] Verify: Version-specific fixtures run with correct typings

### 27.6 Regression Fixtures

- [x] [R6] Create `test/fixtures/typing/regression/` directory
- [ ] Add fixtures named after issues/bugs as found
- [ ] Verify: Regression fixtures document expected behavior

### 27.7 Integration with dune test

- [x] [R8] Wire harness into `dune test` as acceptance_test.ml
- [x] Show diff between expected and actual on failures
- [x] [R9] Run fixtures in parallel for speed
- [x] Target: Full suite <30s
- [x] Verify: `dune test` includes acceptance tests

---

## Priority Order

### Completed Phases (Specs 07-29)

1. **Phase 1**: Signature system ✓
2. **Phase 2**: Prek migration ✓
3. **Phase 3**: Emacs LSP integration ✓
4. **Phase 4**: Forall inference ✓
5. **Phase 5**: Module boundaries ✓
6. **Phase 6**: Error reporting ✓
7. **Phase 7**: Emacs REPL ✓
8. **Phase 8**: tart.el Runtime ✓
9. **Phase 9**: ADT system ✓
10. **Phase 10**: LSP incremental ✓
11. **Phase 11**: LSP navigation features ✓
12. **Phase 12**: LSP completion and symbols ✓
13. **Phase 13**: Expanded stdlib ✓
14. **Phase 14**: Higher-Kinded Types ✓
15. **Phase 15**: Explicit Type Instantiation ✓
16. **Phase 16**: Scoped Type Variables ✓
17. **Phase 17**: Expanded Stdlib Phase 2 ✓
18. **Phase 18**: Remove Type Classes ✓
19. **Phase 19**: Dogfood tart.el (DEFERRED - needs prerequisite stdlib)
20. **Phase 20**: Documentation ✓
21. **Phase 21**: E2E Test Harness ✓
22. **Phase 22**: CI Release Builds ✓
23. **Phase 23**: Binary Installation ✓
24. **Phase 24**: Module Dependency Graph ✓
25. **Phase 25**: LSP Signature File Synchronization ✓
26. **Phase 26**: Versioned Typings Distribution ✓
27. **Phase 27**: Typechecker Test Harness (partial - version-specific tests pending)
28. **Phase 28**: Coverage Report ✓
29. **Phase 29**: Emacs Coverage ✓

### Current Priority (Specs 30-33)

**Active development focus:**

1. **Phase 0** (Spec 31): Fast Feedback - `./tart` wrapper script
   - **Status:** Complete ✅
   - **Priority:** Highest - unblocks all subsequent work
   - **Complexity:** Low (single bash script)

2. **Phase 0.5** (Spec 30): Verbose Coverage Output
   - **Status:** Complete ✅
   - **Priority:** High - enables debugging
   - **Complexity:** Medium (threading verbose flag through multiple modules)

3. **Phase 0.6** (Spec 32): Emacs Core Typings Workflow
   - **Status:** Partial (BUGS.md structure complete)
   - **Priority:** High - systematic type coverage expansion
   - **Complexity:** Medium (validation work, gap documentation)
   - **Depends on:** Phase 0, Phase 0.5

4. **Phase 0.7** (Spec 33): Typing Test Fixtures
   - **Status:** Not started
   - **Priority:** High - documents type checker behavior
   - **Complexity:** Medium (fixture creation, expected file generation)
   - **Depends on:** Phase 27 (test harness)

5. **Phase 29** (Spec 34): Funcall and Apply Typing
   - **Status:** Not started
   - **Priority:** High - type-safe dynamic dispatch
   - **Complexity:** High (dual namespace, apply/funcall special forms, tuple subtyping)
   - **Depends on:** Spec 15 (forall inference) ✓, Spec 17 (HKT) ✓

### Deferred Work

- **Phase 19**: Dogfood tart.el - needs comint, eglot, compile signatures

---

## Phase 28: Coverage Report (Spec 28)

Command to measure type signature coverage for Emacs packages.

**Prerequisite:** Phase 27 (test harness) for testing.

### 28.1 Definition Extractor ✓

- [x] [R4] Extract function definitions (defun, defsubst, cl-defun, defmacro, cl-defmacro)
- [x] [R5] Extract variable definitions (defvar, defcustom, defconst, defvar-local)
- [x] [R6] Extract struct definitions (cl-defstruct with generated accessors)
- [x] [R7] Extract EIEIO class definitions (defclass with slots)
- [x] [R8] Extract face definitions (defface)
- [x] Verify: Extractor captures all standard definition forms

### 28.2 Private Identifier Detection ✓

- [x] [R9] Detect private identifiers (containing `--`)
- [x] Mark private vs public for coverage calculation
- [x] Verify: `my-pkg--private` marked private, `my-pkg-public` marked public

### 28.3 Directory Scanning ✓

- [x] [R1] Default to scanning current directory recursively
- [x] [R2] Accept explicit file/directory path arguments
- [x] [R17] Support `--exclude` patterns for test files
- [x] Verify: `tart coverage src/` scans all `.el` files

### 28.4 Coverage Calculation ✓

- [x] [R10] Compare definitions against sibling `.tart` files
- [x] Match against signatures in search path
- [x] Calculate coverage percentage (public only)
- [x] Verify: Covered = has matching signature, uncovered = no match

### 28.5 Report Generation ✓

- [x] [R11] Summary format with counts and percentage
- [x] [R12] Uncovered list with file:line locations
- [x] [R13] Private list (excluded from percentage)
- [x] [R15] JSON output with `--format=json`
- [x] [R18] Verbose mode with `--verbose`
- [x] Verify: Both human and machine-readable outputs work

### 28.6 CLI Integration ✓

- [x] [R3] Add `tart coverage` and `tart cov` alias
- [x] [R14] Exit codes (0 success, 1 error, 2 usage)
- [x] [R16] `--fail-under=N` threshold flag
- [x] Verify: CLI matches existing tart conventions

---

## Phase 29: Funcall and Apply Typing (Spec 34)

Type `funcall` and `apply` accurately using tracked function types, dual namespaces, and tuple/list subtyping.

**Dependencies:** Spec 15 (forall inference) ✓, Spec 17 (HKT) ✓

### Current State

- `funcall` and `apply` typed as `(forall (r) (-> (Any &rest Any) r))` in `builtin_types.ml`
- No special handling in inference engine — treated as regular built-in functions
- Single namespace environment (`Type_env.t` has unified `bindings`)
- No tuple-list subtyping in unification
- No occurrence typing for type predicates

### 29.1 Dual Namespace Environment (Priority 1 — Foundation)

- [ ] [R1] Add `fn_bindings` field to `lib/core/type_env.ml` alongside `bindings`
- [ ] [R1] Add `lookup_fn` function to look up in function namespace
- [ ] [R1] Update `add_binding` or add `add_fn_binding` for function namespace
- [ ] [R1] Wire through `Env` module in `lib/typing/env.ml`
- [ ] Update `defun` to add bindings to function namespace
- [ ] Update `defalias` to add bindings to function namespace
- [ ] Keep `let`, `setq`, `defvar` in variable namespace (existing behavior)

**Files:** `lib/core/type_env.ml`, `lib/core/type_env.mli`, `lib/typing/env.ml`, `lib/typing/env.mli`, `lib/typing/infer.ml`

**Verify:** `dune test`; same name can have different types in each namespace

### 29.2 Sharp-Quote Function Lookup (Priority 1 — Foundation)

- [ ] [R2] Pattern match `(function name)` special form in `infer.ml`
- [ ] [R2] Look up `name` in function namespace
- [ ] [R2] Return function type directly
- [ ] [R3] Verify variable refs (symbols not in function position) use variable namespace

**Files:** `lib/typing/infer.ml`

**Verify:** `dune test`; `#'name` returns function type from function env

### 29.3 Funcall Type Checking (Priority 2 — Core)

- [ ] [R4] Detect `(funcall f arg1 arg2 ...)` form in `infer.ml`
- [ ] [R4] Infer type of `f`, constrain to be function type `(-> (T1 T2 ...) R)`
- [ ] [R4] Constrain each arg against corresponding param type
- [ ] [R4] Return result type `R`
- [ ] [R5] When `f` is `#'name`, use function namespace lookup (automatic via R2)

**Files:** `lib/typing/infer.ml`

**Verify:** `dune test`; funcall checks function type against arguments

### 29.4 Funcall Error Messages (Priority 2 — Core)

- [ ] [R6] Add funcall-specific context to `lib/typing/constraint.ml`
- [ ] [R6] Add "expected function type, got X" error message
- [ ] [R6] Add "in funcall argument N" context for arg mismatches

**Files:** `lib/typing/constraint.ml`, `lib/typing/diagnostic.ml`

**Verify:** `dune test`; funcall type errors are descriptive

### 29.5 Apply with Rest-Arg Functions (Priority 2 — Core)

- [ ] [R7] Detect `(apply f args... list)` form in `infer.ml`
- [ ] [R7] When `f` has `&rest T` parameter type:
  - Constrain all fixed args to `T`
  - Constrain final list arg to `(List T)`
- [ ] [R7] Return result type

**Files:** `lib/typing/infer.ml`

**Verify:** `dune test`; apply with &rest functions checks element types

### 29.6 Apply with Fixed-Arity Functions (Priority 2 — Core)

- [ ] [R8] When `f` has fixed arity N and apply has M fixed args + tuple:
  - Infer tuple type for final argument
  - Tuple must have exactly (N - M) elements
  - Each tuple element type matches corresponding param

**Files:** `lib/typing/infer.ml`

**Verify:** `dune test`; apply with tuples checks arity and positional types

### 29.7 Tuple-List Subtyping (Priority 2 — Core)

- [ ] [R9] Add tuple-to-list subtyping in `lib/typing/unify.ml`
- [ ] [R9] `(Tuple T1 T2 ... Tn)` unifies with `(List T)` when all Ti unify with T
- [ ] [R9] One-directional: tuple → list, not list → tuple

**Files:** `lib/typing/unify.ml`

**Verify:** `dune test`; tuples unify with compatible list types

### 29.8 Context-Sensitive Tuple Inference (Priority 2 — Core)

- [ ] [R10] In apply position, infer quoted list literals as tuple types
- [ ] [R10] `'(1 "x")` → `(Tuple Int String)` instead of `(List (Or Int String))`

**Files:** `lib/typing/infer.ml`

**Verify:** `dune test`; list literals infer as tuples in apply context

### 29.9 Union Function Types (Priority 3 — Enhancement)

- [ ] [R11] Handle `f : (Or (-> A1 R1) (-> A2 R2))` in funcall
- [ ] [R11] Args must satisfy intersection of param types
- [ ] [R11] Result is union of return types
- [ ] May require constraint system extension

**Files:** `lib/typing/infer.ml`, possibly `lib/typing/constraint.ml`

**Verify:** `dune test`; union function types check all variants

### 29.10 Occurrence Typing for Predicates (Priority 3 — Enhancement)

- [ ] [R12] Track predicate tests in `if` condition
- [ ] [R12] Narrow variable types in consequent branch
- [ ] [R12] Implement for type predicates: `stringp`, `listp`, `functionp`, `numberp`, etc.
- [ ] [R13] Thread narrowed types through funcall

**Files:** `lib/typing/infer.ml`

**Verify:** `dune test`; type predicates narrow types in branches

### 29.11 Never Unify to Any (Priority 3 — Enhancement)

- [ ] [R14] Audit unification for implicit `Any` widening paths
- [ ] [R14] Ensure incompatible types produce errors, not `Any`

**Files:** `lib/typing/unify.ml`

**Verify:** `dune test`; type mismatches produce errors, not Any

### 29.12 Cleanup and Testing

- [ ] Remove weak `funcall`/`apply` signatures from `builtin_types.ml`
- [ ] Add test fixtures for R1-R3 (dual namespace)
- [ ] Add test fixtures for R4-R6 (funcall success/error)
- [ ] Add test fixtures for R7-R8 (apply with rest/fixed-arity)
- [ ] Add test fixtures for R9-R10 (tuple-list subtyping)
- [ ] Add test fixtures for R11-R14 (union types, occurrence typing)

**Files:** `lib/typing/builtin_types.ml`, `test/fixtures/typing/`

**Verify:** `dune test`; all new fixtures pass

### Discovered Issues

- **Dual namespace is pervasive** — updating `Type_env.t` touches many files; expect ~10 files to update
- **Occurrence typing (R12, R13) is significant** — may warrant its own spec or defer
- **Union function types (R11)** — requires careful thought about constraint solving; may defer

### Suggested Implementation Order

1. **R1 → R2 → R3** (namespace foundation) — enables all subsequent work
2. **R4 → R5** (basic funcall) — immediate value for users
3. **R7 → R8 → R9 → R10** (apply and tuples) — completes core functionality
4. **R6 → R14** (error handling) — polish
5. **R11 → R12 → R13** (advanced) — consider deferring if complexity is high

---

## Future Work (Requires New Specs)

The following areas are mentioned as future work in the specs:

1. **Additional Stdlib Coverage**
   - More Emacs packages (org-mode, magit, etc.)
   - Additional third-party package signatures

2. **Feature Availability Detection**
   - Detect optional Emacs features (JSON, tree-sitter, SQLite, etc.)
   - Conditional typing based on available features

3. **Separate Typings Repository**
   - Move typings to separate repo for independent versioning
   - Community contributions for third-party packages

4. **GUI Backend Typings**
   - ns.c, w32.c, pgtk.c, x11.c platform-specific functions

5. **Lisp-Core Typings**
   - subr.el, simple.el after C coverage is solid
