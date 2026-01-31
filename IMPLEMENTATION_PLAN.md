# Implementation Plan: Accurate Types via LSP for Emacs Core

**Goal:** Open any core lisp file that ships with Emacs and get accurate types via the LSP.

**Scope:** E2E completion for core Emacs primitives, not complete typing of all packages.

---

## Phase 1: Foundation Fixes (Critical Refactoring)

These tasks fix spec violations in the existing implementation.

### 1.1 Remove Inferred Type Quantifiers

**Problem:** `lib/sig/forall_infer.ml` automatically infers `[vars]` quantifiers when missing. Specs 07 and 15 require **explicit quantification only**—unbound type variables must be errors.

**Files:**
- `lib/sig/forall_infer.ml` — DELETE or gut completely
- `lib/sig/sig_loader.ml` — Remove calls to `Forall_infer.infer_defun` and `Forall_infer.infer_type_decl`
- `lib/sig/sig_parser.ml` — Ensure parser errors on unbound type variables

**Changes:**
1. Remove `Forall_infer` module entirely
2. Update `sig_loader.ml` lines 1086, 1109-1110, 1153-1154, 1174-1175 to remove inference calls
3. Make `sig_loader.validate_type` stricter: lowercase names without `[...]` quantifier should error if not a known type
4. Update all existing `.tart` files to have explicit quantifiers where missing

**Verify:** `(defun identity (a) -> a)` produces error "unbound type variable 'a'"

### 1.2 Create Prelude (Spec 48)

**Problem:** Prelude types (`list`, `option`, `is`, `nonempty`, `any`, `bool`, `t`) are not implicitly available.

**Files:**
- `typings/tart-prelude.tart` — CREATE
- `lib/sig/sig_loader.ml` — Load prelude before other typings

**Content for `tart-prelude.tart`:**
```lisp
;; The symbol 't—Elisp's canonical truthy value
(type t 't)

;; Universal type: any Elisp value
(type any (truthy | nil))

;; Boolean: t or nil (Elisp's boolean convention)
(type bool (t | nil))

;; Homogeneous list (recursive, nullable)
(type list [a] ((cons a (list a)) | nil))

;; Refinement: remove nil from a type
(type is [a] (a - nil))

;; Optional: add nil to a truthy type
(type option [(a : truthy)] (a | nil))

;; Non-empty list (list without nil)
(type nonempty [a] (is (list a)))
```

**Changes:**
1. Create the prelude file
2. Add `load_prelude` function to `sig_loader.ml` that loads prelude before any other typings
3. Ensure prelude types cannot be shadowed (Spec 07 R17)

**Verify:** Any `.tart` file can use `list`, `option` without explicit import

### 1.3 Emacs Version Detection (Spec 24)

**Problem:** Version detection and fallback chain not implemented.

**Files:**
- `lib/sig/emacs_version.ml` — CREATE
- `lib/sig/emacs_version.mli` — CREATE
- `lib/sig/search_path.ml` — Update to use version detection
- `bin/main.ml` — Add `--emacs-version` flag

**Changes:**
1. Implement `detect_version : unit -> string option` (runs `emacs --version`)
2. Implement fallback chain: `31.0.50 → 31.0 → 31 → latest`
3. Add `latest` symlink in `typings/emacs/`
4. Wire version detection into search path resolution
5. LSP detects version once at startup

**Verify:** `./tart check foo.el` auto-detects Emacs version and uses correct typings

---

## Phase 2: Core Type System Completeness

### 2.1 Type Subtraction Operator (Spec 11 R10)

**Problem:** `(a - nil)` syntax not implemented; needed for `is` and `nonempty` prelude types.

**Files:**
- `lib/sig/sig_ast.ml` — Add `STSubtract` variant
- `lib/sig/sig_parser.ml` — Parse `-` as type subtraction
- `lib/core/types.ml` — Add `TSubtract` or handle during normalization
- `lib/typing/unify.ml` — Handle subtraction during unification

**Changes:**
1. Parse `(a - b)` as type subtraction
2. During normalization: `(T1 | T2 | T3) - T2` → `(T1 | T3)`
3. Error if subtraction produces empty type

**Verify:** `(type nonempty [a] (is (list a)))` expands correctly

### 2.2 Bounded Quantifiers Validation (Spec 07 R4, Spec 48 R6)

**Problem:** Bounds like `[(a : truthy)]` are parsed but not enforced at instantiation.

**Files:**
- `lib/typing/infer.ml` — Check bounds when instantiating polymorphic types
- `lib/typing/unify.ml` — Subtype check for bounds

**Changes:**
1. When instantiating `option [a]` with `(int | nil)`, check `(int | nil) <: truthy` fails
2. Emit clear error: "bound violation: (int | nil) not <: truthy"

**Verify:** `(option (option string))` produces type error

### 2.3 No-Shadowing Rule (Spec 07 R17)

**Problem:** User can redefine imported bindings including prelude types.

**Files:**
- `lib/sig/sig_loader.ml` — Track imported names, error on redefinition

**Changes:**
1. Maintain set of imported names (from prelude, opens, includes)
2. When processing `DType`, `DDefun`, `DDefvar`, check name not in imported set
3. Error: "cannot redefine imported binding 'list'"

**Verify:** `(type list int)` in a `.tart` file produces error

---

## Phase 3: LSP Infrastructure

### 3.1 Signature File Synchronization (Spec 26)

**Problem:** Editing `.tart` doesn't update diagnostics in dependent `.el` files.

**Files:**
- `lib/lsp/signature_tracker.ml` — CREATE
- `lib/lsp/signature_tracker.mli` — CREATE
- `lib/lsp/server.ml` — Handle `.tart` in didOpen/didChange/didClose

**Changes:**
1. Track open `.tart` files and their contents
2. When `.tart` changes, find dependent `.el` files and re-publish diagnostics
3. Prefer buffer contents over disk when loading signatures

**Verify:** Edit `foo.tart`, see diagnostics update in `foo.el` without touching it

### 3.2 Dependency Graph (Spec 27)

**Problem:** No tracking of module dependencies for invalidation.

**Files:**
- `lib/graph/dependency_graph.ml` — EXISTS but may need enhancement
- `lib/graph/graph_builder.ml` — EXISTS but may need enhancement
- `lib/lsp/graph_tracker.ml` — EXISTS, wire into signature sync

**Changes:**
1. Extract `require`/`autoload` edges from `.el` files
2. Extract `open`/`include` edges from `.tart` files
3. Implicit sibling edge: `foo.el` → `foo.tart`
4. Core typings as pseudo-module (change invalidates all)
5. Use graph for invalidation cascade on `.tart` change

**Verify:** Changing a shared `.tart` file updates all dependent `.el` diagnostics

---

## Phase 4: Core Typings Validation

### 4.1 Verbose Coverage Output (Spec 30)

**Problem:** No visibility into what typings are loaded and why.

**Files:**
- `lib/coverage/verbose_log.ml` — EXISTS, enhance
- `bin/main.ml` — Ensure `-v` flag works for `emacs-coverage`

**Changes:**
1. Show search path resolution with found/not-found status
2. Show version detection and fallback chain
3. Show per-file signature counts
4. Show sample matches (first 5 covered, first 5 uncovered)

**Verify:** `./tart emacs-coverage -v` shows diagnostic info

### 4.2 Validate Existing C-Core Typings (Spec 32)

**Status:** `typings/emacs/{29.1,30.1,31.0}/c-core/*.tart` exist (17 files each)

**Task:** Run `./tart check` against Emacs's `lisp/` directory with verbose output. Fix type errors iteratively.

**Files to validate (in order):**
1. `data.tart` — `eq`, `null`, `+`, `-`, `car`, `cdr`, predicates
2. `fns.tart` — `length`, `concat`, `mapcar`, `assoc`
3. `alloc.tart` — `cons`, `list`, `make-vector`
4. `eval.tart` — `funcall`, `apply`, `signal`, `catch`
5. `buffer.tart` — `current-buffer`, `set-buffer`
6. `editfns.tart` — `point`, `goto-char`, `insert`
7. (Continue for remaining files)

**Process per file:**
1. Run `./tart emacs-coverage -v` to see coverage
2. Run `./tart check /path/to/emacs/lisp/*.el` (subset)
3. Analyze type errors
4. Fix signatures or document in `BUGS.md`
5. Target: 95%+ success rate per file

### 4.3 Create BUGS.md Structure (Spec 32 R5-R8)

**Files:**
- `typings/emacs/BUGS.md` — CREATE (cross-version issues)
- `typings/emacs/31.0/BUGS.md` — CREATE (version-specific)

**Format:**
```markdown
## type-system-gap
### `apply`
- **Location:** eval.c:2847
- **Issue:** Requires dependent types
- **Suggested feature:** First-class function type introspection

## untypeable
### `funcall`
- **Location:** eval.c:2789
- **Issue:** Dynamic dispatch
- **Resolution:** See Spec 34

## ergonomic
### `mapcar`
- **Location:** fns.c:2567
- **Issue:** Requires explicit annotation at most call sites

## version-specific
### `json-parse-string`
- **Location:** json.c:523
- **Issue:** :object-type keyword added in Emacs 28
```

---

## Phase 5: Error Quality & Developer Experience

### 5.1 Error Code Registry (Spec 47)

**Files:**
- `lib/typing/diagnostic.ml` — Update to use canonical error codes

**Changes:**
1. Map all error conditions to codes from Spec 47
2. Ensure all diagnostics include `error[EXXXX]:` format

### 5.2 Source Excerpts in Errors (Spec 45)

**Files:**
- `lib/util/ansi.ml` — CREATE (TTY detection, colors)
- `lib/typing/source_excerpt.ml` — CREATE (read source, render underlines)
- `lib/typing/diagnostic.ml` — Use excerpts in `human` format

**Changes:**
1. Read source file around error location
2. Render with line numbers, underlines, Elm-style prose
3. Show `.tart` signature provenance when relevant
4. Colors for TTY, plain text otherwise

**Verify:** Type errors show code context with underlines

### 5.3 File I/O Errors (Spec 37)

**Files:**
- `lib/errors/file_error.ml` — EXISTS, enhance
- `bin/main.ml` — Use structured file errors

**Changes:**
1. Levenshtein suggestions for file not found
2. "did you mean: foo.el" for missing `.el` extension
3. Directory vs file detection
4. Signature not found with search path listing

---

## Phase 6: Testing Infrastructure

### 6.1 Type Checker Acceptance Tests (Spec 25)

**Files:**
- `lib/test_harness/acceptance.ml` — EXISTS, enhance
- `test/fixtures/typing/` — Expand fixtures

**Changes:**
1. Create `.expected` files for pass/fail cases
2. Fixtures for each error category (type-mismatch, arity, unbound, etc.)
3. Version-specific fixtures
4. Wire into `dune test`

### 6.2 Typing Fixtures by Category (Spec 33)

**Structure:**
```
test/fixtures/typing/
├── core/           # Passing primitives
├── errors/
│   ├── type-mismatch/
│   ├── arity/
│   ├── unbound/
│   ├── occurs-check/
│   ├── kind/
│   └── exhaustiveness/
└── regression/     # Bug reproductions
```

---

## Phase 7: CLI & Tooling Polish

### 7.1 Cmdliner Migration (Spec 36)

**Files:**
- `bin/main.ml` — Rewrite with Cmdliner
- `bin/dune` — Add cmdliner dependency

**Benefits:**
- Auto-generated `--help`
- Typo suggestions (`--prot` → `--port`)
- Structured validation errors

### 7.2 Fast Feedback Script (Spec 31)

**Files:**
- `./tart` — CREATE (shell script at repo root)

**Content:**
```bash
#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
if [[ -n "${IN_NIX_SHELL:-}" ]]; then
  exec dune exec tart -- "$@"
else
  exec nix develop --command dune exec tart -- "$@"
fi
```

**Verify:** `./tart check test.el` works inside and outside nix shell

---

## Dependency Order

```
Phase 1.1 (Remove inference) ──┬──→ Phase 1.2 (Prelude)
                               │
                               └──→ Phase 2.1 (Type subtraction)
                                         │
Phase 1.3 (Version detection) ──────────┬┴──→ Phase 4.2 (Validate typings)
                                        │
Phase 3.1 (Sig sync) ←── Phase 3.2 (Dep graph)
                                        │
Phase 2.2 (Bounded quantifiers) ←───────┘
Phase 2.3 (No-shadowing) ←──────────────┘

Phase 4.1 (Verbose coverage) → Phase 4.2 (Validate typings)
Phase 4.3 (BUGS.md) ← Phase 4.2

Phase 5.* (Error quality) — parallel, after Phase 2
Phase 6.* (Testing) — parallel, after Phase 1
Phase 7.* (CLI polish) — after core functionality
```

---

## Acceptance Criteria

**Milestone 1: Foundation**
- [ ] Unbound type variables produce errors (no inference)
- [ ] Prelude types available without import
- [ ] Emacs version auto-detected

**Milestone 2: Core Types**
- [ ] Type subtraction works
- [ ] Bounded quantifiers enforced
- [ ] No shadowing of imports

**Milestone 3: LSP**
- [ ] Editing `.tart` updates `.el` diagnostics
- [ ] Dependency graph tracks invalidation

**Milestone 4: Typings**
- [ ] `data.tart`, `fns.tart`, `alloc.tart`, `eval.tart` validated
- [ ] 95%+ success rate on Emacs lisp/ subset
- [ ] BUGS.md documents gaps

**Final: E2E**
- [ ] Open `simple.el` from Emacs, get accurate hover types
- [ ] Open `subr.el` from Emacs, get accurate hover types
- [ ] Type errors in user code show helpful messages with source excerpts
