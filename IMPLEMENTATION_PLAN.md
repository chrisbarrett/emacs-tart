# Implementation Plan: Accurate Types via LSP for Emacs Core

**Goal:** Open any core lisp file that ships with Emacs and get accurate types via the LSP.

**Scope:** E2E completion for core Emacs primitives, not complete typing of all packages.

---

## Phase 1: Foundation Fixes (Critical Refactoring)

These tasks fix spec violations in the existing implementation.

### 1.1 Remove Inferred Type Quantifiers ✅

**Status:** COMPLETE (commit cdfc87c)

Removed automatic `[vars]` quantifier inference. Unbound type variables now produce errors as required by Specs 07 and 15.

### 1.2 Create Prelude (Spec 48) ✅

**Status:** COMPLETE (commits 927cfb3, b277070, 2e148ad)

Prelude types (`list`, `option`, `is`, `nonempty`, `any`, `bool`, `t`) are now implicitly available. Located at `typings/tart-prelude.tart`.

### 1.3 Emacs Version Detection (Spec 24) ✅

**Status:** COMPLETE

Version detection and fallback chain implemented:
- `lib/sig/emacs_version.ml`: detect(), parse_version(), version_fallback_candidates()
- `lib/sig/search_path.ml`: find_in_versioned_typings(), find_typings_dir()
- `bin/main.ml`: --emacs-version flag, detect_emacs_version() with verbose logging
- Fallback chain: 31.0.50 → 31.0 → 31 → latest

---

## Phase 2: Core Type System Completeness

### 2.1 Type Subtraction Operator (Spec 11 R10) ✅

**Status:** COMPLETE (commit 98ff9ac)

`(a - nil)` syntax implemented. The `is` and `nonempty` prelude types work correctly.

### 2.2 Bounded Quantifiers Validation (Spec 07 R4, Spec 48 R6) ✅

**Status:** COMPLETE (commit 6eb5818)

Bounded quantifier constraints are now enforced at type alias expansion time. The `option` type has a truthy bound that prevents `(option (option x))`.

### 2.3 No-Shadowing Rule (Spec 07 R17) ✅

**Status:** COMPLETE (commit 2e7afa2)

Names imported from prelude, open, or include cannot be redefined. Error message: "cannot redefine imported binding 'name'"

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

## Phase 4: Create Emacs Core Typings (Spec 24, Spec 32)

The previous Emacs typings were deleted to be rewritten using the new type system features (type subtraction, explicit quantification, prelude types).

### 4.1 Directory Structure Setup ✅

**Status:** COMPLETE

Created versioned typings directory structure.

### 4.2 Verbose Coverage Output (Spec 30) ✅

**Status:** COMPLETE

`./tart emacs-coverage -v` shows diagnostic info including version detection, typings loading, and coverage matching.

### 4.3 Create C-Core Typings (Spec 32) ✅

**Status:** COMPLETE

Created 16 c-core type signature files with 848 total signatures:

| File | Signatures | Key Functions |
|------|------------|---------------|
| data.tart | 120 | predicates, accessors, arithmetic |
| fns.tart | 104 | length, concat, mapcar, assoc |
| alloc.tart | 21 | cons, list, make-vector |
| eval.tart | 54 | funcall, apply, signal, catch |
| buffer.tart | 52 | current-buffer, set-buffer |
| editfns.tart | 80 | point, goto-char, insert |
| window.tart | 98 | selected-window, window-buffer |
| frame.tart | 65 | selected-frame, frame-parameters |
| fileio.tart | 54 | file I/O operations |
| search.tart | 17 | re-search-forward, match-string |
| process.tart | 64 | start-process, process-send-string |
| keyboard.tart | 37 | read-key-sequence, input |
| keymap.tart | 29 | define-key, lookup-key |
| minibuf.tart | 22 | read-string, completing-read |
| textprop.tart | 20 | get-text-property, put-text-property |
| print.tart | 11 | prin1, princ, message |

Coverage: 1006/4873 public symbols (20.6%)

The original implementation plan listed 16 files to create. All 16 have been created and validated. Write type signatures for Emacs C primitives from scratch, using the new type system features.

**Files to create (in priority order):**

| File | Source | Key Functions |
|------|--------|---------------|
| `data.tart` | data.c | `eq`, `null`, `+`, `-`, `car`, `cdr`, predicates, arithmetic |
| `fns.tart` | fns.c | `length`, `concat`, `mapcar`, `assoc`, utilities |
| `alloc.tart` | alloc.c | `cons`, `list`, `make-vector`, allocation |
| `eval.tart` | eval.c | `funcall`, `apply`, `signal`, `catch`, control flow |
| `buffer.tart` | buffer.c | `current-buffer`, `set-buffer`, buffer ops |
| `editfns.tart` | editfns.c | `point`, `goto-char`, `insert`, editing |
| `window.tart` | window.c | `selected-window`, `window-buffer`, window ops |
| `frame.tart` | frame.c | `selected-frame`, `frame-parameters`, frame ops |
| `fileio.tart` | fileio.c | `find-file-noselect`, `write-region`, file I/O |
| `search.tart` | search.c | `re-search-forward`, `match-string`, search |
| `process.tart` | process.c | `start-process`, `process-send-string`, subprocs |
| `keyboard.tart` | keyboard.c | `read-key-sequence`, input |
| `keymap.tart` | keymap.c | `define-key`, `lookup-key`, keymaps |
| `minibuf.tart` | minibuf.c | `read-string`, `completing-read`, minibuffer |
| `textprop.tart` | textprop.c | `get-text-property`, `put-text-property`, props |
| `print.tart` | print.c | `prin1`, `princ`, `message`, output |

**Workflow per file (Spec 32 workflow):**
1. **Extract symbols** — Run `tart emacs-coverage -v` to list DEFUNs/DEFVARs from the C file
2. **Write signatures** — Create `.tart` file with types using prelude types
3. **Validate** — Run `./tart check` against Emacs `lisp/` directory
4. **Debug** — Use verbose coverage to confirm signatures loaded
5. **Iterate** — Fix type errors until 95%+ success rate
6. **Document gaps** — Log untypeable items to BUGS.md

**Type system features to leverage:**
- `list`, `option`, `nonempty`, `is` from prelude
- Type subtraction `(a - nil)` for refinements
- Explicit `[vars]` quantifiers for polymorphism
- Bounded quantifiers `[(a : truthy)]` where needed
- Union types `(a | b)` for sum types

### 4.4 BUGS.md Documentation (Spec 32 R5-R8)

Document untypeable or problematic symbols using these categories:

| Category | Description |
|----------|-------------|
| `type-system-gap` | Needs features tart doesn't have (dependent types, row polymorphism) |
| `untypeable` | Behavior can't be captured soundly (dynamic dispatch, eval-based) |
| `ergonomic` | Typeable but awkward (excessive annotations at call sites) |
| `version-specific` | Signature changed between Emacs versions |

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
```

### 4.5 Backfill Older Versions (DEFERRED)

Focus on 31.0 only for now. Backfilling 30.1/29.1 is future work after the 31.0 typings are stable and validated.

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

### 5.4 Surface Signature Parse/Validation Errors

**Problem:** When `.tart` files have parse or validation errors, they are silently skipped. Users see no indication why their signature isn't being loaded.

**Files:**
- `lib/sig/search_path.ml` — Change `parse_signature_file` to return errors
- `lib/sig/search_path.mli` — Update interface
- `lib/lsp/server.ml` — Publish diagnostics for `.tart` file errors
- `lib/typing/diagnostic.ml` — Add signature error formatting

**Current behavior (silent swallowing):**
```ocaml
(* search_path.ml:185-198 *)
let parse_signature_file path : signature option =
  try
    if parse_result.errors <> [] then None  (* Lexer errors lost *)
    ...
    | Error _ -> None  (* Parser errors lost *)
  with _ -> None  (* Exceptions lost *)

(* search_path.ml:255-257, 280-282, 322-324 *)
match Sig_loader.validate_signature sig_file with
| Ok () -> true
| Error _ -> false  (* Validation errors lost *)
```

**Changes:**
1. Change `parse_signature_file` return type to `(signature, sig_error list) result`
2. Create `sig_error` type that covers lexer, parser, and validation errors
3. Propagate errors through `load_module`, `load_module_with_sig`, `make_resolver`
4. In LSP: publish diagnostics for `.tart` files when errors occur
5. In CLI: print signature errors before type-checking proceeds
6. Include `.tart` file path and span in all error messages

**Verify:**
- Create `test.tart` with `(defun foo (a) -> a)` (unbound type var)
- Run `./tart check test.el` — should show error about unbound `a` in `test.tart`
- Open in editor with LSP — should see diagnostic in `test.tart`

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
Phase 1.1 (Remove inference) ✅ ──┬──→ Phase 1.2 (Prelude) ✅
                                  │
                                  └──→ Phase 2.1 (Type subtraction) ✅
                                            │
Phase 1.3 (Version detection) ─────────────┬┴──→ Phase 4.3 (Create typings)
                                           │
Phase 3.1 (Sig sync) ←── Phase 3.2 (Dep graph)
                                           │
Phase 2.2 (Bounded quantifiers) ←──────────┘
Phase 2.3 (No-shadowing) ←─────────────────┘

Phase 4.1 (Directory setup) → Phase 4.2 (Verbose coverage) → Phase 4.3 (Create typings)
Phase 4.4 (BUGS.md) ← Phase 4.3

Phase 5.* (Error quality) — parallel, after Phase 2
Phase 6.* (Testing) — parallel, after Phase 1
Phase 7.* (CLI polish) — after core functionality
```

---

## Acceptance Criteria

**Milestone 1: Foundation** ✅
- [x] Unbound type variables produce errors (no inference)
- [x] Prelude types available without import
- [x] Emacs version auto-detected

**Milestone 2: Core Types** ✅
- [x] Type subtraction works
- [x] Bounded quantifiers enforced
- [x] No shadowing of imports

**Milestone 3: LSP** ✅
- [x] Editing `.tart` updates `.el` diagnostics
- [x] Dependency graph tracks invalidation
- [x] Parse/validation errors in `.tart` files are surfaced (not silently skipped)

**Milestone 4: Typings**
- [x] All 16 c-core files created and validated
- [ ] 95%+ success rate on Emacs lisp/ subset (BUG-001 fixed, needs validation)
- [x] BUGS.md documents gaps

**Final: E2E**
- [ ] Open `simple.el` from Emacs, get accurate hover types
- [ ] Open `subr.el` from Emacs, get accurate hover types
- [ ] Type errors in user code show helpful messages with source excerpts
