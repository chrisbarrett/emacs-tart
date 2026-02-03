# Implementation Plan: Accurate Types via LSP for Emacs Core

**Goal:** Open any core lisp file that ships with Emacs and get accurate types via the LSP.

**Scope:** E2E completion for core Emacs primitives, not complete typing of all packages.

---

## Phase 1: Foundation Fixes (Critical Refactoring)

These tasks fix spec violations in the existing implementation.

### 1.1 Remove Inferred Type Quantifiers ✅

**Status:** COMPLETE (commit cdfc87c)

Removed automatic `[vars]` quantifier inference. Unbound type variables now produce errors as required by Specs 07 and 15.

### 1.2 Create Prelude (Spec 48) ⚠️ PARTIAL

**Status:** PARTIAL - OCaml bootstrap exists, but proper .tart loading not implemented

Prelude types (`list`, `option`, `is`, `nonempty`, `any`, `bool`, `t`) are available via
hardcoded OCaml in `lib/sig/prelude.ml`. However, `typings/tart-prelude.tart` is NOT
actually loaded—the OCaml code duplicates its definitions for bootstrapping.

**Remaining work:** See Phase 1.4 for proper prelude architecture.

### 1.4 Prelude Architecture Refactor (HIGH PRIORITY)

**Status:** NOT STARTED

**Problem:** The prelude is currently implemented as hardcoded OCaml (`lib/sig/prelude.ml`)
that duplicates what should be in `typings/tart-prelude.tart`. The .tart file exists but
isn't loaded. This conflates intrinsics (built into the type checker) with user-level
type definitions (which should be in .tart files).

**Goal:** Clean separation between intrinsics and user-level types:
1. **Intrinsics** — Primitive types built into the type checker, with distinctive names
   that can't occur in real elisp (e.g., `%tart-intrinsic%Int`, `%tart-intrinsic%List`)
2. **tart-prelude.tart** — Actually loaded by the system, bridges intrinsic names to
   user-friendly names
3. **Everything else** — Defined in .tart files on top of the prelude

**Intrinsic naming convention:** Use `%tart-intrinsic%` prefix for intrinsics:
- `%tart-intrinsic%Int`, `%tart-intrinsic%Float`, `%tart-intrinsic%Num` — numeric types
- `%tart-intrinsic%String`, `%tart-intrinsic%Symbol`, `%tart-intrinsic%Keyword` — atom types
- `%tart-intrinsic%Nil`, `%tart-intrinsic%T`, `%tart-intrinsic%Truthy`, `%tart-intrinsic%Never` — special types
- `%tart-intrinsic%List`, `%tart-intrinsic%Vector`, `%tart-intrinsic%Pair`, `%tart-intrinsic%HashTable` — parameterized containers

**tart-prelude.tart becomes:**
```lisp
;; Bridge intrinsics to user-level names
(type int %tart-intrinsic%Int)
(type float %tart-intrinsic%Float)
(type num %tart-intrinsic%Num)
(type string %tart-intrinsic%String)
(type symbol %tart-intrinsic%Symbol)
(type keyword %tart-intrinsic%Keyword)
(type nil %tart-intrinsic%Nil)
(type truthy %tart-intrinsic%Truthy)
(type never %tart-intrinsic%Never)

;; Parameterized intrinsics
(type list [a] (%tart-intrinsic%List a))
(type vector [a] (%tart-intrinsic%Vector a))
(type pair [a b] (%tart-intrinsic%Pair a b))
(type hash-table [k v] (%tart-intrinsic%HashTable k v))

;; Derived types (built on intrinsics)
(type t 't)
(type any (truthy | nil))
(type bool (t | nil))
(type option [(a : truthy)] (a | nil))
(type is [a] (a - nil))
(type nonempty [a] (is (list a)))

;; Opaque types (no intrinsic backing)
(type bool-vector)
(type char-table)
```

**Changes required:**

1. **lib/core/types.ml** — Rename `Prim` constants to use `%tart-intrinsic%` prefix:
   - `int` → `TCon "%tart-intrinsic%Int"` (or keep internal names, just change how they're referenced)

2. **lib/sig/sig_loader.ml** — Remove `sig_name_to_prim` hardcoded mappings. Instead:
   - `%tart-intrinsic%Foo` in .tart → `TCon "%tart-intrinsic%Foo"` (passed through as-is)
   - Unknown names → look up in prelude/aliases/opaques

3. **lib/sig/prelude.ml** — Replace hardcoded aliases with code that:
   - Locates `typings/tart-prelude.tart`
   - Parses and loads it as the prelude context
   - Caches the result

4. **lib/sig/search_path.ml** — Ensure prelude is loaded before other .tart files

5. **typings/tart-prelude.tart** — Update with intrinsic bridges and opaque types

**Benefits:**
- Clear separation of concerns (intrinsics vs user types)
- Prelude is self-documenting .tart, not hidden OCaml
- Adding new types (like `bool-vector`) is just editing the .tart file
- Intrinsic prefix makes it obvious what's built-in vs user-defined

**Verify:**
- `./tart check` works with prelude loaded from .tart file
- `bool-vector` and `char-table` available after adding to prelude
- Error messages show user-friendly names, not `%tart-intrinsic%` prefixed intrinsics

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
Phase 1.1 (Remove inference) ✅ ──┬──→ Phase 1.2 (Prelude bootstrap) ⚠️
                                  │              │
                                  │              └──→ Phase 1.4 (Prelude architecture) ★ HIGH PRIORITY
                                  │                          │
                                  └──→ Phase 2.1 (Type subtraction) ✅
                                            │               │
Phase 1.3 (Version detection) ─────────────┬┴───────────────┴──→ Phase 4.3 (Create typings)
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
Phase 8.* (Remove builtins) — after Phase 1.4
```

**Note:** Phase 1.4 is high priority because it enables adding new types (like `bool-vector`,
`char-table`) by editing .tart files instead of OCaml code. Phase 8 depends on 1.4 being
complete.

---

## Acceptance Criteria

**Milestone 1: Foundation** ⚠️
- [x] Unbound type variables produce errors (no inference)
- [x] Prelude types available without import (via OCaml bootstrap)
- [ ] Prelude loaded from .tart file with intrinsic bridging (Phase 1.4)
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

---

## Phase 8: Remove Redundant Built-in Types

### Problem

`lib/typing/builtin_types.ml` contains 60+ hardcoded function signatures that
duplicate and conflict with the c-core `.tart` typings in `typings/emacs/31.0/c-core/`.

**Current loading order:**
1. `Builtin_types.initial_env()` creates base environment with hardcoded types
2. `Search_path.load_c_core` extends that environment with `.tart` signatures

When a function exists in both, the c-core definition wins (loaded second), but
the existence of duplicate hardcoded types:
- Causes confusion about source of truth
- Makes DESIGN.md documentation inconsistent with c-core
- Bloats the codebase with unmaintained duplicates

**Example conflict:**
- `builtin_types.ml`: `length : (List a) -> Int` (too narrow—rejects strings)
- `fns.tart`: `length : any -> int` (too permissive—accepts anything)

The correct signature is a union of all sequence types that `length` accepts:
```elisp
(defun length (((list any) | string | (vector any) | bool-vector | char-table)) -> int)
```

**Principle:** Every use of `any` must be treated with absolute suspicion. Even
`funcall`/`apply` should NOT return `any`—they should extract the return type
from their function argument. The only legitimate use of `any` is in input
positions for truly polymorphic predicates like `(null any) -> bool`.

For functions like `length`, use precise union types of the accepted inputs.

### Goal

Remove **all** function typings from `builtin_types.ml`:

1. **`funcall`/`apply`** — These need special type-checker handling per Spec 34,
   not signatures. The type checker must extract the return type from the
   function argument, not return `any` or a polymorphic variable.
2. **`function`** — Already handled as a special form in the type checker.
3. **Everything else** — Comes from c-core `.tart` files.

`builtin_types.ml` should become empty or be deleted entirely.

### 8.1 Verify C-Core Coverage

Confirm all functions being removed have corresponding definitions:

| builtin_types.ml | c-core file | Notes |
|------------------|-------------|-------|
| car, cdr, cons | data.tart | ✓ polymorphic |
| list | alloc.tart | ✓ polymorphic |
| length, nth, nthcdr | fns.tart | ⚠️ `length` needs union type fix |
| append, reverse, last | fns.tart | ✓ polymorphic |
| mapcar, member, memq | fns.tart | ✓ polymorphic |
| assoc, assq | fns.tart | ✓ polymorphic |
| +, -, *, /, mod, % | data.tart | ✓ uses `num` type |
| abs, max, min, 1+, 1- | data.tart | ✓ uses `num` type |
| logand, logior, logxor, lognot | data.tart | ✓ |
| <, >, <=, >=, = | data.tart | ✓ comparison |
| null, atom, listp, consp | data.tart | ✓ predicates |
| symbolp, stringp, numberp, integerp, floatp | data.tart | ✓ predicates |
| vectorp, functionp | data.tart | ✓ predicates |
| eq, equal, not | data.tart, fns.tart | ✓ |
| concat, substring, string-length | fns.tart | ✓ |
| upcase, downcase | editfns.tart | Need to add if missing |
| string-to-list | fns.tart | Need to add if missing |
| format | editfns.tart | ✓ |
| vector, aref, aset | data.tart | ✓ polymorphic |
| symbol-name | data.tart | ✓ |
| number-to-string, string-to-number | data.tart | ✓ |
| run-hooks, run-hook-with-args | eval.tart | ✓ |
| commandp, macroexpand, backtrace-frames | eval.tart | ✓ |

### 8.2 Delete or empty builtin_types.ml

**Files:**
- `lib/typing/builtin_types.ml` — Delete or make empty
- `lib/typing/builtin_types.mli` — Update interface

**Option A: Delete entirely**

Remove `builtin_types.ml` and update all callers of `initial_env()` to use
`Type_env.empty` directly.

**Option B: Keep as empty placeholder**

```ocaml
(** Built-in type signatures.

    This module is intentionally empty. All Elisp function types are defined in
    .tart signature files under typings/emacs/{version}/c-core/.

    Special forms (funcall, apply, function, if, let, etc.) are handled directly
    by the type checker, not via signatures. See Spec 34 for funcall/apply. *)

module Env = Core.Type_env

let initial_env () : Env.t = Env.empty
```

**Note on funcall/apply:** These must NOT have signatures returning `any` or
unbound type variables. Per Spec 34, the type checker must:
1. Require the first argument to be a function type `(-> (T1 ... Tn) R)`
2. Check remaining arguments against `T1 ... Tn`
3. Return `R` as the result type

This requires special-case handling in the type checker, not a signature.

### 8.3 Add Missing Types and Signatures

**Add opaque types to prelude:**

Add `bool-vector` and `char-table` as opaque types in `typings/tart-prelude.tart`:
```lisp
;; Opaque types (no intrinsic backing needed)
(type bool-vector)
(type char-table)
```

These don't need to be intrinsics—they have no special type-checker behavior
(no subtyping rules, no special truthiness handling). As opaque types, they
produce `TCon` values that only unify with themselves.

**Prerequisite:** Phase 1.4 (Prelude Architecture Refactor) must be complete for
`tart-prelude.tart` to actually be loaded by the system.

**Fix `length` signature in `fns.tart`:**

Current (wrong):
```elisp
(defun length (any) -> int)
```

Correct:
```elisp
(defun length (((list any) | string | (vector any) | bool-vector | char-table)) -> int)
```

**Missing signatures:**

1. `upcase`, `downcase` — from `casefiddle.c`, need new file `casefiddle.tart`
2. `string-to-list` — from `fns.c`, add to `fns.tart`

**Create `typings/emacs/31.0/c-core/casefiddle.tart`:**
```elisp
;; Type signatures for Emacs casefiddle.c
;;
;; Source: emacs/src/casefiddle.c
;; Emacs version: 31.0
;;
;; Contains case conversion functions.

;; (upcase OBJ) - convert string or char to uppercase
(defun upcase ((string | int)) -> (string | int))

;; (downcase OBJ) - convert string or char to lowercase
(defun downcase ((string | int)) -> (string | int))

;; (capitalize OBJ) - capitalize string or char
(defun capitalize ((string | int)) -> (string | int))

;; (upcase-initials OBJ) - upcase initials in string
(defun upcase-initials ((string | int)) -> (string | int))

;; (upcase-region START END &optional REGION-NONCONTIGUOUS-P) - upcase region
(defun upcase-region (int int &optional any) -> nil)

;; (downcase-region START END &optional REGION-NONCONTIGUOUS-P) - downcase region
(defun downcase-region (int int &optional any) -> nil)

;; (capitalize-region START END &optional REGION-NONCONTIGUOUS-P) - capitalize region
(defun capitalize-region (int int &optional any) -> nil)

;; (upcase-word ARG) - upcase word(s) from point
(defun upcase-word (int) -> nil)

;; (downcase-word ARG) - downcase word(s) from point
(defun downcase-word (int) -> nil)

;; (capitalize-word ARG) - capitalize word(s) from point
(defun capitalize-word (int) -> nil)
```

**Add to `typings/emacs/31.0/c-core/fns.tart`:**
```elisp
;; (string-to-list STRING) - convert string to list of character codes
(defun string-to-list (string) -> (list int))
```

### 8.4 Update DESIGN.md

Update "Built-in Function Types" section to explain the new architecture:
- Intrinsics (`funcall`, `apply`, `function`) in `builtin_types.ml`
- Standard library in `typings/emacs/{version}/c-core/*.tart`

### 8.5 Run Tests

```bash
nix develop --command dune build 2>&1
nix develop --command dune test 2>&1
./tart check test/fixtures/**/*.el
```

### 8.6 Validate with Real Elisp

```bash
./tart check /path/to/emacs/lisp/simple.el
```

### 8.7 Audit `-> any` Return Types in C-Core

Many c-core signatures return `any` inappropriately. Audit and fix:

**Likely legitimate (truly dynamic):**
- `symbol-value` — stored value is dynamic
- `symbol-function` — could be any function type
- `eval` — result depends on runtime form

**Likely fixable:**
- `selected-frame` → should return `frame` type
- `frame-first-window`, `frame-root-window` → should return `window` type
- `car-safe`, `cdr-safe` → could use polymorphism with option
- Bool-vector operations → should use `bool-vector` type once added

Run `grep '-> any)' typings/emacs/31.0/c-core/*.tart` and review each case.

### Acceptance Criteria

**Prerequisite:** Phase 1.4 (Prelude Architecture Refactor) must be complete.

- [ ] `builtin_types.ml` is empty (returns `Type_env.empty`)
- [ ] `bool-vector` and `char-table` opaque types added to `tart-prelude.tart`
- [ ] `length` uses precise union type
- [ ] All `-> any` return types audited and justified or fixed
- [ ] All tests pass
- [ ] `./tart check` works correctly on test fixtures
- [ ] No regression in type errors for real Elisp files
