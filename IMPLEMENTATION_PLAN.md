# Implementation Plan: Accurate Types via LSP for Emacs Core

**Goal:** Open any core lisp file that ships with Emacs and get accurate types via the LSP.

**Scope:** E2E completion for core Emacs primitives, not complete typing of all packages.

---

## Phase 2: Core Type System Completeness

### 2.4 Union Types and Row Polymorphism (Spec 11)

**Status:** NOT STARTED (except R10 type subtraction, complete)

**Goal:** Type-safe structural typing for Elisp's idiomatic alist/plist patterns.
Elisp represents structured data as alists `((name . "Alice") (age . 30))` and
plists `(:name "Alice" :age 30)`. Row polymorphism types these patterns directly.

**Requirements from Spec 11:**

| Req | Description | Status |
|-----|-------------|--------|
| R1 | Union type representation and subtyping | Not started |
| R2 | Type narrowing in pcase branches | Not started |
| R3 | Exhaustiveness checking for unions | Not started |
| R4 | Row-polymorphic alist types `(alist {name string & r})` | Not started |
| R5 | Row-polymorphic plist types `(plist {:name string & r})` | Not started |
| R6 | Map pattern exhaustiveness | Not started |
| R7 | Closed unions vs open row types | Not started |
| R8 | Row type inference from field access | Not started |
| R9 | Literal types with deferred widening | Not started |
| R10 | Type subtraction operator | ✅ Complete |

**Type forms to support:**

```lisp
;; Homogeneous (all keys/values share types)
(alist k v)                      ; e.g., (alist symbol int)
(plist k v)                      ; e.g., (plist keyword string)

;; Record-style (specific fields via row types)
(alist {name string age int})    ; closed—exactly these fields
(alist {name string & r})        ; open—at least these fields
(plist {:name string :age int})  ; closed plist
(plist {:name string & r})       ; open plist

;; Generic map supertype
(map {name string & r})          ; any map-like with these fields
```

**Key implementation points:**

1. **Row variables** (`& r`) enable structural subtyping—a record with extra
   fields can be passed where fewer fields are required

2. **Literal vs variable key inference:**
   - `(alist-get 'name x)` → infer `x : (alist {name a & r})`
   - `(alist-get key x)` → infer `x : (alist k v)`, `key : k`

3. **Map supertype** allows functions to accept alist, plist, or hash-table:
   ```lisp
   (defun get-name (person)
     (declare (tart ((map {name string & r}) -> string)))
     (map-elt person 'name))
   ```

**Files likely affected:**
- `lib/core/types.ml` — Add row type representation
- `lib/typing/unify.ml` — Row unification
- `lib/typing/infer.ml` — Infer row types from field access
- `lib/sig/sig_loader.ml` — Parse row type syntax
- `lib/sig/sig_parser.ml` — Row type grammar

**Verify:** See Spec 11 for detailed test scenarios.

### 2.5 Funcall and Apply Typing (Spec 34)

**Status:** NOT STARTED

**Goal:** Type `funcall` and `apply` accurately instead of returning `any`.
These are fundamental Lisp primitives that currently lose all type information.

**Key insight:** `funcall`/`apply` must NOT have signatures—they need special
type-checker handling that extracts the return type from the function argument.

**Requirements from Spec 34:**

| Req | Description | Status |
|-----|-------------|--------|
| R1 | Dual namespace (function vs variable bindings) | Not started |
| R2 | `#'name` and `'name` lookup in function namespace | Not started |
| R3 | Variable references use variable namespace | Not started |
| R4 | Funcall type checking (infer f, verify args, return result type) | Not started |
| R5 | Style warning for `'name` vs `#'name` in funcall | Not started |
| R6 | Funcall-specific error messages | Not started |
| R7 | Apply with `&rest` functions | Not started |
| R8 | Apply with fixed-arity functions (tuple argument) | Not started |
| R9 | Tuple `<:` list subtyping | Not started |
| R10 | Context-sensitive tuple inference for list literals | Not started |
| R11 | Union function types (dynamic dispatch) | Not started |
| R12 | Occurrence typing for type predicates | Not started |
| R13 | Thread narrowed types through funcall | Not started |
| R14 | Never widen to top type (error instead) | Not started |

**Example of correct typing:**

```elisp
(defun add1 (x) (+ x 1))    ; add1 : (-> (Int) Int)
(funcall #'add1 5)          ; result: Int (not any!)

(apply #'+ '(1 2 3))        ; + : (-> (&rest Int) Int), result: Int
(apply #'cons '(1 (2 3)))   ; infer tuple (Tuple Int (List Int)), result: (List Int)
```

**Anti-pattern (current state):**
```elisp
;; WRONG - this is what we have now, loses type info
(defun funcall (any &rest any) -> any)
```

**Files likely affected:**
- `lib/typing/type_env.ml` — Add separate function namespace
- `lib/typing/infer.ml` — Special-case funcall/apply, occurrence typing
- `lib/typing/unify.ml` — Tuple <: list subtyping
- `lib/typing/builtin_types.ml` — Remove funcall/apply signatures entirely

**Note:** Spec 34 lists HKT (Spec 17) as a dependency, but most requirements
don't need it. HKT is only needed for the most polymorphic cases. Basic
funcall/apply typing can proceed without HKT.

**Verify:** See Spec 34 for detailed test scenarios.

---

## Phase 4: Emacs Core Typings Validation

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

---

## Dependency Order

```
Phase 2.4 (Row polymorphism) ─────────────────────────────┐
Phase 2.5 (Funcall/apply) ────────────────────────────────┤
                                                          ├──→ Phase 8 (Remove builtins)
Phase 5.* (Error quality) ────────────────────────────────┤
Phase 6.* (Testing) ──────────────────────────────────────┘
Phase 7.* (CLI polish)
```

**Notes:**
- Phase 2.4 enables row-polymorphic alist/plist types (Spec 11)—improves c-core typing accuracy
- Phase 2.5 enables accurate typing of funcall/apply (currently return any)
- Phase 8 can proceed in parallel with type system improvements

---

## Remaining Acceptance Criteria

**Milestone 4: Typings (partial)**
- [ ] 95%+ success rate on Emacs lisp/ subset

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

- [ ] `builtin_types.ml` is empty (returns `Type_env.empty`)
- [ ] `bool-vector` and `char-table` opaque types added to `tart-prelude.tart`
- [ ] `length` uses precise union type
- [ ] All `-> any` return types audited and justified or fixed
- [ ] All tests pass
- [ ] `./tart check` works correctly on test fixtures
- [ ] No regression in type errors for real Elisp files
