# Spec 32: Emacs Core Typings Workflow

Systematic creation of complete, verified type signatures for Emacs C core.

**Deps:** [Spec 30](./30-verbose-coverage.md) (verbose coverage), [Spec 31](./31-fast-feedback.md) (fast feedback), [Spec 48](./48-prelude.md) (prelude
types available in signatures).

## Goal

Agents systematically create type signatures for all Emacs C primitives, with quality assurance through real Elisp validation and documented gaps for human review.

## Constraints

| Constraint | Detail |
|------------|--------|
| File-by-file | Work on one C source file at a time (e.g., data.c, fns.c) |
| Version-scoped | Typings target the user's current Emacs version |
| Verified | All signatures validated against Emacs's lisp/ directory |
| Documented gaps | Untypeable items logged to BUGS.md |
| Precise types | Avoid `any`; use unions of specific types (see [Spec 48](./48-prelude.md)) |

## Loading Sequence

| file                                       | defines                                    |
| ------------------------------------------ | ------------------------------------------ |
| `typings/tart-prelude.tart`                | userspace names for compiler intrinsics    |
| `typings/emacs/{version}/c-core/*.tart`    | data structures, functions, variables, etc |
| `typings/emacs/{version}/lisp-core/*.tart` | functions, variables, etc                  |

Primitives without special type-checker support → opaque types.

`(include)` de-duplicates identical definitions across emacs versions.

## Directory Structure

```
typings/emacs/
├── BUGS.md                     ; Package-level issues (cross-version, fundamental)
├── 31.0/
│   ├── BUGS.md                 ; Version-specific issues
│   └── c-core/
│       ├── data.tart           ; Types for data.c DEFUNs/DEFVARs
│       ├── fns.tart            ; Types for fns.c
│       ├── eval.tart           ; Types for eval.c
│       └── ...
```

C source files map 1:1 to `.tart` files per [Spec 24](./24-versioned-typings.md).

## Workflow

1. **Select C file** - Pick next uncovered file (e.g., data.c)
2. **Extract symbols** - Run `tart emacs-coverage -v` to list DEFUNs/DEFVARs
3. **Write signatures** - Create corresponding `.tart` file with types
4. **Validate** - Run `./tart check` against Emacs lisp/ directory
5. **Debug** - Use `tart emacs-coverage -v` to confirm signatures loaded
6. **Document gaps** - Log untypeable items to BUGS.md
7. **Iterate** - Fix type errors until acceptance criteria met

## Requirements

### R1: Create signature file for C source

**Given** a C source file (e.g., data.c) in Emacs src/
**When** an agent creates `typings/emacs/{version}/c-core/data.tart`
**Then** the file contains type signatures for all DEFUNs and DEFVARs from that C file

**Verify:** `tart emacs-coverage -v | grep "data.tart"` shows signature count

### R2: 100% symbol coverage per file

**Given** a completed `.tart` file for a C source
**When** coverage is measured
**Then** every public DEFUN and DEFVAR from that C file has a type signature

**Verify:** `tart emacs-coverage --verbose` shows 100% for that file's symbols

### R3: Validate against Emacs lisp/

**Given** signatures for a C source file
**When** `./tart check` is run against Emacs's lisp/ directory
**Then** 95%+ of usages type-check successfully

**Verify:** `./tart check /path/to/emacs/lisp/*.el` passes with <5% errors

### R4: Use verbose output for debugging

**Given** signatures that fail validation
**When** `tart emacs-coverage -v` is run
**Then** verbose output shows which signatures loaded and match status
**And** agents can identify and fix issues

**Verify:** Verbose output includes "Sample matches" with file locations

### R5: Package-level BUGS.md

**Given** an issue that affects multiple Emacs versions
**When** documenting the gap
**Then** add entry to `typings/emacs/BUGS.md`

**Verify:** `typings/emacs/BUGS.md` exists with documented cross-version issues

### R6: Version-specific BUGS.md

**Given** an issue specific to one Emacs version
**When** documenting the gap
**Then** add entry to `typings/emacs/{version}/BUGS.md`

**Verify:** `typings/emacs/31.0/BUGS.md` exists with version-specific issues

### R7: BUGS.md entry format

**Given** an untypeable or problematic symbol
**When** adding to BUGS.md
**Then** entry contains:
- Function/variable name
- Source location (file:line)
- Category: `type-system-gap` | `untypeable` | `ergonomic` | `version-specific`
- Description of why it's problematic
- Suggested type system feature (if applicable)

**Verify:** BUGS.md entries follow the specified format

### R8: Category definitions

**Given** a problematic symbol
**When** categorizing for BUGS.md
**Then** use these categories:
- **type-system-gap**: Needs features tart doesn't have (dependent types, row polymorphism)
- **untypeable**: Behavior can't be captured soundly (dynamic dispatch, eval-based)
- **ergonomic**: Typeable but awkward (excessive annotations at call sites)
- **version-specific**: Signature changed between Emacs versions

**Verify:** Each BUGS.md entry uses one of these four categories

### R9: Request tooling enhancements

**Given** verbose output is insufficient for debugging
**When** an agent identifies a tooling gap
**Then** they can document the enhancement needed for future implementation

**Verify:** Agents report tooling gaps with specific improvement suggestions

### R10: Avoid `any` in signatures

**Given** a function signature being written
**When** choosing types
**Then** avoid `any` except in input positions for predicates

**Guidelines:**
- **Input position OK:** `(null any) -> bool`, `(type-of any) -> symbol`
- **Output position WRONG:** `-> any` is almost always incorrect
- **Input union preferred:** `(length ((list any) | string | (vector any))) -> int`
  not `(length any) -> int`

If you cannot avoid `any` in output position, document in BUGS.md why the type
is genuinely dynamic (e.g., `symbol-value` returns whatever was stored).

**Verify:** `grep '-> any)' *.tart` returns only justified cases

## BUGS.md Format Example

```markdown
# Emacs Typings Issues

## type-system-gap

### `apply`
- **Location:** eval.c:2847
- **Issue:** Requires dependent types to express that return type depends on
  first argument's return type
- **Suggested feature:** First-class function type introspection

## untypeable

### `funcall`
- **Location:** eval.c:2789
- **Issue:** Accepts any function and any arguments; return type is dynamic
- **Resolution:** See [Spec 34](./34-funcall-apply-typing.md) for type-safe funcall/apply via dual namespaces

## ergonomic

### `mapcar`
- **Location:** fns.c:2567
- **Issue:** Works but requires explicit type annotation at most call sites
  due to polymorphism interaction with union types

## version-specific

### `json-parse-string`
- **Location:** json.c:523
- **Issue:** :object-type keyword added in Emacs 28, changes return type
```

## Acceptance Criteria

Per C source file:
1. `.tart` file exists with signatures for all public DEFUNs/DEFVARs
2. `./tart check` against Emacs lisp/ passes with 95%+ success rate
3. All untypeable items documented in appropriate BUGS.md

## Tasks

- [x] [R5,R6] Create BUGS.md structure
- [x] [R1-R4] Complete data.c → data.tart with validation
- [x] [R1-R4] Complete fns.c → fns.tart with validation
- [x] [R1-R4] Complete eval.c → eval.tart with validation
- [x] [R1-R4] Complete alloc.c → alloc.tart with validation
- [x] [R7,R8] Document all untypeable items per category
- [ ] Continue for remaining C source files per [Spec 24](./24-versioned-typings.md)

## Status

Complete for the four core C source files (data, fns, eval, alloc).

- BUGS.md structure exists at both package and version levels
- All `-> any` return types audited; improved where possible, documented
  in BUGS.md where genuinely untypeable
- 16 entries in package-level BUGS.md (3 type-system-gap, 13 untypeable)
- Remaining C source files (per [Spec 24](./24-versioned-typings.md)) are ongoing work beyond this spec
