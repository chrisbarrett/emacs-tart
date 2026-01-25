# Spec 02: Emacs Lisp Analysis

Analyse Emacs Lisp's typing challenges to inform type system design.

**Dependency:** Can run in parallel with Spec 01.

## Goal

Produce `research/elisp-analysis.md` documenting: language features with typing
implications, tractability assessments (tractable / hard / intractable), and
recommendations for v1 scope.

## Scope

**In scope:** Special forms, calling conventions, data structures, global state,
macro patterns, ecosystem conventions, existing type-like systems (cl-defstruct,
EIEIO, defcustom :type).

**Out of scope:** Implementation strategy, signature file format (Spec 03).

## Requirements

### R1: Core language forms

**Given** the Elisp reference manual is consulted
**When** special forms are catalogued
**Then** `research/elisp-analysis.md` documents typing implications for:
definition forms, binding forms, control flow, mutation, error handling, and
save/restore forms

**Verify:** `grep -c "###" research/elisp-analysis.md` >= 5 (has multiple
sections)

### R2: Calling conventions

**Given** core forms are documented
**When** function calling is analysed
**Then** `research/elisp-analysis.md` documents: funcall/apply, advice system,
hooks, interactive specs, autoloading — each with tractability rating

**Verify:** `grep -E "funcall|advice|hooks" research/elisp-analysis.md` returns
results

### R3: Data structures

**Given** calling conventions are documented
**When** data structures are analysed
**Then** `research/elisp-analysis.md` documents typing strategy for: lists,
vectors, hash-tables, alists, plists, symbols, cl-defstruct, EIEIO, buffers

**Verify:** `grep -E "cl-defstruct|EIEIO" research/elisp-analysis.md` returns
results

### R4: Global state and effects

**Given** data structures are documented
**When** global state is catalogued
**Then** `research/elisp-analysis.md` documents: point, match-data, current
buffer, dynamic variables — with feasibility assessment for tracking each

**Verify:** `grep -E "point|match-data|dynamic" research/elisp-analysis.md`
returns results

### R5: Macro patterns

**Given** language features are documented
**When** macro usage is surveyed
**Then** `research/elisp-analysis.md` categorises macro patterns by
tractability: wrapper macros, control flow, definition macros, code generation

**Verify:** `grep -E "tractable|intractable" research/elisp-analysis.md` returns
results

### R6: Ecosystem conventions

**Given** language analysis is substantially complete
**When** ecosystem conventions are documented
**Then** `research/elisp-analysis.md` covers: naming conventions, require/provide,
autoloads, common libraries

**Verify:** `grep -E "require|provide|autoload" research/elisp-analysis.md`
returns results

### R7: Existing type systems

**Given** ecosystem conventions are documented
**When** existing type-like systems are analysed
**Then** `research/elisp-analysis.md` assesses integration potential for:
defcustom :type, cl-defstruct slots, EIEIO slots, declare forms

**Verify:** `grep "defcustom" research/elisp-analysis.md` returns results

## Tasks

- [ ] [R1] Catalogue special forms with typing implications
- [ ] [R2] Analyse calling conventions and assess tractability
- [ ] [R3] Document data structures with typing strategies
- [ ] [R4] Catalogue global state with tracking feasibility
- [ ] [R5] Categorise macro patterns by tractability
- [ ] [R6] Document ecosystem conventions
- [ ] [R7] Assess existing type system integration
- [ ] Produce final `elisp-analysis.md` with v1 recommendations

Run review agent after elisp-analysis.md is complete before proceeding to Spec 03.
