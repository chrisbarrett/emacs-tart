# Spec 01: Research Phase

Literature review for elisp-refined, a static type analysis system for Emacs
Lisp based on System F with Hindley-Milner inference.

## Goal

Produce agent-consumable summaries that preserve algorithms, inference rules, and
implementation details. Agents should be able to make informed design decisions
without fetching external sources.

## Scope

**In scope:** System F and variants, HM inference algorithms, Typed Racket,
macro typing approaches, typed layers over dynamic languages (Flow, Hack), LSP
patterns.

**Out of scope:** Dependent types, linear types, refinement types, effects
systems.

## Output

All outputs go in `research/`:

- `research/<topic>.md` — Paper/implementation summaries
- `research/tapl/` — Algorithm quick-references
- `research/synthesis.md` — Cross-cutting analysis and recommendations

## Requirements

### R1: System F foundations

**Given** the research directory exists
**When** foundational System F literature is surveyed
**Then** `research/` contains summaries covering: Girard/Reynolds origins,
Algorithm W and variants, let-polymorphism, value restriction

**Verify:** `ls research/*.md | wc -l` >= 5; `grep -l "System F" research/*.md`
returns results

### R2: Typed Racket deep dive

**Given** R1 foundations are in place
**When** Typed Racket literature and implementation are studied
**Then** `research/` documents: type/macro interaction, occurrence typing,
untyped code boundaries

**Verify:** `grep -l "Typed Racket" research/*.md` returns results; `grep
"occurrence typing" research/synthesis.md`

### R3: Macro typing assessment

**Given** Typed Racket research is complete
**When** macro typing literature is surveyed
**Then** `research/synthesis.md` contains a dedicated section assessing
feasibility of typing macros, documenting the gap if literature is sparse

**Verify:** `grep -A5 "## Macro" research/synthesis.md` shows feasibility
assessment

### R4: Implementation prior art

**Given** literature review is substantially complete
**When** existing implementations are studied (Typed Racket, OCaml, Flow)
**Then** `research/` documents architecture patterns, LSP integration, error
message techniques

**Verify:** `grep -l "LSP\|architecture" research/*.md` returns results

### R5: TAPL algorithm references

**Given** research directory exists
**When** TAPL algorithms are summarised
**Then** `research/tapl/` contains: Algorithm W pseudocode, System F type rules,
unification algorithm, let-polymorphism rules

**Verify:** `ls research/tapl/*.md | wc -l` >= 4

## Tasks

- [ ] [R1] Survey System F papers and summarise key algorithms
- [ ] [R2] Document Typed Racket's design, focusing on macro and boundary handling
- [ ] [R3] Assess macro typing feasibility based on available literature
- [ ] [R4] Study implementation architectures and LSP patterns
- [ ] [R5] Produce TAPL algorithm quick-references
- [ ] Produce `synthesis.md` with recommendations for design phase

Run review agent after synthesis.md is complete before proceeding to Spec 03.
