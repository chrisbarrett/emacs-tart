# Spec 03: Type System Design

Synthesise Spec 01 research and Spec 02 analysis into a concrete, implementable
type system design.

**Dependencies:** Spec 01 and Spec 02 substantially complete.

## Goal

Produce `research/design-proposals.md` containing: type syntax grammar, inference
algorithm selection, signature file format, ADT design, module boundaries, error
strategy, LSP architecture, implementation language recommendation, trade-offs,
and open questions.

## Constraints

- **Sound**: No `any` escape; untyped code requires signatures
- **System F base**: Higher-rank polymorphism where needed
- **HM inference**: Top-level signatures only; local inference
- **Phantom ADTs**: Compile-time only; runtime is pure Elisp
- **Package opt-in**: `.eli` sibling triggers type checking
- **LSP delivery**: Eglot integration; hover types is priority

## Requirements

### R1: Type syntax

**Given** Spec 01 research on type system syntax is available
**When** type syntax is designed
**Then** `research/design-proposals.md` contains grammar for: function types,
forall, ADTs (sum/product), type aliases, type variables

**Verify:** `grep -E "grammar|syntax" research/design-proposals.md` returns
results; examples present

### R2: Inference algorithm

**Given** Spec 01 covers Algorithm W and variants
**When** inference algorithm is selected
**Then** `research/design-proposals.md` documents: algorithm choice with
rationale, let-generalisation strategy, higher-rank handling, rejected
alternatives

**Verify:** `grep -E "Algorithm W|inference" research/design-proposals.md`
returns results

### R3: Signature file format

**Given** type syntax is defined
**When** `.eli` format is specified
**Then** `research/design-proposals.md` documents: function signatures, ADT
definitions, imports, built-in type references, relationship to `.el` files

**Verify:** `grep "\.eli" research/design-proposals.md` returns results; example
files present

### R4: ADT system

**Given** signature format is defined
**When** ADT system is designed
**Then** `research/design-proposals.md` documents: declaration syntax, pcase
integration, phantom runtime semantics, constructor syntax

**Verify:** `grep -E "ADT|pcase" research/design-proposals.md` returns results

### R5: Module boundaries

**Given** ADT system is designed
**When** module system is specified
**Then** `research/design-proposals.md` documents: what triggers type checking,
require/provide interaction, untyped code handling, public/private inference

**Verify:** `grep -E "require|module|boundary" research/design-proposals.md`
returns results

### R6: Error reporting

**Given** Spec 01 covers error message techniques
**When** error strategy is designed
**Then** `research/design-proposals.md` contains: strategy description, example
error messages for common mistakes

**Verify:** `grep -E "error|Error:" research/design-proposals.md` returns
results

### R7: Implementation language

**Given** Spec 01 covers OCaml and Typed Racket implementations
**When** implementation language is selected
**Then** `research/design-proposals.md` contains: recommendation (OCaml or Typed
Racket), ecosystem analysis, LSP library availability, trade-off summary

**Verify:** `grep -E "OCaml|Racket" research/design-proposals.md` returns
results

### R8: LSP architecture

**Given** implementation language is selected
**When** LSP architecture is designed
**Then** `research/design-proposals.md` documents: parsing strategy, incremental
checking, caching, hover implementation

**Verify:** `grep -E "LSP|hover|incremental" research/design-proposals.md`
returns results

### R9: Trade-offs and risks

**Given** all design decisions are documented
**When** trade-off analysis is compiled
**Then** `research/design-proposals.md` contains: trade-off for each major
decision, risks, mitigation strategies

**Verify:** `grep -E "trade-off|risk" research/design-proposals.md` returns
results

### R10: Open questions

**Given** trade-offs are documented
**When** open questions are compiled
**Then** `research/design-proposals.md` lists: decisions needing prototyping,
unresolved dependencies, resolution strategies

**Verify:** `grep -E "open question|prototype" research/design-proposals.md`
returns results

## Tasks

- [ ] [R1] Design type syntax grammar with examples
- [ ] [R2] Select inference algorithm with rationale
- [ ] [R3] Specify `.eli` file format with examples
- [ ] [R4] Design ADT system with pcase integration
- [ ] [R5] Specify module boundary rules
- [ ] [R6] Design error reporting with example messages
- [ ] [R7] Recommend implementation language
- [ ] [R8] Design LSP architecture
- [ ] [R9] Compile trade-off analysis
- [ ] [R10] Document open questions with resolution strategies
- [ ] Produce final `design-proposals.md`

Run review agent after design-proposals.md is complete before proceeding to
implementation.
