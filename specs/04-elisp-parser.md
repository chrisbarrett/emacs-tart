# Spec 04: Elisp Parser

OCaml parser for Emacs Lisp S-expressions producing a typed AST.

**Dependencies:** Spec 03 design decisions.

## Goal

Produce a resilient S-expression parser that handles real-world Elisp files,
preserving source locations for error reporting and IDE features.

## Constraints

- **OCaml**: Implementation in OCaml using dune build system
- **Resilient**: Continue after errors; produce partial AST
- **Source locations**: Every node carries span information
- **Reader macros**: Handle `'`, `` ` ``, `,`, `,@`, `#'`, `#(`

## Output

```
tart/
├── lib/
│   └── syntax/
│       ├── location.ml    ; Source location types
│       ├── sexp.ml        ; S-expression AST
│       ├── lexer.mll      ; Ocamllex lexer
│       ├── parser.mly     ; Menhir parser (or hand-written)
│       └── read.ml        ; Top-level reader API
└── test/
    └── syntax/
        └── parser_test.ml ; Parser tests
```

## Requirements

### R1: Lexer tokenizes Elisp atoms

**Given** an Elisp source string
**When** the lexer processes it
**Then** it produces tokens for: integers, floats, strings, symbols, keywords,
parentheses, quote forms, and comments

**Verify:** `dune test` passes; lexer handles `test/fixtures/atoms.el`

### R2: Parser produces S-expression AST

**Given** a token stream from the lexer
**When** the parser processes it
**Then** it produces an AST with these node types:
- `Sexp.Int of int * loc`
- `Sexp.Float of float * loc`
- `Sexp.String of string * loc`
- `Sexp.Symbol of string * loc`
- `Sexp.Keyword of string * loc`
- `Sexp.List of sexp list * loc`
- `Sexp.Vector of sexp list * loc`
- `Sexp.Quote of sexp * loc` (and Backquote, Unquote, UnquoteSplice)

**Verify:** `dune test` passes; round-trip test: parse then pretty-print equals
normalized input

### R3: Source locations are preserved

**Given** any parsed S-expression node
**When** its location is accessed
**Then** it contains: file path, start line/column, end line/column

**Verify:** Location tests verify spans match expected ranges for sample inputs

### R4: String escape sequences

**Given** Elisp strings with escapes: `\n`, `\t`, `\\`, `\"`, `\xNN`, `\uNNNN`
**When** parsed
**Then** the string value contains the unescaped characters

**Verify:** `dune test` with escape sequence corpus

### R5: Reader macro desugaring

**Given** reader macros in source: `'x`, `` `x``, `,x`, `,@x`, `#'f`, `#(vec)`
**When** parsed
**Then** they desugar to:
- `'x` → `(quote x)`
- `` `x`` → `(backquote x)`
- `,x` → `(unquote x)`
- `,@x` → `(unquote-splicing x)`
- `#'f` → `(function f)`
- `#(a b)` → vector node

**Verify:** Desugaring tests for each reader macro form

### R6: Comment handling

**Given** source with `;` line comments and `#|...|#` block comments
**When** parsed
**Then** comments are skipped (or optionally preserved for doc extraction)

**Verify:** Parser produces same AST with and without comments for equivalent code

### R7: Error recovery

**Given** malformed input like `(foo (bar)` (missing close paren)
**When** parsed
**Then** parser produces partial AST with error node marking the problem
**And** parsing continues to extract remaining valid forms

**Verify:** Error recovery tests with intentionally malformed files

### R8: Character literals

**Given** Elisp character literals: `?a`, `?\n`, `?\x41`, `?\C-a`, `?\M-x`
**When** parsed
**Then** they produce integer values matching Emacs semantics

**Verify:** Character literal tests covering ASCII, escapes, control/meta modifiers

## Tasks

- [ ] [R1] Implement lexer with ocamllex
- [ ] [R2] Implement S-expression AST types
- [ ] [R2] Implement parser (Menhir or recursive descent)
- [ ] [R3] Add source location tracking throughout
- [ ] [R4] Handle string escape sequences
- [ ] [R5] Desugar reader macros
- [ ] [R6] Handle comments
- [ ] [R7] Add error recovery for resilient parsing
- [ ] [R8] Parse character literals
- [ ] Set up test fixtures with real Elisp files

Run review agent after parser handles `cl-lib.el` before proceeding to Spec 05.
