# Spec 07: Signature File System

Parser and loader for `.eli` signature files that declare types for Elisp code.

**Dependencies:** Spec 04 parser (reuse S-exp infrastructure), Spec 06 type
representation.

## Goal

Implement the `.eli` file format that provides type signatures for modules,
enabling type checking at module boundaries.

## Constraints

- **Sibling files**: `foo.eli` provides signatures for `foo.el`
- **Declarative**: No executable code; pure type declarations
- **Extensible**: Support ADTs, type aliases, struct imports

## Output

```
tart/
├── lib/
│   └── sig/
│       ├── sig_ast.ml     ; Signature file AST
│       ├── sig_parser.ml  ; Parse .eli to AST
│       ├── sig_loader.ml  ; Load signatures into type env
│       └── stdlib.ml      ; Access bundled signatures
├── stdlib/
│   ├── builtins.eli       ; Emacs primitives
│   ├── cl-lib.eli         ; cl-lib signatures
│   └── seq.eli            ; seq.el signatures
└── test/
    └── sig/
        └── sig_test.ml
```

## Requirements

### R1: Signature file parsing

**Given** a `.eli` file
**When** parsed
**Then** it produces a signature AST containing:
- Module declaration
- Function signatures
- Variable declarations
- Type aliases
- ADT definitions
- Struct imports
- Require/typed imports

**Verify:** `dune test`; parse all files in `stdlib/`

### R2: Type syntax parsing

**Given** type expressions in signatures
**When** parsed
**Then** they produce the type representation from Spec 06:
- Base types: `Int`, `String`, `Nil`, `T`, `Truthy`, `Bool`, `Any`, `Never`
- Type variables: lowercase identifiers
- Function types: `params -> return` (infix arrow)
- Applications: `(List Int)`, `(Option String)`
- Unions: `(Or Int String)`

Single params need no parens; multiple params require grouping. Quantifiers `[vars]`
go at the start of arrow types, or after the name in `defun`.

**Verify:** Type parsing tests cover all grammar productions

### R3: Function signature declarations

**Given** `(defun foo (Int String) -> Bool)`
**When** loaded
**Then** `foo` is bound as a directly callable function in the module's type environment

**Verify:** Signature loaded; type checker uses it for calls to `foo`

### R4: Variable declarations

**Given** `(defvar my-var String)`
**When** loaded
**Then** `my-var` is bound as a String in the type environment

**Verify:** References to `my-var` have type String

### R5: Type alias definitions

**Given** `(deftype IntList (List Int))`
**When** loaded
**Then** `IntList` can be used in signatures and expands to `(List Int)`

**Verify:** `(defun foo IntList -> Int)` equivalent to `(defun foo (List Int) -> Int)`

### R6: ADT definitions

**Given** an ADT definition:
```
(data Result (a e)
  (Ok a)
  (Err e))
```
**When** loaded
**Then** it generates:
- Type constructor `Result` with arity 2
- Constructor functions `Ok : [a e] a -> (Result a e)`
- Constructor functions `Err : [a e] e -> (Result a e)`
- Predicates `result-ok-p`, `result-err-p`
- Accessors `result-ok-value`, `result-err-value`

**Verify:** ADT constructors and accessors type-check correctly

### R7: Struct imports

**Given** `(import-struct person)` where `person` is defined via `cl-defstruct`
**When** loaded
**Then** it generates:
- Type `person`
- Constructor `make-person`
- Predicate `person-p`
- Accessors for each slot

**Verify:** Struct accessor calls type-check based on slot types

### R8: Require/typed imports

**Given** a require/typed declaration:
```
(require/typed seq
  (defun seq-map [a b] ((a -> b) (List a)) -> (List b)))
```
**When** loaded
**Then** `seq-map` is available with the declared type
**And** calls are checked against this signature (trust boundary)

**Verify:** Call to `seq-map` with wrong types produces error

### R9: Module discovery

**Given** a `.el` file being type-checked
**When** it requires another module
**Then** the loader searches for `.eli` files:
1. Sibling `module.eli` for `module.el`
2. `stdlib/module.eli` for built-ins

**Verify:** `(require 'cl-lib)` loads `cl-lib.eli` signatures

### R10: Bundled stdlib signatures

**Given** tart is installed
**When** builtins are referenced
**Then** types are loaded from bundled `builtins.eli`

Minimum coverage:
- Arithmetic: `+`, `-`, `*`, `/`, comparison ops
- Lists: `car`, `cdr`, `cons`, `list`, `nth`, `length`, `mapcar`
- Strings: `concat`, `substring`, `upcase`, `downcase`
- Predicates: `stringp`, `integerp`, `listp`, `null`
- Error: `error`, `signal` (return `Never`)

**Verify:** Built-in calls type-check; coverage test for all listed functions

## Tasks

- [ ] [R1] Define signature file AST
- [ ] [R2] Implement type syntax parser
- [ ] [R3] Handle function signatures
- [ ] [R4] Handle variable declarations
- [ ] [R5] Handle type aliases
- [ ] [R6] Handle ADT definitions with generated constructors
- [ ] [R7] Handle struct imports
- [ ] [R8] Handle require/typed
- [ ] [R9] Implement module discovery
- [ ] [R10] Write bundled stdlib signatures

Run review agent after `builtins.eli` covers basic list/string functions before
proceeding to Spec 08.
