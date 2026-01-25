# Spec 07: Signature File System

Parser and loader for `.tart` signature files that declare types for Elisp code.

**Dependencies:** Spec 04 parser (reuse S-exp infrastructure), Spec 06 type
representation.

## Goal

Implement the `.tart` file format that provides type signatures for modules,
enabling type checking at module boundaries.

## Constraints

- **Sibling files**: `foo.tart` provides signatures for `foo.el`
- **Declarative**: No executable code; pure type declarations
- **Extensible**: Support ADTs, type aliases, struct imports

## Output

```
tart/
├── lib/
│   └── sig/
│       ├── sig_ast.ml     ; Signature file AST
│       ├── sig_parser.ml  ; Parse .tart to AST
│       ├── sig_loader.ml  ; Load signatures into type env
│       └── stdlib.ml      ; Access bundled signatures
├── stdlib/
│   ├── builtins.tart       ; Emacs primitives
│   ├── cl-lib.tart         ; cl-lib signatures
│   └── seq.tart            ; seq.el signatures
└── test/
    └── sig/
        └── sig_test.ml
```

## Requirements

### R1: Signature file parsing

**Given** a `.tart` file
**When** parsed
**Then** it produces a signature AST containing:
- Open directives (import types for use in signatures)
- Include directives (inline and re-export declarations)
- Function signatures
- Variable declarations
- Type declarations (aliases with definition, opaque without)
- Struct imports

The module name is derived from the filename: `foo.tart` provides types for `foo`.

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

**Given** `(type IntList (List Int))`
**When** loaded
**Then** `IntList` can be used in signatures and expands to `(List Int)`

**Verify:** `(defun foo IntList -> Int)` equivalent to `(defun foo (List Int) -> Int)`

### R6: Opaque type definitions

**Given** a type declaration with no definition:
```elisp
(type Buffer)
(type Handle)
```
**When** loaded
**Then**:
- `Buffer` is a distinct opaque type with no exposed structure
- `Handle` is a separate distinct opaque type
- Values can only be created/consumed via functions declared in the same module

**Verify:** Opaque types are not unifiable with each other or other types

### R7: Struct imports

**Given** `(import-struct person)` where `person` is defined via `cl-defstruct`
**When** loaded
**Then** it generates:
- Type `person`
- Constructor `make-person`
- Predicate `person-p`
- Accessors for each slot

**Verify:** Struct accessor calls type-check based on slot types

### R8: Open directive (renumbered)

**Given** `my-collection.tart`:
```elisp
(open 'seq)

(defun my-flatten [a] (Seq (Seq a)) -> (List a))
```
**And** `seq.tart` defines `(type Seq ...)`
**When** loaded
**Then** `Seq` is available for use in type expressions
**And** `Seq` is NOT re-exported from `my-collection`

**Verify:** Type expressions can reference opened types; opened types not in exports

### R9: Include directive

**Given** `my-extended-seq.tart`:
```elisp
(include 'seq)

(defun seq-partition [a] (Int (Seq a)) -> (List (List a)))
```
**And** `seq.tart` defines functions and types
**When** loaded
**Then** all declarations from `seq.tart` are part of `my-extended-seq`'s interface
**And** types from `seq` are available for use in signatures

**Verify:** Included declarations appear in module's exports

### R10: Signature search path

**Given** a configuration with search path:
```elisp
(setq tart-type-path '("~/.config/emacs/tart/" "/nix/store/.../community-types/"))
```
**And** `~/.config/emacs/tart/seq.tart` contains:
```elisp
(defun seq-map [a b] ((a -> b) (Seq a)) -> (List b))
```
**When** a typed module calls `(require 'seq)` and uses `seq-map`
**Then** `seq-map` is available with the declared type from the search path

**Verify:** Call to `seq-map` with wrong types produces error

### R11: Module discovery order

**Given** a `.el` file being type-checked
**When** it requires another module via `(require 'module)`
**Then** the loader searches for `.tart` files in order:
1. Sibling `module.tart` next to `module.el` (project-local)
2. Each directory in `tart-type-path` (user/community types)
3. Bundled `stdlib/module.tart` (shipped with tart)

The first match wins, allowing project-local overrides.

**Verify:** `(require 'cl-lib)` loads `cl-lib.tart` signatures from stdlib

### R12: Bundled stdlib signatures

**Given** tart is installed
**When** builtins are referenced
**Then** types are loaded from bundled `builtins.tart`

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
- [ ] [R5] Handle type aliases (with definition)
- [ ] [R6] Handle opaque types (no definition)
- [ ] [R7] Handle struct imports
- [ ] [R8] Handle open directive (import types)
- [ ] [R9] Handle include directive (re-export declarations)
- [ ] [R10] Implement signature search path
- [ ] [R11] Implement module discovery with search order
- [ ] [R12] Write bundled stdlib signatures

Run review agent after `builtins.tart` covers basic list/string functions before
proceeding to Spec 08.
