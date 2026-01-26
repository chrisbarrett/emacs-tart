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
- **Extensible**: Support type aliases, struct imports, bounded quantifiers

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
- Function signatures with explicit quantifiers
- Variable declarations
- Type declarations (aliases with definition, opaque without)
- Struct imports

The module name is derived from the filename: `foo.tart` provides types for `foo`.

**Verify:** `dune test`; parse all files in `stdlib/`

### R2: Type syntax parsing

**Given** type expressions in signatures
**When** parsed
**Then** they produce the type representation from Spec 06:
- Primitive types: `int`, `string`, `nil`, `t`, `truthy`, `never`
- Type variables: symbols bound by explicit quantifiers
- Function types: `params -> return` (infix arrow)
- Applications: `(list int)`, `(option string)`
- Unions: `(int | string)` with parentheses required

Single params need no parens; multiple params require grouping. Quantifiers `[vars]`
go at the start of arrow types for values, or after the name in `defun`.

**Verify:** Type parsing tests cover all grammar productions

### R3: Explicit quantification

**Given** type variables in signatures
**When** parsed
**Then** a symbol is treated as a type variable if and only if it appears in a `[...]` quantifier

```elisp
(defun identity [a] (a) -> a)            ; 'a' is a type variable
(defun bad (a) -> a)                     ; Error: unbound 'a'
```

**Verify:** Unbound type variables produce parse/load errors

### R4: Bounded quantifiers

**Given** a quantifier with bounds `[(a : bound)]`
**When** parsed and loaded
**Then** the type variable `a` is constrained to subtypes of `bound`

```elisp
(type option [(a : truthy)] (a | nil))
(defun unwrap [(a : truthy)] ((a | nil) a) -> a)
```

**Verify:** Bounded quantifiers parsed correctly; bounds checked at instantiation

### R5: Function signature declarations

**Given** `(defun foo (int string) -> bool)`
**When** loaded
**Then** `foo` is bound as a directly callable function in the module's type environment

**Verify:** Signature loaded; type checker uses it for calls to `foo`

### R6: Variable declarations

**Given** `(defvar my-var string)`
**When** loaded
**Then** `my-var` is bound as a string in the type environment

**Verify:** References to `my-var` have type string

### R7: Type alias definitions

**Given** `(type int-list (list int))`
**When** loaded
**Then** `int-list` can be used in signatures and expands to `(list int)`

**Verify:** `(defun foo int-list -> int)` equivalent to `(defun foo (list int) -> int)`

### R8: Parameterized type aliases

**Given** `(type result [a e] ((ok a) | (err e)))`
**When** loaded
**Then** `result` is a parameterized type alias that can be instantiated

**Verify:** `(result int string)` expands correctly

### R9: Opaque type definitions

**Given** a type declaration with no definition:
```elisp
(type buffer)
(type handle)
```
**When** loaded
**Then**:
- `buffer` is a distinct opaque type with no exposed structure
- `handle` is a separate distinct opaque type
- Values can only be created/consumed via functions declared in the same module

**Verify:** Opaque types are not unifiable with each other or other types

### R10: Opaque types with phantom parameters

**Given** `(type tagged [a])`
**When** loaded
**Then** `tagged` is an opaque type with a phantom type parameter

**Verify:** `(tagged int)` and `(tagged string)` are distinct types

### R11: Struct imports

**Given** `(import-struct person)` where `person` is defined via `cl-defstruct`
**When** loaded
**Then** it generates:
- Type `person`
- Constructor `make-person`
- Predicate `person-p`
- Accessors for each slot

**Verify:** Struct accessor calls type-check based on slot types

### R12: Open directive

**Given** `my-collection.tart`:
```elisp
(open 'seq)

(defun my-flatten [a] ((seq (seq a))) -> (list a))
```
**And** `seq.tart` defines `(type seq ...)`
**When** loaded
**Then** `seq` is available for use in type expressions
**And** `seq` is NOT re-exported from `my-collection`

**Verify:** Type expressions can reference opened types; opened types not in exports

### R13: Include directive

**Given** `my-extended-seq.tart`:
```elisp
(include 'seq)

(defun seq-partition [a] (int (seq a)) -> (list (list a)))
```
**And** `seq.tart` defines functions and types
**When** loaded
**Then** all declarations from `seq.tart` are part of `my-extended-seq`'s interface
**And** types from `seq` are available for use in signatures

**Verify:** Included declarations appear in module's exports

### R14: Union type parsing

**Given** union type syntax `(int | string | nil)`
**When** parsed
**Then** produces a union type with the specified alternatives
**And** parentheses are required around unions

**Verify:** `(a | b)` parses; bare `a | b` is a syntax error

### R15: Signature search path

**Given** a configuration with search path:
```elisp
(setq tart-type-path '("~/.config/emacs/tart/" "/nix/store/.../community-types/"))
```
**And** `~/.config/emacs/tart/seq.tart` contains:
```elisp
(defun seq-map [a b] (((a -> b)) (seq a)) -> (list b))
```
**When** a typed module calls `(require 'seq)` and uses `seq-map`
**Then** `seq-map` is available with the declared type from the search path

**Verify:** Call to `seq-map` with wrong types produces error

### R16: Module discovery order

**Given** a `.el` file being type-checked
**When** it requires another module via `(require 'module)`
**Then** the loader searches for `.tart` files in order:
1. Sibling `module.tart` next to `module.el` (project-local)
2. Each directory in `tart-type-path` (user/community types)
3. Bundled `stdlib/module.tart` (shipped with tart)

The first match wins, allowing project-local overrides.

**Verify:** `(require 'cl-lib)` loads `cl-lib.tart` signatures from stdlib

### R17: Bundled stdlib signatures

**Given** tart is installed
**When** builtins are referenced
**Then** types are loaded from bundled `builtins.tart`

Minimum coverage:
- Arithmetic: `+`, `-`, `*`, `/`, comparison ops
- Lists: `car`, `cdr`, `cons`, `list`, `nth`, `length`, `mapcar`
- Strings: `concat`, `substring`, `upcase`, `downcase`
- Predicates: `stringp`, `integerp`, `listp`, `null`
- Error: `error`, `signal` (return `never`)

**Verify:** Built-in calls type-check; coverage test for all listed functions

## Tasks

- [x] [R1] Define signature file AST
- [x] [R2] Implement type syntax parser
- [x] [R3] Enforce explicit quantification
- [x] [R4] Implement bounded quantifiers
- [x] [R5] Handle function signatures
- [x] [R6] Handle variable declarations
- [x] [R7] Handle type aliases (with definition)
- [x] [R8] Handle parameterized type aliases
- [x] [R9] Handle opaque types (no definition)
- [x] [R10] Handle opaque types with phantom parameters
- [ ] [R11] Handle struct imports
- [x] [R12] Handle open directive (import types)
- [x] [R13] Handle include directive (re-export declarations)
- [x] [R14] Parse union types with `|` syntax
- [ ] [R15] Implement signature search path
- [ ] [R16] Implement module discovery with search order
- [ ] [R17] Write bundled stdlib signatures

Run review agent after `builtins.tart` covers basic list/string functions before
proceeding to Spec 08.
