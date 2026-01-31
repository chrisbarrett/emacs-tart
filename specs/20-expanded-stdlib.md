# Spec 20: Expanded Stdlib Phase 2

Broaden stdlib coverage with additional commonly-used Emacs libraries.

**Dependencies:** Spec 07 (Signature Files)

## Goal

Extend the bundled stdlib with type signatures for more Emacs packages that
are frequently used in modern Emacs Lisp development, improving out-of-box
type checking coverage.

## Motivation

The current stdlib covers core builtins, cl-lib, seq, and utility libraries
(dash, s, f). However, many common Emacs Lisp patterns use additional
built-in libraries and popular packages:

- Hash table operations via ht.el
- Generic map functions via map.el
- Extended string/list utilities via subr-x
- Process management for async operations
- Regular expression utilities

Adding these signatures reduces the number of "unknown function" warnings
users encounter and enables type-safe use of these APIs.

## Constraints

- **No runtime dependencies**: Signatures only, no runtime code
- **Accurate types**: Match actual Emacs behavior precisely
- **Comprehensive coverage**: Cover main entry points of each library
- **Consistent style**: Follow existing stdlib signature conventions

## Output

```
tart/
└── stdlib/
    ├── ht.tart           ; ht.el hash table library
    ├── map.tart          ; map.el generic map operations
    ├── subr-x.tart       ; subr-x.el extra subroutines
    ├── process.tart      ; Process management functions
    └── rx.tart           ; rx.el regular expression builder
```

## Requirements

### R1: Hash table library (ht.el)

**Given** code using ht.el functions
**When** type checking
**Then** function signatures are available:

Key functions:
- `ht-create`, `ht-from-alist`, `ht-from-plist` - construction
- `ht-get`, `ht-get*` - access with nested key paths
- `ht-set!`, `ht-update!`, `ht-remove!` - mutation
- `ht-clear!`, `ht-copy` - management
- `ht-keys`, `ht-values`, `ht-items` - iteration
- `ht-map`, `ht-each` - traversal
- `ht-select`, `ht-reject` - filtering
- `ht-merge`, `ht-equal?`, `ht-empty?` - comparison

**Verify:** `dune test`; ht.tart parses and loads

### R2: Generic map operations (map.el)

**Given** code using map.el functions
**When** type checking
**Then** function signatures are available:

Key functions:
- `map-elt`, `map-put!`, `map-delete` - element operations
- `map-nested-elt`, `map-contains-key` - nested access
- `map-keys`, `map-values`, `map-pairs` - decomposition
- `map-length`, `map-empty-p` - inspection
- `map-apply`, `map-do`, `map-filter` - traversal
- `map-merge`, `map-into` - combination

**Verify:** `dune test`; map.tart parses and loads

### R3: Extended subroutines (subr-x.el)

**Given** code using subr-x functions
**When** type checking
**Then** function signatures are available:

Key functions:
- `string-trim`, `string-trim-left`, `string-trim-right`
- `string-blank-p`, `string-empty-p`, `string-join`
- `when-let`, `when-let*`, `if-let`, `if-let*`
- `thread-first`, `thread-last`
- `hash-table-keys`, `hash-table-values`
- `string-remove-prefix`, `string-remove-suffix`

**Verify:** `dune test`; subr-x.tart parses and loads

### R4: Process management

**Given** code using process functions
**When** type checking
**Then** function signatures are available:

Key functions:
- `start-process`, `start-process-shell-command` - async
- `call-process`, `call-process-shell-command` - sync
- `make-process` - modern API
- `process-send-string`, `process-send-eof`
- `process-status`, `process-exit-status`
- `process-buffer`, `process-name`
- `delete-process`, `signal-process`
- `set-process-filter`, `set-process-sentinel`
- `process-list`, `get-process`, `get-buffer-process`

**Verify:** `dune test`; process.tart parses and loads

### R5: Regular expression builder (rx.el)

**Given** code using rx.el macros
**When** type checking
**Then** the rx macro returns string type:

Note: rx is a macro system, so type-checking is limited. We provide:
- `rx-to-string` - function that returns string

**Verify:** `dune test`; rx.tart parses and loads

### R6: Additional cl-lib coverage

**Given** the existing cl-lib.tart
**When** expanded
**Then** additional commonly-used functions are covered:

Functions to add:
- `cl-destructuring-bind` patterns (macro, limited typing)
- `cl-incf`, `cl-decf` - modification macros
- `cl-flet`, `cl-labels`, `cl-macrolet` - local bindings
- `cl-loop` - basic forms (full loop typing is complex)
- `cl-struct-*` accessor patterns

**Verify:** `dune test`; expanded cl-lib.tart loads

## Non-Requirements

- Full generic type support for map.el (uses different container types)
- Complete rx macro type checking (macro expansion is complex)
- org-mode or magit (too large, need separate specs)

## Tasks

- [ ] [R1] Create stdlib/ht.tart with ht.el signatures
- [ ] [R2] Create stdlib/map.tart with map.el signatures
- [ ] [R3] Create stdlib/subr-x.tart with subr-x.el signatures
- [ ] [R4] Create stdlib/process.tart with process function signatures
- [ ] [R5] Create stdlib/rx.tart with rx-to-string signature
- [ ] [R6] Expand stdlib/cl-lib.tart with additional functions
- [ ] Add tests for each new stdlib file

## Design Notes

### Type Patterns

For hash tables (ht.el), use the existing `hash-table` type with type
parameters for key and value types where possible.

For map.el, since it operates on multiple container types (alist, plist,
hash-table), use the generic `map` supertype (see Spec 11) for polymorphic
signatures.

For process functions, use an opaque `process` type similar to how
`buffer` and `window` are handled.

### Testing Strategy

Each stdlib file should have corresponding tests in search_path_test.ml:
1. Parse test - verifies the file parses correctly
2. Load test - verifies signatures load into type environment
