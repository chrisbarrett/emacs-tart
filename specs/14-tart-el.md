# Spec 14: tart.el Runtime Library

Elisp library providing type annotation macros for inline use in `.el` files.

**Dependencies:** [Spec 07][] signature files (type syntax parsing).

## Links

### Deps
[Spec 07]: ./07-signature-files.md

### Blocks
[Spec 21]: ./21-e2e-test-harness.md

### Related
[Spec 10]: ./10-emacs-integration.md

## Goal

Provide `tart.el` with macros that have no runtime effect but are recognized by
the type checker for:
- Function type signatures via `declare`
- Type assertions on expressions
- Variable type declarations
- File-local type aliases

## Constraints

- **Zero runtime cost**: Macros expand to their form argument unchanged (or nil)
- **Consistent syntax**: Type expressions use same grammar as `.tart` files
- **Assert semantics**: Type annotations are checked, not trusted
- **Invariant containers**: Parameterized types are invariant for soundness
- **Minimal dependencies**: `tart.el` has no dependencies beyond Emacs core, so
  packages can `(require 'tart)` without pulling in development tooling

## Output

```
lisp/
└── tart.el              ; Runtime macros only (no dev tooling)
```

Note: Development tooling (REPL, eglot integration, minor mode) lives in
`tart-mode.el` (see [Spec 10][]).

## Requirements

### R1: Function type signatures

**Given** `(declare (tart TYPE))` inside a function definition
**When** type-checked
**Then** the checker uses TYPE as the function's signature
**And** verifies the body's inferred type matches the declared return type
**And** parameters are bound to their declared types in the body

```elisp
(defun my-add (x y)
  (declare (tart (int int) -> int))
  (+ x y))

(defun my-identity (x)
  (declare (tart [a] (a) -> a))
  x)

;; Works with cl-defun, defmacro, etc.
(cl-defun my-lookup (key table &key default)
  (declare (tart [k v] (k (hash-table k v) &key :default v) -> v))
  (gethash key table default))
```

**Verify:** `dune test`; type error if body doesn't match declared return type

### R2: Type assertion macro

**Given** `(tart TYPE FORM)` in an Elisp file
**When** type-checked
**Then** the checker:
1. Parses TYPE using the `.tart` type syntax
2. Infers the type of FORM
3. Emits an error if FORM's type is not compatible with TYPE
4. Uses TYPE as the resulting type of the expression

**And** at runtime, the macro expands to FORM unchanged.

```elisp
;; Valid assertions
(tart string "hello")
(tart int 42)
(tart (list int) (list 1 2 3))

;; Error: int is not compatible with string
(tart string (+ 1 2))
```

**Verify:** `dune test`; type error on mismatch; no error on valid annotations

### R3: Variable type declarations (defvar/defconst)

**Given** `(defvar NAME (tart TYPE VALUE))` or `(defconst NAME (tart TYPE VALUE))`
**When** type-checked
**Then** NAME has type TYPE in the environment
**And** VALUE is checked against TYPE
**And** subsequent `setq`/`setf` to NAME are checked against TYPE

```elisp
(defvar my-cache (tart (hash-table string int) (make-hash-table)))
(defconst my-version (tart string "1.0.0"))
(defvar my-handler (tart ((string) -> nil) #'ignore))

;; Later assignments are checked
(setq my-cache (make-hash-table))     ; OK
(setq my-cache "not a hash table")    ; Error
```

**Verify:** `dune test`; setq to wrong type produces error

### R4: Variable declaration without initial value

**Given** `(tart-declare NAME TYPE)` at top-level
**When** type-checked
**Then** NAME has type TYPE in the environment
**And** subsequent `setq`/`setf` and reads of NAME use TYPE

```elisp
(tart-declare my-buffer buffer)
(defvar my-buffer)

;; Reads and writes checked against buffer
(setq my-buffer (get-buffer "*scratch*"))  ; OK if returns buffer
(buffer-name my-buffer)                     ; OK if expects buffer
```

**Verify:** `dune test`; uninitialized variable has declared type

### R5: File-local type aliases

**Given** `(tart-type NAME DEFINITION)` at top-level in an Elisp file
**When** type-checked
**Then** NAME is available as a type alias within that file
**And** the alias is NOT exported (not visible to other modules)
**And** at runtime, the form expands to `nil`.

```elisp
(tart-type int-pair (tuple int int))
(tart-type result (string | error))

;; Use in annotations
(defvar my-pair (tart int-pair (cons 1 2)))
```

**Verify:** `dune test`; alias usable in same file; not visible from other modules

### R6: Parameterized type aliases

**Given** `(tart-type NAME [VARS] DEFINITION)` with type variables
**When** type-checked
**Then** NAME is a parameterized type alias

```elisp
(tart-type predicate [a] ((a) -> bool))
(tart-type mapping [k v] (hash-table k v))

;; Usage
(defvar my-pred (tart (predicate int) (lambda (x) (> x 0))))
```

**Verify:** `dune test`; parameterized aliases instantiate correctly

### R7: Invariant type constructors

**Given** parameterized types like `(list a)`, `(vector a)`, `(hash-table k v)`
**When** checking type compatibility
**Then** type parameters must match exactly (invariance)

```elisp
(defvar my-ints (tart (list int) '(1 2 3)))

;; Error: (list int) is not compatible with (list string)
(defun takes-string-list (xs)
  (declare (tart ((list string)) -> nil))
  nil)
(takes-string-list my-ints)  ; Type error

;; This is sound: prevents mutation bugs
(defun mutates-list (xs)
  (declare (tart ((list string)) -> nil))
  (push "oops" xs))  ; Would corrupt my-ints if allowed
```

**Verify:** `dune test`; `(list int)` not subtype of `(list string)` (invariance)

### R8: Macro expansion (runtime behavior)

**Given** the macros at runtime (without type checker)
**When** expanded
**Then**:
- `(tart TYPE FORM)` expands to `FORM`
- `(tart-type NAME DEF)` expands to `nil`
- `(tart-declare NAME TYPE)` expands to `nil`
- `(declare (tart ...))` is ignored (standard declare behavior)

```elisp
(macroexpand '(tart string x))          ; => x
(macroexpand '(tart-type foo int))      ; => nil
(macroexpand '(tart-declare x int))     ; => nil
```

**Verify:** `(ert-deftest ...)` in tart-tests.el confirms expansion

### R9: Error messages

**Given** a type mismatch in any tart form
**When** the error is reported
**Then** the message includes:
- The expected type (from annotation)
- The actual inferred type
- The source location

```
my-file.el:42:5: error: type mismatch in annotation
  expected: string
  actual:   int

my-file.el:10:1: error: function body doesn't match declared return type
  declared: int
  inferred: string
```

**Verify:** `dune test`; error messages contain expected/actual types and location

### R10: Interaction with .tart files

**Given** a module with both `.tart` file and inline annotations
**When** type-checked
**Then**:
- `.tart` declarations define the public interface and are always authoritative
- Inline annotations provide internal types
- Inline annotations cannot contradict `.tart` declarations
    - may be covariant or contravariant vs the public signature as appropriate
    - some though needed here--support explicit co/contra builtin type constructor?

```elisp
;; my-utils.tart declares: (defun my-add (int int) -> int)

;; my-utils.el
(defun my-add (x y)
  (declare (tart (int int) -> int))  ; Must match .tart
  (+ x y))

;; Internal helper not in .tart
(defun internal-helper (s)
  (declare (tart (string) -> int))
  (length s))
```

**Verify:** `dune test`; mismatch between .tart and inline produces error

## Tasks

- [x] [R8] Implement `tart` macro in tart.el (expand to form)
- [x] [R8] Implement `tart-type` macro in tart.el (expand to nil)
- [x] [R8] Implement `tart-declare` macro in tart.el (expand to nil)
- [x] [R1] Recognize `(declare (tart ...))` in function definitions
- [x] [R2] Add expression annotation recognition to checker
- [x] [R3,R4] Track variable types from annotations
- [x] [R3] Check setq/setf against declared variable types
- [x] [R5,R6] Implement file-local type alias scope
- [x] [R7] Enforce invariance for parameterized types
- [x] [R9] Format error messages for annotation mismatches
- [x] [R10] Verify inline annotations match .tart declarations
- [x] [R8] Write ERT tests for macro expansion

## Status

Complete
