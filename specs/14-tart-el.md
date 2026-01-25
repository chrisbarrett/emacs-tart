# Spec 14: tart.el Runtime Library

Elisp library providing type annotation macros for inline use in `.el` files.

**Dependencies:** Spec 07 signature files (type syntax parsing).

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

## Output

```
lisp/
└── tart.el              ; Runtime library (extends Spec 10)
```

## Requirements

### R1: Function type signatures

**Given** `(declare (tart TYPE))` inside a function definition
**When** type-checked
**Then** the checker uses TYPE as the function's signature
**And** verifies the body's inferred type matches the declared return type
**And** parameters are bound to their declared types in the body

```elisp
(defun my-add (x y)
  (declare (tart (Int Int) -> Int))
  (+ x y))

(defun my-identity (x)
  (declare (tart [a] a -> a))
  x)

;; Works with cl-defun, defmacro, etc.
(cl-defun my-lookup (key table &key default)
  (declare (tart [k v] (k (HashTable k v) &key :default v) -> v))
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
(tart String "hello")
(tart Int 42)
(tart (List Int) (list 1 2 3))

;; Error: Int is not compatible with String
(tart String (+ 1 2))
```

**Verify:** `dune test`; type error on mismatch; no error on valid annotations

### R3: Variable type declarations (defvar/defconst)

**Given** `(defvar NAME (tart TYPE VALUE))` or `(defconst NAME (tart TYPE VALUE))`
**When** type-checked
**Then** NAME has type TYPE in the environment
**And** VALUE is checked against TYPE
**And** subsequent `setq`/`setf` to NAME are checked against TYPE

```elisp
(defvar my-cache (tart (HashTable String Int) (make-hash-table)))
(defconst my-version (tart String "1.0.0"))
(defvar my-handler (tart (String -> Nil) #'ignore))

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
(tart-declare my-buffer Buffer)
(defvar my-buffer)

;; Reads and writes checked against Buffer
(setq my-buffer (get-buffer "*scratch*"))  ; OK if returns Buffer
(buffer-name my-buffer)                     ; OK if expects Buffer
```

**Verify:** `dune test`; uninitialized variable has declared type

### R5: File-local type aliases

**Given** `(tart-type NAME DEFINITION)` at top-level in an Elisp file
**When** type-checked
**Then** NAME is available as a type alias within that file
**And** the alias is NOT exported (not visible to other modules)
**And** at runtime, the form expands to `nil`.

```elisp
(tart-type IntPair (Tuple Int Int))
(tart-type Result (Or String Error))

;; Use in annotations
(defvar my-pair (tart IntPair (cons 1 2)))
```

**Verify:** `dune test`; alias usable in same file; not visible from other modules

### R6: Parameterized type aliases

**Given** `(tart-type NAME [VARS] DEFINITION)` with type variables
**When** type-checked
**Then** NAME is a parameterized type alias

```elisp
(tart-type Predicate [a] (a -> Bool))
(tart-type Mapping [k v] (HashTable k v))

;; Usage
(defvar my-pred (tart (Predicate Int) (lambda (x) (> x 0))))
```

**Verify:** `dune test`; parameterized aliases instantiate correctly

### R7: Invariant type constructors

**Given** parameterized types like `(List a)`, `(Vector a)`, `(HashTable k v)`
**When** checking type compatibility
**Then** type parameters must match exactly (invariance)

```elisp
(defvar my-ints (tart (List Int) '(1 2 3)))

;; Error: (List Int) is not compatible with (List Any)
(defun takes-any-list (xs)
  (declare (tart (List Any) -> Nil))
  nil)
(takes-any-list my-ints)  ; Type error

;; This is sound: prevents mutation bugs
(defun mutates-list (xs)
  (declare (tart (List Any) -> Nil))
  (push "oops" xs))  ; Would corrupt my-ints if allowed
```

**Verify:** `dune test`; `(List Int)` not subtype of `(List Any)`

### R8: Macro expansion (runtime behavior)

**Given** the macros at runtime (without type checker)
**When** expanded
**Then**:
- `(tart TYPE FORM)` expands to `FORM`
- `(tart-type NAME DEF)` expands to `nil`
- `(tart-declare NAME TYPE)` expands to `nil`
- `(declare (tart ...))` is ignored (standard declare behavior)

```elisp
(macroexpand '(tart String x))          ; => x
(macroexpand '(tart-type Foo Int))      ; => nil
(macroexpand '(tart-declare x Int))     ; => nil
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
  expected: String
  actual:   Int

my-file.el:10:1: error: function body doesn't match declared return type
  declared: Int
  inferred: String
```

**Verify:** Error messages contain expected/actual types and location

### R10: Interaction with .tart files

**Given** a module with both `.tart` file and inline annotations
**When** type-checked
**Then**:
- `.tart` declarations define the public interface
- Inline annotations provide internal types
- Inline annotations cannot contradict `.tart` declarations

```elisp
;; my-utils.tart declares: (defun my-add (Int Int) -> Int)

;; my-utils.el
(defun my-add (x y)
  (declare (tart (Int Int) -> Int))  ; Must match .tart
  (+ x y))

;; Internal helper not in .tart
(defun internal-helper (s)
  (declare (tart String -> Int))
  (length s))
```

**Verify:** `dune test`; mismatch between .tart and inline produces error

## Tasks

- [ ] [R8] Implement `tart` macro in tart.el (expand to form)
- [ ] [R8] Implement `tart-type` macro in tart.el (expand to nil)
- [ ] [R8] Implement `tart-declare` macro in tart.el (expand to nil)
- [ ] [R1] Recognize `(declare (tart ...))` in function definitions
- [ ] [R2] Add expression annotation recognition to checker
- [ ] [R3,R4] Track variable types from annotations
- [ ] [R3] Check setq/setf against declared variable types
- [ ] [R5,R6] Implement file-local type alias scope
- [ ] [R7] Enforce invariance for parameterized types
- [ ] [R9] Format error messages for annotation mismatches
- [ ] [R10] Verify inline annotations match .tart declarations
- [ ] [R8] Write ERT tests for macro expansion

Run review agent after R1-R2 work (function signatures and expression annotations)
before implementing R3-R4 (variable tracking).
