# Spec 21: Type Classes

Type classes enable ad-hoc polymorphism by defining interfaces that types can implement.

**Dependencies:** Spec 17 (Higher-Kinded Types)

## Goal

Implement a type class system that allows defining generic interfaces and
providing type-specific implementations, enabling patterns like Functor, Show,
and Eq in Emacs Lisp with type safety.

## Motivation

Many common programming patterns require ad-hoc polymorphism where a function
behaves differently based on the type of its arguments. Currently, Emacs Lisp
handles this through:

1. Runtime type predicates (`(if (stringp x) ... (if (listp x) ...)`)
2. Generic functions via cl-generic (`cl-defmethod`)
3. Duck typing (assuming methods exist)

Type classes provide:

- Compile-time verification that required operations exist
- Type-safe abstraction over concrete types
- Documentation of required interfaces
- Better error messages when implementations are missing

## Constraints

- **No runtime overhead**: Type classes are erased; dispatch uses existing mechanisms
- **Compatible with cl-generic**: Generated code uses `cl-defmethod` for dispatch
- **Gradual adoption**: Types without class instances still work
- **Kind-aware**: Works with HKT for Functor-like patterns

## Output

```
tart/
├── lib/
│   └── typing/
│       ├── type_class.ml      ; Type class representation
│       ├── type_class.mli
│       ├── instance.ml        ; Instance checking and resolution
│       └── instance.mli
├── stdlib/
│   ├── classes/
│   │   ├── eq.tart           ; Equality class
│   │   ├── ord.tart          ; Ordering class
│   │   ├── show.tart         ; Printing class
│   │   └── functor.tart      ; Functor class (HKT)
└── test/
    └── typing/
        └── type_class_test.ml
```

## Syntax

### Class Definition

```lisp
;; Simple class
(class (Eq a)
  (eq (a a) -> bool))

;; Class with superclass constraint
(class (Ord a) (Eq a)
  (compare (a a) -> ordering))

;; Higher-kinded class
(class (Functor (f : (* -> *)))
  (fmap [a b] (((a) -> b) (f a)) -> (f b)))
```

### Instance Declaration

```lisp
;; Instance in signature file
(instance (Eq int)
  (eq . int-eq))

(instance (Eq string)
  (eq . string-eq))

;; Parameterized instance
(instance [a] (Eq a) => (Eq (list a))
  (eq . list-eq))

;; HKT instance
(instance (Functor list)
  (fmap . mapcar))
```

### Using Class Constraints

```lisp
;; Function requiring Eq constraint
(defun member-check [a] (Eq a) => (a (list a)) -> bool)

;; Multiple constraints
(defun sort-unique [a] (Eq a) (Ord a) => ((list a)) -> (list a))
```

## Requirements

### R1: Class definition parsing

**Given** a class definition in a signature file
**When** the signature is parsed
**Then** the class is registered with its name, parameters, superclasses, and methods

```lisp
(class (Eq a)
  (eq (a a) -> bool)
  (neq (a a) -> bool))
```

**Verify:** `dune test`; class definitions parse correctly

### R2: Instance declaration parsing

**Given** an instance declaration in a signature file
**When** the signature is parsed
**Then** the instance is registered with its class, type, constraints, and method mappings

```lisp
(instance (Eq int)
  (eq . =)
  (neq . /=))
```

**Verify:** `dune test`; instance declarations parse correctly

### R3: Constraint syntax in defun

**Given** a function signature with class constraints
**When** the signature is parsed
**Then** the constraints are attached to the function's type scheme

```lisp
(defun elem [a] (Eq a) => (a (list a)) -> bool)
```

**Verify:** `dune test`; constraint syntax parses correctly

### R4: Instance resolution for monomorphic types

**Given** a call to a constrained function with concrete type arguments
**When** type checking
**Then** the system verifies an instance exists for each constraint

```lisp
;; Given: (defun elem [a] (Eq a) => (a (list a)) -> bool)
;; And: (instance (Eq int) ...)
(elem 42 '(1 2 3))  ; OK - Eq int exists
(elem buf buffers)  ; Error - no Eq buffer instance
```

**Verify:** `dune test`; instance resolution works for concrete types

### R5: Instance resolution for polymorphic constraints

**Given** a parameterized instance with constraints
**When** type checking a call
**Then** the system recursively resolves required constraints

```lisp
;; Given: (instance [a] (Eq a) => (Eq (list a)) ...)
(elem '(1 2) nested-list)  ; Needs Eq (list int), which needs Eq int
```

**Verify:** `dune test`; nested constraint resolution works

### R6: Superclass constraints

**Given** a class with superclass constraints
**When** resolving an instance
**Then** the system verifies superclass instances exist

```lisp
;; Given: (class (Ord a) (Eq a) (compare (a a) -> ordering))
;; And: (instance (Ord int) ...)
;; Then: Eq int must also have an instance
```

**Verify:** `dune test`; superclass constraints are checked

### R7: HKT class support

**Given** a class parameterized by a higher-kinded type
**When** declaring instances
**Then** type constructors can be used as instance heads

```lisp
(class (Functor (f : (* -> *)))
  (fmap [a b] (((a) -> b) (f a)) -> (f b)))

(instance (Functor list)
  (fmap . mapcar))

(instance (Functor option)
  (fmap . option-map))
```

**Verify:** `dune test`; HKT classes and instances work

### R8: Missing instance error messages

**Given** a call requiring a constraint that cannot be satisfied
**When** type checking fails
**Then** the error message indicates which constraint is unsatisfied

```
Error: No instance of `Eq buffer` found
  Required by: (elem current buffers)
  At: file.el:42:3

Note: `Eq` requires implementations of:
  - eq : (buffer buffer) -> bool
```

**Verify:** Error messages show missing constraint clearly

### R9: Overlapping instance detection

**Given** multiple instances that could match the same type
**When** loading signatures
**Then** a warning or error is produced

```lisp
(instance (Eq (list int)) ...)
(instance [a] (Eq a) => (Eq (list a)) ...)
;; Both could match Eq (list int)
```

**Verify:** `dune test`; overlapping instances detected

### R10: Default method implementations

**Given** a class with default implementations
**When** declaring an instance
**Then** missing methods use the default

```lisp
(class (Eq a)
  (eq (a a) -> bool)
  ;; Default: (defun neq (a b) (not (eq a b)))
  (neq (a a) -> bool :default (not (eq a b))))

(instance (Eq int)
  (eq . =))
;; neq automatically uses default
```

**Verify:** `dune test`; default methods are used when not overridden

## Non-Requirements

- Multi-parameter type classes (single parameter only for now)
- Functional dependencies
- Type families
- Orphan instance warnings
- Instance derivation/deriving clauses
- Associated types

## Tasks

- [x] [R1] Add class definition AST and parsing
- [x] [R2] Add instance declaration AST and parsing
- [x] [R3] Add constraint syntax to defun parsing
- [ ] [R4] Implement basic instance resolution
- [ ] [R5] Implement recursive constraint resolution
- [ ] [R6] Implement superclass constraint checking
- [ ] [R7] Add HKT class support
- [ ] [R8] Implement instance error formatting
- [ ] [R9] Add overlapping instance detection
- [ ] [R10] Implement default method support

Run review agent after R1-R4 complete to validate approach.

## Design Notes

### Instance Dictionary

Type class methods are resolved to specific functions at compile time. The
"instance dictionary" maps (Class, Type) pairs to their method implementations:

```ocaml
type instance_dict = {
  inst_class: string;
  inst_type: typ;
  inst_constraints: constraint list;  (* for parameterized instances *)
  inst_methods: (string * string) list;  (* class method -> impl function *)
}
```

### Resolution Algorithm

1. Given a constraint `C T`:
   - Look for direct instance `(instance (C T) ...)`
   - Look for parameterized instance `(instance [a] Constraints => (C (Constructor a)) ...)`
   - If found parameterized instance, recursively resolve `Constraints`
   - Check superclass constraints

2. Cache resolved instances to avoid recomputation

### Integration Points

- `sig_parser.ml`: Parse class/instance syntax
- `sig_loader.ml`: Load and validate instances
- `type_class.ml`: Class and instance representation
- `instance.ml`: Resolution algorithm
- `infer.ml`: Check constraints at call sites
- `diagnostic.ml`: Instance error messages

### Runtime Representation

Type classes have no runtime representation. The type checker verifies all
constraints are satisfiable and the generated code calls the resolved
implementation functions directly.
