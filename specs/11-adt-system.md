# Spec 11: Algebraic Data Type System

Runtime representation and pattern matching for ADTs declared in `.tart` files.

**Dependencies:** Spec 07 (signature files parse ADT declarations)

## Goal

Define how ADT values are represented at runtime and integrate with Elisp's
`pcase` for exhaustive pattern matching.

## Constraints

- **Zero overhead**: ADT values use standard Elisp data structures
- **pcase integration**: Patterns work with existing `pcase` infrastructure
- **Exhaustiveness**: Type checker warns on non-exhaustive matches

## Runtime Representation

ADT values are represented as tagged cons cells:

| Type-Level   | Runtime                |
|--------------|------------------------|
| `(Some x)`   | `(Some . x)`           |
| `(None)`     | `nil`                  |
| `(Ok x)`     | `(ok . x)`             |
| `(Err e)`    | `(err . e)`            |
| `(Leaf x)`   | `(leaf . x)`           |
| `(Node l r)` | `(node l . r)`         |

**Special case**: Single-value `Option` uses `nil` for `None` and unwrapped
value for `Some` when the value is truthy.

## Requirements

### R1: Constructor code generation

**Given** an ADT definition:
```elisp
(data Result (a e)
  (Ok a)
  (Err e))
```
**When** the module is loaded
**Then** constructor functions exist:
```elisp
(defun Ok (x) (cons 'ok x))
(defun Err (e) (cons 'err e))
```

**Verify:** `(Ok 42)` returns `(ok . 42)`

### R2: Predicate generation

**Given** an ADT definition
**When** loaded
**Then** predicates exist:
```elisp
(defun result-ok-p (r) (and (consp r) (eq (car r) 'ok)))
(defun result-err-p (r) (and (consp r) (eq (car r) 'err)))
```

**Verify:** `(result-ok-p (Ok 1))` returns `t`

### R3: Accessor generation

**Given** an ADT with fields
**When** loaded
**Then** accessors exist:
```elisp
(defun result-ok-value (r) (cdr r))
(defun result-err-value (r) (cdr r))
```

**Verify:** `(result-ok-value (Ok 42))` returns `42`

### R4: pcase pattern integration

**Given** a `pcase` expression matching an ADT:
```elisp
(pcase result
  (`(ok . ,value) (process value))
  (`(err . ,error) (handle error)))
```
**When** type-checked
**Then** `value` has type `a` and `error` has type `e` in their branches

**Verify:** Type narrowing works in pattern branches

### R5: Exhaustiveness checking

**Given** a non-exhaustive pattern match:
```elisp
(pcase opt
  (`(Some . ,x) x))  ; Missing: None
```
**When** type-checked
**Then** warning: "Non-exhaustive pattern match. Missing: None"

**Verify:** Warning on incomplete matches; no warning on complete matches

### R6: Multi-field constructors

**Given** an ADT with multiple fields:
```elisp
(data Point ()
  (Point2D Int Int)
  (Point3D Int Int Int))
```
**When** represented at runtime
**Then** use vectors or nested cons:
```elisp
(Point2D 1 2)   → [point2d 1 2]
(Point3D 1 2 3) → [point3d 1 2 3]
```

**Verify:** Accessors work for all fields

### R7: Recursive types

**Given** a recursive ADT:
```elisp
(data Tree (a)
  (Leaf a)
  (Node (Tree a) (Tree a)))
```
**When** values are constructed
**Then** they nest correctly:
```elisp
(Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))
→ (node (leaf . 1) (node (leaf . 2) (leaf . 3)))
```

**Verify:** Recursive construction and destruction works

## Tasks

- [ ] [R1] Generate constructor functions from ADT definitions
- [ ] [R2] Generate predicate functions
- [ ] [R3] Generate accessor functions
- [ ] [R4] Implement pcase type narrowing
- [ ] [R5] Implement exhaustiveness checking
- [ ] [R6] Handle multi-field constructors
- [ ] [R7] Test recursive type handling
