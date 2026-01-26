# Spec 19: Scoped Type Variables

Enable type variables to be shared across multiple signatures within a scope.

**Dependencies:** Spec 15 (Forall Inference), Spec 17 (Higher-Kinded Types)

## Goal

Allow type variables declared at module level to be shared across multiple
function signatures, enabling patterns like iterators, state machines, and
builder APIs where multiple functions must agree on a type parameter.

## Motivation

Currently, each function signature has its own independent type variables:

```elisp
;; Each 'a' is independent - no connection between iter-next and iter-peek
(defun iter-next [a] ((iter a)) -> ((a | nil)))
(defun iter-peek [a] ((iter a)) -> ((a | nil)))
```

With scoped type variables, we can express that these functions share a type:

```elisp
;; Declare a type variable scope
(type-scope [a]
  (defun iter-next ((iter a)) -> ((a | nil)))
  (defun iter-peek ((iter a)) -> ((a | nil))))
```

Use cases:
- **Iterators**: next, peek, collect all share element type
- **State machines**: transition functions share state type
- **Builder patterns**: chained setters share builder type
- **Test fixtures**: setup/teardown share fixture type

## Constraints

- **Backward compatible**: Existing signatures work unchanged
- **Explicit scoping**: Scopes are opt-in, not implicit
- **No inference across scopes**: Scope boundaries are explicit
- **Works with HKT**: Scoped variables can have higher kinds

## Output

```
tart/
├── lib/
│   └── sig/
│       ├── sig_ast.ml       ; Add TypeScope variant
│       ├── sig_parser.ml    ; Parse type-scope blocks
│       └── sig_loader.ml    ; Load scoped declarations
└── test/
    └── sig/
        └── scoped_test.ml
```

## Requirements

### R1: Type scope declaration syntax

**Given** a `.tart` signature file
**When** it contains a `type-scope` block
**Then** the type variables are shared across all declarations in the block:

```elisp
(type-scope [a]
  (defun iter-next ((iter a)) -> ((a | nil)))
  (defun iter-peek ((iter a)) -> ((a | nil)))
  (defun iter-collect ((iter a)) -> (list a)))
```

**Verify:** `dune test`; scoped functions parse correctly

### R2: Scoped type variable binding

**Given** a type scope with variables `[a b]`
**When** declarations inside use `a` or `b`
**Then** they refer to the same type variable (not fresh ones)
**And** declarations outside the scope cannot use those variables

```elisp
(type-scope [a]
  (defun get-first ((list a)) -> (a | nil))
  (defun get-last ((list a)) -> (a | nil)))

;; This 'a' is independent
(defun other-fn [a] (a) -> a)
```

**Verify:** `dune test`; scoped variables are shared; external are independent

### R3: Explicit forall inside scope

**Given** a function inside a type-scope that needs additional type variables
**When** declared with explicit `[vars]`
**Then** those variables are added to the scope variables:

```elisp
(type-scope [a]
  ;; Uses scope's 'a' plus local 'b'
  (defun iter-map [b] (((a -> b)) (iter a)) -> (iter b)))
```

**Verify:** `dune test`; local forall merges with scope

### R4: Higher-kinded scoped variables

**Given** a type scope with HK variable annotation
**When** used in declarations
**Then** kind constraints are checked:

```elisp
(type-scope [(f : (* -> *))]
  (defun fmap-scope [a b] (((a -> b)) (f a)) -> (f b))
  (defun pure-scope [a] (a) -> (f a)))
```

**Verify:** `dune test`; HK scoped variables work correctly

### R5: Nested type scopes

**Given** nested `type-scope` blocks
**When** parsed
**Then** inner scope shadows outer scope variables of the same name:

```elisp
(type-scope [a]
  (defun outer ((list a)) -> a)
  (type-scope [a]  ; shadows outer 'a'
    (defun inner ((vector a)) -> a)))
```

**Verify:** `dune test`; nested scopes shadow correctly

### R6: Type scope with opaque types

**Given** a type scope combined with opaque type declaration
**When** parsed
**Then** the scoped variable can be used in the opaque type:

```elisp
(type-scope [a]
  (type iter)  ; opaque, but conceptually holds 'a'
  (defun make-iter ((list a)) -> iter)
  (defun iter-next (iter) -> (a | nil)))
```

Note: The `a` in the opaque context is a phantom type - the scope provides
documentation but no structural connection.

**Verify:** `dune test`; opaque types work in scopes

### R7: Scope at module level

**Given** a type-scope at the top level of a `.tart` file
**When** loaded
**Then** all contained declarations are exported with the scope applied:

```elisp
;; iter.tart
(type-scope [a]
  (type iter)
  (defun make-iter ((list a)) -> iter)
  (defun iter-next (iter) -> (a | nil)))

;; Using module:
;; iter-next has type [a] (iter a) -> (a | nil)
```

**Verify:** `dune test`; exported functions have correct polymorphic types

### R8: Error on unbound scoped variable

**Given** a declaration inside type-scope using undefined variable
**When** parsed
**Then** error is reported:

```elisp
(type-scope [a]
  (defun bad (b) -> b))  ; Error: unbound type variable 'b'
```

**Verify:** `dune test`; unbound variables in scope produce error

## Non-Requirements

- Scoped variables in inline `(declare (tart ...))` annotations
- Cross-file scope sharing
- Type-class-like instance resolution
- Implicit scope inference

## Tasks

- [ ] [R1] Add TypeScope variant to sig_ast.ml
- [ ] [R1] Parse type-scope blocks in sig_parser.ml
- [ ] [R2] Implement scope variable binding during loading
- [ ] [R3] Handle explicit forall inside scope
- [ ] [R4] Integrate with kind inference for HK scoped variables
- [ ] [R5] Implement nested scope shadowing
- [ ] [R6] Handle opaque types in scopes
- [ ] [R7] Export scoped declarations with correct types
- [ ] [R8] Validate variable binding in scopes

Run review agent after R1-R3 work to validate approach before implementing R4-R8.

## Design Notes

### AST Representation

```ocaml
type decl =
  | ...
  | DTypeScope of tvar_binder list * decl list
```

### Loading Strategy

When loading a type-scope:
1. Create fresh type variables for the scope's binders
2. Add them to a "scope context" (separate from the module's type context)
3. Load each declaration with the scope context available
4. For each defun, the final scheme includes both scope vars and local vars

### Interaction with Forall Inference

Inside a type-scope:
- Variables from the scope are available without explicit `[vars]`
- Additional variables can be added with explicit `[vars]` on functions
- If a function uses explicit `[vars]`, inference is disabled (existing behavior)
- Scope variables + local variables form the complete quantifier set

### Example: Full Iterator Module

```elisp
;; iter.tart
(type-scope [a]
  (type iter)  ; opaque iterator type

  ;; Construction
  (defun iter-from-list ((list a)) -> iter)
  (defun iter-from-vector ((vector a)) -> iter)

  ;; Consumption
  (defun iter-next (iter) -> (a | nil))
  (defun iter-peek (iter) -> (a | nil))
  (defun iter-collect (iter) -> (list a))

  ;; Transformation (needs extra type var)
  (defun iter-map [b] (((a -> b)) iter) -> (iter b))
  (defun iter-filter (((a) -> bool) iter) -> iter))
```
