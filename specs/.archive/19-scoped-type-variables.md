# Spec 19: Scoped Type Variables

Enable type variables to be shared across multiple signatures within a scope.

- **Dependencies:**
  - [Spec 15](./15-explicit-forall.md) (Explicit Forall)
  - [Spec 17](./17-higher-kinded-types.md) (Higher-Kinded Types)

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
;; Bind type variables across declarations
(let [a]
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
│       ├── sig_ast.ml       ; Add Let variant
│       ├── sig_parser.ml    ; Parse `let` blocks
│       └── sig_loader.ml    ; Load scoped declarations
└── test/
    └── sig/
        └── scoped_test.ml
```

## Requirements

### R1: Type scope declaration syntax

**Given** a `.tart` signature file **When** it contains a `let` block
**Then** the type variables are shared across all declarations in the block:

```elisp
(let [a]
  (defun iter-next ((iter a)) -> ((a | nil)))
  (defun iter-peek ((iter a)) -> ((a | nil)))
  (defun iter-collect ((iter a)) -> (list a)))
```

**Verify:** `dune test`; scoped functions parse correctly

### R2: Scoped type variable binding

**Given** a type scope with variables `[a b]` **When** declarations inside use
`a` or `b` **Then** they refer to the same type variable (not fresh ones)
**And** declarations outside the scope cannot use those variables

```elisp
(let [a]
  (defun get-first ((list a)) -> (a | nil))
  (defun get-last ((list a)) -> (a | nil)))

;; This 'a' is independent
(defun other-fn [a] (a) -> a)
```

**Verify:** `dune test`; scoped variables are shared; external are independent

### R3: Explicit forall inside scope

**Given** a function inside a `let` that needs additional type variables
**When** declared with explicit `[vars]` **Then** those variables are added to
the scope variables:

```elisp
(let [a]
  ;; Uses scope's 'a' plus local 'b'
  (defun iter-map [b] (((a -> b)) (iter a)) -> (iter b)))
```

**Verify:** `dune test`; local forall merges with scope

### R4: Higher-kinded scoped variables

**Given** a type scope with HK variable annotation **When** used in declarations
**Then** kind constraints are checked:

```elisp
(let [(f : (* -> *))]
  (defun fmap-scope [a b] (((a -> b)) (f a)) -> (f b))
  (defun pure-scope [a] (a) -> (f a)))
```

**Verify:** `dune test`; HK scoped variables work correctly

### R5: Nested type scopes

**Given** nested `let` blocks **When** parsed **Then** inner scope
shadows outer scope variables of the same name:

```elisp
(let [a]
  (defun outer ((list a)) -> a)
  (let [a]  ; shadows outer 'a'
    (defun inner ((vector a)) -> a)))
```

**Verify:** `dune test`; nested scopes shadow correctly

### R6: Type scope with opaque types

**Given** a type scope combined with opaque type declaration **When** parsed
**Then** the scoped variable can be used in the opaque type:

```elisp
(let [a]
  (type iter)  ; opaque, but conceptually holds 'a'
  (defun make-iter ((list a)) -> iter)
  (defun iter-next (iter) -> (a | nil)))
```

Note: The `a` in the opaque context is a phantom type - the scope provides
documentation but no structural connection.

**Verify:** `dune test`; opaque types work in scopes

### R7: Scope at module level

**Given** a `let` at the top level of a `.tart` file **When** loaded
**Then** all contained declarations are exported with the scope applied:

```elisp
;; iter.tart
(let [a]
  (type iter)
  (defun make-iter ((list a)) -> iter)
  (defun iter-next (iter) -> (a | nil)))

;; Using module:
;; iter-next has type [a] (iter a) -> (a | nil)
```

**Verify:** `dune test`; exported functions have correct polymorphic types

### R8: Error on unbound scoped variable

**Given** a declaration inside `let` using undefined variable **When**
parsed **Then** error is reported:

```elisp
(let [a]
  (defun bad (b) -> b))  ; Error: unbound type variable 'b'
```

**Verify:** `dune test`; unbound variables in scope produce error

## Non-Requirements

- Scoped variables in inline `(declare (tart ...))` annotations
- Cross-file scope sharing
- Type-class-like instance resolution
- Implicit scope inference

## Tasks

- [x] [R1] Add Let variant to sig_ast.ml
- [x] [R1] Parse `let` blocks in sig_parser.ml
- [x] [R2] Implement scope variable binding during loading
- [x] [R3] Handle explicit forall inside scope
- [x] [R4] Integrate with kind inference for HK scoped variables
- [x] [R5] Implement nested scope shadowing
- [x] [R6] Handle opaque types in scopes
- [x] [R7] Export scoped declarations with correct types
- [x] [R8] Validate variable binding in scopes

**Status:** Implemented via `DTypeScope` in `lib/sig/sig_ast.mli` with kind inference support in `lib/typing/kind_infer.mli`.

## Design Notes

### AST Representation

```ocaml
type decl =
  | ...
  | DLet of tvar_binder list * decl list
```

### Loading Strategy

When loading a `let`:

1. Create fresh type variables for the scope's binders
2. Add them to a "scope context" (separate from the module's type context)
3. Load each declaration with the scope context available
4. For each defun, the final scheme includes both scope vars and local vars

### Example: Full Iterator Module

```elisp
;; iter.tart
(let [a]
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
