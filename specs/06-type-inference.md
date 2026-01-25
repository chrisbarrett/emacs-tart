# Spec 06: Type Inference Engine

Constraint-based Hindley-Milner type inference with levels-based generalization.

**Dependencies:** Spec 04 parser, Spec 05 interpreter for macro expansion.

## Goal

Implement the core type inference engine that infers types for Elisp
expressions, generating diagnostics for type errors.

## Constraints

- **Sound**: Within typed code, no runtime type errors possible
- **HM complete**: Infers principal types without annotations (except boundaries)
- **Levels-based**: Near-linear generalization performance
- **Incremental-ready**: Per-function inference for LSP integration

## Output

```
tart/
├── lib/
│   ├── core/
│   │   ├── types.ml       ; Type representation
│   │   └── type_env.ml    ; Type environment
│   └── typing/
│       ├── constraint.ml  ; Constraint representation
│       ├── infer.ml       ; Constraint generation
│       ├── unify.ml       ; Unification with union-find
│       ├── generalize.ml  ; Let-generalization with levels
│       └── check.ml       ; Top-level type checking API
└── test/
    └── typing/
        ├── infer_test.ml
        └── unify_test.ml
```

## Requirements

### R1: Type representation

**Given** the type grammar from Spec 03
**When** types are represented in OCaml
**Then** the type includes:
- `TVar of tvar ref` (mutable for union-find)
- `TCon of string` (Int, String, Nil, T, etc.)
- `TApp of string * typ list` (List, Option, etc.)
- `TArrow of typ list * typ` (grouped params, return)
- `TForall of string list * typ`

Where `tvar = Unbound of int * int | Link of typ` (id, level)

**Verify:** `dune test`; type pretty-printer produces valid type syntax

### R2: Truthiness constraint

**Given** the truthiness hierarchy: `Nil < Bool < Any`, `Truthy < Any`
**When** `(Option a)` is constructed
**Then** `a` must unify with a type that is `Truthy`

**Verify:** `(Option Nil)` produces type error; `(Option String)` succeeds

### R3: Constraint generation for expressions

**Given** a core expression
**When** `infer` is called
**Then** it produces `(type, constraints)` where constraints are `τ₁ = τ₂` pairs

Expressions to handle:
- Literals: produce base types
- Variables: instantiate from environment
- Lambda: introduce fresh type vars for params
- Application: generate `τ_fun = (τ_args...) -> τ_result`
- Let: generate constraints for binding, solve, then generalize

**Verify:** Constraint generation tests for each expression form

### R4: Unification with union-find

**Given** a set of equality constraints
**When** `solve` is called
**Then** constraints are solved via unification:
- `TVar` links to the other type
- `TCon` unifies only with same `TCon`
- `TArrow` unifies component-wise
- `TApp` unifies constructor and args

**Verify:** Unification tests; occurs check prevents infinite types

### R5: Levels-based generalization

**Given** a let-binding with inferred type
**When** generalization occurs
**Then** only type variables with level > current scope level are generalized
**And** level is decremented on scope exit

**Verify:** `(let ((id (lambda (x) x))) (id 1) (id "s"))` succeeds;
`id` gets `(forall (a) (-> (a) a))`

### R6: Value restriction

**Given** a let-binding where RHS is not a syntactic value
**When** generalization is attempted
**Then** the type is not generalized (remains monomorphic)

Syntactic values: lambda, literal, variable, constructor application

**Verify:** `(let ((xs (reverse '()))) xs)` has monomorphic type

### R7: Core form inference

**Given** core Elisp forms post-expansion
**When** type-checked
**Then** inference handles:
- `defun`: binds name to function type in env
- `if`: branches must unify; result is their common type
- `progn`: result is type of last expression
- `let`/`let*`: standard HM let with generalization
- `setq`: RHS must match variable's type

**Verify:** Test suite covering each core form

### R8: Built-in function types

**Given** a call to a built-in (`car`, `+`, `concat`, etc.)
**When** type-checked
**Then** the call is checked against the built-in's declared signature

**Verify:** `(car '(1 2 3))` infers `(Option Int)`;
`(+ 1 "x")` produces type error

### R9: Polymorphic instantiation

**Given** a polymorphic type `(forall (a) (-> (a) a))`
**When** used in a context requiring `(-> Int Int)`
**Then** fresh type variables replace bound variables during instantiation

**Verify:** `(let ((id (lambda (x) x))) (+ (id 1) (id 2)))` succeeds

### R10: Type error diagnostics

**Given** a type error (unification failure)
**When** reported
**Then** the diagnostic includes:
- Source location of the error
- Expected type
- Actual type
- Related locations (e.g., where expected type originated)

**Verify:** Error message tests check structure and content

### R11: Occurrence typing (basic)

**Given** a conditional with a type predicate
**When** type-checked
**Then** the variable's type is narrowed in the appropriate branch:
- `(if (stringp x) ... ...)` → x is String in then-branch

**Verify:** `(if (stringp x) (upcase x) ...)` type-checks without error

## Tasks

- [ ] [R1] Define type representation with union-find tvars
- [ ] [R2] Implement truthiness constraint checking
- [ ] [R3] Implement constraint generation
- [ ] [R4] Implement unification
- [ ] [R5] Implement levels-based generalization
- [ ] [R6] Implement value restriction
- [ ] [R7] Handle all core forms
- [ ] [R8] Load built-in function signatures
- [ ] [R9] Implement polymorphic instantiation
- [ ] [R10] Implement diagnostic generation
- [ ] [R11] Basic occurrence typing for type predicates

Run review agent after inference handles polymorphic `map` correctly before
proceeding to Spec 07.
