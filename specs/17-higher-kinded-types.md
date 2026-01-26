# Spec 17: Higher-Kinded Types

Enable type constructors as first-class values via kind inference.

**Dependencies:** Spec 15 (Forall Inference) complete

## Goal

Allow polymorphism over type constructors (e.g., `list`, `option`) so generic
functions like `fmap` can operate over any container type.

## Motivation

Currently, tart supports first-order polymorphism:

```elisp
;; Works: polymorphic over values
(defun identity [a] (a) -> a)
(defun map [a b] (((a -> b)) (list a)) -> (list b))
```

But cannot express:

```elisp
;; Cannot express: polymorphic over type constructors
(defun fmap [f a b] (((a -> b)) (f a)) -> (f b))  ; f is a type constructor
```

Higher-kinded types (HKT) enable abstraction over containers, unlocking:
- Functor/Monad-like abstractions
- Generic traversals
- Type-safe builder patterns

## Constraints

- **Backward compatible**: Existing signatures continue to work unchanged
- **Kind inference**: Infer kinds from usage; explicit annotation optional
- **No typeclass syntax**: Express patterns via function signatures only
- **Existing semantics preserved**: Type variables default to kind `*`

## Output

```
tart/
├── lib/
│   ├── typing/
│   │   ├── kind.ml            ; Kind representation
│   │   ├── kind.mli
│   │   ├── kind_infer.ml      ; Kind inference algorithm
│   │   └── kind_infer.mli
│   └── sig/
│       └── sig_loader.ml      ; Updated for kinds
└── test/
    └── typing/
        └── kind_test.ml
```

## Background

### Kinds

Kinds classify types the way types classify values:

| Kind       | Meaning                                           |
| ---------- | ------------------------------------------------- |
| `*`        | Concrete type (int, string, (list int))           |
| `* -> *`   | Type constructor taking one type (list, option)   |
| `* -> * -> *` | Type constructor taking two (hash-table, result) |

### Kind Inference

From a signature like:

```elisp
(defun fmap [f a b] (((a -> b)) (f a)) -> (f b))
```

Inference deduces:
- `a : *` (used as argument type)
- `b : *` (used as return type)
- `f : * -> *` (applied to a single type argument)

## Requirements

### R1: Kind representation

**Given** a type system with type constructors
**When** kinds are represented internally
**Then** use:

```ocaml
type kind =
  | KStar                (* concrete type *)
  | KArrow of kind * kind (* type constructor *)
```

**Verify:** `dune test`; kinds can be constructed and compared

### R2: Kind inference for type variables

**Given** a signature with higher-kinded type variables
**When** parsed and kind-checked
**Then** kinds are inferred from usage patterns:

```elisp
;; f : * -> *, a : *, b : *
(defun fmap [f a b] (((a -> b)) (f a)) -> (f b))
```

**Verify:** `dune test`; `f` inferred as `* -> *`

### R3: Kind checking on type applications

**Given** a type application `(f a)`
**When** kind-checked
**Then** verify `f : k1 -> k2` and `a : k1`, yielding `k2`

```elisp
(defun bad [f a] (f a) -> int)
;; If f : * and a : *, then (f a) is a kind error
```

**Verify:** `dune test`; mismatched kinds produce errors

### R4: Explicit kind annotations

**Given** a type variable with explicit kind annotation
**When** parsed
**Then** the annotation constrains inference:

```elisp
(defun fmap [(f : (* -> *))] [a b] (((a -> b)) (f a)) -> (f b))
```

**Verify:** `dune test`; explicit kinds override inference

### R5: Default kind for type variables

**Given** a type variable used only in concrete position
**When** kind inference runs
**Then** default to kind `*`:

```elisp
(defun identity [a] (a) -> a)  ; a : * (unchanged behavior)
```

**Verify:** `dune test`; existing signatures unchanged

### R6: Kind error messages

**Given** a kind mismatch
**When** reported
**Then** message includes expected/found kinds:

```
Error: kind mismatch
  expected: * -> *
  found: *
  in type application: (f a)
```

**Verify:** `dune test`; error messages show kinds

### R7: Type constructor instantiation

**Given** a higher-kinded function like `fmap`
**When** called with a concrete container:

```elisp
(fmap #'1+ my-list)
```

**Then** `f` instantiates to `list`, preserving type safety

**Verify:** `dune test`; HK instantiation works in type checking

### R8: Nested type constructors

**Given** type constructors with multiple parameters
**When** partially applied:

```elisp
;; result is * -> * -> *, (result e) is * -> *
(defun map-result [e a b] (((a -> b)) (result e a)) -> (result e b))
```

**Then** kind inference handles partial application

**Verify:** `dune test`; multi-param constructors work

## Non-Requirements

- Typeclass/trait syntax (use plain functions)
- Associated types
- Type families
- Functional dependencies

## Tasks

- [ ] [R1] Add `kind` type to lib/typing
- [ ] [R5] Default existing type variables to kind `*`
- [ ] [R2] Implement kind inference algorithm
- [ ] [R3] Add kind checking to type application
- [ ] [R4] Parse explicit kind annotations
- [ ] [R6] Implement kind error formatting
- [ ] [R7] Update unification for HK instantiation
- [ ] [R8] Test nested/partial type constructors

Run review agent after R1-R3 complete to validate approach.

## Design Notes

### Algorithm

Kind inference follows a similar pattern to type inference:

1. Assign fresh kind variables to type parameters
2. Collect kind constraints from type expressions
3. Unify kind constraints
4. Default unconstrained kind variables to `*`

### Integration Points

- `sig_parser.ml`: Parse kind annotations `(f : (* -> *))`
- `sig_loader.ml`: Propagate kinds through loading
- `infer.ml`: Kind-check type applications
- `unify.ml`: Handle HK instantiation
- `diagnostic.ml`: Kind error messages

### Future Extensions

Once HKT is implemented, these become possible:

- `Functor`-like patterns: `(defun fmap [f a b] ...)`
- Generic traversals over nested structures
- Type-safe effect systems
