# Spec 18: Explicit Type Instantiation

Enable explicit instantiation of polymorphic types at call sites.

**Dependencies:** Spec 15 (Forall Inference) and Spec 17 (Higher-Kinded Types) complete

## Goal

Allow programmers to explicitly specify type arguments when calling polymorphic
functions, resolving ambiguity and improving type inference.

## Motivation

With higher-kinded types, type inference can become ambiguous:

```elisp
;; Ambiguous: what is 'f'?
(fmap #'1+ (make-my-container))
```

Explicit instantiation resolves this:

```elisp
;; Clear: f = list
(tart [list] fmap #'1+ my-list)
```

Other use cases:
- Disambiguating return types
- Documenting intended types inline
- Forcing specific instantiation for testing

## Constraints

- **Backward compatible**: Existing code without annotations works unchanged
- **Partial application**: Can specify some type args, infer others with `_`
- **Integration**: Works with HKT and existing forall inference
- **Syntax**: Must be valid Elisp (parsed but ignored at runtime)

## Output

```
tart/
├── lib/
│   ├── typing/
│   │   └── infer.ml          ; Handle @[...] instantiation
│   └── sig/
│       └── sig_parser.ml     ; Parse inline type arguments
├── lisp/
│   └── tart.el               ; @-reader macro (if needed)
└── test/
    └── typing/
        └── infer_test.ml     ; Instantiation tests
```

## Background

### Syntax Options

The syntax must be valid Elisp that the type checker can recognize:

**Option A: Function-like macro**
```elisp
(tart [list int] identity)    ; Instantiate identity at [list int]
```

**Option B: Annotation comment**
```elisp
(identity #|@[list]|# my-value) ; Type in comment
```

**Option C: Vector literal prefix**
```elisp
(identity @[list] my-value)     ; @[...] before first arg
```

We use **Option A** as it's the clearest and most Elisp-friendly.

### Partial Instantiation

When a function has multiple type parameters, some can be left for inference:

```elisp
;; Full: (defun pair [a b] (a b) -> (cons a b))
(tart [int _] pair 1 "hello")   ; a=int, b=inferred as string
```

## Requirements

### R1: Explicit instantiation syntax

**Given** a polymorphic function call
**When** annotated with `tart`
**Then** the type arguments are applied:

```elisp
;; (defun identity [a] (a) -> a)
(tart [int] identity 42)    ; OK: a = int
(tart [string] identity 42) ; Error: int not string
```

**Verify:** `dune test`

### R2: Higher-kinded instantiation

**Given** an HK-polymorphic function
**When** instantiated with a type constructor
**Then** the constructor is applied:

```elisp
;; (defun fmap [f a b] (((a -> b)) (f a)) -> (f b))
(tart [list int string] fmap #'number-to-string my-list)
```

**Verify:** `dune test`

### R3: Partial instantiation with placeholder

**Given** an instantiation with `_` placeholder
**When** type-checked
**Then** placeholders are inferred:

```elisp
;; (defun pair [a b] (a b) -> (cons a b))
(tart [_ string] pair 1 "hello")  ; a inferred as int
```

**Verify:** `dune test`

### R4: Arity checking

**Given** wrong number of type arguments
**When** type-checked
**Then** error is reported:

```elisp
;; identity has 1 type param
(tart [int string] identity 42)  ; Error: expected 1 type arg
```

**Verify:** `dune test`

### R5: Runtime expansion

**Given** a `tart` instantiation form
**When** evaluated at runtime
**Then** it expands to just the function call:

```elisp
(macroexpand '(tart [int] identity 42))
;; => (identity 42)
```

**Verify:** ERT tests; macro expands correctly

### R6: Error messages

**Given** a type mismatch with explicit instantiation
**When** reported
**Then** message references the explicit types:

```
Error: type mismatch
  expected: String (from tart annotation)
  found: Int
```

**Verify:** `dune test`

## Non-Requirements

- Instantiation at definition sites (only call sites)
- Instantiation for non-function types
- Inference from tart to surrounding context

## Tasks

- [x] [R5] Add `tart` macro to tart.el
- [x] [R1] Parse `tart` forms in infer.ml
- [x] [R1] Apply explicit type arguments during inference
- [x] [R3] Handle `_` placeholder for partial instantiation
- [x] [R2] Test HK type constructor instantiation
- [x] [R4] Validate type argument arity
- [x] [R6] Format error messages with annotation context

Run review agent after R1 complete to validate approach.

## Design Notes

### Parsing

The `tart` macro form `(tart [T1 T2 ...] fn arg1 arg2 ...)` is recognized
during inference. The type list is parsed using the existing signature parser
infrastructure.

### Instantiation

When a `tart` instantiation form is encountered:

1. Look up the function's polymorphic type
2. Parse the type arguments from the vector
3. Substitute type variables with provided types (or fresh TVars for `_`)
4. Type-check arguments against the instantiated signature

### Integration with HKT

For higher-kinded instantiation, the type argument can be:
- A concrete type: `int`, `string`
- A type constructor: `list`, `option`
- A partial application: `(result err)` (for `* -> *` where result is `* -> * -> *`)

### Example

```elisp
;; Signature: (defun fmap [f a b] (((a -> b)) (f a)) -> (f b))

;; Full instantiation
(tart [list int string] fmap #'number-to-string my-int-list)
;; f = list, a = int, b = string
;; Expected: ((int -> string)) (list int)) -> (list string)
;; Checks: #'number-to-string : (int -> string), my-int-list : (list int)
;; Returns: (list string)

;; Partial instantiation
(tart [list _ _] fmap #'upcase my-list)
;; f = list, a = inferred, b = inferred
;; From #'upcase : (string -> string), infers a = string, b = string
;; From my-list : (list string), confirms a = string
;; Returns: (list string)
```
