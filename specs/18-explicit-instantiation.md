# Spec 18: Explicit Type Instantiation

Enable explicit instantiation of polymorphic types at call sites.

**Dependencies:** Spec 15 (Explicit Forall); Spec 17 (HKT) for R4 only

## Goal

Allow programmers to explicitly specify type arguments when calling polymorphic
functions, resolving ambiguity and improving type inference.

## Motivation

Type inference sometimes needs help:

```elisp
;; What type is the empty list?
(mapcar #'1+ '())
```

Explicit instantiation resolves this:

```elisp
;; Clear: input is (list int)
(tart [(a = int)] (mapcar #'1+ '()))
```

Other use cases:
- Disambiguating return types
- Documenting intended types inline
- Forcing specific instantiation for testing
- **Forward compatibility**: library authors can add type parameters without
  breaking downstream code that uses explicit instantiation

## Constraints

- **Backward compatible**: Existing code without annotations works unchanged
- **Partial specification**: Specify only what you need, rest inferred
- **Order independent**: Named bindings can appear in any order
- **Integration**: Works with HKT and explicit forall quantifiers
- **Syntax**: Must be valid Elisp (parsed but ignored at runtime)

## Output

```
tart/
├── lib/
│   └── typing/
│       └── infer.ml          ; Handle [(name = type)...] instantiation
├── lisp/
│   └── tart.el               ; tart macro
└── test/
    └── typing/
        └── infer_test.ml     ; Instantiation tests
```

## Background

### Syntax

The `tart` macro has arity 1, 2, or 3:

```
tyinst = '[' tyassigns* ']'
ty     = t | '(' t ')'
expr   = atom | list

(tart expr)                 ; inspect type (outputs note)
(tart ty expr)              ; type assertion
(tart tyinst expr)          ; instantiation
(tart ty tyinst expr)       ; assertion + instantiation
```

Examples:

```elisp
(tart (+ 1 2))                            ; note: Int
(tart int x)                              ; assert x : int
(tart [(a = int)] (identity 42))          ; instantiate, infer result
(tart int [(a = int)] (identity 42))      ; instantiate + assert result : int
```

The function call remains a single expression, not spread across the macro args.

### Why Named-Only Bindings

We use exclusively named type parameter bindings rather than positional:

```elisp
(tart [(a = int) (b = string)] (mapcar fn xs))
```

Benefits over positional syntax:
- **Partial specification is natural**: omit parameters you don't need to specify
- **Order independent**: don't need to remember parameter order
- **Self-documenting**: parameter names visible at call site
- **Forward compatible**: adding new type params to a function doesn't break
  existing call sites that use explicit instantiation

## Requirements

### R0: Type inspection

**Given** a `tart` form with just an expression
**When** type-checked
**Then** a note diagnostic is emitted showing the inferred type:

```elisp
(tart (mapcar #'1+ my-list))
;; Note: (List Int)
```

Useful for inspecting types without LSP hover. The note is informational, not
an error or warning.

**Verify:** `dune test`

### R1: Named binding syntax

**Given** a polymorphic function call
**When** annotated with `tart` using `(name = type)` bindings
**Then** named parameters are bound, unspecified parameters inferred:

```elisp
;; (defun identity [a] (a) -> a)
(tart [(a = int)] (identity 42))    ; OK: a = int
(tart [(a = string)] (identity 42)) ; Error: int not string
```

**Verify:** `dune test`

### R2: Partial instantiation

**Given** a function with multiple type parameters
**When** only some are specified
**Then** unspecified parameters are inferred:

```elisp
;; (defun mapcar [a b] (((a -> b)) (list a)) -> (list b))
(tart [(a = int)] (mapcar #'1+ my-list))  ; b inferred from #'1+
```

**Verify:** `dune test`

### R3: Order independence

**Given** named bindings in any order
**When** type-checked
**Then** bindings are matched by name, not position:

```elisp
;; (defun pair [a b] (a b) -> (cons a b))
(tart [(b = string) (a = int)] (pair 1 "hi"))  ; same as [(a = int) (b = string)]
```

**Verify:** `dune test`

### R4: Higher-kinded instantiation (future)

**Given** an HK-polymorphic function (requires Spec 17)
**When** instantiated with a type constructor
**Then** the constructor is applied:

```elisp
;; Hypothetical HKT signature:
;; (defun fmap [f a b] (((a -> b)) (f a)) -> (f b))
(tart [(f = list) (a = int) (b = string)] (fmap #'number-to-string my-list))
```

**Verify:** `dune test` (after Spec 17 implemented)

### R5: Unknown parameter error

**Given** a named binding that doesn't match any type parameter
**When** type-checked
**Then** an error is reported:

```elisp
;; identity has param 'a', not 'x'
(tart [(x = int)] (identity 42))  ; Error: unknown type parameter 'x'
```

**Verify:** `dune test`

### R6: Runtime expansion

**Given** any `tart` form
**When** evaluated at runtime
**Then** it expands to just the expression:

```elisp
(macroexpand '(tart (+ 1 2)))
;; => (+ 1 2)

(macroexpand '(tart [(a = int)] (identity 42)))
;; => (identity 42)

(macroexpand '(tart int (+ 1 2)))
;; => (+ 1 2)
```

**Verify:** ERT tests; macro expands correctly

### R7: Error messages

**Given** a type mismatch with explicit instantiation
**When** reported
**Then** message references the explicit types:

```
Error: type mismatch
  expected: String (from tart instantiation)
  found: Int
```

**Verify:** `dune test`

## Non-Requirements

- Positional type arguments (use named bindings instead)
- Instantiation at definition sites (only call sites)
- Instantiation for non-function types
- Inference from tart to surrounding context

## Tasks

- [x] [R6] Add `tart` macro to tart.el
- [x] [R7] Format error messages with annotation context
- [ ] [R0] Emit note diagnostic for arity-1 `(tart expr)`
- [ ] [R1] Parse `(name = type)` pairs in vector
- [ ] [R1] Match named bindings to type parameters by name
- [ ] [R2] Infer unspecified type parameters
- [ ] [R3] Test order independence
- [ ] [R4] Test HK type constructor instantiation
- [ ] [R5] Error on unknown type parameter names

## Design Notes

### Parsing

The `tart` macro has one, two, or three arguments:

```elisp
(tart EXPR)                ; inspect type (arity 1)
(tart TYINST EXPR)         ; instantiation (arity 2)
(tart TYPE EXPR)           ; assertion (arity 2)
(tart TYPE TYINST EXPR)    ; both (arity 3)
```

Where `TYINST` is a vector of named bindings: `[(n1 = T1) (n2 = T2) ...]`.
Each binding is a 3-element list: `(symbol = type)`.

Disambiguation for arity 2:
- If first argument is a vector `[...]`, it's instantiation
- Otherwise it's type assertion

### Instantiation Algorithm

When a `tart` instantiation form `(tart TYINST EXPR)` is encountered:

1. Evaluate EXPR to find the function being called
2. Look up the function's polymorphic type and its type parameter names
3. Parse the named bindings from the vector
4. For each binding `(name = type)`:
   - Verify `name` is a valid type parameter
   - Parse `type` using sig_parser
5. Create fresh type variables for unspecified parameters
6. Substitute all type variables in the function type
7. Type-check EXPR against the instantiated signature

### Integration with HKT

For higher-kinded instantiation, the type in a binding can be:
- A concrete type: `(a = int)`, `(b = string)`
- A type constructor: `(f = list)`, `(f = option)`
- A partial application: `(f = (result err))` (for `* -> *`)

### Example

```elisp
;; Signature: (defun mapcar [a b] (((a -> b)) (list a)) -> (list b))

;; Inspect type without assertion (outputs note)
(tart (mapcar #'upcase my-list))
;; Note: (List String)

;; Specify only what inference can't determine
(tart [(a = string)] (mapcar #'upcase my-list))
;; a = string, b inferred from upcase

;; Full specification when needed
(tart [(a = int) (b = string)] (mapcar #'number-to-string my-list))

;; Order doesn't matter
(tart [(b = string) (a = int)] (mapcar #'number-to-string my-list))

;; Empty vector = pure inference (same as no tart wrapper)
(tart [] (mapcar #'upcase my-list))

;; Combined: instantiation + result type assertion
(tart (list string) [(a = int)] (mapcar #'number-to-string nums))
```

### Forward Compatibility

Named bindings enable library evolution. If a library adds a new type parameter:

```elisp
;; v1: (defun process [a] ...)
;; v2: (defun process [a opts] ...)  ; new param added
```

Existing call sites with explicit instantiation continue to work:

```elisp
(tart [(a = int)] (process x))  ; works in both v1 and v2
```

With positional syntax, this would break: `(tart [int] (process x))` would fail
in v2 due to arity mismatch.
