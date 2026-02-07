# Spec 34: Funcall and Apply Typing

Type `funcall` and `apply` accurately using tracked function types, dual namespaces, and tuple/list subtyping.

**Dependencies:** [Spec 15][] (Explicit Forall), [Spec 17][] (HKT)

## Links

### Deps
[Spec 15]: ./15-explicit-forall.md
[Spec 17]: ./17-higher-kinded-types.md

### Blocks
[Spec 46]: ./46-truthiness-unions.md
[Spec 52]: ./52-type-predicates.md

### Related
[Spec 48]: ./48-prelude.md

## Context

`funcall` and `apply` are fundamental Lisp evaluation primitives. Without special handling, they lose all type information. This spec makes them fully type-safe.

## Constraints

- Lisp-2: separate function and variable namespaces
- Never widen to top type during unification
- Tuple/list interop via subtyping
- Support union function types for dynamic dispatch

## Requirements

### R1: Dual namespace environment

**Given** the type environment
**When** bindings are added
**Then** function bindings (`defun`, `defalias`) and variable bindings (`let`, `setq`, `defvar`) are stored separately

```elisp
(defun foo (x) (+ x 1))   ; foo in function namespace: (-> (Int) Int)
(setq foo "hello")        ; foo in variable namespace: String
```

**Verify:** `dune test`; same name can have different types in each namespace

### R2: Quoted symbol function lookup

**Given** `#'name` or `'name` passed to funcall/apply
**When** type-checked
**Then** looks up `name` in function namespace, returns its function type

```elisp
(defun add1 (x) (+ x 1))
#'add1                    ; type: (-> (Int) Int)
'add1                     ; same type—both resolve via function namespace
```

Sharp-quote and regular quote are equivalent for type-checking; the difference
is a byte-compiler hint. Regular quote in funcall/apply position triggers a
style warning but not a type error.

**Verify:** `dune test`; both `#'name` and `'name` return function type

### R3: Variable reference lookup

**Given** `name` in expression position (not function position)
**When** type-checked
**Then** looks up `name` in variable namespace

```elisp
(setq f (lambda (x) (+ x 1)))
f                         ; type: (-> (Int) Int) from variable env
```

**Verify:** `dune test`; variable references use variable namespace

### R4: Funcall type checking

**Given** `(funcall f arg1 arg2 ...)`
**When** type-checked
**Then** infers type of `f`, verifies it's a function type matching `(arg1 arg2 ...) -> r`, returns `r`

```elisp
(defun call-it (f x)
  (funcall f x))
;; With signature: (forall (a b) (-> ((-> (a) b) a) b))
;; f : (-> (a) b), x : a, result : b
```

**Verify:** `dune test`; funcall checks function type against arguments

### R5: Funcall with quoted symbol

**Given** `(funcall #'name args...)` or `(funcall 'name args...)`
**When** type-checked
**Then** looks up `name` in function namespace, checks args against its type

```elisp
(funcall #'+ 1 2 3)       ; + : (-> (&rest Int) Int), result: Int
(funcall '+ 1 2 3)        ; equivalent—same lookup, same type
(funcall #'cons 1 '(2))   ; cons : (-> (a (List a)) (List a)), result: (List Int)
```

Sharp-quote (`#'`) and regular quote (`'`) are semantically equivalent at
runtime. Sharp-quote is a hint to the byte-compiler. When a regular-quoted
symbol is passed to `funcall` or `apply`, emit a style warning recommending
sharp-quote, but type-check identically.

**Verify:** `dune test`; both #' and ' use function namespace; ' emits warning

### R6: Funcall type errors

**Given** `(funcall f args...)` where `f` is not a function type or args don't match
**When** type-checked
**Then** produces descriptive error

```elisp
(funcall "not a function" 1)  ; Error: expected function type, got String
(funcall #'+ "x")             ; Error: expected Int, got String
```

**Verify:** `dune test`; type errors are caught and reported

### R7: Apply with rest-arg functions

**Given** `(apply f args... list)`
**When** `f` has `&rest` parameter type
**Then** fixed args and list elements must match rest element type

```elisp
(apply #'+ '(1 2 3))          ; + : (-> (&rest Int) Int), list: (List Int) ✓
(apply #'+ 1 2 '(3 4 5))      ; fixed args Int, list (List Int) ✓
(apply #'list 'a 'b '(c d))   ; all Symbol ✓
```

**Verify:** `dune test`; apply with &rest functions checks element types

### R8: Apply with fixed-arity functions (tuple)

**Given** `(apply f args... tuple)`
**When** `f` has fixed arity and `tuple` is a tuple type
**Then** combined arity of fixed args + tuple must match, types must align

```elisp
(apply #'cons '(1 (2 3)))
;; cons : (-> (a (List a)) (List a))
;; '(1 (2 3)) : (Tuple Int (List Int))
;; a = Int, result: (List Int) ✓

(apply #'cons '(1 2))         ; Error: expected (List a), got Int
```

**Verify:** `dune test`; apply with tuples checks arity and positional types

### R9: Tuple-list subtyping

**Given** a tuple type `(Tuple T1 T2 ... Tn)`
**When** unified with `(List T)` where all Ti unify with T
**Then** unification succeeds (tuple is subtype of list)

```elisp
(defun sum-list (xs)          ; xs : (List Int)
  (apply #'+ xs))

(sum-list '(1 2 3))           ; '(1 2 3) : (Tuple Int Int Int) <: (List Int) ✓
```

**Verify:** `dune test`; tuples unify with compatible list types

### R10: List literal tuple inference

**Given** a quoted list literal in apply position
**When** the function has fixed arity
**Then** infer as tuple type with per-element types

```elisp
(apply #'cons '(1 (2 3)))     ; '(1 (2 3)) inferred as (Tuple Int (List Int))
```

**Verify:** `dune test`; list literals infer as tuples in apply context

### R11: Union function types (dynamic dispatch)

**Given** `(funcall f args...)` where `f` has union function type
**When** type-checked
**Then** args must satisfy all function variants, result is union of return types

```elisp
(let ((f (if condition #'upcase #'symbol-name)))
  ;; f : (Or (-> (String) String) (-> (Symbol) String))
  ;; For funcall to work, need to pass value matching both
  ;; In this case, types are incompatible - error
  (funcall f x))

(let ((f (if condition #'1+ #'1-)))
  ;; f : (Or (-> (Int) Int) (-> (Int) Int)) = (-> (Int) Int)
  (funcall f 5))              ; works, result: Int
```

**Verify:** `dune test`; union function types check all variants

### R12: Occurrence typing for predicates

**Given** `(if (pred x) then else)`
**When** `pred` is a type predicate
**Then** narrow `x`'s type in `then` branch

```elisp
(defun safe-length (x)
  (if (stringp x)
      (length x)              ; x : string here
    (if (listp x)
        (length x)            ; x : list here
      0)))
```

**Verify:** `dune test`; type predicates narrow types in branches

### R13: Occurrence typing for funcall

**Given** narrowed types in funcall context
**When** a variable has been narrowed by a predicate
**Then** use the narrowed type for function application

```elisp
(defun process (f x)
  (if (functionp f)
      (funcall f x)           ; f known to be function type
    (error "not a function")))
```

**Verify:** `dune test`; funcall uses narrowed types

### R14: Never widen to top type

**Given** incompatible types during unification
**When** no valid unifier exists
**Then** produce type error rather than silently widening

```elisp
(let ((x (if condition 1 "hello")))
  ;; x : (int | string), NOT widened
  (+ x 1))                    ; Error: (Or Int String) not compatible with Int
```

**Verify:** `dune test`; type mismatches produce errors, not silent widening

## Non-Requirements

- Effect tracking for funcall/apply side effects
- Higher-rank polymorphism in function arguments
- Gradual typing escape hatches

## Anti-Pattern: Do NOT Use `any` Return Types

`funcall` and `apply` must NOT be implemented via signatures like:

```elisp
;; WRONG - loses all type information
(defun funcall (any &rest any) -> any)
(defun apply (any &rest any) -> any)
```

These forms require special-case handling in the type checker that extracts the
return type from the function argument. A signature returning `any` defeats the
purpose of the type system. See [Spec 48][] for general `any` discipline.

## Tasks

- [x] [R1] Add separate function env to type environment
- [x] [R2] Handle quoted symbols in funcall/apply (both #' and ')
- [x] [R3] Ensure variable refs use variable namespace
- [x] [R4] Special-case funcall in type inference
- [x] [R5] Emit style warning for regular-quoted symbols in funcall/apply
- [x] [R6] Add funcall-specific error messages
- [x] [R7] Implement apply for &rest functions
- [x] [R8] Implement apply for fixed-arity with tuples
- [x] [R9] Add tuple <: list subtyping to unification
- [x] [R10] Context-sensitive tuple inference for list literals
- [x] [R11] Handle union function types in funcall/apply
- [x] [R12] Implement occurrence typing for type predicates
- [x] [R13] Thread narrowed types through funcall
- [x] [R14] Ensure unification errors rather than widens
- [x] Remove funcall/apply from eval.tart (no signatures, only type-checker special-casing)
- [x] Add test fixtures for all requirements

**Status:** Complete
