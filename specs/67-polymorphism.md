# Spec 67 — Polymorphism & Quantification

> Consolidates specs: [15](./.archive/15-explicit-forall.md), [17](./.archive/17-higher-kinded-types.md), [18](./.archive/18-explicit-instantiation.md), [19](./.archive/19-scoped-type-variables.md)

## Overview

Tart supports explicit polymorphism through universal quantifiers, a kind
system for higher-kinded types, call-site type instantiation via the `tart`
macro, and scoped type variables for sharing quantified names across related
signatures. Together these features enable first-class abstraction over both
value types and type constructors.

## Explicit Quantification

### Syntax

All polymorphic signatures require an explicit `[vars]` quantifier. Without
one, lowercase names resolve as type constructor references, not type
variables:

```elisp
;; 'a' and 'b' are type constructor references, not variables
(defun seq-map (((a -> b)) (seq a)) -> (list b))

;; Polymorphic version requires explicit quantifier
(defun seq-map [a b] (((a -> b)) (seq a)) -> (list b))
```

There is no implicit quantification. This avoids ambiguity with
lowercase-named type constructors.

### Unbound variable errors

Any type variable that appears in the body of a signature but is not listed
in the `[vars]` binder produces a parse error:

```elisp
(defun foo [a] ((a -> b)) -> a)  ; Error: unbound type variable 'b'
```

### Literals vs type references

Quoted symbols are literal types; unquoted lowercase names are type
references:

```elisp
(type status ('pending | 'complete | a))  ; 'pending is literal, a is type ref
```

## Kind System

Kinds classify types the way types classify values.

### Kind representation

| Kind             | Meaning                                              |
| ---------------- | ---------------------------------------------------- |
| `*`              | Concrete type (`int`, `string`, `(list int)`)        |
| `* -> *`         | One-parameter type constructor (`list`, `option`)    |
| `* -> * -> *`    | Two-parameter type constructor (`hash-table`, `result`) |

Internally, kinds are represented as:

```ocaml
type kind =
  | KStar                (* concrete type *)
  | KArrow of kind * kind (* type constructor *)
```

### Kind inference

Kinds are inferred from usage patterns. When a type variable is applied to
arguments, the inference algorithm deduces its kind:

```elisp
;; f : * -> *, a : *, b : *
(defun fmap [f a b] (((a -> b)) (f a)) -> (f b))
```

The algorithm mirrors type inference:

1. Assign fresh kind variables to each type parameter.
2. Collect kind constraints from type expressions.
3. Unify kind constraints.
4. Default unconstrained kind variables to `*`.

### Kind checking on type applications

When a type application `(f a)` is kind-checked, the checker verifies that
`f` has kind `k1 -> k2` and `a` has kind `k1`, yielding result kind `k2`.
A mismatch produces a kind error:

```elisp
(defun bad [f a] (f a) -> int)
;; If f : * and a : *, then (f a) is a kind error
```

### Explicit kind annotations

Type variable binders accept optional kind annotations to constrain
inference:

```elisp
(defun fmap [(f : (* -> *))] [a b] (((a -> b)) (f a)) -> (f b))
```

### Default kind

Unconstrained type variables default to kind `*`. Existing first-order
signatures are unaffected:

```elisp
(defun identity [a] (a) -> a)  ; a : *
```

### Kind error messages

Kind errors report both expected and found kinds:

```
Error: kind mismatch
  expected: * -> *
  found: *
  in type application: (f a)
```

### Nested and partial type constructor application

Type constructors with multiple parameters support partial application.
A constructor of kind `* -> * -> *` applied to one argument yields kind
`* -> *`:

```elisp
;; result is * -> * -> *, (result e) is * -> *
(defun map-result [e a b] (((a -> b)) (result e a)) -> (result e b))
```

## Explicit Instantiation

The `tart` macro provides call-site type inspection, assertion, and
instantiation. At runtime it expands to just the expression argument.

### Forms

```
(tart EXPR)                 ; inspect type (emits note diagnostic)
(tart TYPE EXPR)            ; type assertion
(tart TYINST EXPR)          ; instantiation
(tart TYPE TYINST EXPR)     ; assertion + instantiation
```

Where `TYINST` is a vector of named bindings: `[(name = type) ...]`.

Disambiguation for arity 2: if the first argument is a vector, it is an
instantiation; otherwise it is a type assertion.

### Type inspection

With a single expression argument, `tart` emits an informational note
showing the inferred type:

```elisp
(tart (mapcar #'1+ my-list))
;; Note: (List Int)
```

### Named bindings

Instantiation uses exclusively named bindings `(name = type)`:

```elisp
;; (defun identity [a] (a) -> a)
(tart [(a = int)] (identity 42))    ; OK: a = int
(tart [(a = string)] (identity 42)) ; Error: int not string
```

Benefits of named-only bindings:

- **Partial specification**: omit parameters that inference can handle.
- **Order independence**: bindings match by name, not position.
- **Self-documenting**: parameter names visible at call sites.
- **Forward compatible**: adding type parameters to a library function does
  not break existing call sites using explicit instantiation.

### Partial instantiation

When only some type parameters are specified, the rest are inferred:

```elisp
;; (defun mapcar [a b] (((a -> b)) (list a)) -> (list b))
(tart [(a = int)] (mapcar #'1+ my-list))  ; b inferred from #'1+
```

### Order independence

Named bindings can appear in any order:

```elisp
;; (defun pair [a b] (a b) -> (cons a b))
(tart [(b = string) (a = int)] (pair 1 "hi"))
;; same as [(a = int) (b = string)]
```

### Higher-kinded instantiation

Type constructor arguments work in bindings:

```elisp
;; (defun fmap [f a b] (((a -> b)) (f a)) -> (f b))
(tart [(f = list) (a = int) (b = string)] (fmap #'number-to-string my-list))
```

Bindings can also hold partial applications: `(f = (result err))` for a
type constructor of kind `* -> *`.

### Unknown parameter errors

A binding that names a parameter not present in the function's quantifier
is an error:

```elisp
;; identity has param 'a', not 'x'
(tart [(x = int)] (identity 42))  ; Error: unknown type parameter 'x'
```

### Runtime expansion

The `tart` macro expands to just the expression at runtime:

```elisp
(macroexpand '(tart (+ 1 2)))
;; => (+ 1 2)

(macroexpand '(tart [(a = int)] (identity 42)))
;; => (identity 42)

(macroexpand '(tart int (+ 1 2)))
;; => (+ 1 2)
```

## Scoped Type Variables

### Purpose

By default each function signature has its own independent type variables.
`type-scope` blocks share type variables across multiple declarations,
enabling patterns like iterators, state machines, and builder APIs where
several functions must agree on a type parameter.

### Syntax

In `.tart` files, a `(type-scope [vars] ...)` block binds type variables
across all enclosed declarations:

```elisp
(type-scope [a]
  (defun iter-next ((iter a)) -> (a | nil))
  (defun iter-peek ((iter a)) -> (a | nil))
  (defun iter-collect ((iter a)) -> (list a)))
```

### Shared variables, independent externals

Variables inside a scope are shared; variables declared outside with the
same name are independent:

```elisp
(type-scope [a]
  (defun get-first ((list a)) -> (a | nil))
  (defun get-last ((list a)) -> (a | nil)))

;; This 'a' is independent
(defun other-fn [a] (a) -> a)
```

### Additional quantifiers inside scope

A function inside a `type-scope` that needs extra type variables declares
them with the usual `[vars]` syntax. These are added to the scope
variables:

```elisp
(type-scope [a]
  ;; Uses scope's 'a' plus local 'b'
  (defun iter-map [b] (((a -> b)) (iter a)) -> (iter b)))
```

### Higher-kinded scoped variables

Scoped variables support kind annotations and participate in kind
inference:

```elisp
(type-scope [(f : (* -> *))]
  (defun fmap-scope [a b] (((a -> b)) (f a)) -> (f b))
  (defun pure-scope [a] (a) -> (f a)))
```

### Nested scopes

Nested `type-scope` blocks shadow outer variables of the same name:

```elisp
(type-scope [a]
  (defun outer ((list a)) -> a)
  (type-scope [a]  ; shadows outer 'a'
    (defun inner ((vector a)) -> a)))
```

### Opaque types in scopes

Scoped variables can appear alongside opaque type declarations. The
variable acts as a phantom type -- the scope provides documentation but no
structural connection:

```elisp
(type-scope [a]
  (type iter)  ; opaque, but conceptually holds 'a'
  (defun make-iter ((list a)) -> iter)
  (defun iter-next (iter) -> (a | nil)))
```

### Exported types

When a `type-scope` appears at the top level of a `.tart` file, all
contained declarations are exported with the scope variables universally
quantified:

```elisp
;; iter.tart
(type-scope [a]
  (type iter)
  (defun make-iter ((list a)) -> iter)
  (defun iter-next (iter) -> (a | nil)))

;; When loaded, iter-next has type: [a] (iter) -> (a | nil)
```

### Unbound variables in scopes

Using a type variable inside a `type-scope` that is neither declared in the
scope binder nor in a local `[vars]` quantifier produces an error:

```elisp
(type-scope [a]
  (defun bad (b) -> b))  ; Error: unbound type variable 'b'
```

## Implementation

Key modules:

| Module                                                                         | Role                                          |
| ------------------------------------------------------------------------------ | --------------------------------------------- |
| [`lib/typing/kind.mli`](../lib/typing/kind.mli)                               | Kind representation and kind environments     |
| [`lib/typing/kind_infer.mli`](../lib/typing/kind_infer.mli)                   | Kind inference algorithm                       |
| [`lib/sig/sig_parser.ml`](../lib/sig/sig_parser.ml)                           | Parses `[vars]`, kind annotations, `type-scope` |
| [`lib/sig/sig_loader.ml`](../lib/sig/sig_loader.ml)                           | Loads signatures with scope and kind contexts  |
| [`lib/sig/sig_ast.mli`](../lib/sig/sig_ast.mli)                               | AST nodes: `DTypeScope`, `tvar_binder`        |
| [`lib/typing/infer.ml`](../lib/typing/infer.ml)                               | Explicit instantiation (`tart` forms)         |
| [`lisp/tart.el`](../lisp/tart.el)                                             | `tart` macro (runtime expansion)              |

## Deferred

No items currently deferred.
