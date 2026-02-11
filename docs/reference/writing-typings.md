# Writing Type Signatures

Guide to writing `.tart` signature files and inline type declarations for Elisp
code.

## Overview

Tart uses two mechanisms for type information:

1. **Signature files** (`.tart`) declare the public interface of Elisp
   modules--functions, variables, and types visible to consumers.
2. **Inline declarations** in `.el` files provide type information directly in
   source code, useful for local functions and assertions.

This guide covers both approaches with practical examples drawn from the bundled
typings.

## Part 1: Signature Files

### File Structure and Module Naming

The filename determines the module: `foo.tart` provides types for `foo.el`. A
signature file contains directives followed by declarations:

```elisp
;; my-utils.tart

(open 'seq)                              ; import types from another module
(include 'subr)                          ; inline and re-export declarations

(type status (:pending | :complete))     ; type alias
(defun my-utils-greet (string) -> string)  ; function signature
(defvar my-utils-version string)         ; variable declaration
```

Directives (`open`, `include`) appear first. Declarations (`defun`, `defvar`,
`type`, `let-type`, `defstruct`, `forall`) follow.

### `defun` -- Function Signatures

Declare functions that are directly callable as `(name args...)`.

**Single-clause:**

```elisp
(defun my-add (int int) -> int)
(defun identity [a] (a) -> a)
(defun mapcar [a b] (((a -> b)) (list a)) -> (list b))
```

Parameters are always wrapped in parentheses. Type applications within a
parameter group need their own parens--`((list int))` for a single param of type
`(list int)`.

**With type variables:**

```elisp
(defun identity [a] (a) -> a)
(defun unwrap-or [(a : truthy)] ((a | nil) a) -> a)
```

Type variables must be explicitly quantified in `[...]`. Bounded variables like
`[(a : truthy)]` restrict instantiation to subtypes of the bound.

**Parameter kinds:**

```elisp
(defun substring (string &optional (int | nil) (int | nil)) -> string)
(defun concat (&rest (string | symbol | (list int) | (vector int))) -> string)
(defun make-person (&key (:name string) (:age int)) -> (record person))
```

| Syntax            | Meaning                       |
| ----------------- | ----------------------------- |
| `type`            | Required positional parameter |
| `&optional type`  | Optional parameter            |
| `&rest type`      | Rest args (element type)      |
| `&key :name type` | Keyword argument              |

**Arrow type parameters** (function values passed as arguments) need extra
parens:

```elisp
;; First param is a function (a -> b), second is (list a)
(defun mapcar [a b] (((a -> b)) (list a)) -> (list b))

;; First param is a predicate (a -> any)
(defun seq-filter [a] (((a) -> any) ((list a) | (vector a) | string)) -> (list a))
```

**Multi-clause** (for predicates and overloaded functions):

```elisp
(defun stringp
  ((string) -> t)
  ((_) -> nil))

(defun car [a b]
  (((cons a b)) -> a)
  ((nil) -> nil))

(defun atom
  (((cons any any)) -> nil)
  ((_) -> t))
```

Clauses are tried in order. Multi-clause signatures enable type narrowing in
conditionals--see [Multi-Clause Dispatch](#multi-clause-dispatch) below.

### `defvar` -- Variable Declarations

Declare module-level variables:

```elisp
(defvar load-path (list string))
(defvar my-counter int)
```

For variables holding function values, the type includes the arrow:

```elisp
(defvar handler ((string) -> nil))        ; monomorphic, called with funcall
(defvar id-fn ([a] (a) -> a))             ; polymorphic function value
```

The `->` arrow inside a `defvar` type signals that `funcall` is needed to call
it, matching Elisp's Lisp-2 calling convention.

### `type` -- Type Aliases and Opaque Types

The `type` declaration has four forms.

**Opaque type** (no definition--abstract):

```elisp
(type buffer)
(type handle [a])                         ; with phantom parameter
```

Consumers can pass opaque values around but cannot construct or inspect them
except through declared functions.

**Type alias:**

```elisp
(type int-list (list int))
(type callback ((string) -> nil))
(type status (:pending | :complete | :failed))
```

**Parameterized alias:**

```elisp
(type option [(a : truthy)] (a | nil))
(type result [a e] ((ok . a) | (err . e)))
```

**Mutually recursive types:**

```elisp
(type tree   [a] (leaf a | node (forest a))
      forest [a] (list (tree a)))
```

All names in a multi-binding `type` form are mutually visible. Separate `type`
forms retain sequential scoping.

### `let-type` -- Non-Exported Type Aliases

Same syntax as `type` but the alias is not exported. Use `let-type` for internal
abbreviations that simplify your signatures without polluting the module's
public interface:

```elisp
(let-type pair (cons int int))
(let-type wrapper [a] (list a))

;; pair and wrapper are available from here to end of file
(defun make-pair (int int) -> pair)
(defun wrap [a] (a) -> (wrapper a))
```

`let-type` bindings are visible from point of declaration to end of file. They
are not visible through `open` or `include`.

### `defstruct` -- Struct Declarations

Declare struct types with typed fields:

```elisp
(defstruct person
  (name string)
  (age int)
  (email (string | nil)))
```

This generates:

- Type: `(record person)`
- Constructor:
  `(defun make-person (string int (string | nil)) -> (record person))`
- Predicate: `person-p` (multi-clause, enabling type narrowing)
- Accessors: `person-name`, `person-age`, `person-email`

**With keyword constructor:**

```elisp
(defstruct person :keyword-constructor
  (name string)
  (age int))
```

Generates
`(defun make-person (&key (:name string) (:age int)) -> (record person))`.

### `forall` -- Scoped Type Variables

Share type variables across multiple related declarations:

```elisp
(forall [a]
  (defun my-container-new () -> (container a))
  (defun my-container-add ((container a) a) -> (container a))
  (defun my-container-get ((container a)) -> (a | nil)))
```

Each `defun` inside the `forall` block sees the type variable `a`. This is
useful for families of functions that operate on the same parameterized type.

### Multi-Clause Dispatch

Multi-clause functions enable precise type narrowing when used in conditionals.

**Type predicates:**

```elisp
(defun stringp
  ((string) -> t)
  ((_) -> nil))
```

When `(stringp x)` appears in an `if` or `cond`, the type checker narrows `x` to
`string` in the then-branch. The `_` is an inferred type variable (a fresh tvar,
not `any`). Names like `_foo` and `_bar` are also valid wildcards.

**Overloaded return types:**

```elisp
(defun copy-sequence [a]
  (((list a)) -> (list a))
  (((vector a)) -> (vector a))
  ((string) -> string))
```

**Clause diagnostics** attach warnings or notes to specific clauses:

```elisp
(defun eql
  ((eql-safe eql-safe) -> bool)
  ((_ _) -> bool
    (warn "eql compares by identity for non-numbers; use equal for structural comparison")))
```

The `warn` diagnostic is emitted when the second clause matches.

### Row Types for Map Containers

Row types describe the shape of key-value containers (alists, plists,
hash-tables).

**Open rows** with `& r` accept extra fields:

```elisp
(defun get-name [r] ((alist {name string & r})) -> string)
```

This accepts any alist that has at least a `name` key of type `string`,
regardless of what other keys are present:

```elisp
;; Both calls are valid--the open row accepts extra fields
(get-name '((name . "Alice")))
(get-name '((name . "Alice") (age . 30)))
```

**Map supertype** unifies alist, plist, and hash-table:

```elisp
(defun map-elt [v] ((map v) any &optional any) -> (v | nil))
```

### `open` and `include` Directives

**`(open 'module)`** imports types for use in signatures. Imported names are not
re-exported:

```elisp
(open 'seq)
(defun my-flatten [a] ((seq (seq a))) -> (list a))
```

**`(include 'module)`** inlines and re-exports all declarations from another
`.tart` file:

```elisp
(include 'seq)
;; All of seq's declarations are now part of this module's interface
(defun seq-partition [a] (int (seq a)) -> (list (list a)))
```

Shadowing is not permitted--you cannot redefine a name imported by `open` or
`include`.

### Search Path and Module Discovery

When code uses `require` or autoloaded functions, tart searches for `.tart`
files in order:

1. **Sibling file**: `module.tart` next to `module.el` (highest priority)
2. **Bundled typings**: `typings/` directory shipped with tart

A sibling `.tart` file next to your `.el` file takes priority, letting you
provide types for any module. The bundled typings cover Emacs core:

```
typings/
  tart-prelude.tart               # implicit prelude (always loaded)
  emacs/31.0/
    c-core/                       # C primitives (alloc, data, fns, ...)
    lisp-core/                    # Elisp standard library (seq, cl-lib, ...)
```

## Part 2: Inline Type Declarations

Inline declarations live in `.el` files and are recognized by the type checker
at analysis time. They expand to `nil` at runtime, so they have zero cost.

### `(declare (tart ...))` in `defun`

Annotate a function's signature directly in its body:

```elisp
(defun my-add (x y)
  (declare (tart (int int) -> int))
  (+ x y))
```

The `declare` form goes immediately after the parameter list (before any
docstring or body). It follows the same syntax as a single-clause `defun` in a
`.tart` file.

With type variables:

```elisp
(defun my-identity (x)
  (declare (tart [a] (a) -> a))
  x)
```

With row types:

```elisp
(defun get-person-name (person)
  (declare (tart [r] ((alist {name string & r})) -> string))
  (alist-get 'name person))
```

### `(tart TYPE EXPR)` -- Type Assertion

Assert that an expression has a specific type:

```elisp
1              ; 1 :: int
(tart num 1)   ; 1 :: num (widen to num)
```

### `(tart [TYPES] FN ARGS...)` -- Explicit Instantiation

Supply type arguments explicitly when inference is insufficient:

```elisp
(tart [int] identity 42)            ; instantiate identity at int
(tart [_ string] pair 1 "hi")      ; infer first, fix second
```

### `(tart-type NAME DEF)` -- File-Local Type Alias

Define a type alias scoped to the current `.el` file:

```elisp
(tart-type int-pair (cons int int))
(tart-type predicate [a] ((a) -> bool))
```

Expands to `nil` at runtime. Recognized by the type checker for the rest of the
file.

### `(tart-declare NAME TYPE)` -- Variable Type Declaration

Declare the type of a variable:

```elisp
(tart-declare my-buffer buffer)
(defvar my-buffer)
```

Expands to `nil` at runtime. The type checker uses this declaration for all
references to `my-buffer` in the file.

## Part 3: Best Practices

### Be Specific About Return Types

Avoid `any` in output position. Precise return types enable downstream type
checking:

```elisp
;; Bad: caller gets no information
(defun get-name (person) -> any)

;; Good: caller knows the result is a string
(defun get-name [r] ((alist {name string & r})) -> string)
```

### Use Opaque Types to Hide Implementation

When the internal representation might change, use an opaque type:

```elisp
(type handle)
(defun handle-create (string) -> handle)
(defun handle-name (handle) -> string)
```

### Use Multi-Clause Signatures for Predicates

Multi-clause predicates enable type narrowing in conditionals. Always prefer the
`((specific) -> t) ((_) -> nil)` pattern for type predicates:

```elisp
(defun stringp
  ((string) -> t)
  ((_) -> nil))
```

### Type Hook Variables for Arity Checking

Declare hooks as lists of functions to enable arity checking at `add-hook` call
sites:

```elisp
(defvar after-save-hook (list (() -> any)))
(defvar find-file-hook (list (() -> any)))
```

### Use `let-type` for Internal Abbreviations

Keep the public interface clean by using `let-type` for types that only appear
in your own signatures:

```elisp
(let-type coord (cons int int))

(defun distance (coord coord) -> float)
(defun origin () -> coord)
```

### Prefer Bounded Type Variables

When a type variable should not include `nil`, add a truthy bound:

```elisp
(defun unwrap-or [(a : truthy)] ((a | nil) a) -> a)
```

This prevents callers from instantiating `a` to types that include `nil`,
catching bugs at the call site.

## See Also

- [Signature file format][tart-format] -- formal grammar and type syntax
  reference
- [Type system overview][type-system] -- type system concepts and rules

[tart-format]: tart-format.md
[type-system]: type-system.md
