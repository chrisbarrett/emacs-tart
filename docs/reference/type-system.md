# Type System Reference

Complete reference for tart's type system: what types exist, how they relate,
and how the checker uses them.

## Overview

Tart implements a Hindley-Milner type system with extensions for union types,
occurrence typing, row polymorphism, and literal types. Types are inferred
automatically from Elisp code and checked against signatures declared in
[`.tart` files][tart-format].

The type system is _gradual_: typed modules are fully checked, while calls to
untyped code are assumed correct. There are no runtime contracts — all checking
happens statically.

## Primitive Types

These are built-in to the type checker and bridged to user-facing names in the
[prelude][prelude].

| Type      | Description                | Truthy? |
| --------- | -------------------------- | ------- |
| `int`     | Integers                   | Yes     |
| `float`   | Floating-point numbers     | Yes     |
| `num`     | Numeric (`int` or `float`) | Yes     |
| `string`  | Strings                    | Yes     |
| `symbol`  | Symbols                    | Yes     |
| `keyword` | Keywords (`:foo`)          | Yes     |
| `nil`     | Only `nil`                 | No      |
| `t`       | Only `t`                   | Yes     |
| `truthy`  | Anything except `nil`      | Yes     |
| `never`   | Bottom type (errors)       | N/A     |

`nil` and `truthy` are the two top-types. `any` is their union. `never` is the
bottom type — it inhabits every type and represents expressions that never
return (e.g., `(error ...)`).

## Literal Types and Deferred Widening

Every literal value receives a _literal type_ that carries its precise value:

```elisp
42       ; type: 42   (not int)
"hello"  ; type: "hello" (not string)
'foo     ; type: 'foo (not symbol)
:bar     ; type: :bar (not keyword)
```

Literal types form a subtyping chain with their parent:

```
42 <: int <: num
"hello" <: string
'foo <: symbol
:bar <: keyword
```

Widening to the parent type happens _only_ when context demands it during
unification. This preserves precise types as long as possible and enables
discriminated unions over literal values:

```elisp
(type status (:pending | :complete | :failed))
```

## Container Types

Container types are declared in the [prelude][prelude] as aliases for compiler
intrinsics.

| Type               | Description                             |
| ------------------ | --------------------------------------- |
| `(list a)`         | Homogeneous list                        |
| `(vector a)`       | Homogeneous vector                      |
| `(cons a b)`       | Cons cell                               |
| `(hash-table k v)` | Hash table                              |
| `(alist k v)`      | Association list = `(list (cons k v))`  |
| `(plist k v)`      | Property list (flat key-value sequence) |
| `(map r)`          | Generic map supertype                   |
| `(record tag)`     | Typed vector with tag in slot 0         |
| `(fn a b)`         | Shorthand for `(a -> b)`                |

`nil` is a valid `(list a)` for any `a` — it represents the empty list. This
means `nil <: (list a)` holds universally, reflecting Elisp's convention that
`nil` and `'()` are the same value.

## Derived Types (Prelude)

These types are defined in the [prelude][prelude] using primitives and container
types. They are available in all `.tart` files without an explicit `open`.

| Type       | Definition                                        | Description                   |
| ---------- | ------------------------------------------------- | ----------------------------- |
| `any`      | `(truthy \| nil)`                                 | Universal type                |
| `bool`     | `(t \| nil)`                                      | Boolean                       |
| `option`   | `[(a : truthy)] (a \| nil)`                       | Optional — `a` must be truthy |
| `is`       | `[a] (a - nil)`                                   | Remove `nil` from a union     |
| `nonempty` | `[a] (is (list a))`                               | Non-empty list                |
| `eq-safe`  | `(symbol \| keyword \| int \| t \| nil)`          | Safe for `eq` comparison      |
| `eql-safe` | `(symbol \| keyword \| int \| float \| t \| nil)` | Safe for `eql` comparison     |

The `option` type requires its argument to be `truthy`. This prevents nested
optionals like `(option (option x))`, which would make `nil` ambiguous — is it
"no outer value" or "outer value is nil"?

```elisp
(option string)          ; valid — string is truthy
(option int)             ; valid — int is truthy
(option nil)             ; INVALID — nil is not truthy
(option bool)            ; INVALID — bool includes nil
(option (option string)) ; INVALID — option includes nil
```

### Opaque Emacs Types

The prelude also declares opaque types for core Emacs data structures:

`buffer`, `window`, `frame`, `marker`, `overlay`, `process`, `terminal`,
`finalizer`, `bool-vector`, `char-table`

These only unify with themselves — there is no special type-checker behavior
associated with them.

## Union Types

Union types represent values that belong to one of several types.

```elisp
(string | nil)                ; string or nil
(int | string | nil)          ; multiple alternatives
(:ok | :error | :pending)     ; discriminated union over literals
```

Unions must be parenthesized. The `|` operator binds tighter than `->` in
function types:

```elisp
(defun foo (int) -> (string | nil))    ; returns a union
(defun bar ((string | nil)) -> int)    ; takes a union parameter
```

## Type Subtraction

The `(a - b)` operator removes `b` from a union type.

```elisp
((int | string) - int)            ; => string
((truthy | nil) - nil)            ; => truthy
((string | int | symbol) - string); => (int | symbol)
```

When the subtrahend is itself a union, each of its members is removed from the
minuend. If the result is empty, the type becomes `never`.

Type subtraction is used internally by the `is` type alias and by occurrence
typing to narrow unions in conditional branches.

## Subtyping

Tart uses a structural subtyping lattice.

### Lattice structure

```
         any = (truthy | nil)
        /                    \
   truthy                    nil
   /  |  \                    |
 int  string  symbol  ...   (list a) ← nil is the empty list
  |
 num = (int | float)
  |
never (bottom)
```

### Key subtyping rules

- `never <: T` for all types (bottom)
- `T <: any` for all types (top)
- `nil <: (list a)` for all `a` (nil is the empty list)
- Literal types are subtypes of their base: `42 <: int`, `"hi" <: string`,
  `'foo <: symbol`, `:bar <: keyword`
- `int <: num` and `float <: num`
- Union subtyping: `(a | b) <: T` when both `a <: T` and `b <: T`
- Tuple-to-list: `TTuple [t1...tn] <: (list a)` when every `ti <: a`

## Tuple Types

When `list` is called with arguments of different types, the result is a _tuple_
— a fixed-length heterogeneous sequence:

```elisp
(list 1 2 3)           ; => (list int)  — homogeneous, regular list type
(list 'setq x 1)      ; => tuple (symbol, any, int) — heterogeneous
```

Tuple types support precise element access with literal indices:

```elisp
(nth 0 (list 'setq 42))  ; => symbol (precise first element)
(nth 1 (list 'setq 42))  ; => int    (precise second element)
(nth 5 (list 'setq 42))  ; => nil    (out of bounds)
```

With a non-literal index, `nth` returns the union of all element types plus
`nil`:

```elisp
(defun f (n)
  (nth n (list 'setq 42)))  ; => (symbol | int | nil)
```

Tuples widen to homogeneous lists when all element types share a common
supertype:

```elisp
;; (tuple int int) <: (list int)  — OK
;; (tuple string int) <: (list int)  — ERROR, string is not int
```

## Record Types

The `(record tag)` type represents typed vectors created by `cl-defstruct`. The
`tag` is a phantom type parameter — it distinguishes struct types without
exposing field layout.

### defstruct declarations

A `defstruct` in a `.tart` file generates constructor, predicate, and accessor
signatures:

```elisp
(defstruct person
  (name string)
  (age int))
```

This generates:

```elisp
(defun make-person (string int) -> (record person))
(defun person-p (((record person)) -> t) ((_) -> nil))
(defun person-name ((record person)) -> string)
(defun person-age ((record person)) -> int)
```

The `:keyword-constructor` option generates a keyword-argument constructor:

```elisp
(defstruct person :keyword-constructor
  (name string)
  (age int))

;; Generates:
(defun make-person (&key (:name string) (:age int)) -> (record person))
```

## Row Polymorphism

Row types enable structural typing for map-like containers (alists, plists, hash
tables).

### Closed rows

A closed row declares exactly the fields present:

```elisp
{name string age int}     ; exactly name and age, nothing else
```

### Open rows

An open row declares a minimum set of fields with a row variable for the rest:

```elisp
{name string & r}         ; at least name, r captures additional fields
```

### Row-typed containers

Row types work with all map-like containers through the `(map r)` supertype:

```elisp
;; Accept any alist with at least a 'name' field
(defun get-name [r] ((alist {name string & r})) -> string)

;; Accept any map (alist, plist, or hash-table) with a name field
(defun get-name [r] ((map {name string & r})) -> string)
```

The `alist`, `plist`, and `hash-table` types all unify with `(map r)`, allowing
functions that work generically across map representations.

### Field access

When accessing a field that exists in the row, the checker returns its precise
type:

```elisp
(defun get-person-name (person)
  (declare (tart [r] ((alist {name string & r})) -> string))
  (alist-get 'name person))  ; => string
```

## Function Types

Function types use infix `->` with parenthesized parameters. Elisp functions are
not curried — all parameters are grouped together.

```elisp
(int) -> int                          ; one parameter
(int int) -> int                      ; two parameters
(string &optional int) -> string      ; optional parameter
(&rest string) -> string              ; rest parameter
(&key :name string :age int) -> nil   ; keyword parameters
```

### Parameter kinds

| Syntax            | Meaning                       |
| ----------------- | ----------------------------- |
| `type`            | Required positional parameter |
| `&optional type`  | Optional (caller may omit)    |
| `&rest type`      | Rest args (element type)      |
| `&key :name type` | Keyword argument              |

### Subtyping

Function types are **contravariant** in parameters and **covariant** in return
types:

```elisp
;; (any -> nil) <: (string -> nil)
;; because string <: any (parameter is contravariant)
```

A function with positional parameters is a subtype of a rest-parameter function
when all positional types are subtypes of the rest element type:

```elisp
;; (int int) -> r <: (&rest int) -> r
;; because each int <: int
```

## Polymorphism

### Explicit quantification

All polymorphic signatures require an explicit `[vars]` quantifier. A symbol is
a type variable if and only if it appears in a quantifier:

```elisp
(defun identity [a] (a) -> a)
(defun map [a b] (((a -> b)) (list a)) -> (list b))

;; Error: unbound type variable 'a'
(defun bad (a) -> a)
```

### Bounded quantification

Type variable bounds constrain instantiation to subtypes of a given type:

```elisp
(type option [(a : truthy)] (a | nil))
(defun unwrap-or [(a : truthy)] ((a | nil) a) -> a)
```

An unbounded variable `[a]` can be instantiated to any type. A bounded variable
`[(a : truthy)]` can only be instantiated to subtypes of `truthy`.

Bounded quantification also arises from rest-parameter inference: when a
function has `&rest (T1 | T2 | ...)` and a caller passes a parameter, the solver
infers an upper-bound constraint `a <: (T1 | T2 | ...)` rather than an equality
constraint. This allows callers to pass any subtype without acquiring the full
union.

### forall scoped type variables

By default, each function signature has independent type variables. A `forall`
block in a `.tart` file shares type variables across multiple declarations:

```elisp
(forall [a]
  (defun iter-next ((iter a)) -> (a | nil))
  (defun iter-peek ((iter a)) -> (a | nil))
  (defun iter-collect ((iter a)) -> (list a)))
```

Functions inside a `forall` block can declare additional local variables with
the usual `[vars]` syntax:

```elisp
(forall [a]
  (defun iter-map [b] (((a -> b)) (iter a)) -> (iter b)))
```

### Kind system

Kinds classify types the way types classify values:

| Kind          | Meaning                        | Example              |
| ------------- | ------------------------------ | -------------------- |
| `*`           | Concrete type                  | `int`, `(list int)`  |
| `* -> *`      | One-parameter type constructor | `list`, `option`     |
| `* -> * -> *` | Two-parameter type constructor | `hash-table`, `cons` |

Kinds are inferred from usage. Unconstrained type variables default to kind `*`:

```elisp
(defun identity [a] (a) -> a)         ; a : *
(defun fmap [f a b] (((a -> b)) (f a)) -> (f b))  ; f : * -> *
```

## Truthiness Inference

Tart tracks truthiness through control flow. In Elisp, `nil` is the only falsy
value — everything else is truthy.

The checker understands these forms:

- `if`, `cond`, `when`, `unless` — branch on truthiness
- `and` — narrows progressively (each argument sees previous narrowing)
- `or` — returns first truthy value
- `not` — inverts truthiness

```elisp
(defun f (x)
  (declare (tart ((string | int)) -> int))
  (if (stringp x)
      (string-to-char x)   ; x is string here
    (+ x 1)))              ; x is int here
```

## Occurrence Typing

Type predicates narrow types in conditional branches. This works through
multi-clause function signatures.

### Standard predicates

Predicates like `stringp`, `integerp`, `listp`, `consp`, `atom`, `numberp`, and
`symbolp` have multi-clause signatures that enable narrowing:

```elisp
(defun stringp
  ((string) -> t)
  ((_) -> nil))
```

When used in a conditional, the checker narrows the tested variable:

```elisp
(defun f (x)
  (declare (tart ((string | int | symbol)) -> int))
  (cond
   ((stringp x) (string-to-char x))        ; x : string
   ((symbolp x) (string-to-char
                   (symbol-name x)))        ; x : symbol (string subtracted)
   (t (+ x 1))))                            ; x : int
```

### Cumulative narrowing

In `cond` forms, each clause sees the accumulated narrowing from all previous
clauses. If the first clause handles `string`, the second clause knows `x` is
not a `string`.

### Custom predicates

Any function with multi-clause signatures enables occurrence typing. Define your
own:

```elisp
;; In a .tart file:
(defun my-widget-p
  (((record my-widget)) -> t)
  ((_) -> nil))
```

## Feature Guards

`featurep`, `fboundp`, and `boundp` narrow what symbols are available in their
branches.

`(require 'module)` loads the module's `.tart` signatures, making its types
available for subsequent code.

## See Also

- [Signature File Format (.tart)][tart-format] — syntax and grammar for `.tart`
  files
- [Writing Typings][writing-typings] — guide to writing type signatures for
  Elisp modules

[tart-format]: ./tart-format.md
[writing-typings]: ./writing-typings.md
[prelude]: ../../typings/tart-prelude.tart
