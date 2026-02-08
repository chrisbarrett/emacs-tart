# Spec 66 — Type System Core

> Consolidates specs: [11](./.archive/11-adt-system.md), [46](./.archive/46-truthiness-unions.md), [48](./.archive/48-prelude.md)

## Overview

Tart's type system models Elisp's native data idioms directly. Tagged unions
represent sum types via the cons idiom. Row polymorphism types alist, plist,
and hash-table records without introducing foreign representations. Truthiness
is a first-class distinction: `truthy` and `nil` are separate top-types that do
not unify. The prelude bridges compiler intrinsics to ergonomic type names and
provides foundational derived types (`any`, `bool`, `option`, `is`, `nonempty`,
`eq-safe`, `eql-safe`).

## Union Types

### Declaration

Tagged unions use Elisp's cons idiom. A union type declaration names a set of
variants, each a cons cell with a literal symbol tag:

```lisp
(type result [a e]
  ((ok . a) | (err . e)))
```

At runtime, values are plain cons cells -- no special constructors:

```lisp
(cons 'ok value)   ; Creates (ok . value) : (result a _)
(cons 'err msg)    ; Creates (err . msg)  : (result _ e)
```

### Narrowing in pcase

The type checker narrows union types in `pcase` branches. Each pattern binds
variables at the narrowed type:

```lisp
(pcase result
  (`(ok . ,value) (use value))    ; value : a
  (`(err . ,msg) (report msg)))   ; msg : e
```

### Exhaustiveness Checking

Unions are always closed. The type checker verifies that `pcase` branches cover
all variants. A non-exhaustive match produces a warning:

```lisp
(pcase opt
  (`(some . ,x) x))  ; Warning: non-exhaustive match. Missing: nil
```

When the scrutinee type is a subtype of `truthy`, a single branch suffices
because nil is already excluded.

### Closed Unions vs Open Rows

Unions and row types serve different purposes. Unions are closed (exhaustive).
Row types (`{ ... & r }`) apply only to map-like type constructors (`alist`,
`plist`, `hash-table`, `map`), not unions:

```lisp
;; Closed union -- only 'a and 'b are valid
(type my-tag ('a | 'b))

;; Open record -- extra fields permitted
(type named [r] (alist {name string & r}))
```

### Literal Types and Deferred Widening

Literal values receive literal types. Widening to a parent type occurs only at
unification, when a usage context demands it:

```lisp
(let ((x 1) (y 1.0))
  (list x y))
```

Within the `let`, `x` has type `1` and `y` has type `1.0`. At the usage site:

| Context expects | Result type                 |
| --------------- | --------------------------- |
| `(list num)`    | `(list num)` via `1 <: num` |
| must structure  | `(cons 1 (cons 1.0 nil))`   |
| `truthy`        | `truthy`                    |
| unconstrained   | principal type preserved    |

### Type Subtraction

Type subtraction `(a - b)` removes `b` from a union:

```lisp
((int | string) - int)               ; => string
((truthy | nil) - nil)               ; => truthy
(((cons t (list t)) | nil) - nil)    ; => (cons t (list t))
```

If `b` is not a member of `a`, the result is `a` unchanged. If subtraction
would produce an empty type (no inhabitants), the type checker reports an error.

Standard library types built on subtraction:

```lisp
(type is [a] (a - nil))
(type nonempty [a] (is (list a)))   ; equivalent to ((list a) - nil)
```

## Row Polymorphism

### Row Type Syntax

Row types use `{ ... }` syntax. A row variable `& r` makes the row open
(additional fields permitted); omitting it makes the row closed (exactly these
fields):

```lisp
(alist {name string age int})     ; closed -- exactly these fields
(alist {name string & r})         ; open -- at least name
(plist {:name string :age int})   ; closed plist
(plist {:name string & r})        ; open plist
(hash-table {id int & r})         ; open hash-table
```

Homogeneous forms describe uniform key-value containers:

```lisp
(alist symbol int)       ; all keys symbols, all values ints
(plist keyword string)   ; all keys keywords, all values strings
(hash-table string int)  ; all keys strings, all values ints
```

### Row-Typed Map Expansion

When a map-like constructor receives a single row argument instead of a `k v`
pair, the signature loader expands it structurally. The row occupies the value
position of the underlying key-value pair:

```
(alist {name string age int & r})
  expands to -> (list (cons symbol {name string age int & r}))
```

The expanded form preserves the full field-name-to-type mapping. The type
checker extracts the row from the value slot of `(cons symbol TRow)` and uses
`row_lookup` to resolve whether a literal key is present and what its type is.

### Row Unification Rules

Row types unify by matching fields and threading row variables:

```
{name string & r1} ~ {name string age int}
  => r1 = {age int}

{name string & r1} ~ {name string age int & r2}
  => r1 = {age int & r2}

{name string} ~ {name string age int}
  => FAIL (closed row rejects extra fields)
```

### Row-to-Homogeneous Unification

When a row type appears where a plain type is expected (e.g. in the value slot
of a cons pair during unification with a homogeneous alist), unification
succeeds if all field types in the row unify with that plain type:

```
{name string age string} ~ string       => OK (all fields are string)
{name string age int} ~ string          => FAIL (int /~ string)
{name string & r} ~ string              => OK with constraint: r's fields ~ string
```

This enables structural compatibility between `(alist {name string age string})`
and `(alist symbol string)`, since both expand to `(list (cons symbol ...))`.

### Row Type Inference from Field Access

The type checker infers row types from field access patterns:

**Literal keys infer record-style rows:**

```lisp
(defun get-name (x)
  (alist-get 'name x))
;; Inferred: x : (alist {name a & r}), return : a

(defun summarize (person)
  (format "%s is %d" (alist-get 'name person) (alist-get 'age person)))
;; Inferred: person : (alist {name string age int & r})
```

**Variable keys infer homogeneous types:**

```lisp
(defun lookup (m key)
  (alist-get key m))
;; Inferred: m : (alist k v), key : k, return : v
```

### alist-get Return Type Decision Table

`(alist-get KEY ALIST &optional DEFAULT REMOVE TESTFN)` return types depend on
whether KEY is a literal symbol, whether the row contains that field, and
whether the row is open or closed:

| Case | KEY                  | Row    | TESTFN      | Result       | Diagnostic |
| ---- | -------------------- | ------ | ----------- | ------------ | ---------- |
| 1    | literal, in row      | any    | compatible  | field\_type  | --         |
| 2    | literal, in row      | any    | compat+DEF  | field\_type  | --         |
| 3    | literal, NOT in row  | closed | compatible  | nil          | note       |
| 4    | literal, NOT in row  | closed | compat+DEF  | default      | note       |
| 5    | literal, NOT in row  | open   | compatible  | (a \| nil)   | --         |
| 6    | variable key         | any    | compatible  | join \| nil  | --         |
| 7    | any                  | any    | incompat    | --           | error      |

Cases 1--2: the literal key is found in the row, so the field's type is
returned directly (no `| nil`) because the key is provably present. Cases 3--4:
the literal key is absent from a closed row, so it is statically known to be
missing; the checker emits a note. Case 5: absent from an open row, so the key
might exist in the unknown tail. Case 6: variable key prevents determining
which field, so the return is the join of all field value types. Case 7:
incompatible equality predicate for the key type (see [Equality Predicate
Bounds](#equality-predicate-bounds)).

### Generic map Supertype

The `map` type constructor accepts alist, plist, and hash-table subtypes. A
function typed with `map` works uniformly over all map-like containers:

```lisp
(defun get-name (person)
  (declare (tart ((map {name string & r}) -> string)))
  (map-elt person 'name))

;; Works with:
;; ((name . "Alice") (age . 30))                    ; alist
;; (:name "Alice" :age 30)                          ; plist
;; #s(hash-table data (name "Alice" age 30))        ; hash-table
```

### Map Pattern Integration

Standard `pcase` `map` patterns integrate with row-polymorphic types. Extra
fields beyond those matched are permitted by row polymorphism; accessing a field
not present in the type is a type error:

```lisp
(require 'map)

(pcase-let (((map :name (:age a)) person))
  ...)
```

## Truthiness Inference

The type system distinguishes `truthy` and `nil` as separate top-types. This
distinction drives the inference rules for branching forms.

### `or` Typing

- **Truthy first argument short-circuits:** If `a` is a subtype of `truthy`,
  `(or a b)` has type `a`. The second argument is unreachable.
- **Nullable non-final arguments strip nil:** `(or a b)` where `a : (int | nil)`
  and `b : string` has type `(int | string)`. Nil is stripped from `a` because
  if `a` is nil, control falls through to `b`.
- **All nullable preserves nil:** `(or a b)` where both are nullable has type
  `(int | string | nil)`. The final branch's nil is preserved because no
  further fallthrough exists.

### `and` Typing

- **All truthy returns last:** `(and a b)` where both are subtypes of `truthy`
  has type `b`.
- **Nullable argument adds nil:** `(and a b)` where `a : (int | nil)` and
  `b : string` has type `(string | nil)`. Any nullable argument can
  short-circuit to nil.
- **Nil short-circuits:** `(and a b)` where `a : nil` has type `nil`. The
  second argument is unreachable.

### `not` Typing

`not` inverts truthiness:

| Input type     | Result type |
| -------------- | ----------- |
| `truthy`       | `nil`       |
| `nil`          | `t`         |
| `(int \| nil)` | `bool`      |

### `cond` and `if` Typing

`cond` produces the union of all branch types. Without a default clause
(a final `t` test), the result includes `nil` for the implicit fall-through:

```lisp
(cond
  ((< n 0) "negative")
  ((= n 0) 'zero)
  (t n))
;; Type: (string | symbol | int)

(cond
  ((< n 0) "negative")
  ((= n 0) 'zero))
;; Type: (string | symbol | nil)
```

`if` without an else branch adds nil:

```lisp
(if (> n 0) n)   ; Type: (int | nil)
```

### Branch Error Reporting

When a branch type violates a declared return type, the error points to the
offending branch and references the declaration:

```
error[E0308]: branch type incompatible with return type
  --> utils.el:5:7
   |
 5 |       "negative"
   |       ^^^^^^^^^^ this branch has type: String
   |
note: function declared to return Int
  --> utils.el:1:1
   |
 1 | ;; (-> (Int) Int)
   |              ^^^ expected return type
```

## Subtyping Lattice

The subtyping lattice has two distinct top-types. `truthy` and `nil` do not
unify:

```
   truthy              nil
    / | \               |
 num  ...  (cons a b)  ()    <- empty list
 / \
int float
 |    |
 1   1.0   (literal types at bottom)
```

Key relationships:

- `(nonempty a) <: (list a) <: any`
- `(nonempty a) <: truthy`
- `(is (a | nil)) <: a` (when `a <: truthy`)
- `symbol <: eq-safe`
- `keyword <: eq-safe`
- `int <: eq-safe`
- `eq-safe <: eql-safe <: any`
- `(list a) = ((cons a (list a)) | nil)`
- `(nonempty a) = (cons a (list a))`
- `(option a) = (a | nil)` (when `a <: truthy`)
- `bool = (t | nil)`

The type system does not generate `(truthy | nil)` as a unification result. The
`any` type is always explicitly annotated, never inferred.

## Prelude Types

### Implicit Loading

The prelude ([`typings/tart-prelude.tart`](../typings/tart-prelude.tart)) loads
automatically before any other `.tart` file. No `(open ...)` is required. The
load order is:

| File                                       | Defines                                 |
| ------------------------------------------ | --------------------------------------- |
| `typings/tart-prelude.tart`                | userspace names for compiler intrinsics |
| `typings/emacs/{version}/c-core/*.tart`    | data structures, functions, variables   |
| `typings/emacs/{version}/lisp-core/*.tart` | functions, variables                    |

Prelude types are not user-overridable. Redefining any prelude binding is an
error (the general no-shadowing rule).

### Compiler Intrinsics

These types are built into the type checker itself:

| Type      | Description                    |
| --------- | ------------------------------ |
| `truthy`  | Top type for non-nil values    |
| `nil`     | The nil type (singleton)       |
| `never`   | Bottom type (no inhabitants)   |
| `int`     | Integers                       |
| `float`   | Floating-point numbers         |
| `num`     | Supertype of `int` and `float` |
| `string`  | Strings                        |
| `symbol`  | Symbols                        |
| `keyword` | Keywords (`:foo`)              |
| `cons`    | Cons cells                     |
| `\|`      | Union type operator            |
| `-`       | Type subtraction operator      |

Literal types (`1`, `1.0`, `'foo`, `:kw`) are also intrinsics, as subtypes of
their corresponding base types.

### Derived Types

The prelude defines these types in terms of intrinsics:

```lisp
(type t 't)                                ; Elisp's canonical truthy value
(type any (truthy | nil))                  ; universal type
(type bool (t | nil))                      ; Elisp boolean convention
(type list [a] ((cons a (list a)) | nil))  ; homogeneous list (recursive, nullable)
(type option [(a : truthy)] (a | nil))     ; value or nil
(type is [a] (a - nil))                    ; remove nil from a type
(type nonempty [a] (is (list a)))          ; non-empty list
```

The bound `(a : truthy)` on `option` prevents nested optionals:

```lisp
(option int)              ; OK: int <: truthy
(option (int | nil))      ; Error: (int | nil) not <: truthy
(option (option string))  ; Error: (string | nil) not <: truthy
```

Type aliases expand to their definitions during type checking:

```lisp
(list int)       ; expands to ((cons int (list int)) | nil)
(option string)  ; expands to (string | nil)
(nonempty int)   ; expands to (cons int (list int))
```

### Equality Predicate Bounds

Elisp's `eq` tests identity (pointer equality). For immutable, interned types
(symbols, keywords, small integers, `t`, `nil`), identity coincides with
equality. For strings, lists, floats, and other heap-allocated types, `eq` can
return nil for structurally equal values.

The prelude defines `eq-safe` and `eql-safe` as union types bounding which
types can be safely compared by identity:

```lisp
(type eq-safe (symbol | keyword | int | t | nil))
(type eql-safe (symbol | keyword | int | float | t | nil))
```

These types serve as bounds on equality predicate signatures:

```lisp
(defun eq [(a : eq-safe)] (a a) -> bool)
(defun eql [(a : eql-safe)] (a a) -> bool)
(defun equal (any any) -> bool)   ; no bound needed -- structural equality
```

For `alist-get`, the default testfn is `eq`, so the key type must satisfy
`eq-safe`. An explicit TESTFN like `#'equal` bypasses this check:

```lisp
(alist-get 'name person)          ; OK: symbol <: eq-safe
(alist-get "name" person)         ; Error: string not <: eq-safe
(alist-get key person nil nil #'equal)  ; OK: equal accepts any
```

### Discipline: When to Use `any`

The `any` type is a prelude alias for `(truthy | nil)`, not a primitive. Every
use of `any` requires justification.

**Legitimate uses (input positions):**

```lisp
(defun null (any) -> bool)
(defun stringp (any) -> bool)
(defun type-of (any) -> symbol)
```

**Illegitimate uses (output positions):**

```lisp
;; WRONG: funcall's return type depends on the function argument
(defun funcall (any &rest any) -> any)

;; WRONG: length accepts specific sequence types, not any
(defun length (any) -> int)
```

If the possible types can be enumerated, a union is preferred. Only use `any`
when the type is genuinely unknowable at compile time and the value is in input
position for a predicate.

## Deferred

No items currently deferred for this area.
