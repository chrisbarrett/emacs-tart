# Signature File Format (.eli)

Canonical reference for tart's `.eli` signature file format.

## Overview

`.eli` files declare types for Elisp modules. Presence of `foo.eli` triggers
type checking of `foo.el`.

## File Structure

```elisp
(module module-name)              ; Optional, defaults to filename

(require/typed module             ; Import types from untyped code
  (defun name [quantifiers]? params -> ret)
  ...)

(defun name [quantifiers]? params -> ret)  ; Function (callable)
(defvar variable-name type)                ; Variable declaration
(deftype TypeName type)                    ; Type alias
(data TypeName (params) ...)               ; Algebraic data type
(import-struct name)                       ; Import cl-defstruct
```

## Grammar

```
eli_file     ::= module_decl? require_typed* declaration*

module_decl  ::= '(module' symbol ')'

require_typed ::= '(require/typed' symbol defun* ')'

declaration  ::= func_decl | var_decl | type_alias | adt_decl | struct_import

func_decl    ::= '(defun' symbol ('[' tvar+ ']')? params '->' type ')'
var_decl     ::= '(defvar' symbol type ')'
type_alias   ::= '(deftype' symbol type ')'
struct_import ::= '(import-struct' symbol ')'

adt_decl     ::= '(data' symbol '(' tvar* ')' variant+ ')'
variant      ::= '(' constructor type* ')'

params       ::= type | '(' type+ ')'     ; single or grouped
arrow_type   ::= '(' ('[' tvar+ ']')? params '->' type ')'
```

## Lisp-2 Calling Convention

Elisp separates functions and values into different namespaces:

| Declaration | Calling convention | Example |
|-------------|-------------------|---------|
| `defun`     | Direct: `(f args...)` | `(defun add (Int Int) -> Int)` |
| `defvar` with `->` | Indirect: `(funcall f args...)` | `(defvar handler (String -> Nil))` |

The `->` is infix, separating parameters from return type. Single params need no
grouping; multiple params require parens.

```elisp
;; Function slot (callable)
(defun add (Int Int) -> Int)         ; multiple params, grouped
(defun identity [a] a -> a)          ; single param, no parens needed

;; Value slot (requires funcall)
(defvar handler (String -> Nil))     ; monomorphic function value
(defvar id-fn ([a] a -> a))          ; polymorphic function value
```

## Type Syntax

### Base Types

| Type      | Description              | Truthy? |
|-----------|--------------------------|---------|
| `Int`     | Integers                 | Yes     |
| `Float`   | Floating-point           | Yes     |
| `Num`     | Numeric                  | Yes     |
| `String`  | Strings                  | Yes     |
| `Symbol`  | Symbols                  | Yes     |
| `Keyword` | Keywords (`:foo`)        | Yes     |
| `Nil`     | Only `nil`               | No      |
| `T`       | Only `t`                 | Yes     |
| `Truthy`  | Anything except `nil`    | Yes     |
| `Bool`    | `T \| Nil`               | No      |
| `Any`     | Top type                 | No      |
| `Never`   | Bottom type (errors)     | Yes     |

### Type Variables

Lowercase identifiers: `a`, `b`, `elem`, `key`, `value`

### Function Types

Elisp functions are **not curried**; parameters are grouped.

Arrow types use infix `->`. Single params need no parens; multiple params require grouping:

```elisp
Int -> Int                             ; Single param
(Int Int) -> Int                       ; Multiple params, grouped
(String &optional Int) -> String       ; Optional parameter
(&rest String) -> String               ; Rest parameter
(&key :name String :age Int) -> Nil    ; Keyword parameters
Int -> (Int -> Int)                    ; Returns function value
```

Nested function types are readable:

```elisp
(a -> b)                               ; Function taking a, returning b
((a -> b) (List a)) -> (List b)        ; First param is a function
```

### Parameter Kinds

| Syntax           | Meaning                           |
|------------------|-----------------------------------|
| `type`           | Required positional               |
| `&optional type` | Optional (caller may omit)        |
| `&rest type`     | Rest args (element type)          |
| `&key :k type`   | Keyword argument                  |

### Polymorphic Types

Quantifiers `[vars]` go at the start of arrow types, or right after the name in `defun`:

```elisp
;; As function values
([a] a -> a)                          ; Polymorphic identity value
([a b] (a b) -> a)                    ; Polymorphic const value

;; As callable functions
(defun identity [a] a -> a)           ; Callable as (identity x)
(defun const [a b] (a b) -> a)        ; Callable as (const x y)
(defun length [a] (List a) -> Int)    ; Callable as (length xs)
```

### Type Constructors

```elisp
(List a)                              ; Homogeneous list
(Vector a)                            ; Homogeneous vector
(Option a)                            ; a | Nil (requires a : Truthy)
(Pair a b)                            ; Cons cell
(Tuple a b c)                         ; Fixed-length tuple
(Or a b)                              ; Union type
(HashTable k v)                       ; Hash table
```

### Option Constraint

`(Option a)` requires `a` to be truthy:

```elisp
(Option String)         ; Valid
(Option Int)            ; Valid
(Option (List a))       ; Valid
(Option Nil)            ; INVALID
(Option Bool)           ; INVALID
(Option (Option a))     ; INVALID
```

## Declarations

### Function Declarations

Functions declared with `defun` are directly callable as `(name args...)`:

```elisp
(defun my-add (Int Int) -> Int)
(defun my-identity [a] a -> a)
(defun my-map [a b] ((a -> b) (List a)) -> (List b))
```

Note: The first parameter to `my-map` is an arrow type `(a -> b)`, meaning it's
a function **value** that must be called with `funcall` inside the implementation.

### Variable Declarations

```elisp
(defvar my-default-value String)
(defvar my-counter Int)
```

### Type Aliases

```elisp
(deftype IntList (List Int))
(deftype StringOption (Option String))
(deftype Callback (String -> Nil))
```

### ADT Definitions

```elisp
(data Option (a)
  (Some a)
  (None))

(data Result (a e)
  (Ok a)
  (Err e))

(data Tree (a)
  (Leaf a)
  (Node (Tree a) (Tree a)))
```

ADTs generate:
- Constructors: `Some`, `None`, `Ok`, `Err`
- Predicates: `option-some-p`, `option-none-p`
- Accessors: `option-some-value`, `result-ok-value`

### Struct Imports

```elisp
;; Given (cl-defstruct person name age) in .el file:
(import-struct person)

;; Generates:
;; - Type: person
;; - Constructor: make-person
;; - Predicate: person-p
;; - Accessors: person-name, person-age
```

### Require/Typed

Import types for external untyped modules:

```elisp
(require/typed seq
  (defun seq-map [a b] ((a -> b) (List a)) -> (List b))
  (defun seq-filter [a] ((a -> Bool) (List a)) -> (List a))
  (defun seq-reduce [a b] (((b a) -> b) b (List a)) -> b))

(require/typed subr-x
  (when-let : special-form)
  (if-let : special-form))
```

## Complete Example

```elisp
;; my-utils.eli
(module my-utils)

(require/typed seq
  (defun seq-map [a b] ((a -> b) (List a)) -> (List b)))

;; Type alias
(deftype IntList (List Int))

;; ADT
(data Result (a e)
  (Ok a)
  (Err e))

;; Function declarations (directly callable)
(defun my-utils-trim String -> String)
(defun my-utils-split (String String) -> (List String))
(defun my-utils-identity [a] a -> a)
(defun my-utils-safe-div (Int Int) -> (Result Int String))

;; Variable (non-function)
(defvar my-utils-default-separator String)

;; Variable holding a function value (requires funcall)
(defvar my-utils-error-handler (String -> Nil))
(defvar my-utils-transform ([a] a -> a))
```

## Module Discovery

When type-checking `foo.el`:

1. Look for sibling `foo.eli`
2. Look in `stdlib/` for built-in signatures
3. Follow `require/typed` declarations for external types

## See Also

- [DESIGN.md](../../DESIGN.md) - Type system reference
- [Spec 07](../../specs/07-signature-files.md) - Parser implementation spec
