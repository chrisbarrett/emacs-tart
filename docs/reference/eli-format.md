# Signature File Format (.eli)

Canonical reference for tart's `.eli` signature file format.

## Overview

`.eli` files declare types for Elisp modules. Presence of `foo.eli` triggers
type checking of `foo.el`.

## File Structure

```elisp
(module module-name)              ; Optional, defaults to filename

(require/typed module              ; Import types from untyped code
  (symbol : type)
  ...)

(sig function-name type)          ; Function signature
(defvar variable-name type)       ; Variable declaration
(type TypeName = type)            ; Type alias
(data TypeName (params) ...)      ; Algebraic data type
(import-struct name)              ; Import cl-defstruct
```

## Grammar

```
eli_file     ::= module_decl? require_typed* declaration*

module_decl  ::= '(module' symbol ')'

require_typed ::= '(require/typed' symbol import_spec* ')'
import_spec   ::= '(' symbol ':' type ')'

declaration  ::= func_sig | var_sig | type_alias | adt_decl | struct_import

func_sig     ::= '(sig' symbol type ')'
var_sig      ::= '(defvar' symbol type ')'
type_alias   ::= '(type' symbol '=' type ')'
struct_import ::= '(import-struct' symbol ')'

adt_decl     ::= '(data' symbol '(' tvar* ')' variant+ ')'
variant      ::= '(' constructor type* ')'
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

Elisp functions are **not curried**; parameters are grouped:

```elisp
(-> (Int Int) Int)                     ; Two args
(-> () String)                         ; No args
(-> (String &optional Int) String)     ; Optional parameter
(-> (&rest String) String)             ; Rest parameter
(-> (&key :name String :age Int) Nil)  ; Keyword parameters
(-> (a) (-> (b) c))                    ; Returns function
```

### Parameter Kinds

| Syntax           | Meaning                           |
|------------------|-----------------------------------|
| `type`           | Required positional               |
| `&optional type` | Optional (caller may omit)        |
| `&rest type`     | Rest args (element type)          |
| `&key :k type`   | Keyword argument                  |

### Polymorphic Types

```elisp
(forall (a) (-> (a) a))               ; Identity
(forall (a b) (-> (a b) a))           ; Const
(forall (a) (-> ((List a)) Int))      ; List length
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

### Function Signatures

```elisp
(sig my-add (-> (Int Int) Int))
(sig my-identity (forall (a) (-> (a) a)))
(sig my-map (forall (a b) (-> ((-> (a) b) (List a)) (List b))))
```

### Variable Declarations

```elisp
(defvar my-default-value String)
(defvar my-counter Int)
```

### Type Aliases

```elisp
(type IntList = (List Int))
(type StringOption = (Option String))
(type Callback = (-> (String) Nil))
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
  (seq-map : (forall (a b) (-> ((-> (a) b) (List a)) (List b))))
  (seq-filter : (forall (a) (-> ((-> (a) Bool) (List a)) (List a))))
  (seq-reduce : (forall (a b) (-> ((-> (b a) b) b (List a)) b))))

(require/typed subr-x
  (when-let : special-form)
  (if-let : special-form))
```

## Complete Example

```elisp
;; my-utils.eli
(module my-utils)

(require/typed seq
  (seq-map : (forall (a b) (-> ((-> (a) b) (List a)) (List b)))))

;; Type alias
(type IntList = (List Int))

;; ADT
(data Result (a e)
  (Ok a)
  (Err e))

;; Function signatures
(sig my-utils-trim (-> (String) String))
(sig my-utils-split (-> (String String) (List String)))
(sig my-utils-identity (forall (a) (-> (a) a)))
(sig my-utils-safe-div (-> (Int Int) (Result Int String)))

;; Variable
(defvar my-utils-default-separator String)
```

## Module Discovery

When type-checking `foo.el`:

1. Look for sibling `foo.eli`
2. Look in `stdlib/` for built-in signatures
3. Follow `require/typed` declarations for external types

## See Also

- [DESIGN.md](../../DESIGN.md) - Type system reference
- [Spec 07](../../specs/07-signature-files.md) - Parser implementation spec
