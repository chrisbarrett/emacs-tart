# Signature File Format (.tart)

Canonical reference for tart's `.tart` signature file format.

## Overview

`.tart` files declare the **public interface** of Elisp modules. Only what
appears in a `.tart` file is visible to consumers—implementation details in the
`.el` file remain hidden.

Type checking is available for any `.el` file via LSP. When code uses `require`
or autoloaded functions, tart searches for corresponding `.tart` files. A sibling
`foo.tart` declares the public interface of `foo.el` and enables signature
verification.

**Future extension:** Internal types declared in `.el` files will not be exported.
A `.tart` file can expose an opaque view of internal types for abstraction.

## File Structure

```elisp
(module module-name)                       ; Required for search path files

(open 'module)                             ; Import types for use in signatures
(include 'module)                          ; Inline and re-export declarations

(defun name [quantifiers]? params -> ret)  ; Function (callable)
(defvar variable-name type)                ; Variable declaration
(type TypeName type)                       ; Type alias
(opaque TypeName)                          ; Abstract type (hidden representation)
(import-struct name)                       ; Import cl-defstruct
```

## Grammar

```
tart_file    ::= module_decl? directive* declaration*

module_decl  ::= '(module' symbol ')'

directive    ::= open_decl | include_decl
open_decl    ::= "(open '" symbol ')'
include_decl ::= "(include '" symbol ')'

declaration  ::= func_decl | var_decl | type_alias | opaque_decl | struct_import

func_decl    ::= '(defun' symbol ('[' tvar+ ']')? params '->' type ')'
var_decl     ::= '(defvar' symbol type ')'
type_alias   ::= '(type' symbol type ')'
opaque_decl  ::= '(opaque' symbol ('(' tvar* ')')? ')'
struct_import ::= '(import-struct' symbol ')'

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

Use `type` to define type aliases, including union types:

```elisp
(type IntList (List Int))
(type StringOption (Option String))
(type Callback (String -> Nil))
(type Result (Or Ok Err))             ; Union type
```

### Opaque Types

Abstract types with hidden representation. Consumers can pass values around
but cannot inspect or construct them except via declared functions.

```elisp
;; buffer-manager.tart
(module buffer-manager)

(opaque Buffer)              ; Monomorphic opaque type
(opaque Handle (a))          ; Polymorphic opaque type

(defun buffer-create String -> Buffer)
(defun buffer-name Buffer -> String)
(defun buffer-kill Buffer -> Nil)

(defun handle-wrap [a] a -> (Handle a))
(defun handle-unwrap [a] (Handle a) -> a)
```

Use opaque types for:
- Wrapping external resources (buffers, processes, windows)
- Enforcing API boundaries (consumers must use your functions)
- Hiding implementation details that may change

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

### Open (Import Types)

`(open 'module)` brings type names from another module into scope for use in
type expressions. The imported names are not re-exported.

```elisp
;; my-collection.tart
(module my-collection)
(open 'seq)  ; Seqable now available for use in signatures

(defun my-flatten [a] (Seqable (Seqable a)) -> (List a))
(defun my-frequencies [a] (Seqable a) -> (HashTable a Int))
```

Use `open` when you need to reference types from other modules in your signatures.

### Include (Re-export)

`(include 'module)` inlines all declarations from another `.tart` file, making
them part of this module's interface.

```elisp
;; my-extended-seq.tart
(module my-extended-seq)
(include 'seq)  ; Re-export everything from seq

;; Plus additional functions
(defun seq-partition [a] (Int (Seqable a)) -> (List (List a)))
(defun seq-interleave [a] ((Seqable a) (Seqable a)) -> (List a))
```

Use `include` to extend or re-export another module's type interface.

## Complete Example

```elisp
;; my-utils.tart
(module my-utils)

(open 'seq)  ; Import Seqable for use in signatures

;; Type aliases (including union types)
(type IntList (List Int))
(type Result (Or Success Failure))

;; Opaque type
(opaque Handle)

;; Function declarations (directly callable)
(defun my-utils-trim String -> String)
(defun my-utils-split (String String) -> (List String))
(defun my-utils-identity [a] a -> a)

;; Variable (non-function)
(defvar my-utils-default-separator String)

;; Variable holding a function value (requires funcall)
(defvar my-utils-error-handler (String -> Nil))
(defvar my-utils-transform ([a] a -> a))
```

## Signature Search Path

When a module is required, tart searches for `.tart` files in order:

1. **Sibling file**: `module.tart` next to `module.el` (highest priority)
2. **Search path**: Directories in `tart-type-path` (user/community types)
3. **Bundled stdlib**: `stdlib/module.tart` shipped with tart

This allows providing types for any module, including third-party packages
that don't ship their own type definitions.

### Search Path Configuration

```elisp
;; In Emacs config
(setq tart-type-path '("~/.config/emacs/tart/" "/path/to/community-types/"))
```

### Example: Providing Types for External Packages

```
~/.config/emacs/tart/
├── seq.tart              ; Types for seq.el
├── dash.tart             ; Types for dash.el
└── magit-section.tart    ; Types for magit-section.el
```

```elisp
;; ~/.config/emacs/tart/seq.tart
(module seq)

(defun seq-map [a b] ((a -> b) (Seqable a)) -> (List b))
(defun seq-filter [a] ((a -> Bool) (Seqable a)) -> (List a))
(defun seq-reduce [a b] ((b a -> b) b (Seqable a)) -> b)
(defun seq-find [a] ((a -> Bool) (Seqable a)) -> (Option a))
```

Each `.tart` file must have a `(module name)` declaration matching the feature
name used in `(require 'name)`. The first match in the search order wins,
allowing user overrides of bundled types.

## See Also

- [DESIGN.md](../../DESIGN.md) - Type system reference
- [Spec 07](../../specs/07-signature-files.md) - Parser implementation spec
