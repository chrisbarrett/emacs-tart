# Signature File Format (.tart)

Canonical reference for tart's `.tart` signature file format.

## Overview

`.tart` files declare the **public interface** of Elisp modules--functions,
values and their types.

Only what appears in a `.tart` file is visible to consumers. This helps you work
with all your implementation details in a type-safe way while being deliberate
about what constitutes the stable 'public' API.

Type checking is available for any `.el` file via LSP. When code uses `require`
or autoloaded functions, tart searches for corresponding `.tart` files. A
sibling `foo.tart` declares the public interface of `foo.el` and enables
signature verification.

## File Structure

```elisp
(open 'module)                             ; Import types for use in signatures
(include 'module)                          ; Inline and re-export declarations

(defun name [vars] params -> ret)          ; Function (callable)
(defvar variable-name type)                ; Variable declaration
(type name)                                ; Opaque type
(type name [vars] def)                     ; Type alias
(import-struct name)                       ; Import cl-defstruct
```

The module name comes from the filename: `foo.tart` provides types for `foo`.

## Grammar

```
tart_file    ::= directive* declaration*

directive    ::= open_decl | include_decl
open_decl    ::= "(open '" symbol ')'
include_decl ::= "(include '" symbol ')'

declaration  ::= func_decl | var_decl | type_decl | struct_import

func_decl    ::= '(defun' symbol '[' bind* ']' param-group '->' type ')'
               | '(defun' symbol param-group '->' type ')'
               | '(defun' symbol '[' bind* ']' clause+ ')'
               | '(defun' symbol clause+ ')'
clause       ::= '(' param-group '->' type ')'
var_decl     ::= '(defvar' symbol type ')'
type_decl    ::= '(type' symbol ')'                          ; opaque
               | '(type' symbol '[' bind* ']' ')'            ; opaque + phantom
               | '(type' symbol type ')'                     ; alias
               | '(type' symbol '[' bind* ']' type ')'       ; alias + quantified
struct_import ::= '(import-struct' symbol ')'

bind         ::= symbol | '(' symbol ':' type ')'
param-group  ::= '(' param* ')'                              ; always parenthesized
param        ::= type | '&optional' type | '&rest' type | '&key' keyword type
type         ::= symbol                                      ; type name or variable
               | '_'                                         ; inferred type (fresh tvar)
               | '(' symbol type+ ')'                        ; type application
               | '(' type '|' type ('|' type)* ')'           ; union
               | '(' param-group '->' type ')'               ; function value
               | '(' '[' bind* ']' param-group '->' type ')' ; polymorphic function value
               | literal
literal      ::= integer | string | quoted-symbol | keyword
```

**Note on param groups:** Params are always wrapped in parentheses. This avoids
ambiguity between `(foo bar)` as two params vs a type application. Type
applications inside a param group need their own parens: `((list int))` for a
single param of type `(list int)`.

## Lisp-2 Calling Convention

Elisp separates functions and values into different namespaces:

| Declaration        | Calling convention              | Example                             |
| ------------------ | ------------------------------- | ----------------------------------- |
| `defun`            | Direct: `(f args...)`           | `(defun add (int int) -> int)`      |
| `defvar` with `->` | Indirect: `(funcall f args...)` | `(defvar handler ((string) -> nil))`|

The `->` is infix, separating parameters from return type. Params are always
grouped in parentheses.

```elisp
;; Function slot (callable)
(defun add (int int) -> int)              ; two params
(defun identity [a] (a) -> a)             ; one param

;; Value slot (requires funcall)
(defvar handler ((string) -> nil))        ; monomorphic function value
(defvar id-fn ([a] (a) -> a))             ; polymorphic function value
```

## Type Syntax

### Primitive Types

| Type      | Description           | Truthy? |
| --------- | --------------------- | ------- |
| `int`     | Integers              | Yes     |
| `float`   | Floating-point        | Yes     |
| `num`     | Numeric               | Yes     |
| `string`  | Strings               | Yes     |
| `symbol`  | Symbols               | Yes     |
| `keyword` | Keywords (`:foo`)     | Yes     |
| `nil`     | Only `nil`            | No      |
| `t`       | Only `t`              | Yes     |
| `truthy`  | Anything except `nil` | Yes     |
| `never`   | Bottom type (errors)  | Yes     |

### Standard Library Types

Defined in the standard library, not primitives:

```elisp
(type bool (nil | t))
(type any (truthy | nil))
(type option [(a : truthy)] (a | nil))
```

### Type Variables and Quantification

Type variables must be explicitly quantified. A symbol is a type variable if and
only if it appears in a quantifier `[...]`.

```elisp
;; Explicit quantification required
(defun identity [a] (a) -> a)
(defun map [a b] (((a -> b)) (list a)) -> (list b))

;; Error: unbound type variable 'a'
(defun bad (a) -> a)
```

Quantifiers can include bounds using `(var : type)`:

```elisp
(type option [(a : truthy)] (a | nil))

(defun unwrap-or [(a : truthy)] ((a | nil) a) -> a)
```

An unbounded variable like `[a]` can be instantiated to any type. A bounded
variable like `[(a : truthy)]` can only be instantiated to subtypes of the
bound.

### Literal Types

Singleton types for specific values:

```elisp
42                                    ; integer literal (subtype of int)
"hello"                               ; string literal (subtype of string)
'foo                                  ; symbol literal (subtype of symbol)
:bar                                  ; keyword literal (subtype of keyword)
```

Useful for discriminated unions and exact value types:

```elisp
(type status (:pending | :complete | :failed))
(defun api-version -> "2.0")
```

### Union Types

Union types use `|` and must be parenthesized:

```elisp
(string | nil)                        ; string or nil
(int | string | nil)                  ; multiple alternatives
(:ok | :error | :pending)             ; literal union
```

The `|` operator binds tighter than `->`:

```elisp
(defun foo [a] (a) -> (a | nil))      ; returns union
(defun bar [a] ((a | nil)) -> a)      ; takes union param
```

### Function Types

Elisp functions are **not curried**; parameters are grouped.

Arrow types use infix `->`. Params are always in parentheses:

```elisp
(int) -> int                           ; one param
(int int) -> int                       ; two params
(string &optional int) -> string       ; optional parameter
(&rest string) -> string               ; rest parameter
(&key :name string :age int) -> nil    ; keyword parameters
(int) -> ((int) -> int)                ; returns function value
```

Type applications in params need their own parens:

```elisp
((a) -> b)                             ; function taking a, returning b
(((a -> b)) (list a)) -> (list b)      ; first param is a function type
```

For polymorphic function **values** (in `defvar` or as parameters), quantifiers
go at the start of the arrow type:

```elisp
(defvar id-fn ([a] (a) -> a))         ; polymorphic function value
(defvar handler ((string) -> nil))    ; monomorphic function value
```

### Parameter Kinds

| Syntax           | Meaning                    |
| ---------------- | -------------------------- |
| `type`           | Required positional        |
| `&optional type` | Optional (caller may omit) |
| `&rest type`     | Rest args (element type)   |
| `&key :k type`   | Keyword argument           |

### Type Constructors

```elisp
(list a)                              ; homogeneous list
(vector a)                            ; homogeneous vector
(pair a b)                            ; cons cell
(tuple a b c)                         ; fixed-length tuple
(hash-table k v)                      ; hash table
```

### Option Type

`option` is defined in the standard library with a truthy constraint:

```elisp
(type option [(a : truthy)] (a | nil))
```

This ensures the inner type is distinguishable from `nil`:

```elisp
(option string)         ; valid
(option int)            ; valid
(option (list a))       ; valid
(option nil)            ; INVALID - a : truthy not satisfied
(option bool)           ; INVALID - bool includes nil
(option (option a))     ; INVALID - option includes nil
```

## Declarations

### Function Declarations

Functions declared with `defun` are directly callable as `(name args...)`:

```elisp
(defun my-add (int int) -> int)
(defun my-identity [a] (a) -> a)
(defun my-map [a b] (((a -> b)) (list a)) -> (list b))
```

Note: The first parameter to `my-map` is an arrow type `((a -> b))`, meaning it's
a function **value** that must be called with `funcall` inside the
implementation.

#### Multi-Clause Functions

Functions can have multiple `(params -> return)` clauses, tried in order
(pattern-matching semantics). Used for type predicates and overloaded
signatures:

```elisp
;; Type predicate: narrows argument to string in conditionals
(defun stringp
  ((string) -> t)
  ((_) -> nil))

;; Inverted: atom is anything that's not a cons
(defun atom
  (((cons any any)) -> nil)
  ((_) -> t))

;; Polymorphic with multiple clauses
(defun car [a b]
  (((cons a b)) -> a)
  ((nil) -> nil))
```

#### Inferred Types (`_`)

Any type symbol starting with `_` is an **inferred type variable**--a fresh
tvar that participates in HM unification, not `any`. Each `_` occurrence is
independent:

```elisp
(defun foo
  ((string int) -> string)
  ((_ _) -> nil))           ;; two independent fresh tvars
```

`_foo`, `_bar` etc. are also valid wildcard names.

### Variable Declarations

```elisp
(defvar my-default-value string)
(defvar my-counter int)
```

### Type Declarations

The `type` declaration has four forms:

**Opaque type** (no definition = abstract):

```elisp
(type buffer)
```

**Opaque type with phantom parameters:**

```elisp
(type handle [a])
(type tagged [tag])
```

**Type alias:**

```elisp
(type int-list (list int))
(type callback ((string) -> nil))
(type status (:pending | :complete | :failed))
```

**Type alias with quantifier:**

```elisp
(type option [(a : truthy)] (a | nil))
(type result [a e] ((ok a) | (err e)))
(type predicate [a] ((a) -> bool))
```

The quantifier form `(type name [vars] def)` is equivalent to
`(type name ([vars] def))` -- the quantifier binds to the entire definition.

### Opaque Types

A `type` declaration with no definition creates an abstract type with hidden
representation. Consumers can pass values around but cannot inspect or construct
them except via declared functions.

```elisp
;; buffer-manager.tart
(type buffer)                             ; abstract type (no definition = opaque)

(defun buffer-create (string) -> buffer)
(defun buffer-name (buffer) -> string)
(defun buffer-kill (buffer) -> nil)
```

Each opaque declaration creates a distinct type--`buffer` and `handle` below are
not interchangeable:

```elisp
(type buffer)
(type handle)
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
(open 'seq)  ; seq types now available for use in signatures

(defun my-flatten [a] ((seq (seq a))) -> (list a))
(defun my-frequencies [a] ((seq a)) -> (hash-table a int))
```

Use `open` when you need to reference types from other modules in your
signatures.

### Include (Re-export)

`(include 'module)` inlines all declarations from another `.tart` file, making
them part of this module's interface.

```elisp
;; my-extended-seq.tart
(include 'seq)  ; re-export everything from seq

;; Plus additional functions
(defun seq-partition [a] (int (seq a)) -> (list (list a)))
(defun seq-interleave [a] ((seq a) (seq a)) -> (list a))
```

Use `include` to extend or re-export another module's type interface.

## Complete Example

```elisp
;; my-utils.tart
(open 'seq)  ; import seq for use in signatures

;; Type aliases
(type int-list (list int))
(type result [a e] ((ok a) | (err e)))

;; Opaque type (no definition)
(type handle)

;; Function declarations (directly callable)
(defun my-utils-trim (string) -> string)
(defun my-utils-split (string string) -> (list string))
(defun my-utils-identity [a] (a) -> a)

;; Variable (non-function)
(defvar my-utils-default-separator string)

;; Variable holding a function value (requires funcall)
(defvar my-utils-error-handler ((string) -> nil))
(defvar my-utils-transform ([a] (a) -> a))
```

## Signature Search Path

When a module is required, tart searches for `.tart` files in order:

1. **Sibling file**: `module.tart` next to `module.el` (highest priority)
2. **Search path**: Directories in `tart-type-path` (user/community types)
3. **Bundled stdlib**: `stdlib/module.tart` shipped with tart

This allows providing types for any module, including third-party packages that
don't ship their own type definitions.

### Search Path Configuration

```elisp
;; In Emacs config
(setq tart-type-path '("~/.config/emacs/tart/" "/path/to/community-types/"))
```

### Example: Providing Types for External Packages

```
~/.config/emacs/tart/
├── seq.tart              ; types for seq.el
├── dash.tart             ; types for dash.el
└── magit-section.tart    ; types for magit-section.el
```

```elisp
;; ~/.config/emacs/tart/seq.tart
(defun seq-map [a b] (((a -> b)) (seq a)) -> (list b))
(defun seq-filter [a] (((a -> bool)) (seq a)) -> (list a))
(defun seq-reduce [a b] (((b a -> b)) b (seq a)) -> b)
(defun seq-find [a] (((a -> bool)) (seq a)) -> (option a))
```

The filename determines the module: `seq.tart` provides types for
`(require 'seq)`. The first match in the search order wins, allowing user
overrides of bundled types.

## See Also

- [DESIGN.md](../../DESIGN.md) - Type system reference
- [Spec 07](../../specs/07-signature-files.md) - Parser implementation spec
