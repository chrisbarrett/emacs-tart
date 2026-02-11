# Signature File Format (.tart)

Canonical reference for tart's `.tart` signature file format.

## Overview

`.tart` files declare the **public interface** of Elisp modules — functions,
variables, and their types. Only what appears in a `.tart` file is visible to
consumers, letting you keep implementation details private while exposing a
type-safe public API.

Type checking is available for any `.el` file via LSP. When code uses `require`
or autoloaded functions, tart searches for corresponding `.tart` files. A
sibling `foo.tart` declares the public interface of `foo.el` and enables
signature verification.

## File Structure

A `.tart` file consists of optional directives followed by declarations. The
module name comes from the filename: `foo.tart` provides types for `foo`.

```elisp
;; Directives (imports)
(open 'module)                             ; Import types for use in signatures
(include 'module)                          ; Inline and re-export declarations

;; Declarations
(defun name [vars] (params) -> ret)        ; Function (callable)
(defvar variable-name type)                ; Variable declaration
(type name)                                ; Opaque type
(type name [vars] def)                     ; Type alias
(let-type name def)                        ; File-local type alias (not exported)
(defstruct name (field type) ...)          ; Struct declaration
(forall [vars] decl ...)                   ; Scoped type variables
```

## Grammar

```
tart_file    ::= directive* declaration*

directive    ::= open_decl | include_decl
open_decl    ::= "(open '" symbol ')'
include_decl ::= "(include '" symbol ')'

declaration  ::= func_decl | var_decl | type_decl | let_type_decl
               | struct_decl | forall_decl

func_decl    ::= '(defun' symbol '[' bind* ']' param-group '->' type ')'
               | '(defun' symbol param-group '->' type ')'
               | '(defun' symbol '[' bind* ']' clause+ ')'
               | '(defun' symbol clause+ ')'
clause       ::= '(' param-group '->' type ')'
               | '(' param-group '->' type ':' diagnostic ')'

var_decl     ::= '(defvar' symbol type ')'

type_decl    ::= '(type' symbol ')'                          ; opaque
               | '(type' symbol '[' bind* ']' ')'            ; opaque + phantom
               | '(type' symbol type ')'                     ; alias
               | '(type' symbol '[' bind* ']' type ')'       ; alias + quantified
               | '(type' type_binding type_binding+ ')'      ; mutually recursive

let_type_decl ::= '(let-type' symbol ')'
                | '(let-type' symbol '[' bind* ']' ')'
                | '(let-type' symbol type ')'
                | '(let-type' symbol '[' bind* ']' type ')'
                | '(let-type' type_binding type_binding+ ')'

type_binding  ::= symbol '[' bind* ']'? type

struct_decl  ::= '(defstruct' symbol field_decl+ ')'
               | '(defstruct' symbol ':keyword-constructor' field_decl+ ')'
field_decl   ::= '(' symbol type ')'

forall_decl  ::= '(forall' '[' bind* ']' declaration+ ')'

bind         ::= symbol | '(' symbol ':' type ')'
param-group  ::= '(' param* ')'
param        ::= type | '&optional' type | '&rest' type | '&key' keyword type
type         ::= symbol
               | '_'                                          ; inferred type (fresh tvar)
               | '(' symbol type+ ')'                        ; type application
               | '(' type '|' type ('|' type)* ')'           ; union
               | '(' type '-' type ')'                       ; type subtraction
               | '(' param-group '->' type ')'               ; function value
               | '(' '[' bind* ']' param-group '->' type ')' ; polymorphic function value
               | '{' field_entry* ('&' symbol)? '}'          ; row type
               | literal
field_entry  ::= symbol type
literal      ::= integer | string | quoted-symbol | keyword
diagnostic   ::= string                                      ; message for clause
```

> [!NOTE]
> Param groups are **always** parenthesized. This avoids ambiguity between
> `(foo bar)` as two params vs a type application. Type applications inside a
> param group need their own parens: `((list int))` for a single param of type
> `(list int)`.

## Declarations

### Function Declarations (`defun`)

Functions declared with `defun` are directly callable as `(name args...)`:

```elisp
(defun my-add (int int) -> int)
(defun my-identity [a] (a) -> a)
(defun my-map [a b] (((a -> b)) (list a)) -> (list b))
```

The first parameter to `my-map` is an arrow type `((a -> b))`, meaning it is a
function **value** that must be called with `funcall` inside the implementation.

#### Multi-Clause Functions

Functions can have multiple `(params -> return)` clauses, tried in order
(pattern-matching semantics). Use multi-clause signatures for type predicates
and overloaded functions:

```elisp
;; Type predicate: narrows argument to string in conditionals
(defun stringp
  ((string) -> t)
  ((_) -> nil))

;; Inverted predicate: atom is anything that's not a cons
(defun atom
  (((cons any any)) -> nil)
  ((_) -> t))

;; Polymorphic with multiple clauses
(defun car [a b]
  (((cons a b)) -> a)
  ((nil) -> nil))
```

#### Clause Diagnostics

A clause can attach a diagnostic message that tart emits when that clause
matches. Use this for deprecation warnings, usage notes, or error guidance:

```elisp
clause       ::= '(' param-group '->' type ':' diagnostic ')'
```

```elisp
;; Deprecation warning on a single-clause function
(defun interactive-p () -> bool
  : "interactive-p is deprecated; use called-interactively-p")

;; Warning only on the fallback clause
(defun my-convert
  ((string) -> int)
  ((_) -> nil : "my-convert only accepts strings"))
```

When a call matches a clause with a diagnostic, tart reports the message as a
warning (or error/note depending on severity).

#### Inferred Types (`_`)

Any type symbol starting with `_` is an **inferred type variable** — a fresh
type variable that participates in HM unification. Each `_` occurrence is
independent. These are **not** `any`:

```elisp
(defun foo
  ((string int) -> string)
  ((_ _) -> nil))           ;; two independent fresh tvars
```

`_foo`, `_bar`, etc. are also valid wildcard names.

### Variable Declarations (`defvar`)

```elisp
(defvar my-default-value string)
(defvar my-counter int)
```

### Type Declarations (`type`)

The `type` declaration has four single-binding forms and a multi-binding form
for mutual recursion.

**Opaque type** — no definition, creates an abstract type:

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

**Mutually recursive types** — multiple bindings in a single form make all names
visible to each other:

```elisp
(type tree   [a] (leaf a | node (forest a))
      forest [a] (list (tree a)))
```

Without multi-binding, the second name would not exist when the first is
defined. Opaque types cannot appear in multi-binding forms (they have no body to
reference).

### Local Type Aliases (`let-type`)

`let-type` is identical to `type` in every way except that it is **not
exported**. Modules that `open` or `include` this file do not see `let-type`
names. Use it for internal abbreviations that keep signatures readable without
polluting the public namespace:

```elisp
;; Internal shorthand (not exported)
(let-type pair (cons int int))
(let-type wrapper [a] (list a))
(let-type internal-handle)

;; Mutually recursive (not exported)
(let-type internal-tree   (leaf int | node internal-forest)
          internal-forest (list internal-tree))
```

All forms that `type` supports are valid under `let-type`, including
multi-binding for mutual recursion.

### Struct Declarations (`defstruct`)

`defstruct` generates typed constructor, predicate, and accessor signatures from
field declarations:

```elisp
(defstruct person
  (name string)
  (age int)
  (email (string | nil)))
```

This generates the following signatures:

```elisp
;; Constructor
(defun make-person (string int (string | nil)) -> (record person))

;; Predicate
(defun person-p
  (((record person)) -> t)
  ((_) -> nil))

;; Accessors
(defun person-name ((record person)) -> string)
(defun person-age ((record person)) -> int)
(defun person-email ((record person)) -> (string | nil))
```

Use `:keyword-constructor` to generate a keyword-argument constructor:

```elisp
(defstruct person :keyword-constructor
  (name string)
  (age int))

;; Generates:
;; (defun make-person (&key (:name string) (:age int)) -> (record person))
```

### Forall Declarations (`forall`)

`forall` scopes type variables across multiple declarations. Use it when several
declarations share the same type variable but are not individually polymorphic:

```elisp
(forall [a]
  (defun iter-next ((iter a)) -> (a | nil))
  (defun iter-peek ((iter a)) -> (a | nil))
  (defun iter-reset ((iter a)) -> nil))
```

Without `forall`, each `defun` would need its own `[a]` quantifier. `forall`
binds the variables once and makes them available to all enclosed declarations.

### Open (Import Types)

`(open 'module)` brings type names from another module into scope for use in
type expressions. The imported names are not re-exported:

```elisp
;; my-collection.tart
(open 'seq)

(defun my-flatten [a] ((seq (seq a))) -> (list a))
(defun my-frequencies [a] ((seq a)) -> (hash-table a int))
```

### Include (Re-export)

`(include 'module)` inlines all declarations from another `.tart` file, making
them part of this module's interface:

```elisp
;; my-extended-seq.tart
(include 'seq)

(defun seq-partition [a] (int (seq a)) -> (list (list a)))
(defun seq-interleave [a] ((seq a) (seq a)) -> (list a))
```

## Type Syntax

### Primitive Types

| Type      | Description           | Truthy? |
| :-------- | :-------------------- | :------ |
| `int`     | Integers              | Yes     |
| `float`   | Floating-point        | Yes     |
| `num`     | Numeric               | Yes     |
| `string`  | Strings               | Yes     |
| `symbol`  | Symbols               | Yes     |
| `keyword` | Keywords (`:foo`)     | Yes     |
| `nil`     | Only `nil`            | No      |
| `t`       | Only `t`              | Yes     |
| `truthy`  | Anything except `nil` | Yes     |
| `never`   | Bottom type (errors)  | N/A     |

### Prelude Types

Defined in the [prelude][prelude], not primitives:

```elisp
(type bool (nil | t))
(type any (truthy | nil))
(type option [(a : truthy)] (a | nil))
(type is [a] (a - nil))
```

### Type Variables and Quantification

Type variables must be explicitly quantified. A symbol is a type variable if and
only if it appears in a quantifier `[...]`:

```elisp
;; Explicit quantification required
(defun identity [a] (a) -> a)
(defun map [a b] (((a -> b)) (list a)) -> (list b))

;; Error: unbound type variable 'a'
(defun bad (a) -> a)
```

Quantifiers can include upper bounds using `(var : type)`:

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

### Type Subtraction

Type subtraction removes a type from a union using `-`:

```elisp
(type is [a] (a - nil))               ; removes nil from a
((int | string | nil) - nil)          ; => (int | string)
((truthy | nil) - nil)                ; => truthy
```

### Function Types

Elisp functions are **not curried**; parameters are grouped. Arrow types use
infix `->` with params always in parentheses:

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

### Type Constructors

```elisp
(list a)                              ; homogeneous list
(vector a)                            ; homogeneous vector
(cons a b)                            ; cons cell
(hash-table k v)                      ; hash table
(record tag)                          ; struct record with type tag
```

### Row Types

Row types describe record-like structures with named fields. They use curly
braces:

```elisp
{name string age int}                 ; closed row: exactly these fields
{name string & r}                     ; open row: at least name, r captures rest
```

A **closed** row has a fixed set of fields. An **open** row uses `& var` to
introduce a row variable, enabling row polymorphism — the function accepts any
record with at least the named fields:

```elisp
(defun get-name [r] ({name string & r}) -> string)
```

### Option Type

`option` is defined in the [prelude][prelude] with a truthy constraint:

```elisp
(type option [(a : truthy)] (a | nil))
```

This ensures the inner type is distinguishable from `nil`:

```elisp
(option string)         ; valid
(option int)            ; valid
(option nil)            ; INVALID — a : truthy not satisfied
(option bool)           ; INVALID — bool includes nil
(option (option a))     ; INVALID — option includes nil
```

## Lisp-2 Calling Convention

Elisp separates functions and values into different namespaces:

| Declaration        | Calling convention              | Example                              |
| :----------------- | :------------------------------ | :----------------------------------- |
| `defun`            | Direct: `(f args...)`           | `(defun add (int int) -> int)`       |
| `defvar` with `->` | Indirect: `(funcall f args...)` | `(defvar handler ((string) -> nil))` |

The `->` is infix, separating parameters from return type. Params are always
grouped in parentheses:

```elisp
;; Function slot (callable)
(defun add (int int) -> int)              ; two params
(defun identity [a] (a) -> a)             ; one param

;; Value slot (requires funcall)
(defvar handler ((string) -> nil))        ; monomorphic function value
(defvar id-fn ([a] (a) -> a))             ; polymorphic function value
```

## Parameter Kinds

| Syntax           | Meaning                    |
| :--------------- | :------------------------- |
| `type`           | Required positional        |
| `&optional type` | Optional (caller may omit) |
| `&rest type`     | Rest args (element type)   |
| `&key :k type`   | Keyword argument           |

## Complete Example

```elisp
;; my-utils.tart
(open 'seq)

;; File-local abbreviation (not exported)
(let-type str-list (list string))

;; Exported types
(type result [a e] ((ok a) | (err e)))
(type handle)

;; Struct
(defstruct config
  (host string)
  (port int))

;; Functions
(defun my-utils-trim (string) -> string)
(defun my-utils-split (string string) -> str-list)
(defun my-utils-identity [a] (a) -> a)

;; Scoped type variables
(forall [a]
  (defun my-utils-wrap (a) -> (list a))
  (defun my-utils-unwrap ((list a)) -> a))

;; Variable (non-function)
(defvar my-utils-default-separator string)

;; Variable holding a function value (requires funcall)
(defvar my-utils-error-handler ((string) -> nil))
```

## Signature Search Path

When a module is required, tart searches for `.tart` files in order:

1. **Sibling file** — `module.tart` next to `module.el` (highest priority)
2. **Bundled typings** — [`typings/`][typings] shipped with tart

The sibling file takes priority, allowing project-local signatures to override
bundled ones.

### Example: Providing Types for External Packages

Place a `.tart` file next to the `.el` file or in a directory on the search
path:

```elisp
;; seq.tart (sibling to seq.el, or in typings/)
(defun seq-map [a b] (((a -> b)) (seq a)) -> (list b))
(defun seq-filter [a] (((a -> bool)) (seq a)) -> (list a))
(defun seq-reduce [a b] (((b a -> b)) b (seq a)) -> b)
(defun seq-find [a] (((a -> bool)) (seq a)) -> (option a))
```

The filename determines the module: `seq.tart` provides types for
`(require 'seq)`.

## See Also

- [Type System Reference](type-system.md)
- [Writing Typings](writing-typings.md)
- [Getting Started](../getting-started.md)

[prelude]: ../../typings/tart-prelude.tart
[typings]: ../../typings/
