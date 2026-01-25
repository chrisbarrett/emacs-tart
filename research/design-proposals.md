# Type System Design Proposal for Tart

A concrete, implementable design for a static type analysis system for Emacs Lisp
based on System F with Hindley-Milner inference.

**Document Status**: Design proposal for review

---

## Executive Summary

This document specifies the complete design for tart, a type checker for Emacs
Lisp. The design prioritizes:

1. **Sound typing** within typed modules (no `any` escape hatch)
2. **Minimal annotation burden** via HM inference
3. **LSP-first delivery** for immediate IDE value
4. **Pragmatic boundaries** with untyped code via `.eli` signature files

### Key Design Principles

**Truthiness model**: Elisp's boolean semantics are captured via primitive types:

- `Nil` — unit type with sole inhabitant `nil`
- `T` — unit type with sole inhabitant `t`
- `Truthy` — primitive type: anything that is not `Nil`
- `Bool` = `T | Nil` — strict boolean for predicates
- `Any` = `Truthy | Nil` — top type (all values)

**Non-curried functions**: Elisp functions take all arguments at once, so
function types use grouped parameters: `(-> (A B) C)` rather than `(-> A B C)`.

**Option constraint**: `(Option a)` requires `a : Truthy`, ensuring the "some"
and "none" cases are always distinguishable by truthiness.

---

## 1. Type Syntax Grammar [R1]

### 1.1 Base Types

```
type ::= base_type
       | type_var
       | func_type
       | forall_type
       | type_app
       | tuple_type
       | sum_type

base_type ::= 'Int'           ; integers
            | 'Float'         ; floating-point
            | 'Num'           ; Int | Float
            | 'String'        ; strings
            | 'Symbol'        ; symbols
            | 'Keyword'       ; keywords (:foo)
            | 'Nil'           ; unit type, sole inhabitant: nil
            | 'T'             ; unit type, sole inhabitant: t
            | 'Truthy'        ; anything that is not Nil (primitive)
            | 'Bool'          ; T | Nil
            | 'Any'           ; Truthy | Nil (top type)
            | 'Never'         ; bottom type (errors, non-termination)

type_var ::= lowercase_ident   ; e.g., a, b, elem

func_type ::= '(->' param_list type ')'
            ; e.g., (-> (Int Int) Int) takes two Ints, returns Int
            ; Elisp functions are NOT curried; all args taken at once

param_list ::= '(' param* ')'

param ::= type                              ; positional parameter
        | '&optional' type                  ; optional (must be Option type)
        | '&rest' type                      ; rest args (desugars to List)
        | '&key' keyword_param+             ; keyword parameters
        | '&allow-other-keys'               ; extra keys as (Plist Any)
        | '&allow-other-keys' type          ; extra keys with explicit type

keyword_param ::= keyword type              ; e.g., :name String

forall_type ::= '(forall' '(' type_var+ ')' type ')'
              ; e.g., (forall (a) (-> a a))

type_app ::= '(' type_constructor type+ ')'
           ; e.g., (List Int), (Option String)

type_constructor ::= 'List'
                   | 'Vector'
                   | 'Option'
                   | 'Pair'
                   | 'Tuple'
                   | 'Or'
                   | 'HashTable'
                   | user_defined_type

tuple_type ::= '(Tuple' type+ ')'
             ; fixed-length heterogeneous sequence

sum_type ::= '(Or' type type+ ')'
           ; union types
```

### 1.2 Type Syntax Examples

```elisp
;; Primitive types
Int
String
Nil                              ; unit type (only nil)
T                                ; unit type (only t)
Truthy                           ; anything not nil
Bool                             ; T | Nil
Any                              ; Truthy | Nil (top type)

;; Function types (grouped parameters - Elisp is NOT curried)
(-> (Int) Int)                   ; one arg
(-> (Int Int) Int)               ; two args
(-> (a b) a)                     ; polymorphic, two args

;; Returning a function (manual currying)
(-> (Int) (-> (Int) Int))        ; takes Int, returns function

;; Polymorphic types
(forall (a) (-> (a) a))          ; identity function
(forall (a b) (-> (a b) a))      ; const function
(forall (a) (-> ((List a)) Int)) ; list length

;; Type constructors
(List Int)                       ; list of integers
(Option String)                  ; String | Nil (note: String must be Truthy)
(Pair String Int)                ; cons cell
(Tuple String Int Bool)          ; fixed 3-tuple
(HashTable Symbol String)        ; hash table

;; Union types
(Or Int String)                  ; Int or String
(Or Nil (List a))                ; nullable list
```

### 1.3 Optional, Rest, and Keyword Arguments

```elisp
;; Optional arguments: explicit &optional with Option type
(-> (String &optional (Option Int) &optional (Option Int)) String)
;; (substring STR &optional FROM TO)

;; Rest arguments: &rest desugars to (List a)
(-> (&rest String) String)                    ; (concat &rest STRINGS)

;; Keyword arguments: explicit &key with types
(-> (&key :name (Option String) :age (Option Int)) Person)
;; (make-person &key name age)

;; Allow extra keywords: defaults to (Plist Any)
(-> (&key :name (Option String) &allow-other-keys) Person)

;; Allow extra keywords with explicit type
(-> (&key :name (Option String) &allow-other-keys (Plist Symbol)) Person)

;; Combined example
(-> (String &optional (Option Int) &rest String) String)
;; (format FMT &optional WIDTH &rest ARGS)
```

### 1.4 Option Type Constraint

The `Option` type constructor requires its argument to be `Truthy`:

```elisp
(Option String)    ; valid: String is Truthy
(Option Int)       ; valid: Int is Truthy
(Option Nil)       ; INVALID: Nil is not Truthy
(Option (List a))  ; valid: (List a) is Truthy
```

This ensures "some" and "none" cases are always distinguishable by truthiness.

---

## 2. Inference Algorithm Selection [R2]

### 2.1 Algorithm Choice: Constraint-Based HM with Levels

**Recommendation**: Implement constraint-based Hindley-Milner inference using
the levels technique for efficient generalization.

**Rationale**:

1. **Proven foundation**: Algorithm W is complete for the HM fragment
2. **Separation of concerns**: Constraint generation is distinct from solving
3. **Better error messages**: Constraint-based approach can report all conflicts
4. **Near-linear performance**: Levels-based generalization avoids quadratic
   environment scanning

### 2.2 Core Algorithm Structure

```
infer : Env × Expr → Type × Constraints

infer(Γ, e) = case e of
  | literal    → (type_of_literal(literal), ∅)
  | var x      → (instantiate(Γ(x)), ∅)
  | (lambda (x) body) →
      let α = fresh_var()
      let (τ, C) = infer(Γ ∪ {x : α}, body)
      (α → τ, C)
  | (e₁ e₂)   →
      let (τ₁, C₁) = infer(Γ, e₁)
      let (τ₂, C₂) = infer(Γ, e₂)
      let α = fresh_var()
      (α, C₁ ∪ C₂ ∪ {τ₁ = τ₂ → α})
  | (let ((x e₁)) e₂) →
      let (τ₁, C₁) = infer(Γ, e₁)
      let S = solve(C₁)
      let σ = generalize(S(Γ), S(τ₁))
      let (τ₂, C₂) = infer(Γ ∪ {x : σ}, e₂)
      (τ₂, C₂)
```

### 2.3 Let-Generalisation Strategy

**Standard let-polymorphism with value restriction**:

1. Generalise type variables at `let` bindings only
2. Only generalise when RHS is a syntactic value:
   - Lambda expressions
   - Literals
   - Variables
   - Constructors applied to values
3. Non-values (function applications) receive monomorphic types

**Examples**:

```elisp
;; Generalised (RHS is lambda)
(let ((id (lambda (x) x)))
  (cons (id 1) (id "hello")))  ; OK: id : (forall (a) (-> a a))

;; NOT generalised (RHS is application)
(let ((xs (reverse '())))
  xs)  ; xs : (List '_a) - monomorphic
```

### 2.4 Higher-Rank Type Handling

**Recommendation**: Start with rank-1 (HM). Design for rank-2 extension.

For v1:
- All `forall` quantifiers appear at the outermost level (prenex form)
- No explicit type annotations required
- Inference is complete and decidable

For v2 (if needed):
- Add bidirectional checking for explicit `forall` annotations
- Support rank-2 types for advanced patterns (ST monad, existentials)

### 2.5 Rejected Alternatives

| Alternative | Reason for Rejection |
|-------------|---------------------|
| Full System F inference | Undecidable; requires annotations everywhere |
| Subtyping | Complicates inference; union types suffice |
| Row polymorphism (v1) | Adds complexity; defer to v2 for plists |
| Effect types | Significant complexity; intractable for buffer state |

---

## 3. Signature File Format (.eli) [R3]

### 3.1 Purpose

`.eli` files provide type signatures for:
1. Untyped Elisp code that tart should type-check against
2. External libraries without inline annotations
3. Built-in Emacs functions

### 3.2 File Relationship

```
my-package.el      ; Elisp source code
my-package.eli     ; Type signatures (sibling file)
```

The presence of a `.eli` file triggers type checking for the corresponding
`.el` file.

### 3.3 Signature File Grammar

```
eli_file ::= (module_decl)? (require_typed)* (declaration)*

module_decl ::= '(module' symbol ')'

require_typed ::= '(require/typed' symbol
                    (import_spec)*
                  ')'

import_spec ::= '(' symbol ':' type ')'
              | '(' symbol ':' type ':rename' symbol ')'

declaration ::= func_sig
              | var_sig
              | type_alias
              | adt_decl
              | struct_import

func_sig ::= '(sig' symbol type ')'

var_sig ::= '(defvar' symbol type ')'

type_alias ::= '(type' symbol '=' type ')'

adt_decl ::= '(data' symbol '(' type_var* ')'
               variant+
             ')'

variant ::= '(' constructor_name type* ')'

struct_import ::= '(import-struct' symbol ')'
```

### 3.4 Signature File Examples

**Basic function signatures**:

```elisp
;; my-utils.eli
(module my-utils)

;; Function signatures
(sig my-utils-trim (-> (String) String))
(sig my-utils-split (-> (String String) (List String)))
(sig my-utils-join (-> (String (List String)) String))

;; Polymorphic function
(sig my-utils-identity (forall (a) (-> (a) a)))
(sig my-utils-compose (forall (a b c) (-> ((-> (b) c) (-> (a) b)) (-> (a) c))))

;; Dynamic variable
(defvar my-utils-default-separator String)
```

**Importing from untyped modules**:

```elisp
;; my-app.eli
(module my-app)

(require/typed seq
  (seq-map : (forall (a b) (-> ((-> (a) b) (List a)) (List b))))
  (seq-filter : (forall (a) (-> ((-> (a) Bool) (List a)) (List a))))
  (seq-reduce : (forall (a b) (-> ((-> (b a) b) b (List a)) b))))

(require/typed subr-x
  (when-let : special-form)
  (if-let : special-form))
```

**ADT definitions**:

```elisp
;; result.eli
(module result)

(data Result (a e)
  (Ok a)
  (Err e))

(sig result-map (forall (a b e) (-> ((-> (a) b) (Result a e)) (Result b e))))
(sig result-bind (forall (a b e) (-> ((Result a e) (-> (a) (Result b e))) (Result b e))))
(sig result-unwrap-or (forall (a e) (-> ((Result a e) a) a)))
```

**Importing cl-defstruct**:

```elisp
;; models.eli
(module models)

;; Import struct defined in models.el via cl-defstruct
(import-struct person)
;; Auto-generates:
;;   person : Type
;;   make-person : (-> (String Int) Person)
;;   person-p : (-> (Any) Bool)
;;   person-name : (-> (Person) String)
;;   person-age : (-> (Person) Int)
```

### 3.5 Built-in Type References

Tart ships with signatures for Emacs built-ins:

```elisp
;; builtins.eli (shipped with tart)
(module emacs-builtins)

;; Arithmetic (variadic)
(sig + (-> (&rest Num) Num))
(sig - (-> (Num &rest Num) Num))
(sig * (-> (&rest Num) Num))
(sig / (-> (Num &rest Num) Num))

;; Strings
(sig concat (-> (&rest String) String))
(sig substring (-> (String &optional (Option Int) &optional (Option Int)) String))
(sig upcase (-> (String) String))
(sig downcase (-> (String) String))
(sig string-match (-> (String String) (Option Int)))

;; Lists
(sig car (forall (a) (-> ((List a)) (Option a))))
(sig cdr (forall (a) (-> ((List a)) (List a))))
(sig cons (forall (a) (-> (a (List a)) (List a))))
(sig length (forall (a) (-> ((List a)) Int)))
(sig reverse (forall (a) (-> ((List a)) (List a))))
(sig mapcar (forall (a b) (-> ((-> (a) b) (List a)) (List b))))
(sig nth (forall (a) (-> (Int (List a)) (Option a))))

;; Type predicates (with occurrence typing propositions)
(sig stringp (-> (Any) Bool :refines String))
(sig integerp (-> (Any) Bool :refines Int))
(sig listp (-> (Any) Bool :refines (List Any)))
(sig null (forall (a) (-> ((Option a)) Bool :refines Nil)))

;; Error handling (returns Never)
(sig error (-> (String &rest Any) Never))
(sig signal (-> (Symbol Any) Never))
```

---

## 4. ADT System Design [R4]

### 4.1 ADT Declaration Syntax

```elisp
;; In .eli file
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

### 4.2 Phantom Runtime Semantics

ADT values exist only at the type level. At runtime, they are represented using
standard Elisp:

| Type-Level | Runtime Representation |
|------------|----------------------|
| `(Some x)` | `x` or `(Some . x)` |
| `(None)` | `nil` |
| `(Ok x)` | `(ok . x)` |
| `(Err e)` | `(err . e)` |
| `(Leaf x)` | `(leaf . x)` |
| `(Node l r)` | `(node l . r)` |

**Implementation option 1: Tagged cons cells**

```elisp
;; Runtime representation
(defun Some (x) (cons 'Some x))
(defun None () nil)
(defun Ok (x) (cons 'ok x))
(defun Err (e) (cons 'err e))
```

**Implementation option 2: Pure Elisp (no wrapper)**

For simple cases like `Option`, use direct representation:

```elisp
;; Some x = x (when x is non-nil)
;; None = nil
```

**Recommendation**: Use tagged cons cells for all ADTs for consistency and to
avoid nil/false ambiguity.

### 4.3 pcase Integration

ADT patterns integrate with Elisp's `pcase`:

```elisp
;; Type-checked pcase
(pcase result
  (`(ok . ,value) (process value))    ; value : a
  (`(err . ,error) (handle error)))   ; error : e

;; Occurrence typing narrows types in branches
(pcase x
  ((pred stringp) (upcase x))         ; x : String
  ((pred integerp) (1+ x))            ; x : Int
  (_ (error "unexpected")))           ; x : Any
```

### 4.4 Constructor Syntax

Constructors are functions with generated signatures:

```elisp
;; (data Option (a) (Some a) (None))
;; Generates:
(sig Some (forall (a) (-> (a) (Option a))))
(sig None (forall (a) (-> () (Option a))))
(sig option-some-p (forall (a) (-> ((Option a)) Bool)))
(sig option-none-p (forall (a) (-> ((Option a)) Bool)))
(sig option-some-value (forall (a) (-> ((Option a)) a)))  ; partial, or with default

;; (data Result (a e) (Ok a) (Err e))
;; Generates:
(sig Ok (forall (a e) (-> (a) (Result a e))))
(sig Err (forall (a e) (-> (e) (Result a e))))
(sig result-ok-p (forall (a e) (-> ((Result a e)) Bool)))
(sig result-err-p (forall (a e) (-> ((Result a e)) Bool)))
(sig result-ok-value (forall (a e) (-> ((Result a e)) a)))
(sig result-err-value (forall (a e) (-> ((Result a e)) e)))
```

### 4.5 Pattern Exhaustiveness

The type checker verifies `pcase` exhaustiveness for ADT patterns:

```elisp
;; Warning: non-exhaustive pattern match
(pcase opt
  (`(Some . ,x) x))  ; Missing: None

;; OK: exhaustive
(pcase opt
  (`(Some . ,x) x)
  ('None default))
```

---

## 5. Module Boundary Rules [R5]

### 5.1 What Triggers Type Checking

Type checking is triggered when:

1. A `.eli` signature file exists as sibling to a `.el` file
2. The `.el` file is opened in an LSP-connected editor
3. The user explicitly requests type checking

### 5.2 require/provide Interaction

```elisp
;; In my-app.el
(require 'my-utils)  ; Requires my-utils.eli to exist for typing

;; In my-utils.eli
(module my-utils)
(sig my-utils-foo (-> Int Int))

;; Tart verifies:
;; 1. my-utils.el defines my-utils-foo
;; 2. my-utils-foo's inferred type matches signature
```

### 5.3 Untyped Code Handling

| Scenario | Behavior |
|----------|----------|
| Typed calls untyped | Use `require/typed` to declare expected types |
| Untyped calls typed | No checking (sound within typed world) |
| No `.eli` file | File not type-checked |

**Typed module calling untyped**:

```elisp
;; my-app.eli
(require/typed external-lib
  (external-lib-process : (-> String String)))

;; my-app.el
(require 'external-lib)
(defun my-func (s)
  (external-lib-process s))  ; Type-checked against declared signature
```

### 5.4 Public/Private Inference

Symbols are considered:

- **Public**: Listed in `.eli` file
- **Internal**: Not in `.eli` but defined in `.el`

Internal functions are type-inferred but not exported in the module interface.

```elisp
;; my-utils.eli
(sig my-utils-public-api (-> String String))
;; my-utils--internal not listed

;; my-utils.el
(defun my-utils-public-api (s) ...)  ; Checked against signature
(defun my-utils--internal (x) ...)   ; Inferred, not exported
```

### 5.5 Autoload Handling

Autoloaded functions must have signatures in `.eli` files:

```elisp
;; my-package.eli
(sig my-package-autoloaded-func (-> Int String))

;; my-package.el
;;;###autoload
(defun my-package-autoloaded-func (n)
  (number-to-string n))
```

---

## 6. Error Reporting Strategy [R6]

### 6.1 Error Message Structure

Following Elm and Rust's lead, errors have:

1. **Header**: Error code and one-line summary
2. **Primary span**: The exact location with main issue
3. **Secondary spans**: Related locations explaining "why"
4. **Note**: Additional context
5. **Help**: Actionable fix suggestions

### 6.2 Error Message Template

```
error[E0308]: type mismatch
  --> file.el:LINE:COL
   |
NN |   (expression here)
   |    ^^^^^^^^^^^^^^^^
   |    expected: EXPECTED_TYPE
   |    found: ACTUAL_TYPE
   |
note: EXPLANATION_OF_WHY
  --> file.el:RELATED_LINE:COL
   |
NN |   (related code)
   |    ^^^^^ this is TYPE because...

help: SUGGESTED_FIX
   |
NN |   (corrected code)
```

### 6.3 Example Error Messages

**Type mismatch in function argument**:

```
error[E0308]: type mismatch
  --> init.el:42:10
   |
42 |   (upcase count)
   |          ^^^^^
   |          expected: String
   |          found: Int
   |
note: `upcase` expects a String argument
      (upcase STRING) -> String

help: convert the integer to a string first:
   |
42 |   (upcase (number-to-string count))
```

**If branches have different types**:

```
error[E0317]: if branches have incompatible types
  --> utils.el:28:3
   |
28 |   (if (> n 0)
29 |       n
   |       ^ this branch has type: Int
30 |       "negative")
   |       ^^^^^^^^^^ this branch has type: String
   |
note: both branches of `if` must have the same type

help: return a string in both cases:
   |
29 |       (number-to-string n)
```

**Possible nil value**:

```
error[E0308]: possible nil value
  --> format.el:18:13
   |
18 |   (upcase (get-name user))
   |           ^^^^^^^^^^^^^^^^
   |           expected: String
   |           found: (Option String)
   |
note: `get-name` may return nil

help: check for nil first:
   |
18 |   (when-let ((name (get-name user)))
19 |     (upcase name))

help: or provide a default:
   |
18 |   (upcase (or (get-name user) "Unknown"))
```

**Undefined variable with suggestion**:

```
error[E0425]: variable `strng` is not defined
  --> edit.el:102:5
   |
102|     (upcase strng)
   |             ^^^^^ not found in scope
   |
help: a variable with a similar name exists:
   |
102|     (upcase string)
```

### 6.4 Error Codes

| Code | Category | Description |
|------|----------|-------------|
| E0308 | Type | Type mismatch |
| E0317 | Type | Incompatible branch types |
| E0425 | Name | Undefined variable |
| E0426 | Name | Undefined function |
| E0061 | Arity | Wrong number of arguments |
| E0106 | Annotation | Missing type annotation required |
| E0521 | Polymorphism | Type too specific for context |

---

## 7. Implementation Language Recommendation [R7]

### 7.1 Recommendation: OCaml

**Primary recommendation**: Implement tart in OCaml.

### 7.2 Rationale

| Factor | OCaml | Typed Racket | Elisp |
|--------|-------|--------------|-------|
| Type safety | Excellent | Excellent | None |
| Pattern matching | Native, exhaustive | Native | Via pcase |
| ADT support | Native | Native | Manual |
| Parser generators | Menhir (excellent) | Adequate | Manual |
| LSP libraries | Mature (ocaml-lsp) | Basic | jsonrpc.el |
| Performance | Fast | Slower | Slowest |
| Ecosystem | dune, opam | raco | package.el |
| Precedent | Flow, Hack, Rocq | None for type checkers | None |

### 7.3 OCaml Ecosystem Analysis

**Parser**: Menhir generates LR(1) parsers with excellent error recovery.
Integrates with dune build system.

**LSP**: The `lsp` opam package provides server implementation primitives.
`linol` provides a higher-level framework. `ocaml-lsp-server` demonstrates
production quality.

**Type Representation**:

```ocaml
type typ =
  | TVar of tvar ref
  | TCon of string
  | TArr of typ * typ
  | TForall of string list * typ
  | TApp of string * typ list

and tvar =
  | Unbound of int * int  (* id, level *)
  | Link of typ
```

**Build system**: dune provides fast incremental builds, native/byte targets.

### 7.4 Alternative Considered: Typed Racket

**Pros**:
- Lisp-in-Lisp synergy
- Native S-expression parsing
- Good pattern matching

**Cons**:
- Slower runtime than OCaml
- Smaller ecosystem for tooling
- Less mature LSP support
- No major type checkers written in it

### 7.5 Alternative Considered: Rust

**Pros**:
- Excellent performance
- Strong ecosystem (tower-lsp, salsa)
- Memory safety without GC

**Cons**:
- Verbose for AST manipulation
- Steeper learning curve
- Less natural for tree transformations

---

## 8. LSP Architecture [R8]

### 8.1 Overall Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    tart LSP Server                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐       │
│  │   JSON-RPC   │────│   Dispatch   │────│   Handlers   │       │
│  │   (stdio)    │    │   Router     │    │              │       │
│  └──────────────┘    └──────────────┘    └──────────────┘       │
│                                                 │                │
│                           ┌─────────────────────┴──────┐        │
│                           ▼                            ▼        │
│                    ┌──────────────┐          ┌──────────────┐   │
│                    │   Document   │          │    Query     │   │
│                    │   Manager    │          │   Database   │   │
│                    └──────────────┘          └──────────────┘   │
│                           │                         │           │
│                           ▼                         ▼           │
│                    ┌──────────────┐          ┌──────────────┐   │
│                    │   Virtual    │◄────────►│    Type      │   │
│                    │   File Sys   │          │   Checker    │   │
│                    └──────────────┘          └──────────────┘   │
│                                                     │           │
│                                              ┌──────┴──────┐    │
│                                              ▼             ▼    │
│                                        ┌─────────┐  ┌─────────┐ │
│                                        │ Parser  │  │ Stdlib  │ │
│                                        │         │  │  Sigs   │ │
│                                        └─────────┘  └─────────┘ │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### 8.2 Parsing Strategy

**Two-phase parsing**:

1. **Quick parse**: Extract top-level forms for navigation (fast, resilient)
2. **Full parse**: Complete AST for type checking (thorough)

**Resilient parsing**: Continue after errors to provide partial results.

```
source.el → Lexer → Token Stream → Parser → AST (with error nodes)
                                      ↓
                              macroexpand-all
                                      ↓
                                  Core AST
                                      ↓
                               Type Checker
```

### 8.3 Incremental Checking (Query-Based)

Implement using the Salsa-style query model:

```
source_text(file) → String              ; Input (changes on edit)
tokens(file) → List<Token>              ; Memoized
parsed_forms(file) → List<Form>         ; Memoized
expanded_forms(file) → List<CoreExpr>   ; Memoized
resolved_names(file) → NameMap          ; Memoized
inferred_types(file) → TypeMap          ; Memoized
diagnostics(file) → List<Diagnostic>    ; Memoized
```

**Invalidation rules**:
- Editing a file invalidates `source_text`
- If `tokens` output unchanged, downstream queries preserved
- Per-function granularity: changing one function doesn't invalidate others

### 8.4 Caching Strategy

| Data | Cache Location | Invalidation |
|------|----------------|--------------|
| Parsed forms | In-memory | On file change |
| Expanded forms | In-memory | On source or macro change |
| Type information | In-memory + disk | On source change |
| Stdlib signatures | Disk | Never during session |

### 8.5 Hover Implementation

```ocaml
let handle_hover ~file ~position =
  let form = find_form_at file position in
  match form with
  | Some expr ->
    let ty = lookup_type expr in
    let doc = lookup_doc expr in
    Some { contents = format_hover ty doc; range = expr.range }
  | None -> None

let format_hover ty doc =
  Printf.sprintf "```elisp\n%s\n```\n\n%s" (pretty_type ty) doc
```

### 8.6 Priority Capabilities

| Phase | Capabilities |
|-------|-------------|
| Phase 1 (MVP) | `publishDiagnostics`, `hover`, `didOpen/Change/Close` |
| Phase 2 | `definition`, `references`, `codeAction` |
| Phase 3 | `inlayHint`, `signatureHelp`, `completion` |
| Phase 4 | `semanticTokens`, `rename`, `callHierarchy` |

---

## 9. Trade-offs and Risks [R9]

### 9.1 Design Trade-offs

| Decision | Trade-off | Mitigation |
|----------|-----------|------------|
| No `any` type | Less flexibility | Explicit annotations for complex cases |
| Expand-then-type | Post-expansion errors confusing | Source maps, macro-aware formatting |
| Value restriction | Some code needs annotation | Familiar to ML programmers |
| `.eli` files | Extra files to maintain | Auto-generate from `cl-defstruct` |
| OCaml implementation | Not dogfooding | Best ecosystem for type checkers |
| No effect types | Unsound for buffer state | Accept; focus on value types |

### 9.2 Soundness Trade-offs

| Feature | Soundness Level | Justification |
|---------|-----------------|---------------|
| Pure function types | Sound | Full HM inference |
| `cl-defstruct` | Sound | Direct type correspondence |
| Dynamic variables | Unsound | Require `.eli` annotations |
| Buffer state | Unsound | Effect system too complex |
| Advice system | Excluded | Types change at runtime |
| Dynamic codegen | Excluded | Inherently untyped |

### 9.3 Technical Risks

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Macro expansion API unreliable | Medium | High | Test with `macroexpand-all`; fallback to unexpanded |
| Performance on large files | Medium | Medium | Incremental analysis; lazy checking |
| Error quality post-expansion | High | Medium | Source maps; macro-aware messages |
| Adoption (need `.eli` files) | High | High | Auto-generate; good defaults |
| Type system expressiveness | Medium | Medium | Plan for v2 extensions |

### 9.4 Risk Mitigation Strategies

**Macro expansion risk**:
- Test `macroexpand-all` extensively with common macro patterns
- Provide fallback: if expansion fails, treat as untyped
- Document supported macros explicitly

**Performance risk**:
- Implement query-based incrementality from the start
- Profile early and often with real codebases
- Set latency budgets: hover <100ms, diagnostics <500ms

**Adoption risk**:
- Ship signatures for common packages (seq, map, cl-lib, subr-x)
- Provide `tart generate-sig` command to bootstrap `.eli` files
- Integrate with `cl-defstruct` for automatic type extraction

---

## 10. Open Questions [R10]

### 10.1 Resolved Design Decisions

| Question | Resolution | Notes |
|----------|------------|-------|
| nil/truthiness | `Nil` and `T` are unit types; `Truthy` = not Nil; `Any` = `Truthy \| Nil`; `Bool` = `T \| Nil` | Captures Elisp semantics |
| `(Option a)` constraint | `a` must be `Truthy` | Ensures distinguishability |
| Function types | Grouped parameters: `(-> (A B) C)` | Elisp is not curried |
| Optional arguments | Explicit `&optional` with `(Option a)` type | Mirrors defun syntax |
| Rest arguments | Explicit `&rest` desugars to `(List a)` | Mirrors defun syntax |
| Keyword arguments | Explicit `&key :name Type` with explicit `(Option ...)` | v1 scope |
| `&allow-other-keys` | Defaults to `(Plist Any)`, overridable | Flexibility for strict sigs |
| Error representation | `error`/`signal` return `Never` | Debugger intervention is out of model |
| Struct subtyping | Deferred to future version | Keep v1 simple |
| Stdlib signatures | Ship bundled with tart | Versioning mechanism deferred to post-v1 |
| Macro expansion | Pure Elisp interpreter in OCaml | No Emacs subprocess; security + hermetic builds |
| Advice system | Type-check definitions given known signatures | Same approach as hooks; `:around` takes fn + args |
| Escape hatch | `(tart-ignore EXPR)` macro or comment annotation | Identity at runtime; type checker treats as `Any` |
| Error messages | Simple one-line for v1; structured Elm-style later | LSP squiggles provide location; polish item |

### 10.2 Questions Requiring Prototyping

| Question | Prototype Approach |
|----------|-------------------|
| How reliable is `macroexpand-all`? | Test with 100 common macros from MELPA |
| How fast is incremental type checking? | Implement MVP, benchmark on 10k LOC |
| How good are post-expansion errors? | Build error corpus, measure user comprehension |
| What fraction of MELPA can be typed? | Sample 50 packages, attempt to write `.eli` |

### 10.3 Unresolved Dependencies

| Dependency | Status | Fallback |
|------------|--------|----------|
| Elisp S-expression parser in OCaml | Needs implementation | Use Tree-sitter |
| Pure Elisp interpreter in OCaml | Needs implementation | Core subset for macro expansion |
| LSP library selection | lsp vs linol | Either works |

### 10.4 Future Considerations

**Pure interpreter boundaries**: The pure Elisp interpreter handles macro expansion
up to system access boundaries. At these boundaries:

- File I/O, network, process spawning → opaque, require annotation
- Dynamic symbol construction (`intern`, `make-symbol`) → opaque
- `eval` on computed forms → opaque

For opaque boundaries, options include explicit type annotations or existential
types. The interpreter maintains full source location and type environment
through expansion, enabling error messages that reference pre-expanded syntax.

**Signature versioning**: Bundled stdlib signatures (builtins, cl-lib, seq, etc.)
will need a versioning/sequencing mechanism to handle:

- Incremental additions across Emacs versions (e.g., new functions in Emacs 30)
- Removals and deprecations
- Type signature changes (rare but possible)
- Library version differences (generalises beyond Emacs core)

This likely requires a conditional signature format or version predicate system.
Deferred to post-v1, but architecture should not preclude it.

### 10.5 Resolution Timeline

| Phase | Questions to Resolve |
|-------|---------------------|
| Prototype (2 weeks) | Macro expansion reliability, parser approach |
| MVP (4 weeks) | Incremental performance, basic error quality |
| Beta (8 weeks) | Coverage of common patterns, `.eli` ergonomics |
| Release | Remaining open questions based on user feedback |

---

## Appendix A: Type Checker Phases

```
┌──────────────────────────────────────────────────────────────────┐
│                    Type Checking Pipeline                         │
├──────────────────────────────────────────────────────────────────┤
│                                                                   │
│  Source (.el)                                                     │
│       │                                                           │
│       ▼                                                           │
│  ┌─────────────┐                                                  │
│  │   Parser    │  → Produces surface AST                          │
│  └─────────────┘                                                  │
│       │                                                           │
│       ▼                                                           │
│  ┌─────────────┐                                                  │
│  │   Macro     │  → Calls Emacs subprocess with macroexpand-all   │
│  │  Expansion  │  → Produces core AST                             │
│  └─────────────┘                                                  │
│       │                                                           │
│       ▼                                                           │
│  ┌─────────────┐                                                  │
│  │   Name      │  → Resolves symbols to definitions               │
│  │ Resolution  │  → Builds scope tree                             │
│  └─────────────┘                                                  │
│       │                                                           │
│       ▼                                                           │
│  ┌─────────────┐                                                  │
│  │   Type      │  → Constraint generation                         │
│  │  Inference  │  → Constraint solving via unification            │
│  └─────────────┘  → Generalisation at let-bindings                │
│       │                                                           │
│       ▼                                                           │
│  ┌─────────────┐                                                  │
│  │ Diagnostics │  → Type errors with source locations             │
│  │  Emission   │  → Related information                           │
│  └─────────────┘  → Fix suggestions                               │
│       │                                                           │
│       ▼                                                           │
│  LSP Responses                                                    │
│                                                                   │
└──────────────────────────────────────────────────────────────────┘
```

---

## Appendix B: OCaml Module Structure

```
tart/
├── bin/
│   └── main.ml           ; Entry point
├── lib/
│   ├── syntax/
│   │   ├── lexer.mll     ; Ocamllex lexer
│   │   ├── parser.mly    ; Menhir parser
│   │   └── ast.ml        ; Surface AST types
│   ├── core/
│   │   ├── core_ast.ml   ; Core AST (post-expansion)
│   │   ├── types.ml      ; Type representation
│   │   └── env.ml        ; Type environment
│   ├── typing/
│   │   ├── infer.ml      ; Type inference
│   │   ├── unify.ml      ; Unification
│   │   └── generalize.ml ; Let-generalisation
│   ├── analysis/
│   │   ├── expand.ml     ; Macro expansion (calls Emacs)
│   │   └── resolve.ml    ; Name resolution
│   ├── diagnostics/
│   │   ├── error.ml      ; Error types
│   │   └── format.ml     ; Error formatting
│   └── lsp/
│       ├── server.ml     ; LSP server main loop
│       ├── handlers.ml   ; Request handlers
│       └── protocol.ml   ; JSON-RPC protocol
├── stdlib/
│   ├── builtins.eli      ; Emacs built-in signatures
│   ├── cl-lib.eli        ; cl-lib signatures
│   └── seq.eli           ; seq.el signatures
├── test/
│   ├── infer_test.ml     ; Inference tests
│   └── unify_test.ml     ; Unification tests
└── dune-project
```

---

## Appendix C: Signature File Quick Reference

```elisp
;; ============================================================
;; .eli File Quick Reference
;; ============================================================

;; Module declaration (optional, defaults to filename)
(module my-module)

;; Function signatures (note: grouped parameters)
(sig function-name type)
(sig my-add (-> (Int Int) Int))
(sig my-identity (forall (a) (-> (a) a)))

;; Variable declarations
(defvar var-name type)
(defvar my-default-value String)

;; Type aliases
(type TypeName = type)
(type IntList = (List Int))
(type StringOption = (Option String))

;; Algebraic data types
(data TypeName (type-params...)
  (Constructor1 field-types...)
  (Constructor2 field-types...))

(data Option (a)
  (Some a)
  (None))

(data Result (a e)
  (Ok a)
  (Err e))

;; Import struct from cl-defstruct
(import-struct struct-name)

;; Import from untyped module
(require/typed module-name
  (symbol : type)
  ...)

;; Base types
Nil                                 ; unit type (only nil)
T                                   ; unit type (only t)
Truthy                              ; anything that is not Nil
Bool                                ; T | Nil
Any                                 ; Truthy | Nil (top type)
Never                               ; bottom type (errors)
Int, Float, Num, String, Symbol, Keyword

;; Function types (grouped parameters - Elisp is NOT curried)
(-> (arg-types...) return-type)
(-> (Int Int) Int)                  ; two args, returns Int
(-> (a) (-> (b) c))                 ; returns a function (manual curry)

;; Optional, rest, keyword arguments
(-> (String &optional (Option Int)) String)
(-> (&rest String) String)
(-> (&key :name (Option String) :age (Option Int)) Person)
(-> (&key :name String &allow-other-keys) Result)
(-> (&key :name String &allow-other-keys (Plist Symbol)) Result)

;; Type constructors
(forall (vars...) type)             ; polymorphic
(List elem-type)                    ; lists
(Vector elem-type)                  ; vectors
(Option type)                       ; Truthy | Nil (type must be Truthy)
(Pair a b)                          ; cons cell
(Tuple types...)                    ; fixed tuple
(Or type1 type2)                    ; union
(HashTable key-type value-type)     ; hash tables
(Plist key-type value-type)         ; property lists
user-defined-type                   ; from data/import-struct
```
