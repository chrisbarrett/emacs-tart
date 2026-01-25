# Tart Design Document

Static type checker for Emacs Lisp. OCaml implementation with LSP delivery.

## Core Architecture

```
.el file → Parser → S-exp AST → Macro Expansion → Core AST → Type Inference → Diagnostics
                                      ↑
                              Pure OCaml interpreter
                              (no Emacs subprocess)
```

## Module Structure

```
lib/
├── syntax/     S-expression parser with source locations
├── interp/     Pure Elisp interpreter for macro expansion
├── core/       Type representation and environments
└── typing/     Constraint-based HM inference
```

---

## Type System

### Truthiness Model

Elisp conflates nil with false. The type system captures this:

| Type | Inhabitants | Notes |
|------|-------------|-------|
| `Nil` | `nil` only | Unit type |
| `T` | `t` only | Unit type |
| `Truthy` | Anything except `nil` | Primitive, not a union |
| `Bool` | `T \| Nil` | Strict boolean |
| `Any` | `Truthy \| Nil` | Top type |
| `Never` | None | Bottom type (errors) |

**Invariant**: `(Option a)` requires `a : Truthy`. This ensures some/none distinguishable by truthiness test.

```elisp
(Option String)    ; Valid: String is Truthy
(Option Int)       ; Valid: Int is Truthy
(Option Nil)       ; INVALID: Nil is not Truthy
```

### Function Types

Elisp functions are NOT curried. All arguments taken at once.

```elisp
(-> (Int Int) Int)           ; Two args, returns Int
(-> (a) (-> (b) c))          ; Returns a function (manual currying)
```

**Wrong**: `(-> Int Int Int)` — This is not valid syntax.

### Type Grammar

```
type ::= base_type | type_var | (-> (params...) return) | (forall (vars...) type)
       | (List a) | (Option a) | (Or a b) | (Tuple a b ...) | user_defined

base_type ::= Int | Float | Num | String | Symbol | Keyword
            | Nil | T | Truthy | Bool | Any | Never

params ::= type | &optional type | &rest type | &key :name type
```

---

## Inference Algorithm

### Choice: Constraint-Based HM with Levels

1. **Constraint generation**: Traverse AST, emit `τ₁ = τ₂` constraints
2. **Unification**: Solve constraints via union-find
3. **Generalization**: At let-bindings, generalize unbound tvars using levels

### Type Variable Representation

```ocaml
type typ =
  | TVar of tvar ref      (* Mutable for union-find *)
  | TCon of string        (* Int, String, etc. *)
  | TApp of string * typ list
  | TArrow of typ list * typ
  | TForall of string list * typ

and tvar =
  | Unbound of int * int  (* id, level *)
  | Link of typ
```

### Let-Generalization Rules

Generalize only when RHS is a **syntactic value**:
- Lambda expressions
- Literals
- Variables
- Constructor applications

```elisp
;; Generalized (lambda is syntactic value)
(let ((id (lambda (x) x)))
  (id 1) (id "s"))  ; OK: id : (forall (a) (-> (a) a))

;; NOT generalized (application is not syntactic value)
(let ((xs (reverse '())))
  xs)  ; xs : (List '_a) — monomorphic
```

---

## Pure Interpreter

Macro expansion runs in OCaml, not Emacs. The interpreter handles:

**Supported**:
- Special forms: `quote`, `if`, `let`, `let*`, `lambda`, `progn`, `setq`
- `defmacro` registration and expansion
- Pure built-ins: `cons`, `car`, `cdr`, `list`, `append`, `nth`, `length`
- Arithmetic: `+`, `-`, `*`, `/`, `<`, `>`, `=`
- Predicates: `null`, `atom`, `listp`, `symbolp`, `stringp`, `numberp`
- Backquote: `` ` ``, `,`, `,@`

**Opaque boundaries** (require type annotation):
- `intern`, `make-symbol` — dynamic symbol creation
- `eval` on computed forms
- `load`, `require` — file I/O
- Buffer/window/process operations

When evaluation hits an opaque boundary, it stops and requires explicit type annotation.

---

## Signature Files (.eli)

`.eli` files declare types for `.el` modules. Presence of `foo.eli` triggers type checking of `foo.el`.

### Syntax

```elisp
(module my-utils)

;; Function signatures
(sig my-add (-> (Int Int) Int))
(sig my-identity (forall (a) (-> (a) a)))

;; Variables
(defvar my-default String)

;; Type aliases
(type IntList = (List Int))

;; ADTs
(data Result (a e)
  (Ok a)
  (Err e))

;; Import untyped module with declared types
(require/typed seq
  (seq-map : (forall (a b) (-> ((-> (a) b) (List a)) (List b)))))
```

### Generated Constructors

ADT declaration generates:
- Constructors: `Ok : (forall (a e) (-> (a) (Result a e)))`
- Predicates: `result-ok-p : (forall (a e) (-> ((Result a e)) Bool))`
- Accessors: `result-ok-value : (forall (a e) (-> ((Result a e)) a))`

---

## Key Design Decisions

| Decision | Rationale |
|----------|-----------|
| No `any` escape hatch | Sound typing within typed modules |
| Expand-then-type | Type the expanded code, not surface macros |
| Pure OCaml interpreter | Hermetic builds, no Emacs dependency |
| `.eli` sibling files | Opt-in typing, gradual adoption |
| Grouped function params | Matches Elisp non-curried semantics |
| Levels-based generalization | Near-linear performance |
| `Option` requires `Truthy` | Preserves nil-punning semantics |

---

## Soundness Boundaries

| Feature | Sound? | Notes |
|---------|--------|-------|
| Pure functions | Yes | Full HM inference |
| `cl-defstruct` | Yes | Direct type correspondence |
| Dynamic variables | No | Require `.eli` annotations |
| Buffer state | No | Effect system too complex |
| Advice system | No | Types change at runtime |

---

## Implementation Status

**Complete**:
- S-expression parser with source locations
- Pure interpreter with macro expansion
- Constraint-based type inference
- Levels-based generalization
- Built-in function types
- Type error diagnostics

**Planned**:
- LSP server
- `.eli` signature file parser
- CLI interface
- Emacs minor mode

---

## Invariants

1. Every AST node carries source location
2. Type variables use union-find with path compression
3. Generalization only at let-bindings, only for syntactic values
4. `Option` type argument must unify with `Truthy`
5. Macro expansion preserves source location mapping
6. Opaque boundaries halt evaluation and require annotation
