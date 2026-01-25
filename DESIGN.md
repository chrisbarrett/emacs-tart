# Tart Design Document

Static type checker for Emacs Lisp. OCaml implementation with LSP delivery.

## Architecture

```
.el file → Parser → S-exp AST → Macro Expansion → Core AST → Type Inference → Diagnostics
                                      ↑
                              Pure OCaml interpreter
                              (no Emacs subprocess)
```

```
lib/
├── syntax/     S-expression parser with source locations
├── interp/     Pure Elisp interpreter for macro expansion
├── core/       Type representation and environments
└── typing/     Constraint-based HM inference
```

## Type System

### Truthiness

| Type     | Inhabitants           |
| -------- | --------------------- |
| `Nil`    | `nil` only            |
| `T`      | `t` only              |
| `Truthy` | Anything except `nil` |
| `Bool`   | `T \| Nil`            |
| `Any`    | `Truthy \| Nil` (top) |
| `Never`  | ∅ (bottom)            |

**Invariant:** `(Option a)` requires `a : Truthy` — ensures some/none distinguishable by truthiness.

```elisp
(Option String)    ; Valid
(Option Nil)       ; INVALID
```

### Type Grammar

```
type       ::= base | tvar | (-> (params...) ret) | (forall (vars...) type)
             | (List a) | (Option a) | (Or a b) | (Tuple a...) | user_defined

base       ::= Int | Float | Num | String | Symbol | Keyword
             | Nil | T | Truthy | Bool | Any | Never

params     ::= type | &optional type | &rest type | &key :name type
```

Function args grouped (not curried):

```elisp
(-> (Int Int) Int)           ; Two args
(-> (a) (-> (b) c))          ; Returns function (manual currying)
```

## Inference

Constraint-based HM with levels:

1. Traverse AST, emit `τ₁ = τ₂` constraints
2. Solve via union-find
3. Generalize at let-bindings using levels

```ocaml
type typ =
  | TVar of tvar ref
  | TCon of string
  | TApp of string * typ list
  | TArrow of typ list * typ
  | TForall of string list * typ

and tvar =
  | Unbound of int * int  (* id, level *)
  | Link of typ
```

### Generalization

Only for **syntactic values**: lambda, literals, variables, constructors.

```elisp
(let ((id (lambda (x) x)))
  (id 1) (id "s"))  ; OK: id : (forall (a) (-> (a) a))

(let ((xs (reverse '())))
  xs)  ; xs : (List '_a) — monomorphic (application)
```

## Interpreter

Pure OCaml interpreter for macro expansion. No Emacs subprocess.

**Supported:** `quote`, `if`, `let`, `let*`, `lambda`, `progn`, `setq`,
`defmacro`, `cons`, `car`, `cdr`, `list`, `append`, `nth`, `length`,
`+`, `-`, `*`, `/`, `<`, `>`, `=`, `null`, `atom`, `listp`, `symbolp`,
`stringp`, `numberp`, backquote/unquote.

**Opaque** (require annotation): `intern`, `make-symbol`, `eval`, `load`,
`require`, buffer/window/process ops.

## Signature Files (.eli)

Presence of `foo.eli` triggers type checking of `foo.el`.

```elisp
(module my-utils)

(sig my-add (-> (Int Int) Int))
(sig my-identity (forall (a) (-> (a) a)))

(defvar my-default String)

(type IntList = (List Int))

(data Result (a e)
  (Ok a)
  (Err e))

(require/typed seq
  (seq-map : (forall (a b) (-> ((-> (a) b) (List a)) (List b)))))
```

ADTs generate constructors, predicates (`-p`), and accessors.

## Design Decisions

| Decision                     | Rationale                        |
| ---------------------------- | -------------------------------- |
| No `any` escape              | Sound typing within typed modules |
| Expand-then-type             | Type expanded code, not macros   |
| Pure OCaml interpreter       | Hermetic, no Emacs dependency    |
| `.eli` sibling files         | Opt-in, gradual adoption         |
| Grouped params               | Matches Elisp semantics          |
| Levels-based generalization  | Near-linear performance          |
| `Option` requires `Truthy`   | Preserves nil-punning            |

## Soundness Boundaries

| Feature          | Sound? | Notes                     |
| ---------------- | ------ | ------------------------- |
| Pure functions   | ✓      | Full HM inference         |
| `cl-defstruct`   | ✓      | Direct type correspondence |
| Dynamic vars     | ✗      | Require `.eli` annotations |
| Buffer state     | ✗      | No effect system          |
| Advice           | ✗      | Types change at runtime   |

## Status

**Complete:** Parser, interpreter, type inference, generalization, builtins, diagnostics.

**Planned:** LSP server, `.eli` parser, CLI, Emacs minor mode.

## Invariants

1. AST nodes carry source locations
2. Type variables use union-find with path compression
3. Generalization only at let-bindings, only for syntactic values
4. `Option` argument must unify with `Truthy`
5. Macro expansion preserves source location mapping
6. Opaque boundaries halt and require annotation
