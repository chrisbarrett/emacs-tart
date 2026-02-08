# Spec 70 — Multi-Clause Dispatch

> Consolidates specs: [54](./.archive/54-multi-clause-signatures.md), [56](./.archive/56-plist-type-overloading.md), [57](./.archive/57-clause-diagnostics.md)

## Overview

Multi-clause dispatch is the mechanism by which `defun` signatures express
overloaded behavior, type predicates, row-typed field resolution, and
user-authored diagnostics in a single unified framework. A function declaration
contains one or more ordered clauses tried top-to-bottom at each call site; the
first clause whose parameters unify with the call-site arguments determines the
return type and any associated diagnostic. Virtual clauses generated from row
types at call sites replace hard-coded row inference, and diagnostic annotations
on clauses provide graduated feedback without compiler intrinsics.

## Multi-Clause Signatures

### Syntax

A `defun` with no top-level `->` after its name and binders is parsed as a
multi-clause signature. Each clause is a `(params -> return-type)` form:

```lisp
(defun stringp                        ;; predicate
  ((string) -> t)
  ((_) -> nil))

(defun car [a b]                      ;; polymorphic + multi-clause
  (((cons a b)) -> a)
  ((nil) -> nil))

(defun add (int int) -> int)          ;; single-clause unchanged
```

Clauses are ordered top-to-bottom with pattern-matching semantics. The first
clause whose parameters unify with the call-site arguments wins.

### Wildcards

`_`-prefixed symbols in type position produce fresh type variables (HM
inference), not `any`. Each occurrence is independent. This lets clauses fix
specific parameters while others remain inferred:

```lisp
(defun foo
  ((string int) -> string)
  ((_ _) -> nil))           ;; two independent fresh tvars
```

### Type Variable Binders

Type variable binders declared on the `defun` are shared across all clauses:

```lisp
(defun car [a b]
  (((cons a b)) -> a)
  ((nil) -> nil))
```

### Overall Type Computation

When no specific clause is selected at a call site, the overall type is a
single arrow where:

- `param[i]` = union of clause parameter types at position i
- `return` = union of all clause return types

### Predicate Derivation

Functions returning `t` in some clauses and `nil` in others derive type
predicates for occurrence typing. The checker partitions clauses into
truthy-returning and falsy-returning groups:

- Truthy clauses with concrete params: `narrowed = union(truthy params)`
- Truthy clauses with `_` params: `narrowed = any - union(falsy params)`

| Function   | Derived narrowed type                  |
|:-----------|:---------------------------------------|
| `stringp`  | `string`                               |
| `atom`     | `any - (cons any any)`                 |
| `sequencep`| `(list any) \| (vector any) \| string` |

### Optional and Rest Parameters

Each clause independently supports `&optional` and `&rest` parameters,
following the same syntax as single-clause signatures.

## Virtual Clause Generation

### Motivation

Row-typed map accessors (`plist-get`, `alist-get`, `gethash`, `map-elt`) need
per-field return type precision. Writing a clause for every field is impractical
because the fields depend on the plist type at each call site. The type checker
generates **virtual clauses** from the row type during inference.

### Mechanism

When processing `(plist-get my-plist :name)` where
`my-plist : (plist {:name string :age int})`:

1. Look up the generic `plist-get` signature (a single generic clause)
2. See the plist argument has a row type `{:name string :age int}`
3. Generate a virtual clause for each field:
   - `(((plist {:name string & r}) :name) -> string)`
   - `(((plist {:age int & r}) :age) -> int)`
4. Try virtual clauses first, fall through to the generic clause

The `.tart` file only needs the generic signature:

```lisp
(defun plist-get [k v] ((plist k v) k &optional any) -> (v | nil))
```

The per-field clauses shown in examples are illustrative---they show what the
generated clauses look like, not what users write. User-written multi-clause
signatures with specific row fields remain valid for custom accessors with fixed
schemas.

### Literal Value Matching

Clause matching includes a literal value comparison step. When a clause contains
a literal parameter (e.g. `:name`), the matcher checks:

1. Type unification: `my-plist ~ (plist {:name string & r})`
2. Literal match: `:name` (call arg) = `:name` (clause param)
3. Type unification: remaining args unify with `&optional any`

This distinguishes between `:name` and `:age` clauses at the call site.

### Row Unification During Matching

Clause dispatch handles row types declaratively:

- **Row field lookup**: When a clause parameter is `(plist {k T | r})` and the
  call-site key is a literal `:name`, look up `:name` in the row's field list
  and bind `T` to the field's type
- **Open row extension**: When the literal key is absent from an open row
  `{... & r}`, generate a constraint extending `r` with `{key: T & r'}`
- **Closed row rejection**: When the literal key is absent from a closed row,
  fail the clause match (allowing fallthrough to generic clause)
- **Default argument type binding**: Return types may reference type variables
  bound to optional parameters, extracting the actual argument's type

### Decision Table Coverage

Virtual clause generation covers all cases of the [Spec 66](./66-type-system-core.md) R4 decision
table:

| Case | KEY          | Row    | Result                |
| ---- | ------------ | ------ | --------------------- |
| 1    | literal, in  | any    | `field_type`          |
| 2    | literal, in  | +DEF   | `field_type`          |
| 3    | literal, out | closed | `nil`                 |
| 4    | literal, out | +DEF   | `default`             |
| 5    | literal, out | open   | `(v \| nil)`          |
| 6    | variable     | any    | `join(fields) \| nil` |
| 7    | incompat fn  | any    | error                 |

- Cases 1--2: `generate_virtual_clauses` produces per-field clauses; first
  matching literal clause determines the return type
- Case 3: closed row fails to unify; falls through to generic; returns `nil`
- Case 4: with DEFAULT, generic clause returns `(v | nil)` where `v` includes
  default type
- Case 5: open row unification succeeds with row variable; returns field type
  from row variable
- Case 6: variable key matches generic clause
- Case 7: incompatible function produces a type error

### Removed Code

The hard-coded row inference functions (`infer_alist_get_row`,
`infer_plist_get_row`, `infer_gethash_row`, `infer_map_elt_row`) and the
`row_dispatch.ml` module have been removed. Row helpers live in `infer.ml`
alongside generic function application.

## Clause Diagnostics

### Syntax

A diagnostic annotation follows the return type in a clause:

```
((params) -> return-type
  (SEVERITY fmt-string tvar ...))
```

Where:

- `SEVERITY` is one of `error`, `warn`, `note`
- `fmt-string` is a string literal with `%s` placeholders
- `tvar ...` are zero or more type variable names bound in the enclosing defun
  scope, substituted for `%s` in order via `Types.to_string`

```lisp
;; No format args
(warn "prefer plist type for type-safe property access")

;; With format args referencing defun type variables
(warn "expected (plist _ _), got (list (%s | %s))" k v)
```

Single-clause defuns support diagnostic annotations as well:

```lisp
(defun old-fn (any) -> nil
  (warn "old-fn is deprecated; use new-fn"))
```

### Severity Behavior

- `warn` and `note` are non-blocking: the diagnostic is emitted and the
  clause's return type is used for inference normally
- `error` emits a diagnostic as an error; the clause's return type is still
  used for error recovery (type inference continues)

Severity maps to [Spec 73](./73-diagnostics.md) levels:

| Annotation | Severity          |
|:-----------|:------------------|
| `error`    | `Severity.Error`  |
| `warn`     | `Severity.Warning`|
| `note`     | `Severity.Info`   |

Clause diagnostics respect CLI flags (`--warn-as-error`, `--ignore-warnings`)
identically to other diagnostics.

### Diagnostic Emission

When multi-clause matching selects a clause with a diagnostic at a call site,
the checker emits the diagnostic with:

- Severity from the annotation
- Message with `%s` replaced by inferred types of referenced type variables
- Location pointing to the call expression span in the `.el` file

### Format String Validation

At parse time, the checker validates:

- Number of `%s` placeholders matches number of type variable arguments
- Each argument name is bound as a type variable in the enclosing defun scope

Mismatched `%s` count produces a parse error; unbound names produce a load
error.

### Interaction With Clause Matching

Diagnostics do not alter clause matching order, success, or failure. A clause
matches based on its parameter types. If it matches and has a diagnostic, both
the return type and the diagnostic are produced. The diagnostic is a side-effect
of clause selection, not a filter on it.

An `(error ...)` clause is semantically different from a type mismatch: a type
mismatch means no clause matches (generic unification error), while an error
clause means a clause does match and the match itself is the problem.

### Virtual Clause Exclusion

Virtual clauses generated during row-typed dispatch do not carry diagnostics.
Diagnostics are authored explicitly in `.tart` files and attached only to
clauses written by the signature author.

### Example: plist-member

```lisp
(defun plist-member [k v]
  (((plist k v) k) -> (plist k v))
  (((list (k | v)) k) -> (list (k | v))
    (warn "plist-member on bare list; (plist _ _) enforces alternation")))
```

First clause: well-typed plist, no diagnostic. Second clause: bare list,
accepted with a warning. No compiler intrinsic needed.

## Key Files

| File | Role |
|:-----|:-----|
| `lib/sig/sig_ast.mli` | `defun_clause` with optional `clause_diagnostic`; `diagnostic_severity` |
| `lib/sig/sig_parser.ml` | Multi-clause parsing, `_` wildcards, literal params, diagnostic forms |
| `lib/sig/sig_loader.ml` | Overall type computation, predicate derivation, diagnostic carry-through |
| `lib/typing/infer.ml` | Call-site clause dispatch, virtual clause generation, diagnostic emission |
| `typings/emacs/31.0/lisp-core/subr.tart` | Generic `plist-get` signature |
| `typings/emacs/31.0/c-core/data.tart` | Generic `alist-get` signature |
| `typings/emacs/31.0/c-core/fns.tart` | Generic `gethash` signature |
| `typings/emacs/31.0/lisp-core/map.tart` | Generic `map-elt` signature |

## Deferred

No items currently deferred.
