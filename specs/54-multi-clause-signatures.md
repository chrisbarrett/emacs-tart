# Spec 54: Multi-Clause Function Signatures

Replace `(x is T)` / `((name type))` syntax ([Spec 52](./52-type-predicates.md)) with ordered
pattern-matching clauses on defun signatures.

**Deps:** [Spec 07](./07-signature-files.md), [Spec 52](./52-type-predicates.md) (superseded syntax)

## Goal

Express type predicates and overloaded behavior via clause structure instead
of special-purpose `(x is T)` return syntax. Removes named parameters.

## Syntax

```lisp
(defun stringp                        ;; predicate
  ((string) -> t)
  ((_) -> nil))

(defun atom                           ;; inverted predicate
  (((cons any any)) -> nil)
  ((_) -> t))

(defun sequencep                      ;; multi-type predicate
  (((list any)) -> t)
  (((vector any)) -> t)
  ((string) -> t)
  ((_) -> nil))

(defun car [a b]                      ;; polymorphic + multi-clause
  (((cons a b)) -> a)
  ((nil) -> nil))

(defun add (int int) -> int)          ;; single-clause unchanged
```

Clauses are ordered top-to-bottom (pattern-matching semantics).

### `_` wildcards

`_`-prefixed symbols in type position → fresh tvar (HM inference), not
`any`. Each occurrence is independent. Lets clauses fix specific params
while others remain inferred.

```lisp
(defun foo
  ((string int) -> string)
  ((_ _) -> nil))           ;; two independent fresh tvars
```

### Migration from [Spec 52](./52-type-predicates.md)

| Old                                                    | New                                                |
| ------------------------------------------------------ | -------------------------------------------------- |
| `(defun stringp (((x any))) -> (x is string))`        | `(defun stringp ((string) -> t) ((_) -> nil))`     |
| `(defun null (((x any))) -> (x is nil))`              | `(defun null ((nil) -> t) ((_) -> nil))`           |
| `(defun atom (((x any))) -> (x is (any - (cons …))))` | `(defun atom (((cons any any)) -> nil) ((_) -> t))`|

## Constraints

| Constraint          | Detail                                         |
| ------------------- | ---------------------------------------------- |
| Backward-compat     | Single-clause defuns unchanged                 |
| Subsumes predicates | Replaces `(x is T)` and named parameter syntax|
| Ordered             | Clauses tried top-to-bottom                    |
| Inference           | `_`-prefixed types are fresh tvars, not `any`  |

## Requirements

### R1: Multi-clause parsing

No top-level `->` after name/binders → parse remaining forms as
`(params -> return)` clauses. Reuse `parse_params_list`/`parse_sig_type`.

### R2: Single-clause backward compatibility

`(defun add (int int) -> int)` → one-element clause list. No behavior
change for existing signatures.

### R3: `_` wildcard types

`_`-prefixed symbols → `STInfer` → `Types.fresh_tvar` at current level.
Each occurrence independent.

### R4: Overall type computation

Multi-clause → single arrow type:
- param[i] = union of clause param types at position i
- return = union of all clause return types

### R5: Predicate derivation

Analyze clause structure:

1. Partition into truthy-returning / falsy-returning clauses
2. If not a clean partition → no predicate
3. Truthy clauses have concrete params → `narrowed = union(truthy params)`
4. Truthy clauses have `_` params → `narrowed = any - union(falsy params)`

| Function   | Derived narrowed_type                    |
|:-----------|:-----------------------------------------|
| `stringp`  | `string`                                 |
| `atom`     | `any - (cons any any)`                   |
| `sequencep`| `(list any) \| (vector any) \| string`   |

### R6: Existing narrowing preserved

[Spec 52](./52-type-predicates.md) R1-R4, R8-R12 behavior unchanged. `narrow.ml`/`infer.ml`
unaffected — only `predicate_info` source changes.

### R7: Remove old syntax

Remove: `STPredicate` AST node, `st_predicate`, `((name type))` parse arm.

### R8: Type variable binders shared across clauses

```lisp
(defun car [a b]
  (((cons a b)) -> a)
  ((nil) -> nil))
```

## Non-Requirements

- Overload resolution at call sites (phase 2)
- Per-clause return type refinement
- Multi-parameter predicate narrowing
- Automatic predicate inference from bodies

## Design Notes

### AST

```ocaml
and defun_clause = { clause_params : sig_param list;
                     clause_return : sig_type;
                     clause_loc : span }

and defun_decl = { defun_name : string;
                   defun_tvar_binders : tvar_binder list;
                   defun_clauses : defun_clause list;
                   defun_loc : span }
```

### Key files

| File | Change |
|:-----|:-------|
| `lib/sig/sig_ast.mli` | Add `defun_clause`, restructure `defun_decl` |
| `lib/sig/sig_parser.ml` | Multi-clause parsing, `_` wildcard, remove old syntax |
| `lib/sig/sig_loader.ml` | Overall type computation, predicate derivation |
| `lib/typing/narrow.ml` | Unchanged |
| `lib/typing/infer.ml` | Unchanged |
| `typings/emacs/31.0/c-core/data.tart` | Migrate predicates |
| `docs/reference/tart-format.md` | Document new syntax |

### Phase 2: Overload resolution

Future: at call site `(car x)` where `x : (cons int string)`, try clause 1
→ return `int` instead of `int | nil`.

## Tasks

- [x] [R1] Parse multi-clause defun syntax
- [x] [R2] Single-clause backward compatibility
- [x] [R3] Parse `_`-prefixed wildcards as STInfer
- [x] [R4] Compute overall type from clauses
- [x] [R5] Derive predicate_info from clause structure
- [x] [R6] Verify existing narrowing tests pass
- [x] [R7] Remove named param syntax and STPredicate
- [x] [R8] Type variable binders with multi-clause
- [x] Migrate .tart predicate signatures
- [x] Update docs/reference/tart-format.md
- [x] Update [Spec 52](./52-type-predicates.md) status

**Status:** Complete.
