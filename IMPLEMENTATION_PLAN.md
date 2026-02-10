# Implementation Plan

Gap analysis of specs 81–93 against the codebase.

## Status Summary

| Spec | Feature | Status |
|:-----|:--------|:-------|
| [81](specs/81-nil-list-subtyping.md) | Nil-list subtyping | Done |
| [82](specs/82-special-form-parser-extensions.md) | Special form parser extensions | Done |
| [83](specs/83-function-subtype-widening.md) | Function subtype widening | Done |
| [84](specs/84-heterogeneous-list-inference.md) | Heterogeneous list inference | Done |
| [85](specs/85-condition-case-return-typing.md) | Condition-case return typing | Done |
| [86](specs/86-record-type-constructor.md) | Record type constructor | Done |
| [87](specs/87-bounded-quantification.md) | Bounded quantification | Done |
| [88](specs/88-let-type.md) | Let-type | Done |
| [89](specs/89-mutually-recursive-types.md) | Mutually recursive types | Done |
| [90](specs/90-contravariant-function-subtyping.md) | Contravariant function subtyping | Done |
| [91](specs/91-tuple-element-access.md) | Tuple element access | Done |
| [92](specs/92-hook-arity-checking.md) | Hook arity checking | Not started |
| [93](specs/93-structural-record-types.md) | Structural record types | Done |

## Priority 1: Critical Path

- [x] Update `type-of` signature for records — [Spec 86](specs/86-record-type-constructor.md).
  Updated to multi-clause polymorphic signature with record-tag clause.
  Also fixed literal type preservation during clause matching in
  `unify.ml` so that singleton tags are not widened to base types.

## Priority 2: Core Features

- [x] Contravariant function subtyping —
  [Spec 90](specs/90-contravariant-function-subtyping.md). Added
  contravariant retry to `unify_param` in `lib/typing/unify.ml`: when
  invariant (equality) unification fails for same-kind parameters with
  a type mismatch, speculatively retry with swapped direction to check
  `p_expected <: p_actual`. Guarded against bare tvars on both sides to
  prevent unsound inference. Return types are already covariant via
  existing subtyping rules.

- [x] Tuple element access via `nth` and `elt` intrinsics —
  [Spec 91](specs/91-tuple-element-access.md). Added `nth` and `elt`
  as type-checker intrinsics in `lib/typing/infer.ml`. When the
  sequence is a `TTuple` and the index is a literal integer, returns
  the precise element type. Out-of-bounds literals return `nil`.
  Non-literal indices return the union of all element types plus `nil`.
  Non-tuple sequences fall back to signature-based typing from
  `fns.tart`.

- [x] Structural record types / `defstruct` —
  [Spec 93](specs/93-structural-record-types.md). Added `DDefstruct`
  AST node, parser, validator, and loader expansion. `(defstruct name
  (field type) ...)` generates `make-NAME` constructor, `NAME-p`
  predicate with narrowing, and per-field `NAME-FIELD` accessors.
  Supports `:keyword-constructor` and nullable field optionality.
  Added `cl-defstruct` macro to `typings/tart-macros.el`.

## Priority 3: Enhancements

- [ ] Hook arity checking —
  [Spec 92](specs/92-hook-arity-checking.md). Make `add-hook` and
  `run-hook-with-args` intrinsics that extract the hook contract from
  typed hook variables `(list F)` and validate function arity.
  Requires typed hook variable declarations in typings files. Depends
  on Spec 90 (contravariant subtyping) for full function-type
  compatibility checks.

## Discovered Issues

- `IMPLEMENTATION_PLAN.md` (this file) previously covered only specs
  83, 84, 86 deferred items. Those deferred items have been promoted
  to standalone specs 90–93. The old plan is superseded.

- `type-of` and `cl-type-of` in `data.tart:124-127` both return plain
  `symbol`. `cl-type-of` could also benefit from the record-tag
  refinement, but the spec only covers `type-of`.
