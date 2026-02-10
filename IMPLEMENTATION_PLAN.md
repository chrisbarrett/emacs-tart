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
| [90](specs/90-contravariant-function-subtyping.md) | Contravariant function subtyping | Not started |
| [91](specs/91-tuple-element-access.md) | Tuple element access | Not started |
| [92](specs/92-hook-arity-checking.md) | Hook arity checking | Not started |
| [93](specs/93-structural-record-types.md) | Structural record types | Not started |

## Priority 1: Critical Path

- [x] Update `type-of` signature for records — [Spec 86](specs/86-record-type-constructor.md).
  Updated to multi-clause polymorphic signature with record-tag clause.
  Also fixed literal type preservation during clause matching in
  `unify.ml` so that singleton tags are not widened to base types.

## Priority 2: Core Features

- [ ] Contravariant function subtyping —
  [Spec 90](specs/90-contravariant-function-subtyping.md). Add the
  standard subtyping rule `(A -> R) <: (B -> R)` when `B <: A`
  (contravariant params, covariant return) to `unify_param` in
  `lib/typing/unify.ml`. Only the rest-parameter widening rule exists
  today. Guard against applying contravariance to unresolved tvars.

- [ ] Tuple element access via `nth` intrinsic —
  [Spec 91](specs/91-tuple-element-access.md). Make `nth` a
  type-checker intrinsic in `lib/typing/infer.ml`. When the sequence
  is a `TTuple` and the index is a literal integer, return the precise
  element type. Non-literal indices fall back to union of all elements
  plus nil.

- [ ] Structural record types / `defstruct` —
  [Spec 93](specs/93-structural-record-types.md). Add `defstruct`
  declaration to the `.tart` grammar (`lib/sig/sig_ast.ml`,
  `lib/sig/sig_parser.ml`) and expand to constructor, predicate, and
  accessor signatures in `lib/sig/sig_loader.ml`. Also add
  `cl-defstruct` macro to `typings/tart-macros.el`. Depends on the
  existing opaque `(record tag)` type.

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
