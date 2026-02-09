# Implementation Plan

Gap analysis between specs and codebase. Specs 66–80 are fully implemented;
specs 81–89 are not started.

## Status

| Spec | Title | Status |
|:-----|:------|:-------|
| [66](./specs/66-type-system-core.md) | Type System Core | Done |
| [67](./specs/67-polymorphism.md) | Polymorphism | Done |
| [68](./specs/68-type-inference-special-forms.md) | Type Inference & Special Forms | Done |
| [69](./specs/69-signature-files-modules.md) | Signature Files & Modules | Done |
| [70](./specs/70-multi-clause-dispatch.md) | Multi-Clause Dispatch | Done |
| [71](./specs/71-lsp-server.md) | LSP Server | Done |
| [72](./specs/72-cli.md) | CLI | Done |
| [73](./specs/73-diagnostics.md) | Diagnostics | Done |
| [74](./specs/74-emacs-development-mode.md) | Emacs Development Mode | Done |
| [75](./specs/75-signature-major-mode.md) | Signature Major Mode | Done |
| [76](./specs/76-typings-distribution.md) | Typings Distribution | Done |
| [77](./specs/77-testing-infrastructure.md) | Testing Infrastructure | Done |
| [78](./specs/78-parser-fidelity.md) | Parser Fidelity | Done |
| [79](./specs/79-coverage-reporting.md) | Coverage Reporting | Done |
| [80](./specs/80-release-workflow.md) | Release Workflow | Done |
| [81](./specs/81-nil-list-subtyping.md) | Nil-List Subtyping | Not started |
| [82](./specs/82-special-form-parser-extensions.md) | Special Form Parser Extensions | Not started |
| [83](./specs/83-function-subtype-widening.md) | Function Subtype Widening | Not started |
| [84](./specs/84-heterogeneous-list-inference.md) | Heterogeneous List Inference | Not started |
| [85](./specs/85-condition-case-return-typing.md) | Condition-Case Return Typing | Not started |
| [86](./specs/86-record-type-constructor.md) | Record Type Constructor | Not started |
| [87](./specs/87-bounded-quantification.md) | Bounded Quantification | Not started |
| [88](./specs/88-let-type.md) | let-type | Not started |
| [89](./specs/89-mutually-recursive-types.md) | Mutually Recursive Types | Not started |

## Dependencies

```
81 Nil-List Subtyping         (none)
82 Special Form Extensions    (none)
83 Function Subtype Widening  (none)
84 Heterogeneous List         (none)
85 Condition-Case             (none)
86 Record Type Constructor    (none)
87 Bounded Quantification     (none)
88 let-type                   (none)
89 Mutually Recursive Types   → 88
```

## Phases

### Phase 1 — Error Reduction

Targets the two largest error categories in `./tart check` validation.

#### 1. [Spec 82](./specs/82-special-form-parser-extensions.md) — Special Form Parser Extensions

Eliminates ~49% of all errors (7,171 UNDEFINED VARIABLE). No type system
changes — purely form recognition in `check.ml` and `infer.ml`.

| File | Change |
|:-----|:-------|
| `lib/typing/check.ml` | Recognize `defsubst`, `cl-defgeneric`, `cl-defmethod`, `defmacro`, `define-minor-mode`, `defcustom`, `defgroup`, `defface` as top-level forms |
| `lib/typing/infer.ml` | Add inference cases for `pcase-defmacro`, `gv-letplace`, `macroexp-let2`, `declare-function`, `set-advertised-calling-convention`, `gv-define-setter`, `gv-define-expander`, `cl--defalias`; fix `defcustom` first-arg parsing |
| `test/fixtures/typing/` | Fixture pairs for each new form |

#### 2. [Spec 81](./specs/81-nil-list-subtyping.md) — Nil-List Subtyping

Single subtyping rule `nil <: (list a)` plus broadened `car`/`cdr` signatures.
High impact on TYPE MISMATCH errors.

| File | Change |
|:-----|:-------|
| `lib/typing/unify.ml` | Add `nil <: (list a)` case in subtype checking |
| `typings/emacs/31.0/c-core/data.tart` | Add `((any) -> any)` fallback clause to `car`/`cdr` |
| `test/fixtures/typing/` | Fixture pairs for nil-as-list, car/cdr on unions |

#### 3. [Spec 83](./specs/83-function-subtype-widening.md) — Function Subtype Widening

Fixes `add-hook`/`remove-hook` rejecting lambdas with fixed arities.

| File | Change |
|:-----|:-------|
| `lib/typing/unify.ml` | Add `(a₁ ... aₙ) -> r <: (&rest T) -> R` rule in `unify_param_lists` or arrow subtyping |
| `test/fixtures/typing/` | Fixture pairs for lambda-in-union, add-hook patterns |

### Phase 2 — Inference Improvements

#### 4. [Spec 85](./specs/85-condition-case-return-typing.md) — Condition-Case Return Typing

New inference for `condition-case` and `condition-case-unless-debug`. Return
type is the union of body and all handler bodies.

| File | Change |
|:-----|:-------|
| `lib/typing/infer.ml` | Add `infer_condition_case`: parse VAR, BODYFORM, HANDLERS; bind error variable in handler scopes; union body type with handler types |
| `test/fixtures/typing/` | Fixture pairs for condition-case with handlers |

#### 5. [Spec 84](./specs/84-heterogeneous-list-inference.md) — Heterogeneous List Inference

Make `list` a type-checker intrinsic that infers `TTuple` when arguments have
different types.

| File | Change |
|:-----|:-------|
| `lib/typing/infer.ml` | Handle `list` as intrinsic: if all args unify to same type, produce `(list a)`; otherwise produce `TTuple [t₁; ...; tₙ]` |
| `test/fixtures/typing/` | Fixture pairs for homogeneous and heterogeneous list calls |

#### 6. [Spec 86](./specs/86-record-type-constructor.md) — Record Type Constructor

Add opaque `(record tag)` type to the prelude and update signatures.

| File | Change |
|:-----|:-------|
| `typings/tart-prelude.tart` | Add `(type record [a])` opaque type |
| `typings/emacs/31.0/c-core/alloc.tart` | Type `record`/`make-record` to return `(record tag)` |
| `typings/emacs/31.0/c-core/data.tart` | Update `recordp` to multi-clause: `((record _) -> t) ((_) -> nil)` |

### Phase 3 — Signature System

#### 7. [Spec 88](./specs/88-let-type.md) — let-type

Top-level non-exported type alias form, replacing the unused `(let ...)` block
form.

| File | Change |
|:-----|:-------|
| `lib/sig/sig_ast.ml` / `.mli` | Add `DLetType` variant (or rename `DLet`); remove `DLet` block form |
| `lib/sig/sig_parser.ml` | Parse `let-type` keyword with same grammar as `type`; remove `let` block parsing |
| `lib/sig/sig_loader.ml` | Process `let-type` into local type env; exclude from `build_alias_context`/`build_opaque_context` |
| `lib/sig/sig_convert.ml` | Convert `let-type` AST nodes |
| `lib/sig/sig_validation.ml` | Validate `let-type` forms |
| `test/sig/sig_parser_test.ml` | Tests for `let-type` parsing; remove `let` block tests |

#### 8. [Spec 89](./specs/89-mutually-recursive-types.md) — Mutually Recursive Types

Extend `type` and `let-type` to accept multiple bindings per form.

| File | Change |
|:-----|:-------|
| `lib/sig/sig_ast.ml` / `.mli` | Change `type_decl` / `DType` / `DLetType` to carry a list of bindings |
| `lib/sig/sig_parser.ml` | Greedy multi-binding loop after keyword |
| `lib/sig/sig_loader.ml` | Add all names in a group before resolving definitions |
| `lib/sig/sig_convert.ml` | Convert binding groups with mutual visibility |
| `test/sig/sig_parser_test.ml` | Tests for multi-binding parsing |
| `test/fixtures/typing/` | Fixture pairs for mutually recursive types |

### Phase 4 — Advanced Type System

#### 9. [Spec 87](./specs/87-bounded-quantification.md) — Bounded Quantification

Most complex change. Extends the constraint solver with upper-bound constraints
for rest-parameter union inference. Low priority — the workaround (typing
`concat` as `(&rest any) -> string`) is functional.

| File | Change |
|:-----|:-------|
| `lib/typing/unify.ml` | Add `Upper_bound` constraint variant; generate upper bounds when unifying type variables with union types from rest parameter positions |
| `lib/typing/generalize.ml` | Bounded type scheme generalization `∀(a <: U). T`; instantiation creates fresh variables with bounds |
| `lib/core/types.mli` | Extend type scheme representation with bounds |
| `lib/typing/infer.ml` | Thread rest-parameter origin through unification context |
| `typings/emacs/31.0/c-core/fns.tart` | Tighten `concat` to `(&rest (string \| symbol \| (list int) \| (vector int))) -> string` |
| `test/fixtures/typing/` | Fixture pairs for bounded inference, chained calls |
