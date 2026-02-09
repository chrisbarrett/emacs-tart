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
| [82](./specs/82-special-form-parser-extensions.md) | Special Form Parser Extensions | Done |
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

**Priority: Spec 82 is next up** — highest-impact change, ready for
implementation.

### Phase 1 — Error Reduction

Targets the two largest error categories in `./tart check` validation.

#### 1. [Spec 82](./specs/82-special-form-parser-extensions.md) — Special Form Parser Extensions

Eliminates ~49% of all errors (7,171 UNDEFINED VARIABLE). Rather than
hardcoding each form in OCaml, a **macro expansion pre-pass** rewrites unknown
defining forms into forms the checker already understands (`defun`, `defvar`,
`progn`). A curated `.el` file provides simplified macro definitions. The
existing pure Elisp interpreter (`lib/interp/`) expands these before the type
checker runs, keeping form knowledge in user-land.

**Design**: In `module_check.ml`, between environment setup and
`Check.check_program` (around line 799). Create an interpreter, load curated
macros from `typings/tart-macros.el`, single-step expand each top-level form,
flatten any resulting `progn` into multiple top-level forms.

| Step | File | Change |
|:-----|:-----|:-------|
| 1 | `lib/typing/dune` | Add `tart.interp` to libraries (no circular dep) |
| 2 | `lib/interp/builtin.ml` | Add `keywordp` predicate (needed by `cl-defmethod` macro) |
| 3 | `typings/tart-macros.el` | Curated macro definitions (new file, see table below) |
| 4 | `lib/typing/module_check.ml` | `expand_defining_forms` + `flatten_toplevel_progn` pre-pass |
| 5 | `test/fixtures/typing/special-forms/` | Fixture pairs for each macro category |

**Macro table** (`typings/tart-macros.el`):

| Macro | Expands to | Notes |
|:------|:-----------|:------|
| `defsubst` | `(defun ...)` | Identical structure |
| `cl-defgeneric` | `(defun ...)` | Drop options after arglist |
| `cl-defmethod` | `(defun ...)` | Skip qualifiers, strip specializers |
| `defmacro` | `(defun ...)` | Treat as function for type checking |
| `pcase-defmacro` | `(defun ...)` | Same as defmacro |
| `define-minor-mode` | `(progn (defvar ...) (defun ...))` | Dual binding |
| `defcustom` | `(defvar ...)` | Variable binding |
| `defgroup` | `(defvar ...)` | Variable binding |
| `defface` | `(defvar ...)` | Variable binding |
| `gv-letplace` | `(let ...)` | Local binding form |
| `macroexp-let2` | `(let ...)` | Local binding form |
| `declare-function` | `(defvar ...)` | Just makes name known |
| `gv-define-setter` | `(defun ...)` | Bind name as function |
| `gv-define-expander` | `(defun ...)` | Bind name as function |
| `cl--defalias` | `(defvar ...)` | Bind name |
| `set-advertised-calling-convention` | `nil` | No-op |

**`module_check.ml` additions**:

```ocaml
let rec flatten_toplevel_progn (sexp : Syntax.Sexp.t) : Syntax.Sexp.t list =
  match sexp with
  | List (Symbol ("progn", _) :: exprs, _) ->
      List.concat_map flatten_toplevel_progn exprs
  | _ -> [ sexp ]

let expand_defining_forms (config : config) (sexps : Syntax.Sexp.t list)
    : Syntax.Sexp.t list =
  match config.stdlib_dir with
  | None -> sexps
  | Some stdlib_dir ->
      let macro_file = Filename.concat stdlib_dir "tart-macros.el" in
      if not (Sys.file_exists macro_file) then sexps
      else
        let global = Interp.Eval.make_interpreter () in
        let macro_parse = Syntax.Read.parse_file macro_file in
        Interp.Expand.load_macros global macro_parse.sexps;
        List.concat_map
          (fun sexp ->
            if Interp.Expand.is_macro_call global sexp then
              match Interp.Expand.expand_1 global sexp with
              | Expanded expanded -> flatten_toplevel_progn expanded
              | Expansion_error _ -> [ sexp ]
            else [ sexp ])
          sexps
```

Insert before `Check.check_program` call:

```ocaml
(* Step 4c: Expand defining-form macros *)
let sexps = expand_defining_forms config sexps in
```

**Edge cases**:

- `cl-defmethod` needs qualifier-skipping logic (`:before`, `:after`,
  `:around`, `:extra "str"`). The `keywordp` builtin handles this.
- `defmacro` macro defined *last* in the file — defensive ordering so
  `load_macros` processes all other `(defmacro ...)` forms via the special
  form path before a user-land `defmacro` macro is registered.

**Test fixtures** (`test/fixtures/typing/special-forms/`):

| Fixture | Tests |
|:--------|:------|
| `defsubst.el` / `.expected` | Define via defsubst, call it → PASS |
| `cl-defmethod.el` / `.expected` | With qualifier and specializer → PASS |
| `define-minor-mode.el` / `.expected` | Use mode variable and function → PASS |
| `defcustom.el` / `.expected` | Reference the variable → PASS |
| `binding-forms.el` / `.expected` | gv-letplace, macroexp-let2 → PASS |
| `declaration-only.el` / `.expected` | declare-function etc. → PASS |

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
