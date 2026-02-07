# Implementation Plan: Specs 56 & 57

## Background

Spec 56 (row-typed multi-clause signatures) and Spec 57 (clause diagnostics)
both extend multi-clause dispatch in `infer.ml`. Spec 56 adds call-site
overload resolution with literal matching and virtual clause generation from
row types. Spec 57 adds user-authored diagnostics attached to clauses.

**What's already done:**

- Spec 54 (multi-clause signatures): complete
- Spec 55 (plist intrinsic): complete
- Spec 56 iteration 1–3 (unified `infer_row_accessor` dispatcher, decision
  table test fixtures, variable-key fallback): complete — the 4 per-accessor
  functions are consolidated into a single `infer_row_accessor` at
  `infer.ml:1681`

**Current architecture limitation:** Multi-clause signatures are merged into
a single union-typed `TArrow` at load time (`sig_loader.ml:893–946`). Clause
structure is lost. Both specs require clause-level information to flow
through to inference:

- Spec 56 needs per-clause literal matching and row-field virtual clauses
- Spec 57 needs per-clause diagnostic annotations

The implementation order reflects this: first preserve clause structure
through loading, then add literal matching (56), then add diagnostics (57).

---

## Iteration 1: Preserve clause structure through sig_loader

The loader currently merges all clauses into one union type via
`compute_defun_type`. Instead, preserve the clause list alongside the merged
type so inference can access individual clauses at call sites.

**What to build:**

Add to `lib/core/types.mli`:

```ocaml
type loaded_clause = {
  lc_params : param list;
  lc_return : typ;
  lc_loc : Span.t;
}
```

Extend the function entry in the type environment to carry
`loaded_clause list option` alongside the existing `TArrow` merged type.
When present, `infer_application` can attempt clause-by-clause dispatch;
when absent (e.g. lambda, let-bound), it falls back to the current
constraint-based path.

Update `sig_loader.ml` `compute_defun_type` to produce both the merged union
type (for backward compat) and the clause list (for overload resolution).

**Files:** `lib/core/types.mli`, `lib/core/types.ml`, `lib/sig/sig_loader.ml`

**Verify:** `dune test`; no behavior change yet — clauses are preserved but
not consumed

---

## Iteration 2: Clause-by-clause dispatch in infer_application

Add a new path in `infer_application`: when the function has a clause list,
try each clause top-to-bottom. For each clause, attempt to unify all
arguments with the clause's parameter types in a speculative unification
scope. If unification succeeds, use that clause's return type. If all
clauses fail, fall back to the merged union type path.

**What to build:**

Add `try_clause_dispatch` to `infer.ml`:

```
1. For each clause in order:
   a. Create a speculative unification scope
   b. Unify each argument with the clause's param type
   c. If all succeed → commit scope, return clause's return type
   d. If any fail → rollback scope, try next clause
2. If no clause matched → fall back to union-type constraint path
```

This must handle `&optional` and `&rest` parameter kinds — match actual
argument count against clause arity.

**Files:** `lib/typing/infer.ml`, `lib/typing/unify.ml` (speculative scope)

**Verify:** `dune test`; existing multi-clause predicate signatures still
work (dispatch selects same clause the union would have matched)

---

## Iteration 3: Literal value matching in clause dispatch (Spec 56 R1, R7)

Extend clause dispatch to compare literal arguments (keywords, quoted
symbols) against literal parameter types in clauses. Currently
`sig_parser.ml` parses `STLiteral` for literal values in parameter
position — the loader needs to carry these through, and `try_clause_dispatch`
needs to compare them.

**What to build:**

Add a `TLiteral of string` case to `typ` (or a wrapper in `loaded_clause`)
representing a literal keyword or symbol value in a clause parameter.

In `try_clause_dispatch`, when a clause parameter is a literal:
- Check if the call-site argument is the same literal
- If yes → match succeeds for this param
- If no → clause fails, try next

**Files:** `lib/core/types.mli`, `lib/core/types.ml`,
`lib/sig/sig_loader.ml`, `lib/typing/infer.ml`

**Verify:** `dune test`; add fixtures demonstrating literal `:name` matching
different clauses

---

## Iteration 4: Virtual clause generation from row types (Spec 56 R2–R6)

When `infer_application` sees a call to a row-typed accessor (`plist-get`,
etc.) and the container argument has a known row type, generate virtual
clauses from the row fields and prepend them to the signature's clause list.

**What to build:**

In `infer_application` (or `try_clause_dispatch`), when the function is a
known row accessor and the first relevant argument has a row type:

1. Extract the row via existing `extract_row_for_accessor`
2. For each field `(name, field_ty)` in the row, generate a virtual clause:
   - Parameter: container with row containing `{name : field_ty & r}`,
     literal key matching `name`
   - Return: `field_ty`
3. Append the function's real clauses (the generic fallback) after virtual
   clauses
4. Run `try_clause_dispatch` over the combined list

This replaces the current `infer_row_accessor` special-case with generic
clause dispatch. Once working, delete `infer_row_accessor` and the
`accessor_kind` type.

**Files:** `lib/typing/infer.ml`

**Verify:** `dune test`; all 47+ row fixture tests pass. The decision table
semantics from Spec 11 R4–R8 are preserved.

---

## Iteration 5: Migrate accessor signatures to .tart (Spec 56 R5)

Replace the remaining special-case pattern-match arms for `plist-get`,
`alist-get`, `gethash`, `map-elt` with generic function application. The
`.tart` files already have generic signatures; virtual clause generation
handles field-level precision.

**What to build:**

Remove the pattern-match arms in `infer` (currently ~lines 227–256) that
dispatch to `infer_row_accessor`. These calls should now flow through
`infer_application` → `try_clause_dispatch` → virtual clause generation.

**Files:** `lib/typing/infer.ml`, verify `.tart` signatures are sufficient

**Verify:** `dune test`; update spec 56 task checkboxes

---

## Iteration 6: Clause diagnostic AST and parsing (Spec 57 R1–R3)

Add diagnostic annotations to the clause AST and parser.

**What to build:**

In `lib/sig/sig_ast.mli`, add:

```ocaml
and clause_diagnostic = {
  diag_severity : diagnostic_severity;
  diag_message : string;
  diag_args : string list;
  diag_loc : span;
}

and diagnostic_severity = DiagError | DiagWarn | DiagNote
```

Add `clause_diagnostic : clause_diagnostic option` to `defun_clause`.

In `sig_parser.ml`, after parsing `-> return_type`, check for a trailing
`(warn "...")`, `(note "...")`, or `(error "...")` form:
- Second element: string literal (format string)
- Remaining elements: symbol names (type variable references)
- Validate `%s` count matches argument count
- Validate each argument is a bound type variable in the enclosing defun

**Files:** `lib/sig/sig_ast.mli`, `lib/sig/sig_parser.ml`

**Verify:** `dune build`; add parser tests for diagnostic forms

---

## Iteration 7: Carry diagnostics through loader (Spec 57 R3, R4)

Extend `loaded_clause` (from iteration 1) to carry the diagnostic
annotation. During `sig_loader.ml` processing, validate that diagnostic
argument names resolve to bound type variables and carry the diagnostic
through to the loaded clause.

**What to build:**

Add to `loaded_clause`:

```ocaml
type loaded_clause = {
  lc_params : param list;
  lc_return : typ;
  lc_diagnostic : loaded_diagnostic option;
  lc_loc : Span.t;
}

type loaded_diagnostic = {
  ld_severity : diagnostic_severity;
  ld_message : string;
  ld_args : string list;  (* type var names for %s substitution *)
}
```

In `sig_loader.ml`, when loading a defun with clauses, preserve the
diagnostic on each loaded clause.

**Files:** `lib/core/types.mli`, `lib/core/types.ml`,
`lib/sig/sig_loader.ml`

**Verify:** `dune test`; loaded signatures carry diagnostics

---

## Iteration 8: Emit diagnostics during clause dispatch (Spec 57 R4–R6)

When `try_clause_dispatch` selects a clause with a diagnostic, emit it.

**What to build:**

After clause selection in `try_clause_dispatch`:
1. If the selected clause has a `loaded_diagnostic`:
   - Resolve `%s` placeholders by looking up each type variable name in the
     current substitution and formatting via `Types.to_string`
   - Map severity: `DiagError` → `Severity.Error`, `DiagWarn` →
     `Severity.Warning`, `DiagNote` → `Severity.Info`
   - Emit the diagnostic with the call-site span
2. For `warn`/`note`: return the clause's return type normally (non-blocking)
3. For `error`: emit the diagnostic AND return the clause's return type
   (error recovery — inference continues)

**Files:** `lib/typing/infer.ml`

**Verify:** `dune test`; clause diagnostics appear in output

---

## Iteration 9: Test fixtures and spec cleanup (Spec 57 R7)

Add typing test fixtures for clause diagnostics and update spec statuses.

**What to build:**

Fixtures under `test/fixtures/typing/`:
- Single clause with `warn` (deprecated function)
- Multi-clause with `warn` on fallback clause
- `error` clause that still infers return type
- Format string with type variable substitution
- Clause without diagnostic followed by clause with diagnostic

Update spec task checkboxes:
- `specs/56-plist-type-overloading.md` — check completed tasks
- `specs/57-clause-diagnostics.md` — check completed tasks

**Files:** test fixtures, spec files

**Verify:** `dune test`
