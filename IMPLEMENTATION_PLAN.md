# Implementation Plan: Deferred Work Clusters

Two blocked clusters of deferred work share a common theme — extending the
type-checker and LSP server to handle nuanced dispatch and code actions. This
plan addresses them in dependency order.

---

## Cluster 1 — Plist↔List Subsumption in Clause Dispatch

**Specs:** [56 R5](./specs/56-plist-type-overloading.md),
[57 plist-member](./specs/57-clause-diagnostics.md)

**Problem:** `unify.ml:298–303` allows `(plist k v)` to widen to
`(list (k | v))` during unification. Clause dispatch
(`clause_dispatch.ml:104–147`) uses that same unification, so a plist clause
always swallows bare-list arguments — making the bare-list clause unreachable
and suppressing its diagnostic.

**Approach — context-sensitive unification:** Add a `unify_context` flag
(`Clause_matching | Constraint_solving`) threaded through `unify`. In
`Clause_matching` mode, block plist↔list (and list↔plist) cross-constructor
subsumption so clause dispatch treats them as distinct types. Normal constraint
solving is unaffected.

### Task 1 — Add `unify_context` to unification

Files:
- `lib/typing/unify.mli` — export `unify_context` type; update
  `try_unify_params` signature (or add `try_unify_params_strict`)
- `lib/typing/unify.ml` — add `~context` parameter to `unify` and
  `unify_param_lists`; guard plist↔list subsumption branches on
  `context <> Clause_matching`

Scope: ~25 lines changed.

### Task 2 — Thread context through clause dispatch

Files:
- `lib/typing/clause_dispatch.ml` — pass `Clause_matching` context in
  `try_clause_match` when calling `Unify.try_unify_params`

Scope: ~5 lines changed.

### Task 3 — Write `plist-member` multi-clause signature with diagnostic

Files:
- Appropriate `.tart` signature file (e.g.
  `typings/emacs/31.0/c-core/fns.tart`) — add multi-clause `plist-member`
  with a `(warn ...)` on the bare-list clause
- `test/fixtures/typing/` — add `plist-member-clause.{el,expected}` fixture
  exercising both the plist path (no warning) and the bare-list path (warning
  emitted)

### Task 4 — Migrate map accessors to `.tart` signatures (Spec 56 R5)

With subsumption gated in clause matching, migrate each hard-coded row
inference function to a multi-clause `.tart` signature:

| Function | Hard-coded in | Target `.tart` file |
|----------|---------------|---------------------|
| `plist-get` | `row_dispatch.ml` | `typings/emacs/31.0/c-core/fns.tart` |
| `alist-get` | `row_dispatch.ml` | `typings/emacs/31.0/lisp-core/subr.tart` |
| `gethash` | `row_dispatch.ml` | `typings/emacs/31.0/c-core/fns.tart` |
| `map-elt` | `row_dispatch.ml` | `typings/emacs/31.0/lisp-core/map.tart` |

After each migration passes tests, remove the corresponding
`infer_*_row`/`extract_*_row` function from `row_dispatch.ml`. Goal: delete
`row_dispatch.ml` entirely (~240 lines) and the dispatch call-site in
`infer.ml` (~70 lines).

### Task 5 — Validate with existing test suite

Run `dune test --force` after each task. Existing fixtures in
`test/fixtures/typing/` cover the 7-case decision table from Spec 11 R4–R8 and
must continue to pass without changes.

---

## Cluster 2 — LSP Version-Constraint Code Actions

**Specs:** [50 R13–R15](./specs/50-version-constraints.md),
[49 R14](./specs/49-feature-guards.md)

**Problem:** Version constraint warnings (E0900/E0901) are emitted but the LSP
offers no quickfixes. The code action handler receives `context.diagnostics`
but discards it (`code_action.ml:546`). Three actions are specified in Spec 50.

### Task 6 — Use diagnostic context in code action handler

Files:
- `lib/lsp/code_action.ml` — stop discarding `context`; filter
  `cac_diagnostics` by error code to route to version-specific generators
- `lib/lsp/code_action.mli` — update `handle` signature if needed

### Task 7 — Implement "Bump minimum Emacs version to X.Y"

For E0900 (`VersionTooLow`): edit the `Package-Requires` header in the `.el`
file to raise the Emacs version floor.

Files:
- `lib/lsp/code_action.ml` — add `generate_bump_version_action`
  - Parse `Package-Requires` from document text
  - Produce a `WorkspaceEdit` replacing the version string
- Test with a fixture `.el` file containing a `Package-Requires` header

### Task 8 — Implement "Wrap in feature guard"

For E0900: wrap the offending call in `(when (fboundp 'fn) ...)` or
`(when (featurep 'mod) ...)`.

Files:
- `lib/lsp/code_action.ml` — add `generate_wrap_guard_action`
  - Determine guard kind from the diagnostic (function → `fboundp`, module →
    `featurep`)
  - Produce a `WorkspaceEdit` wrapping the enclosing form

### Task 9 — Implement "Downgrade minimum version to X.Y"

For E0901 (`VersionTooHigh`): lower the `Package-Requires` Emacs version.
Same mechanism as Task 7, opposite direction.

### Task 10 — Implement redundant guard lint (Spec 49 R14)

With version constraints (Spec 50) and feature guards (Spec 49) both complete,
the redundant guard lint compares the guard's target version against the
declared minimum:

- If `(fboundp 'fn)` guards a function available since Emacs 28.1 and the
  package declares `(emacs "29.1")`, the guard is redundant
- Emit a warning diagnostic; optionally offer a "Remove guard" code action

Files:
- `lib/typing/narrow.ml` or `module_check.ml` — add redundancy check during
  version constraint analysis
- `lib/lsp/code_action.ml` — add `generate_remove_guard_action`

---

## Dependency Graph

```
Task 1 ── Task 2 ── Task 3
                 └── Task 4 ── Task 5

Task 6 ── Task 7
       ├── Task 8
       ├── Task 9
       └── Task 10
```

Clusters 1 and 2 are independent and can be worked in parallel.

---

## Risks

- **Subsumption edge cases:** Gating subsumption only for plist↔list may not
  cover future cross-constructor subsumption rules (e.g. cons-chain ↔ plist at
  `unify.ml:304–318`). Review all subsumption branches for the same pattern.
- **Map accessor migration:** Each of the four functions has subtly different
  decision-table semantics (alist default arg, gethash default, map-elt
  testfn). Each migration must be validated individually.
- **Package-Requires parsing:** Elisp `Package-Requires` headers have varied
  formatting. The code action parser needs to handle common variants without a
  full reader.
