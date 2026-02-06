# Code Review: Structural Improvements

Detailed code review of the OCaml type-checker codebase. Focuses on
tightening module interfaces, eliminating duplication, improving
type-safety, and giving agents better feedback during development.

## Iteration 1: Consolidate duplicated type equality functions

**Problem:** Three separate structural type equality implementations exist:

1. `Types.equal` in `lib/core/types.ml` — the canonical one, handles all
   type variants including `TRow`
2. `Sig_loader.types_equal` in `lib/sig/sig_loader.ml` — used only by
   `subtract_type` in that file, missing `TRow` case (falls through to
   `false`)
3. `Sig_loader.bound_types_equal` — used only by `satisfies_bound` and
   `is_union_member`, also missing `TRow` case

The duplication risks bugs: if a new type variant is added, all three must
be updated. The `sig_loader` copies also lack `TRow` handling.

**What to build:**

1. Replace `Sig_loader.types_equal` with `Types.equal` — they do the same
   thing (structural equality after repr) but `Types.equal` handles all
   variants
2. Replace `Sig_loader.bound_types_equal` with `Types.equal` — same
   situation
3. Delete the local `types_equal`, `bound_types_equal`, and `params_equal`
   functions from `sig_loader.ml`
4. Verify that `subtract_type` and `satisfies_bound` work correctly with
   the canonical `Types.equal`

**Files to change:**
- `lib/sig/sig_loader.ml` — remove duplicates, use `Types.equal`

**Verify:** `dune build && dune test`

---

## Iteration 2: Consolidate duplicated subtract_type functions

**Problem:** Two independent `subtract_type` implementations:

1. `Sig_loader.subtract_type` — used during signature loading for
   `STSubtract` (compile-time subtraction in `.tart` files)
2. `Narrow.subtract_type` — used during type narrowing for predicate
   else-branches

They have subtly different behaviour: `Sig_loader` version returns
`TUnion []` for empty results; `Narrow` version returns the original type.
Neither is clearly correct for all cases.

**What to build:**

1. Move the canonical `subtract_type` into `Core.Types` so both
   sig_loader and narrow can share it
2. Decide on empty-result semantics: `TUnion []` is the more principled
   answer (it represents the empty/never type), so use that
3. Update `Narrow.subtract_type` to call the shared version
4. Update `Sig_loader.subtract_type` to call the shared version
5. Export from `types.mli`

**Files to change:**
- `lib/core/types.ml` / `types.mli` — add `subtract_type`
- `lib/sig/sig_loader.ml` — delegate to `Types.subtract_type`
- `lib/typing/narrow.ml` / `narrow.mli` — delegate to `Types.subtract_type`

**Verify:** `dune build && dune test`

---

## Iteration 3: Reduce sig_loader scope-aware conversion duplication

**Problem:** `sig_type_to_typ_with_scope_ctx` duplicates the entire
conversion logic from `sig_type_to_typ_with_ctx` (STArrow, STForall,
STUnion, STTuple, STSubtract, STRow, STPredicate handling is copied
verbatim). This ~100-line duplication means every new type variant must be
added in two places.

**What to build:**

1. Refactor `sig_type_to_typ_with_scope_ctx` to only handle the three
   cases that actually differ (STVar, STCon, STApp — where the constructor
   might be a scope tvar) and delegate all other variants to
   `sig_type_to_typ_with_ctx` with the combined tvar_names list

2. Create a helper that converts scope_tvars to a substitution map, then
   apply it post-hoc to the result of `sig_type_to_typ_with_ctx`. The
   key insight: for STArrow, STForall, STUnion, STTuple, STSubtract,
   STRow, and STPredicate, the scope-aware and regular conversions differ
   only in recursive calls

3. Similarly deduplicate `sig_param_to_param_with_scope_ctx` by delegating
   to `sig_param_to_param_with_ctx`

**Files to change:**
- `lib/sig/sig_loader.ml` — refactor scope-aware conversion

**Verify:** `dune build && dune test`

---

## Iteration 4: Expand catch-all match arms in diagnostic.ml

**Problem:** Per CLAUDE.md: "Write a match arm for every constructor of
the scrutinee — avoids fallthrough bugs." Several functions in
`diagnostic.ml` use catch-all `| _ ->` arms on `Constraint.context`:

- `arity_mismatch_with_context` has a catch-all for "non-applicable"
  contexts
- If a new context variant is added, the catch-all silently handles it
  instead of producing a compiler warning

**What to build:**

1. In `arity_mismatch_with_context`: replace the catch-all with explicit
   arms for each irrelevant context

2. Audit all `match` expressions on `Constraint.context` in
   `diagnostic.ml` for catch-all arms and expand them

3. Audit `constraint.ml` for any catch-all arms on `context`

**Files to change:**
- `lib/typing/diagnostic.ml` — expand catch-all to explicit arms

**Verify:** `dune build && dune test`

---

## Iteration 5: Improve narrow_type implementation

**Problem:** `Narrow.narrow_type` has a placeholder implementation:
```ocaml
let narrow_type (original : typ) (target : typ) : typ =
  ignore original;
  target
```
It ignores the original type entirely. For unions, it should compute the
intersection: `narrow_type (int | string) string` should yield `string`,
which happens to be what the current code does. But
`narrow_type (int | string | nil) truthy` should yield `(int | string)`,
not `truthy`.

**What to build:**

1. Improve `narrow_type` to compute intersection:
   - `narrow_type (TUnion members) target` → filter members that overlap
     with target, then use the filtered result
   - `narrow_type any target` → target (correct, keep as-is)
   - `narrow_type T target` when `T = target` → T (correct)
   - For truthy narrowing: `narrow_type (T | nil) truthy` → `T`
   - Other cases → target (conservative but correct fallback)

**Files to change:**
- `lib/typing/narrow.ml` / `narrow.mli` — improve narrow_type

**Verify:** `dune build && dune test`

---

## Iteration 6: Update spec 11 checklist to reflect completed work

**Problem:** Spec 11's implementation checklist still shows R4–R15 as
partially or fully unchecked, but the row polymorphism implementation plan
iterations 1–9 completed most of them.

**What to build:**

1. Update specs/11-adt-system.md checklist:
   - [x] R4 — Design B alist expansion, refined return types
   - [x] R5 — plist row types
   - [x] R6 — map pattern exhaustiveness
   - [x] R7 — closed unions vs open rows
   - [x] R8 — row type inference from field access
   - [ ] R9 — deferred (literal types with deferred widening)
   - [x] R11 — row unification rules (already checked)
   - [x] R12 — generic map supertype
   - [x] R13 — all map type forms
   - [x] R14 — equality predicate disjointness
   - [x] R15 — row-to-homogeneous unification

**Files to change:**
- `specs/11-adt-system.md` — update checklist

**Verify:** No code changes, just spec update
