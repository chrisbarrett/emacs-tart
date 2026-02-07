# Implementation Plan: Spec 11 R9 — Literal Types with Deferred Widening

## Background

Currently all literals infer their base type immediately: `42 → int`,
`1.0 → float`, `"hello" → string`, `:kw → keyword`. Spec 11 R9
requires literals to carry their precise literal type and widen only
when context demands it via subtyping during unification.

The subtyping lattice for literals:

```
   truthy              nil
    / | \               |
 num  ...  (cons a b)  ()
 / \
int float
 |    |
 1   1.0   (literal types at bottom)
```

Key properties:
- Within `let` bindings, literal types are preserved (`x : 42`)
- At usage sites, type depends on context: `(list num)` → widens,
  unconstrained → principal type preserved
- Same-value literals unify; different-value same-base literals don't
- All truthy literal types are truthy
- Interaction with existing numeric subtyping: `int <: num`, so
  `42 <: int <: num`

## Files with exhaustive `typ` pattern matches

These files will get compiler errors when `TLiteral` is added, serving
as a checklist: types.ml, types.mli, type_env.ml, unify.ml,
generalize.ml, check.ml, narrow.ml, infer.ml, sig_loader.ml.

---

## Iteration 1: Add TLiteral to type representation

**What to do:**

1. Add `literal_value` type and `TLiteral` constructor to `types.mli`
   and `types.ml`:
   ```ocaml
   type literal_value =
     | LitInt of int
     | LitFloat of float
     | LitString of string
     | LitSymbol of string    (* quoted symbol *)
     | LitKeyword of string   (* :keyword *)

   and typ =
     | ...
     | TLiteral of literal_value * typ  (* value, base type *)
   ```

2. Update `to_string`: display as `42`, `1.0`, `"hello"`, `'foo`,
   `:kw` (matching Elisp syntax).

3. Update `equal`: two TLiterals are equal when their values are equal
   (base types are determined by value, so value equality suffices).

4. Update `is_truthy`: `TLiteral (_, base)` delegates to
   `is_truthy base`.

5. Update `repr`: TLiteral is a ground type (no links to follow),
   return as-is.

6. Update `subtract_type`: TLiteral equality via structural equal.

7. Add `literal_base_type : literal_value -> typ` helper returning the
   base Prim type for each literal kind.

8. Add `literal_value_equal` helper.

**Files:**
- `lib/core/types.mli` — add types + signatures
- `lib/core/types.ml` — add implementations

**Verify:** `nix develop --command dune build 2>&1` — will show
exhaustive match warnings in downstream files (expected; fixed in
subsequent iterations).

---

## Iteration 2: Fix exhaustive matches across the codebase

**What to do:**

Add `TLiteral` arms to every exhaustive match on `typ` in:

1. **generalize.ml**: `collect_generalizable_tvars` — TLiteral has no
   tvars, return acc unchanged. `replace_tvars_with_names` — return
   TLiteral unchanged (no tvars inside).

2. **check.ml**: `substitute_tvar_names` — TLiteral has no tvars,
   return unchanged.

3. **unify.ml**: `occurs_check` — TLiteral base type has no tvars
   (it's a TCon), return Ok. `collect_tvar_refs` — no tvars.
   `restore_tvars` — no tvars.

4. **type_env.ml**: `instantiate_with` — TLiteral has no tvar names
   to substitute, return unchanged.

5. **narrow.ml**: any `match` on typ — TLiteral passthrough.

6. **infer.ml**: any structural matches on typ — TLiteral passthrough.

7. **sig_loader.ml**: `sig_type_to_typ_with_ctx` — no sig_ast form
   produces TLiteral (literals appear only from inference, not
   signatures), so this shouldn't need changes. Any `match ty` arms
   need passthrough.

**Files:** All files listed above.

**Verify:** `nix develop --command dune build 2>&1` — zero warnings.
`nix develop --command dune test --force 2>&1` — all tests pass
(behavior unchanged since no code produces TLiteral yet).

---

## Iteration 3: Produce TLiteral from inference + unification rules

**What to do:**

1. **infer.ml**: Change literal inference to produce TLiteral:
   - `Int (n, _)` → `TLiteral (LitInt n, Prim.int)`
   - `Float (f, _)` → `TLiteral (LitFloat f, Prim.float)`
   - `String (s, _)` → `TLiteral (LitString s, Prim.string)`
   - `Keyword (k, _)` → `TLiteral (LitKeyword k, Prim.keyword)`
   - `Char (_, _)` stays `Prim.int` (chars aren't literal-typed)
   - `Symbol ("nil", _)` stays `Prim.nil`
   - `Symbol ("t", _)` stays `Prim.t`

2. **infer_quoted**: Same changes for quoted int/float/string/keyword.
   Quoted symbols: `Symbol (name, _)` → `TLiteral (LitSymbol name,
   Prim.symbol)`.

3. **unify.ml**: Add TLiteral unification rules:
   - `TLiteral(v1, _), TLiteral(v2, _)` when `v1 = v2` → Ok
   - `TLiteral(_, base), TCon n` → widen: `unify base (TCon n)` (uses
     existing numeric subtyping: int→num)
   - `TCon n, TLiteral(_, base)` → symmetric
   - `TLiteral(_, base), TVar _` → link tvar to the literal (preserve
     precision)
   - `TLiteral(v1, _), TLiteral(v2, _)` when `v1 ≠ v2` → type
     mismatch
   - `TLiteral(_, base), TApp _` → widen: try `unify base (TApp ...)`
   - `TLiteral(_, _), TUnion ts` → widen: try matching any union
     member (through existing union-on-left arm after widening)
   - `TUnion ts, TLiteral(_, _)` → existing union-on-left handles it
     since TLiteral will match base type members

   The key insight: TLiteral always widens to its base type when the
   other side isn't also a TLiteral with the same value. The TVar case
   preserves the literal (links the tvar to TLiteral), so
   unconstrained contexts keep precision.

4. **unify.ml**: TLiteral in `occurs_check` — base is always a TCon,
   no tvars to check.

**Files:**
- `lib/typing/infer.ml` — literal inference
- `lib/typing/unify.ml` — unification rules

**Verify:** `nix develop --command dune test --force 2>&1` — existing
tests may need `.expected` file updates where inferred types become
more precise (e.g., `int` → `42`). Fix any test failures.

---

## Iteration 4: Fix test expectations + test fixtures

**What to do:**

1. Update any failing test expectations (infer_test.ml,
   generalize_test.ml, check_test.ml, etc.) where literal types now
   appear in inferred results.

2. Create `test/fixtures/typing/core/literal_types.{el,expected}`:
   - Integer literal preserves type in let: `(let ((x 42)) x)` → PASS
   - Float literal preserves: `(let ((x 1.0)) x)` → PASS
   - Literal widens to base: `(defun f () (declare (tart (-> int))) 42)` → PASS
   - Literal widens transitively: arithmetic with literal → PASS
   - Different literals don't unify: type mismatch when expected
   - String literal: `(let ((s "hello")) (concat s " world"))` → PASS
   - Keyword literal preserved

3. Verify all 100+ existing fixtures still pass.

**Files:**
- Test files with updated expectations
- `test/fixtures/typing/core/literal_types.{el,expected}`

**Verify:** `nix develop --command dune test --force 2>&1` — all pass.

---

## Iteration 5: Spec status update

**What to do:**

1. Check the [R9] task box in `specs/11-adt-system.md`
2. Update the Status section to reflect completion

**Files:**
- `specs/11-adt-system.md` — check box, update status

**Verify:** All tests pass.
