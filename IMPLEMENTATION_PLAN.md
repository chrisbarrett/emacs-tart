# Implementation Plan — Spec 87: Bounded Quantification

Replace equality constraints with upper-bound constraints when a type variable
is unified with a union from a rest parameter, so callers don't inherit the full
union. Purely inferred — no user-facing syntax changes.

Linked spec: [specs/87-bounded-quantification.md](specs/87-bounded-quantification.md)

---

## Architecture Overview

The change threads through four layers:

1. **Type representation** (`types.mli`, `type_env.ml`) — extend `scheme` with
   optional per-variable upper bounds
2. **Unification** (`unify.ml`) — store upper bounds on tvars and check them
   when a bounded tvar is linked to a concrete type
3. **Generalization** (`generalize.ml`) — collect bounds from tvars and emit
   bounded `Poly` schemes
4. **Instantiation** (`type_env.ml`) — propagate bounds to fresh tvars when
   instantiating bounded schemes

Rest-parameter origin is already implicit: in `unify_param_lists`, `PRest` arms
unify the rest element type with each arg type. The union appears because the
signature declares `(&rest (A | B | C))`. When a tvar meets a union *through*
rest-param unification, we record it as a bound rather than a link.

---

## Task 1 — Extend tvar state with an upper-bound side-table

**Files:** `lib/core/types.mli`, `lib/core/types.ml`

Add a global side-table so that any tvar can carry `a <: U` without being linked
to `U`. A side-table avoids changing the `TVar` constructor payload (which would
touch every constructor site across the codebase).

1. Add a module-level `Hashtbl.t` in `types.ml`:
   ```ocaml
   let tvar_bounds : (tvar_id, typ) Hashtbl.t = Hashtbl.create 16
   ```

2. Expose helpers in `types.mli`:
   - `val set_tvar_bound : tvar_id -> typ -> unit`
   - `val get_tvar_bound : tvar_id -> typ option`
   - `val clear_tvar_bounds : unit -> unit`

3. Update `reset_tvar_counter` to also call `clear_tvar_bounds`.

**Tests:** Unit test that `set_tvar_bound`/`get_tvar_bound` round-trip and that
`clear_tvar_bounds` empties the table.

---

## Task 2 — Record upper bound during rest-param unification

**Files:** `lib/typing/unify.ml`

When a tvar is unified with a union type through a `PRest` path, record the
union as an upper bound instead of linking:

1. Add a `~from_rest:bool` optional parameter (default `false`) to the internal
   `unify` function. Thread it from the `PRest` arms in `unify_param_lists`:
   - In `[PRest elem_ty1], params2`: pass `~from_rest:true` when unifying
     `elem_ty1` with each param's type
   - In `params1, [PRest elem_ty2]`: same
   - In the mid-list `PRest` arms: same
   - All other `unify` call sites: leave default `false`

2. In the `TVar tv, ty | ty, TVar tv` case, when `from_rest = true` AND `ty`
   is `TUnion _`:
   - Perform the occurs check as usual
   - Call `Types.set_tvar_bound id ty` instead of `tv := Link ty`
   - Leave the tvar `Unbound` so generalization can see it

3. When a bounded tvar is later unified (the normal `TVar` case,
   `from_rest = false`):
   - After the occurs check, check `Types.get_tvar_bound id`
   - If a bound `U` exists, verify the concrete type `S` is a subtype of `U`:
     use the existing union-subtype logic (if `U` is `TUnion members`, check
     that `S` matches at least one member via speculative unification)
   - If the check passes, link `tv := Link S` and clear the bound
   - If it fails, produce `TypeMismatch`

4. Update speculative unification (`collect_tvar_refs`/`restore_tvars`) to
   snapshot and restore bounds for affected tvars. Add a helper
   `snapshot_tvar_bounds`/`restore_tvar_bounds` that saves and restores the
   bound entries for a given set of tvar ids.

**Tests:** Unit tests in `unify_test.ml`:
- tvar unified via rest with `(int | string)` → tvar stays unbound, has bound
- bounded tvar then unified with `int` → succeeds, tvar linked to `int`
- bounded tvar then unified with `float` → TypeMismatch
- speculative unify rollback restores bound state

---

## Task 3 — Generalize bounded tvars into bounded schemes

**Files:** `lib/core/type_env.mli`, `lib/core/type_env.ml`,
`lib/typing/generalize.ml`

Extend `scheme` to carry per-variable bounds, and teach generalization to
collect them:

1. Change `scheme` from:
   ```ocaml
   type scheme = Mono of typ | Poly of string list * typ
   ```
   to:
   ```ocaml
   type scheme =
     | Mono of typ
     | Poly of poly_scheme

   and poly_scheme = {
     ps_vars : string list;
     ps_bounds : (string * typ) list;
     ps_body : typ;
   }
   ```
   This is a breaking change to a widely-used type. Update all pattern
   matches on `Poly` across the codebase — most need only `ps_vars`/`ps_body`
   and can use `ps_bounds = []`.

2. In `generalize.ml`, after collecting generalizable tvar ids and creating the
   var_map, look up each id's bound via `Types.get_tvar_bound`. For each found
   bound, apply `replace_tvars_with_names` to the bound type (since it may
   reference other tvars being generalized), and include it in `ps_bounds`.

3. Update `scheme_to_string` to display bounds:
   `(forall ((a <: (int | string))) ...)`.

4. Update `instantiate` (in `type_env.ml`): when instantiating a `Poly` scheme,
   for each `(var_name, bound_ty)` in `ps_bounds`:
   - Look up the fresh tvar created for `var_name` in the substitution
   - Get its tvar_id
   - Apply the same substitution to `bound_ty` (replacing other bound vars with
     their fresh tvars)
   - Call `Types.set_tvar_bound fresh_id substituted_bound`
   - This propagates bounds through chained calls

**Tests:** Unit tests:
- Generalize a type with a bounded tvar → scheme has non-empty `ps_bounds`
- Instantiate the scheme → fresh tvar has the bound set
- Instantiate and unify with valid subtype → succeeds
- Instantiate and unify with invalid type → fails

---

## Task 4 — End-to-end integration tests

**Files:** `test/fixtures/typing/`, `test/typing/check_test.ml` or
`test/typing/infer_test.ml`

Write fixture tests that exercise the full pipeline:

1. **Basic rest-param bounded inference:**
   Create a fixture with a signature declaring `(&rest (string | symbol))` and
   a function that passes an inferred param to it. Verify the param accepts
   `string` and `symbol` but rejects `int`.

2. **Chained bounded inference:**
   Function `f` calls the rest-param function, then function `g` calls `f`.
   Verify the bound propagates through.

3. **Non-rest unions unchanged:**
   A function with a fixed param typed as `(int | string)` — verify equality
   constraint still applies (caller gets `(int | string)`, not a bound).

4. **Multiple rest args:**
   Calling a rest-param function with two parameters — both should acquire
   independent bounds.

---

## Task 5 — Tighten `concat` signature and fix fallout

**Files:** `typings/emacs/31.0/c-core/fns.tart`, test fixtures

1. Replace:
   ```lisp
   (defun concat (&rest any) -> string)
   ```
   with:
   ```lisp
   (defun concat (&rest (string | symbol | (list int) | (vector int))) -> string)
   ```

2. Remove the explanatory comment about why the signature was kept loose.

3. Run the full test suite and the corpus check. Fix any fixture or corpus
   regressions caused by the tighter signature.

**Depends on:** Tasks 1–4 complete and passing.

---

## Task Ordering

```
Task 1 (tvar bounds table)
  └─► Task 2 (rest-param bound recording + unification checking)
        └─► Task 3 (scheme extension + generalization + instantiation)
              └─► Task 4 (integration tests)
                    └─► Task 5 (tighten concat)
```

Each task is independently testable. Tasks 1–3 are the core mechanism; Task 4
validates end-to-end; Task 5 is the payoff.

---

## Risks

- **`Poly` refactor breadth.** Changing `scheme` touches every file that pattern
  matches on `Poly`. Mitigate by using a record — most sites only need
  `ps_vars`/`ps_body` and can use `ps_bounds = []`.

- **Speculative unification rollback.** The bounds side-table must be
  snapshot/restored during `try_unify`. Missing this leaks bounds across
  speculative branches.

- **Corpus regressions from tighter `concat`.** Some callers may rely on `any`
  accepting arbitrary types. These need individual assessment — genuine typing
  bugs vs. legitimate patterns needing a wider union.
