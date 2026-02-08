# Spec 76 — Typings Precision: Foundational C-Core Files

Improve the 4 most impactful c-core files by replacing `any` with precise types
using multi-clause dispatch, polymorphism, and union types. Validate changes
against the test suite.

Derived from [Spec 76](./specs/76-typings-distribution.md) deferred items and
[.ralph/IMPLEMENTATION_PLAN.md](./.ralph/IMPLEMENTATION_PLAN.md) Iteration 1.

---

## Task 1 — Precision audit: data.tart

Tighten signatures in `typings/emacs/31.0/c-core/data.tart`:

- `car-safe`: multi-clause `((cons a b) -> a) ((_) -> nil)`
- `cdr-safe`: multi-clause `((cons a b) -> b) ((_) -> nil)`
- `natnump`: multi-clause predicate `((int) -> bool) ((_) -> nil)`
- `nlistp`: multi-clause inverse of `listp`
- `char-or-string-p`: `((int) -> t) ((string) -> t) ((_) -> nil)`
- `integer-or-marker-p`: `((int) -> t) ((marker) -> t) ((_) -> nil)`
- `interactive-form`: tighten arg `any` → `(symbol | (any -> any))`
- Verify `variable-binding-locus` is already precise
- Copy changes to `30.1/` and `29.1/` versions

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 2 — Precision audit: fns.tart

Tighten signatures in `typings/emacs/31.0/c-core/fns.tart`:

- `concat`: `(&rest (string | symbol | (list int) | (vector int))) -> string`
- `vconcat`: `(&rest ((list any) | (vector any) | string | bool-vector)) -> (vector any)`
- `sort`: add vector clause via multi-clause dispatch
- `reverse`/`nreverse`: add vector and string clauses
- `copy-sequence`: multi-clause for list/vector/string preservation
- `plist-put`: tighten to plist type
- Copy changes to `30.1/` and `29.1/` versions

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 3 — Precision audit: eval.tart

Tighten signatures in `typings/emacs/31.0/c-core/eval.tart`:

- `signal`: return type → `never`
- `error`/`user-error`: return type → `never`
- `throw`: return type → `never`
- `commandp`: multi-clause predicate
- `autoload`: tighten parameter types from `any`
- Copy changes to `30.1/` and `29.1/` versions

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 4 — Precision audit: alloc.tart

Verify and tighten signatures in `typings/emacs/31.0/c-core/alloc.tart`:

- `cons`: verify polymorphic `[a b] (a b) -> (cons a b)`
- `list`: verify polymorphic `[a] (&rest a) -> (list a)`
- `vector`: verify polymorphic `[a] (&rest a) -> (vector a)`
- `make-marker`: verify returns `marker`
- `make-finalizer`: verify returns `finalizer`
- Copy changes to `30.1/` and `29.1/` versions

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 5 — Update BUGS.md and sync versions

- Remove entries from `typings/emacs/BUGS.md` that multi-clause dispatch
  resolves (car-safe, cdr-safe overloaded returns)
- Add any new entries for signatures that remain imprecise due to type system
  limitations
- Ensure `30.1/` and `29.1/` directories reflect all changes
- Run full test suite

**Verify:** `nix develop --command dune test --force 2>&1`
