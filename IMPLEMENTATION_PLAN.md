# Spec 55: Plist Intrinsic Type

Make `plist` a compiler intrinsic (like `hash-table`) instead of a type
alias for `(list (k | v))`.

## Iteration 1: Type constructor + display + truthiness (R1, R2)

Add `plist_of` constructor and wire up display/truthiness.

In `lib/core/types.mli` after `hash_table_of`:

```ocaml
val plist_of : typ -> typ -> typ
(** [plist_of k v] creates [(Plist k v)]. *)
```

In `lib/core/types.ml`:

- Add `let plist_of k v = TApp (TCon (intrinsic "Plist"), [ k; v ])` after
  `hash_table_of`
- Add `| "Plist" -> "plist"` to `intrinsic_display_name`
- Add `"Plist"` to both `is_truthy` match arms (TCon and TApp intrinsic
  branches)

**Files:** `lib/core/types.mli`, `lib/core/types.ml`

**Verify:** `Bash(command="nix develop --command dune build 2>&1")`

---

## Iteration 2: Prelude + canonicalization + row expansion (R3, R4, R5)

Update prelude definition and sig_loader canonicalization.

In `typings/tart-prelude.tart`:

- Change `(type plist [k v] (list (k | v)))` to
  `(type plist [k v] (%tart-intrinsic%Plist k v))`

In `lib/sig/sig_loader.ml`:

- Add `| "Plist" -> Types.intrinsic "Plist"` and
  `| "plist" -> Types.intrinsic "Plist"` to `canonicalize_type_name`
- Update `expand_map_row`: change `"plist"` case from
  `Types.list_of (TUnion [ Types.Prim.keyword; arg_typ ])` to
  `Types.plist_of Types.Prim.keyword arg_typ`

**Files:** `typings/tart-prelude.tart`, `lib/sig/sig_loader.ml`

**Verify:** `Bash(command="nix develop --command dune build 2>&1 && nix develop --command dune test --force 2>&1")`

---

## Iteration 3: Extract plist row + concrete map row (R6, R7)

Update row extraction in infer.ml and unify.ml to recognize the new
`(Plist k TRow)` form alongside the legacy `(list (keyword | TRow))`.

In `lib/typing/infer.ml` `extract_plist_row`:

- Add a new match arm **before** the existing one:
  `| TApp (plist_con, [ _key_ty; value_ty ]) when equal (repr plist_con) (TCon (intrinsic "Plist"))` → extract TRow from value_ty
- Keep existing `(list (keyword | TRow))` arm as legacy fallback

In `lib/typing/unify.ml` `extract_concrete_map_row`:

- Add a new match arm for `(Plist keyword TRow)` after the hash-table arm
- Keep existing `(list (keyword | TRow))` arm

Also update `infer_plist_get_row` (and similar) where it constructs
expected plist types: use `Types.plist_of` instead of
`Types.list_of (TUnion [...])`.

**Files:** `lib/typing/infer.ml`, `lib/typing/unify.ml`

**Verify:** `Bash(command="nix develop --command dune test --force 2>&1")`

---

## Iteration 4: Plist-to-list subsumption (R8)

Add unification rule: `(Plist k v)` widens to `(list (k | v))`.

In `lib/typing/unify.ml` `unify`, add a new match arm in the
`TApp, TApp` section (before the normal arity/constructor check):

- If one side is `(Plist k v)` and the other is `(List elem)`:
  unify `(k | v)` with `elem`
- The reverse (list → plist) should NOT match—it falls through to the
  normal TApp mismatch

Helper: add `is_plist` and `is_list_app` predicate functions.

**Files:** `lib/typing/unify.ml`

**Verify:** `Bash(command="nix develop --command dune test --force 2>&1")`

---

## Iteration 5: Cons-chain-to-plist structural promotion (R9)

Add unification rule: a cons chain (n-tuple) with alternating k-v
structure promotes to `(plist k v)`.

In `lib/typing/unify.ml`:

- Add helper `flatten_cons_chain : typ -> (typ list * typ) option` that
  walks nested `(cons a (cons b ... nil))` into a flat element list
- Add match arm: when one side is `(Plist k v)` and the other is a cons
  chain, flatten it, check even length, unify even-position types with
  `k` and odd-position types with `v`
- Odd-length or non-cons → falls through to type mismatch

**Files:** `lib/typing/unify.ml`

**Verify:** `Bash(command="nix develop --command dune test --force 2>&1")`

---

## Iteration 6: Test fixtures + verify existing tests

Add `test/fixtures/typing/rows/plist_intrinsic.{el,expected}`:

- Plist passes where list expected (subsumption)
- Bare list fails where plist expected
- Cons chain with alternating structure promotes to plist
- Cons chain with wrong alternation rejected
- Row-typed plist-get still works
- Existing plist_row_basic, plist_get_precise, etc. still pass

**Files:** test fixtures

**Verify:** `Bash(command="nix develop --command dune test --force 2>&1")`

---

## Iteration 7: Spec status update

- Update `specs/55-plist-intrinsic.md` task checkboxes
