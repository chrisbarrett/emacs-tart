# Spec 54 + Spec 07 R17-R19

Replace `(x is T)` / `((name type))` with multi-clause defun. Complete
Spec 07 R17-R19.

## Iteration 1: AST changes

Add `defun_clause`, `STInfer`; restructure `defun_decl`.

In `lib/sig/sig_ast.mli`:

```ocaml
and defun_clause = {
  clause_params : sig_param list;
  clause_return : sig_type;
  clause_loc : span;
}
```

- Replace `defun_params`/`defun_return` with `defun_clauses : defun_clause list`
- Add `| STInfer of string option * span` to `sig_type`
- Update `sig_ast.ml`: constructors, `sig_type_loc` for `STInfer`
- Fix exhaustiveness in `sig_loader.ml` for old `defun_decl` destructuring

**Files:** `lib/sig/sig_ast.mli`, `lib/sig/sig_ast.ml`, `lib/sig/sig_loader.ml`

**Verify:** `Bash(command="nix develop --command dune build 2>&1")`

---

## Iteration 2: Parse multi-clause defun + `_` wildcards

Modify `parse_defun` in `lib/sig/sig_parser.ml`:

- Top-level `->` present → single-clause (wrap in one-element list)
- No top-level `->` → parse remaining forms as `((params) -> ret)` clauses

Parse `_`-prefixed symbols as `STInfer`:
- `_` → `STInfer (None, span)`
- `_foo` → `STInfer (Some "_foo", span)`

Delete old syntax:
- Named param `((name type))` arm (line 425-439)
- `parse_predicate_type` and `(x is T)` match in `parse_list_type`

**Files:** `lib/sig/sig_parser.ml`

**Verify:** `Bash(command="nix develop --command dune build 2>&1")`

---

## Iteration 3: Remove STPredicate

Delete from `sig_ast.mli`/`.ml`:
- `STPredicate` variant, `st_predicate` constructor

Delete from `sig_loader.ml`:
- `STPredicate` arms in `validate_type`, `sig_type_to_typ_with_ctx`, `substitute_sig_type`
- `extract_predicate_info` (old `STPredicate`-based)

Add `STInfer` handling in `sig_loader.ml`:
- `validate_type`: always Ok
- `sig_type_to_typ_with_ctx`: → `Types.fresh_tvar` at current level
- `substitute_sig_type`: pass through

**Files:** `lib/sig/sig_ast.mli`, `lib/sig/sig_ast.ml`, `lib/sig/sig_loader.ml`

**Verify:** `Bash(command="nix develop --command dune build 2>&1")`

---

## Iteration 4: Overall type + predicate derivation from clauses

New functions in `lib/sig/sig_loader.ml`:

**`compute_defun_type`:** Convert clauses to single arrow type.
- `param[i] = union(clause params at i)`, `return = union(clause returns)`
- Wrap in `TForall` if binders present

**`derive_predicate_info`:** Analyze clause structure.
- Partition clauses: truthy-returning (`t`/`truthy`) vs falsy (`nil`)
- Not clean partition → `None`
- Truthy has concrete param[0] → `narrowed = union(truthy params)`
- Truthy has `STInfer` param[0] → `narrowed = any - union(falsy params)` (inverted, e.g. `atom`)

Replace `load_defun_with_ctx` and `load_defun_with_scope` to use these.
Replace `extract_predicate_info` call sites with `derive_predicate_info`.

**Files:** `lib/sig/sig_loader.ml`

**Verify:** `Bash(command="nix develop --command dune build 2>&1 && nix develop --command dune test --force 2>&1")`

---

## Iteration 5: Migrate .tart predicates

Convert all 21 predicates from `(((x any))) -> (x is T)` to multi-clause:

| File | Function | New signature |
|:-----|:---------|:--------------|
| `data.tart` | `null` | `((nil) -> t) ((_) -> nil)` |
| `data.tart` | `symbolp` | `((symbol) -> t) ((_) -> nil)` |
| `data.tart` | `keywordp` | `((keyword) -> t) ((_) -> nil)` |
| `data.tart` | `integerp` | `((int) -> t) ((_) -> nil)` |
| `data.tart` | `floatp` | `((float) -> t) ((_) -> nil)` |
| `data.tart` | `numberp` | `((num) -> t) ((_) -> nil)` |
| `data.tart` | `number-or-marker-p` | `((num) -> t) ((marker) -> t) ((_) -> nil)` |
| `data.tart` | `consp` | `(((cons any any)) -> t) ((_) -> nil)` |
| `data.tart` | `atom` | `(((cons any any)) -> nil) ((_) -> t)` |
| `data.tart` | `listp` | `(((list any)) -> t) ((_) -> nil)` |
| `data.tart` | `vectorp` | `(((vector any)) -> t) ((_) -> nil)` |
| `data.tart` | `stringp` | `((string) -> t) ((_) -> nil)` |
| `data.tart` | `sequencep` | `(((list any)) -> t) (((vector any)) -> t) ((string) -> t) ((_) -> nil)` |
| `data.tart` | `bufferp` | `((buffer) -> t) ((_) -> nil)` |
| `data.tart` | `markerp` | `((marker) -> t) ((_) -> nil)` |
| `fns.tart` | `hash-table-p` | `(((hash-table any any)) -> t) ((_) -> nil)` |
| `frame.tart` | `framep` | `((frame) -> t) ((_) -> nil)` |
| `window.tart` | `windowp` | `((window) -> t) ((_) -> nil)` |
| `process.tart` | `processp` | `((process) -> t) ((_) -> nil)` |
| `buffer.tart` | `overlayp` | `((overlay) -> t) ((_) -> nil)` |
| `eval.tart` | `functionp` | `(((-> (&rest any) any)) -> t) ((_) -> nil)` |

**Files:** `typings/emacs/31.0/c-core/{data,fns,frame,window,process,buffer,eval}.tart`

**Verify:** `Bash(command="nix develop --command dune test --force 2>&1")`

---

## Iteration 6: Multi-clause test fixtures

Fix any regressions in existing `predicate_narrowing.el` / `predicates.el`.

Add `test/fixtures/typing/core/multi_clause.{el,expected}`:
- Single-clause backward compat
- Multi-clause predicate narrowing (`stringp`, `atom`, `sequencep`)
- Polymorphic multi-clause (`car` on cons)

Update `validate_defun` for clause-list validation if needed.

**Verify:** `Bash(command="nix develop --command dune test --force 2>&1")`

---

## Iteration 7: Shadowing errors (Spec 07 R17)

`check_type_not_shadowing`/`check_value_not_shadowing` use `failwith`.
Convert to `Error { message; span }` and thread through
`load_decls_into_state` / `load_scoped_decl` fold.

Add `test/fixtures/typing/errors/shadowing/` fixtures:
- `.tart` opens module then redefines its type
- Expected: "cannot redefine imported binding 'name'"

**Files:** `lib/sig/sig_loader.ml`, test fixtures

**Verify:** `Bash(command="nix develop --command dune test --force 2>&1")`

---

## Iteration 8: Local type aliases `let` (Spec 07 R18)

Add `DLet` to `sig_ast.mli`:

```ocaml
| DLet of let_decl
and let_decl = {
  let_bindings : (string * tvar_binder list * sig_type) list;
  let_body : decl list;
  let_loc : span;
}
```

Grammar: `(let [(type name [vars] def)...] decl...)`

In `sig_loader.ml`: extend alias context for bindings, load body decls,
restore context. Let-bound types not exported/imported. Shadowing within
let scope allowed.

**Files:** `lib/sig/sig_ast.mli`, `lib/sig/sig_ast.ml`, `lib/sig/sig_parser.ml`, `lib/sig/sig_loader.ml`, test fixtures

**Verify:** `Bash(command="nix develop --command dune test --force 2>&1")`

---

## Iteration 9: Auxiliary .tart files (Spec 07 R19)

In `process_open`: check resolved `.tart` has corresponding `.el`.
No `.el` + `open` → error: "cannot open auxiliary module 'name' (no .el file); use include"

`include` unchanged — works for both. De-duplication via `ls_loaded`
already handles repeated includes.

**Files:** `lib/sig/sig_loader.ml`, module resolver, test fixtures

**Verify:** `Bash(command="nix develop --command dune test --force 2>&1")`

---

## Iteration 10: Docs + spec status

- `docs/reference/tart-format.md`: remove `(x is T)` and `((name type))` references
- `specs/52-type-predicates.md`: mark superseded by Spec 54
- `specs/54-multi-clause-signatures.md`: update task checkboxes
