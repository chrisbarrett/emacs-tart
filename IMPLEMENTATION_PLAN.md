# Spec 56: Row-Typed Multi-Clause Signatures

Replace hard-coded row inference functions (`infer_plist_get_row`,
`infer_alist_get_row`, `infer_gethash_row`, `infer_map_elt_row`) with
call-site clause dispatch that generates virtual clauses from row types.

## Architecture

The current approach: 4 pattern-match arms in `infer` (lines 227–256)
dispatch to 4 hand-coded functions (~400 lines total) that implement a
7-case decision table per accessor.

The target: a single `infer_row_accessor` function that:

1. Infers the container arg type
2. Extracts its row (if any) via the existing `extract_*_row` helpers
3. Generates virtual clauses from row fields
4. Tries each virtual clause (literal key match + type unification)
5. Falls through to the generic signature on failure

No AST or sig_loader changes needed. No `.tart` signature changes needed
for the generic fallback—the existing single-clause signatures remain.
Virtual clauses are synthesized at call sites in `infer.ml`.

## Iteration 1: Unified row accessor dispatcher

Replace the 4 pattern-match arms (lines 227–256 in `infer.ml`) with a
single `infer_row_accessor` function.

**What to build:**

Add `infer_row_accessor` that takes:
- `env`: inference environment
- `accessor_kind`: which accessor (`PlistGet | AlistGet | Gethash | MapElt`)
- `key_name`: the literal key string (e.g. `":name"`, `"name"`)
- `container_expr`: the container expression (plist/alist/table/map)
- `rest_args`: remaining optional args
- `span`: call-site span

The function's body is the unified decision table:

```
1. let container_result = infer env container_expr
2. let rest_results = List.map (infer env) rest_args
3. let row_opt = extract_row accessor_kind container_result.ty
4. match row_opt with
   | Some row ->
       match row_lookup row key_name with
       | Some field_ty -> (* Cases 1-2: key in row *) field_ty
       | None ->
           match row.row_var with
           | None -> (* Cases 3-4: closed row, absent *)
               default_or_nil accessor_kind rest_results rest_args
           | Some _ -> (* Case 5: open row, absent *)
               constrain_and_return_option ...
   | None -> (* R8: unknown type, infer from access *)
       constrain_from_access ...
```

The 4 pattern-match arms become:

```ocaml
| List (Symbol ("plist-get", _) :: plist_expr :: Keyword (key, _) :: rest, span) ->
    infer_row_accessor env PlistGet (":" ^ key) plist_expr rest span
| List (Symbol ("alist-get", _) :: List ([Symbol ("quote",_); Symbol (key,_)],_) :: alist :: rest, span) ->
    infer_row_accessor env AlistGet key alist rest span
| List (Symbol ("gethash", _) :: List ([Symbol ("quote",_); Symbol (key,_)],_) :: table :: rest, span) ->
    infer_row_accessor env Gethash key table rest span
| List (Symbol ("map-elt", _) :: map :: List ([Symbol ("quote",_); Symbol (key,_)],_) :: rest, span) ->
    infer_row_accessor env MapElt key map rest span
```

Add helper `extract_row_for_accessor` that dispatches to existing
`extract_plist_row` / `extract_alist_row` / etc. based on accessor kind.

Add helper `constrain_container` that builds the expected type constraint
for the R8 (unknown container type) case, dispatching on accessor kind.

**Delete:** `infer_plist_get_row`, `infer_alist_get_row`,
`infer_gethash_row`, `infer_map_elt_row` (but keep the `extract_*_row`
helpers—they're still needed).

**Files:** `lib/typing/infer.ml`

**Verify:** `dune test`; all 47 row fixture tests still pass

---

## Iteration 2: Virtual clause generation from row types

Add the core of Spec 56: when a literal key matches a row field, return
the field type directly (already done in iteration 1 via the decision
table). When the key does NOT match but the row is open, generate a
virtual clause that constrains the row to include the key.

This iteration adds no new behavior—it's a verification that iteration 1
preserves the existing decision table. The "virtual clause" concept is
realized by the extract-row + row-lookup pattern already implemented.

Create test fixtures that exercise:
- Literal key present in row → exact field type (Case 1)
- Literal key present in closed row → exact field type (Case 2)
- Literal key absent from closed row → nil (Case 3)
- Literal key absent from closed row with default → default type (Case 4)
- Literal key absent from open row → (α | nil) (Case 5)
- Variable key → falls through to `infer_application` generic path (Case 6)
- Row type inferred from usage (R8)

**Files:** test fixtures under `test/fixtures/typing/rows/`

**Verify:** `dune test`; all existing + new fixtures pass

---

## Iteration 3: Verify fallthrough to generic clause

Ensure that when no literal key is present (variable key, Case 6), the
call falls through to `infer_application` which uses the generic
signature from `.tart`. The pattern-match arms only fire on literal
keys; variable-key calls already reach `infer_application`.

Create a test fixture:
```elisp
;; Variable key falls through to generic plist-get signature
(defun get-field (p key)
  (declare (tart [k v] ((plist k v) k) -> (v | nil)))
  (plist-get p key))
```

**Files:** test fixture

**Verify:** `dune test`

---

## Iteration 4: Spec status update + documentation

- Check all task boxes in `specs/56-plist-type-overloading.md`
- Update status to "Complete"
- Update `docs/reference/tart-format.md` if needed

**Files:** spec, docs

**Verify:** `dune test`
