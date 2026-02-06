# Code Review: Structural Improvements (Phase 2)

Continued code review of the OCaml type-checker codebase. Focuses on
eliminating dead code, consolidating duplication, and tightening catch-all
match arms per CLAUDE.md convention.

## Iteration 1: Remove dead code from public APIs

**Problem:** Several exported functions are never called from library code
or tests:

- `Types.is_tvar` — defined in types.ml, exported in .mli, never called
- `Value.pop_scope` — defined in value.ml, exported in .mli, never called
- `Ansi.warning`, `Ansi.error_code`, `Ansi.init` — defined and exported,
  never called
- `Print.to_strings` — defined and exported, never called
- `Diagnostic.missing_instance` — acknowledged as unimplemented
  ("Type classes are not yet fully implemented")
- `Narrow.subtract_type` — trivial re-export of `Types.subtract_type`

Dead exports enlarge the API surface and confuse readers trying to
understand module interfaces.

**What to build:**

1. Remove `Types.is_tvar` from types.ml and types.mli
2. Remove `Value.pop_scope` from value.ml and value.mli
3. Remove `Ansi.warning`, `Ansi.error_code`, `Ansi.init` from ansi.ml
   and ansi.mli
4. Remove `Print.to_strings` from print.ml and print.mli
5. Remove `Diagnostic.missing_instance` from diagnostic.ml and
   diagnostic.mli
6. Remove `Narrow.subtract_type` re-export from narrow.ml and narrow.mli

**Files to change:**
- `lib/core/types.ml` / `types.mli`
- `lib/interp/value.ml` / `value.mli`
- `lib/typing/ansi.ml` / `ansi.mli`
- `lib/syntax/print.ml` / `print.mli`
- `lib/typing/diagnostic.ml` / `diagnostic.mli`
- `lib/typing/narrow.ml` / `narrow.mli`

**Verify:** `dune build && dune test`

---

## Iteration 2: Extract shared `find_arrow` helper

**Problem:** The `find_arrow` helper (split a sexp list at `->`) is
copy-pasted 4 times:

1. `sig_parser.ml` line ~49 (parse_defun_fields)
2. `sig_parser.ml` line ~364 (parse_arrow_type)
3. `sig_parser.ml` line ~500 (has_arrow_symbol)
4. `infer.ml` line ~1842 (extract_tart_declare)

All have the same structure: `let rec find_arrow before = function
[] -> None | Symbol("->", _) :: after -> Some (List.rev before, after)
| x :: rest -> find_arrow (x :: before) rest`.

**What to build:**

1. Add `find_arrow` to `Sexp` module as a utility (it operates on
   `Sexp.t list` and is purely syntactic)
2. Replace all 4 local definitions with calls to `Sexp.find_arrow`
3. If instance #4 in infer.ml returns a different tuple (includes the
   arrow node), handle that by adapting the call site

**Files to change:**
- `lib/syntax/sexp.ml` / `sexp.mli` — add `find_arrow`
- `lib/sig/sig_parser.ml` — replace 3 local definitions
- `lib/typing/infer.ml` — replace 1 local definition

**Verify:** `dune build && dune test`

---

## Iteration 3: Consolidate map type expansion in sig_loader

**Problem:** In `sig_loader.ml`, the pattern for expanding alist/plist/
hash-table/map with a single row argument repeats the same structure
four times. Each arm converts the arg, checks for TRow, and calls the
appropriate expansion helper.

**What to build:**

1. Create a helper function `expand_map_constructor kind arg_typ` that
   handles the TRow/non-TRow dispatch and calls the right expansion
   based on kind (alist/plist/hash-table/map)
2. Replace the four near-identical arms with calls to this helper

**Files to change:**
- `lib/sig/sig_loader.ml`

**Verify:** `dune build && dune test`

---

## Iteration 4: Clean up wasted computation in infer.ml

**Problem:** In `infer.ml` around line 1470, `default_result` is
inferred and immediately ignored:
```ocaml
let default_result = infer_expr ctx default in
ignore default_result;
```
The comment says the default was already inferred in `rest_results`
above. This is a wasted computation that makes the code harder to
understand.

**What to build:**

1. Verify the default argument is indeed handled by the rest_results
   inference path
2. Remove the dead `infer_expr ctx default` + `ignore default_result`
   lines
3. Add a comment explaining why default is not separately inferred

**Files to change:**
- `lib/typing/infer.ml`

**Verify:** `dune build && dune test`

---

## Iteration 5: Replace `ignore uri` with underscore parameter

**Problem:** In `graph_tracker.ml` line 93, `ignore uri` is used to
suppress an unused parameter warning. The OCaml convention is to use
an underscore-prefixed parameter name instead.

**What to build:**

1. Replace the `ignore uri` call with `_uri` in the parameter name
2. Audit for any other `ignore` calls that suppress unused warnings
   instead of using underscore names

**Files to change:**
- `lib/lsp/graph_tracker.ml`

**Verify:** `dune build && dune test`
