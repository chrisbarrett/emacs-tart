# Implementation Plan

Three specs have new open tasks: [Spec 52](./specs/52-type-predicates.md) (R13,
R5), [Spec 56](./specs/56-plist-type-overloading.md) (R5), and
[Spec 40](./specs/40-content-cache.md) (integration). This plan covers all
remaining work.

## Dependency graph

```
Spec 52 R13 (never subtyping)
  ↓
Spec 52 R5 (or-expression narrowing)

Spec 56 R5 (row-aware clause dispatch)
  → independent of Spec 52

Spec 40 (cache integration)
  → independent of Spec 52 and 56
  → depends on Spec 27 (complete)
```

All three workstreams are independent of each other.

---

## Workstream A: Spec 52 — `never` bottom type and or-narrowing

### Current state

- `Prim.never` exists as `TCon (intrinsic "Never")` (`types.ml:174`)
- Prelude bridges it: `(type never %tart-intrinsic%Never)` (`tart-prelude.tart:40`)
- `error`, `signal`, `throw` already declared as returning `never` in typings
- `is_truthy` already treats `Never` as truthy (`types.ml:414`)
- **Missing:** `never <: T` subtyping rule in `unify.ml`
- **Missing:** `(T | never)` simplification
- **Missing:** or-expression narrowing in `infer_or` (`infer.ml:842`)

### Task A1: `never <: T` subtyping in unify.ml

**File:** `lib/typing/unify.ml`

Add a `never` subtyping rule to the `TCon, TCon` match arm (line 267). When
either side is `Prim.never_name`, unification succeeds — `never` is a subtype
of every type.

```ocaml
(* In the TCon, TCon arm, before the existing numeric subtyping: *)
| TCon n1, TCon n2 ->
    if n1 = n2 then Ok ()
    else
      let is_never name = name = Prim.never_name in
      if is_never n1 || is_never n2 then Ok ()
      else ...existing numeric subtyping...
```

Also handle `never` in mixed-kind arms:

- `TCon name, _` when `name = Prim.never_name` → `Ok ()` (never unifies with
  any type on the other side — `TApp`, `TArrow`, `TUnion`, etc.)
- `_, TCon name` when `name = Prim.never_name` → `Ok ()`

These cases must be added before the main structural match, or as early arms in
the match.

**Expose `never_name`:** Add `val never_name : string` to `types.mli` alongside
`int_name`, `float_name`, `num_name` (currently at line 157–159). Define it in
`types.ml` as `let never_name = intrinsic "Never"`.

**Verify:** `dune test`; write a fixture where `error` return is used in
`string` context — should pass.

### Task A2: `(T | never)` simplification in types.ml

**File:** `lib/core/types.ml`

The empty-union representation (`TUnion []`) and the named never
(`TCon (intrinsic "Never")`) coexist. Add a normalization step: when
constructing or encountering `TUnion` members, filter out `never`. This can be
a helper used by `subtract_type` and by `infer_or`:

```ocaml
let strip_never members =
  List.filter (fun m -> not (equal (repr m) never)) members
```

Apply in `subtract_type` (line 489) — after filtering subtracted members, also
strip `never`. Apply when building union results in `infer_or`.

Also: `subtract_type` currently returns `TUnion []` for empty results. Consider
returning `Prim.never` instead for consistency, though `TUnion []` works as a
bottom type too since the `TUnion ts, t` arm in unify handles empty lists.
Check whether any code pattern-matches on `TUnion []` before changing.

**Verify:** `dune test`; `(string | never)` simplifies to `string`.

### Task A3: or-expression narrowing via `never`-return tracking

**Files:** `lib/typing/infer.ml`, `lib/typing/narrow.ml`

`infer_or` (line 842) currently returns the last argument's type with no
narrowing. The spec requires: when a branch returns `never`, execution can only
reach subsequent code if a preceding predicate was truthy — so the predicate's
narrowing effects apply.

**Pattern:** `(or (pred x) (error "..."))` — two arguments, first is a
predicate call, second returns `never`.

**Implementation:**

In `infer_or`, after inferring all branch types:

1. Check if the last branch returns `never` (via `equal (repr last.ty) Prim.never`)
2. If so, analyze earlier branches for predicate calls using
   `Narrow.analyze_condition`
3. For each predicate found, apply narrowing to the environment for subsequent
   code — this means `infer_or` must return narrowing info that the caller
   (`infer_if`, `infer_progn`, or statement-level inference) can apply

**Challenge:** The current narrowing infrastructure applies narrowing within
`if`/`when`/`unless` branches by modifying the environment passed to branch
inference. `or`-expressions are different: the narrowing applies *after* the
`or`, not inside it. This means `infer_or` needs to propagate narrowing info
upward.

**Approach:** Add a `narrowings` field to the inference result (or use a
side-channel). When `infer_or` detects the `(pred x) (never)` pattern, it
records the predicate narrowing. The caller (typically `infer_progn` or
`infer_body`) applies these narrowings to subsequent statements.

The simplest approach: extend the `infer_result` record with an optional
`env_update` or `narrowings` field. `infer_progn` already threads the
environment through sequential statements — it can apply narrowings from `or`
results to the env before inferring the next statement.

Look at how `infer_if`/`infer_when` currently thread narrowing
(`Narrow.analyze_condition` → modify env). The same mechanism applies, just
triggered from a different site.

**Verify:** `dune test`; fixture:
```elisp
(or (stringp x) (error "Expected string"))
(upcase x)   ; x : string
```

---

## Workstream B: Spec 56 — Row-aware clause dispatch

### Current state

- `clause_dispatch.ml` (232 lines): literal matching + speculative unification,
  no row awareness
- `row_dispatch.ml` (420 lines): hard-coded row inference — detects container
  types, implements the Spec 11 R4 decision table procedurally
- `infer_application` (`infer.ml:1521`): tries clause dispatch first, then row
  dispatch, then generic path
- `unify.ml`: `Clause_matching` context suppresses plist↔list subsumption
- 31 row test fixture pairs under `test/fixtures/typing/rows/`

### Task B1: Row field lookup in clause dispatch

**Files:** `lib/typing/clause_dispatch.ml`, `lib/typing/infer.ml`

Enhance clause dispatch to detect when a clause parameter is a row-typed
container and a call-site argument has a concrete row type with a literal key.
When this combination is detected, perform row field lookup instead of plain
unification.

**Virtual clause approach** (from spec design notes): Rather than making
`clause_dispatch.ml` row-aware, generate virtual clauses from the row type at
the call site. When `infer_application` sees a function with a generic
row-typed clause and a literal key argument:

1. Look up the generic signature (e.g., `plist-get` has clause
   `((plist k v) k &optional any) -> (v | nil)`)
2. Extract the row from the call-site argument's plist type
   (e.g., `{:name string :age int}`)
3. Generate a virtual clause per field:
   - `((plist {:name string & r}) :name &optional any) -> string`
   - `((plist {:age int & r}) :age &optional any) -> int`
4. Try virtual clauses first via existing `Clause_dispatch.try_dispatch`
5. Fall through to the generic clause if no virtual clause matches

This keeps `clause_dispatch.ml` unchanged — the row awareness lives in the
virtual clause generation step in `infer_application`.

**Verify:** `dune test`; plist-get with literal key on a known row type returns
field type.

### Task B2: Open row extension and closed row rejection

**File:** `lib/typing/infer.ml` (virtual clause generation)

Complete the remaining cases from the decision table within clause dispatch:

- **Open row, key absent:** Generate constraint extending the row variable with
  `{key: T & r'}`, return `(T | nil)`
- **Closed row, key absent:** No virtual clause generated for this key, falls
  through to generic clause which returns `(v | nil)` or `nil`
- **Default argument:** When the function accepts a default argument and the key
  is absent from a closed row, the return type is the default's type
- **R8 (container unknown):** When the container argument is a type variable,
  generate a row constraint from the literal key access (currently handled by
  `Row_dispatch.try_dispatch_infer`)

These correspond to Cases 3–5 of the [Spec 11](./specs/11-adt-system.md) R4
decision table.

**Verify:** `dune test`; all 31 row fixtures pass.

### Task B3: Migrate `.tart` signatures

**Files:**
- `typings/emacs/31.0/lisp-core/subr.tart` — `plist-get`
- `typings/emacs/31.0/c-core/data.tart` — `alist-get`
- `typings/emacs/31.0/c-core/fns.tart` — `gethash`
- `typings/emacs/31.0/lisp-core/map.tart` — `map-elt`

Replace current signatures with generic multi-clause forms:

```lisp
;; subr.tart
(defun plist-get [k v] ((plist k v) k &optional any) -> (v | nil))

;; data.tart
(defun alist-get [v] (any (alist any v) &optional any any any) -> (v | nil))

;; fns.tart
(defun gethash [k v] (k (hash-table k v) &optional any) -> (v | nil))

;; map.tart
(defun map-elt [k v] ((map k v) k &optional any) -> (v | nil))
```

The clause dispatch with virtual clause generation (B1–B2) handles the
per-field precision at call sites.

**Verify:** `dune test`; all row fixtures pass with new signatures.

### Task B4: Remove `row_dispatch.ml`

**Files:**
- `lib/typing/row_dispatch.ml` — delete
- `lib/typing/infer.ml` — remove `row_accessor_result` block (lines 1553–1585),
  simplify `infer_application` to use only clause dispatch + generic path
- `lib/tart.mli` — remove `Row_dispatch` module export (line 77)
- `lib/typing/dune` — remove `row_dispatch` from modules list

**Verify:** `dune test`; `dune build`; all tests pass; no references to
`Row_dispatch` remain.

---

## Workstream C: Spec 40 — Cache integration

### Current state

- `content_cache.ml` complete: `compute_key`, `store`, `retrieve`,
  `evict_older_than`, `maybe_evict`
- `compute_key` takes `~binary ~input` only — no `~deps` parameter
- `dependency_graph.ml` complete (`lib/graph/`): `dependencies`, `dependents`,
  cycle detection
- `graph_builder.ml` complete: extracts edges from parsed `.el` and `.tart`
- Type-checking pipeline in `module_check.ml:check_module` (line 689) — no
  cache usage
- CLI entry in `bin/main.ml:check_file` (line 211) — no cache usage

### Task C1: Add `deps` parameter to `compute_key`

**Files:** `lib/cache/content_cache.ml`, `lib/cache/content_cache.mli`

Update signature:

```ocaml
val compute_key : binary:string -> input:string -> deps:string list -> string
```

Implementation: read contents of each dep file, concatenate binary + input +
sorted dep contents, hash the concatenation. Sort deps to ensure deterministic
key regardless of dependency enumeration order.

```ocaml
let compute_key ~binary ~input ~deps =
  let contents =
    List.filter_map read_file_contents
      (binary :: input :: List.sort String.compare deps)
  in
  match contents with
  | [] -> ""
  | parts -> Digest.to_hex (Digest.string (String.concat "" parts))
```

Update the single existing caller in `lib/roundtrip/roundtrip.ml` to pass
`~deps:[]` (roundtrip tests don't need dependency tracking).

**Verify:** `dune test`; changing a dep file produces a different key.

### Task C2: Integrate cache into type-checking pipeline

**File:** `bin/main.ml`

The integration point is `check_file` in `bin/main.ml` (line 211). Wrap the
call to `Module_check.check_module` with cache lookup:

1. **Before type-checking:** Compute the cache key from binary path, input file,
   and transitive dependency file paths
2. **Cache hit:** Deserialize cached diagnostics (using [Spec 35](./specs/35-structured-errors.md)
   format) and return them
3. **Cache miss:** Run `check_module` as normal, serialize the result, store in
   cache

**Dependency resolution for cache key:** Use `Graph_builder.extract_from_sexp`
on the parsed sexps to get direct dependencies, then use the signature search
path to resolve module names to file paths. For transitive deps, either:

- Build a full `Dependency_graph.t` and call `dependencies` — but this requires
  parsing all transitive deps upfront, which is expensive
- Use only direct deps + the c-core/lisp-core typing files as deps (these
  change rarely but must invalidate the cache when they do)

The simpler approach: include the typing directory's content hash as a single
dep rather than tracing the full transitive graph. This is conservative (any
typing change invalidates all caches) but correct and simple.

**Eviction:** Call `Content_cache.maybe_evict ()` once at CLI startup (in
`main.ml`, before processing files).

**Serialization:** Diagnostics are `Tart.Error.t list`. Need a
`to_json`/`of_json` round-trip for `Error.t`. Check whether
[Spec 35](./specs/35-structured-errors.md) already provides this — if so, reuse
it. If not, implement minimal JSON serialization for `Error.t`.

**Verify:** Run `./tart check file.el` twice — second run should be a cache
hit. Modify a `.tart` file — next run should be a cache miss.

---

## Suggested ordering

The three workstreams are independent. Within each:

| Order | Task | Workstream | Rationale |
|-------|------|------------|-----------|
| 1 | A1 | 52 | Foundation: `never <: T` rule |
| 2 | A2 | 52 | Simplification: `(T \| never) → T` |
| 3 | A3 | 52 | Capstone: or-expression narrowing |
| 1 | B1 | 56 | Foundation: row field lookup in clause dispatch |
| 2 | B2 | 56 | Complete: open/closed row handling |
| 3 | B3 | 56 | Migration: `.tart` signatures |
| 4 | B4 | 56 | Cleanup: remove `row_dispatch.ml` |
| 1 | C1 | 40 | Foundation: `deps` in `compute_key` |
| 2 | C2 | 40 | Integration: cache in check pipeline |

Workstream A is smallest (mostly `unify.ml` + `infer.ml` changes). Workstream B
is largest and highest-risk (31 test fixtures to keep passing). Workstream C is
moderate but has the most unknowns around serialization format.

**Recommended start:** Workstream A (smallest, unblocks R5 which is a
user-visible feature).
