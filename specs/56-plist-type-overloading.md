# Spec 56: Row-Typed Multi-Clause Signatures

Extend multi-clause function signatures to support row-typed pattern matching,
enabling precise field-level type overloading for `plist-get`, `alist-get`,
`gethash`, and `map-elt`.

**Dependencies:** Spec 54 (multi-clause signatures), Spec 55 (plist intrinsic),
Spec 11 R4–R8 (row polymorphism), Spec 48 (prelude)

## Goal

Make row-typed inference for map accessors a library concern rather than a
type-checker builtin. Express field-specific return types via multi-clause
signatures that pattern-match on row types and literal keys.

## Rationale

Currently (Spec 11 R4–R8), `plist-get`, `alist-get`, `gethash`, and `map-elt`
have hard-coded row-typed inference in `infer.ml` (lines 227–256,
1584–1921). This special-case logic bypasses the multi-clause dispatch
mechanism from Spec 54 and cannot be expressed in `.tart` files.

Multi-clause signatures (Spec 54) provide ordered pattern matching on types.
Row polymorphism (Spec 11) provides field-level precision for map types. This
spec bridges them: multi-clause dispatch with row-typed parameters and literal
value matching.

Once implemented, the hard-coded inference functions (`infer_alist_get_row`,
`infer_plist_get_row`, `infer_gethash_row`, `infer_map_elt_row`) become
expressible as `.tart` signatures, removing ~500 lines of special-case code
from `infer.ml`.

## Syntax

Multi-clause signatures with row-typed parameters and literal keys:

```lisp
(defun plist-get [v]
  (((plist {:name string & r}) :name &optional any) -> string)
  (((plist {:age int & r}) :age &optional any) -> int)
  (((plist k v) k &optional any) -> (v | nil)))

(defun alist-get [v]
  (('name (alist {name string & r}) &optional any any any) -> string)
  (('age (alist {age int & r}) &optional any any any) -> int)
  ((k (alist k v) &optional any any any) -> (v | nil)))

(defun gethash [v]
  (('id (hash-table {id int & r}) &optional any) -> int)
  ((k (hash-table k v) &optional any) -> (v | nil)))

(defun map-elt [v]
  (((map {name string & r}) 'name &optional any) -> string)
  ((k (map k v) &optional any) -> (v | nil)))
```

Semantics:
- Clauses are tried top-to-bottom (Spec 54 R4)
- Each clause matches parameters structurally and by literal values
- First matching clause determines return type
- Row unification rules (Spec 11 R11) apply during clause matching

## Constraints

| Constraint              | Detail                                              |
| ----------------------- | --------------------------------------------------- |
| Subsumes R4–R8 builtin  | Replaces hard-coded row inference in `infer.ml`     |
| Literal matching        | Clause parameters can be literal values (`:name`)   |
| Row unification         | Clause matching respects row types (Spec 11 R11)    |
| Fallthrough to generic  | Last clause is always the generic homogeneous case  |
| Backward compatible     | Existing multi-clause signatures unchanged          |

## Requirements

### R1: Literal type parameters in clauses

**Given** a multi-clause signature with literal values in parameter position:

```lisp
(defun plist-get [v]
  (((plist {:name string & r}) :name &optional any) -> string)
  ((_ _ &optional _) -> any))
```

**When** type-checking a call `(plist-get my-plist :name)`
**Then** clause matching checks:
1. Does `my-plist` unify with `(plist {:name string & r})`?
2. Does the literal `:name` match the literal `:name` in the clause?
3. If both succeed → return type is `string`

**Verify:** `dune test`; literal key matching works in multi-clause dispatch

### R2: Row type unification during clause matching

**Given** a clause with an open row type:

```lisp
(defun plist-get [v]
  (((plist {:name string & r}) :name &optional any) -> string))
```

**When** called with a plist typed as `(plist {:name string :age int})`
**Then** clause matching applies row unification:
- `{:name string & r}` unifies with `{:name string :age int}`
- Produces constraint: `r = {:age int}`
- Match succeeds → return `string`

**Verify:** `dune test`; row unification works in clause matching

### R3: Closed row rejection

**Given** a clause with an open row and a call with a closed row:

```lisp
(defun get-name
  (((plist {:name string & r}) :name &optional any) -> string))

;; Call site with closed row
(get-name (the (plist {:age int}) my-plist) :name)
```

**When** clause matching runs
**Then** unification fails: `{:name string & r}` does not unify with `{:age
int}` (closed row, no `:name` field)
**Then** no clause matches → type error

**Verify:** `dune test`; closed rows without required fields fail to match

### R4: Fallthrough to generic clause

**Given** a multi-clause signature with field-specific and generic clauses:

```lisp
(defun plist-get [v]
  (((plist {:name string & r}) :name &optional any) -> string)
  (((plist {:age int & r}) :age &optional any) -> int)
  (((plist k v) k &optional any) -> (v | nil)))
```

**When** called with a variable key:

```lisp
(defun get-field (plist key)
  (plist-get plist key))
```

**Then** clauses 1–2 fail to match (key is not a literal)
**Then** clause 3 matches → infer `plist : (plist k v)`, `key : k`, return
`(v | nil)`

**Verify:** `dune test`; generic clause serves as fallback

### R5: Remove hard-coded row inference functions

**Given** the functions in `infer.ml`:
- `infer_alist_get_row` (lines 1584–1697)
- `infer_plist_get_row` (lines 1703–1786)
- `infer_map_elt_row` (lines 1793–1880)
- `infer_gethash_row` (lines 1887–1975)

**When** R1–R4 are implemented and signatures are migrated to `.tart` files
**Then** these functions can be removed
**Then** pattern-match arms (lines 227–256) fall through to generic function
application

**Verify:** After migration, run `dune test`; all row-typed tests pass without
special-case code

### R6: Preserve existing decision table semantics

**Given** the 7-case decision table from Spec 11 R4:

| Case | KEY          | Row    | Result                      |
| ---- | ------------ | ------ | --------------------------- |
| 1    | literal, in  | any    | `field_type`                |
| 2    | literal, in  | +DEF   | `field_type`                |
| 3    | literal, out | closed | `nil`                       |
| 4    | literal, out | +DEF   | `default`                   |
| 5    | literal, out | open   | `(v \| nil)`                |
| 6    | variable     | any    | `join(fields) \| nil`       |
| 7    | incompat fn  | any    | error                       |

**When** expressed as multi-clause signatures:

```lisp
;; Cases 1–2: literal key in row → field_type
(defun plist-get [v]
  (((plist {:name string & r}) :name &optional any) -> string)
  (((plist {:age int & r}) :age &optional any) -> int)
  ;; Case 3–5: literal key not in row → handled by unification failure + fallthrough
  ;; Case 6: variable key
  (((plist k v) k &optional any) -> (v | nil)))
```

**Then** all 7 cases are covered via:
- Cases 1–2: first matching literal clause
- Case 3: closed row fails to unify → falls through to generic → returns `nil`
- Case 4: with DEFAULT, generic clause returns `(v | nil)` where `v` includes default type
- Case 5: open row unification succeeds with row variable → returns field type from row variable
- Case 6: variable key → matches generic clause

**Verify:** `dune test`; existing row-typed test fixtures pass with
multi-clause signatures

### R7: Literal value matching in clause selection

Clause matching requires a new step: literal value comparison for non-type
parameters.

**Given** a clause `(((plist {:name string & r}) :name &optional any) ->
string)`
**When** matching against call `(plist-get my-plist :name)`
**Then** the matcher checks:
1. Type unification: `my-plist ~ (plist {:name string & r})`
2. Literal match: `:name` (call arg) = `:name` (clause param)
3. Type unification: remaining args unify with `&optional any`

**Verify:** Literal matching distinguishes between `:name` and `:age` clauses

## Non-Requirements

- **Multi-parameter predicate narrowing**: Not attempting to narrow multiple
  variables based on clause structure
- **Automatic predicate inference**: Predicate derivation (Spec 54 R5) remains
  limited to return-type analysis, not parameter patterns
- **Full dependent types**: We do not introduce type-level functions or type
  families. Row lookup is built into clause matching, not a general mechanism
- **Literal type inference for keys**: Keys like `:name` remain typed as
  `keyword`, not the literal type `:name`

## Design Notes

### Per-Call Overload Resolution

This spec introduces **call-site overload resolution** for multi-clause
signatures. Spec 54 R4 computes the overall type as the union of all clause
returns. This spec adds a new inference mode: when the type checker has enough
information at a call site to select a specific clause, it uses that clause's
return type directly instead of the union.

This is the "Phase 2: Overload resolution" flagged as future work in Spec 54.
It is scoped here to multi-clause functions with row-typed parameters and
literal key arguments—the combination that makes map accessor inference work.

### Field Enumeration vs Row Lookup

The `.tart` signatures enumerate specific fields:

```lisp
(defun plist-get [v]
  (((plist {:name string & r}) :name &optional any) -> string)
  (((plist {:age int & r}) :age &optional any) -> int)
  (((plist k v) k &optional any) -> (v | nil)))
```

This is impractical for a general `plist-get` because the fields depend on the
plist type at each call site. The solution: the type checker **generates virtual
clauses** from the row type during inference.

When processing `(plist-get my-plist :name)` where
`my-plist : (plist {:name string :age int})`:

1. Look up the generic `plist-get` signature (the last clause)
2. See the plist argument has a row type `{:name string :age int}`
3. Generate a virtual clause for each field:
   - `(((plist {:name string & r}) :name) -> string)`
   - `(((plist {:age int & r}) :age) -> int)`
4. Try virtual clauses first, fall through to the generic clause

This means the `.tart` file only needs the generic signature. The per-field
clauses shown in examples are **illustrative**—they show what the generated
clauses look like, not what users write. User-written multi-clause signatures
with specific row fields remain valid for custom accessors with fixed schemas.

### Clause Matching Algorithm

Current multi-clause matching (Spec 54) only checks return type for predicate
derivation. For row-typed overloading, clause matching must:

1. **Unify parameter types structurally** (already done for single-clause)
2. **Match literal values** (new: `:name` in clause vs `:name` in call)
3. **Thread row variables through unification** (Spec 11 R11 already handles
   this)
4. **Select first matching clause** (top-to-bottom order, Spec 54)

Matching happens during constraint generation (`infer.ml`), not during
unification. The infer step tries each clause in order until one succeeds.

### AST and Representation

No AST changes needed. Spec 54 already supports:

```ocaml
and defun_clause = { clause_params : sig_param list;
                     clause_return : sig_type;
                     clause_loc : span }
```

Literal values in parameter position are parsed as `STLiteral` in `sig_type`.
The literal matching logic is new constraint-generation behavior, not a syntax
change.

### Integration with Existing Row Inference

Currently, `infer_alist_get_row` et al. are invoked via pattern-match arms in
`infer` before reaching generic function application. After this spec:

1. Parse multi-clause signatures for `plist-get`, `alist-get`, `gethash`,
   `map-elt` from `.tart` files
2. At call sites, `infer_application` tries clauses top-to-bottom
3. Each clause attempt unifies arguments with clause parameters
4. Literal parameters introduce literal-equality constraints
5. First successful unification determines return type
6. Remove special-case pattern arms and `infer_*_row` functions

### Key Files

| File                                        | Change                                 |
| ------------------------------------------- | -------------------------------------- |
| `lib/typing/infer.ml`                       | Add literal matching to clause dispatch; remove hard-coded row functions |
| `lib/sig/sig_loader.ml`                     | Load multi-clause signatures for map accessors |
| `typings/emacs/31.0/c-core/data.tart`       | Multi-clause `alist-get`               |
| `typings/emacs/31.0/lisp-core/subr.tart`    | Multi-clause `plist-get`               |
| `typings/emacs/31.0/lisp-core/map.tart`     | Multi-clause `map-elt`                 |
| `typings/emacs/31.0/c-core/fns.tart`        | Multi-clause `gethash`                 |

### Example Migration

**Before (hard-coded in `infer.ml`):**

```ocaml
(* Lines 234–241 *)
| List ( Symbol ("plist-get", _)
        :: plist_expr
        :: Keyword (key_name, _)
        :: rest_args, span ) ->
    infer_plist_get_row env (":" ^ key_name) plist_expr rest_args span
```

**After (generic signature in `.tart`, clause generation in `infer.ml`):**

```lisp
;; subr.tart — only the generic signature
(defun plist-get [k v] ((plist k v) k &optional any) -> (v | nil))
```

```ocaml
(* infer.ml — generic function application handles all cases *)
| List (fn :: args, span) -> infer_application env fn args span
```

Where `infer_application` now:
1. Looks up `plist-get` signature (single generic clause)
2. Sees the plist argument has a row type `{:name string :age int}`
3. Generates virtual clauses from the row fields
4. Tries each virtual clause against the call-site arguments:
   - Does the literal `:name` match the field name?
   - If yes → return the field's type directly
5. Falls through to the generic clause if no row field matches

## Tasks

- [ ] [R1] Add literal value matching to clause dispatch in `infer.ml`
- [ ] [R2] Verify row unification during clause matching
- [ ] [R3] Test closed row rejection in clause matching
- [ ] [R4] Verify fallthrough to generic clause
- [ ] [R5] Migrate `plist-get`, `alist-get`, `gethash`, `map-elt` to
      multi-clause `.tart` signatures
- [ ] [R5] Remove hard-coded row inference functions from `infer.ml`
- [ ] [R6] Run all existing row-typed test fixtures; verify decision table
      semantics preserved
- [ ] [R7] Document literal matching in clause selection

**Status:** Spec written. Implementation not started. Depends on Spec 54
completion.
