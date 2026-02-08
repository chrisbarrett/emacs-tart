# Spec 55: Plist Intrinsic Type

Make `plist` a compiler intrinsic rather than a type alias.

- **Dependencies:**
  - [Spec 07](./07-signature-files.md) (signature files)
  - [Spec 11](./11-adt-system.md) (row polymorphism)
  - [Spec 48](./48-prelude.md) (prelude)

## Goal

Elevate `plist` from a type alias to a compiler intrinsic, enabling stricter type-checking of property lists while preserving compatibility with list operations.

## Rationale

Currently, `plist` is defined in the prelude as `(type plist [k v] (list (k | v)))`. This representation is too loose—it does not enforce the alternating key-value structure that plists require. Any permutation of keys and values would type-check, as would lists with duplicate keys or unbalanced lengths.

Making `plist` an intrinsic (like `hash-table`) allows the type-checker to:
- Distinguish plists from arbitrary lists at the type level
- Reject bare `(list (k | v))` where `(plist k v)` is expected
- Provide a clean row-typed form (`(Plist keyword {row})`)
- Promote cons chains with proven alternating structure to plist types

A plist IS a list, so `(plist k v)` subsumes to `(list (k | v))`. But the
reverse requires structural evidence: a nested cons chain (n-tuple) where the
type checker can verify alternating key-value positions. A bare homogeneous
`(list (k | v))` provides no such evidence and does not unify with
`(plist k v)`.

## Constraints

| Constraint | Detail |
|:-----------|:-------|
| One-way subsumption | `(plist k v)` widens to `(list (k | v))`; reverse requires evidence |
| Structural promotion | Cons chains with alternating k-v structure promote to plist |
| Row expansion | `(plist {:name string & r})` → `(Plist keyword {row})` |
| Intrinsic pattern | Follow `hash-table` implementation pattern |
| Backward compat | Existing plist-typed code continues to work |

## Output

```
lib/core/
├── types.ml        ; Add plist_of constructor, is_truthy case
└── types.mli       ; Add plist_of signature
lib/sig/
└── sig_loader.ml   ; Canonicalize "plist", update expand_map_row
lib/typing/
├── unify.ml        ; Add plist subsumption + promotion rules, update extract_concrete_map_row
└── infer.ml        ; Update extract_plist_row
typings/
└── tart-prelude.tart  ; Change plist to %tart-intrinsic%Plist
```

## Requirements

### R1: Plist intrinsic type constructor

**Given** the `types.ml` module
**When** defining type constructors
**Then** add `val plist_of : typ -> typ -> typ` following the pattern of `hash_table_of`

```ocaml
val plist_of : typ -> typ -> typ
(** [plist_of k v] creates [(Plist k v)]. *)
```

**Given** the plist intrinsic name `%tart-intrinsic%Plist`
**When** displaying types
**Then** format as `plist` (via `intrinsic_display_name`)

**Verify:** `dune build`; plist constructor available

### R2: Truthiness

**Given** a plist type `(plist k v)`
**When** checking truthiness via `is_truthy`
**Then** return `true` (plists are always truthy, like hash-tables)

Add to `types.ml` `is_truthy`:
```ocaml
| TApp (TCon name, _) when is_intrinsic_name name -> (
    match intrinsic_base_name name with
    | "Truthy" | "Never" | "List" | "Vector" | "Pair" | "HashTable" | "Plist" -> true
    | _ -> false)
```

**Verify:** `dune test`; `(plist k v)` recognized as truthy

### R3: Prelude definition

**Given** `typings/tart-prelude.tart`
**When** defining the plist type
**Then** change from `(type plist [k v] (list (k | v)))` to `(type plist [k v] (%tart-intrinsic%Plist k v))`

**Verify:** `./tart check` on prelude succeeds

### R4: Signature loading canonicalization

**Given** a type name `"plist"` in a `.tart` file
**When** `sig_loader.ml` canonicalizes type names
**Then** map `"plist"` to `Types.intrinsic "Plist"` (analogous to `"hash-table"` → `"HashTable"`)

Update `canonicalize_type_name`:
```ocaml
| "hash-table" -> Types.intrinsic "HashTable"
| "plist" -> Types.intrinsic "Plist"
```

**Verify:** `dune test`; plist references resolve to intrinsic

### R5: Row-typed plist expansion

**Given** `(plist {:name string & r})` in a signature
**When** `expand_map_row` processes it
**Then** expand to `(Plist keyword {row})` rather than `(list (keyword | {row}))`

Update `sig_loader.ml` `expand_map_row`:
```ocaml
| "plist" -> Some (Types.plist_of Types.Prim.keyword arg_typ)
```

**Verify:** `dune test`; row-typed plists use intrinsic representation

### R6: Extract plist row pattern

**Given** `extract_plist_row` in `infer.ml`
**When** extracting rows from plist types
**Then** recognize both forms:
- `(Plist k TRow)` (new intrinsic form)
- `(list (k | TRow))` (legacy form for compatibility)

Update `infer.ml`:
```ocaml
and extract_plist_row ty =
  let plist_name = intrinsic "Plist" in
  match repr ty with
  (* New intrinsic form: (Plist keyword TRow) *)
  | TApp (plist_con, [ _key_ty; value_ty ])
    when equal (repr plist_con) (TCon plist_name) -> (
      match repr value_ty with
      | TRow row -> Some row
      | _ -> None)
  (* Legacy form: (list (keyword | TRow)) *)
  | TApp (list_con, [ union_ty ])
    when equal (repr list_con) (TCon (intrinsic "List")) -> (
      (* existing union pattern matching *))
  | _ -> None
```

**Verify:** `dune test`; `plist-get` type inference works with new form

### R7: Extract concrete map row pattern

**Given** `extract_concrete_map_row` in `unify.ml`
**When** extracting rows for unification
**Then** recognize `(Plist k TRow)` pattern alongside alist/hash-table patterns

Update `unify.ml`:
```ocaml
let extract_concrete_map_row ty =
  let plist_name = intrinsic "Plist" in
  (* ... existing patterns ... *)
  (* plist: (Plist keyword TRow) *)
  | TApp (plist_con, [ _key_ty; value_ty ])
    when (match repr plist_con with TCon n -> n = plist_name | _ -> false) -> (
      match repr value_ty with TRow _ -> Some value_ty | _ -> None)
  | _ -> None
```

**Verify:** `dune test`; plist row unification works correctly

### R8: Plist-to-list subsumption

**Given** a plist type `(plist k v)` where a `(list (k | v))` is expected
**When** unifying
**Then** succeed—a plist is a list

```
(plist k v) ~ (list (k | v))   OK (subsumption)
```

Add unification rule in `unify.ml`:
```ocaml
(* Plist → list subsumption: (Plist k v) widens to (list (k | v)).
   Decompose the plist into its underlying list-of-union form. *)
| TApp (con1, [ k1; v1 ]), TApp (con2, [ elem ])
  when is_plist_con con1 && is_list_con con2 ->
    unify ~invariant (TUnion [ k1; v1 ]) elem loc
```

**Given** a bare list type `(list (k | v))` where a `(plist k v)` is expected
**When** unifying
**Then** fail—a homogeneous list provides no evidence of alternating structure

```
(list (k | v)) ~ (plist k v)   FAIL
```

**Verify:** `dune test`; plist accepted where list expected; bare list rejected
where plist expected

### R9: Structural promotion from cons chains

**Given** a cons chain (n-tuple) with alternating key-value structure:

```elisp
;; Literal plist
'(:name "Alice" :age 30)
;; Type: (cons keyword (cons string (cons keyword (cons int nil))))
```

**When** unifying with `(plist k v)`
**Then** succeed if positions alternate between key types and value types:
- Even positions (0, 2, ...) unify with `k`
- Odd positions (1, 3, ...) unify with `v`
- The chain has even length (balanced key-value pairs)

```
(cons keyword (cons string (cons keyword (cons int nil))))
  ~ (plist keyword (string | int))   OK (structural evidence)

(cons keyword (cons string nil))
  ~ (plist keyword string)           OK (single key-value pair)

(cons string (cons keyword nil))
  ~ (plist keyword string)           FAIL (wrong alternation order)

(cons keyword nil)
  ~ (plist keyword v)                FAIL (odd length, no value)
```

**Given** a cons chain where the key type at even positions is not uniform
**When** unifying with `(plist k v)`
**Then** fail—keys must share a common type

**Verify:** `dune test`; list literals with alternating structure promote to
plist; malformed chains are rejected

## Non-Requirements

- Runtime validation of plist structure (type system assumes well-formed input)
- Special handling for `plist-member` or other non-accessor functions
- Type-level enforcement of unique keys
- Bidirectional transparency (list → plist requires structural evidence)
- Promotion from `(list (k | v))` (only cons chains/n-tuples provide evidence)

## Design Notes

### Subsumption with Structural Promotion

The plist intrinsic is **not opaque and not transparent**—it sits between:

- **Opaque** (like a hypothetical newtype): too restrictive, breaks list
  operations on plists
- **Transparent** (bidirectional): too permissive, any `(list (k | v))` silently
  becomes a plist, defeating the point of the intrinsic

Instead, plist uses **directional subsumption with structural promotion**:

| Direction | Rule | Rationale |
|:----------|:-----|:----------|
| plist → list | Always | A plist IS a list |
| list → plist | Never | No alternation evidence |
| cons chain → plist | When alternating | N-tuple structure proves layout |

This catches real errors: passing an arbitrary `(list (keyword | string))` to a
function expecting `(plist keyword string)` is flagged, because the list could
have any ordering. But constructing a plist from a literal like
`'(:name "Alice")` works because the cons chain structure proves alternation.

### Row-Typed Form

With the intrinsic, row-typed plists change:
```
Old: (plist {:name string & r}) → (list (keyword | {row}))
New: (plist {:name string & r}) → (Plist keyword {row})
```

This is more semantically accurate—the row is the value position of the key-value structure, not part of a union with keywords.

### Compatibility

Functions accepting `(list (keyword | string))` will accept
`(plist keyword string)` via subsumption. The reverse direction requires
re-typing: functions that previously accepted `(list (keyword | string))` and
are meant to accept plists should update their signatures to `(plist k v)`.
This is a deliberate strengthening—it catches real misuse.

## Tasks

- [x] [R1] Add `plist_of` constructor to `types.ml` / `types.mli`
- [x] [R1] Add `"Plist"` case to `intrinsic_display_name`
- [x] [R2] Add `"Plist"` to `is_truthy` intrinsic cases
- [x] [R3] Update `typings/tart-prelude.tart` plist definition
- [x] [R4] Add `"plist"` canonicalization to `sig_loader.ml`
- [x] [R5] Update `expand_map_row` in `sig_loader.ml`
- [x] [R6] Update `extract_plist_row` in `infer.ml`
- [x] [R7] Update `extract_concrete_map_row` in `unify.ml`
- [x] [R8] Add plist-to-list subsumption rule in `unify.ml`
- [x] [R9] Add cons-chain-to-plist structural promotion in `unify.ml`
- [x] Add test fixtures for plist intrinsic behavior
- [x] Verify existing plist tests still pass

**Status:** Complete
