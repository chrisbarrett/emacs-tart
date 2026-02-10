# Spec 86 — Record Type Constructor

## Overview

Emacs Lisp records (created by `record` and `make-record`) are typed vectors
with a type tag in slot 0. Tart currently types these functions as
`-> truthy` because the return type depends on which symbol is passed as the
tag. This spec introduces an opaque `(record tag)` type in the prelude and
types `record`/`make-record` to return it. The tag type parameter carries a
singleton literal symbol type (e.g., `'foo`), exploiting the one-to-one
correspondence between atom types and their runtime values to thread the tag
through the type system without dependent types.

## Record Type

### Prelude Definition

Add an opaque type constructor `record` to the prelude:

```lisp
(type record [tag] #opaque)
```

The type parameter `tag` represents the type tag symbol stored in slot 0.
Since `record` is opaque, values of type `(record tag)` can only be created
by `record` and `make-record` and inspected by `type-of` and
`record`-accessor functions.

### Relationship to Other Types

Records are a subtype of `truthy` (they are always non-nil). They are not
vectors, even though they are implemented as vectors internally — Emacs Lisp
distinguishes records from vectors at the type-of level:

```lisp
(type-of (record 'foo 1 2))  ;; => foo (not vector)
(vectorp (record 'foo 1 2))  ;; => nil
(recordp (record 'foo 1 2))  ;; => t
```

The subtyping relationships:

```
record tag <: truthy
```

No relationship to `(vector a)` — they are disjoint types.

## Function Signatures

### record

```lisp
(defun record [tag] (tag &rest any) -> (record tag))
```

The first argument is the type tag. Subsequent arguments are the field values.
The return type captures the tag but not the field types (since field count
and types vary per record type).

### make-record

```lisp
(defun make-record [tag] (tag int any) -> (record tag))
```

Creates a record with `COUNT` fields all initialised to `INIT`. The tag is
captured in the return type.

### recordp

```lisp
(defun recordp
  (((record _)) -> t)
  ((_) -> nil))
```

Type predicate that narrows to `(record _)` in truthy branches.

### type-of

Add a record-specific clause to the existing `type-of` signature:

```lisp
(defun type-of
  [tag] (((record tag)) -> tag)
  ((_) -> symbol))
```

The `tag` type parameter in `(record tag)` is a singleton literal symbol type
(e.g., `'foo`). Since literal types carry their precise value, `type-of` can
return the tag itself rather than the general `symbol` type. This exploits the
singleton relationship between atom types and values — `'foo` at the type level
corresponds to exactly one runtime value, so no dependent typing is needed.

When the tag is a union of literals (e.g., `(record ('foo | 'bar))`), `type-of`
returns the union `'foo | 'bar`. Precision is only lost when the tag is an
unconstrained `symbol`.

## Signature File Updates

Update the following files:

- `typings/tart-prelude.tart` — add `(type record [tag] #opaque)`
- `typings/emacs/31.0/c-core/alloc.tart` — update `record` and `make-record`
  signatures
- `typings/emacs/31.0/c-core/data.tart` — add/update `recordp` and `type-of`
  signatures

## Key Files

| File | Role |
|:-----|:-----|
| `typings/tart-prelude.tart` | Add `record` opaque type |
| `typings/emacs/31.0/c-core/alloc.tart` | `record` and `make-record` signatures |
| `typings/emacs/31.0/c-core/data.tart` | `recordp` and `type-of` signatures |
| `lib/core/types.mli` | May need `TRecord` constructor if not using opaque type mechanism |

## Future Work

- **Structural record types and `cl-defstruct` integration.**
  [Spec 93](93-structural-record-types.md) — adds `defstruct` to the
  `.tart` signature grammar, generating typed constructor, accessor, and
  predicate signatures from field declarations. Also adds `cl-defstruct`
  macro expansion.
- **Positional field access via `aref`.** Typing `(aref record idx)` with
  per-field precision requires tracking field positions in the type system,
  beyond what opaque `(record tag)` provides.
