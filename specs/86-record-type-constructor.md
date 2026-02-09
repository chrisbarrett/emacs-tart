# Spec 86 — Record Type Constructor

## Overview

Emacs Lisp records (created by `record` and `make-record`) are typed vectors
with a type tag in slot 0. Tart currently types these functions as
`-> truthy` because the return type depends on which TYPE symbol is passed —
a form of dependent typing tart does not support. This spec introduces an
opaque `(record tag)` type in the prelude and types `record`/`make-record`
to return it, providing more precision than `truthy` without requiring
dependent types.

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

The existing `type-of` signature is unchanged. It returns `symbol` for all
inputs. For records, the returned symbol is the tag, but this dynamic
relationship is not captured in the type system.

## Signature File Updates

Update the following files:

- `typings/tart-prelude.tart` — add `(type record [tag] #opaque)`
- `typings/emacs/31.0/c-core/alloc.tart` — update `record` and `make-record`
  signatures
- `typings/emacs/31.0/c-core/data.tart` — add/update `recordp` signature

## Key Files

| File | Role |
|:-----|:-----|
| `typings/tart-prelude.tart` | Add `record` opaque type |
| `typings/emacs/31.0/c-core/alloc.tart` | `record` and `make-record` signatures |
| `typings/emacs/31.0/c-core/data.tart` | `recordp` predicate signature |
| `lib/core/types.mli` | May need `TRecord` constructor if not using opaque type mechanism |

## Deferred

- **Structural record types.** Tracking field types by position (e.g.
  `(record foo {0: int, 1: string})`) would require positional field tracking
  and is not part of this spec. The opaque type captures only the tag.
- **cl-defstruct integration.** `cl-defstruct` generates record-based types
  with named field accessors. Connecting these generated accessors to
  positional record fields is deferred.
- **Record field access typing.** Functions like `aref` on records could
  return field-specific types if structural record types were available.
  Deferred pending structural record types.
