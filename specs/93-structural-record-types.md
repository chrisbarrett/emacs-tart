# Spec 93 — Structural Record Types

## Overview

[Spec 86](86-record-type-constructor.md) introduced an opaque
`(record tag)` type that captures the type tag but not field types. This
means `aref` on a record returns `any`, and the checker cannot verify that
`cl-defstruct` accessor functions return the correct field types.

This spec adds a `defstruct` declaration to the `.tart` signature grammar
that generates typed constructor, accessor, and predicate signatures from
field declarations. The core type system is unchanged — the opaque
`(record tag)` type is sufficient when combined with generated per-struct
accessor signatures.

## Signature Grammar

### `defstruct` Declaration

```lisp
(defstruct name
  (field1 type1)
  (field2 type2)
  ...)
```

A `defstruct` declaration in a `.tart` file generates the following
signatures:

```lisp
;; Constructor
(defun make-NAME (type1 type2 ...) -> (record NAME))

;; Predicate
(defun NAME-p
  (((record NAME)) -> t)
  ((_) -> nil))

;; Accessors (one per field)
(defun NAME-field1 ((record NAME)) -> type1)
(defun NAME-field2 ((record NAME)) -> type2)
```

### Example

```lisp
(defstruct person
  (name string)
  (age int)
  (email (string | nil)))
```

Generates:

```lisp
(defun make-person (string int (string | nil)) -> (record person))
(defun person-p (((record person)) -> t) ((_) -> nil))
(defun person-name ((record person)) -> string)
(defun person-age ((record person)) -> int)
(defun person-email ((record person)) -> (string | nil))
```

### Keyword Constructor

Emacs `cl-defstruct` supports a keyword constructor via
`(:constructor make-NAME &key)`. When the `.tart` declaration includes
`:keyword-constructor`:

```lisp
(defstruct person :keyword-constructor
  (name string)
  (age int))
```

Generates a keyword-argument constructor:

```lisp
(defun make-person (&key (:name string) (:age int)) -> (record person))
```

### Default Values

Fields with defaults are optional in the constructor:

```lisp
(defstruct config
  (host string)
  (port int)           ; required
  (timeout (int | nil))) ; optional (nil default implied by type)
```

When the field type includes `nil`, the constructor parameter is
`&optional`:

```lisp
(defun make-config (string int &optional (int | nil)) -> (record config))
```

## Inheritance

`cl-defstruct` supports single inheritance via `(:include parent)`. The
child struct includes all parent fields:

```lisp
(defstruct animal
  (name string)
  (legs int))

(defstruct dog (:include animal)
  (breed string))
```

The child struct `dog` has fields `name`, `legs`, and `breed`. The
generated signatures include all inherited fields in the constructor and
generate accessors for all fields (parent + child).

### Subtyping

`(record dog)` is **not** a subtype of `(record animal)` — the opaque
record type has no inheritance relationship. Functions expecting
`(record animal)` cannot accept `(record dog)`. This matches Emacs
behaviour: `cl-typep` checks the exact type tag, not the inheritance chain.

To accept both, the signature should use a union:
`((record animal) | (record dog))`.

## AST and Parser

### New AST Node

Add `DDefstruct` to `sig_ast.ml`:

```ocaml
type struct_field = {
  sf_name : string;
  sf_type : sig_type;
}

type struct_decl = {
  sd_name : string;
  sd_include : string option;       (* parent struct name *)
  sd_keyword_ctor : bool;
  sd_fields : struct_field list;
  sd_loc : Location.span;
}
```

Add `DDefstruct of struct_decl` to the `decl` variant.

### Parser

Parse `(defstruct name ...)` in `sig_parser.ml`. The form is:

```
(defstruct NAME [OPTIONS...] (FIELD TYPE)...)
```

Options are keyword symbols before the first field:
- `:keyword-constructor` — use keyword arguments in the constructor
- `(:include PARENT)` — inherit fields from parent struct

### Loader

In `sig_loader.ml`, expand `DDefstruct` into the equivalent `DDefun` and
`DType` declarations and load them normally. This avoids special-casing
structs throughout the rest of the system.

The expansion:
1. Look up parent struct fields if `(:include parent)` is present.
2. Concatenate parent fields + child fields.
3. Generate `make-NAME` defun with constructor signature.
4. Generate `NAME-p` defun with predicate signature.
5. Generate `NAME-FIELD` defun for each field accessor.

## Macro Expansion for `cl-defstruct`

Add `cl-defstruct` to `typings/tart-macros.el` so that `cl-defstruct`
forms in `.el` files are expanded before type checking. The macro rewrites
`cl-defstruct` into a `progn` of `defun` forms matching the generated
signatures:

```lisp
(defmacro cl-defstruct (name &rest fields)
  `(progn
     (defvar ,(intern (format "%s-p" name)) nil)
     (defun ,(intern (format "make-%s" name)) (&rest _args) nil)
     ,@(mapcar (lambda (field)
                 `(defun ,(intern (format "%s-%s" name
                                          (if (consp field) (car field) field)))
                    (_obj) nil))
               fields)))
```

This ensures the parser sees the generated function names as defined,
suppressing UNDEFINED VARIABLE errors. The actual types come from the
`.tart` signature, not the macro expansion.

## Key Files

| File | Role |
|:-----|:-----|
| `lib/sig/sig_ast.ml` | `DDefstruct` AST node, `struct_field`, `struct_decl` |
| `lib/sig/sig_ast.mli` | Types for struct declarations |
| `lib/sig/sig_parser.ml` | Parse `(defstruct ...)` form |
| `lib/sig/sig_loader.ml` | Expand `DDefstruct` into defun/type declarations |
| `lib/sig/sig_validation.ml` | Validate struct field types |
| `typings/tart-macros.el` | `cl-defstruct` macro expansion |
| `typings/tart-prelude.tart` | `record` opaque type (unchanged) |

## Future Work

- **Positional field access via `aref`.** Typing `(aref record idx)` with
  per-field precision requires tracking field positions in the type system,
  beyond what opaque `(record tag)` provides.
- **Mutator generation.** `cl-defstruct` generates `setf`-able accessors.
  Typing the mutators requires `setf` support in the checker.
- **Copy constructor.** `cl-defstruct` generates `copy-NAME`. Typing it
  as `(record NAME) -> (record NAME)` is straightforward but omitted from
  this spec for simplicity.
