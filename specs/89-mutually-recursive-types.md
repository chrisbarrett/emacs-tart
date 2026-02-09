# Spec 89 — Mutually Recursive Types

## Overview

`(type ...)` and `(let-type ...)` currently define a single type per form.
Because declarations are scoped from point of declaration forward, two types that
reference each other cannot be expressed — the second name does not exist when
the first is defined. This spec extends both forms to accept multiple bindings in
a single form, making all names in the group mutually visible.

## Syntax

A `type` or `let-type` form may contain one or more bindings. Each binding is
`IDENT [QUANTS]? TYDEF`, where `TYDEF` is a type expression:

```elisp
;; Single binding (unchanged)
(type int-list (list int))

;; Multiple bindings — mutually recursive
(type tree   (leaf int | node forest)
      forest (list tree))

;; With quantifiers
(type tree   [a] (leaf a | node (forest a))
      forest [a] (list (tree a)))

;; Works with let-type too
(let-type internal-tree   (leaf int | node internal-forest)
          internal-forest (list internal-tree))
```

### Opaque Types

Opaque types (no definition body) cannot appear in multi-binding forms. An opaque
type has no body to reference other types, so mutual recursion does not apply.
Opaque types remain single-binding only:

```elisp
;; OK — single opaque binding
(type handle)

;; Error — opaque type in multi-binding form
(type handle
      wrapper (list handle))
```

## Scoping

### Within a group

All names in a multi-binding `type`/`let-type` are in scope for all definitions
in the group. Order within the group does not matter:

```elisp
;; Both are equivalent
(type tree   (leaf int | node forest)
      forest (list tree))

(type forest (list tree)
      tree   (leaf int | node forest))
```

### Between groups

Separate `type`/`let-type` forms retain sequential scoping — a name must be
declared in an earlier form before it can be referenced:

```elisp
(type tree (leaf int | node forest))    ; Error: 'forest' not yet defined
(type forest (list tree))
```

This is unchanged from current behaviour.

## Parsing

Type definitions are either parenthesized forms (`(list int)`, `(a | b)`) or
bare symbols (`int`, `string`). Type names are bare symbols. After consuming a
definition, the next bare symbol unambiguously starts a new binding. Quantifiers
(`[...]`) appear only between a name and its definition.

The parser consumes bindings greedily after the keyword:

1. Read a bare symbol — this is the type name
2. If the next token is `[`, read quantifiers
3. Read a type expression — this is the definition
4. If more tokens remain, go to 1

A single-binding form with no definition (`(type name)`) remains valid as an
opaque type. A multi-binding form where any binding lacks a definition is a parse
error.

## Backward Compatibility

- `(type name)` — opaque type, unchanged
- `(type name def)` — single alias, unchanged
- `(type name [quants] def)` — single parameterized alias, unchanged
- `(type name def name2 def2 ...)` — currently a parse error, now valid

The extension only activates when tokens remain after the first complete binding,
which is currently rejected. No existing valid syntax changes meaning.

## Key Files

| File | Role |
|:-----|:-----|
| `lib/sig/sig_ast.ml` | `Type_decl` carries a list of bindings instead of one |
| `lib/sig/sig_parser.ml` | Parse multiple bindings per `type`/`let-type` form |
| `lib/sig/sig_loader.ml` | Process binding groups: add all names before resolving definitions |
| `lib/sig/sig_convert.ml` | Convert mutually recursive binding groups |

## Deferred

- **Recursive type validation.** Detecting invalid infinite types (e.g.
  `(type a b  b a)` with no productive constructor) is not addressed. The type
  checker may loop or produce confusing errors on such definitions.
- **Cross-form recursion.** Mutual recursion across separate `type`/`let-type`
  forms (analogous to OCaml's non-`and` recursive types) is not supported.
  Types that reference each other must appear in the same form.
