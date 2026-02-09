# Spec 88 — `let-type`

## Overview

`.tart` signature files define types with `(type name ...)`, which are exported
and available to any module that `open`s or `include`s the file. `let-type` adds
a top-level form equivalent to `(type ...)` in all ways except that it is not
exported: file-wide scope, supports parameters and opaque types, but invisible to
`build_alias_context`/`build_opaque_context` when other modules import this one.

## Motivation

Complex signature files often need internal type abbreviations to keep
declarations readable. Without `let-type`, the only option is `(type ...)`, which
exports the alias — polluting the module's public type namespace with
implementation details.

## Syntax

`let-type` uses the same syntax as `(type ...)` with the keyword `let-type`:

```elisp
;; Alias (not exported)
(let-type pair (cons int int))

;; Parameterized alias (not exported)
(let-type wrapper [a] (list a))

;; Opaque (not exported)
(let-type internal-handle)
```

All forms that `(type ...)` supports are valid under `let-type`.

## Scoping

A `let-type` declaration is available from its point of declaration to the end of
the file, matching the scoping behaviour of `(type ...)`. Forward references are
not permitted — a `let-type` name must be declared before it is used:

```elisp
(defun bad (internal-pair) -> int)   ; Error: unbound type 'internal-pair'
(let-type internal-pair (cons int int))

(let-type internal-pair (cons int int))
(defun good (internal-pair) -> int)  ; OK
```

## Export Behaviour

`let-type` declarations are **not exported**. Specifically:

- They are excluded from `build_alias_context` results, so modules that `open`
  or `include` the current file do not see them
- They are excluded from `build_opaque_context` results, so opaque `let-type`
  declarations do not leak into other modules

This is the key difference from `(type ...)`, which is always exported. See
[Spec 69](./69-signature-files-modules.md) for full module/export semantics.

## Removal of `(let [(type ...) ...] ...)`

[Spec 69](./69-signature-files-modules.md) previously defined a `(let ...)`
block form for non-exported local type aliases with lexically-scoped bodies.
`let-type` supersedes it: it offers the same non-exported semantics with
file-wide scope and no nesting requirement. `(let ...)` was never used in
practice and is removed as part of this spec.

## Key Files

| File | Role |
|:-----|:-----|
| `lib/sig/sig_ast.ml` | Add `Let_type` variant to the signature AST |
| `lib/sig/sig_parser.ml` | Parse `let-type` forms |
| `lib/sig/sig_loader.ml` | Process `let-type` into local type environment; exclude from exports |
| `lib/sig/sig_convert.ml` | Convert `let-type` AST nodes to checker types |

## Deferred

- **Mutual recursion.** Each `let-type` can only reference types declared before
  it. See [Spec 89](./89-mutually-recursive-types.md) for multi-binding forms
  that enable mutual recursion.
