# Spec 91 — Tuple Element Access

## Overview

[Spec 84](84-heterogeneous-list-inference.md) introduced `TTuple` types for
heterogeneous `list` calls, but element access via `nth` discards the
per-element type information. When `(list 'setq x 1)` produces
`TTuple [symbol; any; int]`, a subsequent `(nth 0 ...)` returns `(a | nil)`
from the generic signature instead of `symbol`. This spec makes `nth` a
type-checker intrinsic that returns precise element types when the index is
a compile-time literal and the sequence is a tuple.

## Intrinsic Handling

### Current Behaviour

`nth` is a regular function resolved from `fns.tart`:

```lisp
(defun nth [a] (int (list a)) -> (a | nil))
```

When applied to a `TTuple`, the tuple-to-list subtyping rule widens it to
`(list any)`, and `nth` returns `(any | nil)` — losing all element type
precision.

### New Behaviour

Make `nth` a type-checker intrinsic in `infer.ml`, handled alongside
`list`, `funcall`, and `apply`. When the checker encounters an `nth` call:

1. Infer the type of the index argument and the sequence argument.
2. If the sequence type is `TTuple elems` and the index is a literal
   integer `n`:
   - If `0 <= n < length elems`, return `elems[n]`.
   - If `n >= length elems` or `n < 0`, return `nil` (out of bounds).
3. If the sequence type is `TTuple elems` and the index is **not** a
   literal, return a union of all element types plus `nil`.
4. If the sequence type is not a tuple, fall back to the existing
   signature-based typing (`(a | nil)`).

### Literal Detection

The AST represents literal integers as `Int of int * span` in
`lib/syntax/sexp.ml`. When the first argument to `nth` is an `Int` node,
the literal value is available at type-check time.

When the first argument is a symbol or expression, the index is non-literal
and the intrinsic falls back to the union-of-elements path.

## Examples

### Literal index on tuple

```lisp
(let ((xs (list 'setq x 1)))
  (nth 0 xs))
;; xs : TTuple [symbol, any, int]
;; (nth 0 xs) : symbol
```

### Out-of-bounds literal

```lisp
(let ((xs (list 1 2)))
  (nth 5 xs))
;; xs : (list int)  [homogeneous, not a tuple]
;; (nth 5 xs) : (int | nil)  [falls back to signature]
```

### Non-literal index

```lisp
(let ((xs (list "a" 42 t)))
  (nth n xs))
;; xs : TTuple [string, int, t]
;; n : int (not a literal)
;; (nth n xs) : (string | int | t | nil)
```

### Regular list (unchanged)

```lisp
(let ((xs (list 1 2 3)))
  (nth 0 xs))
;; xs : (list int)  [homogeneous]
;; (nth 0 xs) : (int | nil)  [falls back to signature]
```

## Implementation

### Intrinsic Registration

Add `nth` to the pattern match in the `infer` function, before the
catch-all `List (fn :: args, span)` case:

```ocaml
| List (Symbol ("nth", _) :: [index; seq], span) ->
    infer_nth_intrinsic env index seq span
```

### `infer_nth_intrinsic`

```
infer_nth_intrinsic env index seq span:
  1. index_result = infer env index
  2. seq_result = infer env seq
  3. match seq_result.ty with
     | TTuple elems ->
         match index with
         | Int (n, _) ->
             if 0 <= n < List.length elems then List.nth elems n
             else Prim.nil
         | _ ->
             TUnion (elems @ [Prim.nil])  (* non-literal index *)
     | _ ->
         (* Fall back to signature-based typing *)
         infer_application env (Symbol ("nth", span)) [index; seq] span
```

The fallback to `infer_application` ensures that non-tuple sequences
(`(list a)`, `nil`, etc.) use the existing generic signature from
`fns.tart`.

### `elt` Intrinsic

The same treatment applies to `elt`, which is the general sequence accessor.
`elt` works on vectors and lists, so it can also benefit from tuple
precision:

```lisp
(defun elt [a] ((list a) int) -> a)
```

Add `elt` as a second intrinsic with the same logic, except `elt` does not
return `nil` for valid indices (it signals an error for out-of-bounds).

## Key Files

| File | Role |
|:-----|:-----|
| `lib/typing/infer.ml` | Add `nth` and `elt` intrinsic cases |
| `lib/syntax/sexp.ml` | `Int` AST node for literal detection |
| `typings/emacs/31.0/c-core/fns.tart` | Existing `nth` and `elt` signatures (fallback) |

## Future Work

- **`car`/`cdr` on tuples.** `(car tuple)` could return the first element
  type and `(cdr tuple)` the tail tuple. Requires new tuple-cons
  decomposition in the unifier.
- **Tuple narrowing in conditionals.** `(if (= (nth 0 xs) 'foo) ...)` could
  narrow the tuple's first element type in the truthy branch.
