# Spec 76 — Typings Precision: lisp-core Foundational Files

Improve the 4 highest-priority lisp-core files by replacing `any` with precise
types where the type system supports it. Validate changes against the test suite.

Derived from [Spec 76](./specs/76-typings-distribution.md) deferred items and
[.ralph/IMPLEMENTATION_PLAN.md](./.ralph/IMPLEMENTATION_PLAN.md) Iteration 3.

---

## Task 1 — Precision audit: subr.tart

Tighten signatures in `typings/emacs/31.0/lisp-core/subr.tart`:

- `fixnump`: multi-clause predicate `((int) -> bool) ((_) -> nil)` — fixnum is
  a subtype of int
- `bignump`: multi-clause predicate `((int) -> bool) ((_) -> nil)`
- `add-hook`: FN param from `any` to `(symbol | (any -> any))` — hooks take
  function values or symbol names
- `remove-hook`: FN param from `any` to `(symbol | (any -> any))`
- `kbd`: return from `any` to `(string | (vector any))` — returns a key sequence
  (string or vector)
- `char-displayable-p`: return from `any` to `bool`
- `with-case-table`: TABLE from `any` to `char-table`
- `with-syntax-table`: TABLE from `any` to `char-table`

Note: most macros (`when`, `unless`, `dolist`, `if-let*`, etc.) use `&rest any ->
any` because body return types are genuinely dynamic; the type checker handles
these via special-form logic. Leave those unchanged.

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 2 — Precision audit: simple.tart

Tighten signatures in `typings/emacs/31.0/lisp-core/simple.tart`:

- `beginning-of-buffer`: ARG from `any` to `(int | nil)` — prefix arg is numeric
- `end-of-buffer`: ARG from `any` to `(int | nil)`
- `kill-region`: REGION from `any` to `bool`
- `copy-region-as-kill`: REGION from `any` to `bool`
- `kill-ring-save`: REGION from `any` to `bool`
- `yank`: ARG from `any` to `(int | nil)` — prefix arg
- `yank-pop`: ARG from `any` to `(int | nil)`
- `fill-paragraph`: JUSTIFY from `any` to `bool`
- `indent-for-tab-command`: ARG from `any` to `(int | nil)`
- `shell-command-on-region`: OUTPUT-BUFFER from `any` to
  `(buffer | string | bool)`, ERROR-BUFFER from `any` to `(buffer | string)`
- `transpose-subr`: MOVER from `any` to `(any -> any)` — it takes a movement
  function

Note: simple.tart is already quite precise. Most remaining `any` params are
genuinely used as non-nil boolean flags. Leave those.

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 3 — Precision audit: seq.tart

Tighten signatures in `typings/emacs/31.0/lisp-core/seq.tart`:

The seq.el library operates generically over lists, vectors, and strings. The
current signatures use `(list any) | (vector any) | string` unions with `any`
element types. Where polymorphism improves precision (higher-order functions),
tighten:

- `seq-map`: polymorphic `[a b] (((a) -> b) ((list a) | (vector a) | string))
  -> (list b)` — preserves element type through the mapping function
- `seq-filter`: polymorphic `[a] (((a) -> any) ((list a) | (vector a) | string))
  -> (list a)` — filter always returns a list in seq.el
- `seq-remove`: same pattern as seq-filter
- `seq-reduce`: polymorphic `[a b] (((b a) -> b) ((list a) | (vector a) |
  string) b) -> b`
- `seq-find`: polymorphic `[a] (((a) -> any) ((list a) | (vector a) | string)
  &optional a) -> (a | nil)`
- `seq-do`: polymorphic `[a] (((a) -> any) ((list a) | (vector a) | string))
  -> ((list a) | (vector a) | string)`
- `seq-some`: polymorphic `[a] (((a) -> any) ((list a) | (vector a) | string))
  -> any` (returns the predicate's result, not the element)
- `seq-every-p`: polymorphic `[a] (((a) -> any) ((list a) | (vector a) |
  string)) -> bool`
- `seq-count`: polymorphic `[a] (((a) -> any) ((list a) | (vector a) | string))
  -> int`
- `seq-sort`: polymorphic `[a] (((a a) -> any) ((list a) | (vector a) | string))
  -> ((list a) | (vector a) | string)`
- `seq-group-by`: polymorphic `[a b] (((a) -> b) ((list a) | (vector a) |
  string)) -> (list (cons b (list a)))`

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 4 — Precision audit: cl-lib.tart

Tighten signatures in `typings/emacs/31.0/lisp-core/cl-lib.tart`:

- `cl-remove-if`: polymorphic `[a] (((a) -> any) (list a) &rest any) -> (list a)`
- `cl-remove-if-not`: polymorphic `[a] (((a) -> any) (list a) &rest any) ->
  (list a)`
- `cl-find-if`: polymorphic `[a] (((a) -> any) ((list a) | (vector a) | string)
  &rest any) -> (a | nil)`
- `cl-first` through `cl-tenth`: polymorphic `[a] ((list a)) -> (a | nil)` —
  they can return nil for short lists
- `cl-pushnew`: tighten to `[a] (a any &rest any) -> (list a)`

Note: `cl-loop`, `cl-case`, `cl-typecase`, `cl-block` etc. are macros with
genuinely dynamic return types. Leave as `any`.

**Verify:** `nix develop --command dune test --force 2>&1`

---

## Task 5 — Verify and document

- Run full test suite
- Update `typings/emacs/BUGS.md` if any new untypeable cases are found
- No 29.1/30.1 version dirs have lisp-core content; sync not needed

**Verify:** `nix develop --command dune test --force 2>&1`
