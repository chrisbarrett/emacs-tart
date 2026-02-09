# Emacs Typings Issues

Cross-version issues affecting multiple Emacs releases.

## type-system-gap

_Symbols requiring features tart doesn't have (dependent types, row polymorphism)._

### `subr-type`
- **Location:** data.c
- **Issue:** Returns a function type descriptor as a sexp; the shape
  depends on the subr's actual signature. Expressing the precise return
  type would require dependent types (the output structure is a function
  of the input subr). Typed as `-> truthy`, which is a sound
  approximation — `subr-type` always returns a non-nil value

### `record` / `make-record`
- **Location:** alloc.c
- **Issue:** Return a record object (a typed vector with a type tag in
  slot 0). There is no opaque `record` type in the prelude, and the
  return type depends on which TYPE symbol is passed. Typed as
  `-> truthy` (always non-nil)
- **Planned:** [Spec 86](../../specs/86-record-type-constructor.md)

### `condition-case` / `catch`
- **Location:** eval.c
- **Issue:** Return type is the union of the body expression and all
  handler/thrown values. Would need effect typing or exception type
  tracking to express the actual return type
- **Planned:** [Spec 85](../../specs/85-condition-case-return-typing.md)

## untypeable

_Symbols whose return types are determined at runtime; correctly typed as `any`._

### `symbol-value`
- **Location:** data.c
- **Typing:** Returns whatever value was stored in the symbol's value
  cell. The return type is determined at runtime by prior `set` or `setq`
  calls. Correctly typed as `(symbol) -> any`

### `symbol-function`
- **Location:** data.c
- **Typing:** Returns whatever was bound via `fset` or `defun`. Could be
  a function, macro, autoload form, or any other object. Correctly typed
  as `(symbol) -> any`

### `default-value`
- **Location:** data.c
- **Typing:** Returns the global default value of a buffer-local
  variable. Same runtime-determined nature as `symbol-value`. Correctly
  typed as `(symbol) -> any`

### `indirect-function`
- **Location:** data.c
- **Typing:** Follows chains of symbol function bindings until a
  non-symbol is found. The final result depends on the runtime binding
  chain. Correctly typed as `(any) -> any`

### `set` / `fset` / `set-default`
- **Location:** data.c
- **Typing:** Return their VALUE argument unchanged. Typed as
  `(symbol any) -> any` because the return type equals the input value
  type, but tart has no dependent typing to express this. The `any`
  return type is the correct approximation

### `subr-native-comp-unit`
- **Location:** data.c
- **Typing:** Returns a native-comp-unit object, which is an opaque
  internal type. No user-facing type exists for it. Correctly typed as
  `(any) -> any`

### `eval`
- **Location:** eval.c
- **Typing:** Evaluates an arbitrary Emacs Lisp form. Return type is
  determined entirely at runtime by the form being evaluated. Correctly
  typed as `(any &optional any) -> any`

### `macroexpand`
- **Location:** eval.c
- **Typing:** Expands a macro form. The result is an arbitrary Emacs Lisp
  expression whose type depends on the macro definition. Correctly typed
  as `(any &optional any) -> any`

### `default-toplevel-value` / `buffer-local-toplevel-value`
- **Location:** eval.c
- **Typing:** Return the toplevel or buffer-local value of a symbol.
  Same runtime-determined nature as `symbol-value`. Correctly typed as
  `(symbol) -> any`

### `run-hook-with-args` / `run-hook-with-args-until-success` / `run-hook-with-args-until-failure`
- **Location:** eval.c
- **Typing:** Run hook functions whose return types are unknown. The
  overall return is the last/first non-nil/nil result respectively.
  Correctly typed as `(symbol &rest any) -> any`

### `autoload-do-load`
- **Location:** eval.c
- **Typing:** Loads an autoloaded function and returns the result. The
  return type depends on what was loaded — could be a function, macro,
  keymap, or other object. Correctly typed as `(any &optional any any) -> any`

### `funcall-with-delayed-message`
- **Location:** eval.c
- **Typing:** Calls a function after a timeout with a message. The FN
  argument and return type are both runtime-determined. Correctly typed
  as `(num string any) -> any`

### `get` (symbol plist)
- **Location:** fns.c
- **Typing:** Returns the value stored on a symbol's property list for a
  given property. Any value can be stored on a symbol's plist. Correctly
  typed as `(symbol symbol) -> any`

### `put` (symbol plist)
- **Location:** fns.c
- **Typing:** Returns its VALUE argument unchanged. Typed as
  `(symbol symbol any) -> any` because the return type equals the
  input value type, but tart has no dependent typing to express this.
  The `any` return type is the correct approximation

### `nil` is not `(list a)`
- **Location:** type system
- **Issue:** In Emacs Lisp, `nil` is the empty list — `(listp nil)`
  returns `t`. However, the type checker models `nil` as `Nil`, a
  distinct type from `(list a)`. This causes false positives wherever
  code passes literal `nil` as a list argument, e.g.
  `(append sequence nil)` or `(nreverse result)` when `result` is
  initialized to `nil`. Additionally, `(list a)` includes `nil` but
  cannot be passed to functions expecting `(cons b c)`, so patterns
  like `(setcar (cdr xs) val)` fail because `cdr` returns `(list a)`
  which might be `nil`
- **Planned:** [Spec 81](../../specs/81-nil-list-subtyping.md)

### `list` heterogeneous construction
- **Location:** type system
- **Issue:** `list` is polymorphic `[a] (&rest a) -> (list a)`, but
  Emacs Lisp frequently builds heterogeneous lists for code-as-data:
  `(list 'setq place value)`. The first arg `'setq` (symbol) fixes
  `a = symbol`, then subsequent non-symbol args fail. This pattern is
  pervasive in macro-defining code
- **Planned:** [Spec 84](../../specs/84-heterogeneous-list-inference.md)

### Unsupported special forms
- **Location:** parser
- **Issue:** The following forms are not understood by the parser,
  causing all bindings inside their bodies to appear as undefined
  variables: `cl-defgeneric`, `cl-defmethod`, `gv-define-setter`,
  `gv-define-expander`, `pcase-defmacro`, `declare-function`,
  `define-minor-mode`, `cl--defalias`, `gv-letplace`,
  `macroexp-let2`, `set-advertised-calling-convention`. Additionally,
  `defmacro` body bindings and `defsubst` body bindings are not fully
  scoped. These account for the majority of errors when validating
  `seq.el` (422/482), `cl-lib.el` (155/237), `subr.el` (1466/2572),
  `simple.el` (1217/2690), `files.el` (1172/2336),
  `startup.el` (606/954), `minibuffer.el` (614/1530), and
  `window.el` (1519/3707)
- **Planned:** [Spec 82](../../specs/82-special-form-parser-extensions.md)

### Function subtyping with rest parameters
- **Location:** type system
- **Issue:** A function type `() -> t` or `(a) -> t` is not recognized
  as a subtype of `((&rest any) -> any)`. This means `add-hook` and
  `remove-hook`, whose FN parameter is typed as
  `(symbol | ((&rest any) -> any))`, still reject lambda expressions
  with fixed arities. In `minibuffer.el`, 6 `add-hook` and
  7 `remove-hook` calls fail because the lambda has 0–3 fixed params
- **Planned:** [Spec 83](../../specs/83-function-subtype-widening.md)

### `car`/`cdr` on dynamically-typed values
- **Location:** type system
- **Issue:** `car` and `cdr` are typed for `(list a)` and `(cons a b)`,
  but Emacs Lisp frequently applies them to values with inferred
  union types like `(Or symbol keyword int t nil)`. In
  `minibuffer.el` this accounts for 50 `car` and 22 `cdr` arity
  errors; in `window.el`, 4 `car` errors. The root cause is that
  variables bound via `let` with complex control flow get wide union
  types rather than the narrower type the programmer intends
- **Planned:** [Spec 81](../../specs/81-nil-list-subtyping.md)

### `defcustom` parser interpretation
- **Location:** parser
- **Issue:** `defcustom` is typed as
  `(symbol any string &rest any) -> symbol`, but the parser interprets
  `(defcustom foo 42 "doc" ...)` with `foo` as a variable reference
  (yielding the integer 42 as argument 1) rather than the symbol being
  defined. This causes spurious "expects argument 1 to be symbol"
  errors in `simple.el` (12 instances)
- **Planned:** [Spec 82](../../specs/82-special-form-parser-extensions.md)

## ergonomic

_Symbols that are typeable but awkward (excessive annotations at call sites)._

### `concat`
- **Location:** fns.c
- **Issue:** Accepts strings, symbols, and lists/vectors of ints.
  Tightening to `(&rest (string | symbol | (list int) | (vector int)))`
  causes signature mismatches when callers declare string parameters,
  because the constraint solver infers the full union as the parameter
  type. Kept as `(&rest any) -> string`
- **Planned:** [Spec 87](../../specs/87-bounded-quantification.md)

## validation-status

_Error counts from `./tart check --emacs-version 31.0` against Emacs 30.2 `.el` files._

| file           | total | UNDEFINED VARIABLE | TYPE MISMATCH | WRONG ARITY | BRANCH MISMATCH | INFINITE TYPE | DISJOINT EQUALITY | WARNING | HINT |
| :------------- | ----: | -----------------: | ------------: | ----------: | --------------: | ------------: | ----------------: | ------: | ---: |
| seq.el         |   482 |                422 |            44 |          12 |               1 |             3 |                 0 |       0 |    0 |
| cl-lib.el      |   237 |                155 |            66 |           7 |               5 |             2 |                 0 |       1 |    1 |
| subr.el        | 2,572 |              1,466 |           925 |         127 |              28 |             8 |                 8 |       9 |    1 |
| simple.el      | 2,690 |              1,217 |         1,341 |          53 |              44 |             2 |                 7 |      20 |    6 |
| files.el       | 2,336 |              1,172 |         1,064 |          53 |              29 |             1 |                 6 |       8 |    3 |
| startup.el     |   954 |                606 |           327 |           6 |               8 |             1 |                 2 |       0 |    4 |
| minibuffer.el  | 1,530 |                614 |           758 |         118 |              17 |             8 |                11 |       4 |    0 |
| window.el      | 3,707 |              1,519 |         2,081 |          31 |              15 |             0 |                27 |      33 |    1 |
| **total**      | **14,508** |          **7,171** |     **6,606** |     **407** |         **147** |        **25** |            **61** |  **75** | **16** |

UNDEFINED VARIABLE accounts for 49% of all errors and stems primarily from
unsupported special forms (see above).
[Spec 82](../../specs/82-special-form-parser-extensions.md) alone would
eliminate ~49% of all errors. TYPE MISMATCH (46%) includes nil-vs-list,
heterogeneous list, car/cdr on unions, and function subtyping gaps documented
in this file.
