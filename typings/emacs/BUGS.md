# Emacs Typings Issues

Cross-version issues affecting multiple Emacs releases.

## type-system-gap

_Symbols requiring features tart doesn't have (dependent types, row polymorphism)._

### `subr-type`
- **Location:** data.c
- **Issue:** Returns a function type descriptor as a sexp; the shape
  depends on the subr's actual signature. Cannot be expressed statically
- **Suggested feature:** None practical

### `record` / `make-record`
- **Location:** alloc.c
- **Issue:** Return a record object (a typed vector with a type tag in
  slot 0). There is no opaque `record` type in the prelude, and the
  return type depends on which TYPE symbol is passed. Typed as
  `-> truthy` (always non-nil)
- **Suggested feature:** Record type constructor

### `condition-case` / `catch`
- **Location:** eval.c
- **Issue:** Return type is the union of the body expression and all
  handler/thrown values. Would need effect typing or exception type
  tracking to express the actual return type
- **Suggested feature:** Exception type tracking

## untypeable

_Symbols whose behavior can't be captured soundly (dynamic dispatch, eval-based)._

### `symbol-value`
- **Location:** data.c
- **Issue:** Returns whatever value was stored in the symbol's value
  cell. The return type is genuinely dynamic — determined at runtime by
  prior `set` or `setq` calls

### `symbol-function`
- **Location:** data.c
- **Issue:** Returns whatever was bound via `fset` or `defun`. Return
  type is genuinely dynamic — could be a function, macro, autoload form,
  or any other object

### `default-value`
- **Location:** data.c
- **Issue:** Returns the global default value of a buffer-local
  variable. Same dynamic nature as `symbol-value`

### `indirect-function`
- **Location:** data.c
- **Issue:** Follows chains of symbol function bindings until a
  non-symbol is found. The final result is whatever was `fset` at the
  end of the chain — genuinely dynamic

### `set` / `fset` / `set-default`
- **Location:** data.c
- **Issue:** Return their VALUE argument unchanged. Typed as
  `(symbol any) -> any` because the return type equals the input value
  type, but tart has no dependent typing to express this

### `subr-native-comp-unit`
- **Location:** data.c
- **Issue:** Returns a native-comp-unit object, which is an opaque
  internal type. No user-facing type exists for it; typed as `-> any`

### `eval`
- **Location:** eval.c
- **Issue:** Evaluates an arbitrary Emacs Lisp form. Return type is
  determined entirely at runtime by the form being evaluated

### `macroexpand`
- **Location:** eval.c
- **Issue:** Expands a macro form. The result is an arbitrary Emacs Lisp
  expression whose type depends on the macro definition

### `default-toplevel-value` / `buffer-local-toplevel-value`
- **Location:** eval.c
- **Issue:** Return the toplevel or buffer-local value of a symbol.
  Same dynamic nature as `symbol-value` — the stored type is unknown
  statically

### `run-hook-with-args` / `run-hook-with-args-until-success` / `run-hook-with-args-until-failure`
- **Location:** eval.c
- **Issue:** Run hook functions whose return types are unknown. The
  overall return is the last/first non-nil/nil result respectively

### `autoload-do-load`
- **Location:** eval.c
- **Issue:** Loads an autoloaded function and returns the result. The
  return type depends on what was loaded — could be a function, macro,
  keymap, or other object

### `funcall-with-delayed-message`
- **Location:** eval.c
- **Issue:** Calls a function after a timeout with a message. The FN
  argument and return type are both dynamic — same limitation as
  `funcall` but without special-case type checking

### `get` (symbol plist)
- **Location:** fns.c
- **Issue:** Returns the value stored on a symbol's property list for a
  given property. The return type is genuinely dynamic — any value can
  be stored on a symbol's plist

### `put` (symbol plist)
- **Location:** fns.c
- **Issue:** Returns its VALUE argument unchanged. Typed as
  `(symbol symbol any) -> any` because the return type equals the
  input value type, but tart has no dependent typing to express this

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
- **Suggested feature:** Subtyping `Nil <: (list a)` for all `a`, and
  refinement types or non-nil list type `(cons a (list a))`

### `list` heterogeneous construction
- **Location:** type system
- **Issue:** `list` is polymorphic `[a] (&rest a) -> (list a)`, but
  Emacs Lisp frequently builds heterogeneous lists for code-as-data:
  `(list 'setq place value)`. The first arg `'setq` (symbol) fixes
  `a = symbol`, then subsequent non-symbol args fail. This pattern is
  pervasive in macro-defining code
- **Suggested feature:** Tuple type or heterogeneous list support

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
  `seq.el` (422/482), `cl-lib.el` (155/235), `subr.el` (1466/2562),
  `simple.el` (1217/2675), `files.el` (1172/2325), and
  `startup.el` (606/950)
- **Suggested feature:** Parser support for these forms

### `defcustom` parser interpretation
- **Location:** parser
- **Issue:** `defcustom` is typed as
  `(symbol any string &rest any) -> symbol`, but the parser interprets
  `(defcustom foo 42 "doc" ...)` with `foo` as a variable reference
  (yielding the integer 42 as argument 1) rather than the symbol being
  defined. This causes spurious "expects argument 1 to be symbol"
  errors in `simple.el` (12 instances)
- **Suggested feature:** Special-case `defcustom` in the parser to
  treat the first argument as a symbol literal

## ergonomic

_Symbols that are typeable but awkward (excessive annotations at call sites)._

### `concat`
- **Location:** fns.c
- **Issue:** Accepts strings, symbols, and lists/vectors of ints.
  Tightening to `(&rest (string | symbol | (list int) | (vector int)))`
  causes signature mismatches when callers declare string parameters,
  because the constraint solver infers the full union as the parameter
  type. Kept as `(&rest any) -> string`
- **Suggested feature:** Constraint solver union-avoidance heuristic
