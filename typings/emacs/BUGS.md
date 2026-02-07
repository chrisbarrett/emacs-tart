# Emacs Typings Issues

Cross-version issues affecting multiple Emacs releases.

## type-system-gap

_Symbols requiring features tart doesn't have (dependent types, row polymorphism)._

### `car-safe`
- **Location:** data.c
- **Issue:** Returns car of argument if cons, nil otherwise. Return type
  depends on runtime type of argument — would need dependent types to
  express `(cons a b) -> a | (not-cons) -> nil`
- **Suggested feature:** Overloaded return types

### `cdr-safe`
- **Location:** data.c
- **Issue:** Returns cdr of argument if cons, nil otherwise. Same
  dependent-type issue as car-safe
- **Suggested feature:** Overloaded return types

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

## ergonomic

_Symbols that are typeable but awkward (excessive annotations at call sites)._
