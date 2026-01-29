# Emacs Typings Issues

Cross-version issues affecting Emacs type signatures.

## type-system-gap

Issues requiring type system features tart doesn't have (dependent types, row
polymorphism, etc.).

<!-- Example entry:
### `apply`
- **Location:** eval.c:2847
- **Issue:** Requires dependent types to express that return type depends on
  first argument's return type
- **Suggested feature:** First-class function type introspection
-->

## untypeable

Behavior that can't be captured soundly (dynamic dispatch, eval-based, etc.).

<!-- Example entry:
### `funcall`
- **Location:** eval.c:2789
- **Issue:** Accepts any function and any arguments; return type is dynamic
- **Workaround:** Typed as `[a] (-> a) &rest any -> a` (loses argument checking)
-->

## ergonomic

Typeable but awkward (excessive annotations at call sites, etc.).

<!-- Example entry:
### `mapcar`
- **Location:** fns.c:2567
- **Issue:** Works but requires explicit type annotation at most call sites
  due to polymorphism interaction with union types
-->

## version-specific

Signatures that changed between Emacs versions.

<!-- Example entry:
### `json-parse-string`
- **Location:** json.c:523
- **Issue:** :object-type keyword added in Emacs 28, changes return type
-->
