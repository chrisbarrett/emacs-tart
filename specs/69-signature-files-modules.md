# Spec 69 — Signature Files & Module System

> Consolidates specs: [07](./.archive/07-signature-files.md), [12](./.archive/12-module-boundaries.md)

## Overview

The `.tart` signature file format declares types for Elisp modules, enabling
full type checking at module boundaries. Each `.tart` file derives its module
name from its filename and contains declarative type signatures for functions,
variables, type aliases, and opaque types. The type checker loads signatures via
a search path, verifies implementations against declared interfaces, and
enforces that all code is fully typed with no gradual escape hatch. The module
system distinguishes public and internal definitions, handles `require`/autoload
resolution, and supports compositional signature reuse through `open` and
`include` directives.

## Signature File Format

A `.tart` file is a declarative signature: no executable code, only type
declarations and directives. The filename determines the module name
(`foo.tart` provides types for `foo`).

Implementation files:

| file                    | role                               |
| ----------------------- | ---------------------------------- |
| `lib/sig/sig_ast.ml`    | Signature file AST                 |
| `lib/sig/sig_parser.ml` | Parse `.tart` source to AST        |
| `lib/sig/sig_loader.ml` | Load signatures into type env      |
| `lib/sig/sig_convert.ml`| Convert AST types to checker types |
| `lib/sig/search_path.ml`| Module search-path resolution      |
| `lib/sig/prelude.ml`    | Implicit prelude loading           |

### Loading Sequence

Signatures load in this order:

| file pattern                               | defines                                 |
| ------------------------------------------ | --------------------------------------- |
| `typings/tart-prelude.tart`                | userspace names for compiler intrinsics |
| `typings/emacs/{version}/c-core/*.tart`    | data structures, functions, variables   |
| `typings/emacs/{version}/lisp-core/*.tart` | functions, variables                    |

Primitives without special type-checker support are represented as opaque types.

## Type Syntax

Type expressions in signatures map to the type representation in the checker:

| syntax                    | meaning                                    |
| ------------------------- | ------------------------------------------ |
| `int`, `string`, `nil`    | Primitive types                            |
| `t`, `truthy`, `never`    | Special types                              |
| `a` (in quantifier scope) | Type variable                              |
| `(list int)`              | Type application                           |
| `(int \| string)`         | Union (parentheses required)               |
| `params -> return`        | Function type (infix arrow)                |
| `[a b]`                   | Explicit quantifier binding type variables |
| `[(a : bound)]`           | Bounded quantifier                         |

A symbol is a type variable if and only if it appears in a `[...]` quantifier.
Unbound type variables produce errors:

```elisp
(defun identity [a] (a) -> a)   ; OK: 'a' bound by [a]
(defun bad (a) -> a)             ; Error: unbound type variable 'a'
```

Single parameters need no parentheses; multiple parameters require grouping.
Quantifiers go at the start of arrow types for values, or after the name in
`defun`.

### Bounded Quantifiers

A quantifier entry `(a : bound)` constrains `a` to subtypes of `bound`:

```elisp
(type option [(a : truthy)] (a | nil))
(defun unwrap [(a : truthy)] ((a | nil) a) -> a)
```

Bounds are checked at instantiation sites.

## Declarations

### Function Signatures

`(defun name [vars] (params) -> return)` binds `name` as a directly callable
function. Multi-clause signatures are supported (see
[Spec 70](./70-multi-clause-dispatch.md)):

```elisp
(defun stringp
  ((string) -> t)
  ((_) -> nil))
```

### Variable Declarations

`(defvar name type)` binds `name` at the given type:

```elisp
(defvar load-path (list string))
```

### Type Aliases

`(type name def)` defines a type alias that expands at use sites.
Parameterized aliases take quantifiers:

```elisp
(type int-list (list int))
(type result [a e] ((ok a) | (err e)))
```

### Opaque Types

`(type name)` with no definition creates a distinct opaque type. Values are
created and consumed only through functions in the same module. Phantom
parameters are supported:

```elisp
(type buffer)
(type tagged [a])    ; (tagged int) and (tagged string) are distinct
```

### Struct Imports

`(import-struct name)` generates types and accessors for a `cl-defstruct`:

- Type `name`
- Constructor `make-name`
- Predicate `name-p`
- Accessors for each slot

### Non-Exported Type Aliases

`(let-type ...)` introduces a top-level type alias that is not exported. It
supports the same syntax as `(type ...)` — aliases, parameterized aliases, and
opaque types — but is excluded from `build_alias_context`/`build_opaque_context`,
so `open`/`include` from other modules do not see these types:

```elisp
(let-type pair (cons int int))
(defun swap-pair (pair) -> pair)
(defun make-pair (int int) -> pair)
;; 'pair' is not visible outside this module
```

See [Spec 88](./88-let-type.md) for full details.

## Directives

### `(open 'mod)`

Imports types from `mod` for use in type expressions. Opened types are not
re-exported from the current module:

```elisp
(open 'seq)
(defun my-flatten [a] ((seq (seq a))) -> (list a))
;; 'seq' type is usable here but not re-exported
```

### `(include 'mod)`

Inlines all declarations from `mod` into the current module's interface.
Included declarations are re-exported. `(include)` de-duplicates identical
definitions across Emacs versions.

```elisp
(include 'seq)
(defun seq-partition [a] (int (seq a)) -> (list (list a)))
;; All of seq's declarations are now part of this module's exports
```

### Shadowing Rule

A name brought into scope via `open`, `include`, or the prelude
([Spec 66](./66-type-system-core.md)) cannot be redefined in the current file:

```elisp
(open 'seq)
(type seq int)  ; Error: cannot redefine imported binding 'seq'
```

### Auxiliary Signature Files

A `.tart` file with no corresponding `.el` file is auxiliary. Auxiliary files
can be `include`'d (to share type definitions across multiple signatures) but
not `open`'ed (there is no runtime module to import from). They are parsed,
validated, and de-duplicated when included multiple times.

## Module Discovery

### Search Path

When a module is required, the loader searches for its `.tart` file in order:

1. **Sibling**: `module.tart` next to `module.el` (project-local)
2. **User/community**: each directory in `tart-type-path`
3. **Bundled**: `typings/**/*.tart` shipped with tart

The first match wins, allowing project-local overrides of bundled signatures.

### Autoload Resolution

For autoloaded functions, tart derives the module name from the function's
prefix and searches the path for a matching `.tart` file:

```elisp
;; Calling my-package-autoload-fn triggers search for my-package.tart
```

## Module Boundary Rules

### Fully Typed

All code has types, whether inferred or declared. There is no gradual typing
and no silent widening to `any`. Any `.el` file is eligible for type checking
via LSP.

### Signature Verification

When both `foo.el` and `foo.tart` exist, the type checker verifies the
implementation against the declared signatures:

```elisp
;; foo.tart
(defun foo-add (int int) -> int)

;; foo.el — error: signature mismatch (returns string, not int)
(defun foo-add (a b) (concat a b))
```

### Missing Signatures

A reference to a function or variable with no signature (not in the search
path, not locally inferred) produces error E0100 (`UndefinedVariable`).

### Public vs Internal

The `.tart` file lists the module's public API. Names following the Elisp
`--internal` convention that are absent from the signature file are inferred
locally and not exported:

- Public names in `.tart`: checked against signature, exported
- `--internal` names not in `.tart`: inferred, not exported
- Public-looking names defined in `.el` but absent from `.tart`: warning
  ("defined but not in signature file")

### `require`/`provide` Interaction

`(require 'module)` in Elisp triggers signature loading for `module`. The
required module's exported types become available to the caller.

### Circular Dependencies

Modules with mutual `require` cycles are handled via lazy signature loading.

## Deferred

No items currently deferred.
