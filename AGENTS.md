tart: `Bash(command="./tart $args")`

build: `Bash(command="nix develop --command dune build 2>&1")`

## Style guidelines

Write `.mli` for every `lib/**/*.ml`. Docstrings ok; no redundant comments.

Write a `match` arm for every constructor of the scrutinee--avoids fallthrough
bugs; type checking shows where updates are required as data types are modified.

# Testing

Tests under `tests/fixtures/typings/` are pairs of `$name.{el,expected}` files.

## Type Definitions

OCaml layer defines minimal intrinsics; everything else bootstraps in user-land.

| file                                       | defines                                 |
| ------------------------------------------ | --------------------------------------- |
| `typings/tart-prelude.tart`                | userspace names for compiler intrinsics |
| `typings/emacs/{version}/c-core/*.tart`    | data structures, functions, variables   |
| `typings/emacs/{version}/lisp-core/*.tart` | functions, variables                    |

Primitives without special type-checker support → opaque types.

`(include)` de-duplicates across emacs versions.

## Terminology

**Validation = Implementation.** "Validating" signatures IS coding:

1. `./tart check` against `.el` files
2. Fix `.tart` signatures from errors
3. Document untypeable cases in BUGS.md
