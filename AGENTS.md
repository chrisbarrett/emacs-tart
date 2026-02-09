tart: `Bash(command="./tart $args")`

typechecking Elisp: `Bash(command="./tart check --emacs-version 31.0 $FILE")`

build: `Bash(command="nix develop --command dune build 2>&1")`

test: `Bash(command="nix develop --command dune test --force 2>&1")`

**Validation = Implementation.** "Validating" signatures IS coding:

## Key files

- `lib/tart.mli` - library interface
- `lib/core/types.mli` - type checker's typedefs

## OCaml Style

Write `.mli` for every `lib/**/*.ml`. Docstrings ok; no redundant comments.

Write a `match` arm for every constructor of the scrutinee--avoids fallthrough
bugs; type-checking shows locations to update.

## Markdown

References to other files or specs are always hyperlinked.

# Testing

Tests under `test/fixtures/typing/` are pairs of `$name.{el,expected}` files.

Pre-commit hooks can be run manually using `prek` (not `pre-commit`).

## Type Definitions

OCaml layer defines minimal intrinsics; everything else bootstraps in user-land.

| file                                       | defines                                 |
| ------------------------------------------ | --------------------------------------- |
| `typings/tart-prelude.tart`                | userspace names for compiler intrinsics |
| `typings/emacs/{version}/c-core/*.tart`    | data structures, functions, variables   |
| `typings/emacs/{version}/lisp-core/*.tart` | functions, variables                    |

Primitives without special type-checker support → opaque types.

`(include)` de-duplicates across emacs versions.

1. `./tart check` against `.el` files
2. Fix `.tart` signatures from errors
3. Document untypeable cases in BUGS.md
