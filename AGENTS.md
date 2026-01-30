execute tart with args: `Bash(command="./tart $args")`

build: `Bash(command="nix develop --command dune build 2>&1")`

Write `.mli` for every `lib/**/*.ml`

Docstrings ok. No redundant comments.

## Terminology

**Validation = Implementation.** When the plan talks about "validating" type
signatures against Emacs code, this IS implementation work:

1. Run `./tart check` against real `.el` files
2. Analyze type errors in the output
3. Fix `.tart` signature files to resolve errors
4. Document genuinely untypeable cases in BUGS.md

This is programming. The type checker validates programs; you implement the
signatures that make validation succeed. Do not skip "validation" tasks thinking
they are manual review—they are automated, iterative coding work.
