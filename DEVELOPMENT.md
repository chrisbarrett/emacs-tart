<!-- WARNING: This file is generated. Do not edit manually. -->

# Development

## Building from Source

```bash
git clone https://github.com/chrisbarrett/emacs-tart.git
cd emacs-tart
nix develop
dune build
```

The `tart` executable will be available at `_build/default/bin/tart.exe`.

You can run it directly with dune:

```bash
dune exec tart -- --help
```

Or add the build directory to your `PATH`:

```bash
export PATH="$PWD/_build/default/bin:$PATH"
tart --help
```

## Architecture

```
tart/
├── bin/main.ml              # CLI entry point
├── lib/
│   ├── syntax/              # Parser and lexer
│   │   ├── lexer.mll        # Ocamllex lexer
│   │   ├── parser.mly       # Menhir parser
│   │   ├── sexp.ml          # S-expression AST
│   │   └── location.ml      # Source locations
│   ├── interp/              # Interpreter
│   │   ├── value.ml         # Runtime values
│   │   ├── env.ml           # Environments
│   │   ├── builtin.ml       # Built-in functions
│   │   ├── eval.ml          # Core evaluator
│   │   └── expand.ml        # Macro expansion
│   ├── core/                # Type system core
│   │   ├── types.ml         # Type representation
│   │   └── type_env.ml      # Type environment
│   └── typing/              # Type inference
│       ├── constraint.ml    # Constraint representation
│       ├── infer.ml         # Constraint generation
│       ├── unify.ml         # Unification
│       ├── generalize.ml    # Let-generalization
│       ├── builtin_types.ml # Built-in function signatures
│       ├── check.ml         # Top-level type checking API
│       └── diagnostic.ml    # Error diagnostics
└── test/                    # Test suites
```

## Development Environment

```bash
nix develop
```

This provides OCaml, dune, menhir, and all dependencies.

## Common Commands

| Command           | Description              |
| :---------------- | :----------------------- |
| `dune build`      | Build the project        |
| `dune test`       | Run all tests            |
| `dune fmt`        | Format code              |
| `dune exec tart`  | Run the CLI              |

## Running Tests

```bash
dune test
```

## Git Hooks

Install git hooks to run build, test, and format checks:

```bash
prek install
```
