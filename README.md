# Tart

A static type checker for Emacs Lisp.

Tart provides Hindley-Milner type inference for Elisp code, catching type errors
at edit time via LSP integration with Eglot.

> [!WARNING]
> Tart is under active development. The parser, interpreter, and type inference
> engine are functional, but the CLI and LSP integration are not yet complete.

## Features

- **Type inference** — Principal types inferred without annotations
- **Sound typing** — No `any` escape hatch; errors caught at compile time
- **LSP integration** — Diagnostics, hover types, and more via Eglot (planned)
- **Signature files** — `.eli` files declare types for untyped code
- **Macro expansion** — Built-in pure Elisp interpreter expands macros

## Installation

### Using Nix Flakes (Recommended)

If you have [Nix][nix] with flakes enabled:

```bash
# Run directly without installing
nix run github:chrisbarrett/emacs-tart -- --help

# Or install to your profile
nix profile install github:chrisbarrett/emacs-tart
```

### Building from Source

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

## Usage

### Type-Checking Files

```bash
tart file.el
```

Provide a sibling `.eli` signature file to enable type checking:

```
my-package.el      # Elisp source
my-package.eli     # Type signatures
```

### Signature File Format

Signature files (`.eli`) declare types for Elisp functions and variables:

```elisp
;; my-utils.eli
(module my-utils)

;; Function signatures
(sig my-utils-add (-> (Int Int) Int))
(sig my-utils-identity (forall (a) (-> (a) a)))

;; Variable declarations
(defvar my-utils-version String)

;; Type aliases
(type IntList = (List Int))

;; Algebraic data types
(data Result (a e)
  (Ok a)
  (Err e))
```

### Type Syntax

```elisp
;; Base types
Int String Symbol Nil T Bool Any Never

;; Function types (grouped parameters)
(-> (Int Int) Int)              ;; two args, returns Int
(-> (a) a)                      ;; polymorphic identity

;; Polymorphic types
(forall (a) (-> (a) a))         ;; explicit quantification

;; Type constructors
(List Int)                      ;; list of integers
(Option String)                 ;; String | Nil
(Or Int String)                 ;; union type

;; Optional and rest arguments
(-> (String &optional (Option Int)) String)
(-> (&rest String) String)
```

## Emacs Integration

### Eglot Setup

Add to your Emacs configuration:

```elisp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(emacs-lisp-mode . ("tart" "lsp"))))
```

> [!NOTE]
> LSP support is not yet implemented. This configuration is for future use.

## Project Status

### Completed

- Elisp S-expression parser with source locations
- Pure Elisp interpreter for macro expansion
- 50+ built-in functions (arithmetic, lists, strings, predicates)
- Special forms: `quote`, `if`, `let`, `let*`, `lambda`, `progn`, `setq`,
  `defun`, `defmacro`, `function`, `cond`, `or`, `and`, `while`
- Backquote/unquote evaluation
- Type inference engine with:
  - Union-find type variables
  - Constraint-based HM inference
  - Levels-based let-generalization
  - Value restriction for soundness
  - 40+ typed built-in functions
  - Structured diagnostics with source locations

### Planned

- Signature file parser (Spec 07)
- LSP server (Spec 08)
- CLI subcommands: `eval`, `expand`, `repl`, `lsp` (Spec 09)
- Emacs minor mode for REPL interaction (Spec 10)

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

## Development

### Entering the Development Environment

```bash
nix develop
```

This provides OCaml, dune, menhir, and all dependencies.

### Common Commands

| Command           | Description              |
| :---------------- | :----------------------- |
| `dune build`      | Build the project        |
| `dune test`       | Run all tests            |
| `dune fmt`        | Format code              |
| `dune exec tart`  | Run the CLI              |

### Running Tests

```bash
dune test
```

### Pre-commit Hooks

Install pre-commit hooks to run build, test, and format checks:

```bash
pre-commit install
```

## License

GPL-3.0-or-later

[nix]: https://nixos.org/download.html
