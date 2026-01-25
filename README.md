# Tart

A static type checker for Emacs Lisp.

Tart provides Hindley-Milner type inference for Elisp code, catching type errors
at edit time via LSP integration with Eglot.

> [!WARNING]
> Tart is under active development. The parser and interpreter are functional,
> but type inference and LSP features are not yet implemented.

## Features

- **Type inference** — Principal types inferred without annotations
- **Sound typing** — No `any` escape hatch; errors caught at compile time
- **LSP integration** — Diagnostics, hover types, and more via Eglot
- **Signature files** — `.eli` files declare types for untyped code
- **Macro expansion** — Built-in pure Elisp interpreter expands macros

## Installation

### Prerequisites

- [Nix][nix] with flakes enabled

### Building from Source

```bash
git clone https://github.com/chrisbarrett/emacs-tart.git
cd emacs-tart
nix develop
dune build
```

The `tart` executable will be available in `_build/default/bin/tart.exe`, or run
directly with:

```bash
dune exec tart -- --help
```

### Running Tests

```bash
nix develop
dune test
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

### In Progress

- Type inference engine (Spec 06)

### Planned

- Signature file parser (Spec 07)
- LSP server (Spec 08)
- CLI subcommands: `eval`, `expand`, `repl`, `lsp` (Spec 09)

## Architecture

```
tart/
├── bin/main.ml           # CLI entry point
├── lib/
│   ├── syntax/           # Parser and lexer
│   │   ├── lexer.mll     # Ocamllex lexer
│   │   ├── parser.mly    # Menhir parser
│   │   ├── sexp.ml       # S-expression AST
│   │   └── location.ml   # Source locations
│   └── interp/           # Interpreter
│       ├── value.ml      # Runtime values
│       ├── env.ml        # Environments
│       ├── builtin.ml    # Built-in functions
│       ├── eval.ml       # Core evaluator
│       └── expand.ml     # Macro expansion
└── test/                 # Test suites
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

### Pre-commit Hooks

Install pre-commit hooks to run build, test, and format checks:

```bash
pre-commit install
```

## License

GPL-3.0-or-later

[nix]: https://nixos.org/download.html
