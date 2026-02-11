# Getting Started

Tart is a static type checker for Emacs Lisp. Think of it like TypeScript for
JavaScript: you get meaningful error messages at development time, without any
runtime overhead.

Here's the idea:

- Write `.tart` files that declare the types for your public API
- The type checker verifies your implementation matches those declarations
- Consumers of your library get type checking for their calls into your code

Tart has no runtime component. Users of your library don't need to know you used
Tart to check your types.

## Installation

### Managed Binary (Recommended)

The easiest way to get started is with `tart-mode`, which downloads the binary
automatically:

```elisp
(add-to-list 'load-path "/path/to/emacs-tart/lisp")
(require 'tart-mode)
(add-hook 'emacs-lisp-mode-hook #'tart-eglot)
```

The first time you open an `.el` file with a sibling `.tart` file, `tart-eglot`
prompts to download the binary. You can also run `M-x tart-install-binary`
manually.

`tart-executable` defaults to `'managed`. To use a custom binary, set it to a
path string.

### Building from Source

```bash
git clone https://github.com/chrisbarrett/emacs-tart.git
cd emacs-tart
nix build
```

Then point `tart-executable` to your binary:

```elisp
(setq tart-executable "/path/to/emacs-tart/result/bin/tart")
```

## Your First .tart File

Create a simple Emacs Lisp file and its type signature side by side.

**greeter.el:**

```elisp
(defun greet (name)
  (concat "Hello, " name "!"))
```

**greeter.tart:**

```elisp
(defun greet (string) -> string)
```

The `.tart` file declares that `greet` takes a `string` and returns a `string`.
Tart verifies that the implementation in `greeter.el` conforms to this
signature.

Now if someone calls `greet` with the wrong type:

```elisp
(greet 42)
```

Tart catches the error at development time:

```
greeter.el:4:8: error: type mismatch
  expected: string
  found: int
```

## Running the Type Checker

Run `tart check` on your `.el` file:

```bash
tart check greeter.el
```

On success the command is silent and exits with code 0. On failure you get
errors like the one shown above.

> [!TIP]
> `check` is the default subcommand, so `tart greeter.el` works too.

## Inline Type Annotations

Sometimes you want to annotate types directly in your `.el` file, without a
separate `.tart` file. Tart provides two mechanisms for this.

### declare

Add a `(tart ...)` clause to a function's `declare` form to specify its
signature inline:

```elisp
(defun my-internal-fn (x y)
  (declare (tart (int int) -> int))
  (+ x y))
```

### tart macro

Use the `tart` macro to assert that an expression has a specific type:

```elisp
1            ; 1 :: int
(tart num 1) ; 1 :: num — upcast to the more general numeric type

1.0            ; 1.0 :: float
(tart num 1.0) ; 1.0 :: num
(tart int 1.0) ; type error — that's a float
```

The `tart` macro is a no-op at runtime. It is mainly useful for _upcasting_ a
value to a more general type.

See [Writing Typings][writing-typings] for the full annotation reference.

## Editor Integration

Add two lines to your Emacs config for LSP-powered type checking as you edit:

```elisp
(require 'tart-mode)
(add-hook 'emacs-lisp-mode-hook #'tart-eglot-ensure)
```

This gives you live diagnostics, hover information, go-to-definition, and
completion via Eglot and Flymake.

See [Tooling Setup][tooling-setup] for detailed editor configuration.

## REPL Quickstart

Tart includes an interactive REPL for exploring types and evaluating
expressions:

```bash
tart repl
```

Example session:

```
tart> (+ 1 2)
3 :: '_1
tart> (list 1 2 3)
(1 2 3) :: (list int)
tart> ,type (list 1 2 3)
(list int)
tart> ,expand (when t 1)
(when t 1)
tart> ,help
REPL Commands:
  ,quit, ,q      Exit REPL
  ,type <expr>   Show type without evaluating
  ,expand <expr> Show macro expansion
  ,env           List current bindings
  ,help          Show this help
tart> ,quit
```

## Next Steps

- [Tooling Setup][tooling-setup] — Editor configuration and LSP integration
- [CLI Reference][cli-reference] — Full command-line usage
- [Writing Typings][writing-typings] — How to write `.tart` files for your
  packages
- [Type System Reference][type-system] — Types, unions, generics, and more
- [.tart Format Reference][tart-format] — Grammar and syntax for `.tart` files

[tooling-setup]: tooling-setup.md
[cli-reference]: cli-reference.md
[writing-typings]: reference/writing-typings.md
[type-system]: reference/type-system.md
[tart-format]: reference/tart-format.md
