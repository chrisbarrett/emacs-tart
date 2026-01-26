# Tart

**Catch type errors in your Emacs Lisp before runtime.**

Tart is a static type checker for Emacs Lisp. Write type declarations for your
library's public functions, and Tart verifies your implementation matches--no
runtime overhead, no code changes required.

## The Problem

Emacs Lisp's dynamic typing means type errors only appear at runtime:

```elisp
(defun greet (name)
  (concat "Hello, " name "!"))

(greet 42)  ; Runtime error: wrong-type-argument stringp 42
```

## The Solution

Add a `.tart` file next to your `.el` file:

```elisp
;; greeter.tart
(defun greet (string) -> string)
```

Now Tart catches the error before you even run the code:

```
greeter.el:4:8: error: type mismatch
  expected: String
  found: Int
  note: `greet` expects argument 1 to be String
```

## Features

- **Zero runtime cost** -- Tart is purely static analysis
- **Gradual typing** -- Add types incrementally; untyped code keeps working
- **LSP integration** -- Real-time feedback in Emacs via eglot
- **Familiar syntax** -- Type declarations look like Elisp
- **Comprehensive stdlib** -- Built-in signatures for common packages (dash, s, f, cl-lib, seq, and more)

## Quick Start

### 1. Install

```bash
git clone https://github.com/chrisbarrett/emacs-tart.git
cd emacs-tart
nix develop
dune build
```

### 2. Create a .tart file

For `my-lib.el`, create `my-lib.tart`:

```elisp
;; my-lib.tart
(defun my-lib-process (string int) -> string)
(defvar my-lib-timeout int)
```

### 3. Type-check

```bash
tart my-lib.el
```

### 4. Get editor feedback

```elisp
;; In your Emacs config
(add-to-list 'load-path "/path/to/emacs-tart/lisp")
(require 'tart-mode)
(add-hook 'emacs-lisp-mode-hook #'tart-eglot-ensure)
```

## Examples

### Generic functions

```elisp
;; Type parameters in square brackets
(defun my-map [a b] (((a) -> b) (list a)) -> (list b))
```

### Union types

```elisp
;; A value that might be nil
(defun my-find (string) -> (string | nil))
```

### Type classes

```elisp
;; Constrained polymorphism
(defun my-sort [a] (Ord a) => ((list a)) -> (list a))
```

### Inline annotations

```elisp
;; In your .el file
(defun my-internal-fn (x y)
  (declare (tart (int int) -> int))
  (+ x y))
```

## Documentation

- **[Getting Started](docs/getting-started.adoc)** -- Tutorial for new users
- **[Library Authors Guide](docs/library-authors.adoc)** -- Writing `.tart` files
- **[Tooling Setup](docs/tooling-setup.adoc)** -- Editor configuration
- **[CLI Reference](docs/cli-reference.adoc)** -- Command-line usage

## Status

Tart is under active development. Core type checking works well, but APIs may
change. Contributions and bug reports welcome!

## License

GPL-3.0
