# Tart 🍰

Tart is a static type checker built for Emacs Lisp that gives you the meaningful
red squiggles you deserve.

If you're an Emacs user, the goal is: you install Tart. You get squiggles.

If you're a library author, you can use Tart to define a type signature for your
package with a `.tart` file. Tart checks your code conforms to it, and uses it
to provide those tasty squiggles to your consumers.

> [!NOTE]
> Tart has no runtime component. Users of your library don't need to know you
> used tart to check your types. 🎉

## What goes into a Tart?

Tart is a single binary. You can build from source, or (when I get around to
setting this up) download a pre-built version. You can run tart standalone with
`M-x compile`, or use the built-in LSP for incremental type-checking.

Tart comes with two Emacs Lisp libraries:

- **tart** for library authors: macros for writing type annotations. They're
    zero-cost at runtime.

- **tart-mode** for authors & users: a minor mode for Tart's commands and
    integrating the LSP with Eglot.


## The Problem

Emacs Lisp's dynamic typing means type errors only appear at runtime:

```elisp
(defun greet (name)
  (concat "Hello, " name "!"))

(greet 42)  ; Runtime error: wrong-type-argument stringp 42
```

## The Solution

Install Tart. Now you have a static type checker.

```elisp
(defun greet (name)
  (concat "Hello, " name "!"))

(greet 42)  ; red squiggle from eglot/flymake:

;; greeter.el:4:8: error: type mismatch
;; expected: string
;; found: int
;; note: `greet` expects argument 1 to be string
```

## Package Interfaces

If you're a package author, drop a `.tart` file next to your emacs lisp file.
It'll define the public interface to your package and make sure you don't break
it.

```elisp
;; greeter.tart
(defun greet (string) -> string)
```

Right now, tart will verify functions, variables, and let you export your own
types. You can do fun things like polymorphic functions (generics), overloaded
signatures and union types.

In glorious ML tradition, exported types can be **opaque**, allowing you to pass
data across your package's boundary without guaranteeing the representation
details.

## Installation

Add the lisp directory to your load path and enable tart-mode:

```elisp
(add-to-list 'load-path "/path/to/emacs-tart/lisp")
(require 'tart-mode)

(add-hook 'emacs-lisp-mode-hook #'tart-mode)
(add-hook 'emacs-lisp-mode-hook #'tart-eglot)
```

The first time you open an Emacs Lisp file with a sibling `.tart` file,
`tart-eglot` will prompt you to download the binary. You can also run
`M-x tart-install-binary` manually.

### Customization

| Variable                | Description                                      |
|:------------------------|:-------------------------------------------------|
| `tart-executable`       | `'managed` (default) or path to custom binary    |
| `tart-version`          | `'latest` (default) or version string            |
| `tart-install-directory`| Where managed binaries are stored                |

### Building from source

If you prefer to build from source:

```bash
git clone https://github.com/chrisbarrett/emacs-tart.git
cd emacs-tart
nix build
```

Then point `tart-executable` to your binary:

```elisp
(setq tart-executable "/path/to/emacs-tart/result/bin/tart")
```

## Examples

That's great, but what does it look like?

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

### Inline annotations

Tart will treat `(declare (tart ...))` spec as a type annotation, allowing you
to nail down the types of internal functions when it's helpful to do so.

```elisp
;; In your .el file
(defun my-internal-fn (x y)
  (declare (tart (int int) -> int))
  (+ x y))
```

You can also use the `tart` macro for type annotations.

``` elisp
1            ; 1 :: int
(tart num 1) ; 1 :: num

1.0            ; 1.0 :: float
(tart num 1.0) ; 1.0 :: num
(tart int 1.0) ; type error. that's a float dawg.
```

> [!TIP]
> Tart doesn't need type annotations most of the time, but they can be helpful
> for *upcasting* to a more general type.

## Documentation

- **[Getting Started](docs/getting-started.adoc)** -- Tutorial for new users
- **[Library Authors Guide](docs/library-authors.adoc)** -- Writing `.tart` files
- **[Tooling Setup](docs/tooling-setup.adoc)** -- Editor configuration
- **[CLI Reference](docs/cli-reference.adoc)** -- Command-line usage
