# Tart

A static type checker for Emacs Lisp.

Tart provides Hindley-Milner type inference for Elisp code, catching type errors
at edit time via LSP integration with Eglot.

> [!WARNING]
> Tart is under active development and is not yet usable. See
> [DEVELOPMENT.md](DEVELOPMENT.md) for build instructions and project status.

## Signature Files

Type signatures live in `.tart` files alongside your Elisp:

```elisp
;; my-utils.tart
(module my-utils)

(open 'seq)                                   ; import types from another module

(type Handler (String -> Nil))                ; type alias
(opaque Cache)                                ; abstract type (hidden internals)

(defun my-utils-trim String -> String)
(defun my-utils-map [a b] ((a -> b) (List a)) -> (List b))
(defvar my-utils-handler Handler)
```

Tart searches for `.tart` files when you `require` a module, so you can provide
types for any package—even ones you don't control.

See [tart-format.md](docs/reference/tart-format.md) for the full specification.

> [!NOTE]
> Heavy use of AI assistance in this codebase. You have been warned.
