# Tart

A static type checker for Emacs Lisp.

Tart provides Hindley-Milner type inference for Elisp code, catching type errors
at edit time via LSP integration with Eglot.

> [!WARNING]
> Tart is under active development and is not yet usable. See
> [DEVELOPMENT.md](DEVELOPMENT.md) for build instructions and project status.

> [!NOTE]
> Heavy use of AI assistance in this codebase. You have been warned. ☠️

## Signature Files

Type signatures live in `.tart` files. For example, types for `seq.el`:

```elisp
;; seq.tart
(type Seqable (Or (List a) (Vector a) String))

(defun seq-map [a b] ((a -> b) (Seqable a)) -> (List b))
(defun seq-filter [a] ((a -> Bool) (Seqable a)) -> (List a))
(defun seq-reduce [a b] ((b a -> b) b (Seqable a)) -> b)
(defun seq-find [a] ((a -> Bool) (Seqable a)) -> (Option a))
(defun seq-empty-p [a] (Seqable a) -> Bool)
```

Tart searches for `.tart` files when you `require` a module, so you can provide
types for any package—even ones you don't control.

See [tart-format.md](docs/reference/tart-format.md) for the full specification.
