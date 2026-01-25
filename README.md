# Tart

A static type checker for Emacs Lisp.

Tart catches type errors at edit time via LSP. There's no runtime component—your
Elisp runs exactly as before, but with compile-time safety. (Tart provides some
Elisp macros for embedding type annotations; these expand to nothing at runtime.)

Type your implementation details for internal safety, then declare a public API
in a `.tart` signature file. Only what you export is visible to consumers.

> [!WARNING]
> Tart is under active development and is not yet usable. See
> [DEVELOPMENT.md](DEVELOPMENT.md) for build instructions and project status.

> [!NOTE]
> Heavy use of AI assistance in this codebase. You have been warned. ☠️

## Quick Tour

### Functions

```elisp
(defun add (Int Int) -> Int)
(defun identity a -> a)                      ; Polymorphic
(defun map ((a -> b) (List a)) -> (List b))  ; Higher-order
```

### Variables

```elisp
(defvar default-name String)
(defvar handler (String -> Nil))             ; Function value (use with funcall)
```

### Types

```elisp
(type UserId Int)                            ; Alias
(type Result (Or 'ok 'error))                ; Union with literal symbols
(type Buffer)                                ; Opaque (no definition)
```

### Parameters

```elisp
(defun greet (String &optional String) -> String)
(defun sum (&rest Int) -> Int)
(defun make (&key :name String :age Int) -> Person)
```

## Signature Files

A `.tart` file declares the **public interface** of a module. Only what appears
in the `.tart` file is visible to consumers—implementation details stay hidden.

For example, `seq.tart` provides types for `seq.el`:

```elisp
;; seq.tart
(type Seq (Or (List a) (Vector a) String))

(defun seq-map ((a -> b) (Seq a)) -> (List b))
(defun seq-filter ((a -> Bool) (Seq a)) -> (List a))
(defun seq-reduce ((b a -> b) b (Seq a)) -> b)
(defun seq-find ((a -> Bool) (Seq a)) -> (Option a))
(defun seq-empty-p (Seq a) -> Bool)
```

Lowercase identifiers like `a` and `b` are automatically universally quantified.
Explicit quantifiers (`[a b]`) are still supported for phantom types.

Tart searches for `.tart` files when you `require` a module, so you can provide
types for any package—even ones you don't control.

See [tart-format.md](docs/reference/tart-format.md) for the full specification.
