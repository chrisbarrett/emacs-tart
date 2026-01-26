# Tart

Tart is a static type system for Emacs Lisp--think TypeScript but for Emacs Lisp
and you're on the right track.

It's designed to be unobtrusive and is designed from the ground up to support
idiomatic Emacs Lisp library code. There's no runtime component--just pure
static analysis. :)

The big ideas are:

- write `.tart` files that declare the public interface for your libraries
- what you write in your library is type-checked against that interface
- users `require`s your lib as normal

The `.tart` file doesn't change anything from the consumer's perspective...
unless they also use Tart, in which case your public types are used when
typechecking their code too. 👌

> [!WARNING]
> Tart is under active development and is not yet usable. See
> [DEVELOPMENT.md](DEVELOPMENT.md) for build instructions and project status.

> [!NOTE]
> Heavy use of AI assistance in this codebase. You have been warned. ☠️

## Quick Tour

The public interface for your libraries are written in `.tart` files with the
same basename. That's where most of this happens.

Tart also provides a few ways to write type declarations inside regular Emacs
Lisp; these are just used by the type-checker and have no runtime effect.

### Basic Types

Tart defines the types you'd expect for Emacs' primitive data structures. You
can declare your own types to make type signatures more meaningful.

```elisp
(type Id Int)
```

Union types are defined like so:

```elisp
(type OptString (String | Nil))
```

The normal type checks you'd do in an `if`, `cond`, `when`, etc will narrow down
which type you actually have in a given branch.

### Variables

Variables (`defvar`, `defconst` and friends) are all declared via `defvar`.

```elisp
(defvar my-variable String) ; my-variable is a value of type 'String'
```

Tart will type-check the definition and how it gets used.

```elisp
;; regular elisp code

(defvar my-variable "bar")

(setq my-variable 1) ; => type error
(setq my-variable nil) ; => type error
(setq my-variable "ok")
```

### Truthiness

This is a fun one!

In Emacs Lisp, `nil` is always logically false, and anything else is considered
'truthy'. This notion is central to Emacs Lisp, and all sorts of language
features and idioms depend on it, which mean it's very normal to have 'optional'
values in your program. Tart knows about it and helps you work with it.

The `Any` type comes with Tart, for when you just need to check if something is
truthy and don't need to know exactly what it is:

```elisp
(type Any (Truthy | Nil))
```

There's also an `Option` type, for when you _do_ know:

```elisp
(type Option (a | Nil))
```

Checking a value's truthiness will narrow a type as you'd hope. Let's use the
`tart` macro to do a type assertion and see how this plays out:

```elisp
; regular elisp code

(defvar mystery (tart (Option String) nil))

(setq mystery "hello!")
(setq mystery nil)
(setq mystery 1) ; => type error

(if mystery

  (tart Truthy mystery) ; ok - mystery is non-nil in this branch

 (tart Truthy mystery) ; => type error - mystery is nil in this branch
 )
```

### Functions

Functions are declared in `.tart` files with `defun`. The return type is after
the arrow.

```elisp
(defun my-add (Number Number) -> Number)
```

Callers will then get red squiggly type errors in their own code.

```elisp
;; regular elisp code

(require 'my)

(my-add 1 "hello") ; => type error
```

It is sometimes useful to declare types for internal functions too. In regular
Emacs Lisp files, you can put the type signature in a `(declare (tart ...))`
spec.

```elisp
;; regular elisp code

(defun my-package--internal-add (n m)
  (declare (tart (Number Number) -> Number))
  (+ n m))
```

Tart supports `&rest`, `&optional`, `&key` and `&allow-other-keys`.

```elisp
(defun my-greet (String &optional String) -> String)
(defun my-sum (&rest Int) -> Int)
(defun my-make-person (&key :name String :age Int) -> Person)
```

Optional parameters and keys are inferred as `Option a`. `&rest` becomes
`List a`.

### Generic Functions

Lowercase type names introduce type parameters.

```elisp
(defun seq-map ((a -> b) (List a)) -> (List b))
```

There's syntax for explicitly declaring type parameters, but you probably won't
need it.

```elisp
(defun seq-map [a b] ((a -> b) (List a)) -> (List b))
```

### Named vs Anonymous Functions

Emacs Lisp treats values and named functions differently. The type-checker makes
sure you use `funcall` or `apply` correctly.

```elisp
;; regular elisp code

(let ((fn (lambda () "hello")))
  (fn)) ; => type error - values cannot can be called directly.
        ;                 Use `funcall' or `apply' to call `fn'.


(let ((fn (lambda () "hello")))
  (funcall fn) ; ok
  (apply fn))  ; ok
```

### Parameters

See [tart-format.md](docs/reference/tart-format.md) for the full specification.
