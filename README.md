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
typechecking their code too.

> [!WARNING]
> Tart is under active development and is not yet usable. See
> [DEVELOPMENT.md](DEVELOPMENT.md) for build instructions and project status.

> [!NOTE]
> Heavy use of AI assistance in this codebase. You have been warned.

## Quick Tour

The public interface for your libraries are written in `.tart` files with the
same basename. That's where most of this happens.

Tart also provides a few ways to write type declarations inside regular Emacs
Lisp; these are just used by the type-checker and have no runtime effect.

### Basic Types

Tart defines the types you'd expect for Emacs' primitive data structures. You
can declare your own types to make type signatures more meaningful.

```elisp
(type id int)
```

Union types are defined like so:

```elisp
(type opt-string (string | nil))
```

The normal type checks you'd do in an `if`, `cond`, `when`, etc will narrow down
which type you actually have in a given branch.

### Variables

Variables (`defvar`, `defconst` and friends) are all declared via `defvar`.

```elisp
(defvar my-variable string) ; my-variable is a value of type 'string'
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

The `any` type is defined in the standard library, for when you just need to
check if something is truthy and don't need to know exactly what it is:

```elisp
(type any (truthy | nil))
```

There's also an `option` type, for when you _do_ know:

```elisp
(type option [(a : truthy)] (a | nil))
```

The `(a : truthy)` part is a _bound_--it says the type parameter `a` must be
truthy (i.e., can't itself be `nil`). This ensures you can always distinguish
"no value" from "has a value".

Checking a value's truthiness will narrow a type as you'd hope. Let's use the
`tart` macro to do a type assertion and see how this plays out:

```elisp
; regular elisp code

(defvar mystery (tart (option string) nil))

(setq mystery "hello!")
(setq mystery nil)
(setq mystery 1) ; => type error

(if mystery

  (tart truthy mystery) ; ok - mystery is non-nil in this branch

 (tart truthy mystery) ; => type error - mystery is nil in this branch
 )
```

### Functions

Functions are declared in `.tart` files with `defun`. Params are in parentheses,
and the return type is after the arrow.

```elisp
(defun my-add (num num) -> num)
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
  (declare (tart (num num) -> num))
  (+ n m))
```

Tart supports `&rest`, `&optional`, `&key` and `&allow-other-keys`.

```elisp
(defun my-greet (string &optional string) -> string)
(defun my-sum (&rest int) -> int)
(defun my-make-person (&key :name string :age int) -> person)
```

Optional parameters and keys are inferred as `(t | nil)`. `&rest` becomes
`(list t)`.

### Generic Functions

Type parameters must be explicitly declared in square brackets after the
function name.

```elisp
(defun seq-map [a b] (((a -> b)) (list a)) -> (list b))
```

Type parameters can have bounds:

```elisp
(defun unwrap-or [(a : truthy)] ((a | nil) a) -> a)
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
