# Emacs Lisp Calling Conventions: Typing Implications

This document analyses Emacs Lisp's calling conventions and their implications for
static type analysis in tart.

## Overview

Emacs Lisp's calling conventions present significant challenges for static typing
due to the language's dynamic nature, pervasive use of higher-order functions, and
extensive metaprogramming facilities. This analysis covers function application,
higher-order functions, the advice system, hooks, interactive specifications, and
autoloading.

---

## 1. Function Application

### 1.1 Regular Calls

**How it works:**

Standard function calls in Elisp follow Lisp conventions:

```elisp
(foo arg1 arg2 arg3)
```

The function position is evaluated first (typically resolving a symbol to its
function definition), then arguments are evaluated left-to-right, and finally the
function is applied.

**Typing challenges:**

- **Symbol resolution**: The function position is typically a symbol. The type
  checker must resolve the symbol to its function definition, tracking through
  `fset`, `defalias`, and `defun`.

- **Optional and rest parameters**: Elisp functions support `&optional` and
  `&rest` parameters:
  ```elisp
  (defun example (required &optional opt1 opt2 &rest rest)
    ...)
  ```
  Type: `(-> String (Option Int) (Option Bool) (Rest Symbol) Result)`

- **No static arity checking**: Calling with wrong arity is a runtime error.
  The type checker must perform arity analysis.

**Tractability:** TRACTABLE

Standard function calls are the bread and butter of any type system. The main
work is building a symbol table mapping function names to their types.

**Potential approaches:**

1. Build a function environment during analysis
2. Support optional/rest parameter types with explicit syntax
3. Treat missing optional arguments as `nil` (requiring union with `Nil` type)


### 1.2 funcall - Dynamic Function Dispatch

**How it works:**

`funcall` applies a function value to arguments:

```elisp
(funcall fn arg1 arg2)
;; equivalent to (fn arg1 arg2) if fn were in function position

(funcall #'+ 1 2 3)           ; => 6
(funcall (lambda (x) (* x 2)) 5)  ; => 10

;; Common pattern: function stored in variable
(let ((handler (get-handler type)))
  (funcall handler data))
```

**Typing challenges:**

- **First-class functions**: The function being called is a runtime value. The
  type checker must track function types through variables.

- **Loss of static information**: When functions are stored in data structures
  or returned from other functions, their types may be difficult to track.

- **Dynamic dispatch patterns**: Common Elisp idiom of selecting functions at
  runtime based on conditions.

**Tractability:** TRACTABLE with limitations

Basic `funcall` usage is well-understood in typed functional languages. The
challenge is when function values flow through complex paths.

**Potential approaches:**

1. Track function types through let-bindings and function returns
2. Require type annotations when function origin is unclear
3. For stored functions, require the storage site to declare function type


### 1.3 apply - Spread Arguments

**How it works:**

`apply` calls a function with arguments spread from a list:

```elisp
(apply #'+ '(1 2 3))          ; => 6
(apply #'+ 1 2 '(3 4 5))      ; => 15 (partial args + list)
(apply #'list 'a 'b '(c d))   ; => (a b c d)

;; Common pattern: forwarding arguments
(defun wrapper (&rest args)
  (apply #'real-function args))
```

**Typing challenges:**

- **List to tuple unpacking**: The final list argument's length and element
  types must match the function's remaining parameter types. This requires
  dependent types or length-indexed vectors for full precision.

- **Heterogeneous lists**: Elisp lists can contain mixed types, but `apply`
  requires the types to match parameter positions.

- **Rest argument forwarding**: A common pattern that's sound but hard to verify
  without flow-sensitive analysis.

**Tractability:** HARD

Full precision requires dependent types (length-indexed lists). Practical
approaches must approximate.

**Potential approaches:**

1. **Homogeneous approximation**: Treat the spread list as `(List T)` where all
   elements have the same type, matching rest parameters

2. **Special-case common patterns**: Recognize `(apply f args)` where `args`
   comes directly from a `&rest` parameter of matching type

3. **Require annotation**: For complex cases, require explicit type assertions

4. **Trust boundary**: Mark `apply` results as requiring runtime verification


### 1.4 Type System Challenges Summary

| Pattern | Challenge | Approach |
|---------|-----------|----------|
| Regular calls | Symbol resolution, arity | Function environment |
| funcall | First-class functions | Track function types |
| apply with literal list | Tuple typing | Special-case analysis |
| apply with variable | List element types | Homogeneous approximation |
| apply forwarding &rest | Type preservation | Pattern recognition |

---

## 2. Higher-Order Functions

### 2.1 mapcar, mapc, mapcan

**How it works:**

These functions apply a function to each element of a list:

```elisp
(mapcar #'1+ '(1 2 3))        ; => (2 3 4)
(mapcar #'upcase '("a" "b")) ; => ("A" "B")
(mapc #'message '("a" "b"))   ; => ("a" "b"), side effects only
(mapcan #'list '(a b c))      ; => (a b c), nconc results
```

**Typing challenges:**

- **Polymorphism**: These functions are polymorphic in their input and output:
  ```
  mapcar : forall a b. (a -> b) -> List a -> List b
  mapc   : forall a b. (a -> b) -> List a -> List a
  mapcan : forall a b. (a -> List b) -> List a -> List b
  ```

- **Multiple list arguments**: `mapcar` accepts multiple lists:
  ```elisp
  (mapcar #'+ '(1 2 3) '(10 20 30))  ; => (11 22 33)
  ```
  Type becomes: `forall a b c. (a -> b -> c) -> List a -> List b -> List c`
  and generalizes to n lists.

- **Variadic polymorphism**: The multi-list case requires variadic type
  parameters or multiple overloaded signatures.

**Tractability:** TRACTABLE

Standard parametric polymorphism handles the common cases. Multi-list variants
can be handled with overloading.

**Potential approaches:**

1. Provide standard polymorphic types in prelude
2. Use rank-1 polymorphism with let-generalization
3. Handle multi-list variants with explicit overloads (2-list, 3-list versions)


### 2.2 cl-reduce, cl-remove-if, cl-find-if, etc.

**How it works:**

The `cl-lib` higher-order functions provide rich sequence operations:

```elisp
(cl-reduce #'+ '(1 2 3 4))                    ; => 10
(cl-reduce #'+ '(1 2 3) :initial-value 0)     ; => 6
(cl-remove-if #'evenp '(1 2 3 4 5))           ; => (1 3 5)
(cl-find-if #'evenp '(1 2 3 4))               ; => 2
(cl-sort '(3 1 2) #'<)                        ; => (1 2 3)
(cl-mapcar #'cons '(a b) '(1 2))              ; => ((a . 1) (b . 2))
```

**Typing challenges:**

- **Keyword arguments**: Most cl-lib functions accept keyword arguments that
  modify behavior:
  ```elisp
  (cl-reduce #'+ seq :key #'length :initial-value 0)
  ```
  The `:key` function transforms elements before the main operation.

- **Sequence polymorphism**: These work on lists, vectors, and strings:
  ```
  cl-remove-if : forall a. (a -> Bool) -> Sequence a -> Sequence a
  ```
  Requires a `Sequence` type class or union type.

- **Return type depends on input**: `cl-find-if` returns element or `nil`:
  ```
  cl-find-if : forall a. (a -> Bool) -> Sequence a -> (Option a)
  ```

**Tractability:** TRACTABLE with keyword argument complexity

The core higher-order pattern is standard. Keyword arguments add complexity.

**Potential approaches:**

1. Model keyword arguments as optional record fields
2. Provide multiple overloaded signatures for common keyword combinations
3. Use row polymorphism for keyword arguments (more complex)
4. Start with most common usage patterns, require annotations for complex cases


### 2.3 Typing Patterns for Higher-Order Functions

**Core polymorphic types needed:**

```
mapcar     : forall a b. (a -> b) -> List a -> List b
mapc       : forall a b. (a -> b) -> List a -> List a
mapcan     : forall a b. (a -> List b) -> List a -> List b
cl-reduce  : forall a b. (b -> a -> b) -> List a -> Option b -> b
cl-remove-if : forall a. (a -> Bool) -> List a -> List a
cl-find-if : forall a. (a -> Bool) -> List a -> Option a
cl-sort    : forall a. List a -> (a -> a -> Bool) -> List a
```

**Lambda inference:**

When lambdas are passed to higher-order functions, their parameter types can be
inferred from context:

```elisp
(mapcar (lambda (x) (+ x 1)) numbers)
;; x inferred as element type of numbers
```

This requires bidirectional type checking: the expected function type flows into
the lambda.

---

## 3. Advice System

### 3.1 advice-add and Advice Combinators

**How it works:**

Emacs advice system allows wrapping existing functions:

```elisp
;; Around advice: wraps the original
(defun my-around-advice (orig-fn &rest args)
  (message "Calling with %S" args)
  (let ((result (apply orig-fn args)))
    (message "Got %S" result)
    result))

(advice-add 'some-function :around #'my-around-advice)

;; Before advice: runs before original
(defun my-before-advice (&rest args)
  (message "About to call with %S" args))

(advice-add 'some-function :before #'my-before-advice)

;; After advice: runs after original
(defun my-after-advice (&rest args)
  (message "Called with %S" args))

(advice-add 'some-function :after #'my-after-advice)

;; Filter-args: transform arguments
(defun my-filter-args (args)
  (cons (1+ (car args)) (cdr args)))

(advice-add 'some-function :filter-args #'my-filter-args)

;; Filter-return: transform return value
(defun my-filter-return (result)
  (1+ result))

(advice-add 'some-function :filter-return #'my-filter-return)
```

**Typing challenges:**

- **Type modification**: Advice can change the effective type of a function:
  - `:filter-args` changes input types
  - `:filter-return` changes output type
  - `:around` can change both

- **Dynamic attachment**: Advice is added at runtime, potentially anywhere in
  the codebase or even from user configuration.

- **Multiple advice**: Functions can have multiple pieces of advice, composed
  in a specific order.

- **Type preservation obligation**: For sound typing, `:around` advice should
  preserve the type of the original function.

**Tractability:** HARD to INTRACTABLE for full soundness

The fundamental problem is that advice can be added from anywhere, at any time,
potentially changing a function's type in ways the type checker cannot see.

**Tractability assessment:**

- **Static advice (in same module)**: TRACTABLE - can analyze advice and
  compute composed type
- **Advice from other modules**: HARD - requires cross-module analysis
- **Runtime/user advice**: INTRACTABLE - cannot be statically analyzed

**Potential approaches:**

1. **Ignore advice for typing**: Treat advised functions with their original
   types, document this as a soundness escape hatch

2. **Type-preserving advice only**: For type-checked code, require that advice
   preserves the original function's type (`:before`, `:after`, type-preserving
   `:around`)

3. **Advice declarations**: Allow declaring that advice exists and its type
   effect:
   ```
   ;; In .tart file
   (declare-advice some-function :filter-return (-> Int String))
   ```

4. **Track local advice**: Within a module, track `advice-add` calls and
   compute modified types for local usage


### 3.2 How Advice Affects Function Types

| Advice Type | Type Effect | Typeable? |
|-------------|-------------|-----------|
| :before | None (side effects only) | Yes |
| :after | None (side effects only) | Yes |
| :around | Arbitrary (if poorly written) | Conditional |
| :around (type-preserving) | None | Yes |
| :filter-args | Changes domain | With declaration |
| :filter-return | Changes codomain | With declaration |
| :override | Replaces entirely | With declaration |

---

## 4. Hooks

### 4.1 add-hook and run-hooks

**How it works:**

Hooks are lists of functions called at specific points:

```elisp
;; Defining a hook
(defvar my-mode-hook nil
  "Hook run when entering my-mode.")

;; Adding to a hook
(add-hook 'my-mode-hook #'my-setup-function)
(add-hook 'my-mode-hook (lambda () (setq foo t)))

;; Running hooks
(run-hooks 'my-mode-hook)

;; Hooks with arguments
(defvar my-process-hook nil
  "Hook with one argument.")
(run-hook-with-args 'my-process-hook data)
(run-hook-with-args-until-success 'my-process-hook data)
(run-hook-with-args-until-failure 'my-process-hook data)
```

**Typing challenges:**

- **Function list type**: A hook variable holds `(List (-> Args Result))`. The
  function type must be consistent across all hook members.

- **Implicit protocol**: The expected function signature for a hook is often
  only documented, not enforced.

- **Cross-module population**: Hooks are typically populated from many modules,
  making type consistency hard to verify globally.

- **run-hook variants**: Different hook runners have different semantics:
  - `run-hooks`: All functions called, results discarded
  - `run-hook-with-args`: All functions called with args, results discarded
  - `run-hook-with-args-until-success`: Stop on first non-nil return
  - `run-hook-with-args-until-failure`: Stop on first nil return

**Tractability:** TRACTABLE for declared hooks

The pattern is essentially: `hook : List (-> ArgType Unit)`. The challenge is
ensuring consistency across a codebase.

**Potential approaches:**

1. **Hook type declarations**: In `.tart` files, declare hook types:
   ```
   (defhook my-mode-hook (-> Unit))
   (defhook my-process-hook (-> ProcessData Unit))
   ```

2. **Infer from run-hook calls**: If `run-hook-with-args` is called with
   specific types, infer the expected function signature.

3. **Check add-hook calls**: Verify that functions added to hooks match the
   declared type.

4. **Prelude for standard hooks**: Provide types for Emacs built-in hooks like
   `after-init-hook`, `find-file-hook`, etc.


### 4.2 Type Constraints for Hook Variables

A hook variable has type:

```
my-mode-hook : List (-> Unit)
after-save-hook : List (-> Unit)
find-file-hook : List (-> Unit)
post-command-hook : List (-> Unit)
before-change-functions : List (-> Int Int Unit)  ; beg end
after-change-functions : List (-> Int Int Int Unit)  ; beg end old-len
```

The type checker should:
1. Track hook variable types (from declarations or inference)
2. Check `add-hook` adds compatible function types
3. Verify `run-hook-with-args` passes correct argument types

---

## 5. Interactive Specifications

### 5.1 The interactive Declaration

**How it works:**

The `interactive` form declares how a function receives arguments when called
as a command:

```elisp
;; No arguments
(defun my-command ()
  (interactive)
  (message "Hello"))

;; Arguments from minibuffer
(defun greet (name)
  (interactive "sName: ")
  (message "Hello, %s!" name))

;; Complex interactive specs
(defun operate-on-region (beg end text)
  (interactive "r\nsReplacement: ")
  ...)

;; Programmatic interactive spec
(defun flexible-command (arg)
  (interactive
   (list (read-string "Input: ")))
  ...)
```

**Interactive spec codes (selection):**

| Code | Meaning | Type |
|------|---------|------|
| s | String from minibuffer | String |
| n | Number from minibuffer | Number |
| b | Buffer name | String |
| B | Buffer name (may not exist) | String |
| f | Existing file name | String |
| F | File name (may not exist) | String |
| r | Region (two marks) | Int, Int |
| p | Prefix arg as number | Int |
| P | Raw prefix arg | (Option (List Int)) |

**Typing challenges:**

- **Two calling conventions**: Interactive commands can be called:
  1. Interactively (via `M-x` or keybinding) - arguments from interactive spec
  2. Programmatically (from Lisp) - arguments passed directly

- **Type mismatch possible**: The interactive spec might provide different
  types than programmatic calls expect:
  ```elisp
  (defun bad-example (n)  ; expects number
    (interactive "sInput: ")  ; provides string!
    (+ n 1))
  ```

- **Spec interpretation**: The interactive spec is a mini-language that must
  be parsed to understand argument types.

**Tractability:** TRACTABLE

Interactive specs are a well-defined mini-language. The main work is parsing
specs and verifying consistency with function parameter types.

**Potential approaches:**

1. **Parse interactive specs**: Extract expected types from spec codes

2. **Verify consistency**: Check that interactive spec types match parameter
   types (or are subtypes)

3. **Dual type signature**: Conceptually, a command has two signatures:
   - Interactive: `(-> Unit)` with side-effecting argument acquisition
   - Programmatic: `(-> ArgTypes Result)`

4. **Command type**: Introduce a `Command` type that wraps function types:
   ```
   greet : Command { args: (String), interactive: "sName: " }
   ```


### 5.2 Typing Interactive Commands

Proposed approach:

```
;; A command is a function with an interactive spec
(defcommand greet (name : String) : Unit
  (interactive "sName: ")
  (message "Hello, %s!" name))

;; Type checker verifies:
;; 1. "s" spec produces String, matching parameter type
;; 2. Function body type-checks with name : String
```

For complex interactive specs:

```elisp
(defun complex-cmd (beg end text)
  (interactive "r\nsText: ")
  ...)

;; Spec "r" produces (Int, Int) for beg, end
;; Spec "s" produces String for text
;; Type: (-> Int Int String Unit)
```

---

## 6. Autoloading

### 6.1 The Autoload Mechanism

**How it works:**

Autoloading defers loading a file until a function is first called:

```elisp
;; In loaddefs.el or generated autoloads
(autoload 'some-function "some-file" "Documentation." t)
;; Args: symbol, file, docstring, interactive-p, type

;; Usage - file loaded on first call
(some-function arg1 arg2)

;; Magic comments for autoload generation
;;;###autoload
(defun my-function (x)
  "Do something with X."
  ...)

;; Autoloaded macro
;;;###autoload
(defmacro my-macro (form)
  ...)

;; Autoloaded variable
;;;###autoload
(defvar my-variable 42)
```

**Typing challenges:**

- **Type without definition**: At the autoload site, only the function name
  and maybe docstring are known. The actual definition (and thus full type)
  is in another file.

- **Deferred loading**: The file containing the definition may not be analyzed
  yet when the autoload is encountered.

- **Generated autoloads**: `###autoload` cookies cause definitions to be
  extracted to `*-autoloads.el` files, separating declaration from definition.

- **Type consistency**: The autoload declaration and actual definition must
  have compatible types.

**Tractability:** TRACTABLE with project-wide analysis

Autoloading is fundamentally about deferred linking. The type information
exists, just in a different file.

**Potential approaches:**

1. **Signature files (.tart)**: The `.tart` file provides types for autoloaded
   functions without requiring the implementation file to be loaded:
   ```
   ;; some-file.tart
   (declare some-function (-> String Int Bool))
   ```

2. **Two-pass analysis**:
   - Pass 1: Collect all function signatures (from definitions and .tart files)
   - Pass 2: Type-check function bodies and call sites

3. **Autoload type declarations**: Allow type annotations in autoload forms:
   ```elisp
   ;;;###autoload (-> String Int Bool)
   (defun my-function (s n)
     ...)
   ```

4. **Cross-reference verification**: When both autoload and definition are
   available, verify type consistency.


### 6.2 Type Availability Timeline

```
Time 0: Autoload registered
        Type available: Only from .tart or autoload annotation

Time 1: File loaded (on first use)
        Type available: Full definition accessible

Type checker must handle both states:
- Before load: Use declared/annotated type
- After analysis: Verify declaration matches definition
```

---

## 7. Summary and Recommendations

### Tractability Overview

| Feature | Tractability | v1 Recommendation |
|---------|--------------|-------------------|
| Regular function calls | TRACTABLE | Full support |
| funcall | TRACTABLE | Full support |
| apply (simple) | TRACTABLE | Support common patterns |
| apply (complex) | HARD | Require annotations |
| Higher-order (mapcar, etc.) | TRACTABLE | Full support |
| cl-lib HOFs with keywords | TRACTABLE | Common overloads |
| Advice (:before/:after) | TRACTABLE | Type-preserving only |
| Advice (:around/:filter-*) | HARD | Declare or ignore |
| Hooks | TRACTABLE | Declared hooks |
| Interactive specs | TRACTABLE | Parse and verify |
| Autoloading | TRACTABLE | .tart files |

### v1 Scope Recommendations

**Include in v1:**

1. Standard function calls with full arity checking
2. `funcall` with tracked function types
3. Simple `apply` patterns (especially `&rest` forwarding)
4. Standard higher-order functions with polymorphic types
5. Type-preserving advice (`:before`, `:after`)
6. Declared hook types
7. Interactive spec parsing and verification
8. Autoload support via `.tart` files

**Defer to v2:**

1. Complex `apply` with heterogeneous lists
2. Type-modifying advice (`:filter-args`, `:filter-return`)
3. Cross-module advice tracking
4. Keyword argument polymorphism for cl-lib
5. Automatic hook type inference

**Explicitly out of scope:**

1. Runtime-added advice from user configuration
2. Dynamic hook modification
3. Fully sound handling of arbitrary `apply`

### Key Implementation Requirements

1. **Function environment**: Map symbols to function types, handling `defun`,
   `defalias`, `fset`

2. **Polymorphic type system**: System F or HM to type higher-order functions

3. **Signature files**: `.tart` format for declaring types of external/autoloaded
   functions

4. **Interactive spec parser**: Mini-parser for interactive specs to extract
   argument types

5. **Hook type tracking**: Associate hook variables with expected function types

6. **Advice awareness**: At minimum, recognize advice and warn about potential
   type unsoundness
