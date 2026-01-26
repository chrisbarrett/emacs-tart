# Emacs Lisp Ecosystem Conventions

Analysis of Emacs Lisp ecosystem conventions relevant to type checking for tart.

## 1. Naming Conventions

### Package-Name-Function Pattern

Emacs Lisp uses a flat namespace with a package prefix convention:

```elisp
;; Package: org-mode
(defun org-mode ...)
(defun org-schedule ...)
(defun org-get-heading ...)

;; Package: magit
(defun magit-status ...)
(defun magit-commit ...)
```

**Type checking implications:**
- Package prefix provides implicit module boundaries
- Can infer package membership from function name prefix
- Enables scoped type checking per package
- Challenge: No formal module system; prefix is convention only

**Integration opportunity:** Use prefix extraction to group functions into logical
modules for type checking scope. The type checker can treat `foo-*` functions as
belonging to a `foo` module even without explicit declaration.

### Private Functions (Double Dash)

Functions with double dashes are considered internal/private:

```elisp
(defun magit--refresh-buffer ...)    ; Private
(defun magit-refresh-buffer ...)     ; Public API
```

**Type checking implications:**
- Private functions may have weaker type guarantees
- API stability expectations differ: public APIs should have stable types
- Private functions often have narrower, more specific types

**Integration opportunity:**
- Require explicit signatures for public (single-dash) functions
- Allow inference-only for private (double-dash) functions
- Generate warnings for type-breaking changes to public APIs
- Consider stricter checking for public API boundaries

### Type Predicates (`*-p`, `*p`)

Predicates return non-nil for truthy, nil for falsy:

```elisp
(defun stringp (object) ...)         ; Built-in, no hyphen
(defun string-or-null-p (object) ...)  ; Hyphenated predicate
(defun org-at-heading-p () ...)      ; Package predicate
```

Two conventions exist:
- Built-ins: no hyphen (`stringp`, `listp`, `numberp`)
- User-defined: hyphenated (`buffer-live-p`, `string-empty-p`)

**Type checking implications:**
- Predicates are natural type guards for occurrence typing
- Return type is effectively `(or t nil)` (boolean-like)
- Predicate name encodes the type being tested
- Can derive type narrowing from predicate calls

**Integration opportunity:**
- Recognise `*-p` functions as type guards automatically
- Map predicates to types: `stringp` -> `string`, `listp` -> `list`
- Support occurrence typing after predicate checks:
  ```elisp
  (when (stringp x)
    ;; x is known to be string here
    (upcase x))
  ```
- Generate type predicates for user-defined ADTs

### Setters (`setf`, `setq-*`)

Mutation patterns in Emacs Lisp:

```elisp
;; setq for variables
(setq buffer-read-only t)

;; setf for generalised places (uses gv-define-setter)
(setf (point) 100)
(setf (buffer-name) "new-name")
(setf (alist-get key alist) value)

;; Custom setters following setq-* convention
(setq-local indent-tabs-mode nil)
(setq-default fill-column 80)
```

**Type checking implications:**
- `setf` places need getter/setter type consistency
- Generalised variables (`gv.el`) create implicit setter functions
- `setq-local` and `setq-default` interact with buffer-local variables
- Challenge: `setf` expansion happens at macro time

**Integration opportunity:**
- Track `gv-define-setter` declarations for place types
- Ensure setter type matches getter return type
- Special handling for `setq-local` (buffer-local semantics)
- Consider `setf` as mutation effect for future effect tracking

## 2. Module System

### require/provide

The only formal module mechanism:

```elisp
;; In foo.el
(require 'cl-lib)        ; Load dependency
(require 'seq)

;; ... code ...

(provide 'foo)           ; Declare this file provides 'foo
```

**Semantics:**
- `require` loads file if not already loaded
- `provide` registers feature in `features` list
- No namespace isolation; all definitions are global
- Circular requires cause errors

**Type checking implications:**
- `require` establishes dependency graph for type checking order
- No private exports; everything is visible
- Challenge: Feature name may not match filename
- Challenge: Optional requires with `(require 'foo nil t)`

**Integration opportunity:**
- Parse `require` forms to build dependency order
- Type check packages in topological order
- Load `.tart` signature files in same order as `require`
- Handle optional requires as conditional type availability

### Feature Loading

Loading variations:

```elisp
(require 'foo)                    ; Error if missing
(require 'foo nil t)              ; Return nil if missing
(require 'foo nil 'noerror)       ; Same as above

(load "foo")                      ; Load file by name
(load "foo" t)                    ; No error if missing

(eval-after-load 'foo ...)        ; Defer until foo loads
(with-eval-after-load 'foo ...)   ; Modern version
```

**Type checking implications:**
- Optional requires create conditional type availability
- `eval-after-load` code may reference types not yet available
- Dynamic loading complicates static analysis
- Challenge: Runtime feature detection patterns

**Integration opportunity:**
- Track optional requires separately from mandatory
- For optional requires, signatures exist but may be unavailable
- `with-eval-after-load` blocks can assume feature types are available
- Consider "feature-dependent types" for conditional API usage

### Autoload Cookies

Autoloads defer loading until first use:

```elisp
;;;###autoload
(defun org-agenda ()
  "Show the agenda."
  ...)

;;;###autoload
(defvar org-agenda-files nil
  "Files to search for agenda items.")
```

Generated into `-autoloads.el` files:

```elisp
(autoload 'org-agenda "org" "Show the agenda." t)
```

**Type checking implications:**
- Autoloaded functions exist before their file is loaded
- Type must be available from signature without loading implementation
- Autoload entries provide minimal metadata (name, file, docstring, interactive)
- Challenge: Type info not in autoload; need separate mechanism

**Integration opportunity:**
- `.tart` files serve as type-level autoloads
- Type checker reads `.tart` without loading `.el`
- Autoloaded functions must have signatures in `.tart`
- Consider generating type-aware autoloads

### Package.el Conventions

Modern Emacs package structure:

```
foo/
  foo.el              ; Main file, must have foo-version
  foo-autoloads.el    ; Generated
  foo-pkg.el          ; Package metadata (optional)
```

Package header conventions:

```elisp
;;; foo.el --- Short description  -*- lexical-binding: t -*-

;; Author: Name <email>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (dash "2.19"))
;; Keywords: tools
;; URL: https://example.com/foo

;;; Commentary:
;; Long description...

;;; Code:
...
(provide 'foo)
;;; foo.el ends here
```

**Type checking implications:**
- `Package-Requires` declares version dependencies
- Emacs version affects available built-in types
- Dependency versions may affect type signatures
- `lexical-binding: t` is essential for sound type checking

**Integration opportunity:**
- Parse `Package-Requires` for dependency graph
- Conditional types based on Emacs version
- Require `lexical-binding: t` for type checking
- `.tart` file sits alongside `.el` in package structure

## 3. Common Libraries

### cl-lib Functions

Common Lisp compatibility layer:

```elisp
(require 'cl-lib)

;; Sequence operations
(cl-remove-if #'null list)
(cl-find-if #'predicate sequence)
(cl-position item sequence)
(cl-reduce #'+ numbers)

;; Structure definitions
(cl-defstruct point x y)

;; Type predicates
(cl-typep value 'integer)

;; Loop macro
(cl-loop for x in list
         when (pred x)
         collect (transform x))

;; Multiple values
(cl-multiple-value-bind (q r) (cl-floor 10 3)
  (list q r))

;; Destructuring
(cl-destructuring-bind (a b &rest c) list
  ...)
```

**Type checking implications:**
- Higher-order functions need polymorphic types
- `cl-defstruct` generates typed accessors and predicates
- `cl-typep` is explicit type check (occurrence typing opportunity)
- `cl-loop` is complex macro with typed collection
- Multiple values need tuple-like typing

**Integration opportunity:**
- Provide built-in signatures for all `cl-lib` functions
- Parse `cl-defstruct` to generate types automatically:
  ```
  (cl-defstruct point x y)
  ;; Generates:
  ;;   point-p : any -> bool
  ;;   point-x : point -> any
  ;;   point-y : point -> any
  ;;   make-point : &key x y -> point
  ```
- Support `cl-typep` as type guard
- Special handling for `cl-loop` typing

### seq.el Sequence Functions

Generic sequence operations (Emacs 25+):

```elisp
(require 'seq)

(seq-map #'upcase strings)
(seq-filter #'stringp mixed)
(seq-reduce #'+ numbers 0)
(seq-find #'pred sequence)
(seq-empty-p sequence)
(seq-length sequence)
(seq-concatenate 'list seq1 seq2)
(seq-into sequence 'vector)
```

**Type checking implications:**
- Works on lists, vectors, and strings uniformly
- Polymorphic over sequence type
- Result type sometimes depends on input type
- `seq-into` converts between sequence types

**Integration opportunity:**
- Define `Sequence` as type class/protocol
- Polymorphic signatures with sequence constraints:
  ```
  seq-map : forall a b s. Sequence s => (a -> b) -> s a -> s b
  ```
- `seq-into` as type conversion function
- Track concrete sequence types through operations

### map.el Map Functions

Generic map operations (Emacs 25+):

```elisp
(require 'map)

(map-elt map key)
(map-put! map key value)
(map-delete map key)
(map-keys map)
(map-values map)
(map-pairs map)
(map-contains-key map key)
```

Works on alists, plists, and hash-tables.

**Type checking implications:**
- Unifies three different map representations
- Key and value types vary by map kind
- `map-put!` is mutating; `map-put` returns new map
- Challenge: Plist keys are symbols; alist keys are any

**Integration opportunity:**
- Define `Map k v` type class
- Different instances for alist, plist, hash-table
- Track key/value types through map operations
- Distinguish mutable (`!`) vs. immutable operations

### subr-x.el Extensions

Common utility extensions:

```elisp
(require 'subr-x)

;; Threading macros
(thread-first x
  (foo a)
  (bar b))

(thread-last x
  (foo a)
  (bar b))

;; Binding forms
(if-let ((x (get-value))
         (y (process x)))
    (use x y)
  (fallback))

(when-let ((x (get-value)))
  (use x))

(and-let* ((x (get-value))
           (y (process x)))
  (use x y))

;; String utilities
(string-empty-p str)
(string-join strings separator)
(string-trim string)
```

**Type checking implications:**
- Threading macros transform nested calls
- `if-let`/`when-let` bind and nil-check simultaneously
- Binding success implies non-nil (type narrowing)
- Challenge: Macro expansion needed for accurate typing

**Integration opportunity:**
- Threading macros: derive intermediate types through chain
- `if-let`: narrow bound variable to non-nil in success branch
- `when-let`: similar narrowing
- `and-let*`: sequential narrowing through bindings
- These are excellent occurrence typing targets

## 4. Testing Conventions

### ERT Testing Framework

The standard testing framework:

```elisp
(require 'ert)

(ert-deftest my-package-test-foo ()
  "Test that foo works correctly."
  (should (equal (my-foo 1 2) 3))
  (should-not (my-foo nil nil))
  (should-error (my-foo "bad" "input") :type 'wrong-type-argument))

;; Test fixtures
(ert-deftest my-package-test-with-temp-buffer ()
  (with-temp-buffer
    (insert "test")
    (should (equal (buffer-string) "test"))))
```

**Type checking implications:**
- Tests may use functions with intentionally wrong types
- `should-error` explicitly tests type errors
- Test functions don't need return type checking
- Tests validate runtime behaviour, not static types

**Integration opportunity:**
- Skip type checking for `ert-deftest` bodies
- Or use permissive mode allowing type violations
- `should` and `should-not` could provide occurrence typing context
- Type check test helper functions normally

### Test Naming Patterns

```elisp
;; Pattern: package-name-test-description
(ert-deftest org-test-parse-headline ...)
(ert-deftest magit-status-test ...)

;; Tests in separate file
;; foo.el -> foo-test.el or test-foo.el
```

**Type checking implications:**
- Test files have different requirements than source
- Test files require the source package
- Naming convention identifies test code

**Integration opportunity:**
- Detect test files by `-test.el` or `test-*.el` pattern
- Apply relaxed type checking to test files
- Don't require signatures for test helper functions
- Consider separate `.tart` for test utilities if needed

## 5. Documentation Conventions

### Docstring Format

Standard docstring structure:

```elisp
(defun my-function (arg1 arg2 &optional arg3)
  "Short description on first line.

ARG1 is the first argument, a string.
ARG2 is the second argument, a number.
Optional ARG3 is a list of symbols.

Return the processed result as a string."
  ...)
```

Conventions:
- First line: brief description, fits in 67 characters
- Arguments: ALLCAPS in prose
- Optional/rest: explicitly noted
- Return value: documented last

**Type checking implications:**
- Docstrings contain informal type information
- "a string", "a number", "a list of" patterns
- No standardised type syntax
- Challenge: Natural language is ambiguous

**Integration opportunity:**
- Parse docstrings to suggest initial types
- Cross-validate docstring types with inferred types
- Generate type-annotated docstrings from signatures
- Emit warnings for docstring/signature mismatches

### Type Hints in Docstrings

No formal convention, but common patterns:

```elisp
(defun foo (x)
  "Process X (a string) and return a number."
  ...)

(defun bar (items)
  "Process ITEMS, a list of strings."
  ...)
```

**Type checking implications:**
- Useful for initial type extraction
- Inconsistent across codebase
- Often incomplete or outdated

**Integration opportunity:**
- Regex patterns to extract type hints:
  - "a STRING" -> string
  - "list of STRINGS" -> (list string)
  - "STRING or nil" -> (or string nil)
- Use as hints for type inference
- Update docstrings when signatures change

### Declare Forms

Compiler declarations:

```elisp
(defun my-function (x)
  "Description."
  (declare (indent 1)
           (pure t)
           (side-effect-free t)
           (completion (lambda ...))
           (advertised-calling-convention (x) "28.1"))
  ...)

;; In cl-lib
(cl-defun my-function (arg)
  (declare (cl-type (string) string))  ; Unofficial type hint
  ...)
```

Standard declares:
- `indent`: Indentation hint
- `pure`: No side effects, depends only on args
- `side-effect-free`: No side effects (but may depend on global state)
- `advertised-calling-convention`: Deprecation hint

**Type checking implications:**
- `pure` and `side-effect-free` are effect annotations
- `advertised-calling-convention` affects expected arity
- No standard type declaration form
- `cl-type` is non-standard but appears in some code

**Integration opportunity:**
- Respect `pure` for optimisation/inference
- `side-effect-free` informs effect tracking
- Could define new `declare` form for types:
  ```elisp
  (declare (type (string -> string)))
  ```
- Parse existing declares for semantic information

## 6. Customization System

### defcustom Patterns

User customization variables:

```elisp
(defcustom my-option "default"
  "Description of option."
  :type 'string
  :group 'my-package
  :package-version '(my-package . "1.0"))

(defcustom my-flag nil
  "Whether to enable feature."
  :type 'boolean
  :group 'my-package)
```

**Type checking implications:**
- `:type` specifies expected type
- Custom UI validates user input against type
- Runtime type is not enforced after validation
- Variable may have invalid type if set programmatically

**Integration opportunity:**
- Parse `:type` specifications for variable types
- `defcustom` variables have known types
- Validate setq of defcustom variables
- Bridge between customization types and tart types

### :type Specifications (Existing Type System)

The customize type system is expressive:

```elisp
;; Simple types
:type 'string
:type 'integer
:type 'boolean
:type 'symbol
:type 'function

;; Compound types
:type '(repeat string)                    ; list of strings
:type '(choice string integer)            ; union
:type '(cons string integer)              ; pair
:type '(list string integer symbol)       ; fixed-length list
:type '(alist :key-type string :value-type integer)

;; Restricted types
:type '(integer 0 100)                    ; range
:type '(string :tag "Name")               ; tagged
:type '(restricted-sexp :match-alternatives (stringp numberp))

;; Function types
:type '(function :args (string) :returns integer)

;; Complex example
:type '(choice
        (const nil)
        (repeat
         (choice string
                 (cons string string))))
```

**Type checking implications:**
- Rich type language already exists in Emacs
- Users familiar with this syntax
- Not used for static checking, only customize UI
- Challenge: Some features are UI-specific (`:tag`)

**Integration opportunity:**
- Translate customize types to tart types:
  - `string` -> `String`
  - `(repeat string)` -> `(List String)`
  - `(choice string nil)` -> `(Option String)`
  - `(cons a b)` -> `(Tuple a b)`
- Automatic type extraction from defcustom
- Consider using customize syntax for signature files
- Validate compatibility: all customize types expressible in tart

### defgroup Organization

Grouping customizations:

```elisp
(defgroup my-package nil
  "My package customization."
  :group 'tools
  :prefix "my-package-"
  :link '(url-link "https://example.com"))

(defgroup my-package-advanced nil
  "Advanced options."
  :group 'my-package)
```

**Type checking implications:**
- Groups establish logical boundaries
- `:prefix` reinforces naming convention
- Group hierarchy mirrors package structure

**Integration opportunity:**
- Use group membership for scoping
- Group prefix aids module inference
- Consider group-level type exports

## 7. Error Handling Conventions

### user-error vs error

Two primary error functions:

```elisp
;; User errors: expected conditions, user can fix
(user-error "No file selected")
(user-error "Buffer %s is read-only" (buffer-name))

;; Programming errors: bugs, unexpected conditions
(error "Invalid state: %s" state)
(error "Implementation error in %s" function-name)
```

Hierarchy:
- `user-error` is subtype of `error`
- `user-error` suppresses backtrace by default
- Signals different condition symbols

**Type checking implications:**
- Both are non-returning (bottom type)
- `user-error` is semantically "expected failure"
- Error messages often include type information
- Challenge: Error conditions are not typed

**Integration opportunity:**
- `error`/`user-error` calls have return type `Never`
- Code after error call is unreachable
- Use for exhaustiveness checking:
  ```elisp
  (pcase x
    ('a ...)
    ('b ...)
    (_ (error "Unexpected: %s" x)))  ; Proves exhaustive
  ```
- Parse format strings for type hints in error messages

### Condition Symbols

Structured error handling:

```elisp
;; Define condition
(define-error 'my-error "My error" 'error)
(define-error 'my-specific-error "Specific" 'my-error)

;; Signal condition
(signal 'my-error (list "data" more-data))

;; Handle condition
(condition-case err
    (risky-operation)
  (my-error
   (message "Caught: %s" (error-message-string err)))
  (error
   (message "Generic error")))

;; Ignore specific errors
(ignore-errors (risky-operation))
(with-demoted-errors "Warning: %s" (risky-operation))
```

**Type checking implications:**
- Condition hierarchy is a form of exception typing
- `signal` payload is untyped list
- `condition-case` binds error value
- Challenge: No typed exception system

**Integration opportunity:**
- Track condition types through `define-error`
- Type the error data payload
- `condition-case` handlers have error type in scope
- Consider typed conditions for v2:
  ```
  define-error my-error : { message: String, code: Int }
  ```

### Error Message Conventions

Standard formats:

```elisp
;; Context: what, where, why
(error "Cannot find %s in %s" name directory)
(user-error "File %s is not readable" filename)

;; Type errors follow Emacs convention
(signal 'wrong-type-argument (list 'stringp actual))
(signal 'wrong-number-of-arguments (list 'my-func 3 2))
```

Built-in type error symbols:
- `wrong-type-argument`: (predicate actual-value)
- `wrong-number-of-arguments`: (function expected actual)
- `void-function`: (symbol)
- `void-variable`: (symbol)

**Type checking implications:**
- Built-in errors encode type information
- Predicate in `wrong-type-argument` names expected type
- tart type errors should follow this convention

**Integration opportunity:**
- Generate type errors using Emacs conventions:
  ```
  Type error: expected String, got Integer
  -> signal wrong-type-argument (stringp actual)
  ```
- Use familiar error format for adoption
- Consider Emacs-style error messages in LSP diagnostics

## Summary: Type Checker Integration Priorities

### High Value, Low Effort
1. **Naming conventions**: Use prefixes for module scoping
2. **defcustom :type**: Parse for variable types (existing type system)
3. **cl-defstruct**: Generate types from struct definitions
4. **Type predicates**: Map `*-p` to type guards automatically
5. **Require/provide**: Build dependency graph

### High Value, Medium Effort
1. **Occurrence typing**: Leverage predicates and `if-let`/`when-let`
2. **seq.el/map.el**: Provide polymorphic built-in signatures
3. **Docstring parsing**: Extract type hints as suggestions
4. **Error unreachability**: Use error calls for exhaustiveness

### Lower Priority (v2+)
1. **cl-loop typing**: Complex macro analysis
2. **Typed conditions**: Exception types
3. **Effect tracking**: `pure`/`side-effect-free` semantics
4. **Test file handling**: Relaxed checking mode

### Risks
1. **No formal module system**: Prefix convention is not enforced
2. **Dynamic loading**: `eval-after-load` complicates static analysis
3. **Customize type UI features**: Not all `:type` specs map cleanly
4. **Macro-heavy code**: `cl-loop`, threading macros need expansion
