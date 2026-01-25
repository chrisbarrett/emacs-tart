# Emacs Lisp Macro Patterns: Type Checking Tractability Analysis

This document categorizes Emacs Lisp macro patterns by their tractability for
static type analysis in the context of the tart type system.

## Overview

Macros present a fundamental challenge for type checking: they operate at the
syntactic level before type checking occurs. Unlike functions, which have
stable signatures, macros can produce arbitrary code transformations that may
introduce new bindings, change control flow, or generate entirely new
constructs.

The key insight for tractability is whether a macro's expansion can be
predicted and typed without evaluating the macro itself. We categorize macros
into six patterns with decreasing tractability.

---

## 1. Wrapper Macros

**Tractability: High (tractable for v1)**

Wrapper macros establish a dynamic context, execute a body, and return the
body's result. They have predictable type behavior: the return type equals the
type of the final body expression.

### Common Examples

```elisp
;; Buffer context
(with-temp-buffer BODY...)           ; -> type of last form in BODY
(with-current-buffer BUFFER BODY...) ; -> type of last form in BODY
(save-current-buffer BODY...)        ; -> type of last form in BODY

;; Point/mark context
(save-excursion BODY...)             ; -> type of last form in BODY
(save-restriction BODY...)           ; -> type of last form in BODY
(save-match-data BODY...)            ; -> type of last form in BODY

;; Window context
(save-window-excursion BODY...)      ; -> type of last form in BODY
(with-selected-window WINDOW BODY...)

;; File context
(with-temp-file FILE BODY...)        ; -> type of last form in BODY

;; Output context
(with-output-to-string BODY...)      ; -> String (always)

;; Locking/mutex
(with-mutex MUTEX BODY...)           ; -> type of last form in BODY
```

### Type Signature Pattern

```
with-wrapper : forall a. (...setup-args) -> (... -> a) -> a
```

The type checker can treat these as: given a body of type `a`, the wrapper
returns type `a`. Setup arguments may have specific type requirements but
don't affect the return type.

### Special Cases

- `with-output-to-string`: Always returns `String` regardless of body type
- `with-temp-buffer`: Body executes in a different buffer; point-dependent
  operations apply to the temp buffer
- `condition-case`: Return type is union of body type and handler types

### What Would Break Type Checking

- Non-local exits from body (these affect control flow, not types)
- Wrappers that transform the body's return value (rare)

### Recommended v1 Approach

Treat as built-in special forms with signatures like:
```
(with-temp-buffer : (-> a) -> a)
(with-output-to-string : (-> a) -> String)
```

---

## 2. Binding Macros

**Tractability: High (tractable for v1)**

Binding macros introduce new variable bindings with specific scoping rules.
They're tractable because the binding structure is syntactically apparent.

### Common Examples

```elisp
;; Conditional binding
(when-let ((VAR EXPR)) BODY...)      ; VAR bound if EXPR non-nil
(when-let* ((V1 E1) (V2 E2)) BODY...)
(if-let ((VAR EXPR)) THEN ELSE)
(if-let* ((V1 E1) (V2 E2)) THEN ELSE)

;; Pattern binding
(pcase-let ((PATTERN EXPR)) BODY...) ; binds pattern variables
(pcase-let* ...)

;; Destructuring (cl-lib)
(cl-destructuring-bind (A B . REST) LIST BODY...)
(cl-multiple-value-bind (V1 V2) FORM BODY...)

;; Augmented let
(let-alist ALIST BODY...)            ; binds .key accessors
```

### Type Inference Implications

For `when-let` and `if-let`:
- The bound variable has the type of EXPR with `nil` removed (narrowing)
- In ELSE branch (or after the form), variable may be nil or unbound

For `pcase-let`:
- Pattern matching provides type refinement
- Each pattern arm narrows the type of matched expressions
- Bindings have types derived from pattern structure

For `cl-destructuring-bind`:
- List structure provides type information
- Can express as: `(List a b) -> a` for extracting first element
- Rest patterns (`&rest`, `. rest`) bind to list types

### What Would Break Type Checking

- Dynamic patterns (patterns computed at runtime)
- Patterns that depend on custom `pcase` macro extensions
- `let-alist` with unknown alist shape (requires alist type inference)

### Recommended v1 Approach

1. Support `when-let`, `if-let` with occurrence typing (nil narrowing)
2. Support basic `pcase-let` patterns (literals, symbols, cons, vectors)
3. Defer complex `pcase` patterns (guards, custom patterns) to v2
4. Support `cl-destructuring-bind` for known list structures

---

## 3. Definition Macros

**Tractability: Medium-High (tractable with signature conventions)**

Definition macros generate top-level definitions following predictable
patterns. They're tractable when the generated names and types follow
conventions.

### Common Examples

```elisp
;; Mode definitions
(define-minor-mode foo-mode ...)
;; Generates: foo-mode (command), foo-mode (variable), foo-mode-hook,
;;            foo-mode-map, foo-mode-lighter

(define-derived-mode foo-mode parent-mode "Foo" ...)
;; Generates: foo-mode (command), foo-mode-syntax-table, foo-mode-abbrev-table

(define-generic-mode ...)
(define-compilation-mode ...)

;; Variable definitions with types
(defcustom VAR VALUE DOC :type TYPE ...)  ; has explicit :type
(defvar VAR VALUE DOC)                     ; no type info
(defconst VAR VALUE DOC)                   ; immutable

;; Structure definitions
(cl-defstruct NAME SLOTS...)               ; typed slots possible
(defclass NAME PARENTS SLOTS... OPTIONS)   ; EIEIO

;; Other definition forms
(define-error NAME MESSAGE PARENT)
(define-advice FN (WHERE PROPS) BODY...)
```

### Type Generation Patterns

For `define-minor-mode foo-mode`:
```
foo-mode      : (-> (Optional Int) Unit)  ; command
foo-mode      : Bool                       ; variable (when called from elisp)
foo-mode-hook : Hook                       ; specifically (List (-> Unit))
foo-mode-map  : Keymap
```

For `cl-defstruct`:
```elisp
(cl-defstruct point x y)
;; Generates:
make-point    : (-> Number Number Point)
point-x       : (-> Point Number)
point-y       : (-> Point Number)
point-p       : (-> Any Bool)
copy-point    : (-> Point Point)
```

### What Would Break Type Checking

- Custom `:constructor` names in cl-defstruct
- Dynamic slot definitions
- Generated names that don't follow conventions
- EIEIO with complex inheritance (method dispatch)

### Recommended v1 Approach

1. Built-in knowledge of `define-minor-mode`, `define-derived-mode`
2. Full support for `cl-defstruct` including slot types from `:type`
3. Basic `defclass` support (slots, simple inheritance)
4. Extract `defcustom` `:type` specs as type information source
5. Treat generated names as following the standard patterns

---

## 4. Control Flow Macros

**Tractability: Medium (requires pattern analysis)**

Control flow macros implement branching, looping, or pattern matching with
varying complexity. Tractability depends on how dynamic the control flow is.

### Common Examples

```elisp
;; Pattern matching
(pcase EXPR
  (PATTERN1 BODY1...)
  (PATTERN2 BODY2...)
  (_ DEFAULT...))

(pcase-exhaustive EXPR CLAUSES...)  ; must be exhaustive

(cl-case EXPR
  (KEY1 BODY1...)
  ((KEY2 KEY3) BODY2...)
  (otherwise DEFAULT...))

(cl-typecase EXPR
  (STRING BODY1...)
  (INTEGER BODY2...)
  (t DEFAULT...))

(cl-ecase EXPR CLAUSES...)  ; error if no match
(cl-etypecase EXPR CLAUSES...)

;; Iteration
(cl-loop ...)                ; extremely complex DSL
(cl-do ((VAR INIT STEP)...) (END-TEST RESULT) BODY...)
(dolist (VAR LIST) BODY...)
(dotimes (VAR COUNT) BODY...)
(while TEST BODY...)         ; actually a special form
```

### Type Analysis for Pattern Matching

For `pcase`:
- Return type is union of all arm result types
- Each pattern narrows the type within that arm
- `_` (underscore) is wildcard, matches anything
- Exhaustiveness checking possible with ADT types

For `cl-typecase`:
- Directly expresses type-based dispatch
- Natural fit for occurrence typing
- Return type is union of arm result types

### Type Analysis for Loops

For `cl-loop`:
- Has its own complex sublanguage
- Accumulation clauses determine return type:
  - `collect` -> List
  - `append` -> List
  - `count` -> Integer
  - `sum` -> Number
  - `maximize/minimize` -> Number (or nil)
- Without accumulation, returns nil

For `dolist`, `dotimes`:
- Body type is ignored
- Return type comes from optional RESULT form (or nil)

### What Would Break Type Checking

- `cl-loop` with computed iteration specs
- Custom `pcase` pattern macros (`pcase-defmacro`)
- Non-exhaustive pattern matching (need runtime fallback type)
- Loop variables mutated to different types

### Recommended v1 Approach

1. Support `pcase` with built-in patterns only
2. Support `cl-typecase` as type-directed dispatch
3. Basic `cl-loop` support (common patterns: `for`, `collect`, `do`)
4. Full support for `dolist`, `dotimes`
5. Flag non-exhaustive patterns as warnings

---

## 5. Code Generation Macros

**Tractability: Low (requires macro expansion or special handling)**

Code generation macros produce code that can't be predicted from syntax alone.
They often use eval, intern symbols dynamically, or construct code from
templates.

### Common Examples

```elisp
;; Dynamic function/variable generation
(defmacro define-foo-commands (names)
  `(progn
     ,@(mapcar (lambda (name)
                 `(defun ,(intern (format "foo-%s" name)) ()
                    ...))
               names)))

;; String-based symbol construction
(intern (concat prefix "-mode"))
(funcall (intern (concat pkg "-" func)))

;; Computed macro calls
(macroexpand `(some-macro ,computed-arg))

;; Advice with dynamic targets
(advice-add (intern name) :around #'wrapper)
```

### Analysis Challenges

1. **Dynamic symbol generation**: `(intern (concat ...))` produces symbols
   whose names aren't statically known

2. **Computed macro arguments**: When macro arguments are runtime values,
   expansion can't be predicted

3. **Template-based generation**: Macros that generate multiple definitions
   based on input lists

4. **Self-modifying patterns**: Code that modifies its own definitions

### What Would Break Type Checking

- Essentially everything about these patterns breaks type checking
- Symbol identity unknown at compile time
- Generated code structure varies based on runtime values

### Recommended v1 Approach

1. **Don't attempt to type these directly**
2. Require explicit type signatures for generated definitions
3. Treat `intern`/`make-symbol` results as `Symbol` (opaque)
4. Provide escape hatch: `(declare (type ...))`annotation
5. Consider macro expansion as pre-processing step (complex, deferred)

---

## 6. Quasiquote Patterns

**Tractability: Variable (depends on usage)**

Quasiquotation (backquote) is used both for simple templating and for complex
code generation. Tractability varies widely.

### Usage Categories

#### A. Data construction (tractable)

```elisp
;; Building static data structures
`(a b c)                    ; -> (List Symbol)
`(1 ,x 3)                   ; -> (List (Union Integer (typeof x)))
`((key . ,value))           ; -> (List (Cons Symbol (typeof value)))
```

#### B. Simple code templates (mostly tractable)

```elisp
;; Macro that wraps body
(defmacro my-with-foo (var &rest body)
  `(let ((,var (make-foo)))
     ,@body))
```

The expansion is predictable: `let` with known structure.

#### C. Complex code generation (intractable)

```elisp
;; Dynamic structure
(defmacro define-accessors (struct slots)
  `(progn
     ,@(mapcar (lambda (slot)
                 `(defun ,(intern (format "%s-%s" struct slot)) (obj)
                    (slot-value obj ',slot)))
               slots)))
```

### Hygiene Considerations

Emacs Lisp macros are unhygienic:
- Captured bindings can leak into expansion
- Generated symbols may conflict with user code
- `gensym` / `make-symbol` used for hygiene

For type checking:
- Unhygienic capture can change types unexpectedly
- Fresh symbols (gensym) are harder to track
- No guarantee of referential transparency

### What Would Break Type Checking

- Quasiquotes with computed splices (`,@(compute-list)`)
- Mixed-type lists constructed via quasiquote
- Unhygienic variable capture
- Nested quasiquotes with complex unquoting

### Recommended v1 Approach

1. Treat backquote as list/cons constructor when outside macros
2. For known macro patterns, expand and type the result
3. Flag complex quasiquote patterns as requiring annotation
4. Don't attempt to type macro definitions themselves in v1

---

## Summary: Tractability Ratings

| Category | Rating | v1 Recommendation |
|----------|--------|-------------------|
| Wrapper macros | High | Full support via built-in signatures |
| Binding macros | High | Support common forms, basic patterns |
| Definition macros | Medium-High | Support with convention knowledge |
| Control flow macros | Medium | Support common patterns, defer complex |
| Code generation macros | Low | Require explicit annotations |
| Quasiquote patterns | Variable | Support data construction only |

---

## v1 Scope Recommendations

### Include in v1

1. **Wrapper macros**: All standard `with-*` and `save-*` forms
   - Implement as special forms with polymorphic signatures
   - Handle `with-output-to-string` special case

2. **Binding macros**: `when-let`, `if-let`, `pcase-let` (basic patterns)
   - Implement occurrence typing for nil-checking
   - Support literal, symbol, cons, vector patterns

3. **Definition macros**: `define-minor-mode`, `cl-defstruct`
   - Generate type signatures for known generated names
   - Extract types from `cl-defstruct` slot declarations
   - Use `defcustom` `:type` as type source

4. **Control flow**: `pcase`, `cl-case`, `cl-typecase`
   - Union types for result
   - Type narrowing within pattern arms

### Defer to v2

1. Custom `pcase` patterns (`pcase-defmacro`)
2. Full `cl-loop` DSL analysis
3. EIEIO method dispatch typing
4. Macro expansion for type checking
5. User-defined macro typing declarations

### Out of Scope

1. Dynamic code generation (`intern`, `eval`)
2. String-based symbol construction
3. Runtime macro expansion
4. Self-modifying code

---

## Appendix: Common Macros Reference

### High Priority (must support in v1)

```elisp
;; Wrappers
with-temp-buffer with-current-buffer save-excursion
with-temp-file with-output-to-string condition-case

;; Binding
let let* when-let when-let* if-let if-let*
pcase-let pcase-let* cl-destructuring-bind

;; Definition
defun defmacro cl-defun cl-defmacro
defvar defconst defcustom
cl-defstruct define-minor-mode

;; Control
pcase cl-case cl-typecase
dolist dotimes while
```

### Medium Priority (v1 stretch / v2)

```elisp
;; Definition
define-derived-mode defclass define-generic-mode

;; Control
cl-loop cl-do cl-block cl-return

;; Binding
let-alist cl-multiple-value-bind
```

### Low Priority (v2 or later)

```elisp
;; Complex forms
define-advice advice-add
edebug-defun eval-when-compile
cl-labels cl-flet cl-macrolet
```
