# Existing Type-like Systems in Emacs Lisp

Analysis of type-related systems in Emacs Lisp for integration potential with
tart.

## Overview

Emacs Lisp has several systems that provide type-like information, though none
constitute a full static type system. These systems were designed for runtime
validation, documentation, or optimisation rather than compile-time type
checking. Understanding their semantics and limitations is essential for
determining integration strategies.

## 1. defcustom :type - The Customization Type System

### What It Is

The `defcustom` macro uses a `:type` specifier to describe valid values for user
customisation options. This system provides rich type descriptions primarily for
the Customize UI.

### Available Type Specifiers

| Specifier     | Meaning                                    | Example                                    |
| :------------ | :----------------------------------------- | :----------------------------------------- |
| `symbol`      | Any symbol                                 | `:type 'symbol`                            |
| `integer`     | Any integer                                | `:type 'integer`                           |
| `number`      | Integer or float                           | `:type 'number`                            |
| `string`      | Any string                                 | `:type 'string`                            |
| `regexp`      | String intended as regexp                  | `:type 'regexp`                            |
| `character`   | A character (integer in valid range)       | `:type 'character`                         |
| `file`        | File path string                           | `:type 'file`                              |
| `directory`   | Directory path string                      | `:type 'directory`                         |
| `hook`        | A hook (list of functions)                 | `:type 'hook`                              |
| `boolean`     | `t` or `nil`                               | `:type 'boolean`                           |
| `key-sequence`| Keyboard sequence                          | `:type 'key-sequence`                      |
| `coding-system`| A coding system symbol                    | `:type 'coding-system`                     |
| `color`       | Color string                               | `:type 'color`                             |
| `face`        | A face name                                | `:type 'face`                              |
| `function`    | Any function                               | `:type 'function`                          |
| `variable`    | A variable name                            | `:type 'variable`                          |
| `sexp`        | Any Lisp object                            | `:type 'sexp`                              |

### Composite Type Specifiers

```elisp
;; Choice between alternatives (sum type)
:type '(choice (const nil) string integer)

;; Cons cell with typed car and cdr
:type '(cons symbol string)

;; Homogeneous list
:type '(repeat string)

;; Fixed-length list with typed elements (product type)
:type '(list string integer symbol)

;; Association list
:type '(alist :key-type symbol :value-type string)

;; Property list
:type '(plist :key-type symbol :value-type sexp)

;; Restricted values
:type '(choice (const :tag "None" nil)
               (const :tag "All" t)
               (repeat string))

;; Restricted set of symbols
:type '(radio (const left)
              (const right)
              (const center))
```

### Type Information Available

- Structural description of valid values
- Tagged unions via `choice`
- Product types via `list` and `cons`
- Recursive types via `repeat`, `alist`, `plist`
- Constant values via `const`
- Semantic hints (`file`, `directory`, `regexp`, `face`, etc.)

### How to Extract It

```elisp
;; Get the :type specification
(get 'some-defcustom 'custom-type)

;; Or via customize internals
(custom-variable-type 'some-defcustom)
```

### Integration Strategy for tart

**Potential uses:**

1. **Initial type hints**: Use defcustom types as starting points for variable
   types in typed modules
2. **Type refinement**: Semantic types like `file`, `directory`, `face` could
   map to tart type aliases
3. **Validation**: Cross-check inferred types against declared custom types

**Mapping to tart types:**

| defcustom type     | tart type equivalent                      |
| :----------------- | :---------------------------------------- |
| `symbol`           | `Symbol`                                  |
| `integer`          | `Int`                                     |
| `number`           | `Num`                                     |
| `string`           | `String`                                  |
| `boolean`          | `Bool`                                    |
| `function`         | `(-> ... ...)`  (no arity info)           |
| `(choice a b)`     | `(Or A B)`                                |
| `(cons a b)`       | `(Pair A B)`                              |
| `(list a b c)`     | `(Tuple A B C)`                           |
| `(repeat a)`       | `(List A)`                                |
| `(alist ...)`      | `(List (Pair K V))`                       |
| `sexp`             | Top / requires explicit annotation        |

### Limitations

- **No function arity or argument types**: `function` type provides no signature
  information
- **Runtime-oriented**: Designed for validation in Customize UI, not static
  analysis
- **Incomplete coverage**: Only applies to user options, not arbitrary variables
- **No inference**: Types are manually declared, may drift from actual usage
- **Semantic types are opaque**: `file` is just a `string` structurally
- **No parametric polymorphism**: Cannot express `(repeat 'a)` generically

## 2. cl-defstruct - Common Lisp Structure Definitions

### What It Is

`cl-defstruct` defines record types with named slots, generating constructors,
accessors, predicates, and copiers. It supports optional slot type declarations.

### Slot Type Declarations

```elisp
(cl-defstruct person
  (name nil :type string)
  (age 0 :type integer)
  (email nil :type (or null string)))

;; Generates:
;; - Constructor: make-person, person-create (cl-defstruct specific)
;; - Accessors: person-name, person-age, person-email
;; - Predicate: person-p
;; - Copier: copy-person
```

### Type Information Available

- Slot names and their declared types
- Default values for each slot
- Read-only slots (`:read-only t`)
- Inheritance via `:include`
- Constructor name customisation
- Named vs unnamed (vector-backed) structs

### How to Extract It

```elisp
;; Get struct descriptor
(cl-struct-slot-info 'person)
;; Returns: ((cl-tag-slot) (name nil :type string) (age 0 :type integer) ...)

;; Get slot type specifically
(cl-struct-slot-value 'person 'name instance)

;; Introspect via cl--struct-class-* functions (internal)
(get 'person 'cl-struct-slots)  ; slot descriptors

;; For each slot, extract type:
(let ((slots (cl-struct-slot-info 'person)))
  (mapcar (lambda (slot)
            (cons (car slot)
                  (plist-get (cddr slot) :type)))
          (cdr slots)))  ; skip cl-tag-slot
```

### Integration Strategy for tart

**Extraction approach:**

1. Parse `cl-defstruct` forms during analysis
2. Extract slot types from `:type` declarations
3. Generate tart ADT definitions

**Mapping:**

```
;; cl-defstruct
(cl-defstruct point
  (x 0 :type number)
  (y 0 :type number))

;; tart equivalent (phantom ADT)
(type Point = { x: Num, y: Num })

;; Constructor type
make-point : (-> Num Num Point)

;; Accessor types
point-x : (-> Point Num)
point-y : (-> Point Num)

;; Predicate type
point-p : (-> Any Bool)
```

**Integration points:**

1. **Automatic signature generation**: Derive `.eli` signatures from struct
   definitions
2. **Pattern matching**: `pcase` patterns on structs should type-check
3. **Constructor inference**: Infer types from constructor calls

### Limitations

- **Types are hints only**: Emacs does not enforce slot types at runtime (except
  with native compilation, which may optimise based on them)
- **Limited type language**: Only supports CL-style type specifiers
- **No parametric types**: Cannot define `(cl-defstruct (box a) (value :type a))`
- **Inheritance complicates typing**: `:include` creates subtype relationships
  not easily expressed in System F
- **cl-type specifiers differ from defcustom**: Uses CL-style `(or null string)`
  rather than `(choice (const nil) string)`

### CL Type Specifier Mapping

| CL type specifier   | tart type                                |
| :------------------ | :--------------------------------------- |
| `t`                 | Top / Any                                |
| `nil`               | Bottom / Never                           |
| `null`              | `Nil` (unit)                             |
| `symbol`            | `Symbol`                                 |
| `integer`           | `Int`                                    |
| `float`             | `Float`                                  |
| `number`            | `Num`                                    |
| `string`            | `String`                                 |
| `list`              | `(List Any)`                             |
| `vector`            | `(Vector Any)`                           |
| `(or a b)`          | `(Or A B)`                               |
| `(and a b)`         | `(And A B)` (intersection)               |
| `(member x y z)`    | `(Or (Lit x) (Lit y) (Lit z))`           |
| `(satisfies pred)`  | Requires refinement types (out of scope) |

## 3. EIEIO - Enhanced Implementation of Emacs Interpreted Objects

### What It Is

EIEIO is Emacs's object system, inspired by CLOS (Common Lisp Object System). It
provides classes, inheritance, generic functions, and method dispatch.

### defclass Slot Types

```elisp
(defclass rectangle ()
  ((width :initarg :width
          :initform 0
          :type number
          :documentation "Width of rectangle")
   (height :initarg :height
           :initform 0
           :type number
           :documentation "Height of rectangle"))
  "A rectangle class.")

;; Inheritance
(defclass colored-rectangle (rectangle)
  ((color :initarg :color
          :initform "black"
          :type string))
  "A colored rectangle.")
```

### Method Signatures

```elisp
;; Generic function with type specifier on argument
(cl-defmethod area ((rect rectangle))
  "Calculate area of RECT."
  (* (oref rect width) (oref rect height)))

;; Method with multiple dispatch
(cl-defmethod combine ((r1 rectangle) (r2 rectangle))
  "Combine two rectangles.")

;; Method with primitive type specifier
(cl-defmethod scale ((rect rectangle) (factor number))
  "Scale RECT by FACTOR."
  ...)
```

### Type Information Available

- Class definitions with slot types
- Inheritance hierarchy
- Generic function signatures (dispatched argument types)
- Slot accessors (`oref`, `oset`)
- Constructor patterns (`:initarg`)

### How to Extract It

```elisp
;; Get class object
(eieio--class-object 'rectangle)

;; Get slots
(eieio-class-slots 'rectangle)

;; Get slot properties
(cl-loop for slot in (eieio-class-slots 'rectangle)
         collect (cons (eieio-slot-descriptor-name slot)
                       (eieio--slot-descriptor-type slot)))

;; Get parent classes
(eieio-class-parents 'colored-rectangle)

;; Check method existence
(cl-find-method 'area nil '(rectangle))
```

### Integration Strategy for tart

**Class to ADT mapping:**

```
;; EIEIO
(defclass point ()
  ((x :type number)
   (y :type number)))

;; tart (record type)
(type Point = { x: Num, y: Num })
```

**Method typing:**

```
;; EIEIO method
(cl-defmethod distance ((p1 point) (p2 point)) ...)

;; tart signature (challenge: dispatch semantics)
distance : (-> Point Point Num)
```

**Integration points:**

1. **Extract class hierarchies**: Build subtype relationships
2. **Type generic functions**: Use dispatched types as constraints
3. **Handle slot access**: `oref`/`oset` as typed accessors

### Limitations and Challenges

- **Subtype polymorphism vs parametric**: EIEIO uses OO subtyping; tart uses
  parametric polymorphism - these are fundamentally different
- **Runtime dispatch**: Method selection happens at runtime based on argument
  types; this is dynamic and hard to type statically
- **Multiple inheritance**: EIEIO supports multiple inheritance, which creates
  diamond problems for typing
- **Open world**: New methods can be added to existing generics anywhere,
  violating closed-world assumptions
- **Mixin patterns**: Common Elisp patterns mix in behaviours dynamically
- **slot-value is untyped**: Direct slot access via `slot-value` bypasses type
  information

**Recommendation:** Treat EIEIO classes as opaque types in v1. Generate
signatures for accessors and well-typed methods, but do not attempt full OO type
checking.

## 4. declare Forms

### What It Is

The `declare` form provides hints to the byte compiler and other tools about
function properties.

### Available Declarations

```elisp
(defun pure-add (a b)
  "Add A and B."
  (declare (pure t)
           (side-effect-free t))
  (+ a b))

(defmacro my-when (cond &rest body)
  "Execute BODY when COND is non-nil."
  (declare (indent 1)
           (debug (form body)))
  `(if ,cond (progn ,@body)))
```

### Declaration Types

| Declaration           | Meaning                                           |
| :-------------------- | :------------------------------------------------ |
| `(pure t)`            | Function result depends only on arguments         |
| `(side-effect-free t)`| Function has no side effects                      |
| `(side-effect-free error-free)` | Pure and cannot signal errors           |
| `(indent SPEC)`       | Indentation hint for editors                      |
| `(debug SPEC)`        | Edebug specification for macros                   |
| `(doc-string N)`      | Which argument is the docstring                   |
| `(advertised-calling-convention ARGS VERSION)` | Preferred call signature |
| `(obsolete NEW VERSION)`| Function is obsolete                            |
| `(compiler-macro FN)` | Use FN for compile-time expansion                 |
| `(gv-expander FN)`    | Generalized variable expander                     |
| `(gv-setter FN)`      | Generalized variable setter                       |

### Type Information Available

Currently minimal, but valuable metadata:

- **Purity information**: `pure` and `side-effect-free` indicate function
  behaviour
- **Arity hints**: `advertised-calling-convention` suggests expected arguments

### How to Extract It

```elisp
;; Check function properties
(function-get 'pure-add 'pure)
(function-get 'pure-add 'side-effect-free)

;; Get advertised calling convention
(function-get 'some-fn 'advertised-calling-convention)
```

### Potential for Type Declarations

The `declare` form could theoretically be extended for type declarations:

```elisp
;; Hypothetical extension
(defun typed-add (a b)
  (declare (type (-> Int Int Int)))  ; NOT REAL
  (+ a b))
```

However, this would require changes to Emacs core.

### Integration Strategy for tart

**Use purity information:**

1. Functions marked `pure` can be safely evaluated at compile time
2. `side-effect-free` functions can be reordered by optimisers
3. Track these properties in tart's function database for optimisation

**Advertised calling convention:**

1. Use as hint for expected arity
2. Cross-check against inferred types
3. Warn on mismatches

### Limitations

- **No actual type declarations**: `declare` does not support type annotations
- **Voluntary**: Declarations are hints, not enforced
- **Incomplete coverage**: Most functions lack declarations
- **Purity is approximation**: Functions may be marked pure incorrectly

## 5. Native Compilation Type Hints

### What It Is

Emacs 28+ native compilation (libgccjit) supports type hints that enable better
code generation. These are primarily for performance, not safety.

### comp-hint Functions

```elisp
;; Hint that expression evaluates to a fixnum
(comp-hint-fixnum (1+ x))

;; Hint that expression evaluates to a cons
(comp-hint-cons (car alist))
```

### cl-the and cl-locally

```elisp
;; CL-style type assertion
(cl-the integer (compute-value))

;; Block with local type declarations
(cl-locally
  (declare (type integer x y))
  (+ x y))
```

### Type Information Available

- Type assertions for expressions
- Local variable type declarations
- Primarily primitive types: `integer`, `fixnum`, `float`, `cons`, etc.

### How to Extract It

These hints are typically consumed during native compilation and not easily
introspectable after the fact. They exist in source form only.

```elisp
;; Would need to parse source and extract cl-the forms
;; No runtime introspection available
```

### Integration Strategy for tart

**Use as ground truth:**

1. When source contains `cl-the`, treat the declared type as an assertion
2. Verify inferred type is compatible with declared type
3. Report errors if assertion is violated

**Potential extension point:**

1. tart could emit `cl-the` assertions when compiling typed code
2. Native compiler would then optimise based on tart's analysis

### Limitations

- **Not widely used**: Most Elisp code lacks these hints
- **Limited type language**: Only basic types, no function types or generics
- **Unsafe**: Incorrect hints cause undefined behaviour in native code
- **Source-level only**: Cannot extract from compiled code

## 6. Edebug Type Specs

### What It Is

Edebug specifications describe the syntactic structure of macro invocations,
enabling the debugger to step through macro expansions correctly.

### Specification Syntax

```elisp
(defmacro my-let (bindings &rest body)
  "Custom let form."
  (declare (debug ((&rest (symbolp form)) body)))
  `(let ,bindings ,@body))

(defmacro my-dolist (spec &rest body)
  (declare (debug ((symbolp form &optional form) body)))
  `(dolist ,spec ,@body))
```

### Edebug Spec Elements

| Spec element    | Meaning                                              |
| :-------------- | :--------------------------------------------------- |
| `symbolp`       | A symbol                                             |
| `form`          | Any Lisp form                                        |
| `body`          | Remaining forms (implicit progn)                     |
| `sexp`          | Any s-expression                                     |
| `&optional`     | Following elements are optional                      |
| `&rest`         | Following element repeats                            |
| `&or`           | Alternatives                                         |
| `(spec ...)`    | Grouped elements                                     |
| `[spec ...]`    | Grouped elements treated as vector                   |
| `gate`          | Stop backtracking                                    |
| `def-form`      | Defining form (creates binding)                      |
| `def-body`      | Body with definitions                                |
| `lambda-list`   | Standard lambda argument list                        |

### Type Information Available

- Structural patterns of macro arguments
- Binding forms (which arguments introduce variables)
- Optional and rest arguments
- Alternative syntaxes

### How to Extract It

```elisp
;; Get edebug spec for a macro
(get 'my-let 'edebug-form-spec)

;; Or via function property
(function-get 'my-let 'edebug-form-spec)
```

### Integration Strategy for tart

**Use for macro typing:**

1. Edebug specs describe macro syntax, which helps type-check macro invocations
2. `symbolp` indicates a binding position - the variable introduced has an
   inferred type
3. `form` positions are expressions to type-check
4. `body` indicates a sequence of forms

**Example mapping:**

```
;; Edebug spec
(debug ((&rest (symbolp form)) body))

;; Interpretation for tart:
;; - First arg is list of (symbol, form) pairs
;; - symbol in each pair is bound to result of form
;; - body is checked with those bindings in scope
```

**Limitations:**

- **Syntax, not semantics**: Specs describe structure, not type relationships
- **No return types**: Does not indicate what the macro expands to
- **Incomplete coverage**: Many macros lack specs
- **Compile-time only**: Relevant for macro expansion, not runtime

## Summary: Integration Recommendations for tart v1

### High Value, Tractable

| System              | Integration                                           | Effort |
| :------------------ | :---------------------------------------------------- | :----- |
| cl-defstruct        | Auto-generate ADT types and accessor signatures       | Medium |
| defcustom :type     | Import as type hints for customisation variables      | Low    |
| declare (purity)    | Track pure/side-effect-free for optimisation          | Low    |
| cl-the              | Treat as type assertions, verify compatibility        | Low    |

### Medium Value, Moderate Difficulty

| System              | Integration                                           | Effort |
| :------------------ | :---------------------------------------------------- | :----- |
| Edebug specs        | Use for macro argument typing                         | Medium |
| EIEIO (classes)     | Treat as opaque nominal types                         | Medium |
| EIEIO (methods)     | Type first-dispatch argument, ignore others           | Medium |

### Low Value or High Difficulty

| System              | Issue                                                 |
| :------------------ | :---------------------------------------------------- |
| EIEIO (full OO)     | Subtyping incompatible with System F                  |
| Native comp hints   | Rare in practice, unsafe semantics                    |
| declare (extend)    | Would require Emacs core changes                      |

### Type Mapping Summary

```
;; defcustom/cl type -> tart type

string          -> String
integer         -> Int
number          -> Num
float           -> Float
symbol          -> Symbol
boolean         -> Bool
null            -> Nil
t (CL)          -> Any
nil (CL)        -> Never

(or a b)        -> (Or A B)
(choice a b)    -> (Or A B)
(cons a b)      -> (Pair A B)
(list a b c)    -> (Tuple A B C)
(repeat a)      -> (List A)
function        -> (-> Any ... Any)  ; unknown arity

;; Struct/class -> ADT
(cl-defstruct point (x :type number) (y :type number))
    -> (type Point = { x: Num, y: Num })
```

### Open Questions

1. **How to handle sexp/t/Any**: These escape the type system. Options:
   - Reject and require annotation
   - Introduce `Any` with restricted operations
   - Treat as type variable requiring instantiation

2. **EIEIO subtyping**: Should tart model class hierarchies?
   - Option A: Ignore inheritance, treat each class as distinct
   - Option B: Generate subtype constraints (adds complexity)
   - Recommendation: Option A for v1

3. **Validation timing**: Should tart validate that runtime values match
   declared defcustom types?
   - This is properly a runtime concern
   - tart could generate assertions if desired

4. **Source of truth**: When both defcustom :type and tart .eli declare types,
   which wins?
   - Recommendation: .eli is authoritative; warn on conflicts with defcustom

[gfm-spec]: https://github.github.com/gfm/
