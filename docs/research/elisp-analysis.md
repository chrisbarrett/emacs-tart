# Emacs Lisp Analysis: Typing Challenges and v1 Recommendations

This document synthesizes the Spec 02 research phase, documenting Elisp
language features with typing implications and providing tractability
assessments and v1 scope recommendations.

## Executive Summary

Emacs Lisp presents a mixed picture for static typing:

- **Core language features** (binding, control flow, functions) are tractable
- **Data structures** require union types and (ideally) row polymorphism
- **Macro patterns** range from tractable to intractable
- **Global state** (point, match-data, buffers) is largely intractable
- **Existing type systems** (`cl-defstruct`, `defcustom`) provide integration
  opportunities

**Recommended v1 scope**: Focus on tractable features, require annotations for
hard features, exclude intractable patterns.

---

## 1. Special Forms Analysis

### Tractability Summary

| Category | Forms | Tractability |
|----------|-------|--------------|
| Definition | `defun`, `defconst` | Tractable |
| Definition | `defvar`, `defcustom`, `defmacro` | Hard/Intractable |
| Binding | `let`, `let*`, `letrec`, `lambda` | Tractable |
| Control | `if`, `cond`, `when`, `unless` | Tractable |
| Control | `and`, `or` | Tractable* |
| Sequencing | `progn`, `prog1`, `prog2` | Tractable |
| Error | `unwind-protect` | Tractable |
| Error | `condition-case` | Hard |
| Save/Restore | `save-excursion`, etc. | Tractable |
| Mutation | `setq` | Tractable* |
| Control | `catch`/`throw` | Hard |

\* With occurrence typing, these become harder but still tractable.

### v1 Recommendations

**Include**:

1. All binding forms with standard let-polymorphism
2. Control flow with basic union types
3. Sequencing forms (type of last expression)
4. `defun` with function type inference
5. `defconst` with type inference from value

**Require annotations**:

1. `defvar` - dynamic scope requires explicit type
2. `defcustom` - extract hints from `:type` but allow override

**Exclude**:

1. `defmacro` - expand before type checking
2. `catch`/`throw` - dynamic tag matching intractable

---

## 2. Calling Conventions Analysis

### Tractability Summary

| Feature | Tractability | Notes |
|---------|--------------|-------|
| Direct function calls | Tractable | Standard typing |
| `funcall` | Hard | Dynamic dispatch |
| `apply` | Hard | Spread arguments |
| Higher-order functions | Tractable | `mapcar`, etc. |
| Advice system | Intractable | Modifies function types |
| Hooks | Hard | Lists of functions |
| Interactive specs | Tractable | Ignore for typing |
| Autoloading | Hard | Type info may be unavailable |

### `funcall` and Dynamic Dispatch

```elisp
(funcall f x)  ; f must be (-> X Y)
```

When `f` is a known symbol, typing is straightforward. When `f` is computed
(e.g., `(funcall (intern "foo") x)`), typing is impossible.

**Recommendation**: Type `funcall` when function is statically known; require
annotation otherwise.

### `&optional` and `&rest` Arguments

```elisp
(defun foo (a &optional b &rest c) ...)
;; Type: (-> A (Optional B) (List C) R)
```

Model optional arguments with `Optional` type (union with nil). Model rest
arguments as `List`.

### v1 Recommendations

**Include**:

1. Direct calls with known function types
2. `funcall` with statically known function
3. Higher-order functions (`mapcar`, `seq-*`, etc.)
4. `&optional` as `Optional` type
5. `&rest` as `List` type

**Require annotations**:

1. Hook variables (type as `(List (-> ...)`)
2. Autoloaded functions (signature in `.eli`)

**Exclude**:

1. Advice system - types may change at runtime
2. Dynamic `funcall`/`apply` with computed function

---

## 3. Data Structures Analysis

### Tractability Summary

| Structure | Basic Typing | Advanced Features | Priority |
|-----------|--------------|-------------------|----------|
| Lists | `(List a)` | Heterogeneous, nil | High |
| Vectors | `(Vector a)` | - | Medium |
| Strings | `String` | - | High |
| Hash tables | `(HashTable k v)` | nil-as-missing | Medium |
| Alists | `(Alist k v)` | Row types | High |
| Plists | `(Plist k v)` | Row types, kwargs | High |
| Symbols | `Symbol`, `Keyword` | Literal types | High |
| `cl-defstruct` | Record types | Inheritance | Critical |
| EIEIO | Class types | Multi-dispatch | Medium |
| Buffers | `Buffer` | Context, modes | Low |

### The nil Problem

`nil` serves multiple purposes:

1. Boolean false
2. Empty list `()`
3. Missing value (from `gethash`, etc.)

**Recommendation**: Use contextual typing. In boolean context, `nil` is `Bool`.
In list context, `nil` is `(List a)`. For missing values, use `(Option a)`.

### Lists: Homogeneous vs Heterogeneous

Elisp lists are often heterogeneous:

```elisp
'(name "Alice" age 30)  ; mixed types
```

**Recommendation for v1**:

- Default to homogeneous `(List a)`
- Provide `(Tuple a b c)` for fixed-length heterogeneous sequences
- Use `(Union a b)` for variable-length mixed lists
- Defer row-typed plists to v2

### `cl-defstruct` Integration

This is the highest-value integration opportunity:

```elisp
(cl-defstruct person
  (name nil :type string)
  (age 0 :type integer))
```

Extract slot types and generate:

```
Person : Type
make-person : (-> String Int Person)
person-name : (-> Person String)
person-age : (-> Person Int)
person-p : (-> Any Bool)
```

### v1 Recommendations

**Include**:

1. `(List a)` - homogeneous lists
2. `(Vector a)` - homogeneous vectors
3. `String` - primitive type
4. `(Pair a b)` - cons cells
5. `(Option a)` - nullable types
6. `(Tuple a b c)` - fixed heterogeneous sequences
7. `(Union a b)` - for common mixed patterns
8. `cl-defstruct` - full support with slot types

**Partially support**:

1. `(HashTable k v)` - with `Option` for `gethash`
2. `(Alist k v)` - as `(List (Pair k v))`
3. EIEIO classes - as opaque nominal types

**Defer**:

1. Row types for plists
2. Buffer-local variables
3. Mode-specific buffer types

---

## 4. Global State Analysis

### Tractability Summary

| State | Tractability | Recommendation |
|-------|--------------|----------------|
| Point, mark | Intractable | Ignore for v1 |
| Match data | Intractable | Ignore for v1 |
| Current buffer | Hard | Treat as context |
| Dynamic variables | Hard | Require annotations |
| Buffer-local variables | Intractable | Treat as regular |
| Mode state | Intractable | Ignore |

### The Buffer Context Problem

Many Elisp functions implicitly operate on the "current buffer":

```elisp
(point)           ; returns position in current buffer
(insert "text")   ; inserts in current buffer
(buffer-string)   ; returns content of current buffer
```

**Options**:

1. **Ignore** - treat all buffer operations as context-independent
2. **Effect types** - `(Buffer!) => Int` (complex)
3. **Explicit context** - require `with-current-buffer` (breaks idiom)

**Recommendation for v1**: Ignore buffer context. Accept unsoundness. Buffer
operations work on "some buffer"; type checker doesn't track which.

### Dynamic Variables

```elisp
(defvar my-var nil "A dynamic variable.")
(let ((my-var 42))
  (call-function))  ; my-var is 42 inside call-function
```

Dynamic variables can be rebound anywhere, making flow-sensitive typing
difficult.

**Recommendation**: Require type annotations for dynamic variables in `.eli`
files. Treat as having that fixed type.

### v1 Recommendations

**Accept unsoundness for**:

1. Point, mark state
2. Match data
3. Buffer-local distinctions
4. Mode-specific operations

**Require annotations for**:

1. Dynamic variables (`defvar`)

**Model as context-independent**:

1. `with-current-buffer` - as higher-order function
2. `save-excursion` - transparent wrapper

---

## 5. Macro Patterns Analysis

### Tractability Categories

| Category | Examples | Tractability |
|----------|----------|--------------|
| Wrapper | `with-temp-buffer`, `save-excursion` | High |
| Binding | `when-let`, `if-let`, `pcase-let` | High |
| Definition | `define-minor-mode`, `cl-defstruct` | Medium-High |
| Control flow | `pcase`, `cl-case`, `cl-typecase` | Medium |
| Code generation | Dynamic `intern`, `eval` | Low |
| Quasiquote | Data templates vs code gen | Variable |

### Wrapper Macros

These are the easiest to type:

```elisp
(with-temp-buffer BODY...)  ; type = type of last form in BODY
(save-excursion BODY...)    ; type = type of last form in BODY
```

**Recommendation**: Implement as built-in special forms with polymorphic
signatures.

### Binding Macros

`when-let`, `if-let` enable occurrence typing:

```elisp
(when-let ((x (get-value)))
  (use-x x))  ; x is non-nil here
```

**Recommendation**: Support with nil-narrowing occurrence typing.

### Definition Macros

`define-minor-mode`, `define-derived-mode` generate predictable names:

```elisp
(define-minor-mode foo-mode ...)
;; Generates: foo-mode, foo-mode-hook, foo-mode-map
```

**Recommendation**: Built-in knowledge of generated names and types.

### Control Flow Macros

`pcase` is the most complex:

```elisp
(pcase x
  ((pred stringp) (length x))  ; x : String
  ((pred integerp) (1+ x))     ; x : Int
  (_ nil))
```

**Recommendation for v1**: Support basic `pcase` patterns (literals, symbols,
cons, vectors). Defer custom patterns (`pcase-defmacro`) to v2.

### v1 Recommendations

**Include**:

1. All standard `with-*` and `save-*` wrappers
2. `when-let`, `if-let`, `if-let*`, `when-let*`
3. Basic `pcase` patterns
4. `cl-case`, `cl-typecase`
5. `dolist`, `dotimes`
6. `define-minor-mode` (generated names)
7. `cl-defstruct` (full support)

**Partially support**:

1. `cl-loop` (common patterns only)
2. EIEIO `defclass` (slots, simple inheritance)

**Exclude**:

1. Custom `pcase` patterns
2. Dynamic code generation
3. String-based symbol construction

---

## 6. Ecosystem Conventions Analysis

### Naming Conventions

| Convention | Example | Typing Implication |
|------------|---------|-------------------|
| `package-name-*` | `my-pkg-function` | Namespace, no type impact |
| Private `--` | `my-pkg--internal` | Could restrict visibility |
| Predicates `*p` | `stringp`, `my-thing-p` | Returns `Bool` |
| Type predicates | `listp`, `bufferp` | Returns `Bool` with refinement |

**Recommendation**: Use predicate naming convention to identify type guards for
occurrence typing.

### Module System

Elisp's `require`/`provide` is weak:

- No private exports
- No type information attached
- Autoloads defer loading

**Recommendation**: Use package boundaries as type boundaries. Typed packages
provide `.eli` files; untyped packages require `require/typed` declarations.

### Common Libraries

| Library | Typing Approach |
|---------|-----------------|
| `cl-lib` | Provide built-in signatures |
| `seq.el` | Polymorphic sequence functions |
| `map.el` | Polymorphic map functions |
| `subr-x.el` | Binding macros, special forms |

**Recommendation**: Ship signatures for standard libraries.

### Testing Conventions

```elisp
(ert-deftest my-test ()
  (should (= 2 (add 1 1))))
```

**Recommendation**: Type `ert` test forms; `should` returns `Bool`.

---

## 7. Existing Type Systems Analysis

### Integration Opportunities

| System | Type Information | Integration Value |
|--------|------------------|-------------------|
| `defcustom :type` | Customize types | Medium |
| `cl-defstruct` | Slot types | High |
| EIEIO `defclass` | Slot types | Medium |
| `declare` forms | Purity hints | Low |
| `cl-the` | Type assertions | Low |
| Edebug specs | Macro structure | Low |

### `defcustom` Mapping

```elisp
(defcustom my-var nil
  "Doc."
  :type '(choice (const nil) string))
```

Maps to: `(Option String)`

| Customize Type | tart Type |
|----------------|-----------|
| `string` | `String` |
| `integer` | `Int` |
| `boolean` | `Bool` |
| `symbol` | `Symbol` |
| `(choice a b)` | `(Or A B)` |
| `(repeat a)` | `(List A)` |
| `sexp` | Requires annotation |

### `cl-defstruct` Integration

Parse `cl-defstruct` forms and extract:

1. Struct name → nominal type
2. Slot names and `:type` → field types
3. `:include` → subtype relationship
4. Generated accessors → function types

### v1 Recommendations

**Integrate**:

1. `cl-defstruct` - auto-generate signatures
2. `defcustom :type` - import as type hints

**Defer**:

1. EIEIO full OO - subtyping complications
2. `cl-the` - rare in practice
3. Edebug specs - useful for v2 macro typing

---

## 8. Tractability Assessment by Feature

### v1 Scope (Tractable)

| Feature | Status | Notes |
|---------|--------|-------|
| Function types | Include | `(-> (A B) C)` (non-curried) |
| Let-polymorphism | Include | Generalization at let |
| Basic union types | Include | `(Or A B)` |
| Option types | Include | `(Option A)` where `A : Truthy` |
| Lists | Include | `(List A)` |
| Tuples | Include | `(Tuple A B C)` |
| Structs | Include | Via `cl-defstruct` |
| Basic occurrence typing | Include | `if`/`cond` with predicates |
| Wrapper macros | Include | Built-in knowledge |
| Binding macros | Include | `when-let`, etc. |
| Advice definitions | Include | Type-check with known signatures |
| Escape hatch | Include | `(tart-ignore EXPR)` → `Any` |

### v2 Scope (Hard)

| Feature | Status | Notes |
|---------|--------|-------|
| Row polymorphism | Defer | For plists, keyword args |
| Higher-rank types | Defer | Bidirectional checking |
| Full `pcase` | Defer | Custom patterns |
| `cl-loop` typing | Defer | Complex DSL |
| Runtime contracts | Defer | Optional soundness |
| EIEIO methods | Defer | Multi-dispatch |

### Out of Scope (Intractable)

| Feature | Status | Notes |
|---------|--------|-------|
| Dynamic code gen | Exclude | `intern`, `eval` |
| Buffer-local tracking | Exclude | Needs effect system |
| Point/mark effects | Exclude | Pervasive state |
| Macro definitions | Exclude | Expand first |

Note: Advice system moved to v1 — advice definitions can be type-checked with
known signatures (same approach as hooks). Use `(tart-ignore EXPR)` as escape
hatch for expressions that cannot be typed.

---

## 9. Recommended Type System Primitives

### Base Types

```
Int      ; integer
Float    ; floating-point
Num      ; Int | Float
String   ; string
Symbol   ; symbol
Keyword  ; keyword (:foo)
Nil      ; unit type, sole inhabitant: nil
T        ; unit type, sole inhabitant: t
Truthy   ; primitive: anything that is not Nil
Bool     ; T | Nil
Any      ; Truthy | Nil (top type)
Never    ; bottom type (errors)
```

### Type Constructors

```
(-> (A) B)         ; function from A to B (non-curried)
(-> (A B) C)       ; two args to C
(-> (A) (-> (B) C)); manual currying: returns function
(List A)           ; homogeneous list
(Vector A)         ; homogeneous vector
(Pair A B)         ; cons cell
(Tuple A B C)      ; fixed-length heterogeneous
(Option A)         ; A | Nil (A must be Truthy)
(Or A B)           ; union
(HashTable K V)    ; hash table
(Alist K V)        ; (List (Pair K V))
```

### User-Defined Types

```
;; Via cl-defstruct
(type Point = { x : Num, y : Num })

;; Sum types (ADTs)
(type Result a e = (Ok a) | (Err e))

;; Type aliases
(type IntList = (List Int))
```

---

## 10. v1 Implementation Priorities

### Must Have

1. **HM inference** for typed modules
2. **Function types** with `&optional`/`&rest`
3. **Let-polymorphism** with value restriction
4. **Basic union/option types**
5. **`cl-defstruct` integration**
6. **Wrapper macro support** (built-in knowledge)
7. **`.eli` signature files**
8. **LSP diagnostics and hover**

### Should Have

1. **Occurrence typing** for `if`/`cond` with predicates
2. **Binding macro support** (`when-let`, `if-let`)
3. **`defcustom` type hints** extraction
4. **Basic `pcase` patterns**
5. **Standard library signatures** (subset)

### Could Have

1. **`define-minor-mode` knowledge**
2. **EIEIO class types** (opaque)
3. **Inlay hints** in LSP
4. **Code actions** for type annotations

### Won't Have (v1)

1. Row polymorphism
2. Runtime contracts
3. Effect types
4. Buffer context tracking
5. Custom `pcase` patterns
6. Full `cl-loop` typing

Note: Advice definitions are now v1 scope (type-checked with known signatures).

---

## 11. Open Questions

### Design Decisions Needed

1. **nil representation**: Contextual typing vs explicit coercion?
2. **Optional arguments**: `(Optional A)` vs separate type form?
3. **Keyword arguments**: Tuple-based vs row-based?
4. **Subtyping**: Structural vs nominal for structs?
5. **Error handling**: Model `signal`/`error` as `Never`?

### Questions Requiring Prototyping

1. **Macro expansion API**: How reliable is `macroexpand-all`?
2. **Incremental performance**: How fast can we type-check on change?
3. **Error message quality**: Can we map back to source locations?
4. **Real-world coverage**: What fraction of MELPA packages can we type?

---

## 12. Conclusion

Emacs Lisp is typeable within significant constraints:

- **Tractable core**: binding, control flow, data structures, functions
- **Hard boundaries**: dynamic scope, global state, macros
- **Intractable features**: advice, buffer-local, dynamic codegen

The recommended approach:

1. Type what we can type soundly
2. Require annotations at boundaries
3. Accept unsoundness for global state
4. Expand macros before type checking
5. Provide immediate value through LSP integration

Proceed to Spec 03: Type System Design.
