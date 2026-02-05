# Spec 48: Prelude

Implicit utility types built on compiler intrinsics.

**Deps:** None (foundational). **Consumers:** Spec 07 (sig loading), Spec 11
(unions), Spec 24 (versioned typings), Spec 32 (core typings workflow).

## Goal

Define the prelude: a small, fixed set of type aliases that bridge compiler
intrinsics to ergonomic everyday types. The prelude loads implicitly before any
other `.tart` file.

## Rationale

Tart's type system has primitive types built into the compiler (intrinsics).
Users shouldn't need to manually construct common patterns like "a value that
might be nil" or "a non-empty list" from intrinsics. The prelude provides these
building blocks so all other typings can use them.

## Constraints

| Constraint        | Detail                                          |
| ----------------- | ----------------------------------------------- |
| Minimal           | Only types derivable from intrinsics            |
| Implicit          | Loaded automatically; no `(open 'tart-prelude)` |
| Not overridable   | Users cannot shadow or replace prelude types    |
| No functions      | Prelude contains only `(type ...)` declarations |
| Version-agnostic  | Same prelude across all Emacs versions          |

## Compiler Intrinsics

These types are built into the type system itself, defined in `builtin_types.ml`:

| Type      | Description                                      |
| --------- | ------------------------------------------------ |
| `truthy`  | Top type for non-nil values                      |
| `nil`     | The nil type (singleton)                         |
| `never`   | Bottom type (no inhabitants)                     |
| `int`     | Integers                                         |
| `float`   | Floating-point numbers                           |
| `num`     | Supertype of `int` and `float`                   |
| `string`  | Strings                                          |
| `symbol`  | Symbols                                          |
| `keyword` | Keywords (`:foo`)                                |
| `cons`    | Cons cells                                       |
| `\|`      | Union type operator                              |
| `-`       | Type subtraction operator                        |

Literal types (`1`, `1.0`, `'foo`, `:kw`) are also intrinsics, as subtypes of
their corresponding base types.

**Key invariant:** `truthy` and `nil` do not unify. This is the foundation for
nullable/non-nullable type safety.

## Prelude Types

The prelude defines these types in terms of intrinsics:

```lisp
;; typings/tart-prelude.tart

;; The symbol 't—Elisp's canonical truthy value
(type t 't)

;; Universal type: any Elisp value
(type any (truthy | nil))

;; Boolean: t or nil (Elisp's boolean convention)
(type bool (t | nil))

;; Homogeneous list (recursive, nullable)
(type list [a] ((cons a (list a)) | nil))

;; Refinement: remove nil from a type
(type is [a] (a - nil))

;; Optional: add nil to a truthy type
(type option [(a : truthy)] (a | nil))

;; Non-empty list (list without nil)
(type nonempty [a] (is (list a)))
```

### Type Relationships

```
Subtyping:
  (nonempty a) <: (list a) <: any
  (nonempty a) <: truthy
  (is (a | nil)) <: a (when a <: truthy)

Equivalences:
  (list a) = ((cons a (list a)) | nil)
  (nonempty a) = (cons a (list a))
  (option a) = (a | nil) (when a <: truthy)
  bool = (t | nil)
```

### Discipline: When to Use `any`

The `any` type is a prelude alias for `(truthy | nil)`—not a primitive. The
actual top types are `truthy` and `nil`; `any` is merely a convenience for
"accepts any Elisp value."

Every use of `any` must be justified—treat it with absolute suspicion.

**Legitimate uses (input positions):**

```lisp
;; Predicates that accept any value
(defun null (any) -> bool)
(defun stringp (any) -> bool)
(defun type-of (any) -> symbol)
```

**Illegitimate uses (output positions):**

```lisp
;; WRONG: funcall's return type depends on the function argument
(defun funcall (any &rest any) -> any)

;; WRONG: length accepts specific sequence types, not any
(defun length (any) -> int)
```

**Correct alternatives:**

- For `funcall`/`apply`: special type-checker handling extracts return type
  from the function argument (see Spec 34)
- For `length`: use precise union of accepted types:
  `((list any) | string | (vector any) | bool-vector | char-table) -> int`
- For dynamic lookups like `symbol-value`: `any` may be legitimate since the
  stored value is truly dynamic at runtime

**Rule of thumb:** If you can enumerate the possible types, use a union. Only
use `any` when the type is genuinely unknowable at compile time AND the value
is in input position for a predicate.

## Output

```
typings/
└── tart-prelude.tart    ; Prelude type definitions
lib/sig/
└── sig_loader.ml        ; (modify) Load prelude before other typings
```

## Requirements

### R1: Prelude file location

**Given** the tart installation
**When** looking for the prelude
**Then** it exists at `typings/tart-prelude.tart`

**Verify:** File exists; parseable as valid `.tart` syntax

### R2: Implicit loading

**Given** any `.tart` file being loaded
**When** the signature loader processes it
**Then** prelude types are already available without explicit `(open ...)`

```lisp
;; In any .tart file, these just work:
(defun head [a] ((list a)) -> (option a))
(defun safe-head [a] ((nonempty a)) -> a)
```

**Verify:** `dune test`; prelude types usable without import

### R3: Load order

**Given** the signature loading process
**When** typings are loaded
**Then** they load in this sequence:

| file                                       | defines                                    |
| ------------------------------------------ | ------------------------------------------ |
| `typings/tart-prelude.tart`                | userspace names for compiler intrinsics    |
| `typings/emacs/{version}/c-core/*.tart`    | data structures, functions, variables, etc |
| `typings/emacs/{version}/lisp-core/*.tart` | functions, variables, etc                  |

After these, user typings (via `tart-type-path`) and project-local sibling
`.tart` files load.

**Verify:** Prelude types available in all subsequent signature files

### R4: Not user-overridable

Prelude types are subject to the general no-shadowing rule (Spec 07 R17):
redefining any imported binding—including prelude types—is an error.

```elisp
(type list int)  ; Error: cannot redefine imported binding 'list'
```

**Verify:** `dune test`; redefining `list`, `option`, etc. produces error

### R5: Type alias semantics

**Given** prelude types are aliases
**When** used in type expressions
**Then** they expand to their definitions during type checking

```lisp
(list int) expands to ((cons int (list int)) | nil)
(option string) expands to (string | nil)
(nonempty int) expands to (cons int (list int))
```

**Verify:** Type errors show expanded form when helpful

### R6: Bounded quantifier on `option`

**Given** `(type option [(a : truthy)] (a | nil))`
**When** instantiating `option` with a nullable type
**Then** error: bound violation

```lisp
(option int)              ; OK: int <: truthy
(option (int | nil))      ; Error: (int | nil) not <: truthy
(option (option string))  ; Error: (string | nil) not <: truthy
```

**Verify:** `dune test`; nested option is a type error

## Non-Goals

- **Emacs primitives:** Functions like `car`, `cdr`, `cons` are defined in
  versioned typings (Spec 24, Spec 32), not the prelude
- **Third-party types:** Types for dash.el, s.el, etc. are separate typings
- **User extensibility:** Users cannot add to the prelude; they create their own
  `.tart` files

## Tasks

- [x] [R1] Create `typings/tart-prelude.tart` with type definitions
- [x] [R2,R3] Modify sig_loader.ml to load prelude first
- [x] [R4] Covered by Spec 07 R17 (no-shadowing rule)
- [x] [R5] Ensure type aliases expand correctly
- [x] [R6] Validate bounded quantifier on `option`

**Status:** Fully implemented. Prelude at `typings/tart-prelude.tart` (104 lines) with management via `lib/sig/prelude.mli`.
