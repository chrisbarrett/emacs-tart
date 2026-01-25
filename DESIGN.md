# Tart Design Document

Static type checker for Emacs Lisp. OCaml implementation with LSP delivery.

## Architecture

```
.el file → Parser → S-exp AST → Macro Expansion → Core AST → Type Inference → Diagnostics
                                      ↑
                              Pure OCaml interpreter
                              (no Emacs subprocess)
```

```
lib/
├── syntax/     S-expression parser with source locations
├── interp/     Pure Elisp interpreter for macro expansion
├── core/       Type representation and environments
└── typing/     Constraint-based HM inference
```

## Type System Reference

### Type Syntax

(see: [`Types.typ`](lib/core/types.ml#L36), [`Types.param`](lib/core/types.ml#L49))

```
τ ::= α                        Type variable       (TVar)
    | C                        Type constant       (TCon)
    | F τ₁ ... τₙ              Type application    (TApp)
    | (π₁ ... πₙ) → τ          Function type       (TArrow)
    | [α₁...αₙ] τ              Universal type      (TForall)
    | τ₁ ∪ τ₂ ∪ ...            Union type          (TUnion)
    | τ₁ × τ₂ × ...            Tuple type          (TTuple)

π ::= τ                        Positional          (PPositional)
    | τ?                       Optional            (POptional)
    | τ*                       Rest element        (PRest)
    | :k τ                     Keyword             (PKey)
```

**Quantifier syntax:** `[α₁...αₙ]` binds type variables. Scope extends to end of
enclosing s-expression.

**Quantifier inference:** Lowercase identifiers in type position are inferred as
universally quantified, ordered by left-to-right first occurrence:

```elisp
;; Implicit: inferred as [a b]
(defun seq-map ((a -> b) (Seq a)) -> (List b))

;; Explicit: only listed vars bound; unlisted = error
(defun foo [a] (a -> b) -> a)  ; Error: unbound type variable 'b'
```

Explicit quantification required for phantom types (var in quantifier but not body).

### Primitive Types

(see: [`Types.Prim`](lib/core/types.ml#L103))

| Type      | Description                    | Truthy? |
| --------- | ------------------------------ | ------- |
| `Int`     | Integer values                 | ✓       |
| `Float`   | Floating-point numbers         | ✓       |
| `Num`     | Numeric (no subtyping yet)     | ✓       |
| `String`  | String values                  | ✓       |
| `Symbol`  | Elisp symbols                  | ✓       |
| `Keyword` | Elisp keywords (`:foo`)        | ✓       |
| `Nil`     | The only falsy value           | ✗       |
| `T`       | The `t` constant               | ✓       |
| `Truthy`  | Anything except `nil`          | ✓       |
| `Bool`    | `T \| Nil`                     | ✗       |
| `Any`     | `Truthy \| Nil` (top type)     | ✗       |
| `Never`   | Uninhabited (bottom type)      | ✓       |

### Literal Types

Singleton types representing specific values:

| Syntax      | Description                    | Supertype |
| ----------- | ------------------------------ | --------- |
| `42`        | Integer literal                | `Int`     |
| `"hello"`   | String literal                 | `String`  |
| `'foo`      | Symbol literal                 | `Symbol`  |
| `:bar`      | Keyword literal                | `Keyword` |

Literal types are useful for:
- Discriminated unions: `(Or :success :failure)`
- Exact return values: `(defun version -> "1.0")`
- Keyword arguments with specific values

### Type Constructors

(see: [`Types.list_of`](lib/core/types.ml#L119), [`Types.option_of`](lib/core/types.ml#L121), [`Types.vector_of`](lib/core/types.ml#L120), [`Types.pair_of`](lib/core/types.ml#L122), [`Types.hash_table_of`](lib/core/types.ml#L123))

| Constructor    | Syntax               | Truthy? | Notes                          |
| -------------- | -------------------- | ------- | ------------------------------ |
| `List`         | `(List a)`           | ✓       | Homogeneous list               |
| `Vector`       | `(Vector a)`         | ✓       | Homogeneous vector             |
| `Option`       | `(Option a)`         | ✗       | Requires `a : Truthy`          |
| `Pair`         | `(Pair a b)`         | ✓       | Heterogeneous cons cell        |
| `HashTable`    | `(HashTable k v)`    | ✓       | Mutable hash table             |
| `Or` (Union)   | `(Or a b ...)`       | *       | Truthy iff all members truthy  |
| `Tuple`        | `(Tuple a b ...)`    | ✓       | Fixed-length heterogeneous     |
| Arrow          | `(params) -> ret`    | ✓       | Function type (value)          |
| Arrow+Forall   | `[a] (params) -> ret`| ✓       | Polymorphic function (value)   |

### Truthiness

A type is **truthy** if it cannot be `nil`. This is fundamental to Elisp semantics.

```
         ⊤ (Any)
        /   \
    Truthy  Nil
      |
      T
```

**Truthiness predicate:** (see: [`Types.is_truthy`](lib/core/types.ml#L229))

```
truthy(C)           = C ∉ {Nil, Bool, Any}
truthy(F τ̄)         = F ≠ Option
truthy((π̄) → τ)     = true
truthy(τ₁ × ... × τₙ) = true
truthy(∀ᾱ. τ)       = truthy(τ)
truthy(τ₁ ∪ ... ∪ τₙ) = ∀i. truthy(τᵢ)
truthy(α)           = false  (conservative for unresolved)
```

**Option well-formedness:** (see: [`Types.option_of_checked`](lib/core/types.ml#L283))

Option τ requires truthy(τ) to ensure some/none distinguishable:

```elisp
(Option String)          ; Valid
(Option (List Int))      ; Valid
(Option Nil)             ; INVALID - can't distinguish Some nil from None
(Option Bool)            ; INVALID - Bool includes Nil
(Option (Option a))      ; INVALID - nested Option
```

### Function Types

Parameters are grouped (not curried), matching Elisp semantics. The `->` is infix,
separating parameters from return type:

```elisp
Int -> Int                             ; Single param, no parens needed
(Int Int) -> Int                       ; Multiple params need parens
(String &optional Int) -> String       ; With optional
(&rest Int) -> Int                     ; Variadic
(&key :name String :age Int) -> Nil    ; Keyword args
Int -> (Int -> Int)                    ; Returns function value
```

Nested function types are readable:

```elisp
(a -> b)                               ; Function taking a, returning b
((a -> b) (List a)) -> (List b)        ; First param is a function
```

Parameter unification rules:
- τ₁ = τ₂ (positional with positional)
- τ₁? = τ₂? (optional with optional)
- τ* consumes remaining positional/optional params
- :k τ₁ = :k τ₂ (keyword names must match)

### Function Slot vs Value Slot (Lisp-2)

Elisp separates functions and values into different namespaces:

```elisp
(defun foo (x) x)           ; Function slot: (foo 1) works
(setq bar (lambda (x) x))   ; Value slot: (bar 1) errors, need (funcall bar 1)
```

The type system tracks this distinction via declaration forms:

| Declaration | Calling convention | Example |
|-------------|-------------------|---------|
| `defun`     | Direct: `(f args...)` | `(defun add (Int Int) -> Int)` |
| `defvar` with `->` | Indirect: `(funcall f args...)` | `(defvar handler (String -> Nil))` |

In `defun`, quantifiers go right after the name:

```elisp
(defun identity [a] a -> a)          ; callable as (identity x)
(defun add (Int Int) -> Int)         ; monomorphic, no quantifiers needed
```

For explicit arrow types (in `defvar` or as parameters), quantifiers go at the start:

```elisp
(defvar id-fn ([a] a -> a))          ; polymorphic function value
(defvar handler (String -> Nil))     ; monomorphic function value
```

Higher-order functions receive **values**, so their parameters are arrow types:

```elisp
(defun map [a b] ((a -> b) (List a)) -> (List b))
;; The first parameter is a function value; inside map, use (funcall f elem)
```

When a function returns a function, the result is a value:

```elisp
(defun make-adder Int -> (Int -> Int))
;; (make-adder 1) returns a value; caller must (funcall (make-adder 1) 2)
```

### Surface Syntax (Elisp)

The `.tart` signature file syntax maps to the type theory notation:

| Surface                  | Type Theory    | Example                           |
| ------------------------ | -------------- | --------------------------------- |
| `Int`, `String`, ...     | C              | Primitive constants               |
| `a`, `b`, ...            | α              | Type variables                    |
| `τ -> τ`                 | τ → τ          | `Int -> Int`                      |
| `(τ...) -> τ`            | (π̄) → τ        | `(Int Int) -> Int`                |
| `[α...] τ -> τ`          | [ᾱ] τ → τ      | `[a] a -> a`                      |
| `(List τ)`               | List τ         | `(List Int)`                      |
| `(Option τ)`             | Option τ       | `(Option String)`                 |
| `(Or τ₁ τ₂ ...)`         | τ₁ ∪ τ₂ ∪ ...  | `(Or Int String)`                 |
| `(Tuple τ₁ τ₂ ...)`      | τ₁ × τ₂ × ...  | `(Tuple Int String Bool)`         |
| `&optional τ`            | τ?             | Optional parameter                |
| `&rest τ`                | τ*             | Rest parameter (element type)     |
| `&key :k τ`              | :k τ           | Keyword parameter                 |

## Inference Algorithm

Constraint-based Hindley-Milner with levels-based generalization.

### Algorithm Overview

(see: [`Infer.infer`](lib/typing/infer.ml#L56), [`Unify.solve`](lib/typing/unify.ml#L271), [`Generalize.generalize`](lib/typing/generalize.ml#L133))

1. **Constraint generation:** Traverse AST, emit `τ₁ = τ₂` constraints
2. **Constraint solving:** Unify types via union-find with path compression
3. **Generalization:** At let-bindings, generalize type variables with level > outer level

### Unification

(see: [`Unify.unify`](lib/typing/unify.ml#L103), [`Unify.occurs_check`](lib/typing/unify.ml#L52))

```
unify : τ × τ → Result ()
```

| Constraint              | Rule                                           |
| ----------------------- | ---------------------------------------------- |
| α = τ                   | Occurs check, then α ↦ τ                       |
| ⊤ = τ                   | Always succeeds (Any is top)                   |
| C₁ = C₂                 | Succeeds iff C₁ ≡ C₂                           |
| F τ̄₁ = F τ̄₂            | Unify arguments pairwise                       |
| (π̄₁) → τ₁ = (π̄₂) → τ₂  | Unify param lists, then return types           |
| ∀ᾱ.τ₁ = ∀β̄.τ₂          | Same arity, unify bodies (simplified)          |
| τ̄₁ = τ̄₂ (unions)       | Structural equality (no subtyping yet)         |
| τ̄₁ = τ̄₂ (tuples)       | Same length, unify pairwise                    |

**Occurs check:** Prevents infinite types (α ∉ FV(τ) before α ↦ τ). Also adjusts levels for generalization.

### Generalization

(see: [`Generalize.generalize`](lib/typing/generalize.ml#L133), [`Generalize.is_syntactic_value`](lib/typing/generalize.ml#L27))

Only **syntactic values** can be generalized:

| Syntactic Value          | Generalizable? |
| ------------------------ | -------------- |
| Lambda expressions       | ✓              |
| Literals (int, string)   | ✓              |
| Variables                | ✓              |
| Quoted expressions       | ✓              |
| Vectors of values        | ✓              |
| Empty list (`nil`)       | ✓              |
| Function applications    | ✗              |
| Cons pairs               | ✗              |

```elisp
(let ((id (lambda (x) x)))
  (id 1) (id "s"))          ; OK: id : [a] a -> a

(let ((xs (reverse '())))
  xs)                       ; xs : (List '_a) — monomorphic
```

**Level-based generalization:**

```
gen(Γ, τ, e) = ∀ᾱ.τ  where ᾱ = {α ∈ FV(τ) | level(α) > level(Γ)} ∧ syntactic_value(e)
            = τ       otherwise
```

1. Enter new level for let-binding scope (level ← level + 1)
2. Infer binding at higher level
3. Solve constraints immediately
4. Generalize: ᾱ = {α | level(α) > outer_level}
5. If ᾱ ≠ ∅ and e is syntactic value: σ = ∀ᾱ.τ, else σ = τ

### Type Schemes

(see: [`Type_env.scheme`](lib/core/type_env.ml#L17), [`Type_env.instantiate`](lib/core/type_env.ml#L69))

```
σ ::= τ                        Monomorphic     (Mono)
    | ∀α₁...αₙ. τ              Polymorphic     (Poly)
```

**Instantiation:** Given σ = ∀ᾱ.τ, instantiate by σ ↝ τ[ᾱ ↦ β̄] where β̄ are fresh at current level.

### Typing Rules

(see: [`Infer.infer_if`](lib/typing/infer.ml#L211), [`Infer.infer_lambda`](lib/typing/infer.ml#L180), [`Infer.infer_application`](lib/typing/infer.ml#L459), [`Infer.infer_let`](lib/typing/infer.ml#L272))

```
Γ ⊢ c : τ₁    Γ ⊢ t : τ₂    Γ ⊢ e : τ₃    τ₂ = τ₃
────────────────────────────────────────────────── [If]
              Γ ⊢ (if c t e) : τ₂

Γ ⊢ c : τ₁    Γ ⊢ t : τ₂
──────────────────────────────────────────────────  [If-No-Else]
        Γ ⊢ (if c t) : τ₂  (simplified; should be τ₂ ∪ Nil)

Γ, x:τ₁ ⊢ e : τ₂
───────────────────────────────────────────────── [Lambda]
      Γ ⊢ (lambda (x) e) : τ₁ → τ₂

Γ ⊢ f : (π̄) → τ    Γ ⊢ args match π̄
───────────────────────────────────────────────── [App]
            Γ ⊢ (f args...) : τ

Γ ⊢ e : τ    gen(Γ, τ, e) = σ    Γ, x:σ ⊢ body : τ'
──────────────────────────────────────────────────── [Let]
           Γ ⊢ (let ((x e)) body) : τ'
```

| Form                  | Type                                              |
| --------------------- | ------------------------------------------------- |
| `(and)` / `(or)`      | T / Nil                                           |
| `(and e₁ ... eₙ)`     | τₙ (simplified; should be ⋃τᵢ)                    |
| `(or e₁ ... eₙ)`      | τₙ (simplified; should be ⋃τᵢ)                    |
| `(not e)`             | Bool                                              |
| `(setq x e)`          | τ where Γ(x) = τ (if bound)                       |
| `[e₁ ... eₙ]`         | Vector α where ∀i. eᵢ : α                         |
| `'sym`                | Symbol                                            |
| `'(...)`              | List ⊤                                            |

## Built-in Function Types

(see: [`Builtin_types.signatures`](lib/typing/builtin_types.ml#L42), [`Builtin_types.initial_env`](lib/typing/builtin_types.ml#L245))

Types for built-in Elisp functions, loaded into the initial environment.
Shown in `defun` format (directly callable via `(name args...)`).

### List Operations

```elisp
(defun car     [a] (List a) -> (Option a))
(defun cdr     [a] (List a) -> (List a))
(defun cons    [a] (a (List a)) -> (List a))
(defun list    [a] (&rest a) -> (List a))
(defun length  [a] (List a) -> Int)
(defun nth     [a] (Int (List a)) -> (Option a))
(defun nthcdr  [a] (Int (List a)) -> (List a))
(defun append  [a] (&rest (List a)) -> (List a))
(defun reverse [a] (List a) -> (List a))
(defun member  [a] (a (List a)) -> (List a))
```

### Arithmetic

```elisp
(defun +   (&rest Int) -> Int)
(defun -   (Int &rest Int) -> Int)
(defun *   (&rest Int) -> Int)
(defun /   (Int &rest Int) -> Int)
(defun mod (Int Int) -> Int)
(defun abs Int -> Int)
(defun 1+  Int -> Int)
(defun 1-  Int -> Int)
```

### Comparisons

```elisp
(defun <  (Int &rest Int) -> Bool)
(defun >  (Int &rest Int) -> Bool)
(defun <= (Int &rest Int) -> Bool)
(defun >= (Int &rest Int) -> Bool)
(defun =  (Int &rest Int) -> Bool)
```

### Predicates

```elisp
(defun null      Any -> Bool)
(defun atom      Any -> Bool)
(defun listp     Any -> Bool)
(defun consp     Any -> Bool)
(defun symbolp   Any -> Bool)
(defun stringp   Any -> Bool)
(defun numberp   Any -> Bool)
(defun integerp  Any -> Bool)
(defun floatp    Any -> Bool)
(defun vectorp   Any -> Bool)
(defun functionp Any -> Bool)
(defun eq        (Any Any) -> Bool)
(defun equal     (Any Any) -> Bool)
(defun not       Any -> Bool)
```

### Strings

```elisp
(defun concat        (&rest String) -> String)
(defun substring     (String Int &optional Int) -> String)
(defun string-length String -> Int)
(defun upcase        String -> String)
(defun downcase      String -> String)
(defun format        (String &rest Any) -> String)
```

### Vectors

```elisp
(defun vector [a] (&rest a) -> (Vector a))
(defun aref   [a] ((Vector a) Int) -> a)
(defun aset   [a] ((Vector a) Int a) -> a)
```

## Interpreter

Pure OCaml interpreter for macro expansion. No Emacs subprocess.

**Supported:** `quote`, `if`, `let`, `let*`, `lambda`, `progn`, `setq`,
`defmacro`, `cons`, `car`, `cdr`, `list`, `append`, `nth`, `length`,
`+`, `-`, `*`, `/`, `<`, `>`, `=`, `null`, `atom`, `listp`, `symbolp`,
`stringp`, `numberp`, backquote/unquote.

**Opaque** (require annotation): `intern`, `make-symbol`, `eval`, `load`,
`require`, buffer/window/process ops.

## Signature Files (.tart)

Type checking is available for any `.el` file via LSP. When code uses `require`
or calls autoloaded functions, tart searches for corresponding `.tart` files.
A sibling `foo.tart` declares the public interface of `foo.el` and enables
signature verification.

```elisp
;; my-utils.tart
(open 'seq)  ; Import types for use in signatures (not re-exported)

;; Function signatures (directly callable)
(defun my-add (Int Int) -> Int)
(defun my-identity [a] a -> a)
(defun my-process [a] (Seq a) -> (List a))  ; Uses Seq from seq

;; Variable with function type (requires funcall)
(defvar my-handler (String -> Nil))
(defvar my-poly-handler ([a] a -> a))

;; Variable with non-function type
(defvar my-default String)

;; Type alias
(type IntList (List Int))
(type Result (Or Success Failure))

;; Opaque type (no definition = abstract)
(type Handle)
```

### Public vs Internal Types

A `.tart` file declares the **public interface** of a module. Only what appears in
`.tart` is visible to consumers.

| Location | Visibility | Purpose |
|----------|------------|---------|
| `.tart` file | Public | Exported API contract |
| `.el` file (future) | Internal | Implementation types, not exported |

This enables abstraction: the implementation can use rich internal types while
exposing a simpler or opaque public interface.

```elisp
;; my-cache.tart (public interface)
(type Cache)                      ; Opaque - hide implementation
(defun cache-create Int -> Cache)
(defun cache-get [a] (Cache String) -> (Option a))

;; my-cache.el (implementation - future extension)
;; Internal: Cache is actually (HashTable String (Pair Int Any))
;; But consumers only see the opaque type
```

### Module Directives

| Directive | Effect |
|-----------|--------|
| `(open 'module)` | Import types for use in signatures (not re-exported) |
| `(include 'module)` | Inline all declarations (re-exported as part of interface) |

`open` is for referencing external types; `include` is for extending or re-exporting modules.

### Signature Search Path

When a module is required, tart searches for `.tart` files in order:

1. **Sibling file**: `module.tart` next to `module.el` (project-local types)
2. **Search path**: Directories in `tart-type-path` (user/community types)
3. **Bundled stdlib**: `stdlib/module.tart` shipped with tart

This allows providing types for any module, including third-party packages:

```
~/.config/emacs/tart/          ; User's custom type definitions
├── seq.tart              ; Types for seq.el
├── dash.tart             ; Types for dash.el
└── magit-section.tart    ; Types for magit-section.el
```

Each `.tart` in the search path is a standalone file with a `(module name)` declaration.
The first match wins, allowing user overrides of bundled types.

## Design Decisions

| Decision                     | Rationale                        |
| ---------------------------- | -------------------------------- |
| No `any` escape              | Sound typing within typed modules |
| Expand-then-type             | Type expanded code, not macros   |
| Pure OCaml interpreter       | Hermetic, no Emacs dependency    |
| Type checking always on      | Benefit from types without writing `.tart` |
| `.tart` search path          | Community types, user overrides  |
| `.tart` = public interface   | Abstraction; hide implementation details |
| Sibling `.tart` for interface | Verify implementation matches declared API |
| Grouped params               | Matches Elisp semantics          |
| Levels-based generalization  | Near-linear performance          |
| `Option` requires `Truthy`   | Preserves nil-punning            |
| `defun` vs `defvar`          | Tracks Lisp-2 calling convention |
| `[vars]` after `->` or name  | Quantifiers scoped to their arrow |
| Forall inference             | Reduce boilerplate; explicit for phantoms/HKT |

## Limitations and Simplifications

Current implementation simplifications documented for future work:

| Simplification                 | Current Behavior                              | Full Behavior                    |
| ------------------------------ | --------------------------------------------- | -------------------------------- |
| `if` without else              | Returns then-type                             | Should return `(Or then Nil)`    |
| `and`/`or` types               | Type of last argument                         | Should be union of all branches  |
| Union unification              | Requires structural equality                  | Needs subtyping                  |
| `Num` type                     | No relation to `Int`/`Float`                  | Should be supertype              |
| Forall unification             | Same arity, unify bodies directly             | Needs alpha-renaming             |
| Option truthy check            | `is_truthy` returns false for tvars           | Could defer to constraint        |

## Soundness Boundaries

| Feature          | Sound? | Notes                     |
| ---------------- | ------ | ------------------------- |
| Pure functions   | ✓      | Full HM inference         |
| `cl-defstruct`   | ✓      | Direct type correspondence |
| Dynamic vars     | ✗      | Require `.tart` annotations |
| Buffer state     | ✗      | No effect system          |
| Advice           | ✗      | Types change at runtime   |

## Status

**Complete:** Parser, interpreter, type inference, generalization, builtins, diagnostics.

**Planned:** LSP server, `.tart` parser, CLI, Emacs minor mode.

## File Locations

| Component            | Implementation | Interface |
| -------------------- | -------------- | --------- |
| Type representation  | [`types.ml`](lib/core/types.ml) | [`types.mli`](lib/core/types.mli) |
| Type environment     | [`type_env.ml`](lib/core/type_env.ml) | [`type_env.mli`](lib/core/type_env.mli) |
| Constraints          | [`constraint.ml`](lib/typing/constraint.ml) | [`constraint.mli`](lib/typing/constraint.mli) |
| Inference            | [`infer.ml`](lib/typing/infer.ml) | [`infer.mli`](lib/typing/infer.mli) |
| Unification          | [`unify.ml`](lib/typing/unify.ml) | [`unify.mli`](lib/typing/unify.mli) |
| Generalization       | [`generalize.ml`](lib/typing/generalize.ml) | [`generalize.mli`](lib/typing/generalize.mli) |
| Built-in signatures  | [`builtin_types.ml`](lib/typing/builtin_types.ml) | [`builtin_types.mli`](lib/typing/builtin_types.mli) |
| Diagnostics          | [`diagnostic.ml`](lib/typing/diagnostic.ml) | [`diagnostic.mli`](lib/typing/diagnostic.mli) |
| Type checker         | [`check.ml`](lib/typing/check.ml) | [`check.mli`](lib/typing/check.mli) |

## Invariants

1. AST nodes carry source locations (see: [`Location`](lib/syntax/location.ml))
2. Type variables use union-find with path compression (see: [`Types.repr`](lib/core/types.ml#L84))
3. Generalization only at let-bindings, only for syntactic values (see: [`Generalize`](lib/typing/generalize.ml))
4. `Option` argument must be truthy (see: [`Types.is_truthy`](lib/core/types.ml#L229))
5. Macro expansion preserves source location mapping
6. Opaque boundaries halt and require annotation
7. Levels are strictly increasing in nested scopes (see: [`Type_env.enter_level`](lib/core/type_env.ml#L37))
8. Occurs check prevents infinite types and adjusts levels (see: [`Unify.occurs_check`](lib/typing/unify.ml#L52))
