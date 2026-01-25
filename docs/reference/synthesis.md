# Synthesis: Cross-Cutting Analysis and Recommendations

This document synthesizes findings from the Spec 01 research phase, providing
actionable recommendations for the tart type system design.

## Executive Summary

A static type system for Emacs Lisp based on System F with Hindley-Milner
inference is feasible and valuable. The key design decisions are:

1. **Algorithm**: Constraint-based HM inference with levels-based generalization
2. **Macro handling**: Expand-then-type-check (following Typed Racket)
3. **Gradual typing**: Pragmatic soundness with explicit signature files
4. **Implementation language**: OCaml (ecosystem maturity, pattern matching)
5. **LSP-first delivery**: Prioritize hover types and diagnostics

---

## 1. Type System Foundation

### Algorithm Selection: HM with Constraints

**Recommendation**: Use constraint-based Hindley-Milner inference.

**Rationale**:

- Algorithm W is proven sound and complete for the HM fragment
- Constraint-based variant separates generation from solving
- Better error messages: can report all constraint conflicts
- Levels-based generalization (Remy) achieves near-linear performance

**Key implementation points**:

```
source -> constraint generation -> constraint solving -> generalization
              |                         |                      |
         emit τ₁ = τ₂            unification with         let-bindings
                                   union-find              generalize
```

From the Algorithm W research:

> "Level-based generalization... runs in near-linear time (avoiding quadratic
> scanning)... detects escaping type variables... resembles region-based memory
> management."

### Let-Polymorphism and Value Restriction

**Recommendation**: Standard let-polymorphism with value restriction.

Elisp lacks typed mutable references in the core language, so the value
restriction is primarily about ensuring predictable behavior:

- Generalize only syntactic values (lambdas, literals, constructors)
- Conservative but simple
- Familiar to ML programmers

### Higher-Rank Polymorphism

**Recommendation**: Design types to accommodate higher-rank, but start with
rank-1 (HM).

Higher-rank types (like `(forall a. a -> a) -> Int`) are useful for:

- ST monad pattern
- Existential encoding
- Advanced callback patterns

These are rare in typical Elisp. Phase 2 can add bidirectional checking if
needed.

---

## 2. Macro Typing Assessment

### The Fundamental Challenge

From the macro typing research:

> "The problem of typing macros is fundamentally hard. All practical approaches
> involve trade-offs."

Macros operate on syntax before type checking occurs. There is no known way to
type arbitrary Lisp macros without severe restrictions.

### Practical Consensus

Every production type system for Lisp-family languages uses the same approach:

| System       | Approach               |
| ------------ | ---------------------- |
| Typed Racket | Expand-then-type-check |
| Rust         | Expand-then-type-check |
| OCaml PPX    | Expand-then-type-check |

**Recommendation**: Follow this consensus. Expand macros fully, then type-check
the core forms.

### Built-in Macro Knowledge

For common Elisp macros, provide built-in typing rules:

**Tractable (v1)**:

- Wrapper macros: `with-temp-buffer`, `save-excursion`, etc.
- Binding macros: `when-let`, `if-let`, `pcase-let`
- Definition macros: `define-minor-mode`, `cl-defstruct`

**Hard (v2)**:

- `cl-loop` (complex DSL)
- Custom `pcase` patterns
- EIEIO method dispatch

**Intractable**:

- Dynamic code generation (`intern`, `eval`)
- String-based symbol construction

### Error Message Strategy

Post-expansion errors reference expanded code. Mitigation:

1. Track source locations through expansion
2. Provide "this came from macro X" context
3. For known macros, generate errors at source level

---

## 3. Gradual Typing Approach

### Soundness Spectrum

From the gradual typing research:

| Approach             | Runtime Cost | Guarantees     |
| -------------------- | ------------ | -------------- |
| TypeScript (unsound) | Zero         | Types erased   |
| Typed Racket (sound) | Up to 80x    | Full contracts |

### Recommendation: Pragmatic Soundness

**Within typed code**: Full soundness via HM inference.

**At boundaries**: Explicit signatures in `.eli` files, no runtime contracts
(initially).

```elisp
;; foo.eli (signature file)
(sig string-utils
  (declare trim-whitespace (-> String String))
  (declare split-string (-> String String (List String))))
```

**Rationale**:

- Immediate value through IDE integration (hover, completion)
- No runtime overhead
- Clear boundary semantics
- Foundation for optional contracts later

**Migration path**:

1. Write `.eli` signatures for key APIs
2. Type checker verifies typed modules
3. Untyped callers assumed correct
4. Add contracts later if needed

---

## 4. Occurrence Typing

### Essential for Elisp

Elisp heavily uses predicate-based dispatch:

```elisp
(if (stringp x)
    (length x)        ; x : String
  (error "not string"))
```

From the Typed Racket research:

> "Occurrence typing is Typed Racket's signature innovation, enabling the type
> system to reason about predicate-based dispatch common in Lisp code."

### Implementation Approach

**Phase 1**: Basic `if`/`cond` support with standard predicates:

```
stringp  : (-> Any Bool : String)
listp    : (-> Any Bool : List)
integerp : (-> Any Bool : Int)
```

The third component is a type proposition: "when true, argument is String."

**Phase 2**: Custom predicates, `and`/`or` propagation, `pcase` patterns.

### Mutation Limitations

> "Mutation defeats refinement - variables mutated with `set!` cannot be
> refined, since concurrent modification could invalidate the type."

For Elisp: track which variables are mutated; don't refine those.

---

## 5. Implementation Architecture

### Language Choice: OCaml

**Recommendation**: Implement tart in OCaml.

**Rationale**:

From the OCaml implementation research:

- Strong static typing catches implementer bugs early
- Exhaustive pattern matching ensures all cases handled
- Algebraic data types naturally represent ASTs
- Ecosystem: dune, Menhir, ocaml-lsp-server
- Track record: Flow, Hack, Rocq all written in OCaml

**Alternative considered**: Typed Racket

- Pro: Lisp-in-Lisp synergy
- Con: Slower, smaller ecosystem for tooling

### Compiler Phases

```
Source (.el)
    |
    v
[Parsing] --> AST
    |
    v
[Macro Expansion] --> Core AST
    |
    v
[Type Checking] --> Typed AST + Diagnostics
    |
    v
[LSP Integration] --> Hover, Completions, Errors
```

### Type Representation

```ocaml
type typ =
  | TVar of string * int        (* variable with level *)
  | TCon of string              (* Int, String, etc. *)
  | TArr of typ * typ           (* function type *)
  | TForall of string * typ     (* polymorphic type *)
  | TApp of string * typ list   (* type constructor application *)
  | TProduct of typ list        (* tuples *)
  | TSum of (string * typ) list (* ADTs *)
```

Use mutable `TLink` nodes for efficient unification (union-find pattern).

---

## 6. LSP Architecture

### Priority Capabilities

From the LSP patterns research:

| Capability         | Priority  | Purpose                 |
| ------------------ | --------- | ----------------------- |
| publishDiagnostics | Essential | Type errors             |
| hover              | Essential | Display inferred types  |
| definition         | High      | Jump to definitions     |
| codeAction         | High      | Quick fixes             |
| inlayHint          | Medium    | Inline type annotations |

### Incremental Analysis

**Recommendation**: Query-based architecture (Salsa-style).

```
source_text(file) → String              ; Input
parsed_forms(file) → List<Form>         ; Memoized
resolved_symbols(file) → SymbolTable    ; Memoized
inferred_types(file) → TypeMap          ; Memoized
diagnostics(file) → List<Diagnostic>    ; Memoized
```

Key invariant: typing inside a function body should not invalidate type
information for other functions.

### Eglot Integration

Eglot delegates to Emacs builtins:

- Diagnostics → Flymake
- Documentation → ElDoc
- Navigation → Xref

Design the LSP server to work well with these systems.

---

## 7. Error Message Design

### Principles

From the error messages research:

> "The error message should tell programmers what they need to know to fix their
> error."

**Structure**:

```
Error at foo.el:42:10

Type mismatch in function application:

  (add x "hello")
         ^^^^^^^

Expected: Integer
Found:    String

The second argument to 'add' must be an Integer.
```

### Techniques

1. **Track source locations**: Every type carries its origin
2. **Preserve user names**: Don't rename type variables unnecessarily
3. **Type diffing**: Highlight what differs between expected and actual
4. **Related information**: Show where conflicting types originated

---

## 8. Open Questions and Risks

### Design Questions

1. **nil handling**: How to reconcile nil-as-false and nil-as-empty-list?
   - Recommendation: Contextual typing in v1; explicit coercions if needed

2. **Row polymorphism**: Plists strongly motivate row types.
   - Recommendation: Defer to v2; use explicit struct types for v1

3. **Dynamic scope**: How to type `defvar` special variables?
   - Recommendation: Require type annotations in `.eli` files

4. **Advice system**: How do types interact with `advice-add`?
   - Recommendation: Type-check advice definitions with known signatures (same
     as hooks)
   - `:around` advice takes `(-> Args Ret)` as first arg, then same args

5. **Escape hatch**: How to opt out of checking for specific expressions?
   - Recommendation: `(tart-ignore EXPR)` macro; identity at runtime, `Any` at
     type level

### Technical Risks

1. **Macro expansion**: Need to expand macros before type checking
   - Mitigation: Pure Elisp interpreter in OCaml (no Emacs subprocess for
     security)

2. **Performance**: Type checking must be fast for interactive use
   - Mitigation: Incremental analysis, caching, lazy checking

3. **Adoption**: Users must write `.eli` files for untyped code
   - Mitigation: Auto-generate from `cl-defstruct`, `defcustom`

4. **Error message quality**: Post-expansion errors may be confusing
   - Mitigation: Source maps, macro-aware error formatting

---

## 9. Recommendations Summary

### Core Design

| Decision          | Recommendation      | Rationale                    |
| ----------------- | ------------------- | ---------------------------- |
| Base calculus     | System F            | Higher-rank when needed      |
| Inference         | HM with constraints | Sound, complete, good errors |
| Generalization    | Levels-based        | Near-linear performance      |
| Macro handling    | Expand first        | Industry consensus           |
| Gradual typing    | Signature files     | IDE value without overhead   |
| Occurrence typing | Phase 1 basic       | Essential for Lisp           |

### Implementation

| Decision       | Recommendation            | Rationale                   |
| -------------- | ------------------------- | --------------------------- |
| Language       | OCaml                     | Ecosystem, pattern matching |
| Parser         | Menhir                    | Excellent Emacs integration |
| Incrementality | Query-based               | Fast interactive response   |
| LSP            | Hover + diagnostics first | Immediate user value        |

### Scope

| Feature                   | v1 | v2 | Out of scope |
| ------------------------- | -- | -- | ------------ |
| Basic HM inference        | ✓  |    |              |
| `let`-polymorphism        | ✓  |    |              |
| Occurrence typing (basic) | ✓  |    |              |
| Common macro rules        | ✓  |    |              |
| `cl-defstruct` types      | ✓  |    |              |
| `.eli` signature files    | ✓  |    |              |
| Advice definitions        | ✓  |    |              |
| `(tart-ignore ...)`       | ✓  |    |              |
| Higher-rank types         |    | ✓  |              |
| Row polymorphism          |    | ✓  |              |
| Full `pcase` typing       |    | ✓  |              |
| Runtime contracts         |    | ✓  |              |
| EIEIO multi-dispatch      |    |    | ✓            |
| Dynamic code generation   |    |    | ✓            |

---

## 10. Next Steps

1. **Design type syntax grammar** (Spec 03 R1)
2. **Select and document inference algorithm** (Spec 03 R2)
3. **Specify `.eli` file format** (Spec 03 R3)
4. **Design ADT system** (Spec 03 R4)
5. **Prototype basic inference on small Elisp files**

Proceed to Spec 03: Type System Design.
