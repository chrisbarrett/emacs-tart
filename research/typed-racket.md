# Typed Racket: Design and Implementation Analysis

This document examines Typed Racket's design and implementation as reference
material for building a type system for Emacs Lisp. Both languages share key
characteristics: Lisp syntax, powerful macro systems, and dynamic typing
traditions.

## Overview

Typed Racket is a gradually-typed dialect of Racket that allows incremental
addition of static type annotations to existing Racket programs. It was
designed by Sam Tobin-Hochstadt and Matthias Felleisen, with the foundational
work published in "The Design and Implementation of Typed Scheme" (POPL 2008).

Key design principles:

1. **Gradual adoption** - Types can be added incrementally to existing untyped
   code
2. **Sound type system** - Runtime contracts enforce types at boundaries
3. **Macro compatibility** - Type checking works with Racket's macro system
4. **Practical idiom support** - Occurrence typing handles common Lisp patterns

## Type/Macro Interaction

### The Challenge

Macros place a significant constraint on type system design. Supporting macros
requires type-checking a language with a user-defined set of syntactic forms.
The Typed Racket designers overcame this by integrating the type checker with
the macro expander.

### Expansion Before Type Checking

Typed Racket fully expands macros before type checking. The key mechanism is
Racket's `local-expand` primitive, which expands a form in the current syntactic
environment. This allows:

```
Surface Syntax (with macros)
        |
        v
  Macro Expansion (via local-expand)
        |
        v
  Fully Expanded Core Forms
        |
        v
    Type Checking
        |
        v
  Contract Insertion + Code Generation
```

A fully-expanded Typed Racket program uses only Racket's core forms, which the
type checker understands. This design means:

- Macro implementations (compile-time code) are NOT type checked
- Macro expansions (the generated code) ARE type checked
- Users can write macros that expand to well-typed code

### Type Systems as Macros (Turnstile)

The "Type Systems as Macros" paper (Chang, Knauth, Greenman - POPL 2017)
demonstrates that type checking can be embedded directly within macro
expansion. The Turnstile library implements this approach:

```racket
;; Type rules resemble traditional judgment notation
(define-typed-syntax (lambda ([x : τ] ...) body)
  [⊢ body ≫ body- ⇒ τ_out]
  --------
  [⊢ (λ (x ...) body-) ⇒ (→ τ ... τ_out)])
```

Key insights:

- **Linguistic reuse** - The macro system provides variable binding, scope, and
  hygiene for free
- **Bidirectional typing** - Inference mode (⇒) and checking mode (⇐)
  correspond to syntax patterns and templates
- **Type environment via lexical scope** - Rather than threading an explicit
  environment, Turnstile reuses Racket's lexical scope

### Type Tailoring

Macros can implement domain-specific type rules through "type tailoring":

```racket
;; A macro that validates regex patterns at compile time
(define-syntax smart-match
  (syntax-parser
    [(_ str:string pattern:string)
     #:with n (count-capture-groups #'pattern)
     #'(ann (regexp-match pattern str)
            (Option (List String ...n)))]))
```

This enables:

- Validating domain-specific constraints at compile time
- Generating more precise types than the base system provides
- Extending the type system without modifying the type checker

### Implications for Elisp

1. **Expand first, type check later** - Type check the fully macro-expanded
   code, not surface syntax
2. **Core form vocabulary** - Define a fixed set of core forms the type checker
   understands
3. **Macro bodies escape checking** - Accept that macro implementation code
   won't be type checked
4. **Type-aware macros possible** - Macros can attach type information to their
   expansions

## Occurrence Typing

Occurrence typing is Typed Racket's signature innovation, enabling the type
system to reason about predicate-based dispatch common in Lisp code.

### Core Mechanism

When a predicate check succeeds or fails, the type of the tested variable
narrows in the corresponding branch:

```racket
(define (f [x : (U String (Listof Any))]) : Number
  (if (string? x)
      (string-length x)    ; x : String
      (length x)))         ; x : (Listof Any)
```

The type system understands that in the "then" branch, `string?` must have
returned true, so `x` must be a `String`.

### Predicate Types with Propositions

Predicates carry logical propositions that describe what information the type
system gains:

```racket
;; The type of string?
(-> Any Boolean : String)
```

The third component (`: String`) is a proposition indicating that when the
predicate returns true, the argument has type `String`.

Custom predicates require explicit propositions:

```racket
;; One-sided proposition (only refines on success)
(: positive-integer? (-> Any Boolean : #:+ Positive-Integer))
```

### Logical Foundations

The "Logical Types for Untyped Languages" paper (ICFP 2010) formalizes
occurrence typing using propositional logic:

- Expressions derive general propositions (not just types)
- A proposition environment replaces the traditional type environment
- Standard logical rules combine propositions from control flow

This logical foundation enables reasoning like:

```racket
(if (and (string? x) (positive? (string-length x)))
    ;; x : String, and we know (string-length x) > 0
    ...)
```

### Control Flow Analysis

Occurrence typing works across control flow constructs:

- `if`, `cond`, `when`, `unless`
- `and`, `or` (short-circuit evaluation)
- `match` (with limitations)

All these forms macro-expand to `if` in the end, so the type checker only needs
to understand `if`.

### Limitations

1. **Mutation defeats refinement** - Variables mutated with `set!` cannot be
   refined, since concurrent modification could invalidate the type
2. **Top-level variables** - REPL variables cannot be refined (could be mutated
   later)
3. **Aliasing** - Only direct variable references can be refined
4. **match limitations** - Pattern match failures don't always refine types
   correctly

### Implications for Elisp

1. **Predicate-based dispatch is essential** - Elisp heavily uses predicates
   (`stringp`, `listp`, etc.) for type dispatch
2. **Propositions enable precision** - Type predicates should carry refinement
   information
3. **Mutation complicates analysis** - Elisp's pervasive mutation may limit
   refinement opportunities
4. **Keep it simple initially** - Start with `if`/`cond` support, extend later

## Untyped Code Boundaries

### The Contract System

When typed and untyped code interact, Typed Racket generates runtime contracts
to enforce type safety:

```
Typed Module                         Untyped Module
     |                                     |
     |  export: (-> String Integer)        |
     +------------[CONTRACT]---------------+
                      |
            Checks: argument is String
            Checks: result is Integer
```

This is the foundation of "sound" gradual typing - types mean something at
runtime, not just at compile time.

### require/typed

Typed modules import untyped bindings using `require/typed`, providing explicit
type annotations:

```racket
(require/typed "untyped-lib.rkt"
  [distance (-> point point Real)]
  [#:struct point ([x : Real] [y : Real])]
  [#:opaque OpaqueType opaque-type?])
```

Key mechanisms:

- **Function types** become contracts checking arguments and results
- **Struct types** check field types on construction and access
- **Opaque types** hide implementation, exposing only a predicate

### Three Enforcement Strategies

Typed Racket offers a spectrum of type enforcement:

| Strategy | Enforcement | Performance | Guarantees |
|----------|-------------|-------------|------------|
| **Deep** | Full contracts | Highest overhead | Complete type safety |
| **Shallow** | Shape checks | Moderate overhead | Basic structural safety |
| **Optional** | None | Zero overhead | Compile-time only |

#### Deep Types (Default)

- Rigorous contract checks at every boundary crossing
- Higher-order values wrapped with monitoring proxies
- Enables safe type-directed optimizations
- Can detect errors far from their source

```racket
;; (-> pt pt Integer) becomes a function contract that
;; raises an exception if it ever returns a non-integer
```

#### Shallow Types

- Check only the "shape" of values (is it a list? a function?)
- Element-by-element validation happens lazily on access
- Constant-time for most types
- Based on the Transient semantics (Vitousek)

```racket
#lang typed/racket/shallow
;; Shape checks, not full contracts
```

#### Optional Types

- No runtime enforcement
- Identical to untyped Racket at runtime
- Useful for:
  - Prototyping type annotations
  - Documentation
  - Cases where contracts are too restrictive

### Performance Implications

The Typed Racket team has extensively studied gradual typing performance:

- **Best case**: Large typed regions with few boundaries have minimal overhead
- **Worst case**: Many typed-untyped crossings can cause 10-100x slowdowns
- **Higher-order values**: Functions, vectors, objects are expensive to wrap
- **Mutable state**: Requires proxy wrappers for ongoing monitoring

From experience reports:

> "Some programmers found 25x-50x slowdowns when using an array library"
>
> "Two others reported over 1000x slowdowns when using a library of functional
> data structures"

The Shallow and Optional strategies were developed specifically to address
these performance issues.

### Contract Verification

Recent research (Corpse Reviver - POPL 2020) explores static verification of
contracts:

- Analyze untyped code to prove contracts can't fail
- Eliminate proven contracts at compile time
- Reduced median overhead from 2.5x to near-zero

### Implications for Elisp

1. **Contracts essential for soundness** - Without runtime checks, types are
   just documentation
2. **Performance matters** - Elisp users expect interactive responsiveness
3. **Consider multiple enforcement levels** - Allow users to choose their
   tradeoff point
4. **Higher-order is hard** - Functions crossing boundaries are expensive
5. **Bulk checking helps** - Type-check large modules together to minimize
   boundaries

## Type Syntax

### Annotation Forms

Typed Racket provides multiple ways to annotate types:

```racket
;; Top-level annotation (before definition)
(: x Number)
(define x 7)

;; Inline in define
(define (f [x : Number]) : Number
  (+ x 1))

;; Lambda with types
(lambda ([x : Number] [y : String]) : Number
  (+ x (string-length y)))

;; Expression annotation
(ann (+ 1 2) Number)

;; Let bindings
(let ([x : Number 7])
  (add1 x))
```

### Type Constructors

#### Base Types

```racket
Number, Integer, String, Symbol, Boolean, Char, Void, Any, Nothing
```

#### Function Types

```racket
(-> Number Number)              ; one arg, one result
(-> Number Number Number)       ; two args
(-> Number * Number)            ; variadic
(->* (Number) (String) Number)  ; optional args
```

#### Union Types

```racket
(U Number String Boolean)       ; any of these types
(Option Number)                 ; (U Number #f)
```

#### Recursive Types

```racket
(define-type BinaryTree
  (U 'leaf (List Number BinaryTree BinaryTree)))
```

#### Polymorphic Types

```racket
(All (A) (-> A A))                    ; identity function
(All (A B) (-> A (-> B A)))           ; const
(All (A ...) (-> A ... A (Listof A))) ; variadic polymorphism
```

#### Struct Types

```racket
(struct point ([x : Real] [y : Real]))
; Creates type: point
; Constructor: (-> Real Real point)
; Accessors: (-> point Real)
```

### Type Inference

Typed Racket performs local type inference:

- Local bindings (`let`, `let*`) inferred automatically
- Top-level constants often inferred
- Functions require annotations (no global inference)
- Polymorphic applications may need explicit instantiation

```racket
;; Explicit instantiation needed for polymorphic + higher-order
(map (inst identity Number) '(1 2 3))
```

### Implications for Elisp

1. **Multiple annotation styles** - Support both prefix annotations and inline
2. **Union types essential** - Elisp functions often accept multiple types
3. **Keep inference local** - Global inference is complex and has poor error
   messages
4. **Struct/record types** - `cl-defstruct` should integrate with the type
   system

## Integration with Racket

### Language Tower

Typed Racket sits atop untyped Racket:

```
    typed/racket
         |
         | (contracts + type erasure)
         v
      racket
```

After type checking, Typed Racket code becomes plain Racket with contracts
inserted at boundaries. This enables:

- Full access to Racket libraries (via `require/typed`)
- Typed modules callable from untyped code
- Shared runtime and tooling

### Module-Level Granularity

The typed/untyped boundary operates at module granularity:

- Each module is entirely typed OR entirely untyped
- Contracts are inserted at `require`/`provide` points
- No mixed typed/untyped code within a module

This simplifies the contract story and enables module-level optimization.

### Macro Export Limitation

> "Macros defined in typed modules may not be used in untyped modules"

This limitation exists because:

1. Macros run at compile time
2. Typed Racket only checks runtime-phase code
3. Macro expansion in untyped context could produce ill-typed code

### Implications for Elisp

1. **Module/package boundaries** - Use Elisp's package system as the typed
   boundary
2. **Interop is crucial** - Most code will need to call untyped libraries
3. **Consider file-level typing** - A file is typed or untyped, no mixing
4. **Macro handling** - Decide whether typed macros can be used in untyped
   contexts

## Implementation Architecture

### Compilation Pipeline

```
Source Code (typed/racket)
        |
        v
  Macro Expansion (Racket expander)
        |
        v
  Fully Expanded Syntax
        |
        v
  Type Checking (custom pass)
        |
        v
  Contract Generation
        |
        v
  Optimization (optional)
        |
        v
  Plain Racket Code
        |
        v
  Racket Compiler
```

### Key Implementation Points

1. **Type checker is a macro** - Typed Racket installs itself as the
   module-level macro that orchestrates everything
2. **Syntax properties carry types** - Type information attached to syntax
   objects as properties
3. **Separate compilation** - Each module type-checked independently
4. **Phases matter** - Only runtime phase (phase 0) is type checked

### Performance Characteristics

From user reports:

- **Compilation**: ~1500 lines takes 14.2 seconds (7.7s cached)
- **Contract overhead**: Significant for higher-order values
- **Type-driven optimization**: Can improve runtime performance

### Implications for Elisp

1. **Leverage byte-compiler** - Hook into Elisp's existing compilation
2. **Syntax properties** - Use text properties or symbols to carry type info
3. **Incremental checking** - File-by-file checking like separate compilation
4. **Cache type information** - Persist signatures for library code

## Lessons for Elisp Type System Design

### What Works Well in Typed Racket

1. **Occurrence typing** - Essential for idiomatic Lisp code with predicates
2. **Gradual adoption** - Add types incrementally, not all-or-nothing
3. **Module-level boundaries** - Clear typed/untyped separation points
4. **Macro-first design** - Expand before checking avoids macro complexity

### Known Pain Points

1. **Compilation speed** - Type checking adds significant compile time
2. **Contract overhead** - Can cause 10-1000x slowdowns in worst cases
3. **Error messages** - Post-expansion errors reference expanded code
4. **Tooling gaps** - Editor support lags behind other typed languages
5. **Higher-order polymorphism** - Requires manual type instantiation

### Design Recommendations for Elisp

1. **Start with occurrence typing** - This is the key feature for Lisp idioms
2. **Expand macros first** - Don't try to type-check macro definitions
3. **Support gradual typing** - Types must be optional and incremental
4. **Consider enforcement levels** - Offer Deep/Shallow/Optional choices
5. **Keep inference local** - Require annotations on function signatures
6. **Union types are essential** - Elisp functions are polymorphic in practice
7. **Handle mutation carefully** - Elisp uses mutation more than Racket
8. **Integrate with existing tooling** - Work with byte-compiler, not against it

### Open Questions for Elisp

1. **Advice and hooks** - How do types interact with Elisp's advice system?
2. **Buffer-local variables** - How to type variables that vary per-buffer?
3. **Property lists** - How to type plist-based polymorphism?
4. **Dynamically-scoped variables** - What types for `defvar` special variables?
5. **Eval and dynamic loading** - How strict can we be with runtime evaluation?

## References

### Primary Sources

- [The Design and Implementation of Typed Scheme](https://www2.ccs.neu.edu/racket/pubs/popl08-thf.pdf) - Tobin-Hochstadt & Felleisen, POPL 2008
- [Logical Types for Untyped Languages](https://www2.ccs.neu.edu/racket/pubs/icfp10-thf.pdf) - Tobin-Hochstadt & Felleisen, ICFP 2010
- [Type Systems as Macros](https://www.khoury.northeastern.edu/home/stchang/popl2017/) - Chang, Knauth, Greenman, POPL 2017
- [How to Evaluate the Performance of Gradual Type Systems](https://janvitek.org/pubs/jfp18.pdf) - Greenman et al., JFP 2018

### Documentation

- [The Typed Racket Guide](https://docs.racket-lang.org/ts-guide/)
- [Occurrence Typing](https://docs.racket-lang.org/ts-guide/occurrence-typing.html)
- [Typed-Untyped Interaction](https://docs.racket-lang.org/ts-guide/typed-untyped-interaction.html)
- [Deep, Shallow, and Optional Semantics](https://docs.racket-lang.org/ts-reference/behavior-of-types.html)
- [The Turnstile Guide](https://docs.racket-lang.org/turnstile/The_Turnstile_Guide.html)

### Experience Reports

- [The Good and the Bad of Typed Racket](https://micahcantor.com/blog/thoughts-typed-racket) - Micah Cantor
- [First Impressions with Typed Racket](https://pavpanchekha.com/blog/typed-racket-1.html) - Pavel Panchekha
- [Gradual Typing Across the Spectrum](https://prl.khoury.northeastern.edu/blog/2016/05/18/gradual-typing-across-the-spectrum/) - PRL Blog
- [Shallow Typed Racket](https://blog.racket-lang.org/2022/11/shallow-typed-racket.html) - Racket Blog
- [Type Tailoring](https://blog.racket-lang.org/2017/04/type-tailoring.html) - Racket Blog
