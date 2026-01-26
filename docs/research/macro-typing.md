# Typing Macros: Research Survey and Feasibility Assessment

This document surveys approaches to typing macro systems in the context of
developing a type system for Emacs Lisp.

## 1. The Fundamental Challenge

### Why Macros Are Hard to Type

Macros present a fundamental challenge for static type systems because they
violate the principle that types describe values. Instead, macros operate on
*syntax*---they transform code before evaluation occurs. This creates a temporal
ordering problem:

1. **Expansion precedes typing.** In traditional compilation pipelines, macros
   expand first, producing new syntax. Only then does type checking occur on the
   expanded code.

2. **Code as data.** Lisp macros receive their arguments as unevaluated
   syntax---lists, symbols, and literals---not as typed values. The macro body
   can inspect, destructure, and reconstruct this syntax arbitrarily.

3. **Type-level computation.** A macro's output type often depends on its input
   *syntax*, not just input *types*. For example, a struct-defining macro
   generates accessor functions whose types depend on slot names provided as
   literal symbols.

4. **Hygiene complications.** Macros can introduce new bindings that interact
   with the surrounding scope. In unhygienic systems like Emacs Lisp, variable
   capture can change the types of expressions in non-local ways.

### The Core Tension

The fundamental tension is between:

- **Expressiveness**: Macros enable domain-specific abstractions, code
  generation, and syntactic conveniences that would be impossible or verbose
  otherwise.

- **Static reasoning**: Type systems require knowing the structure and types of
  code *before* execution.

There is no known way to fully resolve this tension. All practical approaches
involve trade-offs.

---

## 2. Typed Racket's Approach: Expand-Then-Type-Check

### Overview

Typed Racket, developed at Northeastern University, is the most mature typed
dialect of a Lisp-family language. It takes a pragmatic approach: expand macros
first, then type-check the fully-expanded program.

### How It Works

1. Macros expand using standard Racket expansion rules
2. The expanded program contains only core forms (lambda, if, etc.)
3. The type checker runs on this expanded core program
4. Type annotations survive expansion via syntax properties

### Advantages

- **Full macro compatibility.** Any Racket macro works in Typed Racket; the type
  checker only sees the expansion.
- **Mature implementation.** Over a decade of development and real-world use.
- **Occurrence typing.** Supports flow-sensitive type narrowing based on
  conditionals.

### Limitations

- **Error messages.** Errors refer to expanded code, not source syntax. Users
  see internal macro implementation details.
- **Macro exports.** Typed Racket cannot safely export macros to untyped code
  because macro expansion could bypass type invariants.
- **No macro typing.** Macro *definitions* are not type-checked; only their
  *expansions* are.

### Key Insight

> "Expressions inside of macros defined in Typed Racket are not type-checked. On
> the other hand, the macro's expansion is always type-checked."
>
> --- [Typed Racket Caveats][tr-caveats]

This is the fundamental limitation: macro bodies are black boxes. The type
system cannot reason about what code a macro will produce without actually
expanding it.

[tr-caveats]: https://docs.racket-lang.org/ts-guide/caveats.html

---

## 3. MacroML and Typed Macros

### Overview

MacroML, introduced by Ganz, Sabry, and Taha (ICFP 2001), represents the most
significant academic work on *typing the macro system itself*. It frames macros
as a form of multi-stage computation.

### Key Ideas

1. **Staging annotations.** Code is annotated with *stages*: present (stage 0)
   vs. future (stage 1+). Macros are stage-0 computations that produce stage-1
   code.

2. **Type safety across stages.** MacroML ensures that:
   - Macro expansion does not depend on runtime values (stage safety)
   - Generated code is well-typed (type safety)

3. **Semantic foundation.** MacroML provides semantics via translation to
   MetaML, a typed multi-stage language. This eliminates the need for
   freshness conditions on variable names.

### What MacroML Supports

- **Inlining.** Type-safe macro-based inlining
- **Recursive macros.** Macros that invoke themselves
- **Binding constructs.** Macros that introduce new variables (the most novel
  contribution)

### Limitations

- **Restrictive.** MacroML cannot express many common Lisp macro patterns,
  particularly those involving:
  - Arbitrary syntax manipulation
  - Dynamic code generation based on literal values
  - Unhygienic variable capture

- **Not adopted.** Despite theoretical elegance, MacroML has not been adopted in
  production language implementations. No mainstream language uses this approach.

### Academic Impact

MacroML demonstrated that *some* forms of macros can be given types, but the
restrictions required are severe enough that the approach hasn't gained
traction. The paper remains primarily of theoretical interest.

**Reference:** Ganz, Sabry, Taha. ["Macros as Multi-Stage Computations"][macroml]
(ICFP 2001)

[macroml]: https://dl.acm.org/doi/10.1145/507669.507646

---

## 4. Template Haskell

### Overview

Template Haskell (TH) provides compile-time metaprogramming for Haskell. Unlike
Lisp macros, TH operates on a typed AST representation.

### How It Works

1. **Typed AST representation.** Code is represented using algebraic data types
   (`Exp`, `Dec`, `Pat`, `Type`) that mirror Haskell's syntax.

2. **Quotation and splicing.** The syntax `[| e |]` quotes expression `e` into
   its AST representation; `$(...)` splices AST back into code.

3. **Q monad.** Metaprograms run in the Q monad, which provides fresh name
   generation and compile-time IO.

### Type Safety Guarantees

Template Haskell provides **partial** type safety:

- **Untyped TH.** The original TH uses untyped AST (`Exp`). Generated code is
  type-checked after splicing, but the metaprogram itself cannot ensure type
  correctness.

- **Typed TH.** Extensions add `TExp a`, representing AST of type `a`. Typed
  quotation `[|| e ||]` and typed splicing `$$e` preserve types through staging.

> "A value of type `TExp a` is the abstract syntax of a Haskell expression of
> type `a`."
>
> --- [Template Haskell Wiki][th-wiki]

### Practical Considerations

- **Stage restriction.** Spliced code cannot refer to same-module bindings
  (required for separate compilation).
- **Error messages.** Errors in generated code can be cryptic.
- **Not Turing-complete typing.** Cannot express "generate N functions for N
  given types."

### Comparison to Lisp

Template Haskell is more restrictive than Lisp macros:

| Aspect | Template Haskell | Lisp Macros |
|--------|-----------------|-------------|
| AST representation | Typed ADTs | Untyped S-expressions |
| Hygiene | Guaranteed by monad | Manual (gensym) |
| Type checking | After splice | After expansion |
| Full language access | Limited (Q monad) | Full Lisp |

[th-wiki]: https://wiki.haskell.org/Template_Haskell

---

## 5. Other Approaches

### 5.1 Rust Procedural Macros

Rust macros operate on token streams before type checking:

> "Procedural macros operate at the syntax level, as source code transformations,
> before any type checking happens."
>
> --- [Rust Reference][rust-macros]

Rust's approach is essentially "expand then type-check," like Typed Racket.
Hygiene is provided through span-based name resolution.

[rust-macros]: https://doc.rust-lang.org/reference/procedural-macros.html

### 5.2 Scala Macros (Blackbox vs. Whitebox)

Scala 2 distinguished two macro types:

- **Blackbox macros.** Must respect their declared type signature. The type
  checker treats them as ordinary methods.

- **Whitebox macros.** Can refine their return type at each call site. More
  expressive but harder to reason about.

Scala 3 removed whitebox macros due to their complexity. Only blackbox-style
macros remain, under the `inline` mechanism.

### 5.3 MetaOCaml

MetaOCaml provides staged metaprogramming with strong guarantees:

> "The generated code is well-formed, well-typed and well-scoped, by
> construction."
>
> --- [MetaOCaml Documentation][metaocaml]

This is achieved by restricting what metaprograms can do. MetaOCaml supports
*staging* (generating specialized code) but not arbitrary syntax transformation.

[metaocaml]: https://okmij.org/ftp/ML/MetaOCaml.html

### 5.4 OCaml PPX

OCaml's PPX system operates on the untyped AST before type checking:

> "Since the AST is constructed before type-checking, optimizations, and code
> generation, all transformations are purely syntactic."
>
> --- [OCaml Documentation][ocaml-ppx]

This is the standard "expand then type-check" approach. PPX transformers are
not type-checked; only their output is.

[ocaml-ppx]: https://ocaml.org/docs/metaprogramming

### 5.5 F# Type Providers

F# type providers are an interesting alternative to macros:

- Types and methods are generated at compile time
- The compiler invokes the type provider during type checking
- Generated types integrate with IntelliSense and compile-time error checking

> "Type providers are compiler plugins... There is no separate code generation
> phase."

This works well for data access (databases, JSON schemas) but is more
restrictive than general macros. Type providers cannot define new syntax.

### 5.6 Turnstile (Racket)

Turnstile takes a different approach: interleave macro expansion and type
checking:

> "Programmers may implement typed languages using a declarative syntax that
> interleaves program rewriting (i.e., macro expansion) and type checking."
>
> --- [Turnstile Guide][turnstile]

This enables type information to guide macro expansion, but requires writing
type rules for every surface construct, not just core forms.

[turnstile]: https://docs.racket-lang.org/turnstile/The_Turnstile_Guide.html

---

## 6. Literature Gaps

### What's Missing

1. **Production-quality typed Lisp macros.** No production system successfully
   types arbitrary `defmacro`-style macros. All working systems either:
   - Type-check after expansion (Typed Racket, Rust)
   - Restrict macro expressiveness (MacroML, MetaOCaml)
   - Eschew macros for other mechanisms (F# type providers)

2. **Gradual typing for macro exports.** Typed Racket cannot export macros
   safely. Research exists on workarounds (Typed Clojure's approach) but with
   significant limitations.

3. **Incremental type checking with macros.** Changing a macro definition may
   require re-type-checking all use sites. No published solutions exist for
   efficient incremental re-checking.

4. **Error messages through macros.** Mapping type errors from expanded code
   back to source locations remains an open problem with ad-hoc solutions.

### Honest Assessment

The academic literature on typing macros is sparse relative to the difficulty of
the problem. MacroML (2001) remains the primary reference for typed macro
*definitions*, but it has not influenced practical language design.

The consensus in practice is: **type the expansion, not the macro**.

---

## 7. Practical Approaches in Production

### What Works

| Approach | Used By | Trade-offs |
|----------|---------|------------|
| Expand-then-type | Typed Racket, Rust, OCaml | Full macro compat; poor errors |
| Restricted staging | MetaOCaml, MacroML | Strong guarantees; limited power |
| Blackbox macros | Scala 2/3 | Predictable; less flexible |
| Type providers | F# | Type-safe; not general macros |
| No macros | Most MLs | Simplicity; less abstraction |

### Common Patterns

1. **Built-in knowledge.** Type checkers have special knowledge of common macros
   (`with-*`, `define-*`). This is manual but effective.

2. **Convention-based inference.** Generate types based on naming conventions
   (e.g., `define-minor-mode foo` creates `foo-mode`, `foo-mode-hook`).

3. **Declaration files.** Provide type signatures for macro expansions in
   separate declaration files (like TypeScript's `.d.ts`).

4. **Expansion caching.** Cache macro expansions to avoid repeated expansion
   during type checking.

---

## 8. Feasibility Assessment for Emacs Lisp

### Unique Challenges of Elisp

1. **Pervasive macros.** Core Elisp functionality relies heavily on macros
   (`defun`, `defvar`, `with-*`, `when`, `unless`, etc.).

2. **Unhygienic by design.** Elisp macros use lexical scoping for captured
   variables but have no hygiene guarantees.

3. **Dynamic scope legacy.** Some macros rely on dynamic binding for context.

4. **Macro expansion at load time.** Elisp expands macros when loading `.elc`
   files, not just at compile time.

5. **No module system.** `provide`/`require` is weaker than Racket modules,
   complicating boundary typing.

### Tractability Categories

Based on analysis of Elisp macro patterns:

#### Tractable (v1 Target)

- **Wrapper macros** (`with-temp-buffer`, `save-excursion`)
  - Fixed return type or body type
  - Implement as special forms with polymorphic signatures

- **Binding macros** (`when-let`, `if-let`, `pcase-let`)
  - Predictable binding structure
  - Supports occurrence typing

- **Definition macros** (`defun`, `cl-defstruct`, `define-minor-mode`)
  - Convention-based name generation
  - Can extract types from `:type` declarations

#### Hard (v2 Target)

- **Pattern matching** (`pcase` with custom patterns)
  - User-defined patterns via `pcase-defmacro`
  - Would require macro-level type signatures

- **Loop DSLs** (`cl-loop`)
  - Complex sublanguage
  - Return type depends on accumulation clauses

- **EIEIO methods** (generic function dispatch)
  - Requires type-directed dispatch analysis

#### Intractable (Out of Scope)

- **Dynamic code generation** (`intern`, `eval`)
  - Symbol identity unknown at compile time
  - Cannot be statically typed

- **String-based metaprogramming**
  - `(funcall (intern (concat pkg "-" func)))`
  - Fundamentally runtime-dependent

- **Self-modifying code**
  - Advice that changes function types
  - Dynamic redefinition

### Recommended Strategy for Tart

1. **Expand-then-type-check.** Follow Typed Racket's proven approach. Expand
   macros first, then type-check the expanded code.

2. **Built-in macro knowledge.** Hard-code type signatures for ~50 common Elisp
   macros. This covers the majority of real-world code.

3. **Convention-based inference.** For definition macros, infer generated names
   from conventions (e.g., `foo-mode-hook` from `define-minor-mode foo-mode`).

4. **Declaration file fallback.** When built-in knowledge is insufficient,
   require `.tart` declarations for macro-generated bindings.

5. **Expansion caching.** Cache expansions for performance, invalidating when
   macro definitions change.

6. **Graceful degradation.** When a macro cannot be typed, emit a warning and
   assign a top type, rather than failing entirely.

### What Not to Attempt

1. **Typing macro definitions.** Don't try to type-check macro bodies. This is
   a research problem without practical solutions.

2. **Arbitrary pcase patterns.** User-defined patterns are effectively macros;
   defer to v2 or require annotations.

3. **Dynamic code generation.** Mark `intern`/`eval` results as opaque symbols
   requiring explicit annotation.

---

## 9. Summary and Recommendations

### Key Takeaways

1. **No silver bullet.** The problem of typing macros is fundamentally hard.
   All practical approaches involve trade-offs.

2. **Expand-then-type works.** Typed Racket demonstrates this approach scales to
   real-world Lisp code, despite imperfect error messages.

3. **Built-in knowledge is pragmatic.** Special-casing common macros is
   inelegant but effective. This is what all production type checkers do.

4. **MacroML is elegant but unused.** Theoretical solutions exist but are too
   restrictive for Lisp's macro culture.

### Recommended Research Directions

For Tart specifically:

1. **Investigate macro expansion APIs.** Can Emacs provide expansion without
   full evaluation? Look at `macroexpand-all`.

2. **Study error message techniques.** How does Typed Racket map errors back
   to source? Consider syntax properties or source maps.

3. **Prototype incrementality.** How expensive is re-expansion when macros
   change? Profile real-world packages.

4. **Survey Elisp macro usage.** What fraction of code uses intractable
   patterns? Empirical data would guide design.

---

## References

### Primary Sources

- Chang, Knauth, Greenman. ["Type Systems as Macros"][tsam] (POPL 2017)
- Ganz, Sabry, Taha. ["Macros as Multi-Stage Computations"][macroml] (ICFP 2001)
- Sheard, Peyton Jones. ["Template Meta-programming for Haskell"][th-paper] (Haskell Workshop 2002)
- Herman, Wand. ["A Theory of Hygienic Macros"][hygiene] (ESOP 2007)

### Documentation

- [Typed Racket Guide][tr-guide]
- [Template Haskell Wiki][th-wiki]
- [Rust Procedural Macros][rust-macros]
- [Scala Blackbox vs Whitebox Macros][scala-macros]
- [MetaOCaml Documentation][metaocaml]

[tsam]: https://www2.ccs.neu.edu/racket/pubs/popl17-ckg.pdf
[th-paper]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/meta-haskell.pdf
[hygiene]: https://www.khoury.northeastern.edu/home/wand/papers/herman-wand-07.pdf
[tr-guide]: https://docs.racket-lang.org/ts-guide/
[scala-macros]: https://docs.scala-lang.org/overviews/macros/blackbox-whitebox.html
