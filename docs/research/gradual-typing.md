# Gradual Typing Theory

Research notes on gradual typing for the tart type system project, focusing on
practical implications for implementing a gradually typed system over Emacs
Lisp.

## Gradual Typing Fundamentals

Gradual typing was introduced by [Siek and Taha in 2006][siek-taha-2006] as a
theory for integrating static and dynamic typing within a single language. The
core goals are:

1. Put the programmer in control of which regions of code are statically or
   dynamically typed
2. Enable gradual evolution of code between the two typing disciplines

### The Consistency Relation

The key innovation in gradual typing is replacing type equality with a new
relation called **consistency** (written `~`). Unlike equality:

- Consistency relates the dynamic type `?` (or `*`) to every other type
- Consistency is reflexive and symmetric but **not transitive**

The non-transitivity is crucial. If consistency were transitive, we would have:

```
Int ~ ? ~ String  implies  Int ~ String  (WRONG!)
```

The consistency relation permits implicit casts to and from the unknown type
while still catching static type errors. For example:

```
? ~ Int         -- dynamic can flow to Int
Int ~ ?         -- Int can flow to dynamic
Int ~ Int       -- reflexive
Int ~ String    -- NOT consistent (static error)
```

In typing rules, function application requires the argument type to be
*consistent* with (not equal to) the parameter type. This allows both `? ~ Int`
and `? ~ Bool` to hold, making dynamically typed expressions compatible with any
statically typed context.

### The Dynamic Type and Its Semantics

The dynamic type (variously written `?`, `*`, `Dyn`, or `any`) represents
statically unknown types. Values of dynamic type are not untyped--they carry
their runtime type with them.

At runtime, when a value flows from dynamic type to a static type, a **cast** is
inserted:

```
<Int ⇐ ?>  -- cast from dynamic to Int
<? ⇐ Int>  -- cast from Int to dynamic
```

Casts have these properties:

- A cast from `T` to `?` always succeeds (values can always become dynamic)
- A cast from `?` to `T` may fail at runtime if the underlying value is
  incompatible with `T`
- Cast failure raises a runtime error with blame information

### Blame Tracking

When a cast fails, the system must identify which part of the program is
responsible. This mechanism, called **blame tracking**, was developed by
[Findler and Felleisen (2002)][findler-felleisen] for contracts and adapted to
gradual typing.

A cast is annotated with a **blame label** identifying its source location:

```
<Int ⇐ ?>^l  -- cast at location l
```

When this cast fails (e.g., the value is actually a string), the blame label `l`
is reported, identifying where the type boundary was crossed.

### The Gradual Guarantee

The **gradual guarantee** (or "graduality") states that:

1. Removing type annotations always produces a well-typed program
2. A program remains well-typed as long as only *correct* type annotations are
   added

"Correct" means the annotation agrees with the corresponding annotation in some
fully annotated and well-typed version of the program. This guarantee ensures
that adding types is always safe and predictable.

## Sound vs Unsound Approaches

The gradual typing landscape is divided between sound systems that preserve type
safety and unsound systems that prioritize developer experience.

### TypeScript's Unsound Approach

TypeScript [deliberately chose unsoundness][typescript-unsound]:

> "TypeScript's type system allows certain operations that can't be known at
> compile-time to be safe. When a type system has this property, it is said to
> not be 'sound.'"

Key unsoundness sources in TypeScript include:

- **Bivariant function parameters** (historic, partially fixed)
- **`any` type escapes** - `any` bypasses all checking
- **Type assertions** - `as T` trusts the programmer
- **Index signatures** - accessing missing properties
- **Method extraction** - losing `this` context

The rationale is pragmatic:

- Seamless integration with existing JavaScript libraries
- Lower cognitive overhead for JavaScript developers
- Avoiding the friction of explicit boundaries

TypeScript's philosophy: "We don't aim for soundness, we aim for productivity."

**Trade-offs:**

- Faster adoption and better developer experience
- Works with untyped JavaScript without runtime changes
- No runtime overhead
- But: cannot prove properties about programs
- Type errors may manifest as runtime crashes far from their source

### Typed Racket's Sound Approach

[Typed Racket][typed-racket] is the most sophisticated sound gradual type
system. It guarantees:

- Well-typed programs cannot produce type errors
- Type-driven optimizations are safe
- Values maintain type invariants at runtime

Sound gradual typing requires **runtime enforcement** at the boundaries between
typed and untyped code. Typed Racket uses contracts to:

1. Check values flowing from untyped to typed code
2. Wrap higher-order values (functions, objects) with monitors
3. Delay checking until values are used

**Trade-offs:**

- Strong safety guarantees enable reasoning about code
- Type-driven compiler optimizations are sound
- But: significant performance overhead (up to 80x in pathological cases)
- Explicit boundary management required
- Steeper learning curve

### The Soundness Spectrum

Between these extremes lie intermediate approaches:

| Approach | Soundness | Runtime Cost | Guarantees |
| :--- | :---: | :---: | :--- |
| TypeScript | None | Zero | None (types erased) |
| Flow | Partial | Zero | Limited (types erased) |
| Safe TypeScript | Full | ~15% | Full with contracts |
| Typed Racket | Full | Up to 80x | Full with contracts |

## Migration Strategies

Gradual typing promises incremental migration from untyped to typed code, but
this process involves significant trade-offs.

### Coarse-Grained vs Fine-Grained

**Coarse-grained** (Typed Racket): Each module is fully typed or fully untyped.
Boundaries exist at module edges.

- Simpler reasoning about where checks occur
- Higher overhead when typed/untyped modules interact frequently
- Natural fit for module-based languages

**Fine-grained**: Individual expressions, parameters, or variables can be typed
or untyped within the same module.

- More flexible migration paths
- More complex boundary semantics
- Can lead to "boundary soup" with many small transitions

### The Migration Path Problem

Research on [profiler-guided migration][profiler-migration] reveals:

> "The component-by-component migration of a program from untyped to typed can
> trigger unintended performance degradations."

The key insight: **typing order matters**. Adding types to the "wrong" component
first can make performance worse, not better.

Effective strategies:

1. **Boundary profiling**: Identify the costliest inter-component boundaries
2. **Type both sides**: When a boundary is expensive, add types to both modules
3. **Deep before shallow**: Prefer strong type enforcement over weak

### Mixed Typed/Untyped Modules

At module boundaries, the type system must:

1. **Export checking**: Ensure typed values match their declared types
2. **Import wrapping**: Protect typed code from untyped imports
3. **Re-export handling**: Track type information through re-exports

Typed Racket's approach:

```racket
#lang typed/racket
(require/typed "untyped-lib.rkt"
  [process-data (-> String Integer)])
```

The `require/typed` form inserts contracts at the import boundary.

### Boundary Costs

In sound gradual typing, [boundaries impose costs][boundary-costs]:

- **Checking flat values**: O(1) for primitives, O(n) for structures
- **Wrapping higher-order values**: Allocates proxy objects
- **Proxy chains**: Repeated crossings accumulate wrappers
- **Memory pressure**: Proxies increase allocation

Measured overhead in Typed Racket benchmarks:

- Median overhead: 2.5x
- Worst case: 80x
- Only 2% of configurations achieve < 1.4x overhead

## Contract Semantics

Sound gradual typing is built on contract systems that monitor values at type
boundaries.

### Flat vs Higher-Order Contracts

**Flat contracts** check values immediately:

```
(flat-contract integer?)  -- checks x is an integer
(flat-contract string?)   -- checks x is a string
```

Flat contracts are simple predicates with O(1) or O(n) checking cost.

**Higher-order contracts** wrap functions to delay checking:

```
(-> integer? integer?)  -- function from int to int
```

This contract cannot be checked immediately because we cannot inspect a
function's behavior. Instead, we wrap the function:

```
(lambda (x)
  (check-integer!
    (f (check-integer! x))))
```

The wrapper checks arguments before passing them in and checks results before
returning them.

### Runtime Checking Overhead

Higher-order contracts impose several costs:

1. **Immediate traversal**: Compound values (lists, records) are traversed
2. **Proxy allocation**: Wrappers consume memory
3. **Indirection**: Every call through a wrapper adds overhead
4. **Proxy accumulation**: Values crossing boundaries repeatedly accumulate
   layers

[Space-efficient semantics][space-efficient] address proxy accumulation by
merging adjacent casts:

```
<Int ⇐ ?> ∘ <? ⇐ String>  ==>  <Int ⇐ String>  (fails immediately)
```

### Chaperones and Impersonators (Racket)

Racket provides two mechanisms for [contract interposition][chaperones]:

**Chaperones** wrap values while preserving their behavior:

- Must return results equivalent to the original
- Can add checks but not modify behavior
- Safe for use on immutable data
- Preserve equality: `(eq? v (chaperone v))` can be true

**Impersonators** can modify behavior:

- Can return different results
- Required for mutable data contracts
- More powerful but fewer guarantees

This hierarchy enables:

- Lazy checking of large data structures
- Contracts on mutable fields
- Object equality preservation where appropriate

Contract types in Racket:

| Contract Type | Can Wrap | Guarantees |
| :--- | :--- | :--- |
| Flat | Immediate values | Checked immediately |
| Chaperone | Immutable data, functions | Behavior preserved |
| Impersonator | Mutable data | None (full interposition) |

### Deep vs Shallow Enforcement

**Deep types** (Natural semantics): Full contract enforcement at boundaries.
Values are wrapped, and contracts check on every use.

**Shallow types** (Transient semantics): Only tag-level checks. Verifies a
function is a function, not that it has the right signature.

Research shows the best debugging experience comes from combining Natural
semantics with blame tracking.

## Blame and Error Messages

Effective error reporting is crucial for gradual typing adoption.

### Positive vs Negative Blame

[Wadler and Findler][wadler-findler] decomposed blame into two directions:

**Positive blame**: The value producer violated the contract.

```
-- Typed code expects Integer, untyped code provides String
<Integer ⇐ ?>^+l  applied to "hello"
-- Positive blame on l: untyped code broke the contract
```

**Negative blame**: The value consumer violated the contract.

```
-- Untyped code receives function (Integer -> Integer)
-- Untyped code calls it with String
-- Negative blame: consumer misused the value
```

For function contracts `A -> B`:

- Positive blame: function returned wrong type (violated B)
- Negative blame: caller passed wrong type (violated A)

### The Blame Theorem

Wadler and Findler's central result:

> "Well-typed programs can't be blamed."

Formally: In a cast between more-precise and less-precise types, blame always
falls on the less-precise side.

```
<Int ⇐ ?>^l  -- if this fails, blame l (the dynamic side)
```

This means:

- Typed code is never blamed when interacting with untyped code
- Adding types can only reduce blame, never introduce it
- The dynamic side is always "at fault" when boundaries fail

### Practical Implications for Error Messages

Good gradual typing errors should:

1. **Identify the boundary**: Where typed and untyped code meet
2. **Show expected vs actual**: What type was expected, what was received
3. **Provide blame**: Which side of the boundary is responsible
4. **Include stack context**: How we got here

Example error format:

```
Type error: contract violation
  expected: Integer
  received: "hello"
  at boundary: typed-module.rkt:15
  blamed: untyped-client.rkt:42
  context:
    untyped-client.rkt:42: (process-data user-input)
    typed-module.rkt:15: (define (process-data [x : Integer]) ...)
```

### The Effectiveness of Blame

Recent [research on blame effectiveness][blame-effectiveness] found surprising
results:

> "In most scenarios, strategies with imprecise blame assignment are as helpful
> to a rationally acting programmer as strategies with provably correct blame."

However, combining Natural semantics with blame tracking provides "significantly
superior debugging hints" compared to alternatives. The combination matters more
than perfect blame precision.

## Relevance to Elisp

Emacs Lisp presents unique challenges and opportunities for gradual typing.

### The Elisp Landscape

Most Elisp code is completely untyped:

- No standard type annotation mechanism
- Dynamic dispatch via `cl-defgeneric` is common
- Heavy use of macros and advice
- `defcustom` variables modified by users at runtime
- Buffer-local variables with dynamic extent

The ecosystem includes:

- Core Emacs (C + Elisp, mostly untyped)
- ELPA/MELPA packages (all untyped)
- User configurations (inherently dynamic)

### Package Boundaries

Natural boundaries exist at package edges:

- `require`/`provide` semantics
- Declared public API vs internal functions
- Package dependencies form a DAG

A gradual type system could:

1. Type-check within opted-in packages
2. Insert contracts at package boundaries
3. Treat untyped packages as providing `?` types

### Soundness Considerations

Full soundness in Elisp faces challenges:

- **Advice system**: Functions can be modified after definition
- **Buffer-local variables**: Types may vary by buffer
- **Dynamic `let`**: Can rebind any variable
- **`eval` and `funcall`**: Runtime code generation

Options:

1. **Full unsoundness** (TypeScript model): Types as documentation only
2. **Partial soundness**: Check what we can, leave escape hatches
3. **Opt-in boundaries**: Contracts only at explicit boundaries

### Recommended Approach for tart

Given the constraints in the [project specifications][specs], I recommend a
**pragmatic soundness** approach:

**Type checking should be sound within typed code:**

- Full Hindley-Milner inference within `.tart`-annotated modules
- No implicit `any` - all types must be declared at boundaries
- Type errors are always reported, never silently ignored

**Boundaries with untyped code use explicit signatures:**

```elisp
;; In foo.tart (signature file)
(declare process-data (-> String Integer))
```

The typed world trusts these declarations. If untyped code violates them, the
error occurs at runtime but is attributed to the boundary.

**No runtime contracts (initially):**

- Focus on static checking for IDE features (hover, completion)
- Contracts add complexity and performance concerns
- Can be added later as an opt-in feature

**Blame at the specification level:**

- If runtime errors occur, the `.tart` file location identifies the boundary
- Users know where typed/untyped worlds meet
- Encourages accurate signature writing

**Migration path:**

1. Start with signature files for key APIs
2. Type checker runs on typed modules
3. Untyped callers are assumed correct
4. Add contracts later if soundness guarantees become valuable

This approach provides:

- Immediate value through IDE integration
- Incremental adoption without performance penalty
- Clear boundaries without complex contract machinery
- A foundation for optional soundness later

### Open Questions

1. **Macro typing**: How do macros interact with type signatures?
2. **Generic functions**: Can `cl-defgeneric` dispatch be typed?
3. **Dynamic `let`**: Should dynamically-scoped variables have special types?
4. **Advice**: Can advised functions maintain type safety?

These questions require further investigation and likely compromise between type
safety and Elisp idioms.

## References

[siek-taha-2006]: http://scheme2006.cs.uchicago.edu/13-siek.pdf
[findler-felleisen]: https://www2.ccs.neu.edu/racket/pubs/icfp2002-ff.pdf
[typescript-unsound]: https://effectivetypescript.com/2021/05/06/unsoundness/
[typed-racket]: https://docs.racket-lang.org/ts-guide/
[profiler-migration]: https://www2.ccs.neu.edu/racket/pubs/oopsla23-gfd.pdf
[boundary-costs]: https://janvitek.org/pubs/jfp18.pdf
[space-efficient]: https://link.springer.com/article/10.1007/s10990-011-9066-z
[chaperones]: https://www2.ccs.neu.edu/racket/pubs/NU-CCIS-12-01.pdf
[wadler-findler]: https://users.cs.northwestern.edu/~robby/pubs/papers/esop2009-wf.pdf
[blame-effectiveness]: https://dl.acm.org/doi/10.1145/3473573
[specs]: /Users/chris/src/chrisbarrett/emacs-tart/specs/03-type-system-design.md
