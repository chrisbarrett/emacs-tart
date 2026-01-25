# Flow and Hack: Typed Layers over Dynamic Languages

Research notes on Flow (JavaScript) and Hack (PHP) as examples of retrofitting
static type systems onto dynamically typed languages.

## 1. Flow (JavaScript)

### 1.1 Architecture

Flow is a static type checker for JavaScript, developed by Meta (Facebook) and
implemented in OCaml.

```
+------------------+     +------------------+     +------------------+
|   Source Files   |     |   Flow Server    |     |   IDE/Editor     |
|   (.js + @flow)  |---->|   (persistent)   |<--->|   (LSP client)   |
+------------------+     +------------------+     +------------------+
                               |
                               v
                         +------------------+
                         |  Shared Memory   |
                         |  (type cache)    |
                         +------------------+
```

Key architectural decisions:

1. **Persistent server**: Flow runs as a background daemon that watches the
   filesystem. The server maintains semantic information about the entire
   codebase in memory, enabling sub-second response times even on millions of
   lines of code.

2. **Modular analysis**: Type checking is performed modularly at file
   boundaries. This enables aggressive parallelisation and incremental
   rechecking.

3. **Separate from transpilation**: Unlike TypeScript, Flow only performs type
   checking. Removal of type annotations is delegated to Babel or
   `flow-remove-types`.

4. **OCaml implementation**: The type checker is written in OCaml, providing
   strong guarantees about the checker's own correctness and enabling
   sophisticated type-theoretic algorithms.

### 1.2 Type Inference Approach

Flow uses a constraint-based inference system that tracks types as they "flow"
through the program.

```
// Flow infers x: number from usage
function double(x) {
  return x * 2;  // multiplication implies number
}

// Flow tracks narrowing through control flow
function process(x: ?string) {
  if (x != null) {
    return x.length;  // x refined to string here
  }
  return 0;
}
```

**Inference philosophy:**

- Flow assumes most JavaScript is *implicitly statically typed*: developers have
  types in mind even when not written
- The checker infers types automatically where possible
- Interprocedural analysis enables finding bugs without annotations

**Evolution to "Types-First":**

Flow's original inference was powerful but caused scaling problems. The
"types-first" architecture (now mandatory) requires explicit type annotations at
module boundaries:

```javascript
// Module boundary must be explicitly typed
export function processUser(user: User): Result { ... }

// Internal code can still use inference
function helper(x) { return x + 1; }
```

This trade-off sacrifices some inference power for:
- 6x faster rechecking (p90)
- Better parallelisation
- More predictable error messages

### 1.3 Gradual Typing and Any Types

Flow implements gradual typing via opt-in file checking and the `any` type.

**File-level opt-in:**

```javascript
// @flow         - Enable type checking
// @flow strict  - Enable + ban any/Object/Function types
// @noflow       - Parse as Flow but suppress all errors
// (no pragma)   - File ignored entirely
```

**The `any` type as escape hatch:**

```javascript
// any disables type checking for this value
const config: any = loadConfig();
config.whatever.you.want;  // No errors

// mixed is the safe "unknown" type
function process(x: mixed) {
  if (typeof x === 'string') {
    return x.length;  // Must refine before use
  }
}
```

**Strict mode hierarchy:**

| Mode            | Requires annotations | Allows `any` | Requires typed deps |
|-----------------|---------------------|--------------|---------------------|
| `@flow`         | At boundaries       | Yes          | No                  |
| `@flow strict-local` | Yes            | No           | No                  |
| `@flow strict`  | Yes                 | No           | Yes                 |

### 1.4 Flow vs TypeScript Design Differences

| Aspect | Flow | TypeScript |
|--------|------|------------|
| **Primary goal** | Soundness | Pragmatism/productivity |
| **Implementation** | Type checker only | Checker + transpiler |
| **Inference** | More aggressive, interprocedural | More conservative, local |
| **Class typing** | Nominal (classes are distinct types) | Structural (shape-based) |
| **Object types** | Open by default (`{name: string}` accepts extra props) | Closed by default |
| **Variance** | Explicit annotations supported | Mostly inferred |
| **any handling** | Strict modes ban it | Always allowed |
| **Ecosystem** | Meta-focused | Broad community |

**Soundness vs pragmatism:**

TypeScript's design goals explicitly state: "Apply a sound or 'provably correct'
type system" is a *non-goal*. Instead, TypeScript prioritises working with
existing JavaScript patterns.

Flow historically prioritised soundness, though practical considerations have
led to compromises. The team acknowledges: "this change may make Flow less
suitable for some projects."

### 1.5 Error Message Strategies

Flow's error messages evolved significantly:

**Early approach (problems):**

- Complex inferred types led to confusing errors
- Error suppression could hide more than intended
- Type inference bugs caused unreliable errors

**Current approach:**

- Types-first reduces inference complexity, improving error clarity
- Explicit boundary annotations mean errors point to authored code
- Local type inference makes errors more predictable

**Error design principles learned:**

1. Simpler inferred types lead to better errors
2. Requiring annotations at key points grounds errors in developer intent
3. Incremental adoption needs clear "you need to annotate this" guidance

## 2. Hack (PHP)

### 2.1 Retrofitting Types onto PHP

Hack is a programming language for HHVM (HipHop Virtual Machine), created by
Meta as a gradually typed dialect of PHP.

```
+------------------+     +------------------+     +------------------+
|   Source Files   |     |  Type Checker    |     |      HHVM        |
|   (.hack/.php)   |---->|   (hh_server)    |     |   (runtime)      |
+------------------+     +------------------+     +------------------+
        |                       |                        |
        |                       v                        v
        |                 +------------+          +------------+
        +---------------->| Static     |          | Runtime    |
                          | Checking   |          | Enforcement|
                          +------------+          +------------+
```

**Key architectural decisions:**

1. **Same server architecture as Flow**: Type checker runs as a persistent local
   server watching the filesystem, responding in <200ms typically.

2. **Runtime enforcement**: Unlike Flow/TypeScript, Hack enforces types at
   runtime. This provides safety during gradual migration.

3. **Language evolution**: Hack started as PHP-compatible but has diverged into
   its own language with features PHP lacks (generics, async/await, shapes).

### 2.2 Gradual Typing Approach

Hack's gradual typing operates at the file level through typing modes:

```hack
<?hh // strict
// All types required, no untyped code calls allowed

<?hh // partial (or just <?hh)
// Types checked where present, untyped calls allowed

<?hh // decl
// Signatures absorbed for strict callers, body not checked
```

**Mode characteristics:**

| Mode | Annotations | Call untyped code | Top-level code |
|------|-------------|-------------------|----------------|
| strict | Required everywhere | No | No |
| partial | Optional | Yes | Yes |
| decl | Required on signatures | Yes | Yes |

**The `UNSAFE` escape hatch:**

```hack
// UNSAFE disables checking until block ends
/* UNSAFE_EXPR */
$result = legacy_function($data);
```

### 2.3 Migration Path from Untyped Code

Hack's migration strategy was designed for Facebook's hundreds of millions of
lines of PHP.

**Gradual migration approach:**

1. **Start in partial mode**: Files begin as `<?hh` (partial), treating untyped
   values as `dynamic`/`any`

2. **Add types incrementally**: Annotate functions and classes as time permits

3. **Promote to strict**: Once a file is fully typed, change to `<?hh //
   strict`

4. **Use decl for legacy bridges**: Wrap old PHP APIs in decl-mode files to
   expose typed signatures

**Slack's migration experience:**

Slack migrated their multi-million line PHP codebase to Hack:

- Started with all files in partial mode
- Added types over time, promoting to strict as files became fully typed
- Observed that gradual typing made developers *more thoughtful* about types
  than mandatory typing in Java/Go
- Types served as verifiable documentation

**Facebook's results:**

- Migrated nearly entire PHP codebase to Hack
- Used homegrown refactoring tools for bulk annotation
- Type checker performance: typically <200ms, rarely >1 second

### 2.4 Runtime vs Static Checking

Hack uniquely combines static and runtime type enforcement.

**Static checking:**

```hack
function greet(string $name): string {
  return "Hello, " . $name;
}

greet(42);  // Static error: expected string, got int
```

**Runtime enforcement:**

```hack
// Even if static checker is bypassed (via partial mode or UNSAFE),
// HHVM enforces types at runtime

function process(int $x): int {
  return $x * 2;
}

// At runtime: TypeError if $x is not an int
```

**Benefits of runtime enforcement:**

1. **Safety net during migration**: Mixed typed/untyped code still has
   guarantees at boundaries

2. **JIT optimisation**: HHVM's JIT can trust type annotations for
   optimisation

3. **Debugging**: Runtime type errors are caught close to the source

**The `is` and `as` operators:**

```hack
// Type refinement the checker understands
if ($x is int) {
  return $x * 2;  // Checker knows $x is int
}

// Type assertion (throws on failure)
$y = $x as string;
return $y.length();
```

## 3. Common Patterns

### 3.1 Handling Dynamic Features

Both Flow and Hack must handle languages designed for dynamic use.

**Dynamic property access:**

```javascript
// Flow: Use mapped/indexed types
type StringDict = { [key: string]: number };

// Hack: Use shapes for known keys
type UserShape = shape('name' => string, 'age' => int);
```

**Metaprogramming:**

Both languages treat heavy metaprogramming as an escape hatch use case:
- Flow: Use `any` or `Object`
- Hack: Use `dynamic` type or `UNSAFE`

**Duck typing patterns:**

```javascript
// Flow: Structural object types (open by default)
function greet(obj: { name: string }) {
  return `Hello ${obj.name}`;
}
greet({ name: "Alice", age: 30 });  // OK, extra props allowed

// TypeScript equivalent requires explicit index signature
```

### 3.2 Signature File Patterns

**Flow libdefs:**

```javascript
// flow-typed/lodash.js.flow
declare module 'lodash' {
  declare function map<T, U>(
    array: Array<T>,
    fn: (T) => U
  ): Array<U>;
}
```

Location hierarchy:
1. `@flowtyped/` directory (since v0.251)
2. `flow-typed/` directory
3. `node_modules` (if library includes `.js.flow` files)

**Hack HHI files:**

Hack uses `.hhi` (Hack Header Interface) files for builtin and library type
signatures:

```hack
// Located in hphp/hack/hhi/
// Provides types for builtin functions

function array_map<Tv1, Tv2>(
  (function(Tv1): Tv2) $callback,
  Container<Tv1> $arr,
): vec<Tv2>;
```

**Common patterns:**

1. **Separate declaration files**: Keep signatures separate from
   implementation for external libraries

2. **Community repositories**: `flow-typed` repo provides community-maintained
   definitions

3. **Fallback to any**: Missing definitions default to untyped rather than
   error

### 3.3 IDE/LSP Integration Approaches

Both tools use similar integration architectures:

```
+------------------+     +------------------+     +------------------+
|      Editor      |<--->|   LSP Client     |<--->|  Type Server     |
|  (VSCode, etc)   |     |   (extension)    |     |  (Flow/hh_server)|
+------------------+     +------------------+     +------------------+
                                |
                                v
                         JSON-RPC over stdio/socket
```

**Integration features:**

| Feature | Flow | Hack |
|---------|------|------|
| Error reporting | Real-time via server | Real-time via server |
| Hover types | Yes | Yes |
| Go to definition | Yes | Yes |
| Find references | Yes | Yes |
| Autocomplete | Yes | Yes |
| Refactoring | Limited | Yes |

**Performance strategy:**

Both use the persistent server model:
1. Initial startup: Full codebase analysis (can be slow)
2. Incremental: Only re-analyse changed files + dependents
3. Caching: Keep type information in memory

### 3.4 Opt-in Mechanisms

**Flow:**

```javascript
// File-level opt-in via pragma comment
// @flow

// Strict variants
// @flow strict
// @flow strict-local
```

**Hack:**

```hack
// Mode declaration after opening tag
<?hh // strict
<?hh // partial
<?hh // decl

// File extension (.hack assumes strict)
```

**Comparison:**

| Mechanism | Flow | Hack |
|-----------|------|------|
| Default behaviour | Ignore file | Partial mode |
| Strict opt-in | `// @flow strict` | `// strict` mode |
| Per-expression escape | Cast to `any` | `UNSAFE` comments |
| Dependency requirements | Optional in strict | Required in strict |

## 4. Key Design Decisions

### 4.1 What Worked

**Persistent server architecture (both):**
- Sub-second response times on large codebases
- Enables IDE-quality experience
- Incremental checking scales well

**Gradual typing with file-level control:**
- Enables incremental adoption
- Teams can migrate at their own pace
- Partial typing still catches bugs

**Explicit module boundary types (Flow types-first):**
- 6x performance improvement
- Better error messages
- More predictable behaviour

**Runtime enforcement (Hack):**
- Safety net during migration
- Enables JIT optimisation
- Catches errors close to source

### 4.2 What Did Not Work

**Over-aggressive inference (Flow original):**
- Complex inferred types caused confusing errors
- Scaling problems as codebase grew
- Required architectural rewrite (types-first)

**Divergence from host language (Flow):**
- Flow has become "not just JavaScript with types"
- Reduced adoption outside Meta
- TypeScript's pragmatism won the ecosystem

**Partial mode as default (Hack):**
- Too easy to leave code untyped
- Slack recommends starting strict for new code

### 4.3 Lessons for Elisp Type System

**Architecture:**

1. **Use persistent server model**: Essential for IDE-quality experience.
   Emacs has native support for background processes.

2. **Modular analysis at file/feature boundaries**: Elisp packages have natural
   boundaries. Type annotations at `provide`/`require` boundaries enable
   parallel checking.

3. **Incremental from the start**: Design for incremental rechecking. Full
   reanalysis won't scale.

**Gradual typing:**

1. **File-level opt-in is essential**: Elisp ecosystem is vast and untyped.
   Must support mixing typed and untyped code.

2. **Multiple strictness levels**: Offer partial (check what's annotated) and
   strict (require all annotations) modes.

3. **Runtime checking optional but valuable**: Elisp has `cl-check-type`.
   Optional runtime enforcement aids migration and debugging.

**Inference:**

1. **Require annotations at boundaries**: Learn from Flow's types-first
   evolution. Require signatures at defun, defvar, provide boundaries.

2. **Local inference only**: Infer types within function bodies, but don't try
   to infer across module boundaries.

3. **Predictable over powerful**: Simple inference with clear errors beats
   complex inference with confusing errors.

**Ecosystem:**

1. **Declaration files for builtins**: Need comprehensive type signatures for
   Emacs primitives and core packages.

2. **Community contribution model**: `flow-typed` model works well. Central
   repo for package type definitions.

3. **Graceful degradation**: Missing types should degrade to `any`/`dynamic`,
   not hard errors.

**Dynamic features:**

1. **Escape hatches are necessary**: Elisp macros and advice are too dynamic
   for full typing. Need `any` type or similar.

2. **Document the boundary**: Clear guidance on what can/cannot be typed helps
   set expectations.

3. **Occurrence typing for refinement**: Both Flow and Hack use control-flow
   analysis for type narrowing. Essential for predicate-heavy Lisp idioms.

## References

- [Flow Documentation](https://flow.org/)
- [Types-First Architecture](https://medium.com/flow-type/types-first-a-scalable-new-architecture-for-flow-3d8c7ba1d4eb)
- [Local Type Inference for Flow](https://medium.com/flow-type/introducing-local-type-inference-for-flow-6af65b7830aa)
- [Hack Documentation](https://docs.hhvm.com/hack/)
- [Hack: a new programming language for HHVM](https://engineering.fb.com/2014/03/20/developer-tools/hack-a-new-programming-language-for-hhvm/)
- [Hacklang at Slack](https://slack.engineering/hacklang-at-slack-a-better-php/)
- [Flow vs TypeScript comparison](https://github.com/niieani/typescript-vs-flowtype)
- [Gradual Typing (Wikipedia)](https://en.wikipedia.org/wiki/Gradual_typing)
- [The Seven Sources of Unsoundness in TypeScript](https://effectivetypescript.com/2021/05/06/unsoundness/)
