# Unification: Quick Reference

## Problem Statement

**Unification** finds a substitution that makes two terms syntactically identical.

Given terms `s` and `t`, find substitution `σ` such that `σ(s) = σ(t)`.

```
Example: unify(f(X, b), f(a, Y))
Solution: σ = {X ↦ a, Y ↦ b}
Check: σ(f(X, b)) = f(a, b) = σ(f(a, Y)) ✓
```

**Applications**: Type inference, logic programming, theorem proving, pattern matching.

---

## Substitutions

### Definition

A **substitution** `σ` is a finite mapping from variables to terms:

```
σ = {X₁ ↦ t₁, X₂ ↦ t₂, ..., Xₙ ↦ tₙ}
```

where each `Xᵢ` is a variable and each `tᵢ` is a term.

### Application

Apply substitution `σ` to term `t`, written `σ(t)` or `t[σ]`:

```
σ(X)           = σ(X) if X ∈ dom(σ), else X
σ(c)           = c                           (constants)
σ(f(t₁,...,tₙ)) = f(σ(t₁),...,σ(tₙ))         (compound terms)
```

### Composition

Composition `σ ∘ τ` applies `τ` first, then `σ`:

```
(σ ∘ τ)(t) = σ(τ(t))
```

**Computation**: Given `σ = {X₁ ↦ s₁, ...}` and `τ = {Y₁ ↦ t₁, ...}`:

```
σ ∘ τ = {Y₁ ↦ σ(t₁), ...} ∪ {X₁ ↦ s₁, ... | Xᵢ ∉ dom(τ)}
```

**Example**:
```
σ = {X ↦ f(Y)}
τ = {Y ↦ a, Z ↦ X}
σ ∘ τ = {Y ↦ a, Z ↦ f(Y), X ↦ f(Y)}

Check: (σ ∘ τ)(Z) = σ(τ(Z)) = σ(X) = f(Y) ✓
```

---

## Most General Unifier (MGU)

### Definition

Substitution `σ` is a **unifier** of `s` and `t` if `σ(s) = σ(t)`.

Unifier `σ` is **most general** if for every other unifier `θ`, there exists `ρ` such that `θ = ρ ∘ σ`.

### Properties

1. **Uniqueness**: MGU is unique up to variable renaming
2. **Idempotence**: `σ ∘ σ = σ` for MGU `σ`
3. **Minimality**: MGU introduces no unnecessary bindings

**Example**:
```
Terms: f(X, X) and f(Y, a)
MGU: σ = {X ↦ a, Y ↦ a}

Other unifier: θ = {X ↦ a, Y ↦ a, Z ↦ b}
θ = {Z ↦ b} ∘ σ ✓
```

---

## Robinson's Unification Algorithm

### Pseudocode

```
unify(s, t) → substitution or FAIL

unify(s, t):
    S = [(s, t)]           // stack of equations
    σ = {}                  // accumulated substitution

    while S ≠ []:
        (s', t') = pop(S)
        s' = σ(s')          // apply current substitution
        t' = σ(t')

        if s' == t':
            continue        // already equal

        else if s' is variable X:
            if X occurs in t':
                return FAIL  // occurs check
            σ = {X ↦ t'} ∘ σ

        else if t' is variable X:
            if X occurs in s':
                return FAIL  // occurs check
            σ = {X ↦ s'} ∘ σ

        else if s' = f(s₁,...,sₙ) and t' = f(t₁,...,tₙ):
            // same functor and arity
            push (s₁, t₁), ..., (sₙ, tₙ) onto S

        else:
            return FAIL      // structural mismatch

    return σ
```

### Alternative: Recursive Formulation

```
unify(s, t):
    if s == t:
        return {}
    if s is variable X:
        return unify_var(X, t)
    if t is variable X:
        return unify_var(X, s)
    if s = f(s₁,...,sₙ) and t = f(t₁,...,tₙ):
        σ = {}
        for i = 1 to n:
            σ' = unify(σ(sᵢ), σ(tᵢ))
            if σ' == FAIL: return FAIL
            σ = σ' ∘ σ
        return σ
    return FAIL

unify_var(X, t):
    if X occurs in t:
        return FAIL
    return {X ↦ t}
```

---

## Occurs Check

### Purpose

The **occurs check** prevents constructing infinite terms.

### Example of Failure Without Occurs Check

```
unify(X, f(X))

Without occurs check:
  σ = {X ↦ f(X)}
  σ(X) = f(X)
  σ(f(X)) = f(f(X))

These are NOT equal! X would need to be:
  X = f(f(f(f(f(...)))))  // infinite term
```

### Consequences of Omitting

| Context | Effect |
|---------|--------|
| Prolog (traditional) | Omitted for speed; unsound but often works |
| Type inference | Must include; omission causes infinite types |
| Theorem provers | Must include; soundness critical |

### Implementation Note

```
occurs(X, t):
    if t is variable Y:
        return X == Y
    if t is constant:
        return false
    if t = f(t₁,...,tₙ):
        return any(occurs(X, tᵢ) for i in 1..n)
```

---

## Complexity

### Time Complexity

| Algorithm | Complexity | Notes |
|-----------|------------|-------|
| Robinson's (naive) | O(2ⁿ) | Exponential in term size |
| With memoization | O(n²) | Quadratic |
| Union-find based | O(n α(n)) | Near-linear; α is inverse Ackermann |

### Space Complexity

| Representation | Space |
|----------------|-------|
| Explicit substitution | O(n²) | Can grow quadratically |
| Union-find (in-place) | O(n) | Linear in term size |

### Why Robinson's is Exponential

```
Terms: f(X₁, X₁, ..., X₁)  and  f(X₂, X₃, ..., Xₙ₊₁)
           \___ n ___/

After unification, applying σ to any variable produces
a term that doubles in size at each step.
```

---

## Common Implementation Patterns

### 1. Union-Find (Disjoint Set)

**Key idea**: Represent equivalence classes of terms; unification merges classes.

```
Structure:
    parent[node] → node     // path to representative
    rank[node] → int        // for union by rank

find(x):
    if parent[x] ≠ x:
        parent[x] = find(parent[x])  // path compression
    return parent[x]

union(x, y):
    rx, ry = find(x), find(y)
    if rx == ry: return
    if rank[rx] < rank[ry]: swap(rx, ry)
    parent[ry] = rx
    if rank[rx] == rank[ry]: rank[rx]++
```

**For unification**: Nodes are term cells. Union merges variable with term.

### 2. Triangular Substitution

Store substitutions as a list; lookup walks the chain.

```
σ = [(X, f(Y)), (Y, a)]

lookup(X, σ):
    for (V, t) in σ:
        if V == X:
            return apply(σ, t)  // chase further
    return X
```

**Pros**: Simple, append-only (good for backtracking)
**Cons**: O(n) lookup, O(n²) total

### 3. Explicit Substitution with Sharing

Use hash-consing to share structure:

```
Terms stored as DAG (directed acyclic graph)
Each unique term created only once
Substitution application creates new shared nodes
```

**Pros**: Memory efficient for large terms with repetition
**Cons**: Overhead of hash-consing

### 4. Mutable vs Immutable

| Style | Unification | Backtracking |
|-------|-------------|--------------|
| Mutable (union-find) | Fast, in-place | Requires trail/undo stack |
| Immutable | Copy on modify | Natural (keep old substitution) |

---

## Worked Examples

### Example 1: Simple Unification

```
unify(f(X, g(Y)), f(a, g(b)))

Stack: [(f(X, g(Y)), f(a, g(b)))]
σ = {}

Step 1: Pop (f(X, g(Y)), f(a, g(b)))
        Same functor f/2
        Push: [(X, a), (g(Y), g(b))]

Step 2: Pop (X, a)
        X is variable, a is constant
        σ = {X ↦ a}

Step 3: Pop (g(Y), g(b))
        Same functor g/1
        Push: [(Y, b)]

Step 4: Pop (Y, b)
        Y is variable, b is constant
        σ = {X ↦ a, Y ↦ b}

Stack empty. Return σ = {X ↦ a, Y ↦ b}
```

### Example 2: Variable-Variable

```
unify(f(X, Y), f(Y, a))

Step 1: Push [(X, Y), (Y, a)]
        σ = {}

Step 2: Pop (X, Y)
        Both variables
        σ = {X ↦ Y}

Step 3: Pop (Y, a)
        Apply σ: (Y, a) → (Y, a)  // Y not in σ yet
        σ = {X ↦ Y, Y ↦ a}

Final: σ = {X ↦ Y, Y ↦ a}

Verify: σ(f(X, Y)) = f(Y, a) = σ(f(Y, a)) ✓

Note: Fully resolved, σ(X) = σ(Y) = a
```

### Example 3: Occurs Check Failure

```
unify(X, f(X))

Step 1: X is variable, t = f(X)
        occurs(X, f(X)) = true
        FAIL

No finite term t satisfies X = f(t).
```

### Example 4: Structural Mismatch

```
unify(f(a, X), g(a, b))

Step 1: f ≠ g (different functors)
        FAIL
```

### Example 5: Arity Mismatch

```
unify(f(a), f(a, b))

Step 1: f/1 ≠ f/2 (different arities)
        FAIL
```

### Example 6: Complex (Type Inference Style)

```
unify((α → β) → α, (γ → δ) → γ)

Using → as binary constructor:

Step 1: Same functor →/2
        Push: [((α → β), (γ → δ)), (α, γ)]

Step 2: Pop (α, γ)
        σ = {α ↦ γ}

Step 3: Pop ((α → β), (γ → δ))
        Apply σ: ((γ → β), (γ → δ))
        Same functor →/2
        Push: [(γ, γ), (β, δ)]

Step 4: Pop (γ, γ)
        Equal, continue

Step 5: Pop (β, δ)
        σ = {α ↦ γ, β ↦ δ}

Final: σ = {α ↦ γ, β ↦ δ}
```

---

## References

- Robinson, J.A. (1965). "A Machine-Oriented Logic Based on the Resolution Principle"
- Martelli, A. & Montanari, U. (1982). "An Efficient Unification Algorithm"
- Paterson, M.S. & Wegman, M.N. (1978). "Linear Unification" (near-linear algorithm)
- Pierce, B.C. *Types and Programming Languages*, Ch. 22
