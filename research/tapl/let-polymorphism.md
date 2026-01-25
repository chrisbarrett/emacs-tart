# Let-Polymorphism (ML-Style Polymorphism)

Quick reference for Hindley-Milner let-polymorphism (TAPL Chapter 22).

---

## 1. The Problem

In simply-typed lambda calculus, the identity function must have a single type:

```
id : Int → Int       -- can only be used with Int
id : Bool → Bool     -- or only with Bool
```

We want `id` to work at *any* type, but **without** requiring explicit type
abstractions (as in System F).

**The tension:** We need polymorphism, but full System F type inference is
undecidable. Let-polymorphism provides a sweet spot: decidable inference with
useful polymorphism.

---

## 2. Syntax

Types are stratified into *monotypes* (τ) and *polytypes/type schemes* (σ):

```
τ ::= α              -- type variable
    | τ → τ          -- function type
    | T              -- base types (Int, Bool, etc.)

σ ::= τ              -- monotype
    | ∀α. σ          -- universally quantified type
```

Expressions include let-bindings:

```
e ::= x              -- variable
    | λx. e          -- abstraction
    | e e            -- application
    | let x = e in e -- let binding
```

---

## 3. Generalization

**When to generalize:** At `let`-bindings only—never at lambda abstractions.

**How to generalize:** Given context Γ and type τ, generalize free variables in
τ that are *not* free in Γ:

```
Gen(Γ, τ) = ∀α₁...αₙ. τ
  where {α₁, ..., αₙ} = FV(τ) \ FV(Γ)
```

**Example:**

```
Γ = {y : Int}
τ = α → α

FV(τ) = {α}
FV(Γ) = ∅
Gen(Γ, τ) = ∀α. α → α
```

**Key insight:** Variables free in Γ represent "ambient" type constraints that
must not be generalized away—they may be unified later.

---

## 4. Instantiation

When a polymorphic variable is used, its type scheme is *instantiated* with
fresh type variables:

```
Inst(∀α₁...αₙ. τ) = [α₁ ↦ β₁, ..., αₙ ↦ βₙ]τ
  where β₁, ..., βₙ are fresh
```

Each use can instantiate to a different type:

```
let id = λx. x in
  (id 42, id true)
-- First use: id : Int → Int
-- Second use: id : Bool → Bool
```

---

## 5. Typing Rules

### Core Rules (Damas-Milner / Algorithm W)

**Variable (with instantiation):**
```
    x : σ ∈ Γ      τ = Inst(σ)
    ─────────────────────────────
           Γ ⊢ x : τ
```

**Abstraction:**
```
       Γ, x : τ₁ ⊢ e : τ₂
    ────────────────────────
      Γ ⊢ λx. e : τ₁ → τ₂
```

**Application:**
```
    Γ ⊢ e₁ : τ₁ → τ₂      Γ ⊢ e₂ : τ₁
    ────────────────────────────────────
              Γ ⊢ e₁ e₂ : τ₂
```

**Let (with generalization):**
```
    Γ ⊢ e₁ : τ      σ = Gen(Γ, τ)      Γ, x : σ ⊢ e₂ : τ'
    ──────────────────────────────────────────────────────
                  Γ ⊢ let x = e₁ in e₂ : τ'
```

---

## 6. Why Let ≠ Application

The expressions `let x = e₁ in e₂` and `(λx. e₂) e₁` are **not** equivalent in
a polymorphic setting:

| `let x = e₁ in e₂`                  | `(λx. e₂) e₁`                   |
|-------------------------------------|---------------------------------|
| Type `e₁` first, then generalize    | Must pick single type for `x`   |
| `x` gets a type *scheme*            | `x` gets a *monotype*           |
| Each use of `x` can instantiate differently | All uses share same type |

**Example:**

```
-- This works:
let id = λx. x in (id 1, id true)    -- id : ∀α. α → α

-- This fails:
(λid. (id 1, id true)) (λx. x)       -- id : τ → τ for some fixed τ
```

**The key insight:** Let-polymorphism types the definition *before* the body,
allowing generalization. Lambda abstractions must handle their argument
polymorphically, requiring the argument's type be fixed.

---

## 7. Value Restriction

### The Problem

Generalizing references (or other effects) breaks soundness:

```
let r = ref [] in        -- Naively: r : ∀α. ref (list α)
  r := [1];              -- Instantiate as ref (list Int)
  hd (!r)                -- Instantiate as ref (list Bool) → boom!
```

### Solutions

**1. Value Restriction (SML, OCaml):**

Only generalize if `e₁` is a *syntactic value* (lambda, constant, variable):

```
    Γ ⊢ e₁ : τ      σ = if is_value(e₁) then Gen(Γ, τ) else τ
    ─────────────────────────────────────────────────────────
                  Γ ⊢ let x = e₁ in e₂ : τ'
```

Values: `λx. e`, `x`, `n`, `true`, `false`, `(v₁, v₂)`, `C v`, ...
Non-values: `e₁ e₂`, `ref e`, `!e`, `e₁ := e₂`, ...

**2. Relaxed Value Restriction (OCaml):**

Generalize non-values at *covariant* positions only (variables appearing only
in positive positions).

**3. Effect Systems:**

Track effects and only generalize pure expressions.

---

## 8. Worked Examples

### Example 1: Identity Function

```
Infer: let id = λx. x in id 42

1. Infer λx. x:
   - Assume x : α (fresh)
   - Body x : α
   - Result: α → α

2. Generalize (Γ = ∅):
   - FV(α → α) \ FV(∅) = {α}
   - id : ∀α. α → α

3. Infer id 42:
   - Instantiate id : β → β (fresh β)
   - Infer 42 : Int
   - Unify β = Int
   - Result: Int

Final: Int
```

### Example 2: Polymorphic Pairing

```
Infer: let f = λx. (x, x) in (f 1, f true)

1. Infer λx. (x, x):
   - Assume x : α
   - Body: (α, α)
   - Result: α → (α, α)

2. Generalize: f : ∀α. α → (α, α)

3. Infer (f 1, f true):
   - f 1: Instantiate f : β → (β, β), unify β = Int → (Int, Int)
   - f true: Instantiate f : γ → (γ, γ), unify γ = Bool → (Bool, Bool)
   - Result: ((Int, Int), (Bool, Bool))
```

### Example 3: Escaping Type Variable (No Generalization)

```
Infer: λy. let x = y in x

1. Assume y : α

2. Infer y : α (from context)

3. Generalize with Γ = {y : α}:
   - FV(α) \ FV({y : α}) = {α} \ {α} = ∅
   - x : α (NOT generalized—α escapes via y)

4. Infer x : α

5. Result: α → α
```

---

## 9. Algorithm W (Sketch)

```
W(Γ, x) =
  let σ = lookup(x, Γ)
  (∅, Inst(σ))

W(Γ, λx. e) =
  let α = fresh()
  let (S, τ) = W(Γ ∪ {x : α}, e)
  (S, S(α) → τ)

W(Γ, e₁ e₂) =
  let (S₁, τ₁) = W(Γ, e₁)
  let (S₂, τ₂) = W(S₁(Γ), e₂)
  let α = fresh()
  let S₃ = unify(S₂(τ₁), τ₂ → α)
  (S₃ ∘ S₂ ∘ S₁, S₃(α))

W(Γ, let x = e₁ in e₂) =
  let (S₁, τ₁) = W(Γ, e₁)
  let σ = Gen(S₁(Γ), τ₁)
  let (S₂, τ₂) = W(S₁(Γ) ∪ {x : σ}, e₂)
  (S₂ ∘ S₁, τ₂)
```

---

## 10. Key Implementation Notes

1. **Fresh variables:** Use a counter or supply monad to generate unique names.

2. **Substitution composition:** `(S₂ ∘ S₁)(τ) = S₂(S₁(τ))`.

3. **Occurs check:** In unification, reject `α = τ` if α ∈ FV(τ) (prevents
   infinite types).

4. **Generalization timing:** Apply pending substitutions to Γ before
   computing FV(Γ).

5. **Let-generalization levels:** For efficiency, use "levels" instead of
   computing FV(Γ) directly (see "Efficient ML Type Inference Using Ranked
   Type Variables" by Kuan and MacQueen).

---

## References

- Pierce, *Types and Programming Languages*, Chapter 22
- Damas & Milner, "Principal type-schemes for functional programs" (1982)
- Milner, "A Theory of Type Polymorphism in Programming" (1978)
- Wright & Felleisen, "A Syntactic Approach to Type Soundness" (1994) — value restriction
