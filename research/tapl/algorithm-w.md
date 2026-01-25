# Algorithm W Quick Reference

Hindley-Milner type inference for the lambda calculus with let-polymorphism.

## Syntax

```
Types       τ ::= α                     type variable
              |   τ → τ                 function type
              |   T τ₁ ... τₙ           type constructor

Type Schemes σ ::= τ                    monotype
              |   ∀α. σ                 polymorphic type

Expressions  e ::= x                    variable
              |   λx. e                 abstraction
              |   e₁ e₂                 application
              |   let x = e₁ in e₂      let binding

Contexts     Γ ::= ∅                    empty context
              |   Γ, x : σ              type binding
```

## Typing Rules

### Variables

```
        x : σ ∈ Γ    τ = inst(σ)
        ─────────────────────────── [Var]
              Γ ⊢ x : τ
```

### Abstraction

```
            Γ, x : τ ⊢ e : τ'
        ─────────────────────────── [Abs]
          Γ ⊢ (λx. e) : τ → τ'
```

### Application

```
        Γ ⊢ e₁ : τ → τ'    Γ ⊢ e₂ : τ
        ─────────────────────────────── [App]
              Γ ⊢ e₁ e₂ : τ'
```

### Let (with generalization)

```
        Γ ⊢ e₁ : τ    Γ, x : gen(Γ, τ) ⊢ e₂ : τ'
        ───────────────────────────────────────── [Let]
              Γ ⊢ let x = e₁ in e₂ : τ'
```

## Key Operations

### Free Type Variables

```
ftv(α)       = {α}
ftv(τ₁ → τ₂) = ftv(τ₁) ∪ ftv(τ₂)
ftv(∀α. σ)   = ftv(σ) \ {α}
ftv(Γ)       = ∪ { ftv(σ) | x : σ ∈ Γ }
```

### Instantiation

```
inst(∀α₁...αₙ. τ) = [α₁ ↦ β₁, ..., αₙ ↦ βₙ]τ
                    where β₁...βₙ are fresh
```

### Generalization

```
gen(Γ, τ) = ∀α₁...αₙ. τ
            where {α₁...αₙ} = ftv(τ) \ ftv(Γ)
```

## Algorithm W

```
W(Γ, e) → (S, τ)   where S is a substitution, τ is a type

W(Γ, x) =
    if x : σ ∈ Γ then
        return (∅, inst(σ))
    else
        error "unbound variable"

W(Γ, λx. e) =
    let β = fresh type variable
    let (S, τ) = W(Γ ∪ {x : β}, e)
    return (S, S(β) → τ)

W(Γ, e₁ e₂) =
    let (S₁, τ₁) = W(Γ, e₁)
    let (S₂, τ₂) = W(S₁(Γ), e₂)
    let β = fresh type variable
    let S₃ = unify(S₂(τ₁), τ₂ → β)
    return (S₃ ∘ S₂ ∘ S₁, S₃(β))

W(Γ, let x = e₁ in e₂) =
    let (S₁, τ₁) = W(Γ, e₁)
    let σ = gen(S₁(Γ), τ₁)
    let (S₂, τ₂) = W(S₁(Γ) ∪ {x : σ}, e₂)
    return (S₂ ∘ S₁, τ₂)
```

## Unification (Robinson's Algorithm)

```
unify(τ, τ') → S   where S is the most general unifier (MGU)

unify(α, τ) =
    if α = τ then
        return ∅
    else if α ∈ ftv(τ) then
        error "occurs check: infinite type"
    else
        return [α ↦ τ]

unify(τ, α) =
    return unify(α, τ)

unify(τ₁ → τ₂, τ₁' → τ₂') =
    let S₁ = unify(τ₁, τ₁')
    let S₂ = unify(S₁(τ₂), S₁(τ₂'))
    return S₂ ∘ S₁

unify(T τ₁...τₙ, T τ₁'...τₙ') =
    let S₁ = unify(τ₁, τ₁')
    let S₂ = unify(S₁(τ₂), S₁(τ₂'))
    ...
    let Sₙ = unify(Sₙ₋₁(...(S₁(τₙ))...), Sₙ₋₁(...(S₁(τₙ'))...))
    return Sₙ ∘ ... ∘ S₁

unify(τ, τ') =
    error "type mismatch"
```

### Substitution Composition

```
(S₂ ∘ S₁)(τ) = S₂(S₁(τ))
```

## Key Properties

| Property | Statement |
|----------|-----------|
| **Soundness** | If `W(Γ, e) = (S, τ)` then `S(Γ) ⊢ e : τ` |
| **Completeness** | If `Γ ⊢ e : τ` then `W(Γ, e)` succeeds with `(S, τ')` where `τ` is an instance of `τ'` |
| **Principality** | The type returned by W is the most general (principal) type |
| **Decidability** | W always terminates |
| **Complexity** | Exponential worst-case (rare); nearly linear in practice |

### The Occurs Check

Required to prevent infinite types:
- `unify(α, α → α)` would give `α = α → α = (α → α) → (α → α) = ...`
- The occurs check detects `α ∈ ftv(α → α)` and rejects

## Worked Examples

### Example 1: Identity Function

```
W(∅, λx. x)

1. β = fresh                           -- for x
2. W({x : β}, x)
   - x : β ∈ {x : β}
   - return (∅, β)
3. return (∅, β → β)

Result: ∀α. α → α
```

### Example 2: Application

```
W(∅, (λf. λx. f x))

1. α = fresh                           -- for f
2. W({f : α}, λx. f x)
   a. β = fresh                        -- for x
   b. W({f : α, x : β}, f x)
      i.   W({f : α, x : β}, f) = (∅, α)
      ii.  W({f : α, x : β}, x) = (∅, β)
      iii. γ = fresh
      iv.  S = unify(α, β → γ) = [α ↦ β → γ]
      v.   return ([α ↦ β → γ], γ)
   c. return ([α ↦ β → γ], (β → γ) → γ)
3. return ([α ↦ β → γ], (β → γ) → β → γ)

Result: ∀α β. (α → β) → α → β
```

### Example 3: Let Polymorphism

```
W(∅, let id = λx. x in id id)

1. W(∅, λx. x) = (∅, α → α)           -- from Example 1
2. gen(∅, α → α) = ∀α. α → α          -- α not in ftv(∅)
3. W({id : ∀α. α → α}, id id)
   a. W({id : ∀α. α → α}, id)
      - inst(∀α. α → α) = β → β       -- fresh β
      - return (∅, β → β)
   b. W({id : ∀α. α → α}, id)
      - inst(∀α. α → α) = γ → γ       -- fresh γ
      - return (∅, γ → γ)
   c. δ = fresh
   d. unify(β → β, (γ → γ) → δ)
      - unify(β, γ → γ) = [β ↦ γ → γ]
      - unify(γ → γ, δ) = [δ ↦ γ → γ]
      - S = [β ↦ γ → γ, δ ↦ γ → γ]
   e. return (S, γ → γ)
4. return (S, γ → γ)

Result: ∀α. α → α
```

### Example 4: Occurs Check Failure

```
W(∅, λx. x x)

1. α = fresh
2. W({x : α}, x x)
   a. W({x : α}, x) = (∅, α)
   b. W({x : α}, x) = (∅, α)
   c. β = fresh
   d. unify(α, α → β)
      - α ∈ ftv(α → β)
      - ERROR: occurs check failed

Result: Type error (no principal type exists)
```

## Algorithm W vs Algorithm J

| W | J |
|---|---|
| Returns substitution explicitly | Uses mutable references |
| Compositional | More efficient |
| Easier to prove correct | Harder to reason about |
| Standard presentation | Implementation-oriented |

## Extensions

| Extension | Modification |
|-----------|--------------|
| **Recursive let** | Use fix-point or tie the knot before generalization |
| **Type annotations** | Unify with declared type, then proceed |
| **Type classes** | Add constraint solving phase |
| **Higher-rank types** | Subsumption checking, bidirectional typing |
| **Row polymorphism** | Extended unification for records/variants |

## References

- Damas & Milner (1982). "Principal type-schemes for functional programs"
- Milner (1978). "A Theory of Type Polymorphism in Programming"
- Pierce, TAPL Chapter 22: Type Reconstruction
- Heeren et al. (2002). "Generalizing Hindley-Milner Type Inference Algorithms"
