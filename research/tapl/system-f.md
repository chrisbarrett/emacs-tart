# System F Quick Reference

Also known as the *polymorphic lambda calculus* or *second-order lambda calculus*.
Introduced independently by Girard (1972) and Reynolds (1974).

## 1. Syntax

### Types

```
τ ::= X                 type variable
    | τ₁ → τ₂           function type
    | ∀X.τ              universal type
```

### Terms

```
e ::= x                 term variable
    | λx:τ.e            term abstraction
    | e₁ e₂             term application
    | ΛX.e              type abstraction
    | e [τ]             type application
```

### Contexts

```
Γ ::= ∅                 empty context
    | Γ, x:τ            term variable binding
    | Γ, X              type variable binding
```

## 2. Type Formation Rules

Well-formed types under context Γ:

```
X ∈ Γ
────────────── [TVar]
Γ ⊢ X

Γ ⊢ τ₁    Γ ⊢ τ₂
────────────────── [TArrow]
Γ ⊢ τ₁ → τ₂

Γ, X ⊢ τ
─────────────── [TForall]
Γ ⊢ ∀X.τ
```

## 3. Typing Rules

```
x:τ ∈ Γ
────────────── [Var]
Γ ⊢ x : τ


Γ, x:τ₁ ⊢ e : τ₂
────────────────────── [Abs]
Γ ⊢ λx:τ₁.e : τ₁ → τ₂


Γ ⊢ e₁ : τ₁ → τ₂    Γ ⊢ e₂ : τ₁
─────────────────────────────────── [App]
Γ ⊢ e₁ e₂ : τ₂


Γ, X ⊢ e : τ
───────────────────── [TAbs]
Γ ⊢ ΛX.e : ∀X.τ


Γ ⊢ e : ∀X.τ₁    Γ ⊢ τ₂
─────────────────────────── [TApp]
Γ ⊢ e [τ₂] : τ₁[X ↦ τ₂]
```

## 4. Type Substitution

Capture-avoiding substitution τ₁[X ↦ τ₂]:

```
X[X ↦ τ]           = τ
Y[X ↦ τ]           = Y                     (X ≠ Y)
(τ₁ → τ₂)[X ↦ τ]   = τ₁[X ↦ τ] → τ₂[X ↦ τ]
(∀Y.τ₁)[X ↦ τ]     = ∀Y.(τ₁[X ↦ τ])        (Y ∉ FV(τ), Y ≠ X)
(∀X.τ₁)[X ↦ τ]     = ∀X.τ₁                 (X is bound)
```

**Free type variables:**

```
FV(X)       = {X}
FV(τ₁ → τ₂) = FV(τ₁) ∪ FV(τ₂)
FV(∀X.τ)    = FV(τ) \ {X}
```

**Alpha-equivalence:** Bound variables can be renamed:
`∀X.X → X ≡ ∀Y.Y → Y`

## 5. Reduction Rules

### Beta-reduction (terms)

```
(λx:τ.e₁) e₂  ⟶β  e₁[x ↦ e₂]
```

### Type-beta-reduction

```
(ΛX.e) [τ]  ⟶β  e[X ↦ τ]
```

### Congruence rules

```
e₁ ⟶ e₁'                    e₂ ⟶ e₂'
──────────────              ──────────────
e₁ e₂ ⟶ e₁' e₂              v e₂ ⟶ v e₂'

e ⟶ e'
─────────────
e [τ] ⟶ e' [τ]
```

## 6. Key Theorems

### Type Preservation (Subject Reduction)

If `Γ ⊢ e : τ` and `e ⟶ e'`, then `Γ ⊢ e' : τ`.

### Progress

If `∅ ⊢ e : τ`, then either `e` is a value or `∃e'. e ⟶ e'`.

### Strong Normalization

Every well-typed term in System F terminates. All reduction sequences are finite.

*Proof technique:* Girard's reducibility candidates (logical relations).

### Decidability

- **Type checking:** Decidable (given fully annotated terms)
- **Type inference:** Undecidable (Wells 1999)
- **Typability:** Undecidable

## 7. Encodings in System F

### Church Booleans

```
Bool = ∀X. X → X → X

true  = ΛX. λt:X. λf:X. t
false = ΛX. λt:X. λf:X. f
if    = ΛX. λb:Bool. λt:X. λf:X. b [X] t f
```

### Church Naturals

```
Nat = ∀X. (X → X) → X → X

zero = ΛX. λs:X→X. λz:X. z
succ = λn:Nat. ΛX. λs:X→X. λz:X. s (n [X] s z)
```

### Products

```
A × B = ∀X. (A → B → X) → X

pair = ΛA. ΛB. λa:A. λb:B. ΛX. λf:A→B→X. f a b
fst  = ΛA. ΛB. λp:A×B. p [A] (λa:A. λb:B. a)
snd  = ΛA. ΛB. λp:A×B. p [B] (λa:A. λb:B. b)
```

### Sums

```
A + B = ∀X. (A → X) → (B → X) → X

inl = ΛA. ΛB. λa:A. ΛX. λl:A→X. λr:B→X. l a
inr = ΛA. ΛB. λb:B. ΛX. λl:A→X. λr:B→X. r b
```

### Existentials

```
∃X.τ = ∀Y. (∀X. τ → Y) → Y

pack   : τ[X ↦ σ] → ∃X.τ
unpack : ∃X.τ → (∀X. τ → ρ) → ρ    (X ∉ FV(ρ))
```

## 8. Relationship to Hindley-Milner

| Aspect | System F | Hindley-Milner |
|--------|----------|----------------|
| Polymorphism | Explicit, first-class | Implicit, prenex only |
| Type inference | Undecidable | Decidable (Algorithm W) |
| ∀ position | Anywhere in types | Only at top level |
| Type annotations | Required | Optional |
| Expressiveness | More expressive | Restricted subset |

### Hindley-Milner as a Restriction

HM types (polytypes σ, monotypes τ):

```
τ ::= α | τ₁ → τ₂ | T τ₁...τₙ     (monotypes)
σ ::= τ | ∀α.σ                     (polytypes, ∀ at top only)
```

**Key restriction:** No `∀` to the left of `→` or nested inside type constructors.

```
System F allows:    (∀X. X → X) → Int     -- higher-rank
HM forbids this:    ∀ only at outermost position
```

### Let-polymorphism

HM achieves polymorphism via `let`:

```
Γ ⊢ e₁ : σ    Γ, x:σ ⊢ e₂ : τ
────────────────────────────────── [Let]
Γ ⊢ let x = e₁ in e₂ : τ
```

The type σ is *generalized* at `let` bindings, not at λ.

### Implicit vs Explicit

HM term `λx. x` corresponds to System F `ΛX. λx:X. x`.

Type application/abstraction is implicit in HM, explicit in System F.

## 9. Extensions

### System Fω (Higher-kinded polymorphism)

Add type-level functions:

```
κ ::= * | κ₁ → κ₂              kinds
τ ::= ... | λX:κ.τ | τ₁ τ₂     type operators
```

### System F<: (Bounded quantification)

Add subtyping with bounds:

```
τ ::= ... | ∀X<:τ₁.τ₂
```

### Fω<: (Higher-order bounded)

Combines Fω and F<:. Foundation for Scala's type system.

## 10. Implementation Notes

### De Bruijn indices

Avoid capture issues by using indices instead of names:

```
∀. 0 → 0      -- represents ∀X. X → X
```

### Bidirectional type checking

Split typing judgment into checking and synthesis modes:

```
Γ ⊢ e ⇒ τ     (synthesis: infer type from term)
Γ ⊢ e ⇐ τ     (checking: check term against type)
```

### Type application syntax

Common notations:
- `e [τ]` - TAPL style
- `e @τ` - some implementations
- `e {τ}` - GHC Core

## References

- Girard, J.-Y. (1972). *Interprétation fonctionnelle et élimination des coupures*
- Reynolds, J. C. (1974). *Towards a theory of type structure*
- Pierce, B. C. (2002). *Types and Programming Languages*, Chapters 23-25
- Wells, J. B. (1999). *Typability and type checking in System F are equivalent and undecidable*
