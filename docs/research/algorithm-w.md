# Algorithm W and Hindley-Milner Type Inference

A comprehensive research document for implementing type inference in Emacs Lisp.

## Table of Contents

1. [Overview](#overview)
2. [Algorithm W](#algorithm-w)
3. [Unification](#unification)
4. [Let-Polymorphism](#let-polymorphism)
5. [The Value Restriction](#the-value-restriction)
6. [Constraint-Based Variants](#constraint-based-variants)
7. [Complexity Analysis](#complexity-analysis)
8. [Implementation Considerations for Emacs Lisp](#implementation-considerations-for-emacs-lisp)

---

## Overview

The Hindley-Milner (HM) type system is a classical type system for the lambda
calculus with parametric polymorphism. It was first described by J. Roger
Hindley (1969) and independently rediscovered by Robin Milner (1978). Luis Damas
contributed formal analysis and proof of the method in his PhD thesis (1982).

### Key Properties

1. **Completeness**: Always infers the most general (principal) type
2. **Decidability**: Type inference terminates for all inputs
3. **No annotations required**: Works without programmer-supplied type hints
4. **Efficient in practice**: Runs in approximately linear time on real programs

### Core Concepts

**Types** are either:
- Base types: `Int`, `Bool`, `String`, etc.
- Type variables: `α`, `β`, `γ`, etc.
- Function types: `τ₁ → τ₂`
- Type constructors: `List α`, `Maybe α`, etc.

**Type Schemes** (polytypes) include quantified type variables:
```
σ ::= τ | ∀α. σ

Examples:
  Int → Int                    (monotype)
  ∀α. α → α                    (polytype - the identity function)
  ∀α. ∀β. α → β → α            (polytype - the K combinator)
```

**Important**: Quantification only appears at the outermost level (prenex form).
This restriction is key to making type inference decidable—System F (with
arbitrary ∀ placement) has undecidable type inference.

---

## Algorithm W

Algorithm W is Milner's original type inference algorithm. It performs a
syntax-directed traversal of the expression tree, generating and solving type
constraints on the fly.

### The Algorithm

```
W : TypeEnv × Expr → Substitution × Type

W(Γ, e) = case e of

  -- Variable
  | x →
      if x ∉ dom(Γ) then error "unbound variable"
      let σ = Γ(x)
      let τ = instantiate(σ)        -- Replace ∀-bound vars with fresh vars
      return (∅, τ)

  -- Lambda abstraction
  | λx. e₁ →
      let α = fresh_type_var()
      let (S₁, τ₁) = W(Γ ∪ {x : α}, e₁)
      return (S₁, S₁(α) → τ₁)

  -- Application
  | e₁ e₂ →
      let (S₁, τ₁) = W(Γ, e₁)
      let (S₂, τ₂) = W(S₁(Γ), e₂)
      let α = fresh_type_var()
      let S₃ = unify(S₂(τ₁), τ₂ → α)
      return (S₃ ∘ S₂ ∘ S₁, S₃(α))

  -- Let binding
  | let x = e₁ in e₂ →
      let (S₁, τ₁) = W(Γ, e₁)
      let σ = generalize(S₁(Γ), τ₁)  -- Quantify free vars not in env
      let (S₂, τ₂) = W(S₁(Γ) ∪ {x : σ}, e₂)
      return (S₂ ∘ S₁, τ₂)

  -- Literal
  | n (integer) → return (∅, Int)
  | b (boolean) → return (∅, Bool)
  | s (string)  → return (∅, String)
```

### Supporting Operations

**Instantiation** replaces quantified variables with fresh type variables:

```
instantiate : TypeScheme → Type

instantiate(∀α₁...αₙ. τ) =
    let β₁...βₙ = fresh_type_vars()
    return τ[α₁ ↦ β₁, ..., αₙ ↦ βₙ]
```

**Generalization** quantifies type variables not free in the environment:

```
generalize : TypeEnv × Type → TypeScheme

generalize(Γ, τ) =
    let αs = free_type_vars(τ) - free_type_vars(Γ)
    return ∀αs. τ
```

**Substitution composition**:

```
(S₂ ∘ S₁)(τ) = S₂(S₁(τ))
```

### Worked Example

Infer the type of `let id = λx. x in id 42`:

```
1. W(∅, let id = λx.x in id 42)

2. First, infer type of λx.x:
   W(∅, λx.x)
   - Create fresh α₀ for x
   - W({x:α₀}, x) = (∅, α₀)
   - Return (∅, α₀ → α₀)

3. Generalize α₀ → α₀ in empty environment:
   - free_vars(α₀ → α₀) = {α₀}
   - free_vars(∅) = {}
   - σ = ∀α₀. α₀ → α₀

4. Infer type of (id 42) in {id : ∀α₀. α₀ → α₀}:

   4a. W(Γ, id):
       - instantiate(∀α₀. α₀ → α₀) with fresh α₁
       - Return (∅, α₁ → α₁)

   4b. W(Γ, 42):
       - Return (∅, Int)

   4c. Unify (α₁ → α₁) with (Int → α₂):
       - unify(α₁, Int) yields {α₁ ↦ Int}
       - unify(α₁, α₂) yields {α₂ ↦ Int}
       - S₃ = {α₁ ↦ Int, α₂ ↦ Int}

   4d. Return (S₃, Int)

5. Final result: Int
```

### Algorithm J (Imperative Variant)

Algorithm J uses a mutable global substitution instead of threading
substitutions through the computation. It's equivalent to W but more efficient
and simpler to implement:

```
J : TypeEnv × Expr → Type
-- Uses global mutable substitution S

J(Γ, e) = case e of
  | x →
      instantiate(Γ(x))

  | λx. e₁ →
      let α = fresh()
      let τ₁ = J(Γ ∪ {x : α}, e₁)
      return apply_subst(α) → τ₁

  | e₁ e₂ →
      let τ₁ = J(Γ, e₁)
      let τ₂ = J(Γ, e₂)
      let α = fresh()
      unify!(τ₁, τ₂ → α)           -- Mutates global S
      return apply_subst(α)

  | let x = e₁ in e₂ →
      let τ₁ = J(Γ, e₁)
      let σ = generalize(Γ, τ₁)
      return J(Γ ∪ {x : σ}, e₂)
```

### Algorithm M (Top-Down Variant)

Algorithm M takes an expected type as an additional parameter, enabling earlier
error detection. It stops sooner on ill-typed expressions because it can detect
mismatches before fully analyzing subexpressions.

```
M : TypeEnv × Expr × Type → Substitution

M(Γ, e, τ_expected) = case e of
  | x →
      let τ = instantiate(Γ(x))
      unify(τ, τ_expected)

  | λx. e₁ →
      let α, β = fresh(), fresh()
      let S₁ = unify(τ_expected, α → β)
      let S₂ = M(S₁(Γ) ∪ {x : S₁(α)}, e₁, S₁(β))
      return S₂ ∘ S₁

  -- ... etc
```

---

## Unification

Unification finds a substitution that makes two types equal. It's the core
operation that "solves" type constraints.

### Robinson's Algorithm

```
unify : Type × Type → Substitution

unify(τ₁, τ₂) = case (τ₁, τ₂) of

  -- Identical types unify trivially
  | (T, T) where T is a base type →
      return ∅

  -- Variable on left: bind it
  | (α, τ) →
      if α = τ then return ∅
      if α ∈ free_vars(τ) then error "occurs check failed"
      return {α ↦ τ}

  -- Variable on right: symmetric
  | (τ, α) →
      unify(α, τ)

  -- Function types: unify components
  | (τ₁ → τ₂, τ₃ → τ₄) →
      let S₁ = unify(τ₁, τ₃)
      let S₂ = unify(S₁(τ₂), S₁(τ₄))
      return S₂ ∘ S₁

  -- Type constructors: same constructor, unify args
  | (C τ₁...τₙ, C τ'₁...τ'ₙ) →
      unify_many([(τ₁,τ'₁), ..., (τₙ,τ'ₙ)])

  -- Mismatch
  | _ →
      error "cannot unify τ₁ with τ₂"


unify_many : [(Type, Type)] → Substitution

unify_many([]) = ∅
unify_many((τ₁, τ₂) :: rest) =
    let S₁ = unify(τ₁, τ₂)
    let S₂ = unify_many(apply_subst(S₁, rest))
    return S₂ ∘ S₁
```

### The Occurs Check

The occurs check prevents creating infinite types. Without it, unifying `α` with
`List α` would create the infinite type `List (List (List ...))`.

```
occurs_check : TypeVar × Type → Bool

occurs_check(α, τ) = α ∈ free_vars(τ)
```

**Example**: The expression `λx. x x` triggers the occurs check:

```
W(∅, λx. x x)
1. Create fresh α for x
2. Infer (x x) in {x : α}:
   - x : α, x : α
   - Unify α with (α → β)
   - α ∈ free_vars(α → β)  -- Occurs check fails!
```

This is the famous omega combinator, which would have type `α` where
`α = α → β`—an infinite type.

### Efficient Unification

Robinson's original algorithm has exponential worst-case complexity due to
substitution application. Modern implementations use:

1. **Union-Find with path compression**: Type variables point to their
   representative. O(α(n)) amortized per operation.

2. **Mutable references**: Type variables are ref cells. Unification mutates
   cells to point to their unified type.

```
-- Type variable as mutable cell
type TypeVar = Ref (Maybe Type)

unify_var(α, τ) =
    case !α of
    | Some τ' → unify(τ', τ)  -- α already bound
    | None →
        if occurs(α, τ) then error
        α := Some τ            -- Bind α to τ
```

### Unification Pitfalls

1. **Forgetting the occurs check**: Leads to infinite loops or stack overflow

2. **Not applying substitutions**: After unifying `α` with `Int`, must apply
   this to all types being processed

3. **Order dependence**: `unify(α, Int → Int)` vs `unify(Int → Int, α)` must
   give same result

4. **Destructive update issues**: If using mutable cells, be careful with
   sharing and backtracking

---

## Let-Polymorphism

Let-polymorphism is the key feature distinguishing HM from simply-typed lambda
calculus. It allows let-bound variables to have polymorphic types that are
instantiated at each use site.

### Why Lambda Isn't Enough

Consider:
```
(λid. (id 1, id true)) (λx. x)
```

In simply-typed lambda calculus, `id` must have a single monomorphic type. If
`id : Int → Int` then `id true` fails. If `id : Bool → Bool` then `id 1` fails.

But with let:
```
let id = λx. x in (id 1, id true)
```

Here `id : ∀α. α → α`, and each use instantiates α differently:
- `id 1` : instantiate with `α = Int`
- `id true` : instantiate with `α = Bool`

### The Generalization Rule

```
      Γ ⊢ e₁ : τ₁    Γ, x : Gen(Γ, τ₁) ⊢ e₂ : τ₂
      ──────────────────────────────────────────
               Γ ⊢ let x = e₁ in e₂ : τ₂

where Gen(Γ, τ) = ∀(FV(τ) - FV(Γ)). τ
```

**Key insight**: We only quantify type variables that are NOT constrained by the
environment. Variables appearing in Γ might be unified later, so we can't
generalize them.

### When Generalization Happens

Generalization occurs ONLY at let bindings, not at lambda bindings:

```
-- Polymorphic (generalized at let)
let f = λx. x in ...       -- f : ∀α. α → α

-- Monomorphic (no generalization)
(λf. ...) (λx. x)          -- f : α → α (for some specific α)
```

This restriction is crucial for decidability. Full System F (generalization
anywhere) has undecidable type inference.

### Efficient Generalization with Levels

Didier Rémy discovered an efficient method for generalization that avoids
scanning the type environment. It uses "levels" (or "ranks") to track when type
variables were created.

```
-- Each type variable has a level
type TypeVar = { id : Int, level : Int, binding : Ref (Maybe Type) }

-- Current level increases when entering let RHS
current_level : Ref Int

-- Generalize: quantify vars with level > current_level
generalize(τ) =
    for each α in free_vars(τ):
        if α.level > !current_level:
            mark α as quantified
```

**How it works**:
1. Start at level 0
2. When entering `let x = e₁ in e₂`:
   - Increment level
   - Infer `e₁` (fresh vars get current level)
   - Decrement level
   - Generalize vars with level > current
   - Infer `e₂`
3. Type variables at higher levels aren't in the environment's free vars

This is O(size of type) rather than O(size of environment × size of type).

### Generalization Example

```
let f = λx. λy. x in
let g = f 1 in
(g true, g "hello")

Step 1: Infer λx.λy.x
  - x : α, y : β, body : α
  - Type: α → β → α

Step 2: Generalize in empty env
  - Gen(∅, α → β → α) = ∀αβ. α → β → α
  - f : ∀αβ. α → β → α

Step 3: Infer f 1
  - Instantiate: f : α₁ → β₁ → α₁
  - Unify α₁ with Int
  - Type: β₁ → Int

Step 4: Generalize in {f : ...}
  - β₁ not in FV(Γ)
  - Gen(Γ, β₁ → Int) = ∀β₁. β₁ → Int
  - g : ∀β. β → Int

Step 5: Infer (g true, g "hello")
  - g true: instantiate g, unify β₂ with Bool → Int
  - g "hello": instantiate g, unify β₃ with String → Int
  - Type: (Int, Int)
```

---

## The Value Restriction

The value restriction addresses a subtle unsoundness that arises when combining
polymorphism with mutable state.

### The Problem

Consider this ML code:

```ml
let r = ref None in        (* r : ∀α. ref (option α) ? *)
r := Some 1;               (* Use r as ref (option int) *)
match !r with
| Some s -> s ^ "hello"    (* Use r as ref (option string) ! *)
| None -> ""
```

Without restriction, `r` would get type `∀α. ref (option α)`. Each use
instantiates α differently, but they all refer to the SAME mutable cell. This
allows putting an Int in and taking a String out—a type error at runtime!

### The Original Solution: Imperative Type Variables

Early ML distinguished "applicative" and "imperative" type variables:
- Applicative variables (α): Can be generalized freely
- Imperative variables (α'): Cannot be generalized if they "escape"

This worked but was complex and leaked implementation details into module
signatures.

### Standard ML's Value Restriction

Andrew Wright proposed a simpler solution: only generalize if the RHS is a
"syntactic value" (non-expansive expression):

**Syntactic values**:
- Variables: `x`
- Lambda abstractions: `λx. e`
- Constructors applied to values: `Some v`, `(v₁, v₂)`
- Literals: `42`, `"hello"`

**Non-values** (expansive):
- Function applications: `f x` (might allocate)
- Let expressions: `let x = e₁ in e₂`
- `ref e` (definitely allocates)

```
let id = λx. x                    -- Value: generalize
let r = ref []                    -- Not a value: don't generalize (r : '_a list ref)
let f = compose id id             -- Not a value: don't generalize
```

### OCaml's Relaxed Value Restriction

OCaml relaxes the value restriction using variance analysis:

**Key insight**: A type variable in a covariant position cannot denote mutable
state (because mutable locations are invariant).

```ml
type +'a t = 'a list    (* 'a is covariant *)
type 'a ref             (* 'a is invariant *)
```

The relaxed rule: Generalize covariant type variables even for non-values:

```ml
let empty = []                (* empty : 'a list -- generalized! *)
let x = List.map id []        (* x : 'a list -- generalized! *)
let r = ref []                (* r : '_a list ref -- NOT generalized *)
```

### Workarounds

**Eta-expansion**: Convert non-value to value:

```ml
(* Won't generalize *)
let const_none = List.map (fun _ -> None)

(* Will generalize *)
let const_none xs = List.map (fun _ -> None) xs
```

**Type annotation**: Force a specific polymorphic type:

```ml
let empty : 'a list = []
```

### Comparison of Approaches

| Approach | Soundness | Expressiveness | Complexity |
|----------|-----------|----------------|------------|
| No restriction | Unsound | N/A | N/A |
| Imperative vars | Sound | High | High |
| Value restriction | Sound | Medium | Low |
| Relaxed (OCaml) | Sound | Medium-High | Medium |

For Emacs Lisp, the value restriction is likely sufficient since Lisp
programmers are accustomed to occasional type annotations.

---

## Constraint-Based Variants

Constraint-based type inference separates constraint generation from constraint
solving, providing clearer semantics and better error messages.

### Basic Approach

Instead of solving constraints immediately (as in Algorithm W), collect all
constraints first:

```
generate : TypeEnv × Expr → Type × Constraints

generate(Γ, e) = case e of
  | x →
      let τ = instantiate(Γ(x))
      (τ, ∅)

  | λx. e₁ →
      let α = fresh()
      let (τ₁, C₁) = generate(Γ ∪ {x : α}, e₁)
      (α → τ₁, C₁)

  | e₁ e₂ →
      let (τ₁, C₁) = generate(Γ, e₁)
      let (τ₂, C₂) = generate(Γ, e₂)
      let α = fresh()
      (α, C₁ ∪ C₂ ∪ {τ₁ = τ₂ → α})

  | let x = e₁ in e₂ →
      let (τ₁, C₁) = generate(Γ, e₁)
      let S = solve(C₁)
      let σ = generalize(S(Γ), S(τ₁))
      let (τ₂, C₂) = generate(Γ ∪ {x : σ}, e₂)
      (τ₂, C₂)   -- C₁ already solved


infer(Γ, e) =
    let (τ, C) = generate(Γ, e)
    let S = solve(C)
    return S(τ)
```

### Advantages

1. **Better error messages**: Can analyze which constraints conflict
2. **Flexibility**: Different solving strategies for different needs
3. **Extensibility**: Easy to add new constraint types

### HM(X): Parameterized Constraint Domain

HM(X) generalizes HM by parameterizing over the constraint domain X:

- **HM(=)**: Standard HM with equality constraints (Herbrand unification)
- **HM(≤)**: Subtyping constraints
- **HM(type classes)**: Haskell-style constraints

```
-- Constraints are elements of domain X
type Constraint(X) = ...

-- Solver is domain-specific
solve : Constraint(X)* → Substitution
```

### Connection to Logic Programming

HM(X) type inference is equivalent to CLP(X) (Constraint Logic Programming):

```prolog
% Type inference as logic program
infer(Γ, var(X), T) :-
    lookup(X, Γ, Scheme),
    instantiate(Scheme, T).

infer(Γ, app(E1, E2), T) :-
    infer(Γ, E1, T1),
    infer(Γ, E2, T2),
    T1 = (T2 -> T).    % Constraint

infer(Γ, lam(X, E), (T1 -> T2)) :-
    infer([(X, T1) | Γ], E, T2).
```

### Bidirectional Type Checking

Bidirectional checking separates "checking" mode (given expected type) from
"synthesis" mode (produce type):

```
check : Env × Expr × Type → ()
synth : Env × Expr → Type

check(Γ, λx. e, τ₁ → τ₂) =
    check(Γ ∪ {x : τ₁}, e, τ₂)

check(Γ, e, τ) =
    let τ' = synth(Γ, e)
    unify(τ, τ')

synth(Γ, x) = instantiate(Γ(x))

synth(Γ, e₁ e₂) =
    let τ₁ = synth(Γ, e₁)
    let (τ_arg, τ_ret) = ensure_function(τ₁)
    check(Γ, e₂, τ_arg)
    return τ_ret

synth(Γ, (e : τ)) =       -- Type annotation
    check(Γ, e, τ)
    return τ
```

This approach naturally handles type annotations and higher-rank polymorphism.

---

## Complexity Analysis

### Theoretical Complexity

**Hindley-Milner type inference is DEXPTIME-complete.**

This was proven independently by:
- Mairson (1990)
- Kfoury, Tiuryn & Urzyczyn (1990)

The complexity arises from let-polymorphism: each use of a polymorphic variable
can instantiate its type with fresh variables, potentially causing exponential
blowup.

### Pathological Example

```ml
let f0 = fun x -> x in
let f1 = fun x -> f0 (f0 x) in
let f2 = fun x -> f1 (f1 x) in
let f3 = fun x -> f2 (f2 x) in
...
```

At each level, the type roughly doubles in size:
- f0 : α → α
- f1 : α → α (size 3)
- f2 : type of size ~7
- f3 : type of size ~15
- fn : type of size ~2^n

### Another Pathological Case

```ml
let p x y = fun z -> z x y in
let x1 = fun x -> p x x in
let x2 = fun z -> x1 (x1 z) in
let x3 = fun z -> x2 (x2 z) in
x3 (fun z -> z)
```

The type at each step is squared and doubled, leading to exponential growth.

### Practical Performance

Despite DEXPTIME-complete worst case, HM performs well in practice:

1. **Real programs don't trigger pathological cases**: The exponential blowup
   requires specific nested let structures rarely seen in practice.

2. **Type sizes stay small**: In typical code, types are small terms. The
   constant factor in O(n) dominates.

3. **Top-level annotations help**: Explicit type signatures at module boundaries
   prevent type variables from propagating.

4. **Linear in practice**: Studies show HM runs in roughly O(n) time on real
   codebases, where n is program size.

### Complexity Sources

| Feature | Impact |
|---------|--------|
| Simply-typed λ-calculus | Linear |
| Let expressions | Potentially exponential |
| Polymorphic recursion | Undecidable (requires annotations) |
| Higher-rank polymorphism | Undecidable (requires annotations) |

### Optimization Techniques

1. **Levels for generalization**: O(1) per variable instead of O(|Γ|)

2. **Union-find for substitutions**: Near-constant time per unification

3. **Hash-consing types**: Share common type substructures

4. **Lazy substitution**: Don't apply substitutions until necessary

5. **Occur-check optimization**: Only check when unifying with a variable

---

## Implementation Considerations for Emacs Lisp

### Language Features to Handle

Emacs Lisp presents unique challenges:

1. **Dynamic typing by default**: Need gradual typing or separate typed subset
2. **Macros**: Type checking must occur after macro expansion
3. **Special forms**: `if`, `cond`, `let`, `let*`, `progn`, etc.
4. **Multiple values**: Some functions return multiple values
5. **Optional/rest parameters**: `&optional`, `&rest` in argument lists
6. **Property lists**: Ad-hoc polymorphism via plists
7. **Advice and hooks**: Dynamic modification of functions

### Recommended Approach

For an Emacs Lisp type system, I recommend:

1. **Algorithm J** (imperative variant): Simpler to implement, more efficient
   than pure Algorithm W

2. **Levels-based generalization**: Rémy's algorithm avoids scanning the
   environment

3. **Value restriction**: Simpler than the relaxed variant, sufficient for most
   code

4. **Constraint collection for errors**: Generate constraints, solve, report
   all conflicts

### Type Representation

```elisp
;; Types as s-expressions
(defconst tart-type-int 'Int)
(defconst tart-type-string 'String)
(defconst tart-type-bool 'Bool)

;; Type variables: (TVar id level)
;; Function types: (-> arg-type ret-type)
;; List types: (List elem-type)
;; Type schemes: (Forall (vars...) type)
```

### Unification Implementation

```elisp
(defun tart-unify (t1 t2)
  "Unify types T1 and T2, mutating the substitution."
  (let ((t1 (tart-find t1))   ; Follow variable chains
        (t2 (tart-find t2)))
    (cond
     ;; Same type: done
     ((equal t1 t2) nil)

     ;; Variable cases
     ((tart-tvar-p t1)
      (tart-unify-var t1 t2))
     ((tart-tvar-p t2)
      (tart-unify-var t2 t1))

     ;; Function types
     ((and (tart-arrow-p t1) (tart-arrow-p t2))
      (tart-unify (tart-arrow-arg t1) (tart-arrow-arg t2))
      (tart-unify (tart-arrow-ret t1) (tart-arrow-ret t2)))

     ;; Type constructors
     ((and (tart-tycon-p t1) (tart-tycon-p t2)
           (eq (tart-tycon-name t1) (tart-tycon-name t2)))
      (cl-mapc #'tart-unify
               (tart-tycon-args t1)
               (tart-tycon-args t2)))

     ;; Mismatch
     (t (error "Cannot unify %s with %s" t1 t2)))))

(defun tart-unify-var (var typ)
  "Unify type variable VAR with type TYP."
  (when (tart-occurs-in-p var typ)
    (error "Infinite type: %s occurs in %s" var typ))
  (tart-bind-var var typ))
```

### Testing Strategy

1. **Unit tests for unification**: All cases including occurs check
2. **Integration tests for inference**: Standard examples (id, const, compose)
3. **Regression tests**: Known pathological cases
4. **Property-based testing**: Random expressions should either type-check or
   give sensible errors

### Error Messages

Good error messages are crucial. Track source locations and report:

```
Type error at line 42, column 10:
  Cannot unify 'Int' with 'String'

  In expression: (+ x "hello")
                    ^^^^^^^^^
  'x' has type Int
  but "hello" has type String
```

---

## References

### Original Papers

- Hindley, R. (1969). "The Principal Type-Scheme of an Object in Combinatory
  Logic"
- Milner, R. (1978). "A Theory of Type Polymorphism in Programming"
- Damas, L. & Milner, R. (1982). "Principal Type-Schemes for Functional
  Programs"

### Tutorials and Implementations

- Grabmüller, M. "Algorithm W Step by Step"
- Heeren, B. et al. "Generalizing Hindley-Milner Type Inference Algorithms"
- Oleg Kiselyov. "Efficient and Insightful Generalization"
  (https://okmij.org/ftp/ML/generalization.html)

### Value Restriction

- Wright, A.K. (1995). "Simple Imperative Polymorphism"
- Garrigue, J. (2004). "Relaxing the Value Restriction"

### Complexity

- Mairson, H.G. (1990). "Deciding ML Typability is Complete for Deterministic
  Exponential Time"
- Kfoury, A.J. et al. (1990). "ML Typability is DEXPTIME-Complete"

### Modern Treatments

- Pierce, B.C. "Types and Programming Languages" (Chapter 22)
- Fan, A. et al. (2025). "Practical Type Inference with Levels"
