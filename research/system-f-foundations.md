# System F Foundations

Comprehensive reference on the polymorphic lambda calculus for implementing a
static type checker targeting Emacs Lisp.

## Historical Context and Origins

### Girard's System F (1972)

Jean-Yves Girard introduced System F in his doctoral thesis "Interpretation
fonctionnelle et elimination des coupures de l'arithmetique d'ordre superieur"
(1972). His motivation was proof-theoretic: establishing the consistency of
second-order arithmetic through a computational interpretation.

**Key insight:** Types correspond to propositions in second-order logic;
polymorphic abstraction corresponds to universal quantification over
propositions.

### Reynolds' Polymorphic Lambda Calculus (1974)

John C. Reynolds independently discovered the same system, approaching it from
a programming languages perspective in "Towards a theory of type structure"
(1974). Reynolds was motivated by:

1. Providing a mathematical foundation for generic programming
2. Explaining parametric polymorphism as found in ML
3. Establishing a semantics for type abstraction

**The Reynolds-Girard isomorphism:** Both systems are equivalent, demonstrating
a deep connection between logic and computation (Curry-Howard correspondence
extended to second-order logic).

### Seminal Papers

| Paper | Author(s) | Year | Contribution |
|-------|-----------|------|--------------|
| "Interpretation fonctionnelle..." | Girard | 1972 | Original System F |
| "Towards a theory of type structure" | Reynolds | 1974 | Programming perspective |
| "A Theory of Type Polymorphism in Programming" | Milner | 1978 | Algorithm W, HM inference |
| "Principal type-schemes for functional programs" | Damas & Milner | 1982 | Formal treatment of HM |
| "Type Inference with Polymorphic Recursion" | Mycroft | 1984 | Polymorphic recursion |
| "Typability and Type Checking in System F are Equivalent and Undecidable" | Wells | 1999 | Undecidability proof |

## Core Syntax

### Types

```
tau, sigma ::=
  | alpha                     -- Type variable
  | tau -> sigma              -- Function type
  | forall alpha. tau         -- Universal type (polymorphism)
```

**Extended syntax for practical systems:**

```
tau, sigma ::=
  | alpha                     -- Type variable
  | C                         -- Type constant (Int, Bool, String, etc.)
  | tau -> sigma              -- Function type
  | forall alpha. tau         -- Universal type
  | tau_1 * tau_2 * ... * tau_n  -- Product types (tuples)
  | tau_1 + tau_2 + ... + tau_n  -- Sum types (variants)
  | T tau_1 ... tau_n         -- Type constructor application
```

### Terms

```
e ::=
  | x                         -- Variable
  | \x:tau. e                 -- Lambda abstraction (annotated)
  | e_1 e_2                   -- Application
  | /\alpha. e                -- Type abstraction
  | e [tau]                   -- Type application
```

**Notation conventions:**
- `/\` represents capital lambda (type-level abstraction)
- `[tau]` represents explicit type instantiation
- In practical systems, type applications are often implicit (inferred)

## Typing Rules

### Environments

```
Gamma ::= . | Gamma, x:tau | Gamma, alpha
```

A typing environment contains:
- Term variable bindings: `x:tau`
- Type variable declarations: `alpha` (indicates alpha is in scope)

### Formation Rules

**Type well-formedness:** `Gamma |- tau : *`

```
                                        Gamma |- tau_1 : *    Gamma |- tau_2 : *
------------- (TVar)                    ------------------------------------ (TArr)
Gamma |- alpha : *                      Gamma |- tau_1 -> tau_2 : *
(if alpha in Gamma)

Gamma, alpha |- tau : *
--------------------------- (TAll)
Gamma |- forall alpha. tau : *
```

### Core Typing Rules

**Term typing judgment:** `Gamma |- e : tau`

```
x:tau in Gamma
--------------- (Var)
Gamma |- x : tau


Gamma, x:tau_1 |- e : tau_2
------------------------------- (Abs)
Gamma |- \x:tau_1. e : tau_1 -> tau_2


Gamma |- e_1 : tau_1 -> tau_2    Gamma |- e_2 : tau_1
----------------------------------------------------- (App)
Gamma |- e_1 e_2 : tau_2


Gamma, alpha |- e : tau
--------------------------------- (TAbs)
Gamma |- /\alpha. e : forall alpha. tau


Gamma |- e : forall alpha. tau_1    Gamma |- tau_2 : *
------------------------------------------------------ (TApp)
Gamma |- e [tau_2] : tau_1[tau_2/alpha]
```

### Substitution

Type substitution `tau_1[tau_2/alpha]` replaces free occurrences of `alpha`
in `tau_1` with `tau_2`, avoiding capture:

```
alpha[tau/alpha] = tau
beta[tau/alpha] = beta                     (when beta != alpha)
(tau_1 -> tau_2)[tau/alpha] = tau_1[tau/alpha] -> tau_2[tau/alpha]
(forall beta. tau_1)[tau/alpha] = forall beta. tau_1[tau/alpha]
                                           (when beta != alpha and beta not free in tau)
```

**Alpha-equivalence:** Types that differ only in bound variable names are
considered equal:
```
forall alpha. alpha -> alpha  =  forall beta. beta -> beta
```

## Key Properties

### Strong Normalization

**Theorem (Girard 1972):** Every well-typed term in System F reduces to a
normal form in a finite number of steps.

**Implications for implementation:**
1. Type checking always terminates (no infinite loops from evaluation)
2. Every well-typed program terminates (no runtime infinite loops)
3. System F is not Turing-complete as a programming language

**Practical note:** Real programming languages extend System F with general
recursion (fix-point operators), sacrificing strong normalization for
computational completeness.

### Type Preservation (Subject Reduction)

**Theorem:** If `Gamma |- e : tau` and `e --> e'` then `Gamma |- e' : tau`.

**Implication:** Types are preserved during computation. If a program
type-checks, it will not "go wrong" due to type errors at runtime.

### Progress

**Theorem:** If `|- e : tau` (closed, well-typed term) then either:
1. `e` is a value, or
2. There exists `e'` such that `e --> e'`

**Implication:** Well-typed programs don't get stuck. Combined with
preservation, this gives us type safety.

### Undecidability of Type Inference

**Theorem (Wells 1999):** Both type checking and typability are undecidable
for System F.

- **Type checking:** Given `Gamma`, `e`, `tau`, is `Gamma |- e : tau`?
- **Typability:** Given `e`, does there exist `Gamma`, `tau` such that
  `Gamma |- e : tau`?

**Implications for implementation:**
1. Full System F type inference is impossible
2. Practical systems must restrict polymorphism or require annotations
3. This motivates Hindley-Milner as a decidable fragment

### Parametricity (Free Theorems)

**Theorem (Reynolds 1983):** Polymorphic functions satisfy relational
parametricity - they must work uniformly across all type instantiations.

**Example:** For `f : forall a. a -> a`, the only possible implementation
is the identity function.

**Free theorem for `forall a. [a] -> [a]`:** Any such function must
produce a permutation/selection of input elements; it cannot fabricate
new elements or inspect their values.

**Implication:** Types provide strong guarantees about behavior without
examining implementations.

## Relationship to Simply-Typed Lambda Calculus

### Simply-Typed Lambda Calculus (STLC)

STLC is the monomorphic foundation:

```
Types:   tau ::= B | tau_1 -> tau_2       (B = base types)
Terms:   e   ::= x | \x:tau. e | e_1 e_2
```

**Limitations:**
- No polymorphism: `\x. x` must have a specific type like `Int -> Int`
- Code duplication: separate identity functions for each type
- No generic data structures

### System F as Extension

System F extends STLC with:

| Feature | STLC | System F |
|---------|------|----------|
| Type variables | No | Yes (`alpha`) |
| Universal types | No | Yes (`forall alpha. tau`) |
| Type abstraction | No | Yes (`/\alpha. e`) |
| Type application | No | Yes (`e [tau]`) |

**The identity function:**
- STLC: Multiple versions (`\x:Int. x`, `\x:Bool. x`, etc.)
- System F: Single polymorphic version `/\alpha. \x:alpha. x : forall alpha. alpha -> alpha`

### The Lambda Cube

System F occupies a specific position in Barendregt's lambda cube:

```
                    lambda-C (CoC)
                   /           \
              lambda-P2         lambda-omega
             /        \       /        \
       lambda-P    lambda-P-omega    lambda-2 (System F)
            \        /       \        /
             lambda->          lambda-omega-
                   \           /
                    lambda-> (STLC)
```

**Axes:**
1. Terms depending on types (polymorphism) - System F adds this
2. Types depending on types (type operators) - System F-omega adds this
3. Types depending on terms (dependent types) - lambda-P adds this

## Hindley-Milner: A Decidable Fragment

### Restricting System F

Hindley-Milner (HM) restricts System F to achieve decidable type inference:

**HM Type Syntax:**

```
Monotypes:    tau   ::= alpha | C | tau_1 -> tau_2 | T tau_1 ... tau_n
Polytypes:    sigma ::= tau | forall alpha. sigma
```

**Key restriction:** Universal quantifiers only at the outermost level
(prenex form). No types like `(forall a. a -> a) -> Int`.

### Rank-1 Polymorphism

HM = Rank-1 polymorphism:
- Rank-0: No quantifiers (monomorphic)
- Rank-1: Quantifiers only at outermost level (HM)
- Rank-2: Quantifiers may appear to the left of one arrow
- Rank-n: Quantifiers may appear nested to depth n
- Rank-omega: Full System F (unrestricted)

**Example ranks:**
```
Rank-0:  Int -> Bool
Rank-1:  forall a. a -> a
Rank-2:  (forall a. a -> a) -> Int
Rank-3:  ((forall a. a -> a) -> Int) -> Bool
```

### Let-Polymorphism

HM achieves polymorphism through `let`:

```
Gamma |- e_1 : tau    Gamma, x:Gen(tau, Gamma) |- e_2 : sigma
------------------------------------------------------------- (Let)
Gamma |- let x = e_1 in e_2 : sigma
```

Where `Gen(tau, Gamma)` generalizes free type variables in `tau` that don't
appear free in `Gamma`:

```
Gen(tau, Gamma) = forall alpha_1...alpha_n. tau
  where {alpha_1, ..., alpha_n} = FTV(tau) - FTV(Gamma)
```

**The let-expansion property:** `let x = e_1 in e_2` is semantically equivalent
to `e_2[e_1/x]`, but typing is not preserved by this transformation in HM.
This is by design: `let` enables controlled polymorphism.

### Algorithm W

Milner's Algorithm W computes principal types:

```
W(Gamma, x) =
  if x:sigma in Gamma then
    let forall alpha_1...alpha_n. tau = sigma
    return (id, tau[beta_1/alpha_1, ..., beta_n/alpha_n])  -- fresh beta_i
  else fail

W(Gamma, \x. e) =
  let beta = fresh_typevar()
  let (S, tau) = W(Gamma + {x:beta}, e)
  return (S, S(beta) -> tau)

W(Gamma, e_1 e_2) =
  let (S_1, tau_1) = W(Gamma, e_1)
  let (S_2, tau_2) = W(S_1(Gamma), e_2)
  let beta = fresh_typevar()
  let S_3 = unify(S_2(tau_1), tau_2 -> beta)
  return (S_3 . S_2 . S_1, S_3(beta))

W(Gamma, let x = e_1 in e_2) =
  let (S_1, tau_1) = W(Gamma, e_1)
  let sigma = Gen(tau_1, S_1(Gamma))
  let (S_2, tau_2) = W(S_1(Gamma) + {x:sigma}, e_2)
  return (S_2 . S_1, tau_2)
```

**Properties:**
- Sound and complete for HM
- Returns principal (most general) type
- Complexity: Nearly linear in practice, exponential worst case

### Value Restriction

**The problem:** Unrestricted let-generalization is unsound with mutable
references.

```
let r = ref [] in           -- r : forall a. Ref [a]  (unsound!)
  r := [1];                  -- instantiate a = Int
  head (!r) + "hello"        -- instantiate a = String, type error at runtime
```

**The ML value restriction:** Only generalize let-bindings where the RHS is
a syntactic value (variable, lambda, constructor application to values).

**For Elisp:** Without mutable references in the type system, value restriction
may be relaxed. However, if `setq` side effects are modeled, similar care is
needed.

## Higher-Rank Polymorphism

### When Higher-Rank is Needed

Higher-rank types enable:

1. **ST monad pattern:** `runST : forall a. (forall s. ST s a) -> a`
2. **Existential encoding:** `exists a. (a, a -> Int)` as `forall r. (forall a. a -> (a -> Int) -> r) -> r`
3. **Church encodings:** Data structures as their eliminators

### Bidirectional Type Checking

For rank-2 and beyond, bidirectional checking provides practical inference:

**Judgments:**
- `Gamma |- e => tau` (inference/synthesis mode)
- `Gamma |- e <= tau` (checking mode)

**Key rules:**

```
Gamma |- e => tau_1    tau_1 <: tau_2
-------------------------------------- (Sub)
Gamma |- e <= tau_2

Gamma, x:tau_1 |- e <= tau_2
--------------------------------- (Abs-Check)
Gamma |- \x. e <= tau_1 -> tau_2

Gamma |- e_1 => tau_1 -> tau_2    Gamma |- e_2 <= tau_1
-------------------------------------------------------- (App)
Gamma |- e_1 e_2 => tau_2

Gamma |- e => tau
----------------------- (Annot)
Gamma |- (e : tau) <= tau
```

**Practical implication:** Higher-rank arguments require type annotations, but
these propagate inward eliminating most annotation burden.

### Complete and Easy Bidirectional Checking

Dunfield and Krishnaswami's "Complete and Easy Bidirectional Typechecking for
Higher-Rank Polymorphism" (2013) provides an algorithm that:

- Handles arbitrary-rank polymorphism
- Requires annotations only at higher-rank introductions
- Uses an ordered context for constraint solving
- Is proven sound and complete

**Recommendation for Elisp:** Start with HM (rank-1), but design the type
representation to accommodate higher-rank if needed for advanced patterns.

## Implementation Considerations for Elisp

### Type Representation

```ocaml
type typ =
  | TVar of string                    (* Type variable *)
  | TCon of string                    (* Type constant: Int, Bool, etc. *)
  | TArr of typ * typ                 (* Function type *)
  | TForall of string * typ           (* Universal type *)
  | TApp of typ * typ list            (* Type constructor application *)
  | TTuple of typ list                (* Product types *)
  | TSum of (string * typ list) list  (* Sum types (ADTs) *)
```

### Substitution Implementation

Use explicit substitutions or de Bruijn indices for efficiency:

**Named variables (simpler):**
- Pro: Readable error messages, matches source
- Con: Alpha-renaming needed during substitution

**De Bruijn indices (more efficient):**
- Pro: No capture-avoiding substitution needed
- Con: Harder error messages, complex term manipulation

**Recommendation:** Named variables with careful alpha-renaming for a
type checker focused on error messages.

### Unification

Robinson's unification algorithm:

```
unify(alpha, tau) =
  if alpha in FTV(tau) then fail "occurs check"
  else return [tau/alpha]

unify(tau, alpha) = unify(alpha, tau)

unify(C, C) = return id

unify(tau_1 -> tau_2, tau_3 -> tau_4) =
  let S_1 = unify(tau_1, tau_3)
  let S_2 = unify(S_1(tau_2), S_1(tau_4))
  return S_2 . S_1

unify(T tau_1...tau_n, T sigma_1...sigma_n) =
  unify-list([tau_1,...,tau_n], [sigma_1,...,sigma_n])

unify(_, _) = fail "type mismatch"
```

**Occurs check:** Essential for termination. Prevents infinite types like
`alpha = alpha -> Int`.

### Constraint-Based Inference

Alternative to Algorithm W - separate constraint generation from solving:

1. **Generate constraints:** Traverse term, emit equations `tau_1 = tau_2`
2. **Solve constraints:** Unify all equations to find substitution

**Advantages:**
- Cleaner separation of concerns
- Better error locations (constraint generation preserves source info)
- Easier to extend with subtyping constraints

### Error Message Quality

**Strategies:**

1. **Track source locations:** Every type and constraint carries source span
2. **Preserve user names:** Don't rename type variables unnecessarily
3. **Explain unification failures:** Show both types and why they conflict
4. **Suggest fixes:** Common mistakes have known solutions

**Example error format:**
```
Error at foo.el:42:10

Type mismatch in function application:

  (mapcar f xs)
          ^

Expected: (a -> b)
Found:    String

The first argument to 'mapcar' must be a function.
```

## Theorems and Their Implications

### For Type Checker Correctness

| Theorem | Implication |
|---------|-------------|
| Type Preservation | Type errors don't appear during execution |
| Progress | Well-typed closed terms don't get stuck |
| Principal Types (HM) | Algorithm W finds the most general type |
| Decidability (HM) | Type inference always terminates |

### For User Experience

| Theorem | Implication |
|---------|-------------|
| Parametricity | Types document behavior guarantees |
| Strong Normalization | (For core calculus) no infinite loops |
| Undecidability (System F) | Some annotations required for expressiveness |

## Summary: Design Recommendations for Elisp Type Checker

1. **Start with HM:** Decidable, well-understood, good inference
2. **Prepare for higher-rank:** Design types to accommodate `forall` anywhere
3. **Use constraint-based inference:** Better error messages
4. **Implement bidirectional checking:** Enables higher-rank when needed
5. **Named type variables:** Prioritize error message quality
6. **No value restriction initially:** Elisp lacks typed mutable references
7. **Phantom types for ADTs:** Sum types exist only at compile time

## References

### Primary Sources

1. Girard, J-Y. (1972). "Interpretation fonctionnelle et elimination des
   coupures de l'arithmetique d'ordre superieur." These d'Etat.

2. Reynolds, J.C. (1974). "Towards a theory of type structure." Colloque sur
   la Programmation, Paris.

3. Milner, R. (1978). "A Theory of Type Polymorphism in Programming." JCSS 17.

4. Damas, L. & Milner, R. (1982). "Principal type-schemes for functional
   programs." POPL '82.

5. Wells, J.B. (1999). "Typability and Type Checking in System F are Equivalent
   and Undecidable." Annals of Pure and Applied Logic 98.

### Implementation References

6. Pierce, B.C. (2002). "Types and Programming Languages." MIT Press.
   - Chapters 22-23: System F and variants
   - Chapters 9-11: Simply typed lambda calculus

7. Dunfield, J. & Krishnaswami, N. (2013). "Complete and Easy Bidirectional
   Typechecking for Higher-Rank Polymorphism." ICFP '13.

8. Odersky, M. & Laufer, K. (1996). "Putting Type Annotations to Work." POPL '96.

9. Peyton Jones, S. et al. (2007). "Practical type inference for arbitrary-rank
   types." JFP 17(1).

### Parametricity

10. Reynolds, J.C. (1983). "Types, Abstraction and Parametric Polymorphism."
    Information Processing 83.

11. Wadler, P. (1989). "Theorems for free!" FPCA '89.
