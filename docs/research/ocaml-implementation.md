# OCaml Type System Implementation

Research notes on OCaml's type system implementation, focusing on aspects
relevant to building a type checker for Emacs Lisp.

## Type System Features

### Hindley-Milner Inference with Extensions

OCaml's type inference is based on the Hindley-Milner (HM) algorithm, which
infers the most general (principal) type for expressions without requiring
explicit type annotations. The algorithm has been extended significantly from
its original formulation.

**Core algorithm phases:**

1. **Constraint generation**: Walk the AST and collect type equations
2. **Unification**: Solve the constraint set to find a most general unifier
3. **Generalization**: Convert solved type variables to polymorphic types

**Let-polymorphism** is central to HM: only let-bound variables may have
polymorphic types. Lambda-bound variables are monomorphic within their scope.
This restriction keeps type inference decidable (unlike System F where inference
is undecidable).

```ocaml
(* Let-bound: polymorphic *)
let id = fun x -> x in
(id 1, id "hello")  (* Both applications valid *)

(* Lambda-bound: monomorphic *)
(fun id -> (id 1, id "hello"))  (* Type error *)
```

**Level-based generalization** (Didier Remy, 1988): Instead of scanning the
entire type environment to determine which type variables can be generalized,
OCaml tracks "levels" for each type variable. Variables at deeper levels than
the current let-binding can be generalized. This approach:

- Runs in near-linear time (avoiding quadratic scanning)
- Detects escaping type variables (types defined locally escaping their scope)
- Extends naturally to existentials and universals
- Resembles region-based memory management (variables "live" at certain levels)

```
Level tracking example:
  let x =           (* level 1 *)
    let y =         (* level 2 *)
      ...           (* type variable 'a created at level 2 *)
    in y            (* 'a generalized if level > 1 *)
  in ...
```

### Module System and Functors

OCaml's module system provides type abstraction at a higher level than the core
language.

**Key concepts:**

- **Signatures**: Specify module interfaces with types, values, and submodules
- **Structures**: Implement signatures with concrete definitions
- **Functors**: Functions from modules to modules (parameterized modules)
- **Abstract types**: Types whose representation is hidden behind a signature

```ocaml
module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module MakeSet (Elt : ORDERED) = struct
  type t = Elt.t list
  let empty = []
  let add x s = x :: s
  (* ... *)
end
```

**Applicative vs. generative functors:**

- **Applicative** (default): Same arguments yield compatible abstract types
- **Generative**: Each application produces incompatible types (use `()` arg)

Generative functors are appropriate when the functor body is effectful or
dynamically chooses type implementations.

**Type sharing constraints** (`with type`): Expose abstract types when needed:

```ocaml
module IntSet = MakeSet(Int)
(* IntSet.Elt.t is abstract outside *)

module IntSet : SET with type elt = int = MakeSet(Int)
(* IntSet.elt is known to be int *)
```

### GADTs (Generalized Algebraic Data Types)

GADTs extend algebraic data types by allowing constructors to constrain the type
parameters of the resulting type.

```ocaml
type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
```

**Implementation considerations:**

- Pattern matching on GADTs refines types in branches
- Requires explicit polymorphic type annotations (locally abstract types)
- Type information flows bidirectionally (propagation required)

```ocaml
let rec eval : type a. a expr -> a = function
  | Int n -> n           (* return type refined to int *)
  | Bool b -> b          (* return type refined to bool *)
  | Add (e1, e2) -> eval e1 + eval e2
  | If (c, t, e) -> if eval c then eval t else eval e
```

**Limitations with polymorphic variants:**

- Historically, GADTs and polymorphic variants had limited interaction
- Recent OCaml versions improved this but with some typing consequences
- Type propagation may still be restricted in certain cases

### Polymorphic Variants

Polymorphic variants provide structural (rather than nominal) sum types:

```ocaml
(* No declaration needed *)
let f = function
  | `Red -> 255
  | `Green -> 128
  | `Blue -> 0

(* Inferred type: [< `Blue | `Green | `Red ] -> int *)
```

**Variance markers:**

- `[< ...]`: upper bound (accepts at most these constructors)
- `[> ...]`: lower bound (accepts at least these constructors)
- `[= ...]`: exact set

**Trade-offs:**

- More flexible than nominal variants
- Weaker type discipline (no exhaustiveness guarantees across modules)
- Useful for extensible ASTs and error handling

## Implementation Architecture

### Compiler Phases

OCaml compilation proceeds through well-defined phases:

```
Source (.ml/.mli)
    |
    v
[1. Parsing] --> Parsetree (untyped AST)
    |
    v
[2. PPX rewriters] --> Modified Parsetree
    |
    v
[3. Type checking] --> Typedtree (typed AST)
    |
    v
[4. Lambda] --> Simplified IR (types erased)
    |
    v
[5. Backend] --> Bytecode or Native code
```

**Key directories in OCaml source:**

- `parsing/`: Lexer, parser, Parsetree definition
- `typing/`: Type checker, Types module, inference
- `lambda/`: Lambda IR, pattern match compilation
- `bytecomp/`: Bytecode generation
- `asmcomp/`: Native code generation
- `middle_end/`: Flambda optimizations (optional)
- `driver/`: Compilation orchestration

### Type Representation in the Compiler

The core type representation lives in `typing/types.mli`:

```ocaml
type type_expr = {
  mutable desc : type_desc;
  mutable level : int;        (* For generalization *)
  mutable scope : int;
  id : int;
}

and type_desc =
  | Tvar of string option     (* Type variable *)
  | Tarrow of ...             (* Function type *)
  | Ttuple of type_expr list  (* Tuple type *)
  | Tconstr of ...            (* Named type constructor *)
  | Tlink of type_expr        (* Unification indirection *)
  | Tsubst of ...             (* Temporary during substitution *)
  | Tpoly of ...              (* Polymorphic type *)
  | Tpackage of ...           (* First-class module *)
  | ...
```

**Key implementation details:**

- `Tlink`: Indirection node for unification (union-find style)
- `level`: Tracks binding depth for generalization
- Mutable fields enable efficient in-place unification
- `Tsubst` used temporarily during type copying/instantiation

**Weak type variables** (`'_weak`): Variables that cannot be generalized yet:

```ocaml
let r = ref []  (* val r : '_weak1 list ref *)
(* Type not yet determined; will be fixed on first use *)
```

### Constraint Solving Approach

OCaml uses unification-based constraint solving:

**Unification algorithm:**

1. If constraints empty, return empty substitution
2. If both sides are identical base types, recurse on remaining constraints
3. If one side is a type variable not occurring in the other, substitute
4. If both sides are type constructors, unify arguments pairwise
5. Otherwise, fail (type error)

```ocaml
(* Simplified unification *)
let rec unify t1 t2 =
  match (t1.desc, t2.desc) with
  | Tvar _, _ when not (occurs t1 t2) ->
      t1.desc <- Tlink t2
  | _, Tvar _ when not (occurs t2 t1) ->
      t2.desc <- Tlink t1
  | Tarrow (l1, r1), Tarrow (l2, r2) ->
      unify l1 l2; unify r1 r2
  | Tconstr (p1, args1), Tconstr (p2, args2) when p1 = p2 ->
      List.iter2 unify args1 args2
  | _ -> raise (Unification_error (t1, t2))
```

**Occurs check**: Prevents infinite types (e.g., `'a = 'a -> 'b`)

**Path compression**: Following `Tlink` chains and shortening them

### Error Message Generation

Type error messages are a known challenge in ML-family languages. OCaml's
approach:

**Standard error format:**

```
This expression has type X
but an expression was expected of type Y
```

**Challenges:**

- Left-to-right bias in inference can misplace blame
- Higher-order functions make errors harder to localize
- Polymorphism can cause confusing "type X is not compatible with type X"

**Research improvements** (Chargueraud, 2015):

- Catch unification errors and generate better messages
- Report expected vs. actual types in tables
- Save type copies before unification to show original types
- Detect common mistakes: missing `()`, missing `!`, missing `rec`

**Implementation approach:**

```ocaml
(* Pseudo-code for improved error handling *)
try
  unify_application func_type arg_types
with Unification_error _ ->
  (* Fall back to detailed error generation *)
  let expected = save_types func_params in
  let actual = save_types arg_types in
  report_mismatch ~expected ~actual ~location
```

## OCaml as Implementation Language

### Why OCaml Is Popular for Type Checkers

**Type system advantages:**

- Strong static typing catches bugs at compile time
- Exhaustive pattern matching ensures all cases handled
- Algebraic data types naturally represent ASTs
- Type inference reduces annotation burden

**Pattern matching for compilers:**

```ocaml
let rec typecheck env = function
  | Var x -> lookup env x
  | App (f, arg) ->
      let tf = typecheck env f in
      let ta = typecheck env arg in
      unify tf (Arrow (ta, fresh_var ()));
      ...
  | Lambda (x, body) ->
      let tv = fresh_var () in
      let tbody = typecheck ((x, tv) :: env) body in
      Arrow (tv, tbody)
  | Let (x, e1, e2) ->
      let t1 = typecheck env e1 in
      let t1_gen = generalize env t1 in
      typecheck ((x, t1_gen) :: env) e2
```

**Exhaustiveness checking**: Compiler warns about missing cases:

```ocaml
(* Warning 8: this pattern-matching is not exhaustive *)
let f = function
  | Int _ -> "int"
  | Bool _ -> "bool"
  (* Missing: Add, If *)
```

**Performance:**

- OCaml is "basically as fast as C" (Xavier Leroy)
- Optimizing compiler with good code generation
- Efficient memory representation with minimal boxing

### Ecosystem for Building Type Systems

**Package manager (opam):**

- Mature ecosystem with thousands of packages
- Easy dependency management
- Compiler switches for testing multiple OCaml versions

**Build system (dune):**

- Declarative, composable build specifications
- First-class support for Menhir parser generator
- Fast incremental builds
- Good IDE integration

**Parser generators:**

- **Menhir**: LR(1) parser generator, superior to ocamlyacc
  - Better error messages
  - Incremental parsing API
  - Parameterized grammars
  - Generates efficient, well-typed code

```ocaml
(* dune file *)
(menhir
 (modules parser))

(ocamllex lexer)
```

- **ocamllex**: Standard lexer generator, works well with Menhir

**Other useful libraries:**

- **ppx**: Metaprogramming via AST transformations
- **cmdliner**: Command-line parsing
- **fmt**: Pretty-printing
- **sedlex**: Unicode-aware lexer generator

### Notable Type Checkers Written in OCaml

- **Flow**: Facebook's static type checker for JavaScript
- **Hack**: Facebook's typed PHP variant
- **Rocq (Coq)**: Proof assistant
- **F***: Verification-oriented language
- **Reason**: Alternative OCaml syntax

## LSP Tooling

### ocaml-lsp-server Architecture

The OCaml LSP server provides IDE features through the Language Server Protocol.

**Package structure:**

- `jsonrpc`: IO-agnostic protocol serialization
- `lsp`: LSP-specific types and semantics
- `ocaml-lsp-server`: Actual server implementation

**Core components:**

```
Server.start
    |
    v
Document_store (tracks open files)
    |
    v
Configuration (user settings)
    |
    v
Merlin integration (type queries)
    |
    v
Dune integration (build info)
```

**Merlin integration:**

- Merlin is vendored (not a separate dependency)
- One Merlin pipeline per document
- Queries dispatched via `Document.Merlin.dispatch_exn`
- Provides: completion, type info, go-to-definition, error checking

### Incremental Compilation

**Dune watch mode integration:**

- OCaml-LSP communicates with Dune's RPC system
- Diagnostics updated after each build
- Users run `dune build --watch` alongside editor

**Semantic highlighting:**

- Experimental incremental semantic tokens
- Full mode: complete re-computation
- Delta mode: incremental updates (faster but less stable)

**File tracking:**

- LSP server maintains file state from last build
- Changes require rebuild to update type information
- Watch mode keeps information current

### IDE Features Implementation

**Type information on hover:**

- Query Merlin for type at position
- Merlin maintains typed AST for each file
- Returns instantiated type (not just declared type)

**Completion:**

- Context-sensitive using Merlin's analysis
- Considers scope, expected types, and documentation

**Go-to-definition:**

- Uses `.cmt` files (typed AST with locations)
- Handles cross-module references
- Distinguishes interface vs. implementation

**Error diagnostics:**

- Type errors from compiler/Merlin
- Syntax errors from parser
- Updated on save or in watch mode

## Key Takeaways for Tart Implementation

1. **Level-based generalization** is essential for efficient HM inference

2. **Mutable type representation** with union-find enables efficient unification

3. **Separate constraint generation from solving** for cleaner architecture

4. **Exhaustive pattern matching** catches implementation bugs early

5. **Good error messages require extra infrastructure** (saving types, tracking
   provenance)

6. **Menhir + ocamllex** provide excellent parsing infrastructure

7. **LSP architecture**: separate protocol handling from language-specific logic

8. **Incremental compilation** requires careful state management

## References

- [Efficient and Insightful Generalization](https://okmij.org/ftp/ML/generalization.html) - Oleg Kiselyov
- [The Compiler Frontend: Parsing and Type Checking](https://dev.realworldocaml.org/compiler-frontend.html) - Real World OCaml
- [OCaml Type Inference](https://cs3110.github.io/textbook/chapters/interp/inference.html) - Cornell CS3110
- [Improving Type Error Messages in OCaml](https://arxiv.org/abs/1512.01897) - Chargueraud
- [GADTs Tutorial](https://ocaml.org/manual/5.4/gadts-tutorial.html) - OCaml Manual
- [Functors](https://ocaml.org/docs/functors) - OCaml Documentation
- [Parsing with OCamllex and Menhir](https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html) - Real World OCaml
- [ocaml-lsp GitHub](https://github.com/ocaml/ocaml-lsp)
- [Merlin](https://ocaml.github.io/merlin/)
- [OCaml types.mli](https://github.com/ocaml/ocaml/blob/trunk/typing/types.mli)
