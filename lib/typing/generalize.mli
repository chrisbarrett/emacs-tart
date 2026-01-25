(** Levels-based let-generalization for Hindley-Milner type inference.

    This module implements the generalization step of HM inference using
    the levels-based approach. Type variables are generalized (become
    polymorphic) only if their level is greater than the current scope level.

    The key insight is that type variables with level > current_level were
    created inside the current let binding and are safe to generalize.

    Reference: "How OCaml type checker works" by Oleg Kiselyov *)

(** {1 Value restriction} *)

val is_syntactic_value : Syntax.Sexp.t -> bool
(** Check if an expression is a syntactic value (eligible for generalization).

    The value restriction prevents unsound generalization of expressions that
    might have side effects. Only syntactic values can be generalized:
    - Lambda expressions
    - Literals (numbers, strings, etc.)
    - Variables
    - Quoted expressions
    - Vectors of values

    Non-values (like function applications) remain monomorphic. *)

(** {1 Generalization} *)

val generalize : int -> Core.Types.typ -> Core.Type_env.scheme
(** [generalize level ty] generalizes a type at the given level.

    Finds all type variables with level > [level], and wraps the type
    in a forall quantifier binding those variables.

    Returns a [Mono] scheme if no variables can be generalized, or a [Poly]
    scheme with the generalized type. *)

val generalize_if_value : int -> Core.Types.typ -> Syntax.Sexp.t -> Core.Type_env.scheme
(** [generalize_if_value level ty expr] generalizes only if [expr] is a syntactic value.

    This implements the value restriction: non-values remain monomorphic
    to ensure soundness in the presence of mutable state. *)
