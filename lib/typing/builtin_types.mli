(** Type signatures for built-in Elisp functions.

    This module defines the types for pure built-in functions that are
    available in the interpreter. These types are loaded into the initial
    type environment used for type checking.

    The signatures follow the type system design:
    - Uses Option for nullable returns (e.g., car on a list)
    - Uses polymorphic types where appropriate (e.g., car, cons)
    - Numeric operations use Int for simplicity *)

(** {1 Signatures} *)

val signatures : (string * Core.Type_env.scheme) list
(** All built-in function type signatures.

    Includes:
    - List operations (car, cdr, cons, list, etc.)
    - Arithmetic (+, -, *, /, mod, etc.)
    - Comparisons (<, >, <=, >=, =)
    - Predicates (null, atom, listp, symbolp, etc.)
    - String operations (concat, substring, upcase, etc.)
    - Vector operations (vector, aref, aset)
    - Symbol operations (symbol-name)
    - Type coercion (number-to-string, string-to-number) *)

(** {1 Environment} *)

val initial_env : unit -> Core.Type_env.t
(** Create a type environment with all built-in function signatures. *)
