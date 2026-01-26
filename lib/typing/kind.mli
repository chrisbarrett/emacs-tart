(** Kind representation for higher-kinded types.

    Kinds classify types the way types classify values:
    - [*] (KStar) is the kind of concrete types like Int, String, (List Int)
    - [* -> *] is the kind of type constructors like List, Option
    - [* -> * -> *] is the kind of two-parameter constructors like HashTable

    This enables polymorphism over type constructors, allowing functions like:
    {[
      (defun fmap [f a b] (((a -> b)) (f a)) -> (f b))
    ]}
    where [f] has kind [* -> *]. *)

(** {1 Kind Representation} *)

type kind =
  | KStar  (** Concrete type kind *)
  | KArrow of kind * kind  (** Type constructor kind *)
[@@deriving show, eq]

(** {1 Kind Variables} *)

type kvar_id = int
(** Unique identifier for kind variables. *)

(** Kind variable state - uses union-find representation like type variables. *)
type kvar =
  | KUnbound of kvar_id  (** Unresolved kind variable *)
  | KLink of kind  (** Unified with another kind *)

(** Kind or kind variable (for inference). *)
type kind_scheme =
  | KConcrete of kind  (** Known kind *)
  | KVar of kvar ref  (** Kind variable for inference *)

(** {1 Construction} *)

val star : kind
(** The kind of concrete types [*]. *)

val ( @-> ) : kind -> kind -> kind
(** [k1 @-> k2] creates a type constructor kind [k1 -> k2]. *)

val arity : int -> kind
(** [arity n] creates a kind for an n-ary type constructor. [arity 0 = *],
    [arity 1 = * -> *], [arity 2 = * -> * -> *], etc. *)

(** {1 Kind Variables} *)

val reset_kvar_counter : unit -> unit
(** Reset the kind variable counter (for testing). *)

val fresh_kvar : unit -> kind_scheme
(** Create a fresh unbound kind variable. *)

(** {1 Operations} *)

val repr_scheme : kind_scheme -> kind_scheme
(** Follow links to find the representative kind scheme. *)

val to_kind : kind_scheme -> kind option
(** Extract concrete kind from scheme, or None if still a variable. *)

val default_to_star : kind_scheme -> kind
(** Default unconstrained kind variables to [*]. *)

(** {1 Pretty-printing} *)

val to_string : kind -> string
(** Pretty-print a kind to string. *)

val scheme_to_string : kind_scheme -> string
(** Pretty-print a kind scheme to string. *)

(** {1 Kind Environment}

    A kind environment maps type variable names to their kind schemes. This is
    used during kind inference to track the kinds of type variables.

    When a type variable is looked up but not found, it defaults to kind [*].
    This ensures backward compatibility: existing signatures that don't use
    higher-kinded types continue to work unchanged. *)

type env
(** Kind environment mapping type variable names to kind schemes. *)

val empty_env : env
(** Empty kind environment. *)

val extend_env : string -> kind_scheme -> env -> env
(** [extend_env name ks env] adds a binding from [name] to [ks]. *)

val extend_star : string -> env -> env
(** [extend_star name env] extends [env] with [name] at kind [*]. *)

val extend_fresh : string list -> env -> env
(** [extend_fresh names env] extends [env] with fresh kind variables for each
    name. *)

val lookup : string -> env -> kind_scheme
(** [lookup name env] returns the kind scheme for [name], or a fresh kind
    variable if not found. *)

val lookup_defaulted : string -> env -> kind
(** [lookup_defaulted name env] returns the kind for [name], defaulting
    unconstrained variables to [*]. This is the main interface for getting the
    final kind of a type variable. *)

val default_all : env -> unit
(** [default_all env] defaults all unresolved kind variables in [env] to [*].
    Call this after kind inference is complete to finalize all kinds. *)

val names : env -> string list
(** [names env] returns all type variable names in the environment. *)
