(** Runtime value representation for Elisp interpreter.

    This module defines the tagged value representation used by the interpreter.
    Values mirror Emacs's internal types, with closures capturing lexical
    environments for proper scoping.

    This is a pure interpreter - values requiring effects (buffers, processes,
    etc.) are represented as opaque boundaries. *)

(** {1 Types} *)

type params = {
  required : string list;  (** Required positional parameters *)
  optional : (string * value option) list;
      (** [&optional] params with defaults *)
  rest : string option;  (** [&rest] parameter name *)
}
(** Parameter specification for functions. *)

and env = (string * value ref) list list
(** Lexical environment: a list of scopes, each scope is a list of bindings. *)

and closure = {
  params : params;
  body : Syntax.Sexp.t list;  (** Body forms *)
  env : env;  (** Captured lexical environment *)
  name : string option;  (** Optional name for error messages *)
}
(** A closure captures its parameter list, body, and lexical environment. *)

and builtin = {
  builtin_name : string;
  builtin_arity : int * int option;  (** (min, max) - None means variadic *)
  builtin_fn : value list -> (value, string) result;
}
(** A built-in function implemented in OCaml. *)

and macro = {
  macro_name : string;
  macro_params : params;
  macro_body : Syntax.Sexp.t list;
  macro_env : env;
}
(** Macro representation. *)

(** Elisp runtime values. *)
and value =
  | Nil
  | T
  | Int of int
  | Float of float
  | String of string
  | Symbol of string
  | Keyword of string  (** Keywords like [:foo] *)
  | Cons of value * value
  | Vector of value array
  | Closure of closure
  | Builtin of builtin
  | Macro of macro
  | Opaque of string  (** Opaque boundary - requires type annotation *)

type t = value
(** Alias for value for external use. *)

(** {1 Comparison} *)

val equal : value -> value -> bool
(** Structural equality for values. Closures never compare equal. *)

val equal_params : params -> params -> bool
val equal_closure : closure -> closure -> bool
val equal_builtin : builtin -> builtin -> bool
val equal_macro : macro -> macro -> bool
val equal_env : env -> env -> bool

(** {1 Pretty-printing} *)

val pp : Format.formatter -> value -> unit
val show : value -> string
val pp_params : Format.formatter -> params -> unit
val pp_closure : Format.formatter -> closure -> unit
val pp_builtin : Format.formatter -> builtin -> unit
val pp_macro : Format.formatter -> macro -> unit
val pp_env : Format.formatter -> env -> unit

(** {1 Conversion} *)

val to_string : value -> string
(** Pretty-print a value in Elisp syntax. *)

val to_list : value -> value list option
(** Convert a proper list value to an OCaml list. Returns [None] for improper
    lists or non-list values. *)

val of_list : value list -> value
(** Convert an OCaml list to a proper Elisp list value. *)

val to_alist : value -> (value * value) list option
(** Convert value to association list (list of cons cells). Returns [None] if
    the value is not a proper alist. *)

(** {1 Type inspection} *)

val type_name : value -> string
(** Get the type name of a value for error messages. *)

val is_truthy : value -> bool
(** Check if a value is truthy (non-nil). *)

val is_nil : value -> bool
(** Check if a value is nil. *)

(** {1 Environment operations} *)

val empty_env : env
(** Create an empty environment. *)

val push_scope : env -> env
(** Push a new scope onto the environment. *)

val bind : string -> value -> env -> env
(** [bind name value env] binds [name] to [value] in the innermost scope. *)

val bind_all : (string * value) list -> env -> env
(** Bind multiple variables in the innermost scope. *)

val lookup : string -> env -> value option
(** Look up a variable in the environment. *)

val set : string -> value -> env -> bool
(** Set a variable in the environment (for setq). Returns [true] if the variable
    was found and updated. *)
