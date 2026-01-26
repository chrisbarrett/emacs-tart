(** Environment management for the Elisp interpreter.

    The environment tracks:
    - Lexical bindings (let, lambda parameters)
    - Global variable definitions (defvar, defconst)
    - Macro definitions (defmacro)
    - Special variable declarations

    This module provides a higher-level interface over {!Value.env}. *)

(** {1 Types} *)

type global = {
  mutable globals : (string, Value.value) Hashtbl.t;
      (** Global variable bindings *)
  mutable macros : (string, Value.macro) Hashtbl.t;  (** Macro definitions *)
  mutable specials : (string, bool) Hashtbl.t;
      (** Special (dynamic) variables *)
}
(** The global interpreter state. *)

(** {1 Global state management} *)

val make_global : unit -> global
(** Create a fresh global state. *)

val default_global : global
(** Default global state (shared, use with care). *)

(** {1 Variable operations} *)

val lookup_var : string -> Value.env -> global -> Value.value option
(** Look up a variable, checking lexical env first, then globals. *)

val set_var : string -> Value.value -> Value.env -> global -> unit
(** Set a variable - lexical if bound, otherwise global. *)

val define_global : string -> Value.value -> global -> unit
(** Define a global variable. *)

(** {1 Macro operations} *)

val define_macro : string -> Value.macro -> global -> unit
(** Define a macro. *)

val lookup_macro : string -> global -> Value.macro option
(** Look up a macro by name. *)

val is_macro : string -> global -> bool
(** Check if a symbol names a macro. *)

(** {1 Special variables} *)

val declare_special : string -> global -> unit
(** Declare a variable as special (dynamically scoped). *)

val is_special : string -> global -> bool
(** Check if a variable is special. *)

(** {1 Function call support} *)

val make_call_env :
  Value.params -> Value.value list -> Value.env -> (Value.env, string) result
(** Create a new lexical environment for a function call. Binds parameters to
    arguments according to the parameter spec. Returns [Error] with a message if
    arity doesn't match. *)
