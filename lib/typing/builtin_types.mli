(** Type signatures for built-in Elisp primitives.

    This module is intentionally minimal. All Elisp function types are defined
    in .tart signature files under typings/emacs/{version}/c-core/.

    Special forms (if, let, cond, etc.) are handled directly by the type
    checker, not via signatures. See Spec 34 for funcall/apply. *)

(** {1 Environment} *)

val initial_env : unit -> Core.Type_env.t
(** Create the initial type environment.

    Returns an empty environment. All function signatures are loaded from .tart
    files in the typings directory at runtime. *)
