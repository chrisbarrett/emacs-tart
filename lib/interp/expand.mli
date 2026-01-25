(** Macro expansion entry point.

    This module provides the public API for macro expansion, hiding the
    internal evaluation machinery. *)

(** {1 Types} *)

(** Result of macro expansion. *)
type expansion_result =
  | Expanded of Syntax.Sexp.t
  | Expansion_error of { message : string; span : Syntax.Location.span }

(** {1 Expansion functions} *)

val expand_all : Env.global -> Syntax.Sexp.t -> expansion_result
(** [expand_all global sexp] recursively expands all macros in a form.
    Does not expand inside [quote]. *)

val expand_1 : Env.global -> Syntax.Sexp.t -> expansion_result
(** [expand_1 global sexp] expands a single macro call (one step).
    Returns the unchanged form if it's not a macro call. *)

(** {1 Macro management} *)

val load_macros : Env.global -> Syntax.Sexp.t list -> unit
(** [load_macros global sexps] loads macro definitions from parsed forms.
    Only processes [defmacro] forms; other forms are ignored.
    Errors during macro definition are silently ignored. *)

val is_macro_call : Env.global -> Syntax.Sexp.t -> bool
(** [is_macro_call global sexp] checks if a form is a macro call. *)

val register_macro :
  Env.global ->
  string ->
  Value.params ->
  Syntax.Sexp.t list ->
  Value.env ->
  unit
(** [register_macro global name params body env] registers a macro
    from its component parts. *)
