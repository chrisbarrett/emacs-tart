(** Multi-clause function dispatch.

    When a function has multiple signature clauses (from .tart files), this
    module tries each clause top-to-bottom via speculative unification. The
    first clause whose parameters match the call-site argument types wins,
    producing a more precise return type than the merged union type. *)

(** {1 Types} *)

type resolved_diagnostic = {
  rcd_severity : Core.Type_env.diagnostic_severity;
  rcd_message : string;  (** Fully resolved message (no remaining %s) *)
  rcd_span : Syntax.Location.span;  (** Call-site span *)
}
(** A clause diagnostic resolved at a call site.

    When multi-clause dispatch selects a clause with a diagnostic annotation,
    the format string's [%s] placeholders are resolved against the actual types
    inferred at the call site. *)

(** {1 Argument extraction} *)

val extract_arg_literal : Syntax.Sexp.t -> string option
(** Extract the literal value of a call-site argument for clause matching.

    Returns [Some literal] for keywords ([:name]) and quoted symbols (['name]),
    [None] for all other expression forms. *)

(** {1 Type substitution} *)

val substitute_tvar_names :
  (string * Core.Types.typ) list -> Core.Types.typ -> Core.Types.typ
(** [substitute_tvar_names subst ty] replaces [TCon name] with the substitution
    for [name] when present. Used to instantiate clause type parameters. *)

(** {1 Dispatch} *)

val try_dispatch :
  Core.Type_env.t ->
  tvar_names:string list ->
  clauses:Core.Type_env.loaded_clause list ->
  arg_types:Core.Types.typ list ->
  arg_literals:string option list ->
  loc:Syntax.Location.span ->
  (Core.Types.typ * resolved_diagnostic option) option
(** [try_dispatch env ~tvar_names ~clauses ~arg_types ~arg_literals ~loc]
    attempts clause-by-clause dispatch.

    Returns [Some (return_type, diagnostic_opt)] if a clause matched, [None] if
    no clause matched (caller should fall back to union-type constraint path).
*)
