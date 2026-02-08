(** Row accessor dispatch for functions taking row-typed containers.

    When a function call has an argument with a row-typed container (plist,
    alist, hash-table, or map) and a literal key argument, this module
    implements the decision table from Spec 11 R4-R8 to produce precise
    field-level return types.

    The dispatch is signature-driven: rather than recognising hard-coded
    function names, it detects row-typed containers and literal keys from the
    actual argument types at each call site. *)

(** {1 Container kinds} *)

(** Which kind of row-typed container was detected at the call site. *)
type container_kind = Plist | Alist | HashTable | Map

(** {1 Row extraction} *)

val extract_plist_row : Core.Types.typ -> Core.Types.row option
(** Extract a row type from a plist type: [(Plist k TRow)] or legacy form. *)

val extract_alist_row : Core.Types.typ -> Core.Types.row option
(** Extract a row type from an alist type: [(list (cons symbol TRow))]. *)

val extract_hash_table_row : Core.Types.typ -> Core.Types.row option
(** Extract a row type from a hash-table type: [(HashTable symbol TRow)]. *)

val extract_map_row : Core.Types.typ -> Core.Types.row option
(** Extract a row type from a map supertype: [(Map TRow)]. *)

val detect_container_in_type :
  Core.Types.typ -> (container_kind * Core.Types.row) option
(** [detect_container_in_type ty] tries to detect a row-typed container in an
    argument's type. Returns [Some (kind, row)] if the type has a known
    row-typed container type, [None] otherwise. *)

val build_expected_container :
  container_kind -> Core.Types.typ -> Core.Types.typ
(** [build_expected_container kind row] builds the expected container type for a
    given container kind from a row type. *)

val has_default : container_kind -> bool
(** Whether a container kind supports a DEFAULT argument. *)

(** {1 Clause analysis} *)

type clause_config = {
  cc_kind : container_kind;
  cc_container_index : int;
  cc_key_index : int;
}
(** Configuration derived from clause analysis for R8 dispatch. *)

val analyze_clause : Core.Type_env.loaded_clause -> clause_config option
(** [analyze_clause clause] detects the expected container kind and parameter
    layout from a loaded clause's type structure.

    Returns [Some config] if the clause has a row-typed container parameter,
    [None] otherwise. *)

val analyze_fn_type : Core.Types.typ -> clause_config option
(** [analyze_fn_type ty] detects the expected container kind and parameter
    layout from a function type ([TArrow] or [TForall ... TArrow]).

    Used for R8 dispatch when clauses are not stored (single-clause functions
    without diagnostics). *)

(** {1 Dispatch} *)

type dispatch_result = {
  result_ty : Core.Types.typ;
  container_constraint : Constraint.t option;
}
(** Result of row accessor dispatch. *)

val try_dispatch :
  Core.Type_env.t ->
  arg_types:Core.Types.typ list ->
  arg_literals:string option list ->
  args:Syntax.Sexp.t list ->
  dispatch_result option
(** [try_dispatch env ~arg_types ~arg_literals ~args] attempts row accessor
    dispatch by detecting row-typed container arguments and literal keys.

    Returns [Some result] if dispatched, [None] to fall through. *)

val try_dispatch_infer :
  Core.Type_env.t ->
  container_kind:container_kind ->
  container_index:int ->
  key_index:int ->
  arg_types:Core.Types.typ list ->
  arg_literals:string option list ->
  args:Syntax.Sexp.t list ->
  dispatch_result option
(** [try_dispatch_infer env ~container_kind ~container_index ~key_index
     ~arg_types ~arg_literals ~args] attempts row accessor dispatch for the R8
    case where the container type is unknown.

    Requires the container kind from clause analysis to know what type of
    constraint to generate. *)
