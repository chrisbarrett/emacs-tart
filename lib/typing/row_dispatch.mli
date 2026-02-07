(** Row accessor dispatch for plist-get, alist-get, gethash, and map-elt.

    When a call targets a known row-typed accessor and the key argument is a
    literal, this module implements the decision table from Spec 11 R4-R8
    without falling through to the generic constraint path. *)

(** {1 Accessor configuration} *)

(** Which row-typed accessor is being called. *)
type accessor_kind = PlistGet | AlistGet | Gethash | MapElt

type config = {
  kind : accessor_kind;
  container_arg : int;  (** Index of the container argument (0-based) *)
  key_arg : int;  (** Index of the key argument (0-based) *)
}
(** Configuration for a row-typed accessor function. *)

val get_config : string -> config option
(** [get_config name] returns the accessor configuration for a known row-typed
    accessor function, or [None] if [name] is not a recognized accessor. *)

(** {1 Row extraction} *)

val extract_alist_row : Core.Types.typ -> Core.Types.row option
(** Extract a row type from an alist type: [(list (cons symbol TRow))]. *)

val extract_plist_row : Core.Types.typ -> Core.Types.row option
(** Extract a row type from a plist type: [(Plist k TRow)] or legacy form. *)

val extract_map_row : Core.Types.typ -> Core.Types.row option
(** Extract a row type from a map supertype: [(Map TRow)]. *)

val extract_hash_table_row : Core.Types.typ -> Core.Types.row option
(** Extract a row type from a hash-table type: [(HashTable symbol TRow)]. *)

val extract_row : accessor_kind -> Core.Types.typ -> Core.Types.row option
(** [extract_row kind ty] dispatches to the appropriate row extractor. *)

val build_expected_container : accessor_kind -> Core.Types.typ -> Core.Types.typ
(** [build_expected_container kind row] builds the expected container type for a
    given accessor kind from a row type. *)

val has_default : accessor_kind -> bool
(** Whether an accessor kind supports a DEFAULT argument. *)

(** {1 Dispatch} *)

type dispatch_result = {
  result_ty : Core.Types.typ;
  container_constraint : Constraint.t option;
}
(** Result of row accessor dispatch. *)

val try_dispatch :
  Core.Type_env.t ->
  config ->
  arg_types:Core.Types.typ list ->
  arg_literals:string option list ->
  args:Syntax.Sexp.t list ->
  rest_arg_types:Core.Types.typ list ->
  dispatch_result option
(** [try_dispatch env config ~arg_types ~arg_literals ~args ~rest_arg_types]
    attempts row accessor dispatch.

    - [arg_types]: inferred types of all arguments
    - [arg_literals]: literal values extracted from argument ASTs
    - [args]: argument AST nodes
    - [rest_arg_types]: types of arguments after the container and key (for
      default value handling)

    Returns [Some result] if dispatched, [None] to fall through. *)
