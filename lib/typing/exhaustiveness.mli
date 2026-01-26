(** Exhaustiveness checking for pcase pattern matching.

    This module checks whether pcase patterns exhaustively cover all
    constructors of an ADT type. Non-exhaustive matches generate warnings.

    To use exhaustiveness checking: 1. Build an [adt_registry] from loaded ADT
    definitions 2. Call [check_exhaustiveness] with the scrutinee type and
    patterns 3. Convert the result to a warning with [warning_of_result] *)

module Types = Core.Types
module Loc = Syntax.Location

(** {1 ADT Registry} *)

type adt_info = {
  adt_name : string;  (** Name of the ADT type *)
  adt_constructors : string list;  (** Names of all constructors *)
}
(** Information about an ADT needed for exhaustiveness checking *)

type adt_registry
(** Registry mapping type constructor names to ADT info *)

val empty_registry : adt_registry
(** Empty registry *)

val register_adt : string -> adt_info -> adt_registry -> adt_registry
(** Register an ADT in the registry.

    [register_adt type_con info registry] adds the ADT with type constructor
    [type_con] (e.g., "mymod/result") and info [info] to the registry. *)

val lookup_adt : string -> adt_registry -> adt_info option
(** Look up an ADT by its type constructor name *)

(** {1 Exhaustiveness Check} *)

(** Result of exhaustiveness checking *)
type exhaustiveness_result =
  | Exhaustive  (** All constructors are covered *)
  | NonExhaustive of string list  (** List of missing constructor names *)
  | NotADT  (** Scrutinee is not a known ADT type *)

val check_exhaustiveness :
  registry:adt_registry ->
  scrutinee_type:Types.typ ->
  patterns:Syntax.Sexp.t list ->
  exhaustiveness_result
(** Check if patterns exhaustively cover an ADT type.

    Returns [Exhaustive] if all constructors are covered (including when there's
    a wildcard pattern), [NonExhaustive] with the list of missing constructors
    otherwise, or [NotADT] if the scrutinee type is not a registered ADT.

    @param registry The ADT registry with known ADT definitions
    @param scrutinee_type The type of the value being matched
    @param patterns The list of pcase clauses (each is a sexp) *)

(** {1 Warnings} *)

type warning = { span : Loc.span; message : string }
(** A warning about non-exhaustive patterns *)

val warning_of_result : span:Loc.span -> exhaustiveness_result -> warning option
(** Generate a warning for non-exhaustive pattern match.

    Returns [Some warning] for [NonExhaustive], [None] otherwise. The warning
    message includes the names of missing constructors. *)

(** {1 Building Registry from Signatures} *)

val build_registry_from_signature : Sig.Sig_ast.signature -> adt_registry
(** Build ADT registry from a signature AST.

    Extracts all data declarations and registers them with their
    module-qualified type constructor name (e.g., "mymod/result"). *)

val merge_registries : adt_registry list -> adt_registry
(** Merge multiple registries into one *)

(** {1 Finding pcase Expressions} *)

type pcase_info = {
  pcase_span : Loc.span;
  pcase_scrutinee : Syntax.Sexp.t;
  pcase_clauses : Syntax.Sexp.t list;
}
(** Information about a pcase expression *)

val find_all_pcases : Syntax.Sexp.t list -> pcase_info list
(** Find all pcase expressions in a list of top-level forms *)

(** {1 Full Exhaustiveness Check} *)

val check_all_pcases :
  registry:adt_registry ->
  env:Core.Type_env.t ->
  Syntax.Sexp.t list ->
  warning list
(** Check exhaustiveness for all pcase expressions in a program.

    Returns a list of warnings for non-exhaustive matches.

    @param registry ADT registry with constructor information
    @param env Type environment for inferring scrutinee types
    @param sexps The program's S-expressions *)
