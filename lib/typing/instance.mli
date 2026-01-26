(** Type class instance resolution.

    This module handles instance registry and constraint resolution for type
    classes. Instances are collected from signature files and stored in a
    registry. When a constrained function is called, the resolver checks that
    appropriate instances exist for all required constraints. *)

(** {1 Instance Registry} *)

type instance = {
  inst_class : string;  (** Class name (e.g., "Eq", "Functor") *)
  inst_type : Core.Types.typ;  (** Instance head type (e.g., Int, (List a)) *)
  inst_tvars : string list;
      (** Bound type variables for parameterized instances *)
  inst_constraints : Core.Types.type_constraint list;
      (** Required constraints (e.g., [(Eq, a)] for (Eq (list a))) *)
}
(** A loaded instance ready for resolution. *)

type registry
(** Instance registry: maps class names to their instances. *)

val empty_registry : registry
(** Empty registry *)

val add_instance : instance -> registry -> registry
(** Add an instance to the registry *)

val instances_for_class : string -> registry -> instance list
(** Get all instances for a given class *)

val all_instances : registry -> instance list
(** Get all instances in the registry *)

val merge : registry -> registry -> registry
(** Merge two registries. Instances from the second override instances from the
    first if they overlap. *)

(** {1 Instance Resolution} *)

(** Resolution result *)
type resolution_result =
  | Resolved  (** Instance found and constraints satisfied *)
  | NotFound of string * Core.Types.typ  (** No instance for (class, type) *)
  | Recursive of Core.Types.type_constraint list
      (** Instance found but requires these constraints to be resolved *)

val resolve_constraint :
  registry -> Core.Types.type_constraint -> resolution_result
(** Resolve a single constraint against the registry.

    Returns Resolved if an instance exists, NotFound if no instance exists, or
    Recursive if the instance requires additional constraints. *)

val resolve_all :
  registry ->
  Core.Types.type_constraint list ->
  (unit, string * Core.Types.typ) result
(** Resolve all constraints, recursively resolving any sub-constraints.

    Returns [Ok ()] if all constraints can be satisfied, or
    [Error (class, type)] with the first unsatisfied constraint. *)

(** {1 Registry Building} *)

val load_instance : Sig.Sig_ast.instance_decl -> registry -> registry
(** Load an instance declaration into the registry *)

(** {1 Debugging} *)

val instance_to_string : instance -> string
(** Pretty-print an instance for debugging *)

val registry_to_string : registry -> string
(** Pretty-print the registry for debugging *)
