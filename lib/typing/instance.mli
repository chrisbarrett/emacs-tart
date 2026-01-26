(** Type class instance resolution.

    This module handles instance registry and constraint resolution for type
    classes. Instances are collected from signature files and stored in a
    registry. When a constrained function is called, the resolver checks that
    appropriate instances exist for all required constraints. *)

(** {1 Class Registry} *)

type class_info = {
  cls_name : string;  (** Class name (e.g., "Eq", "Ord") *)
  cls_tvar : string;  (** The class type parameter name *)
  cls_superclasses : string list;
      (** Superclass names (e.g., ["Eq"] for Ord which requires Eq) *)
}
(** Information about a type class, including its superclass constraints.

    Used during instance resolution to ensure superclass instances exist. *)

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
(** Instance and class registry: stores both instances and class definitions. *)

val empty_registry : registry
(** Empty registry *)

val add_instance : instance -> registry -> registry
(** Add an instance to the registry *)

val add_class : class_info -> registry -> registry
(** Add a class to the registry *)

val instances_for_class : string -> registry -> instance list
(** Get all instances for a given class *)

val all_instances : registry -> instance list
(** Get all instances in the registry *)

val all_classes : registry -> class_info list
(** Get all classes in the registry *)

val find_class : string -> registry -> class_info option
(** Look up a class by name *)

val merge : registry -> registry -> registry
(** Merge two registries. Items from the second are added after the first. *)

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

    Also checks superclass constraints (R6): when resolving (Ord int), we also
    verify that (Eq int) exists because Eq is a superclass of Ord.

    Returns [Ok ()] if all constraints can be satisfied, or
    [Error (class, type)] with the first unsatisfied constraint. *)

(** {1 Registry Building} *)

val load_instance : Sig.Sig_ast.instance_decl -> registry -> registry
(** Load an instance declaration into the registry *)

val load_class : Sig.Sig_ast.class_decl -> registry -> registry
(** Load a class declaration into the registry.

    Extracts class name, type parameter, and superclass names for use during
    instance resolution. *)

(** {1 Overlap Detection (R9)} *)

type overlap = {
  overlap_class : string;  (** The class where overlap occurs *)
  overlap_inst1 : instance;  (** First overlapping instance *)
  overlap_inst2 : instance;  (** Second overlapping instance *)
}
(** An overlap between two instances. *)

val instances_overlap : instance -> instance -> bool
(** Check if two instances for the same class overlap.

    Two instances overlap if they could both match the same concrete type.
    Example: [(Eq (list int))] and [(Eq (list a))] overlap because the
    parameterized instance can match [(list int)] when [a=int]. *)

val find_overlaps : registry -> overlap list
(** Find all overlapping instance pairs in a registry.

    Returns a list of overlaps. Each pair is reported once (not twice). *)

(** {1 Debugging} *)

val instance_to_string : instance -> string
(** Pretty-print an instance for debugging *)

val registry_to_string : registry -> string
(** Pretty-print the registry for debugging *)

val overlap_to_string : overlap -> string
(** Pretty-print an overlap for debugging *)
