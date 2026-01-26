(** Type class instance resolution.

    This module handles instance registry and constraint resolution for type
    classes. Instances are collected from signature files and stored in a
    registry. When a constrained function is called, the resolver checks that
    appropriate instances exist for all required constraints.

    Instance resolution follows Haskell-style dictionary passing semantics:
    - Monomorphic instances (e.g., (Eq int)) match directly
    - Parameterized instances (e.g., (Eq (list a))) require recursive resolution
    - Superclass constraints must also be satisfied *)

open Core.Types

(** Binding operator for Result *)
let ( let* ) = Result.bind

(** {1 Class Registry} *)

(** Information about a type class, including its superclass constraints.

    Used during instance resolution to ensure superclass instances exist. *)
type class_info = {
  cls_name : string;  (** Class name (e.g., "Eq", "Ord") *)
  cls_tvar : string;  (** The class type parameter name *)
  cls_superclasses : string list;
      (** Superclass names (e.g., ["Eq"] for Ord which requires Eq) *)
}
(** Type class information.

    Example: Ord with superclass Eq would be:
    {cls_name="Ord"; cls_tvar="a"; cls_superclasses=["Eq"]} *)

(** {1 Instance Registry} *)

(** A loaded instance ready for resolution.

    Instances are converted from AST form during signature loading. The type is
    represented as a concrete Types.typ rather than a sig_type to enable
    unification during resolution. *)
type instance = {
  inst_class : string;  (** Class name (e.g., "Eq", "Functor") *)
  inst_type : typ;  (** Instance head type (e.g., Int, (List a)) *)
  inst_tvars : string list;
      (** Bound type variables for parameterized instances *)
  inst_constraints : type_constraint list;
      (** Required constraints (e.g., [(Eq, a)] for (Eq (list a))) *)
}
(** A type class instance for resolution.

    Examples:
    - Simple: {inst_class="Eq"; inst_type=Int; inst_tvars=[]; inst_constraints=[]}
    - Parameterized: {inst_class="Eq"; inst_type=(List a); inst_tvars=["a"];
                      inst_constraints=[("Eq", a)]} *)

type registry = {
  instances : instance list;  (** All loaded instances *)
  classes : class_info list;  (** All loaded class definitions *)
}
(** Instance registry: stores both instances and class definitions.

    The registry is populated during signature loading and queried during type
    checking. Multiple instances for the same class are allowed as long as they
    have non-overlapping types. *)

(** Empty registry *)
let empty_registry = { instances = []; classes = [] }

(** Add an instance to the registry *)
let add_instance inst registry =
  { registry with instances = inst :: registry.instances }

(** Add a class to the registry *)
let add_class cls registry = { registry with classes = cls :: registry.classes }

(** Get all instances for a given class *)
let instances_for_class class_name registry =
  List.filter (fun i -> i.inst_class = class_name) registry.instances

(** Get all instances in the registry *)
let all_instances registry = registry.instances

(** Get all classes in the registry *)
let all_classes registry = registry.classes

(** Look up a class by name *)
let find_class class_name registry =
  List.find_opt (fun c -> c.cls_name = class_name) registry.classes

(** Merge two registries. Items from the second are added after the first. *)
let merge r1 r2 =
  { instances = r1.instances @ r2.instances; classes = r1.classes @ r2.classes }

(** {1 Instance Resolution} *)

(** Resolution result *)
type resolution_result =
  | Resolved  (** Instance found and constraints satisfied *)
  | NotFound of string * typ  (** No instance for (class, type) *)
  | Recursive of type_constraint list
      (** Instance found but requires these constraints to be resolved *)

(** Try to match a type against an instance head.

    For simple instances like (Eq int), this checks exact equality. For
    parameterized instances like (Eq (list a)), this attempts to unify the
    constraint type with the instance head, returning the substitution for any
    bound type variables.

    Returns Some substitution if the type matches, None otherwise. *)
let match_instance_head (inst : instance) (target_type : typ) :
    (string * typ) list option =
  (* For monomorphic instances, check structural equality *)
  if inst.inst_tvars = [] then
    if equal (repr inst.inst_type) (repr target_type) then Some [] else None
  else
    (* For parameterized instances, we need to match the structure.
       This is a simplified pattern matching - full unification would be more complex.

       For (Eq (list a)) matching (Eq (list int)):
       - The outer structure must match: TApp(TCon "List", [a]) vs TApp(TCon "List", [int])
       - We extract bindings: a -> int *)
    let rec match_types inst_ty target_ty bindings =
      match (repr inst_ty, repr target_ty) with
      | TCon a, _ when List.mem a inst.inst_tvars ->
          (* Bound type variable - record binding *)
          Some ((a, target_ty) :: bindings)
      | TCon a, TCon b when a = b ->
          (* Same type constructor *)
          Some bindings
      | TApp (con1, args1), TApp (con2, args2)
        when List.length args1 = List.length args2 -> (
          (* Match constructor then args *)
          match match_types con1 con2 bindings with
          | None -> None
          | Some bindings' ->
              List.fold_left2
                (fun acc a1 a2 ->
                  match acc with None -> None | Some b -> match_types a1 a2 b)
                (Some bindings') args1 args2)
      | _, _ -> None
    in
    match_types inst.inst_type target_type []

(** Substitute type variables in constraints using the given bindings *)
let substitute_constraints (bindings : (string * typ) list)
    (constraints : type_constraint list) : type_constraint list =
  let subst ty =
    List.fold_left
      (fun t (var, replacement) ->
        match repr t with TCon v when v = var -> replacement | _ -> t)
      ty bindings
  in
  List.map (fun (cls, ty) -> (cls, subst ty)) constraints

(** Resolve a single constraint against the registry.

    Returns Resolved if an instance exists (and any sub-constraints can be
    resolved), NotFound if no instance exists, or Recursive if the instance
    requires additional constraints that need resolution. *)
let resolve_constraint (registry : registry)
    ((class_name, target_type) : type_constraint) : resolution_result =
  let candidates = instances_for_class class_name registry in
  (* Try each instance in order *)
  let rec try_instances = function
    | [] -> NotFound (class_name, target_type)
    | inst :: rest -> (
        match match_instance_head inst target_type with
        | None -> try_instances rest
        | Some bindings ->
            if inst.inst_constraints = [] then
              (* Simple instance with no constraints *)
              Resolved
            else
              (* Parameterized instance - need to resolve sub-constraints *)
              let sub_constraints =
                substitute_constraints bindings inst.inst_constraints
              in
              Recursive sub_constraints)
  in
  try_instances candidates

(** Resolve all constraints, recursively resolving any sub-constraints.

    Also checks superclass constraints (R6 from spec 21): when resolving (Ord
    int), we also verify that (Eq int) exists because Eq is a superclass of Ord.

    Returns Ok () if all constraints can be satisfied, or Error with the first
    unsatisfied constraint. *)
let resolve_all (registry : registry) (constraints : type_constraint list) :
    (unit, string * typ) result =
  let rec resolve_one ((class_name, target_type) as c) =
    match resolve_constraint registry c with
    | Resolved ->
        (* Check superclass constraints (R6) *)
        check_superclasses class_name target_type
    | NotFound (cls, ty) -> Error (cls, ty)
    | Recursive sub_constraints ->
        (* First resolve sub-constraints, then check superclasses *)
        let* () = resolve_many sub_constraints in
        check_superclasses class_name target_type
  and check_superclasses class_name target_type =
    match find_class class_name registry with
    | None ->
        (* Class not in registry - this is okay, we just can't check superclasses *)
        Ok ()
    | Some cls_info ->
        (* For each superclass, add a constraint with the same target type *)
        let super_constraints =
          List.map (fun super -> (super, target_type)) cls_info.cls_superclasses
        in
        resolve_many super_constraints
  and resolve_many constraints =
    List.fold_left
      (fun acc c -> match acc with Error _ -> acc | Ok () -> resolve_one c)
      (Ok ()) constraints
  in
  resolve_many constraints

(** {1 Registry Building} *)

(** Convert a sig_type to Types.typ in the context of instance loading.

    This is a simplified conversion that handles the common cases for instance
    types. Type variables become TCon with their names. *)
let rec sig_type_to_typ (st : Sig.Sig_ast.sig_type) : typ =
  match st with
  | Sig.Sig_ast.STVar (name, _) -> TCon name
  | Sig.Sig_ast.STCon (name, _) -> TCon name
  | Sig.Sig_ast.STApp (name, args, _) ->
      TApp (TCon name, List.map sig_type_to_typ args)
  | Sig.Sig_ast.STArrow (params, ret, _) ->
      TArrow (List.map sig_param_to_param params, sig_type_to_typ ret)
  | Sig.Sig_ast.STTuple (elems, _) -> TTuple (List.map sig_type_to_typ elems)
  | Sig.Sig_ast.STForall (_, body, _) ->
      (* For instance heads, foralls shouldn't appear - just use the body *)
      sig_type_to_typ body
  | Sig.Sig_ast.STUnion (types, _) -> TUnion (List.map sig_type_to_typ types)

and sig_param_to_param = function
  | Sig.Sig_ast.SPPositional ty -> PPositional (sig_type_to_typ ty)
  | Sig.Sig_ast.SPOptional ty -> POptional (sig_type_to_typ ty)
  | Sig.Sig_ast.SPRest ty -> PRest (sig_type_to_typ ty)
  | Sig.Sig_ast.SPKey (name, ty) -> PKey (name, sig_type_to_typ ty)

(** Load an instance declaration into the registry *)
let load_instance (decl : Sig.Sig_ast.instance_decl) (registry : registry) :
    registry =
  let inst_tvars =
    List.map (fun b -> b.Sig.Sig_ast.name) decl.inst_tvar_binders
  in
  let inst_type = sig_type_to_typ decl.inst_type in
  let inst_constraints =
    List.map
      (fun (cls, sig_ty) -> (cls, sig_type_to_typ sig_ty))
      decl.inst_constraints
  in
  let instance =
    { inst_class = decl.inst_class; inst_type; inst_tvars; inst_constraints }
  in
  add_instance instance registry

(** Load a class declaration into the registry.

    Extracts class name, type parameter, and superclass names for use during
    instance resolution (R6 from spec 21). *)
let load_class (decl : Sig.Sig_ast.class_decl) (registry : registry) : registry
    =
  let cls_superclasses =
    List.map (fun (super_name, _sig_ty) -> super_name) decl.class_superclasses
  in
  let cls =
    {
      cls_name = decl.class_name;
      cls_tvar = decl.class_tvar_binder.Sig.Sig_ast.name;
      cls_superclasses;
    }
  in
  add_class cls registry

(** {1 Debugging} *)

(** Pretty-print an instance for debugging *)
let instance_to_string inst =
  let tvars =
    if inst.inst_tvars = [] then ""
    else Printf.sprintf "[%s] " (String.concat " " inst.inst_tvars)
  in
  let constraints =
    if inst.inst_constraints = [] then ""
    else
      Printf.sprintf "%s => "
        (String.concat " "
           (List.map
              (fun (cls, ty) -> Printf.sprintf "(%s %s)" cls (to_string ty))
              inst.inst_constraints))
  in
  Printf.sprintf "(instance %s%s(%s %s))" tvars constraints inst.inst_class
    (to_string inst.inst_type)

(** Pretty-print the registry for debugging *)
let registry_to_string registry =
  String.concat "\n" (List.map instance_to_string registry.instances)
