(** Forall quantifier inference.

    This module infers universal quantifiers from unquantified type signatures.
    When a signature contains lowercase type variables but no explicit [vars]
    quantifier, the variables are collected in left-to-right first-occurrence
    order and quantified automatically.

    Examples:
    - [(defun seq-map (((a -> b)) (seq a)) -> (list b))] infers [a b]
    - [(defun compose (((b -> c)) ((a -> b))) -> ((a -> c)))] infers [b c a]

    When explicit quantifiers are provided, inference is disabled and any
    unbound type variables produce an error.
*)

open Sig_ast

(** {1 Type Variable Collection}

    Collect type variables in left-to-right first-occurrence order.
    Variables are deduplicated (each name appears only once). *)

(** Check if a name is a primitive type (not a type variable) *)
let is_primitive name =
  match name with
  | "int" | "float" | "num" | "string" | "symbol" | "keyword"
  | "nil" | "t" | "bool" | "truthy" | "any" | "never" -> true
  | _ -> false

(** Add a name to the list if not already present (preserves first-occurrence order) *)
let add_unique name lst =
  if List.mem name lst then lst else lst @ [name]

(** Collect type variables from a sig_type.
    Returns variables in left-to-right first-occurrence order.
    [bound] contains already bound variables to exclude.
    [known_types] contains user-defined type names to exclude. *)
let rec collect_sig_type ~(bound : string list) ~(known_types : string list) (ty : sig_type) : string list =
  match ty with
  | STVar (name, _) ->
      (* Type variable - include if not primitive, not bound, not a known type *)
      if is_primitive name || List.mem name bound || List.mem name known_types then
        []
      else
        [name]

  | STCon (_, _) ->
      (* Type constant (primitive) - no variables *)
      []

  | STApp (_name, args, _) ->
      (* Type application - the constructor name is NOT a type variable.
         Type constructors (like 'seq' in '(seq a)') are never inferred as
         type variables; only the arguments are collected.
         In Elisp signature syntax, (foo a b) means foo is a type constructor
         applied to arguments a and b - foo is not a type variable. *)
      collect_sig_types ~bound ~known_types args

  | STArrow (params, ret, _) ->
      (* Arrow type - collect from params left-to-right, then return type *)
      let param_vars = collect_params ~bound ~known_types params in
      let ret_vars = collect_sig_type ~bound ~known_types ret in
      List.fold_left (fun acc v -> add_unique v acc) param_vars ret_vars

  | STForall (binders, body, _) ->
      (* Forall type - bound variables shadow outer scope *)
      let new_bound = List.map (fun b -> b.name) binders @ bound in
      (* Collect from bounds first (in outer scope) *)
      let bound_vars = List.concat_map
        (fun b ->
           match b.bound with
           | Some ty -> collect_sig_type ~bound ~known_types ty
           | None -> [])
        binders
      in
      let body_vars = collect_sig_type ~bound:new_bound ~known_types body in
      List.fold_left (fun acc v -> add_unique v acc) bound_vars body_vars

  | STUnion (types, _) ->
      collect_sig_types ~bound ~known_types types

  | STTuple (types, _) ->
      collect_sig_types ~bound ~known_types types

(** Collect type variables from multiple types (left-to-right order) *)
and collect_sig_types ~(bound : string list) ~(known_types : string list) (types : sig_type list) : string list =
  List.fold_left
    (fun acc ty ->
       let vars = collect_sig_type ~bound ~known_types ty in
       List.fold_left (fun acc' v -> add_unique v acc') acc vars)
    [] types

(** Collect type variables from a parameter *)
and collect_param ~(bound : string list) ~(known_types : string list) (param : sig_param) : string list =
  match param with
  | SPPositional ty -> collect_sig_type ~bound ~known_types ty
  | SPOptional ty -> collect_sig_type ~bound ~known_types ty
  | SPRest ty -> collect_sig_type ~bound ~known_types ty
  | SPKey (_, ty) -> collect_sig_type ~bound ~known_types ty

(** Collect type variables from parameters (left-to-right order) *)
and collect_params ~(bound : string list) ~(known_types : string list) (params : sig_param list) : string list =
  List.fold_left
    (fun acc param ->
       let vars = collect_param ~bound ~known_types param in
       List.fold_left (fun acc' v -> add_unique v acc') acc vars)
    [] params

(** {1 Inference for Declarations} *)

(** Infer quantifiers for a defun declaration.
    If defun_tvar_binders is empty, collect all type variables from the
    signature and create binders for them.
    Returns the declaration with binders filled in. *)
let infer_defun ~(known_types : string list) (d : defun_decl) : defun_decl =
  if d.defun_tvar_binders <> [] then
    (* Explicit quantifiers - no inference needed *)
    d
  else
    (* Collect type variables from params and return type *)
    let bound = [] in
    let param_vars = collect_params ~bound ~known_types d.defun_params in
    let ret_vars = collect_sig_type ~bound ~known_types d.defun_return in
    let all_vars = List.fold_left (fun acc v -> add_unique v acc) param_vars ret_vars in
    (* Create binders (no bounds) *)
    let binders = List.map
      (fun name -> { name; bound = None; loc = d.defun_loc })
      all_vars
    in
    { d with defun_tvar_binders = binders }

(** Infer quantifiers for a type declaration.
    If type_params is empty and there's a body, collect type variables.
    Returns the declaration with params filled in. *)
let infer_type_decl ~(known_types : string list) (d : type_decl) : type_decl =
  if d.type_params <> [] then
    (* Explicit params - no inference needed *)
    d
  else
    match d.type_body with
    | None ->
        (* Opaque type - nothing to infer *)
        d
    | Some body ->
        (* Collect type variables from body *)
        let vars = collect_sig_type ~bound:[] ~known_types body in
        let params = List.map
          (fun name -> { name; bound = None; loc = d.type_loc })
          vars
        in
        { d with type_params = params }

(** Infer quantifiers for a single declaration *)
let infer_decl ~(known_types : string list) (decl : decl) : decl =
  match decl with
  | DDefun d -> DDefun (infer_defun ~known_types d)
  | DType d -> DType (infer_type_decl ~known_types d)
  | _ -> decl

(** {1 Signature Inference}

    Process an entire signature file to infer quantifiers for all declarations. *)

(** Get all type names defined in a signature (for use as known_types) *)
let get_type_names (sig_file : signature) : string list =
  List.filter_map
    (fun decl ->
       match decl with
       | DType d -> Some d.type_name
       | DImportStruct d -> Some d.struct_name
       | _ -> None)
    sig_file.sig_decls

(** Infer quantifiers for all declarations in a signature file.
    Type names defined in the signature are treated as known types,
    not type variables.
    Returns the signature with inferred quantifiers. *)
let infer_signature (sig_file : signature) : signature =
  let known_types = get_type_names sig_file in
  let decls = List.map (infer_decl ~known_types) sig_file.sig_decls in
  { sig_file with sig_decls = decls }
