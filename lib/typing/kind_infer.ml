(** Kind inference algorithm.

    This module infers kinds for type variables from their usage patterns. When
    a type variable is applied to arguments (e.g., [f a] in a signature), we
    infer that it must have kind [* -> *].

    The algorithm follows the same pattern as type inference: 1. Assign fresh
    kind variables to type parameters 2. Collect kind constraints from type
    expressions 3. Unify kind constraints 4. Default unconstrained kind
    variables to [*]

    Examples:
    - [(defun identity [a] (a) -> a)] - [a : *] (used only in type positions)
    - [(defun fmap [f a b] (((a -> b)) (f a)) -> (f b))] - [f : * -> *],
      [a : *], [b : *]
    - [(defun bimap [f a b c d] (((a -> b)) ((c -> d)) (f a c)) -> (f b d))] -
      [f : * -> * -> *] *)

open Sig.Sig_ast
module Kind = Kind

(** {1 Kind Inference Errors} *)

(** A kind error that occurs during inference. *)
type kind_error =
  | KindMismatch of {
      expected : Kind.kind;
      found : Kind.kind;
      location : string;  (** Description of where the error occurred *)
    }
  | OccursCheckFailed of { kvar_id : Kind.kvar_id; kind : Kind.kind }
  | ArityMismatch of { type_con : string; expected : int; found : int }

(** Pretty-print a kind error. *)
let kind_error_to_string = function
  | KindMismatch { expected; found; location } ->
      Printf.sprintf "Kind mismatch in %s: expected %s, found %s" location
        (Kind.to_string expected) (Kind.to_string found)
  | OccursCheckFailed { kvar_id; kind } ->
      Printf.sprintf "Infinite kind: ?k%d occurs in %s" kvar_id
        (Kind.to_string kind)
  | ArityMismatch { type_con; expected; found } ->
      Printf.sprintf "Type constructor %s expects %d argument(s), found %d"
        type_con expected found

(** {1 Kind Unification}

    Kind unification is simpler than type unification since kinds have no
    quantifiers or complex constructors - just [*] and [->]. *)

(** Occurs check: ensure a kind variable doesn't occur in a kind. *)
let rec occurs_in (kvar_id : Kind.kvar_id) (kind : Kind.kind) : bool =
  match kind with
  | Kind.KStar -> false
  | Kind.KArrow (k1, k2) -> occurs_in kvar_id k1 || occurs_in kvar_id k2

(** Unify two kinds, returning an error if they don't match. *)
let rec unify_kinds (k1 : Kind.kind) (k2 : Kind.kind) (location : string) :
    (unit, kind_error) result =
  match (k1, k2) with
  | Kind.KStar, Kind.KStar -> Ok ()
  | Kind.KArrow (a1, r1), Kind.KArrow (a2, r2) ->
      let* () = unify_kinds a1 a2 location in
      unify_kinds r1 r2 location
  | Kind.KStar, Kind.KArrow _ | Kind.KArrow _, Kind.KStar ->
      Error (KindMismatch { expected = k1; found = k2; location })

and ( let* ) = Result.bind

let ( let* ) = Result.bind

(** Unify a kind scheme with a concrete kind. *)
let unify_scheme_with_kind (ks : Kind.kind_scheme) (k : Kind.kind)
    (location : string) : (unit, kind_error) result =
  match Kind.repr_scheme ks with
  | Kind.KConcrete k' -> unify_kinds k' k location
  | Kind.KVar kv -> (
      match !kv with
      | Kind.KUnbound id ->
          if occurs_in id k then
            Error (OccursCheckFailed { kvar_id = id; kind = k })
          else (
            kv := Kind.KLink k;
            Ok ())
      | Kind.KLink k' -> unify_kinds k' k location)

(** {1 Kind Inference for Signature Types}

    Walk through a signature type expression, collecting kind constraints. When
    a type variable is applied to arguments, constrain its kind accordingly. *)

(** Infer the kind of a signature type expression.

    @param env Kind environment mapping type variable names to kind schemes
    @param ty The signature type to infer the kind of
    @return The kind scheme of the type, or an error *)
let rec infer_sig_type_kind (env : Kind.env) (ty : sig_type) :
    (Kind.kind_scheme, kind_error) result =
  match ty with
  | STVar (name, _) ->
      (* Type variable - look up its kind in the environment *)
      Ok (Kind.lookup name env)
  | STCon (_, _) ->
      (* Type constant (primitive) - always kind * *)
      Ok (Kind.KConcrete Kind.KStar)
  | STApp (name, args, _) ->
      (* Type application: (con arg1 arg2 ...)
         The constructor must have kind k1 -> k2 -> ... -> kn -> *
         where each ki is the kind of the corresponding argument.
         The result kind is *. *)
      let n = List.length args in
      (* Infer the kinds of all arguments - they must all be kind * *)
      let check_arg arg : (unit, kind_error) result =
        match infer_sig_type_kind env arg with
        | Ok arg_kind ->
            unify_scheme_with_kind arg_kind Kind.KStar
              (Printf.sprintf "argument in (%s ...)" name)
        | Error e -> Error e
      in
      let* () =
        List.fold_left
          (fun acc arg ->
            match acc with Ok () -> check_arg arg | Error _ as e -> e)
          (Ok ()) args
      in
      (* Look up the constructor's kind if it's a type variable *)
      let con_kind_scheme = Kind.lookup name env in
      (* The constructor's kind must be * -> * -> ... -> * (n arrows) *)
      let expected_kind = Kind.arity n in
      let* () =
        unify_scheme_with_kind con_kind_scheme expected_kind
          (Printf.sprintf "type constructor %s" name)
      in
      (* The result kind is always * *)
      Ok (Kind.KConcrete Kind.KStar)
  | STArrow (params, ret, _) ->
      (* Arrow type: all param types and return type must be kind * *)
      let check_params : (unit, kind_error) result =
        List.fold_left
          (fun acc param ->
            match acc with
            | Ok () -> infer_param_kind env param
            | Error _ as e -> e)
          (Ok ()) params
      in
      let* () = check_params in
      let* ret_kind = infer_sig_type_kind env ret in
      let* () = unify_scheme_with_kind ret_kind Kind.KStar "return type" in
      Ok (Kind.KConcrete Kind.KStar)
  | STForall (binders, body, _) ->
      (* Forall: add binders with fresh kind variables, then infer body *)
      let binder_names = List.map (fun b -> b.name) binders in
      let inner_env = Kind.extend_fresh binder_names env in
      (* Infer body kind - should be * *)
      let* body_kind = infer_sig_type_kind inner_env body in
      let* () = unify_scheme_with_kind body_kind Kind.KStar "forall body" in
      Ok (Kind.KConcrete Kind.KStar)
  | STUnion (types, _) ->
      (* Union: all component types must be kind * *)
      let check_member ty : (unit, kind_error) result =
        match infer_sig_type_kind env ty with
        | Ok ty_kind -> unify_scheme_with_kind ty_kind Kind.KStar "union member"
        | Error e -> Error e
      in
      let* () =
        List.fold_left
          (fun acc ty ->
            match acc with Ok () -> check_member ty | Error _ as e -> e)
          (Ok ()) types
      in
      Ok (Kind.KConcrete Kind.KStar)
  | STTuple (types, _) ->
      (* Tuple: all element types must be kind * *)
      let check_element ty : (unit, kind_error) result =
        match infer_sig_type_kind env ty with
        | Ok ty_kind ->
            unify_scheme_with_kind ty_kind Kind.KStar "tuple element"
        | Error e -> Error e
      in
      let* () =
        List.fold_left
          (fun acc ty ->
            match acc with Ok () -> check_element ty | Error _ as e -> e)
          (Ok ()) types
      in
      Ok (Kind.KConcrete Kind.KStar)

(** Infer the kind of a function parameter type. *)
and infer_param_kind (env : Kind.env) (param : sig_param) :
    (unit, kind_error) result =
  let ty =
    match param with
    | SPPositional ty | SPOptional ty | SPRest ty | SPKey (_, ty) -> ty
  in
  let* kind = infer_sig_type_kind env ty in
  unify_scheme_with_kind kind Kind.KStar "parameter type"

(** {1 High-Level Interface}

    Functions to infer kinds for entire declarations. *)

type infer_result = {
  kind_env : Kind.env;  (** Final kind environment with inferred kinds *)
  errors : kind_error list;  (** Any errors encountered *)
}
(** Result of kind inference for a declaration. *)

(** Infer kinds for a defun declaration's type parameters.

    @param d The defun declaration to analyze
    @return The inferred kind environment and any errors *)
let infer_defun_kinds (d : defun_decl) : infer_result =
  Kind.reset_kvar_counter ();
  (* Create fresh kind variables for each type parameter *)
  let tvar_names = List.map (fun b -> b.name) d.defun_tvar_binders in
  let env = Kind.extend_fresh tvar_names Kind.empty_env in

  (* Infer kinds from parameter types *)
  let errors =
    List.fold_left
      (fun errs param ->
        match infer_param_kind env param with
        | Ok () -> errs
        | Error e -> e :: errs)
      [] d.defun_params
  in

  (* Infer kind from return type *)
  let errors =
    match infer_sig_type_kind env d.defun_return with
    | Ok kind_scheme -> (
        match unify_scheme_with_kind kind_scheme Kind.KStar "return type" with
        | Ok () -> errors
        | Error e -> e :: errors)
    | Error e -> e :: errors
  in

  (* Default all unconstrained kind variables to * *)
  Kind.default_all env;
  { kind_env = env; errors = List.rev errors }

(** Infer kinds for a type declaration's type parameters.

    @param d The type declaration to analyze
    @return The inferred kind environment and any errors *)
let infer_type_decl_kinds (d : type_decl) : infer_result =
  Kind.reset_kvar_counter ();
  (* Create fresh kind variables for each type parameter *)
  let param_names = List.map (fun b -> b.name) d.type_params in
  let env = Kind.extend_fresh param_names Kind.empty_env in

  (* Infer kinds from body if present *)
  let errors =
    match d.type_body with
    | None -> []
    | Some body -> (
        match infer_sig_type_kind env body with
        | Ok kind_scheme -> (
            match unify_scheme_with_kind kind_scheme Kind.KStar "type body" with
            | Ok () -> []
            | Error e -> [ e ])
        | Error e -> [ e ])
  in

  (* Default all unconstrained kind variables to * *)
  Kind.default_all env;
  { kind_env = env; errors }

(** Infer kinds for a data declaration's type parameters.

    @param d The data declaration to analyze
    @return The inferred kind environment and any errors *)
let infer_data_kinds (d : data_decl) : infer_result =
  Kind.reset_kvar_counter ();
  (* Create fresh kind variables for each type parameter *)
  let param_names = List.map (fun b -> b.name) d.data_params in
  let env = Kind.extend_fresh param_names Kind.empty_env in

  (* Infer kinds from constructor fields *)
  let errors =
    List.fold_left
      (fun errs (ctor : ctor_decl) ->
        List.fold_left
          (fun errs field_ty ->
            match infer_sig_type_kind env field_ty with
            | Ok kind_scheme -> (
                match
                  unify_scheme_with_kind kind_scheme Kind.KStar
                    "constructor field"
                with
                | Ok () -> errs
                | Error e -> e :: errs)
            | Error e -> e :: errs)
          errs ctor.ctor_fields)
      [] d.data_ctors
  in

  (* Default all unconstrained kind variables to * *)
  Kind.default_all env;
  { kind_env = env; errors = List.rev errors }

(** {1 Kind Lookup}

    Functions to look up the inferred kind of a type parameter. *)

(** Look up the kind of a type parameter after inference.

    @param result The inference result
    @param name The type parameter name
    @return The inferred kind *)
let lookup_kind (result : infer_result) (name : string) : Kind.kind =
  Kind.lookup_defaulted name result.kind_env
