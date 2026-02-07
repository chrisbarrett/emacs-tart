(** Defun inference: type-check top-level definitions.

    Extracted from {!Infer} to reduce module size. The entry points receive
    [~infer_lambda] and [~infer_progn] as parameters to break the mutual
    recursion with the main inference engine. *)

open Core.Types
module Env = Core.Type_env
module C = Constraint
module G = Generalize
module Loc = Syntax.Location
module Sig_parser = Sig.Sig_parser
module Sig_loader = Sig.Sig_loader
module Sig_ast = Sig.Sig_ast
open Infer_types

(** Extract (declare (tart TYPE)) from a defun body.

    Returns [(Some type_sexp, remaining_body)] if a tart declaration is found,
    or [(None, body)] if not. The declare form must be the first expression in
    the body. *)
let extract_tart_declare (body : Syntax.Sexp.t list) :
    Syntax.Sexp.t option * Syntax.Sexp.t list =
  let open Syntax.Sexp in
  match body with
  | List
      ([ Symbol ("declare", _); List (Symbol ("tart", _) :: type_parts, _) ], _)
    :: rest ->
      (* Found (declare (tart ...)), extract the type expression.
         The type parts should form a single arrow type: (params) -> return
         or [vars] (params) -> return *)
      let type_sexp =
        match type_parts with
        | [ single ] -> single
        | _ -> (
            (* Multiple parts: typically [vars] (params -> return).
               The arrow and return type are inside the last list element.
               We split the arrow out so the parser sees [vars] (params) -> return
               at the top level. *)
            let rev = List.rev type_parts in
            match rev with
            | List (inner, sp) :: prefix -> (
                match Syntax.Sexp.find_arrow inner with
                | Some (params_before, return_after) ->
                    (* Rebuild as: prefix... (params_before) -> return_after *)
                    let arrow = Symbol ("->", Loc.dummy_span) in
                    let params_list = List (params_before, sp) in
                    List
                      ( List.rev_append prefix
                          (params_list :: arrow :: return_after),
                        Loc.dummy_span )
                | None ->
                    (* No arrow found — wrap as before *)
                    List (type_parts, Loc.dummy_span))
            | _ ->
                (* Fallback: wrap as before *)
                List (type_parts, Loc.dummy_span))
      in
      (Some type_sexp, rest)
  | _ -> (None, body)

(** Parse a tart type annotation from an S-expression.

    Returns the parsed sig_type or None if parsing fails. *)
let parse_tart_type (type_sexp : Syntax.Sexp.t) : Sig_ast.sig_type option =
  match Sig_parser.parse_sig_type type_sexp with
  | Ok sig_type -> Some sig_type
  | Error _ -> None

(** Infer a defun as an expression.

    As an expression, defun returns a symbol (the function name). The function
    type is inferred like a lambda.

    For the side effect of binding the name to the type, use [infer_defun]. *)
let infer_defun_as_expr
    ~(infer_lambda :
       Env.t -> Syntax.Sexp.t list -> Syntax.Sexp.t list -> Loc.span -> result)
    env _name params body span =
  (* Infer as a lambda, but return Symbol as the expression type *)
  let fn_result = infer_lambda env params body span in
  (* defun returns the symbol naming the function, but propagate undefineds *)
  {
    ty = Prim.symbol;
    constraints = fn_result.constraints;
    undefineds = fn_result.undefineds;
    clause_diagnostics = fn_result.clause_diagnostics;
  }

(** Infer a defun with a type declaration.

    Uses the declared type for parameters and checks the body against the
    declared return type. Creates fresh type variables for polymorphic type
    parameters so they can be properly generalized. *)
let infer_defun_with_declaration
    ~(infer_progn : Env.t -> Syntax.Sexp.t list -> Loc.span -> result)
    (env : Env.t) (name : string) (params : Syntax.Sexp.t list)
    (body : Syntax.Sexp.t list) (span : Loc.span) (sig_type : Sig_ast.sig_type)
    : defun_result option =
  (* Extract type variable names from forall, if present *)
  let tvar_names, arrow_type =
    match sig_type with
    | Sig_ast.STForall (binders, inner, _) ->
        (List.map (fun b -> b.Sig_ast.name) binders, inner)
    | _ -> ([], sig_type)
  in

  (* Extract parameter types and return type from arrow *)
  let sig_params, return_sig_type =
    match arrow_type with
    | Sig_ast.STArrow (params, ret, _) -> (params, ret)
    | _ ->
        (* Not an arrow type - treat as nullary function returning this type *)
        ([], arrow_type)
  in

  let inner_env = Env.enter_level env in

  (* Create fresh type variables for each declared type variable name.
     This is critical: we need actual TVar refs, not TCon names. *)
  let tvar_subst =
    List.map
      (fun name -> (name, fresh_tvar (Env.current_level inner_env)))
      tvar_names
  in

  (* Convert sig_params to types, then substitute fresh TVars for type var names *)
  let prelude = Sig.Prelude.prelude_alias_context () in
  let convert_and_subst sty =
    let base_ty =
      Sig_loader.sig_type_to_typ_with_aliases prelude tvar_names sty
    in
    Clause_dispatch.substitute_tvar_names tvar_subst base_ty
  in

  (* Bind parameters with declared types (with fresh TVars substituted) *)
  let param_info =
    let open Syntax.Sexp in
    let rec loop params sig_params acc =
      match (params, sig_params) with
      | [], _ | _, [] -> List.rev acc
      | Symbol (pname, _) :: ps, sp :: sps ->
          let ty =
            match sp with
            | Sig_ast.SPPositional (_, sty)
            | Sig_ast.SPOptional (_, sty)
            | Sig_ast.SPRest sty
            | Sig_ast.SPKey (_, sty) ->
                convert_and_subst sty
            | Sig_ast.SPLiteral _ ->
                (* Literal params have no type — give fresh tvar *)
                fresh_tvar (Env.current_level inner_env)
          in
          loop ps sps ((pname, ty) :: acc)
      | _ :: ps, _ :: sps -> loop ps sps acc
    in
    loop params sig_params []
  in

  (* Extend environment with parameter types *)
  let body_env = Env.extend_monos param_info inner_env in

  (* Infer body *)
  let body_result = infer_progn body_env body span in

  (* Convert declared return type (with fresh TVars) *)
  let declared_return = convert_and_subst return_sig_type in

  (* Add constraint: body type = declared return type *)
  let context =
    C.DeclaredReturn
      {
        fn_name = name;
        declared_type = declared_return;
        declared_span = Sig_ast.sig_type_loc return_sig_type;
      }
  in
  let return_constraint =
    C.equal ~context body_result.ty declared_return span
  in
  let all_constraints = C.add return_constraint body_result.constraints in

  (* Solve constraints *)
  let _ = Unify.solve all_constraints in

  (* Build function type from declaration (using fresh TVars) *)
  let param_types =
    List.map
      (fun sp ->
        match sp with
        | Sig_ast.SPPositional (_, sty) -> PPositional (convert_and_subst sty)
        | Sig_ast.SPOptional (_, sty) -> POptional (convert_and_subst sty)
        | Sig_ast.SPRest sty -> PRest (convert_and_subst sty)
        | Sig_ast.SPKey (kname, sty) -> PKey (kname, convert_and_subst sty)
        | Sig_ast.SPLiteral (value, _) -> PLiteral value)
      sig_params
  in
  let fn_type = TArrow (param_types, declared_return) in

  (* Return the type with fresh TVars still unbound.
     check_form will call G.generalize to create the proper polymorphic scheme. *)
  Some
    {
      name;
      fn_type;
      defun_constraints = all_constraints;
      defun_undefineds = body_result.undefineds;
      defun_clause_diagnostics = body_result.clause_diagnostics;
    }

(** Infer a defun without type declaration (original behavior). *)
let infer_defun_inferred
    ~(infer_progn : Env.t -> Syntax.Sexp.t list -> Loc.span -> result)
    (env : Env.t) (name : string) (params : Syntax.Sexp.t list)
    (body : Syntax.Sexp.t list) (span : Loc.span) : defun_result option =
  let open Syntax.Sexp in
  let outer_level = Env.current_level env in
  let inner_env = Env.enter_level env in

  (* Create fresh type variables for each parameter *)
  let param_info =
    List.map
      (fun p ->
        match p with
        | Symbol (pname, _) ->
            let tv = fresh_tvar (Env.current_level inner_env) in
            (pname, tv)
        | _ ->
            let tv = fresh_tvar (Env.current_level inner_env) in
            ("_", tv))
      params
  in

  (* Extend environment with parameter types *)
  let body_env = Env.extend_monos param_info inner_env in

  (* Infer body as a progn *)
  let body_result = infer_progn body_env body span in

  (* Solve constraints for the defun body *)
  let _ = Unify.solve body_result.constraints in

  (* Build function type *)
  let param_types = List.map (fun (_, ty) -> PPositional ty) param_info in
  let fn_type = TArrow (param_types, body_result.ty) in

  (* Generalize the function type (defun is always a syntactic value) *)
  let scheme = G.generalize outer_level fn_type in
  let generalized_ty =
    match scheme with
    | Env.Mono ty -> ty
    | Env.Poly (vars, ty) -> TForall (vars, ty)
  in

  Some
    {
      name;
      fn_type = generalized_ty;
      defun_constraints = body_result.constraints;
      defun_undefineds = body_result.undefineds;
      defun_clause_diagnostics = body_result.clause_diagnostics;
    }

(** Infer the type of a defun and return the binding information.

    Unlike [infer_defun_as_expr], this returns the function name and type so
    callers can bind it in the environment.

    If the body contains [(declare (tart TYPE))], the declared type is used
    instead of inferring. The body is checked against the declared return type.
*)
let infer_defun
    ~(infer_progn : Env.t -> Syntax.Sexp.t list -> Loc.span -> result)
    (env : Env.t) (sexp : Syntax.Sexp.t) : defun_result option =
  let open Syntax.Sexp in
  match sexp with
  | List
      (Symbol ("defun", _) :: Symbol (name, _) :: List (params, _) :: body, span)
    -> (
      (* Check for inline type declaration *)
      let tart_decl, actual_body = extract_tart_declare body in
      match tart_decl with
      | Some type_sexp -> (
          match parse_tart_type type_sexp with
          | Some sig_type ->
              infer_defun_with_declaration ~infer_progn env name params
                actual_body span sig_type
          | None ->
              (* Parse failed - fall through to regular inference *)
              infer_defun_inferred ~infer_progn env name params body span)
      | None ->
          (* No declaration - infer as usual *)
          infer_defun_inferred ~infer_progn env name params body span)
  | _ -> None
