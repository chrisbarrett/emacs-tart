(** Module-aware type checking.

    This module extends the basic type checker with module boundary support:
    - Loads sibling `.tart` files for signature verification
    - Loads required modules from the search path
    - Verifies implementations match declared signatures

    R1: Any `.el` file can be type-checked via LSP R2: Verify implementations
    match `.tart` signatures R3: Load signatures from search path for required
    modules *)

module Env = Core.Type_env
module Types = Core.Types
module Search = Sig.Search_path
module Loader = Sig.Sig_loader

(** {1 Module Check Configuration} *)

type config = {
  search_path : Search.t;  (** Search path for finding `.tart` files *)
  stdlib_dir : string option;  (** Path to bundled stdlib *)
}
(** Configuration for module-aware type checking *)

let default_config () = { search_path = Search.empty; stdlib_dir = None }

let with_stdlib stdlib_dir config =
  let search_path = Search.with_stdlib stdlib_dir config.search_path in
  { search_path; stdlib_dir = Some stdlib_dir }

let with_search_dirs dirs config =
  let search_path = Search.of_dirs dirs in
  let search_path =
    match config.stdlib_dir with
    | Some dir -> Search.with_stdlib dir search_path
    | None -> search_path
  in
  { config with search_path }

(** {1 Signature Mismatch Errors} *)

type mismatch_error = {
  name : string;
  expected : Types.typ;
  actual : Types.typ;
  span : Syntax.Location.span;
}
(** Error when implementation doesn't match signature *)

(** {1 Module Check Result} *)

type check_result = {
  type_errors : Unify.error list;  (** Type errors from inference *)
  mismatch_errors : mismatch_error list;  (** Signature mismatch errors *)
  signature_env : Env.t option;  (** Environment from loaded signature, if any *)
}
(** Result of module-aware type checking *)

(** {1 Require Detection} *)

(** Extract module names from require forms in the parsed sexps *)
let extract_requires (sexps : Syntax.Sexp.t list) : string list =
  let open Syntax.Sexp in
  List.filter_map
    (fun sexp ->
      match sexp with
      | List
          ( Symbol ("require", _) :: Symbol ("quote", _) :: Symbol (name, _) :: _,
            _ )
      | List
          ( Symbol ("require", _)
            :: List ([ Symbol ("quote", _); Symbol (name, _) ], _)
            :: _,
            _ ) ->
          Some name
      | _ -> None)
    sexps

(** {1 Signature Loading} *)

(** Load signature for a module, returning the extended environment *)
let load_module_signature ~(config : config) ~(el_path : string option)
    ~(env : Env.t) (module_name : string) : Env.t option =
  Search.load_module ~search_path:config.search_path ?el_path ~env module_name

(** Try to find and load a sibling `.tart` file for an `.el` file *)
let load_sibling_signature ~(config : config) (el_path : string) :
    (Env.t * Sig.Sig_ast.signature) option =
  let dir = Filename.dirname el_path in
  let basename = Filename.basename el_path in
  (* Handle case where file doesn't have .el extension *)
  let module_name =
    if Filename.check_suffix basename ".el" then
      Filename.chop_suffix basename ".el"
    else basename
  in
  let tart_path = Filename.concat dir (module_name ^ ".tart") in
  (* Try to parse the signature file - returns None if file doesn't exist *)
  match Search.parse_signature_file tart_path with
  | Some sig_ast ->
      let resolver = Search.make_resolver ~el_path config.search_path in
      let base_env = Builtin_types.initial_env () in
      let env =
        Loader.load_signature_with_resolver ~resolver base_env sig_ast
      in
      Some (env, sig_ast)
  | None -> None

(** {1 Implementation Verification} *)

(** Check if a defun's inferred type is compatible with its declared type.

    This performs a simple structural check. The declared type should be an
    instance of the inferred type (the inferred type can be more general). *)
let verify_defun_type ~(name : string) ~(declared : Types.typ)
    ~(inferred : Types.typ) ~(span : Syntax.Location.span) :
    mismatch_error option =
  (* Reset type variable counter for fresh comparison *)
  Types.reset_tvar_counter ();

  (* Create fresh copies of both types for comparison *)
  let declared' = Types.repr declared in
  let inferred' = Types.repr inferred in

  (* Try to unify the declared type with the inferred type.
     The declared type should be compatible with (possibly more specific than)
     the inferred type. *)
  let c = Constraint.equal declared' inferred' span in
  match Unify.solve (Constraint.add c Constraint.empty) with
  | Ok () -> None (* Types are compatible *)
  | Error _ ->
      (* Types don't match - report mismatch *)
      Some { name; expected = declared; actual = inferred; span }

(** {1 Main Check Function} *)

(** Type-check an elisp file with module awareness.

    This is the main entry point for module-aware type checking: 1. Load sibling
    `.tart` signature if present 2. Load signatures for required modules 3.
    Type-check the implementation 4. If signature exists, verify implementations
    match

    @param config Module check configuration
    @param filename Path to the `.el` file (for sibling lookup)
    @param sexps Parsed S-expressions from the file
    @return Check result with type errors and mismatch errors *)
let check_module ~(config : config) ~(filename : string)
    (sexps : Syntax.Sexp.t list) : check_result =
  (* Step 1: Try to load sibling signature *)
  let sibling_result = load_sibling_signature ~config filename in

  (* Step 2: Build base environment
     If we have a signature, use the env from loading it.
     Otherwise, start with default builtins. *)
  let base_env, sig_ast_opt =
    match sibling_result with
    | Some (env, sig_ast) -> (env, Some sig_ast)
    | None -> (Builtin_types.initial_env (), None)
  in

  (* Step 3: Load signatures for required modules *)
  let required_modules = extract_requires sexps in
  let env_with_requires =
    List.fold_left
      (fun env module_name ->
        match
          load_module_signature ~config ~el_path:(Some filename) ~env
            module_name
        with
        | Some env' -> env'
        | None -> env)
      base_env required_modules
  in

  (* Step 4: Type-check the implementation *)
  let check_result = Check.check_program ~env:env_with_requires sexps in

  (* Step 5: If we have a signature, verify implementations match *)
  let mismatch_errors =
    match sig_ast_opt with
    | None -> []
    | Some sig_ast ->
        (* For each defun in the signature, find the corresponding
           definition in check_result and verify types match *)
        List.filter_map
          (fun decl ->
            match decl with
            | Sig.Sig_ast.DDefun d -> (
                (* Find the inferred type for this function *)
                let name = d.defun_name in
                match Env.lookup name check_result.env with
                | Some (Env.Mono inferred) | Some (Env.Poly (_, inferred)) ->
                    (* Get the declared type from the signature *)
                    let declared = Loader.load_defun d in
                    let declared_ty =
                      match declared with
                      | Env.Mono ty -> ty
                      | Env.Poly (vars, ty) -> Types.TForall (vars, ty)
                    in
                    (* Find span for error reporting - use first sexp as fallback *)
                    let span =
                      match sexps with
                      | first :: _ -> Syntax.Sexp.span_of first
                      | [] -> Syntax.Location.dummy_span
                    in
                    verify_defun_type ~name ~declared:declared_ty ~inferred
                      ~span
                | None ->
                    (* Function declared but not defined - skip for now
                       (could be a warning in future) *)
                    None)
            | _ -> None)
          sig_ast.sig_decls
  in

  {
    type_errors = check_result.errors;
    mismatch_errors;
    signature_env =
      (match sibling_result with Some (env, _) -> Some env | None -> None);
  }

(** {1 Diagnostic Conversion} *)

(** Convert mismatch errors to diagnostics *)
let mismatch_to_diagnostic (err : mismatch_error) : Diagnostic.t =
  Diagnostic.type_mismatch ~span:err.span ~expected:err.expected
    ~actual:err.actual ()

(** Get all diagnostics from a check result *)
let diagnostics_of_result (result : check_result) : Diagnostic.t list =
  let type_diagnostics = Diagnostic.of_unify_errors result.type_errors in
  let mismatch_diagnostics =
    List.map mismatch_to_diagnostic result.mismatch_errors
  in
  type_diagnostics @ mismatch_diagnostics
