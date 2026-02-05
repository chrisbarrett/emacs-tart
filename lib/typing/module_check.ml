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

let search_path config = config.search_path
let with_search_path search_path config = { config with search_path }

(** {1 Signature Mismatch Errors} *)

type mismatch_error = {
  name : string;
  expected : Types.typ;
  actual : Types.typ;
  impl_span : Syntax.Location.span;
      (** Location of implementation in .el file *)
  sig_span : Syntax.Location.span;  (** Location of signature in .tart file *)
}
(** Error when implementation doesn't match signature *)

(** {1 Missing Signature Warnings} *)

type missing_signature_warning = { name : string; span : Syntax.Location.span }
(** Warning when a public function is not in the signature file *)

(** {1 Kind Errors} *)

type kind_check_error = {
  kind_error : Kind_infer.kind_error;
  span : Syntax.Location.span;
}
(** A kind error with its source location *)

(** {1 Module Check Result} *)

type check_result = {
  type_errors : Unify.error list;  (** Type errors from inference *)
  mismatch_errors : mismatch_error list;  (** Signature mismatch errors *)
  missing_signature_warnings : missing_signature_warning list;
      (** Warnings for public functions not in signature *)
  undefined_errors : Infer.undefined_var list;  (** Undefined variable errors *)
  exhaustiveness_warnings : Exhaustiveness.warning list;
      (** Warnings for non-exhaustive pcase matches *)
  kind_errors : kind_check_error list;
      (** Kind mismatch errors in signatures *)
  signature_env : Env.t option;
      (** Environment from loaded signature, if any *)
  final_env : Env.t;  (** Final type environment after checking *)
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

(** {1 Autoload Detection (R7)} *)

(** Extract candidate module prefixes from a function name.

    Emacs convention: function names are `package-name-function-name`. We try
    progressively shorter prefixes to find a matching module.

    Example: `my-package-autoload-fn` yields
    [`my-package-autoload`; `my-package`; `my`] *)
let extract_module_prefixes (name : string) : string list =
  let parts = String.split_on_char '-' name in
  (* Generate prefixes from longest to shortest (but never the full name).
     For ["my"; "package"; "fn"], we want:
     - "my-package" (first 2 parts)
     - "my" (first 1 part) *)
  let rec build_prefixes n acc =
    if n < 1 then acc
    else
      let prefix_parts = List.filteri (fun i _ -> i < n) parts in
      let prefix = String.concat "-" prefix_parts in
      build_prefixes (n - 1) (prefix :: acc)
  in
  (* Start from (num_parts - 1), never include the full name *)
  let num_parts = List.length parts in
  if num_parts <= 1 then [] else build_prefixes (num_parts - 1) [] |> List.rev

(** Collect all symbol references in function call position.

    We only care about symbols at the head of a list (function calls), not
    variable references. *)
let rec collect_call_symbols (sexp : Syntax.Sexp.t) : string list =
  let open Syntax.Sexp in
  match sexp with
  | List (Symbol (name, _) :: args, _) ->
      (* Function call - collect the function name and recurse into args *)
      let from_args = List.concat_map collect_call_symbols args in
      name :: from_args
  | List (fn :: args, _) ->
      (* Non-symbol in call position - just recurse *)
      let from_fn = collect_call_symbols fn in
      let from_args = List.concat_map collect_call_symbols args in
      from_fn @ from_args
  | List ([], _) -> []
  | Vector (elems, _) -> List.concat_map collect_call_symbols elems
  | Cons (car, cdr, _) -> collect_call_symbols car @ collect_call_symbols cdr
  | _ -> []

(** Collect all call symbols from a list of top-level forms *)
let collect_all_call_symbols (sexps : Syntax.Sexp.t list) : string list =
  let all = List.concat_map collect_call_symbols sexps in
  (* Deduplicate *)
  List.sort_uniq String.compare all

(** Try to load a module for an autoloaded function.

    Tries progressively shorter prefixes until one matches a `.tart` file. *)
let try_load_autoload_module ~(config : config) ~(el_path : string option)
    ~(env : Env.t) ~(loaded_modules : string list ref) (name : string) :
    Env.t option =
  let prefixes = extract_module_prefixes name in
  let rec try_prefixes = function
    | [] -> None
    | prefix :: rest ->
        if List.mem prefix !loaded_modules then
          (* Already tried this module - skip *)
          try_prefixes rest
        else (
          loaded_modules := prefix :: !loaded_modules;
          match
            Search.load_module ~search_path:config.search_path ?el_path ~env
              prefix
          with
          | Some env' -> Some env'
          | None -> try_prefixes rest)
  in
  try_prefixes prefixes

(** Load signatures for autoloaded functions (R7).

    Scans the code for function calls not in the environment, extracts module
    prefixes from their names, and tries to load matching `.tart` files. *)
let load_autoload_signatures ~(config : config) ~(el_path : string option)
    ~(env : Env.t) ~(required_modules : string list)
    (sexps : Syntax.Sexp.t list) : Env.t =
  (* Track which modules we've already tried loading *)
  let loaded_modules = ref required_modules in

  (* Collect all function calls *)
  let call_symbols = collect_all_call_symbols sexps in

  (* For each symbol not in env, try to load its module *)
  List.fold_left
    (fun env name ->
      match Env.lookup name env with
      | Some _ ->
          (* Already in environment - skip *)
          env
      | None -> (
          match
            try_load_autoload_module ~config ~el_path ~env ~loaded_modules name
          with
          | Some env' -> env'
          | None -> env))
    env call_symbols

(** {1 Signature Loading} *)

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
      let prelude_ctx = Sig.Prelude.prelude_type_context () in
      let env =
        Loader.load_signature_with_resolver ~prelude_ctx ~resolver base_env
          sig_ast
      in
      Some (env, sig_ast)
  | None -> None

(** {1 Internal Function Detection} *)

(** Helper to check if string contains substring *)
let string_contains s ~substring =
  let len = String.length substring in
  let slen = String.length s in
  if len > slen then false
  else
    let rec check i =
      if i > slen - len then false
      else if String.sub s i len = substring then true
      else check (i + 1)
    in
    check 0

(** Check if a function name is internal (contains "--") *)
let is_internal_name (name : string) : bool =
  (* Emacs convention: internal functions have "--" in their name *)
  string_contains name ~substring:"--"

(** Extract defun names and their spans from parsed sexps *)
let extract_defun_spans (sexps : Syntax.Sexp.t list) :
    (string * Syntax.Location.span) list =
  let open Syntax.Sexp in
  List.filter_map
    (fun sexp ->
      match sexp with
      | List (Symbol ("defun", _) :: Symbol (name, _) :: _, span) ->
          Some (name, span)
      | _ -> None)
    sexps

(** {1 Implementation Verification} *)

(** Instantiate a TForall type with fresh type variables. Returns the body with
    bound variables substituted. *)
let instantiate_forall (ty : Types.typ) : Types.typ =
  match ty with
  | Types.TForall (vars, body) ->
      (* Create fresh type variables for each bound variable.
         Level 0 is fine since we're just doing a one-off comparison. *)
      let subst = List.map (fun v -> (v, Types.fresh_tvar 0)) vars in
      (* Substitute bound variables (which appear as TCon) with fresh tvars *)
      let rec substitute ty =
        match ty with
        | Types.TCon name -> (
            match List.assoc_opt name subst with Some tv -> tv | None -> ty)
        | Types.TVar { contents = Types.Link t } -> substitute t
        | Types.TVar _ -> ty
        | Types.TApp (con, args) ->
            Types.TApp (substitute con, List.map substitute args)
        | Types.TArrow (params, ret) ->
            Types.TArrow (List.map substitute_param params, substitute ret)
        | Types.TForall (vs, t) -> Types.TForall (vs, substitute t)
        | Types.TUnion tys -> Types.TUnion (List.map substitute tys)
        | Types.TTuple tys -> Types.TTuple (List.map substitute tys)
        | Types.TRow { row_fields; row_var } ->
            Types.TRow
              {
                row_fields =
                  List.map (fun (n, t) -> (n, substitute t)) row_fields;
                row_var = Option.map substitute row_var;
              }
      and substitute_param (p : Types.param) : Types.param =
        match p with
        | Types.PPositional t -> Types.PPositional (substitute t)
        | Types.POptional t -> Types.POptional (substitute t)
        | Types.PRest t -> Types.PRest (substitute t)
        | Types.PKey (name, t) -> Types.PKey (name, substitute t)
      in
      substitute body
  | _ -> ty

(** Check if a defun's inferred type is compatible with its declared type.

    This performs a simple structural check. The declared type should be an
    instance of the inferred type (the inferred type can be more general). *)
let verify_defun_type ~(name : string) ~(declared : Types.typ)
    ~(inferred : Types.typ) ~(impl_span : Syntax.Location.span)
    ~(sig_span : Syntax.Location.span) : mismatch_error option =
  (* Reset type variable counter for fresh comparison *)
  Types.reset_tvar_counter ();

  (* Instantiate polymorphic types before comparison.
     If the inferred type is forall a. a -> a and declared is int -> int,
     we instantiate to get ?a -> ?a, then unify with int -> int. *)
  let declared' = instantiate_forall (Types.repr declared) in
  let inferred' = instantiate_forall (Types.repr inferred) in

  (* Try to unify the declared type with the inferred type.
     The declared type should be compatible with (possibly more specific than)
     the inferred type. *)
  let c = Constraint.equal declared' inferred' impl_span in
  match Unify.solve (Constraint.add c Constraint.empty) with
  | Ok () -> None (* Types are compatible *)
  | Error _ ->
      (* Types don't match - report mismatch *)
      Some { name; expected = declared; actual = inferred; impl_span; sig_span }

(** {1 Kind Checking} *)

(** Build a kind environment from type variable binders, respecting explicit
    kind annotations. *)
let build_scope_kind_env (binders : Sig.Sig_ast.tvar_binder list) : Kind.env =
  List.fold_left
    (fun env (b : Sig.Sig_ast.tvar_binder) ->
      match b.kind with
      | Some sk ->
          Kind.extend_env b.name (Kind.KConcrete (Kind.of_sig_kind sk)) env
      | None -> Kind.extend_env b.name (Kind.fresh_kvar ()) env)
    Kind.empty_env binders

(** Check kinds for a single declaration. Returns any kind errors found.
    @param scope_env Kind environment from enclosing type-scope (if any) *)
let rec check_decl_kinds_with_scope (scope_env : Kind.env)
    (decl : Sig.Sig_ast.decl) : kind_check_error list =
  match decl with
  | Sig.Sig_ast.DDefun d ->
      let result = Kind_infer.infer_defun_kinds_with_scope scope_env d in
      List.map (fun e -> { kind_error = e; span = d.defun_loc }) result.errors
  | Sig.Sig_ast.DType d ->
      let result = Kind_infer.infer_type_decl_kinds_with_scope scope_env d in
      List.map (fun e -> { kind_error = e; span = d.type_loc }) result.errors
  | Sig.Sig_ast.DData d ->
      let result = Kind_infer.infer_data_kinds_with_scope scope_env d in
      List.map (fun e -> { kind_error = e; span = d.data_loc }) result.errors
  | Sig.Sig_ast.DTypeScope d ->
      (* Build kind environment from scope's binders (respecting explicit kind annotations) *)
      let inner_scope_env = build_scope_kind_env d.scope_tvar_binders in
      (* Merge with outer scope environment (inner shadows outer) *)
      let combined_env = Kind.merge scope_env inner_scope_env in
      (* Recursively check kinds for declarations inside the scope with combined env *)
      List.concat_map (check_decl_kinds_with_scope combined_env) d.scope_decls
  | Sig.Sig_ast.DDefvar _ | Sig.Sig_ast.DOpen _ | Sig.Sig_ast.DInclude _
  | Sig.Sig_ast.DImportStruct _ ->
      (* These declarations don't have type parameters to kind-check *)
      []

(** Check kinds for a single declaration. Returns any kind errors found. *)
let check_decl_kinds (decl : Sig.Sig_ast.decl) : kind_check_error list =
  check_decl_kinds_with_scope Kind.empty_env decl

(** Check kinds for all declarations in a signature file.

    Returns a list of kind errors with their source locations. An empty list
    means all kind checks passed. *)
let check_signature_kinds (sig_file : Sig.Sig_ast.signature) :
    kind_check_error list =
  List.concat_map check_decl_kinds sig_file.sig_decls

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

  (* Step 2b: Load c-core signatures (Emacs primitives like +, car, etc.) *)
  let base_env = Search.load_c_core ~search_path:config.search_path base_env in

  (* Step 2c: Load lisp-core signatures (macros and functions from subr.el etc.) *)
  let base_env =
    Search.load_lisp_core ~search_path:config.search_path base_env
  in

  (* Step 3: Load signatures for required modules *)
  let required_modules = extract_requires sexps in
  let env_with_requires =
    List.fold_left
      (fun env module_name ->
        match
          Search.load_module ~search_path:config.search_path ~el_path:filename
            ~env module_name
        with
        | Some env' -> env'
        | None -> env)
      base_env required_modules
  in

  (* Step 4: Load signatures for autoloaded functions (R7) *)
  let env_with_autoloads =
    load_autoload_signatures ~config ~el_path:(Some filename)
      ~env:env_with_requires ~required_modules sexps
  in

  (* Step 5: Type-check the implementation *)
  let check_result = Check.check_program ~env:env_with_autoloads sexps in

  (* Build a lookup table for defun spans from the parsed sexps *)
  let defun_spans = extract_defun_spans sexps in

  (* Step 6: If we have a signature, verify implementations match *)
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
                | Some scheme ->
                    (* Convert scheme to type, preserving polymorphism *)
                    let inferred =
                      match scheme with
                      | Env.Mono ty -> ty
                      | Env.Poly (vars, ty) -> Types.TForall (vars, ty)
                    in
                    (* Get the declared type from the signature, using prelude
                       context to resolve primitive type names to intrinsics *)
                    let prelude_ctx = Sig.Prelude.prelude_type_context () in
                    let declared = Loader.load_defun_with_ctx prelude_ctx d in
                    let declared_ty =
                      match declared with
                      | Env.Mono ty -> ty
                      | Env.Poly (vars, ty) -> Types.TForall (vars, ty)
                    in
                    (* Find implementation span - look up by name, fallback to first sexp *)
                    let impl_span =
                      match List.assoc_opt name defun_spans with
                      | Some span -> span
                      | None -> (
                          match sexps with
                          | first :: _ -> Syntax.Sexp.span_of first
                          | [] -> Syntax.Location.dummy_span)
                    in
                    (* Get signature span from the defun declaration *)
                    let sig_span = d.defun_loc in
                    verify_defun_type ~name ~declared:declared_ty ~inferred
                      ~impl_span ~sig_span
                | None ->
                    (* Function declared but not defined - skip for now
                       (could be a warning in future) *)
                    None)
            | _ -> None)
          sig_ast.sig_decls
  in

  (* Step 7: Warn on public functions not in signature file (R5, R8) *)
  let missing_signature_warnings =
    match sig_ast_opt with
    | None -> [] (* No signature file - no warnings *)
    | Some sig_ast ->
        (* Build set of declared function names *)
        let declared_names =
          List.filter_map
            (fun decl ->
              match decl with
              | Sig.Sig_ast.DDefun d -> Some d.defun_name
              | _ -> None)
            sig_ast.sig_decls
        in
        (* Get all defined functions with their spans *)
        let defined_fns = extract_defun_spans sexps in
        (* Warn on public functions not in signature *)
        List.filter_map
          (fun (name, span) ->
            if is_internal_name name then
              (* R5: Internal functions are inferred, not exported - no warning *)
              None
            else if List.mem name declared_names then
              (* Function is in signature - no warning *)
              None
            else
              (* R8: Public function not in signature - warning *)
              Some { name; span })
          defined_fns
  in

  (* Step 8: Check exhaustiveness for pcase expressions (R5 from spec 11) *)
  let exhaustiveness_warnings =
    (* Build ADT registry from loaded signature *)
    let registry =
      match sig_ast_opt with
      | Some sig_ast -> Exhaustiveness.build_registry_from_signature sig_ast
      | None -> Exhaustiveness.empty_registry
    in
    Exhaustiveness.check_all_pcases ~registry ~env:check_result.Check.env sexps
  in

  (* Step 9: Check kinds in signature file (R3 from spec 17) *)
  let kind_errors =
    match sig_ast_opt with
    | Some sig_ast -> check_signature_kinds sig_ast
    | None -> []
  in

  {
    type_errors = check_result.Check.errors;
    mismatch_errors;
    missing_signature_warnings;
    undefined_errors = check_result.Check.undefineds;
    exhaustiveness_warnings;
    kind_errors;
    signature_env =
      (match sibling_result with Some (env, _) -> Some env | None -> None);
    final_env = check_result.Check.env;
  }

(** {1 Diagnostic Conversion} *)

(** Convert mismatch errors to diagnostics *)
let mismatch_to_diagnostic (err : mismatch_error) : Diagnostic.t =
  Diagnostic.signature_mismatch ~name:err.name ~impl_span:err.impl_span
    ~impl_type:err.actual ~sig_span:err.sig_span ~sig_type:err.expected ()

(** Convert missing signature warnings to diagnostics *)
let missing_signature_to_diagnostic (warn : missing_signature_warning) :
    Diagnostic.t =
  Diagnostic.missing_signature ~span:warn.span ~name:warn.name ()

(** Convert an undefined variable error to a diagnostic *)
let undefined_to_diagnostic (candidates : string list)
    (err : Infer.undefined_var) : Diagnostic.t =
  Diagnostic.undefined_variable ~span:err.span ~name:err.name ~candidates ()

(** Convert an exhaustiveness warning to a diagnostic *)
let exhaustiveness_to_diagnostic (warn : Exhaustiveness.warning) : Diagnostic.t
    =
  Diagnostic.non_exhaustive_match ~span:warn.span ~message:warn.message ()

(** Convert a kind error to a diagnostic *)
let kind_error_to_diagnostic (err : kind_check_error) : Diagnostic.t =
  Diagnostic.of_kind_error err.span err.kind_error

(** Get all diagnostics from a check result *)
let diagnostics_of_result (result : check_result) : Diagnostic.t list =
  let type_diagnostics = Diagnostic.of_unify_errors result.type_errors in
  let mismatch_diagnostics =
    List.map mismatch_to_diagnostic result.mismatch_errors
  in
  let missing_sig_diagnostics =
    List.map missing_signature_to_diagnostic result.missing_signature_warnings
  in
  let candidates = Env.names result.final_env in
  let undefined_diagnostics =
    List.map (undefined_to_diagnostic candidates) result.undefined_errors
  in
  let exhaustiveness_diagnostics =
    List.map exhaustiveness_to_diagnostic result.exhaustiveness_warnings
  in
  let kind_diagnostics = List.map kind_error_to_diagnostic result.kind_errors in
  type_diagnostics @ mismatch_diagnostics @ missing_sig_diagnostics
  @ undefined_diagnostics @ exhaustiveness_diagnostics @ kind_diagnostics
