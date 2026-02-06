(** Signature search path and module resolution.

    This module implements the search path configuration (R15) and module
    discovery order (R16) for finding `.tart` signature files.

    Discovery order: 1. Sibling file: `module.tart` next to `module.el` 2.
    Search path: Each directory in the configured search path 3. Stdlib: Bundled
    signatures shipped with tart

    The first match wins, allowing project-local overrides. *)

module Loc = Syntax.Location
module V = Emacs_version

(** {1 Search Path Configuration} *)

type t = {
  search_dirs : string list;
      (** Directories to search, in order of precedence *)
  stdlib_dir : string option;  (** Path to bundled stdlib directory, if any *)
  emacs_version : V.version option;  (** Emacs version for typings lookup *)
  typings_root : string option;
      (** Root directory for versioned typings (typings/emacs/) *)
}
(** Search path configuration. Contains a list of directories to search for
    `.tart` files. *)

(** Empty search path (no directories). *)
let empty =
  {
    search_dirs = [];
    stdlib_dir = None;
    emacs_version = None;
    typings_root = None;
  }

(** Create a search path from a list of directories. *)
let of_dirs dirs =
  {
    search_dirs = dirs;
    stdlib_dir = None;
    emacs_version = None;
    typings_root = None;
  }

(** Create a search path with stdlib. *)
let with_stdlib stdlib_dir t = { t with stdlib_dir = Some stdlib_dir }

(** Set Emacs version for typings lookup. *)
let with_emacs_version version t = { t with emacs_version = Some version }

(** Set the typings root directory (e.g., /path/to/typings/emacs). *)
let with_typings_root root t = { t with typings_root = Some root }

(** Add a directory to the front of the search path. *)
let prepend_dir dir t = { t with search_dirs = dir :: t.search_dirs }

(** Add a directory to the end of the search path. *)
let append_dir dir t = { t with search_dirs = t.search_dirs @ [ dir ] }

(** {1 File Discovery} *)

(** Check if a file exists. *)
let file_exists path =
  try
    let _ = Unix.stat path in
    true
  with Unix.Unix_error _ -> false

(** Check if a directory exists. *)
let dir_exists path =
  try
    let st = Unix.stat path in
    st.Unix.st_kind = Unix.S_DIR
  with Unix.Unix_error _ -> false

(** {2 Version Fallback Chain}

    Implements R3 of Spec 24: search order is exact → minor → major → latest.
    For version 31.0.50: try 31.0.50/ then 31.0/ then 31/ then latest/ *)

(** Generate version fallback candidates for a given version.

    For 31.0.50 returns ["31.0.50"; "31.0"; "31"; "latest"] For 31.0 returns
    ["31.0"; "31"; "latest"] For 31 returns ["31"; "latest"] *)
let version_fallback_candidates (v : V.version) : string list =
  let full = V.version_to_string v in
  let minor_only = V.version_to_dir v in
  let major_only = string_of_int v.major in
  (* Build list, removing duplicates while preserving order *)
  let candidates =
    [ full; minor_only; major_only; "latest" ]
    |> List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) []
    |> List.rev
  in
  candidates

(** Find a file in versioned typings directory.

    Searches the fallback chain: exact → minor → major → latest.
    For each version candidate, looks in {typings_root}/{candidate}/c-core/{module}.tart *)
let find_in_versioned_typings ~(typings_root : string) ~(version : V.version)
    (module_name : string) : string option =
  let candidates = version_fallback_candidates version in
  let rec try_candidates = function
    | [] -> None
    | ver_str :: rest ->
        let path =
          Filename.concat typings_root ver_str |> fun dir ->
          Filename.concat dir "c-core" |> fun dir ->
          Filename.concat dir (module_name ^ ".tart")
        in
        if file_exists path then Some path else try_candidates rest
  in
  try_candidates candidates

(** Find a typings directory for a version using fallback chain.

    Returns the first existing directory in the fallback chain, or None if no
    version directory exists. *)
let find_typings_dir ~(typings_root : string) ~(version : V.version) :
    string option =
  let candidates = version_fallback_candidates version in
  let rec try_candidates = function
    | [] -> None
    | ver_str :: rest ->
        let dir = Filename.concat typings_root ver_str in
        if dir_exists dir then Some dir else try_candidates rest
  in
  try_candidates candidates

(** Try to find a `.tart` file for a module in a directory. *)
let find_in_dir (module_name : string) (dir : string) : string option =
  let path = Filename.concat dir (module_name ^ ".tart") in
  if file_exists path then Some path else None

(** Find a `.tart` file using the search path. Searches in order: each
    search_dir, then versioned typings (with fallback), then stdlib_dir.

    @param module_name The module name (e.g., "cl-lib")
    @return The path to the `.tart` file, if found *)
let find_signature (t : t) (module_name : string) : string option =
  (* Search each directory in order *)
  let rec search = function
    | [] -> (
        (* Try versioned typings if configured *)
        match (t.typings_root, t.emacs_version) with
        | Some root, Some version -> (
            match
              find_in_versioned_typings ~typings_root:root ~version module_name
            with
            | Some path -> Some path
            | None -> (
                (* Fall back to stdlib if configured *)
                match t.stdlib_dir with
                | Some stdlib_dir -> find_in_dir module_name stdlib_dir
                | None -> None))
        | _ -> (
            (* No versioned typings, check stdlib if configured *)
            match t.stdlib_dir with
            | Some stdlib_dir -> find_in_dir module_name stdlib_dir
            | None -> None))
    | dir :: rest -> (
        match find_in_dir module_name dir with
        | Some path -> Some path
        | None -> search rest)
  in
  search t.search_dirs

(** Find a sibling `.tart` file for an `.el` file.

    @param el_path Path to the `.el` file being type-checked
    @param module_name The required module name
    @return The path to the sibling `.tart` file, if found *)
let find_sibling (el_path : string) (module_name : string) : string option =
  let dir = Filename.dirname el_path in
  find_in_dir module_name dir

(** {1 Signature Errors} *)

(** Error kind for signature file issues *)
type sig_error_kind =
  | LexerError of string  (** Lexer error (e.g., invalid character) *)
  | ParseError of string  (** Parser error (e.g., invalid syntax) *)
  | ValidationError of string  (** Validation error (e.g., unbound type var) *)
  | IOError of string  (** File read error *)

type sig_error = {
  path : string;  (** Path to the .tart file *)
  kind : sig_error_kind;  (** Type of error *)
  span : Loc.span;  (** Location in the file *)
}
(** An error that occurred while loading a signature file *)

(** Format a sig_error for display *)
let string_of_sig_error (e : sig_error) : string =
  let kind_str =
    match e.kind with
    | LexerError msg -> Printf.sprintf "Lexer error: %s" msg
    | ParseError msg -> Printf.sprintf "Parse error: %s" msg
    | ValidationError msg -> Printf.sprintf "Validation error: %s" msg
    | IOError msg -> Printf.sprintf "I/O error: %s" msg
  in
  Printf.sprintf "%s:%d:%d: %s" e.path e.span.start_pos.line
    e.span.start_pos.col kind_str

(** {1 Module Resolution} *)

(** Parse a `.tart` file and return its signature AST or errors.

    @param path Path to the `.tart` file
    @return Ok signature or Error list of errors *)
let parse_signature_file_with_errors (path : string) :
    (Sig_ast.signature, sig_error list) result =
  try
    let content = In_channel.with_open_text path In_channel.input_all in
    let parse_result = Syntax.Read.parse_string ~filename:path content in
    (* Check for lexer errors *)
    if parse_result.errors <> [] then
      Error
        (List.map
           (fun (e : Syntax.Read.parse_error) ->
             { path; kind = LexerError e.message; span = e.span })
           parse_result.errors)
    else
      (* Extract module name from filename *)
      let basename = Filename.basename path in
      let module_name = Filename.chop_suffix basename ".tart" in
      match Sig_parser.parse_signature ~module_name parse_result.sexps with
      | Ok sig_file -> Ok sig_file
      | Error errors ->
          Error
            (List.map
               (fun (e : Sig_parser.parse_error) ->
                 { path; kind = ParseError e.message; span = e.span })
               errors)
  with
  | Sys_error msg ->
      Error [ { path; kind = IOError msg; span = Loc.dummy_span } ]
  | _ ->
      Error
        [
          {
            path;
            kind = IOError "Unknown error reading file";
            span = Loc.dummy_span;
          };
        ]

(** Parse and validate a `.tart` file, returning all errors.

    This function combines parsing and validation to catch all issues:
    - Lexer errors (invalid characters)
    - Parser errors (invalid syntax)
    - Validation errors (unbound type variables, invalid types)

    Prelude types (buffer, window, etc.) are automatically available.

    @param path Path to the `.tart` file
    @return Ok signature or Error list of all errors *)
let parse_and_validate_signature (path : string) :
    (Sig_ast.signature, sig_error list) result =
  match parse_signature_file_with_errors path with
  | Error errors -> Error errors
  | Ok sig_file ->
      (* Now validate the parsed signature, allowing prelude types *)
      let validation_errors =
        Sig_loader.validate_signature_all
          ~prelude_type_names:Prelude.prelude_type_names sig_file
      in
      if validation_errors = [] then Ok sig_file
      else
        Error
          (List.map
             (fun (e : Sig_loader.load_error) ->
               { path; kind = ValidationError e.message; span = e.span })
             validation_errors)

(** Parse a `.tart` file and return its signature AST.

    @param path Path to the `.tart` file
    @return The parsed signature, or None if parsing fails *)
let parse_signature_file (path : string) : Sig_ast.signature option =
  match parse_signature_file_with_errors path with
  | Ok sig_file -> Some sig_file
  | Error _ -> None

(** Create a module resolver from a search path configuration. This resolver
    implements the full discovery order: 1. Sibling `.tart` next to the current
    file (if el_path provided) 2. Each directory in the search path 3. Bundled
    stdlib

    @param search_path The search path configuration
    @param el_path Optional path to the `.el` file being type-checked
    @return A resolver function suitable for load_signature_with_resolver *)
let make_resolver ?(el_path : string option) (search_path : t) :
    Sig_loader.module_resolver =
 fun module_name ->
  (* Step 1: Check for sibling .tart file *)
  let sibling_path =
    match el_path with
    | Some path -> find_sibling path module_name
    | None -> None
  in
  let tart_path =
    match sibling_path with
    | Some _ as p -> p
    | None ->
        (* Step 2 & 3: Search path and stdlib *)
        find_signature search_path module_name
  in
  match tart_path with Some path -> parse_signature_file path | None -> None

(** Create a has_el_file checker from a search path configuration.

    Checks whether a module has a corresponding .el file, used to enforce Spec
    07 R19: auxiliary .tart files (no .el) can be included but not opened.

    - Sibling .tart files found next to the current .el → has .el (the current
      file's directory is searched because an .el exists there)
    - Typings and stdlib .tart files → always considered to have .el (they
      correspond to Emacs built-in modules)
    - Search path .tart files → check for a sibling .el file

    @param el_path Optional path to the .el file being type-checked
    @param search_path The search path configuration
    @return A function that checks if a module has a corresponding .el file *)
let make_has_el_file ?(el_path : string option) (search_path : t) :
    string -> bool =
 fun module_name ->
  (* Step 1: Check for sibling .tart file *)
  let sibling_path =
    match el_path with
    | Some path -> find_sibling path module_name
    | None -> None
  in
  match sibling_path with
  | Some _ ->
      (* Found as sibling to the current .el file — the .el must exist *)
      true
  | None -> (
      (* Step 2: Check search path directories *)
      let rec check_search_dirs = function
        | [] -> None
        | dir :: rest -> (
            match find_in_dir module_name dir with
            | Some tart_path ->
                (* Found in search dir — check for sibling .el *)
                let el_path =
                  Filename.concat
                    (Filename.dirname tart_path)
                    (module_name ^ ".el")
                in
                Some (file_exists el_path)
            | None -> check_search_dirs rest)
      in
      match check_search_dirs search_path.search_dirs with
      | Some has_el -> has_el
      | None ->
          (* Found in typings/stdlib or not found at all — allow open *)
          true)

(** {1 Loading Utilities} *)

(** Check if a signature has open or include directives. Signatures with these
    directives cannot be validated standalone because they depend on types from
    other modules. *)
let has_external_deps (sig_file : Sig_ast.signature) : bool =
  List.exists
    (function Sig_ast.DOpen _ | Sig_ast.DInclude _ -> true | _ -> false)
    sig_file.sig_decls

(** Load signatures for a module using the search path.

    @param search_path The search path configuration
    @param el_path Optional path to the `.el` file being type-checked
    @param with_prelude
      If true, prelude types are available in loaded signatures
    @param env Base type environment to extend
    @param module_name The module name to load
    @return Extended type environment, or None if module not found *)
let load_module ~(search_path : t) ?(el_path : string option)
    ?(with_prelude = true) ~(env : Core.Type_env.t) (module_name : string) :
    Core.Type_env.t option =
  let resolver = make_resolver ?el_path search_path in
  let has_el_file = make_has_el_file ?el_path search_path in
  match resolver module_name with
  | None -> None
  | Some sig_file ->
      (* Validate the signature if it has no external dependencies.
         Signatures with open/include can't be validated standalone
         because they reference types from other modules.
         When with_prelude is true, include prelude type names in validation
         so signatures can reference buffer, window, etc. *)
      let prelude_type_names =
        if with_prelude then Prelude.prelude_type_names else []
      in
      let valid =
        if has_external_deps sig_file then true
        else
          match Sig_loader.validate_signature ~prelude_type_names sig_file with
          | Ok () -> true
          | Error _ -> false
      in
      if valid then
        let prelude_ctx =
          if with_prelude then Some (Prelude.prelude_type_context ()) else None
        in
        match
          Sig_loader.load_signature_with_resolver ?prelude_ctx ~has_el_file
            ~resolver env sig_file
        with
        | Ok new_env -> Some new_env
        | Error _ -> None
      else None

(** Load signatures for a module and also return the signature AST.

    Like [load_module] but also returns the parsed signature AST for further
    processing (e.g., instance extraction).

    @param with_prelude
      If true, prelude types are available in loaded signatures *)
let load_module_with_sig ~(search_path : t) ?(el_path : string option)
    ?(with_prelude = true) ~(env : Core.Type_env.t) (module_name : string) :
    (Core.Type_env.t * Sig_ast.signature) option =
  let resolver = make_resolver ?el_path search_path in
  let has_el_file = make_has_el_file ?el_path search_path in
  match resolver module_name with
  | None -> None
  | Some sig_file ->
      let prelude_type_names =
        if with_prelude then Prelude.prelude_type_names else []
      in
      let valid =
        if has_external_deps sig_file then true
        else
          match Sig_loader.validate_signature ~prelude_type_names sig_file with
          | Ok () -> true
          | Error _ -> false
      in
      if valid then
        let prelude_ctx =
          if with_prelude then Some (Prelude.prelude_type_context ()) else None
        in
        match
          Sig_loader.load_signature_with_resolver ?prelude_ctx ~has_el_file
            ~resolver env sig_file
        with
        | Ok new_env -> Some (new_env, sig_file)
        | Error _ -> None
      else None

(** List all .tart files in a c-core directory. *)
let list_c_core_files (c_core_dir : string) : string list =
  if dir_exists c_core_dir then
    Sys.readdir c_core_dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".tart")
    |> List.map (fun f -> Filename.concat c_core_dir f)
    |> List.sort String.compare
  else []

(** Load all c-core signature files into a type environment.

    Iterates through all .tart files in the c-core directory and loads their
    signatures into the environment. Each file is treated as an independent
    module. Duplicate function definitions across files will use the last loaded
    value.

    @param c_core_dir Path to the c-core directory containing .tart files
    @param with_prelude
      If true, prelude types are available in loaded signatures
    @param env Base type environment to extend
    @return Extended type environment with all c-core signatures *)
let load_c_core_files ~(c_core_dir : string) ?(with_prelude = true)
    (env : Core.Type_env.t) : Core.Type_env.t =
  let files = list_c_core_files c_core_dir in
  let prelude_ctx =
    if with_prelude then Some (Prelude.prelude_type_context ()) else None
  in
  let prelude_type_names =
    if with_prelude then Prelude.prelude_type_names else []
  in
  List.fold_left
    (fun acc_env path ->
      match parse_signature_file path with
      | None -> acc_env (* Skip files that fail to parse *)
      | Some sig_file ->
          (* Validate standalone signatures *)
          let valid =
            if has_external_deps sig_file then true
            else
              match
                Sig_loader.validate_signature ~prelude_type_names sig_file
              with
              | Ok () -> true
              | Error _ -> false
          in
          if valid then
            (* Use empty resolver since c-core files shouldn't have deps *)
            let resolver _ = None in
            match
              Sig_loader.load_signature_with_resolver ?prelude_ctx ~resolver
                acc_env sig_file
            with
            | Ok new_env -> new_env
            | Error _ -> acc_env
          else acc_env)
    env files

(** Load c-core signatures from versioned typings.

    Uses the version fallback chain to find the c-core directory, then loads all
    .tart files from it.

    @param search_path The search path configuration
    @param env Base type environment to extend
    @return Extended type environment with c-core signatures *)
let load_c_core ~(search_path : t) (env : Core.Type_env.t) : Core.Type_env.t =
  match (search_path.typings_root, search_path.emacs_version) with
  | Some typings_root, Some version -> (
      match find_typings_dir ~typings_root ~version with
      | Some typings_dir ->
          let c_core_dir = Filename.concat typings_dir "c-core" in
          load_c_core_files ~c_core_dir env
      | None -> env)
  | _ -> env

(** Load lisp-core signatures from versioned typings.

    Uses the version fallback chain to find the lisp-core directory, then loads
    all .tart files from it. Lisp-core contains signatures for functions and
    macros defined in Emacs Lisp (as opposed to C primitives in c-core).

    @param search_path The search path configuration
    @param env Base type environment to extend
    @return Extended type environment with lisp-core signatures *)
let load_lisp_core ~(search_path : t) (env : Core.Type_env.t) : Core.Type_env.t
    =
  match (search_path.typings_root, search_path.emacs_version) with
  | Some typings_root, Some version -> (
      match find_typings_dir ~typings_root ~version with
      | Some typings_dir ->
          let lisp_core_dir = Filename.concat typings_dir "lisp-core" in
          if Sys.file_exists lisp_core_dir && Sys.is_directory lisp_core_dir
          then load_c_core_files ~c_core_dir:lisp_core_dir env
          else env
      | None -> env)
  | _ -> env
