(** Signature search path and module resolution.

    This module implements the search path configuration (R15) and module
    discovery order (R16) for finding `.tart` signature files.

    Discovery order: 1. Sibling file: `module.tart` next to `module.el` 2.
    Search path: Each directory in the configured search path 3. Stdlib: Bundled
    signatures shipped with tart

    The first match wins, allowing project-local overrides. *)

module Loc = Syntax.Location

(** {1 Search Path Configuration} *)

type t = {
  search_dirs : string list;
      (** Directories to search, in order of precedence *)
  stdlib_dir : string option;  (** Path to bundled stdlib directory, if any *)
}
(** Search path configuration. Contains a list of directories to search for
    `.tart` files. *)

(** Empty search path (no directories). *)
let empty = { search_dirs = []; stdlib_dir = None }

(** Create a search path from a list of directories. *)
let of_dirs dirs = { search_dirs = dirs; stdlib_dir = None }

(** Create a search path with stdlib. *)
let with_stdlib stdlib_dir t = { t with stdlib_dir = Some stdlib_dir }

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

(** Try to find a `.tart` file for a module in a directory. *)
let find_in_dir (module_name : string) (dir : string) : string option =
  let path = Filename.concat dir (module_name ^ ".tart") in
  if file_exists path then Some path else None

(** Find a `.tart` file using the search path. Searches in order: each
    search_dir, then stdlib_dir.

    @param module_name The module name (e.g., "cl-lib")
    @return The path to the `.tart` file, if found *)
let find_signature (t : t) (module_name : string) : string option =
  (* Search each directory in order *)
  let rec search = function
    | [] -> (
        (* Finally, check stdlib if configured *)
        match t.stdlib_dir with
        | Some stdlib_dir -> find_in_dir module_name stdlib_dir
        | None -> None)
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

(** {1 Module Resolution} *)

(** Parse a `.tart` file and return its signature AST.

    @param path Path to the `.tart` file
    @return The parsed signature, or None if parsing fails *)
let parse_signature_file (path : string) : Sig_ast.signature option =
  try
    let content = In_channel.with_open_text path In_channel.input_all in
    let parse_result = Syntax.Read.parse_string ~filename:path content in
    (* Check for parse errors *)
    if parse_result.errors <> [] then None
    else
      (* Extract module name from filename *)
      let basename = Filename.basename path in
      let module_name = Filename.chop_suffix basename ".tart" in
      match Sig_parser.parse_signature ~module_name parse_result.sexps with
      | Ok sig_file -> Some sig_file
      | Error _ -> None
  with _ -> None

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
    @param env Base type environment to extend
    @param module_name The module name to load
    @return Extended type environment, or None if module not found *)
let load_module ~(search_path : t) ?(el_path : string option)
    ~(env : Core.Type_env.t) (module_name : string) : Core.Type_env.t option =
  let resolver = make_resolver ?el_path search_path in
  match resolver module_name with
  | None -> None
  | Some sig_file ->
      (* Validate the signature if it has no external dependencies.
         Signatures with open/include can't be validated standalone
         because they reference types from other modules. *)
      let valid =
        if has_external_deps sig_file then true
        else
          match Sig_loader.validate_signature sig_file with
          | Ok () -> true
          | Error _ -> false
      in
      if valid then
        let new_env =
          Sig_loader.load_signature_with_resolver ~resolver env sig_file
        in
        Some new_env
      else None

(** Load signatures for a module and also return the signature AST.

    Like [load_module] but also returns the parsed signature AST for further
    processing (e.g., instance extraction). *)
let load_module_with_sig ~(search_path : t) ?(el_path : string option)
    ~(env : Core.Type_env.t) (module_name : string) :
    (Core.Type_env.t * Sig_ast.signature) option =
  let resolver = make_resolver ?el_path search_path in
  match resolver module_name with
  | None -> None
  | Some sig_file ->
      let valid =
        if has_external_deps sig_file then true
        else
          match Sig_loader.validate_signature sig_file with
          | Ok () -> true
          | Error _ -> false
      in
      if valid then
        let new_env =
          Sig_loader.load_signature_with_resolver ~resolver env sig_file
        in
        Some (new_env, sig_file)
      else None
