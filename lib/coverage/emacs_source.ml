(** Emacs source directory discovery.

    Discovers the Emacs C source directory for scanning DEFUNs and DEFVARs.
    Supports auto-detection via running Emacs, or explicit path override.

    @see Spec 29, R1-R3 for requirements. *)

(** {1 Types} *)

type discovery_result =
  | Found of { source_dir : string; version : string }
  | NotFound of string
  | InvalidPath of string

(** {1 Path Validation} *)

(** Check if a directory looks like valid Emacs source.

    A valid source directory contains src/*.c files. *)
let is_valid_emacs_source (path : string) : bool =
  let src_dir = Filename.concat path "src" in
  if not (Sys.file_exists src_dir && Sys.is_directory src_dir) then false
  else
    let entries = Sys.readdir src_dir |> Array.to_list in
    List.exists (fun f -> Filename.check_suffix f ".c") entries

(** {1 Auto-Detection} *)

(** Query Emacs for source directory and version.

    Runs:
    {v
    emacs --batch --eval '(message "%S" (list :source-directory find-function-C-source-directory :version emacs-version))'
    v}

    Returns (source_dir option, version option). *)
let query_emacs () : (string option * string option) option =
  let cmd =
    "emacs --batch --eval '(message \"%S\" (list :source-directory \
     find-function-C-source-directory :version emacs-version))' 2>&1"
  in
  try
    let ic = Unix.open_process_in cmd in
    let output =
      try
        let buf = Buffer.create 256 in
        (try
           while true do
             Buffer.add_channel buf ic 1
           done
         with End_of_file -> ());
        Buffer.contents buf
      with _ -> ""
    in
    let status = Unix.close_process_in ic in
    match status with
    | Unix.WEXITED 0 ->
        (* Parse the S-expression output.
           Expected format: (:source-directory "/path/to/src" :version "31.0.50")
           or (:source-directory nil :version "31.0.50") *)
        let source_re = Str.regexp {|:source-directory \("\([^"]+\)"\|nil\)|} in
        let version_re = Str.regexp {|:version "\([^"]+\)"|} in
        let source_dir =
          if Str.string_match source_re output 0 then
            try Some (Str.matched_group 2 output) with Not_found -> None
          else None
        in
        let version =
          try
            let _ = Str.search_forward version_re output 0 in
            Some (Str.matched_group 1 output)
          with Not_found -> None
        in
        Some (source_dir, version)
    | _ -> None
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> None
  | _ -> None

(** Detect Emacs source directory automatically.

    Uses `find-function-C-source-directory` from a running Emacs to find the
    source location. Returns [NotFound] with helpful message if:
    - Emacs is not installed
    - Source is not available (e.g., package manager install without source) *)
let detect () : discovery_result =
  match query_emacs () with
  | None -> NotFound "Emacs is not installed or not on PATH"
  | Some (None, _) ->
      NotFound
        "Emacs source directory not available. Install Emacs source or use \
         --emacs-source"
  | Some (Some path, version_opt) ->
      if is_valid_emacs_source path then
        let version = Option.value ~default:"unknown" version_opt in
        Found { source_dir = path; version }
      else
        InvalidPath
          (Printf.sprintf "Detected path %s does not contain src/*.c files" path)

(** {1 Explicit Path} *)

(** Use an explicit source path.

    Validates that the path contains src/*.c files. *)
let from_path (path : string) : discovery_result =
  if not (Sys.file_exists path) then
    InvalidPath (Printf.sprintf "Path does not exist: %s" path)
  else if not (Sys.is_directory path) then
    InvalidPath (Printf.sprintf "Path is not a directory: %s" path)
  else if not (is_valid_emacs_source path) then
    InvalidPath (Printf.sprintf "Path does not contain src/*.c files: %s" path)
  else
    (* Try to get version from build info if available *)
    let version =
      let configure_info = Filename.concat path "src/config.h" in
      if Sys.file_exists configure_info then
        try
          let ic = open_in configure_info in
          let rec find_version () =
            try
              let line = input_line ic in
              (* Look for #define PACKAGE_VERSION "31.0.50" *)
              if
                String.length line > 24
                && String.sub line 0 24 = "#define PACKAGE_VERSION "
              then
                let start = 25 in
                let len = String.length line - start - 1 in
                if len > 0 then Some (String.sub line start len) else None
              else find_version ()
            with End_of_file -> None
          in
          let v = find_version () in
          close_in ic;
          v
        with _ -> None
      else None
    in
    let version = Option.value ~default:"unknown" version in
    Found { source_dir = path; version }

(** {1 Combined Discovery} *)

(** Discover Emacs source, using explicit path if provided.

    @param explicit_path If [Some path], uses that path; otherwise auto-detects
*)
let discover ~(explicit_path : string option) : discovery_result =
  match explicit_path with Some path -> from_path path | None -> detect ()

(** {1 Error Formatting} *)

(** Format a discovery error as a user-friendly message. *)
let format_error (result : discovery_result) : string =
  match result with
  | Found _ -> ""
  | NotFound msg ->
      Printf.sprintf
        "Error: %s\n\n\
         To run emacs-coverage, you need access to the Emacs C source code.\n\
         Options:\n\
        \  1. Install Emacs from source (not a package manager)\n\
        \  2. Use --emacs-source /path/to/emacs to specify the source directory"
        msg
  | InvalidPath msg ->
      Printf.sprintf
        "Error: %s\n\n\
         The Emacs source directory should contain src/*.c files.\n\
         Example: --emacs-source /path/to/emacs-31.0.50"
        msg
