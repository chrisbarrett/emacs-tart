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

(** Check if a directory contains *.c files. *)
let has_c_files (dir : string) : bool =
  if Sys.file_exists dir && Sys.is_directory dir then
    let entries = Sys.readdir dir |> Array.to_list in
    List.exists (fun f -> Filename.check_suffix f ".c") entries
  else false

(** Check if a directory looks like valid Emacs source.

    A valid source directory either:
    - Contains src/*.c files (traditional layout)
    - Contains *.c files directly (e.g., find-function-C-source-directory) *)
let is_valid_emacs_source (path : string) : bool =
  has_c_files (Filename.concat path "src") || has_c_files path

(** {1 Auto-Detection} *)

(** Query Emacs for source directory and version.

    Runs:
    {v
    emacs --batch --eval '(princ (prin1-to-string (list :source-directory find-function-C-source-directory :version emacs-version)))'
    v}

    Returns (source_dir option, version option). *)
let query_emacs () : (string option * string option) option =
  let cmd =
    "emacs --batch --eval '(princ (prin1-to-string (list :source-directory \
     find-function-C-source-directory :version emacs-version)))'"
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
          try
            let _ = Str.search_forward source_re output 0 in
            try Some (Str.matched_group 2 output) with Not_found -> None
          with Not_found -> None
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

(** Normalize a path that might be the src/ directory itself.

    If the path contains *.c files directly and ends with /src, return the
    parent directory. Otherwise return the path unchanged. *)
let normalize_source_path (path : string) : string =
  if has_c_files path then
    let basename = Filename.basename path in
    if basename = "src" then Filename.dirname path else path
  else path

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
      let normalized = normalize_source_path path in
      if is_valid_emacs_source normalized then
        let version = Option.value ~default:"unknown" version_opt in
        Found { source_dir = normalized; version }
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
