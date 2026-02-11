(** Emacs source directory discovery and version resolution.

    Discovers the Emacs C source directory for scanning DEFUNs and DEFVARs.
    Supports auto-detection via running Emacs, or explicit path override.
    Resolves version specifiers to concrete Emacs refs.

    See Spec 29, R1-R3 for requirements; Spec 99 for version resolution. *)

(** {1 Types} *)

type discovery_result =
  | Found of { source_dir : string; version : string }
  | NotFound of string
  | InvalidPath of string

type resolved_version =
  | Release of { tag : string; version : Sig.Emacs_version.version }
  | Dev
  | Commit of string

type resolution_error = No_matching_tag of string | Git_error of string

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

(** {1 Version Resolution} *)

let emacs_repo_url = "https://git.savannah.gnu.org/git/emacs.git"

(** Check if a string is a hex git SHA (7+ hex chars). *)
let is_sha s =
  let len = String.length s in
  len >= 7 && len <= 40
  && String.to_seq s
     |> Seq.for_all (fun c ->
         (c >= '0' && c <= '9')
         || (c >= 'a' && c <= 'f')
         || (c >= 'A' && c <= 'F'))

(** Check if a string is a development identifier. *)
let is_dev_identifier s =
  match String.lowercase_ascii s with
  | "dev" | "devel" | "git" -> true
  | _ -> false

(** Parse a tag like "emacs-29.1" or "emacs-29.1.2" into a version. *)
let parse_emacs_tag (tag : string) : Sig.Emacs_version.version option =
  let prefix = "emacs-" in
  let prefix_len = String.length prefix in
  if String.length tag > prefix_len && String.sub tag 0 prefix_len = prefix then
    let version_str =
      String.sub tag prefix_len (String.length tag - prefix_len)
    in
    Sig.Emacs_version.parse_version version_str
  else None

(** Fetch remote tags from the Emacs git repository.

    Runs [git ls-remote --tags] and parses the output into (tag, version) pairs.
    Only includes release tags (not pre-release or deref markers).

    @param fetch_tags
      Optional override for testing. When provided, this function is called
      instead of running git. *)
let list_remote_tags ?(url = emacs_repo_url) ?fetch_tags () :
    (string list, resolution_error) result =
  let raw_output =
    match fetch_tags with
    | Some f -> f ()
    | None -> (
        let cmd =
          Printf.sprintf "git ls-remote --tags %s 2>&1" (Filename.quote url)
        in
        try
          let ic = Unix.open_process_in cmd in
          let buf = Buffer.create 8192 in
          (try
             while true do
               Buffer.add_channel buf ic 1
             done
           with End_of_file -> ());
          let output = Buffer.contents buf in
          let status = Unix.close_process_in ic in
          match status with
          | Unix.WEXITED 0 -> Ok output
          | Unix.WEXITED code ->
              Error
                (Git_error
                   (Printf.sprintf "git ls-remote exited %d: %s" code
                      (String.trim output)))
          | Unix.WSIGNALED n ->
              Error (Git_error (Printf.sprintf "git killed by signal %d" n))
          | Unix.WSTOPPED n ->
              Error (Git_error (Printf.sprintf "git stopped by signal %d" n))
        with
        | Unix.Unix_error (err, _, _) ->
            Error
              (Git_error
                 (Printf.sprintf "exec failed: %s" (Unix.error_message err)))
        | exn ->
            Error
              (Git_error
                 (Printf.sprintf "unexpected error: %s" (Printexc.to_string exn)))
        )
  in
  match raw_output with
  | Error _ as e -> e
  | Ok output ->
      let lines = String.split_on_char '\n' output in
      let tags =
        List.filter_map
          (fun line ->
            let line = String.trim line in
            if String.length line = 0 then None
            else
              (* Each line: "<sha>\trefs/tags/<tag>" *)
              match String.split_on_char '\t' line with
              | [ _sha; refname ] ->
                  let prefix = "refs/tags/" in
                  let prefix_len = String.length prefix in
                  if
                    String.length refname >= prefix_len
                    && String.sub refname 0 prefix_len = prefix
                  then
                    let tag =
                      String.sub refname prefix_len
                        (String.length refname - prefix_len)
                    in
                    (* Skip dereferenced tags (^{}) and pre-release tags *)
                    if
                      String.length tag > 0
                      && (not (Filename.check_suffix tag "^{}"))
                      && Option.is_some (parse_emacs_tag tag)
                    then
                      (* Also skip pre-release tags like emacs-29.1-rc1 *)
                      let version_part =
                        String.sub tag 6 (String.length tag - 6)
                      in
                      let has_dash = String.contains version_part '-' in
                      if has_dash then None else Some tag
                    else None
                  else None
              | _ -> None)
          lines
      in
      Ok tags

(** Sort parsed tags by version, most recent first. *)
let sort_tags_descending (tags : string list) :
    (string * Sig.Emacs_version.version) list =
  let parsed =
    List.filter_map
      (fun tag ->
        match parse_emacs_tag tag with Some v -> Some (tag, v) | None -> None)
      tags
  in
  List.sort (fun (_, a) (_, b) -> Sig.Emacs_version.compare_version b a) parsed

(** Format the 3 most recent tags for error messages. *)
let format_recent_tags (tags : string list) : string =
  let sorted = sort_tags_descending tags in
  let recent = List.filteri (fun i _ -> i < 3) sorted in
  String.concat ", " (List.map fst recent)

(** Resolve a version specifier to a concrete Emacs ref.

    @param fetch_tags Optional override for tag listing (for testing).
    @param specifier The version string to resolve. *)
let resolve_version ?fetch_tags (specifier : string) :
    (resolved_version, resolution_error) result =
  let specifier = String.trim specifier in

  (* R3: Development identifiers *)
  if is_dev_identifier specifier then Ok Dev (* R4: Arbitrary git SHAs *)
  else if is_sha specifier then Ok (Commit specifier)
  else
    (* Need to consult remote tags for R1, R2, R5 *)
    match list_remote_tags ?fetch_tags () with
    | Error _ as e -> e
    | Ok tags -> (
        let sorted = sort_tags_descending tags in

        (* R2: latest identifier *)
        if String.lowercase_ascii specifier = "latest" then
          match sorted with
          | (tag, version) :: _ -> Ok (Release { tag; version })
          | [] -> Error (No_matching_tag "No release tags found")
        else
          (* R1: Semver shorthand *)
          match String.split_on_char '.' specifier with
          | [ major_s ] -> (
              (* Major-only: "29" → latest 29.x.y *)
              match int_of_string_opt major_s with
              | None ->
                  Error
                    (No_matching_tag
                       (Printf.sprintf
                          "Invalid version specifier: %s\nRecent tags: %s"
                          specifier (format_recent_tags tags)))
              | Some major -> (
                  let matching =
                    List.filter
                      (fun (_, v) -> v.Sig.Emacs_version.major = major)
                      sorted
                  in
                  match matching with
                  | (tag, version) :: _ -> Ok (Release { tag; version })
                  | [] ->
                      Error
                        (No_matching_tag
                           (Printf.sprintf
                              "No release tags found for Emacs %d\n\
                               Recent tags: %s"
                              major (format_recent_tags tags)))))
          | [ major_s; minor_s ] -> (
              (* Major.minor: "29.1" → exact tag emacs-29.1 *)
              match (int_of_string_opt major_s, int_of_string_opt minor_s) with
              | Some major, Some minor ->
                  let target_tag = Printf.sprintf "emacs-%d.%d" major minor in
                  if List.exists (fun t -> t = target_tag) tags then
                    Ok
                      (Release
                         {
                           tag = target_tag;
                           version = { major; minor; patch = None };
                         })
                  else
                    Error
                      (No_matching_tag
                         (Printf.sprintf "Tag %s not found\nRecent tags: %s"
                            target_tag (format_recent_tags tags)))
              | _ ->
                  Error
                    (No_matching_tag
                       (Printf.sprintf
                          "Invalid version specifier: %s\nRecent tags: %s"
                          specifier (format_recent_tags tags))))
          | [ major_s; minor_s; patch_s ] -> (
              (* Full version: "29.1.2" → exact tag emacs-29.1.2 *)
              match
                ( int_of_string_opt major_s,
                  int_of_string_opt minor_s,
                  int_of_string_opt patch_s )
              with
              | Some major, Some minor, Some patch ->
                  let target_tag =
                    Printf.sprintf "emacs-%d.%d.%d" major minor patch
                  in
                  if List.exists (fun t -> t = target_tag) tags then
                    Ok
                      (Release
                         {
                           tag = target_tag;
                           version = { major; minor; patch = Some patch };
                         })
                  else
                    Error
                      (No_matching_tag
                         (Printf.sprintf "Tag %s not found\nRecent tags: %s"
                            target_tag (format_recent_tags tags)))
              | _ ->
                  Error
                    (No_matching_tag
                       (Printf.sprintf
                          "Invalid version specifier: %s\nRecent tags: %s"
                          specifier (format_recent_tags tags))))
          | _ ->
              Error
                (No_matching_tag
                   (Printf.sprintf
                      "Invalid version specifier: %s\nRecent tags: %s" specifier
                      (format_recent_tags tags))))

(** Format a resolution error as a user-friendly message. *)
let format_resolution_error (err : resolution_error) : string =
  match err with
  | No_matching_tag msg ->
      Printf.sprintf
        "Error: %s\n\n\
         Usage: --emacs-version VERSION\n\
         VERSION can be:\n\
        \  29       latest 29.x release\n\
        \  29.1     exact release\n\
        \  latest   most recent stable release\n\
        \  dev      development HEAD\n\
        \  <sha>    git commit (7+ hex chars)"
        msg
  | Git_error msg -> Printf.sprintf "Error: %s" msg

(** Convert a resolved version to a display string. *)
let resolved_version_to_string (rv : resolved_version) : string =
  match rv with
  | Release { tag; version } ->
      Printf.sprintf "%s (%s)" tag (Sig.Emacs_version.version_to_string version)
  | Dev -> "main (development HEAD)"
  | Commit sha -> Printf.sprintf "commit %s" sha
