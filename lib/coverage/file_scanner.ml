(** Scan directories for Emacs Lisp files.

    This module implements directory scanning for the coverage command,
    supporting recursive directory traversal, explicit paths, and exclude
    patterns.

    See Spec 28, R1, R2, R17 for requirements. *)

(** {1 Types} *)

type scan_config = {
  exclude_patterns : string list;
      (** Glob patterns for files to exclude (e.g., "*-test.el"). *)
}
(** Configuration for file scanning. *)

(** Default configuration with no exclusions. *)
let default_config : scan_config = { exclude_patterns = [] }

(** {1 Pattern Matching} *)

(** Convert a glob pattern to a regex pattern.

    Supports:
    - * matches any characters except /
    - ? matches any single character except / *)
let glob_to_regex (pattern : string) : Str.regexp =
  let escape_char c =
    match c with
    | '.' | '^' | '$' | '+' | '{' | '}' | '[' | ']' | '|' | '(' | ')' | '\\' ->
        "\\" ^ String.make 1 c
    | _ -> String.make 1 c
  in
  let rec convert i acc =
    if i >= String.length pattern then acc
    else
      match pattern.[i] with
      | '*' -> convert (i + 1) (acc ^ "[^/]*")
      | '?' -> convert (i + 1) (acc ^ "[^/]")
      | c -> convert (i + 1) (acc ^ escape_char c)
  in
  Str.regexp (convert 0 "^" ^ "$")

(** Check if a filename matches any exclude pattern. *)
let matches_exclude ~(patterns : string list) (filename : string) : bool =
  let basename = Filename.basename filename in
  List.exists
    (fun pattern ->
      let regex = glob_to_regex pattern in
      Str.string_match regex basename 0)
    patterns

(** {1 File Discovery} *)

(** Check if a file is an Emacs Lisp source file. *)
let is_elisp_file (path : string) : bool = Filename.check_suffix path ".el"

(** List all .el files in a directory recursively. *)
let rec scan_directory ~(config : scan_config) (dir : string) : string list =
  if not (Sys.file_exists dir && Sys.is_directory dir) then []
  else
    let entries = Sys.readdir dir |> Array.to_list in
    List.concat_map
      (fun entry ->
        let path = Filename.concat dir entry in
        if Sys.is_directory path then scan_directory ~config path
        else if
          is_elisp_file path
          && not (matches_exclude ~patterns:config.exclude_patterns path)
        then [ path ]
        else [])
      entries

(** Scan a single path (file or directory).

    If path is a file, returns it if it's a .el file. If path is a directory,
    scans it recursively. *)
let scan_path ~(config : scan_config) (path : string) : string list =
  if not (Sys.file_exists path) then []
  else if Sys.is_directory path then scan_directory ~config path
  else if
    is_elisp_file path
    && not (matches_exclude ~patterns:config.exclude_patterns path)
  then [ path ]
  else []

(** Scan multiple paths.

    If no paths are given, scans the current directory. *)
let scan_paths ~(config : scan_config) (paths : string list) : string list =
  let paths = if paths = [] then [ "." ] else paths in
  List.concat_map (scan_path ~config) paths |> List.sort_uniq String.compare
