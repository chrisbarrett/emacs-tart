(** Structured file I/O error handling with suggestions.

    Provides rich error messages for file operations including:
    - File not found with similar filename suggestions
    - Permission denied with helpful hints
    - Directory detection when a file was expected
    - Missing signature file with search paths tried *)

(** File error types with structured data for formatting. *)
type t =
  | File_not_found of { path : string; suggestions : string list }
  | Permission_denied of { path : string }
  | Is_directory of { path : string }
  | Read_error of { path : string; message : string }
  | Signature_not_found of {
      module_name : string;
      search_paths : string list;
      span : Syntax.Location.span option;
    }

(** Error codes for each file error type. *)
let error_code = function
  | File_not_found _ -> "E0001"
  | Permission_denied _ -> "E0002"
  | Is_directory _ -> "E0003"
  | Signature_not_found _ -> "E0004"
  | Read_error _ -> "E0005"

(** Compute Levenshtein edit distance between two strings. *)
let levenshtein s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  if len1 = 0 then len2
  else if len2 = 0 then len1
  else
    let matrix = Array.make_matrix (len1 + 1) (len2 + 1) 0 in
    for i = 0 to len1 do
      matrix.(i).(0) <- i
    done;
    for j = 0 to len2 do
      matrix.(0).(j) <- j
    done;
    for i = 1 to len1 do
      for j = 1 to len2 do
        let cost = if s1.[i - 1] = s2.[j - 1] then 0 else 1 in
        matrix.(i).(j) <-
          min
            (min (matrix.(i - 1).(j) + 1) (matrix.(i).(j - 1) + 1))
            (matrix.(i - 1).(j - 1) + cost)
      done
    done;
    matrix.(len1).(len2)

(** Find similar filenames in a directory using Levenshtein distance.

    Returns filenames with edit distance <= max_distance, sorted by distance. *)
let find_similar_files ~max_distance dir basename =
  if not (Sys.file_exists dir && Sys.is_directory dir) then []
  else
    let entries =
      try Array.to_list (Sys.readdir dir) with Sys_error _ -> []
    in
    entries
    |> List.filter_map (fun name ->
           let dist = levenshtein basename name in
           if dist > 0 && dist <= max_distance then Some (name, dist) else None)
    |> List.sort (fun (_, d1) (_, d2) -> compare d1 d2)
    |> List.map fst

(** Check if adding .el extension would match an existing file. *)
let suggest_el_extension path =
  let with_el = path ^ ".el" in
  if Sys.file_exists with_el then Some with_el else None

(** Build suggestions for a file not found error.

    Checks for: 1. Missing .el extension 2. Similar filenames in the same
    directory *)
let build_suggestions path =
  let suggestions = [] in
  (* Check for missing .el extension *)
  let suggestions =
    match suggest_el_extension path with
    | Some suggestion -> suggestion :: suggestions
    | None -> suggestions
  in
  (* Check for similar filenames *)
  let dir = Filename.dirname path in
  let basename = Filename.basename path in
  let similar = find_similar_files ~max_distance:2 dir basename in
  let similar_with_path =
    List.map (fun name -> Filename.concat dir name) similar
  in
  suggestions @ similar_with_path

(** Attempt to open a file and classify any error.

    Returns [Ok ()] if the file exists and is readable, or [Error t] with a
    structured file error. *)
let check_file path =
  try
    if Sys.is_directory path then Error (Is_directory { path })
    else
      let ic = open_in path in
      close_in ic;
      Ok ()
  with
  | Sys_error msg when String.ends_with ~suffix:"No such file or directory" msg
    ->
      let suggestions = build_suggestions path in
      Error (File_not_found { path; suggestions })
  | Sys_error msg when String.ends_with ~suffix:"Permission denied" msg ->
      Error (Permission_denied { path })
  | Sys_error msg -> Error (Read_error { path; message = msg })
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      let suggestions = build_suggestions path in
      Error (File_not_found { path; suggestions })
  | Unix.Unix_error (Unix.EACCES, _, _) -> Error (Permission_denied { path })
  | Unix.Unix_error (Unix.EISDIR, _, _) -> Error (Is_directory { path })
  | Unix.Unix_error (err, _, _) ->
      Error (Read_error { path; message = Unix.error_message err })

(** Format the error header line with error code. *)
let format_header err =
  let code = error_code err in
  let title =
    match err with
    | File_not_found _ -> "file not found"
    | Permission_denied _ -> "permission denied"
    | Is_directory _ -> "expected file, found directory"
    | Signature_not_found _ -> "signature file not found"
    | Read_error _ -> "read error"
  in
  Printf.sprintf "error[%s]: %s" code title

(** Format a file error as a human-readable string. *)
let to_string = function
  | File_not_found { path; suggestions } ->
      let header = format_header (File_not_found { path; suggestions }) in
      let body =
        Printf.sprintf
          "  --> %s\n   |\n   | Cannot open '%s': No such file or directory"
          path path
      in
      let help =
        match suggestions with
        | [] -> ""
        | [ s ] -> Printf.sprintf "\n   |\nhelp: did you mean: %s" s
        | _ ->
            Printf.sprintf "\n   |\nhelp: did you mean one of these?\n%s"
              (String.concat "\n"
                 (List.map (fun s -> "   | - " ^ s) suggestions))
      in
      header ^ "\n" ^ body ^ help
  | Permission_denied { path } ->
      let header = format_header (Permission_denied { path }) in
      Printf.sprintf
        "%s\n\
        \  --> %s\n\
        \   |\n\
        \   | Cannot read '%s': Permission denied\n\
        \   |\n\
         help: check file permissions with: ls -la %s"
        header path path path
  | Is_directory { path } ->
      let header = format_header (Is_directory { path }) in
      Printf.sprintf
        "%s\n\
        \  --> %s\n\
        \   |\n\
        \   | '%s' is a directory, not a file\n\
        \   |\n\
         help: to check all .el files in a directory, use: tart check %s*.el"
        header path path
        (if String.ends_with ~suffix:"/" path then path else path ^ "/")
  | Signature_not_found { module_name; search_paths; span } ->
      let header =
        format_header (Signature_not_found { module_name; search_paths; span })
      in
      let location =
        match span with
        | Some s -> Typing.Diagnostic.format_span s
        | None -> module_name ^ ".tart"
      in
      let paths_note =
        if search_paths = [] then ""
        else
          Printf.sprintf "\n   |\nnote: searched for '%s.tart' in:\n%s"
            module_name
            (String.concat "\n"
               (List.map (fun p -> "   | - " ^ p) search_paths))
      in
      Printf.sprintf
        "%s\n  --> %s%s\n   |\nhelp: create a signature file at ./%s.tart"
        header location paths_note module_name
  | Read_error { path; message } ->
      let header = format_header (Read_error { path; message }) in
      Printf.sprintf
        "%s\n\
        \  --> %s\n\
        \   |\n\
        \   | Error reading '%s': %s\n\
        \   |\n\
         note: occurred while parsing file"
        header path path message

(** Serialize a file error to JSON. *)
let to_json err : Yojson.Safe.t =
  let code = error_code err in
  match err with
  | File_not_found { path; suggestions } ->
      `Assoc
        [
          ("code", `String code);
          ("kind", `String "file_not_found");
          ("severity", `String "error");
          ("path", `String path);
          ( "message",
            `String
              (Printf.sprintf "Cannot open '%s': No such file or directory" path)
          );
          ("suggestions", `List (List.map (fun s -> `String s) suggestions));
        ]
  | Permission_denied { path } ->
      `Assoc
        [
          ("code", `String code);
          ("kind", `String "permission_denied");
          ("severity", `String "error");
          ("path", `String path);
          ( "message",
            `String (Printf.sprintf "Cannot read '%s': Permission denied" path)
          );
        ]
  | Is_directory { path } ->
      `Assoc
        [
          ("code", `String code);
          ("kind", `String "is_directory");
          ("severity", `String "error");
          ("path", `String path);
          ( "message",
            `String (Printf.sprintf "'%s' is a directory, not a file" path) );
        ]
  | Signature_not_found { module_name; search_paths; span } ->
      let location =
        match span with
        | Some s ->
            `Assoc
              [
                ("file", `String s.start_pos.file);
                ("line", `Int s.start_pos.line);
                ("column", `Int (s.start_pos.col + 1));
              ]
        | None -> `Null
      in
      `Assoc
        [
          ("code", `String code);
          ("kind", `String "signature_not_found");
          ("severity", `String "error");
          ("module_name", `String module_name);
          ("search_paths", `List (List.map (fun s -> `String s) search_paths));
          ("location", location);
        ]
  | Read_error { path; message } ->
      `Assoc
        [
          ("code", `String code);
          ("kind", `String "read_error");
          ("severity", `String "error");
          ("path", `String path);
          ("message", `String message);
        ]

(** Get the path associated with a file error. *)
let path_of = function
  | File_not_found { path; _ } -> path
  | Permission_denied { path } -> path
  | Is_directory { path } -> path
  | Signature_not_found { module_name; _ } -> module_name ^ ".tart"
  | Read_error { path; _ } -> path
