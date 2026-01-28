(** Emacs version detection.

    Detects the installed Emacs version by running `emacs --version` and parsing
    the output. This is used to load version-appropriate typings. *)

(** {1 Types} *)

type version = { major : int; minor : int; patch : int option }
(** A parsed Emacs version number *)

type detection_result =
  | Detected of version
  | NotFound
  | ParseError of string  (** Result of version detection *)

(** {1 Version String Formatting} *)

let version_to_string (v : version) : string =
  match v.patch with
  | Some p -> Printf.sprintf "%d.%d.%d" v.major v.minor p
  | None -> Printf.sprintf "%d.%d" v.major v.minor

let version_to_dir (v : version) : string =
  Printf.sprintf "%d.%d" v.major v.minor

(** {1 Version Parsing} *)

(** Parse a version string like "31.0.50" or "30.1". Returns None if the string
    doesn't match the expected format. *)
let parse_version (s : string) : version option =
  let s = String.trim s in
  try
    match String.split_on_char '.' s with
    | [ major_s; minor_s ] ->
        Some
          {
            major = int_of_string major_s;
            minor = int_of_string minor_s;
            patch = None;
          }
    | [ major_s; minor_s; patch_s ] ->
        Some
          {
            major = int_of_string major_s;
            minor = int_of_string minor_s;
            patch = Some (int_of_string patch_s);
          }
    | _ -> None
  with Failure _ -> None

(** Parse the output of `emacs --version`. Expected format: "GNU Emacs
    31.0.50\n..." *)
let parse_emacs_version_output (output : string) : version option =
  let lines = String.split_on_char '\n' output in
  match lines with
  | first_line :: _ ->
      (* Expected format: "GNU Emacs 31.0.50" or similar *)
      let prefix = "GNU Emacs " in
      let prefix_len = String.length prefix in
      if
        String.length first_line > prefix_len
        && String.sub first_line 0 prefix_len = prefix
      then
        let version_str =
          String.sub first_line prefix_len
            (String.length first_line - prefix_len)
        in
        (* Take just the version number, strip any trailing info *)
        let version_str =
          match String.index_opt version_str ' ' with
          | Some idx -> String.sub version_str 0 idx
          | None -> version_str
        in
        parse_version version_str
      else None
  | [] -> None

(** {1 Version Detection} *)

(** Run `emacs --version` and parse the output. Uses the `emacs` executable on
    PATH. *)
let detect () : detection_result =
  try
    let ic = Unix.open_process_in "emacs --version 2>/dev/null" in
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
    | Unix.WEXITED 0 -> (
        match parse_emacs_version_output output with
        | Some v -> Detected v
        | None ->
            ParseError
              (Printf.sprintf "Could not parse: %s" (String.trim output)))
    | _ -> NotFound
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> NotFound
  | _ -> NotFound

(** Detect version with a fallback. Returns the detected version, or the
    fallback if detection fails. *)
let detect_or_default ~(default : version) () : version =
  match detect () with Detected v -> v | NotFound | ParseError _ -> default

(** {1 Version Comparison} *)

let compare_version (a : version) (b : version) : int =
  match compare a.major b.major with
  | 0 -> (
      match compare a.minor b.minor with
      | 0 -> (
          match (a.patch, b.patch) with
          | Some pa, Some pb -> compare pa pb
          | Some _, None -> 1
          | None, Some _ -> -1
          | None, None -> 0)
      | n -> n)
  | n -> n

(** {1 Default Versions} *)

(** The "latest" version, used as fallback *)
let latest : version = { major = 31; minor = 0; patch = None }
