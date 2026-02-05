(** C source scanner for Emacs DEFUNs, DEFVARs, and DEFSYMs.

    Parses Emacs C source files to extract Lisp-accessible definitions.
    Supports DEFUN (functions), DEFVAR_* (variables), and DEFSYM (symbols).

    See Spec 29, R4-R6 for requirements. *)

(** {1 Types} *)

type def_kind = Defun | Defvar | Defsym

type c_definition = {
  name : string;
  kind : def_kind;
  file : string;
  line : int;
}

(** {1 Regex Patterns} *)

(** Match DEFUN ("lisp-name", ... The Lisp name is the first string argument. *)
let defun_re = Str.regexp {|DEFUN[ \t\n]*([ \t\n]*"\([^"]+\)"|}

(** Match DEFVAR_LISP/DEFVAR_INT/DEFVAR_BOOL ("lisp-name", ... The Lisp name is
    the first string argument. *)
let defvar_re =
  Str.regexp {|DEFVAR_\(LISP\|INT\|BOOL\)[ \t\n]*([ \t\n]*"\([^"]+\)"|}

(** Match DEFSYM (Qfoo, "lisp-name"); The Lisp name is the second string
    argument. *)
let defsym_re =
  Str.regexp
    {|DEFSYM[ \t\n]*([ \t\n]*[A-Za-z_][A-Za-z0-9_]*[ \t\n]*,[ \t\n]*"\([^"]+\)"|}

(** {1 Line Counting} *)

(** Count newlines in a string up to a given position. *)
let count_lines_up_to (content : string) (pos : int) : int =
  let count = ref 1 in
  for i = 0 to min pos (String.length content - 1) - 1 do
    if content.[i] = '\n' then incr count
  done;
  !count

(** {1 Scanning Implementation} *)

(** Find all matches of a regex in content, returning (name, position) pairs. *)
let find_all_matches (re : Str.regexp) (group : int) (content : string) :
    (string * int) list =
  let rec find_from pos acc =
    try
      let match_start = Str.search_forward re content pos in
      let name = Str.matched_group group content in
      let next_pos = match_start + 1 in
      find_from next_pos ((name, match_start) :: acc)
    with Not_found -> List.rev acc
  in
  find_from 0 []

(** Scan a single C file for definitions. *)
let scan_file (path : string) : c_definition list =
  if not (Sys.file_exists path) then []
  else
    try
      let ic = open_in path in
      let n = in_channel_length ic in
      let content = really_input_string ic n in
      close_in ic;

      let file = Filename.basename path in

      (* Find DEFUNs - group 1 has the name *)
      let defuns =
        find_all_matches defun_re 1 content
        |> List.map (fun (name, pos) ->
               {
                 name;
                 kind = Defun;
                 file;
                 line = count_lines_up_to content pos;
               })
      in

      (* Find DEFVARs - group 2 has the name (group 1 is LISP|INT|BOOL) *)
      let defvars =
        find_all_matches defvar_re 2 content
        |> List.map (fun (name, pos) ->
               {
                 name;
                 kind = Defvar;
                 file;
                 line = count_lines_up_to content pos;
               })
      in

      (* Find DEFSYMs - group 1 has the name *)
      let defsyms =
        find_all_matches defsym_re 1 content
        |> List.map (fun (name, pos) ->
               {
                 name;
                 kind = Defsym;
                 file;
                 line = count_lines_up_to content pos;
               })
      in

      defuns @ defvars @ defsyms
    with _ -> []

(** Scan all C files in a directory. *)
let scan_dir (dir : string) : c_definition list =
  if not (Sys.file_exists dir && Sys.is_directory dir) then []
  else
    let entries = Sys.readdir dir |> Array.to_list in
    let c_files =
      entries
      |> List.filter (fun f -> Filename.check_suffix f ".c")
      |> List.map (fun f -> Filename.concat dir f)
      |> List.sort String.compare
    in
    List.concat_map scan_file c_files

(** {1 Filtering} *)

let is_private (name : string) : bool =
  try
    let _ = Str.search_forward (Str.regexp_string "--") name 0 in
    true
  with Not_found -> false

let partition_public_private (defs : c_definition list) :
    c_definition list * c_definition list =
  List.partition (fun d -> not (is_private d.name)) defs
