(** Parse Package-Requires from Emacs Lisp file headers. *)

let max_header_lines = 50

let extract_requires_value line =
  (* Match: ;; Package-Requires: VALUE or ;;; Package-Requires: VALUE *)
  let line = String.trim line in
  let rec skip_semicolons i =
    if i < String.length line && line.[i] = ';' then skip_semicolons (i + 1)
    else i
  in
  let i = skip_semicolons 0 in
  if i = 0 then None
  else
    let rest = String.trim (String.sub line i (String.length line - i)) in
    let prefix = "Package-Requires:" in
    let prefix_ci =
      String.lowercase_ascii
        (String.sub rest 0 (min (String.length prefix) (String.length rest)))
    in
    if prefix_ci = String.lowercase_ascii prefix then
      let value_start = String.length prefix in
      Some
        (String.trim
           (String.sub rest value_start (String.length rest - value_start)))
    else None

let extract_emacs_version_from_sexp s =
  (* Look for (emacs "X.Y") inside the requires sexp.
     We don't need a full sexp parser — just find emacs followed by a quoted version. *)
  let s = String.lowercase_ascii s in
  let pat = "emacs" in
  let pat_len = String.length pat in
  let s_len = String.length s in
  let rec find_emacs i =
    if i + pat_len > s_len then None
    else if String.sub s i pat_len = pat then find_quoted_version (i + pat_len)
    else find_emacs (i + 1)
  and find_quoted_version i =
    if i >= s_len then None
    else
      match s.[i] with
      | '"' -> extract_up_to_quote (i + 1) (i + 1)
      | ' ' | '\t' | '\n' | '\r' -> find_quoted_version (i + 1)
      | _ -> None
  and extract_up_to_quote start i =
    if i >= s_len then None
    else if s.[i] = '"' then
      let version_str = String.sub s start (i - start) in
      Emacs_version.parse_version version_str
    else extract_up_to_quote start (i + 1)
  in
  find_emacs 0

let parse_package_requires content =
  let lines = String.split_on_char '\n' content in
  let rec scan lines n =
    if n >= max_header_lines then None
    else
      match lines with
      | [] -> None
      | line :: rest -> (
          match extract_requires_value line with
          | Some value -> extract_emacs_version_from_sexp value
          | None -> scan rest (n + 1))
  in
  scan lines 0

let find_package_version path =
  match In_channel.with_open_text path In_channel.input_all with
  | content -> parse_package_requires content
  | exception Sys_error _ -> None
