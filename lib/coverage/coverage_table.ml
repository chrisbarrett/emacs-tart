(** Per-file coverage table renderer.

    Renders per-file coverage data as aligned text tables or JSON. Shared by
    [emacs-coverage] and [coverage] subcommands.

    See Spec 96 for requirements. *)

(** {1 Types} *)

type file_row = {
  filename : string;
  private_count : int;
  public_covered : int;
  public_total : int;
  coverage_pct : float;
  uncovered_names : string list;
}

type color_mode = Auto | Always | Off
type output_format = Human | Json
type table_config = { color : color_mode; format : output_format }

let default_config : table_config = { color = Auto; format = Human }

(** {1 Row Construction} *)

(** Build table rows from C layer coverage results, one row per source file. *)
let rows_of_c_result (result : Emacs_coverage.c_coverage_result) : file_row list
    =
  let tbl : (string, Emacs_coverage.c_coverage_item list) Hashtbl.t =
    Hashtbl.create 16
  in
  List.iter
    (fun (item : Emacs_coverage.c_coverage_item) ->
      let file = item.definition.C_scanner.file in
      let prev =
        match Hashtbl.find_opt tbl file with Some l -> l | None -> []
      in
      Hashtbl.replace tbl file (item :: prev))
    result.items;
  Hashtbl.fold
    (fun filename items acc ->
      let private_count =
        List.length
          (List.filter
             (fun (i : Emacs_coverage.c_coverage_item) ->
               C_scanner.is_private i.definition.name)
             items)
      in
      let public_items =
        List.filter
          (fun (i : Emacs_coverage.c_coverage_item) ->
            not (C_scanner.is_private i.definition.name))
          items
      in
      let public_total = List.length public_items in
      let public_covered =
        List.length
          (List.filter
             (fun (i : Emacs_coverage.c_coverage_item) ->
               i.status = Emacs_coverage.Covered)
             public_items)
      in
      let coverage_pct =
        if public_total = 0 then 100.0
        else float_of_int public_covered /. float_of_int public_total *. 100.0
      in
      let uncovered_names =
        public_items
        |> List.filter (fun (i : Emacs_coverage.c_coverage_item) ->
            i.status = Emacs_coverage.Uncovered)
        |> List.map (fun (i : Emacs_coverage.c_coverage_item) ->
            i.definition.name)
        |> List.sort String.compare
      in
      {
        filename;
        private_count;
        public_covered;
        public_total;
        coverage_pct;
        uncovered_names;
      }
      :: acc)
    tbl []

(** Build table rows from Elisp layer coverage results. *)
let rows_of_elisp_result (result : Emacs_coverage.elisp_coverage_result) :
    file_row list =
  List.map
    (fun (fr : Emacs_coverage.elisp_file_result) ->
      let filename = Filename.basename fr.filename in
      let private_count =
        List.length
          (List.filter
             (fun (i : Emacs_coverage.elisp_coverage_item) ->
               i.definition.is_private)
             fr.items)
      in
      let public_items =
        List.filter
          (fun (i : Emacs_coverage.elisp_coverage_item) ->
            not i.definition.is_private)
          fr.items
      in
      let public_total = List.length public_items in
      let public_covered =
        List.length
          (List.filter
             (fun (i : Emacs_coverage.elisp_coverage_item) ->
               i.status = Emacs_coverage.Covered)
             public_items)
      in
      let coverage_pct =
        if public_total = 0 then 100.0
        else float_of_int public_covered /. float_of_int public_total *. 100.0
      in
      let uncovered_names =
        public_items
        |> List.filter (fun (i : Emacs_coverage.elisp_coverage_item) ->
            i.status = Emacs_coverage.Uncovered)
        |> List.map (fun (i : Emacs_coverage.elisp_coverage_item) ->
            i.definition.name)
        |> List.sort String.compare
      in
      {
        filename;
        private_count;
        public_covered;
        public_total;
        coverage_pct;
        uncovered_names;
      })
    result.file_results

(** {1 Sorting} *)

(** File extension for sorting: [.c] sorts before [.el]. *)
let sort_key (row : file_row) : int * string =
  if Filename.check_suffix row.filename ".c" then (0, row.filename)
  else (1, row.filename)

(** Sort rows: [.c] first, then [.el]; alphabetical within each group. *)
let default_sort (rows : file_row list) : file_row list =
  List.sort
    (fun a b ->
      let ka = sort_key a in
      let kb = sort_key b in
      compare ka kb)
    rows

(** {1 Color} *)

(** ANSI escape for the given coverage percentage. *)
let color_code (pct : float) : string =
  if pct >= 95.0 then "\027[32m"
  else if pct >= 50.0 then "\027[33m"
  else "\027[31m"

let reset_code : string = "\027[0m"

(** Wrap text in ANSI color escapes when enabled. *)
let colorize ~(use_color : bool) (pct : float) (s : string) : string =
  if use_color then color_code pct ^ s ^ reset_code else s

(** Determine whether to use color based on config. *)
let should_color (config : table_config) : bool =
  match config.color with
  | Always -> true
  | Off -> false
  | Auto -> Unix.isatty Unix.stdout

(** {1 Human-Readable Rendering} *)

(** Render the aligned text table. *)
let render_human ~(use_color : bool) (rows : file_row list) : string =
  let filename_hdr = "FILENAME" in
  let private_hdr = "PRIVATE" in
  let public_hdr = "PUBLIC" in
  let coverage_hdr = "COVERAGE" in
  (* Pre-format each row's values *)
  let formatted =
    List.map
      (fun r ->
        let pub_s = Printf.sprintf "%d/%d" r.public_covered r.public_total in
        let priv_s = string_of_int r.private_count in
        let cov_s = Printf.sprintf "%.1f%%" r.coverage_pct in
        (r, pub_s, priv_s, cov_s))
      rows
  in
  (* Column widths *)
  let filename_w =
    List.fold_left
      (fun acc (r, _, _, _) -> max acc (String.length r.filename))
      (String.length filename_hdr)
      formatted
  in
  let private_w =
    List.fold_left
      (fun acc (_, _, s, _) -> max acc (String.length s))
      (String.length private_hdr)
      formatted
  in
  let public_w =
    List.fold_left
      (fun acc (_, s, _, _) -> max acc (String.length s))
      (String.length public_hdr) formatted
  in
  let coverage_w =
    List.fold_left
      (fun acc (_, _, _, s) -> max acc (String.length s))
      (String.length coverage_hdr)
      formatted
  in
  let buf = Buffer.create 256 in
  (* Header line *)
  Buffer.add_string buf
    (Printf.sprintf "%-*s  %*s  %*s  %*s" filename_w filename_hdr private_w
       private_hdr public_w public_hdr coverage_w coverage_hdr);
  (* Data rows *)
  List.iter
    (fun (r, pub_s, priv_s, cov_s) ->
      Buffer.add_char buf '\n';
      let pub_padded =
        colorize ~use_color r.coverage_pct (Printf.sprintf "%*s" public_w pub_s)
      in
      let cov_padded =
        colorize ~use_color r.coverage_pct
          (Printf.sprintf "%*s" coverage_w cov_s)
      in
      Buffer.add_string buf
        (Printf.sprintf "%-*s  %*s  %s  %s" filename_w r.filename private_w
           priv_s pub_padded cov_padded))
    formatted;
  Buffer.contents buf

(** {1 JSON Rendering} *)

(** Escape a string for JSON output. *)
let json_escape (s : string) : string =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | _ -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(** Render the JSON structure per Spec 96 R5. *)
let render_json ~(emacs_version : string) (rows : file_row list) : string =
  let buf = Buffer.create 512 in
  Buffer.add_string buf "{\n";
  Buffer.add_string buf
    (Printf.sprintf "  \"emacs_version\": \"%s\",\n"
       (json_escape emacs_version));
  Buffer.add_string buf "  \"files\": [\n";
  let n = List.length rows in
  List.iteri
    (fun i row ->
      let uncovered_json =
        "["
        ^ String.concat ", "
            (List.map
               (fun name -> "\"" ^ json_escape name ^ "\"")
               row.uncovered_names)
        ^ "]"
      in
      Buffer.add_string buf
        (Printf.sprintf
           "    {\n\
           \      \"filename\": \"%s\",\n\
           \      \"private\": %d,\n\
           \      \"public_covered\": %d,\n\
           \      \"public_total\": %d,\n\
           \      \"coverage_pct\": %.1f,\n\
           \      \"uncovered\": %s\n\
           \    }"
           (json_escape row.filename) row.private_count row.public_covered
           row.public_total row.coverage_pct uncovered_json);
      if i < n - 1 then Buffer.add_string buf ",\n"
      else Buffer.add_char buf '\n')
    rows;
  Buffer.add_string buf "  ],\n";
  let total_private =
    List.fold_left (fun acc r -> acc + r.private_count) 0 rows
  in
  let total_covered =
    List.fold_left (fun acc r -> acc + r.public_covered) 0 rows
  in
  let total_public =
    List.fold_left (fun acc r -> acc + r.public_total) 0 rows
  in
  let total_pct =
    if total_public = 0 then 100.0
    else float_of_int total_covered /. float_of_int total_public *. 100.0
  in
  Buffer.add_string buf
    (Printf.sprintf
       "  \"totals\": {\n\
       \    \"private\": %d,\n\
       \    \"public_covered\": %d,\n\
       \    \"public_total\": %d,\n\
       \    \"coverage_pct\": %.1f\n\
       \  }\n"
       total_private total_covered total_public total_pct);
  Buffer.add_string buf "}";
  Buffer.contents buf

(** {1 Table Rendering} *)

(** Render a coverage table in the configured format. *)
let render_table ~(config : table_config) (rows : file_row list) : string =
  match config.format with
  | Human ->
      let use_color = should_color config in
      render_human ~use_color rows
  | Json -> render_json ~emacs_version:"unknown" rows
