(** Emacs coverage analysis for C and Elisp layers.

    Compares C definitions (DEFUNs, DEFVARs, DEFSYMs) and Elisp definitions
    (defun, defvar, etc.) scanned from Emacs source against loaded typings to
    determine type coverage.

    See Spec 29, R8 and Spec 95 for requirements. *)

(** {1 Types} *)

type coverage_status =
  | Covered
  | Uncovered  (** Whether a C definition has a matching type signature. *)

type c_coverage_item = {
  definition : C_scanner.c_definition;
  status : coverage_status;
}
(** A single C definition with its coverage status. *)

type c_coverage_result = {
  items : c_coverage_item list;
  source_dir : string;
  emacs_version : string;
  files_scanned : int;
}
(** Results from C layer coverage analysis. *)

type c_coverage_summary = {
  total_public : int;
  covered_public : int;
  uncovered_public : int;
  total_private : int;
}
(** Summary statistics for C layer coverage. *)

(** {1 Typings Loading} *)

(** Build a type environment from versioned typings.

    [R8] and [R12]: Load typings for the specified Emacs version using the
    version fallback chain. *)
let load_typings ~(typings_root : string) ~(version : Sig.Emacs_version.version)
    : Core.Type_env.t =
  let search_path =
    Sig.Search_path.empty
    |> Sig.Search_path.with_typings_root typings_root
    |> Sig.Search_path.with_emacs_version version
  in
  Sig.Search_path.load_c_core ~search_path Core.Type_env.empty

(** {1 Coverage Calculation} *)

(** Check if a name exists in the type environment. *)
let has_signature (env : Core.Type_env.t) (name : string) : bool =
  match Core.Type_env.lookup name env with Some _ -> true | None -> false

(** Calculate coverage for C definitions.

    [R8]: Compare scanned symbols against loaded typings. A symbol is "covered"
    if it has a type signature in the typings.

    @param source_dir Path to the Emacs source directory
    @param emacs_version Version string for the report header
    @param typings_root Root directory for versioned typings
    @param version Emacs version for typings lookup
    @param definitions List of C definitions from the scanner *)
let calculate_coverage ~(source_dir : string) ~(emacs_version : string)
    ~(typings_root : string) ~(version : Sig.Emacs_version.version)
    (definitions : C_scanner.c_definition list) : c_coverage_result =
  (* Load typings for this version *)
  let env = load_typings ~typings_root ~version in

  (* Count unique source files *)
  let files_scanned =
    definitions
    |> List.map (fun d -> d.C_scanner.file)
    |> List.sort_uniq String.compare
    |> List.length
  in

  (* Check each definition against the environment *)
  let items =
    List.map
      (fun def ->
        let status =
          if has_signature env def.C_scanner.name then Covered else Uncovered
        in
        { definition = def; status })
      definitions
  in
  { items; source_dir; emacs_version; files_scanned }

(** {1 Summary Statistics} *)

(** Compute summary statistics from coverage results. *)
let summarize (result : c_coverage_result) : c_coverage_summary =
  let public, private_ =
    C_scanner.partition_public_private
      (List.map (fun item -> item.definition) result.items)
  in
  let public_items =
    List.filter
      (fun item -> not (C_scanner.is_private item.definition.name))
      result.items
  in
  let covered =
    List.filter (fun item -> item.status = Covered) public_items |> List.length
  in
  let total_public = List.length public in
  {
    total_public;
    covered_public = covered;
    uncovered_public = total_public - covered;
    total_private = List.length private_;
  }

(** Calculate coverage percentage (0.0 to 100.0). *)
let coverage_percentage (summary : c_coverage_summary) : float =
  if summary.total_public = 0 then 100.0
  else
    float_of_int summary.covered_public
    /. float_of_int summary.total_public
    *. 100.0

(** {1 Filtering} *)

(** Get all uncovered public definitions, sorted alphabetically. *)
let uncovered_public (result : c_coverage_result) : c_coverage_item list =
  result.items
  |> List.filter (fun item ->
      (not (C_scanner.is_private item.definition.name))
      && item.status = Uncovered)
  |> List.sort (fun a b ->
      String.compare a.definition.C_scanner.name b.definition.C_scanner.name)

(** Get all covered public definitions, sorted alphabetically. *)
let covered_public (result : c_coverage_result) : c_coverage_item list =
  result.items
  |> List.filter (fun item ->
      (not (C_scanner.is_private item.definition.name)) && item.status = Covered)
  |> List.sort (fun a b ->
      String.compare a.definition.C_scanner.name b.definition.C_scanner.name)

(** Get all private definitions, sorted alphabetically. *)
let private_definitions (result : c_coverage_result) : c_coverage_item list =
  result.items
  |> List.filter (fun item -> C_scanner.is_private item.definition.name)
  |> List.sort (fun a b ->
      String.compare a.definition.C_scanner.name b.definition.C_scanner.name)

(** {1 Elisp Layer Types} *)

type elisp_coverage_item = {
  definition : Definition_extractor.definition;
  status : coverage_status;
}
(** A single Elisp definition with its coverage status. *)

type elisp_file_result = { filename : string; items : elisp_coverage_item list }
(** Coverage results for a single Elisp file. *)

type elisp_coverage_result = {
  file_results : elisp_file_result list;
  source_dir : string;
  emacs_version : string;
}
(** Results from Elisp layer coverage analysis. *)

type elisp_coverage_summary = {
  elisp_total_public : int;
  elisp_covered_public : int;
  elisp_uncovered_public : int;
  elisp_total_private : int;
}
(** Summary statistics for Elisp layer coverage. *)

(** {1 Elisp Typings Loading} *)

(** Build a type environment from versioned lisp-core typings. *)
let load_lisp_typings ~(typings_root : string)
    ~(version : Sig.Emacs_version.version) : Core.Type_env.t =
  let search_path =
    Sig.Search_path.empty
    |> Sig.Search_path.with_typings_root typings_root
    |> Sig.Search_path.with_emacs_version version
  in
  Sig.Search_path.load_lisp_core ~search_path Core.Type_env.empty

(** {1 Elisp Coverage Calculation} *)

(** Scan all .el files under the lisp/ subdirectory of [source_dir]. *)
let scan_elisp_files (source_dir : string) :
    Definition_extractor.extraction_result list =
  let lisp_dir = Filename.concat source_dir "lisp" in
  if not (Sys.file_exists lisp_dir && Sys.is_directory lisp_dir) then []
  else
    let files = Sys.readdir lisp_dir in
    files |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".el")
    |> List.sort String.compare
    |> List.filter_map (fun f ->
        let path = Filename.concat lisp_dir f in
        Definition_extractor.extract_from_file path)

(** Calculate coverage for Elisp definitions.

    [Spec 95 R1-R4]: Scan lisp/*.el files and compare definitions against
    lisp-core typings. Files without matching typings show 0/N public coverage.

    @param source_dir Path to the Emacs source directory
    @param emacs_version Version string for the report header
    @param typings_root Root directory for versioned typings
    @param version Emacs version for typings lookup *)
let calculate_elisp_coverage ~(source_dir : string) ~(emacs_version : string)
    ~(typings_root : string) ~(version : Sig.Emacs_version.version) :
    elisp_coverage_result =
  let env = load_lisp_typings ~typings_root ~version in
  let extraction_results = scan_elisp_files source_dir in
  let file_results =
    List.map
      (fun (result : Definition_extractor.extraction_result) ->
        let items =
          List.map
            (fun (def : Definition_extractor.definition) ->
              let status =
                if has_signature env def.name then Covered else Uncovered
              in
              { definition = def; status })
            result.definitions
        in
        { filename = result.filename; items })
      extraction_results
  in
  { file_results; source_dir; emacs_version }

(** {1 Elisp Summary Statistics} *)

(** Compute summary statistics from Elisp coverage results. *)
let elisp_summarize (result : elisp_coverage_result) : elisp_coverage_summary =
  let all_items = List.concat_map (fun fr -> fr.items) result.file_results in
  let public_items =
    List.filter (fun item -> not item.definition.is_private) all_items
  in
  let private_items =
    List.filter (fun item -> item.definition.is_private) all_items
  in
  let covered =
    List.filter (fun item -> item.status = Covered) public_items |> List.length
  in
  let total_public = List.length public_items in
  {
    elisp_total_public = total_public;
    elisp_covered_public = covered;
    elisp_uncovered_public = total_public - covered;
    elisp_total_private = List.length private_items;
  }

(** Calculate Elisp coverage percentage (0.0 to 100.0). *)
let elisp_coverage_percentage (summary : elisp_coverage_summary) : float =
  if summary.elisp_total_public = 0 then 100.0
  else
    float_of_int summary.elisp_covered_public
    /. float_of_int summary.elisp_total_public
    *. 100.0

(** {1 Elisp Filtering} *)

(** Get all uncovered public Elisp definitions, sorted alphabetically. *)
let elisp_uncovered_public (result : elisp_coverage_result) :
    elisp_coverage_item list =
  result.file_results
  |> List.concat_map (fun fr -> fr.items)
  |> List.filter (fun item ->
      (not item.definition.is_private) && item.status = Uncovered)
  |> List.sort (fun a b -> String.compare a.definition.name b.definition.name)

(** Get all covered public Elisp definitions, sorted alphabetically. *)
let elisp_covered_public (result : elisp_coverage_result) :
    elisp_coverage_item list =
  result.file_results
  |> List.concat_map (fun fr -> fr.items)
  |> List.filter (fun item ->
      (not item.definition.is_private) && item.status = Covered)
  |> List.sort (fun a b -> String.compare a.definition.name b.definition.name)

(** Get all private Elisp definitions, sorted alphabetically. *)
let elisp_private_definitions (result : elisp_coverage_result) :
    elisp_coverage_item list =
  result.file_results
  |> List.concat_map (fun fr -> fr.items)
  |> List.filter (fun item -> item.definition.is_private)
  |> List.sort (fun a b -> String.compare a.definition.name b.definition.name)

(** {1 Combined Summary} *)

type combined_summary = {
  c_summary : c_coverage_summary;
  elisp_summary : elisp_coverage_summary;
  total_public : int;
  total_covered : int;
  total_uncovered : int;
  total_private : int;
}
(** Aggregate summary across C and Elisp layers. *)

(** Combine C and Elisp coverage summaries. *)
let combine_summaries (c : c_coverage_summary) (e : elisp_coverage_summary) :
    combined_summary =
  {
    c_summary = c;
    elisp_summary = e;
    total_public = c.total_public + e.elisp_total_public;
    total_covered = c.covered_public + e.elisp_covered_public;
    total_uncovered = c.uncovered_public + e.elisp_uncovered_public;
    total_private = c.total_private + e.elisp_total_private;
  }

(** Calculate combined coverage percentage (0.0 to 100.0). *)
let combined_coverage_percentage (summary : combined_summary) : float =
  if summary.total_public = 0 then 100.0
  else
    float_of_int summary.total_covered
    /. float_of_int summary.total_public
    *. 100.0
