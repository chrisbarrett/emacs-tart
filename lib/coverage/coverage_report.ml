(** Coverage report generation.

    This module compares definitions from .el files against signatures
    in .tart files to calculate coverage.

    @see Spec 28, R10-R13 for requirements. *)

(** {1 Types} *)

type coverage_status =
  | Covered
  | Uncovered  (** Whether a definition has a matching signature. *)

type coverage_item = {
  definition : Definition_extractor.definition;
  source_file : string;  (** Path to the .el file *)
  status : coverage_status;
}
(** A single definition with its coverage status. *)

type coverage_result = { items : coverage_item list; files_scanned : int }
(** Results from coverage analysis. *)

type coverage_summary = {
  total_public : int;
  covered_public : int;
  total_private : int;
}
(** Summary statistics for coverage. *)

(** {1 Signature Matching} *)

(** Check if a definition name exists in a type environment. *)
let has_signature (env : Core.Type_env.t) (name : string) : bool =
  match Core.Type_env.lookup name env with Some _ -> true | None -> false

(** Get the sibling .tart path for a .el file. *)
let sibling_tart_path (el_path : string) : string =
  let base = Filename.remove_extension el_path in
  base ^ ".tart"

(** Load signatures from a sibling .tart file if it exists. *)
let load_sibling_signatures (el_path : string) (env : Core.Type_env.t) :
    Core.Type_env.t =
  let tart_path = sibling_tart_path el_path in
  if Sys.file_exists tart_path then
    match Sig.Search_path.parse_signature_file tart_path with
    | Some sig_ast -> Sig.Sig_loader.load_signature env sig_ast
    | None -> env
  else env

(** {1 Coverage Calculation} *)

(** Calculate coverage for a single file. *)
let analyze_file ~(search_path : Sig.Search_path.t) (el_path : string) :
    coverage_item list =
  match Definition_extractor.extract_from_file el_path with
  | None -> []
  | Some result ->
      (* Start with an empty environment and load signatures *)
      let base_env = Core.Type_env.empty in
      (* Load c-core signatures *)
      let env = Sig.Search_path.load_c_core ~search_path base_env in
      (* Load sibling .tart file if present *)
      let env = load_sibling_signatures el_path env in
      (* Check each definition against the environment *)
      List.map
        (fun def ->
          let status =
            if has_signature env def.Definition_extractor.name then Covered
            else Uncovered
          in
          { definition = def; source_file = el_path; status })
        result.definitions

(** Calculate coverage for multiple files. *)
let analyze_files ~(search_path : Sig.Search_path.t) (files : string list) :
    coverage_result =
  let items = List.concat_map (analyze_file ~search_path) files in
  { items; files_scanned = List.length files }

(** {1 Summary Statistics} *)

(** Compute summary statistics from coverage results. *)
let summarize (result : coverage_result) : coverage_summary =
  let public_items =
    List.filter (fun item -> not item.definition.is_private) result.items
  in
  let private_items =
    List.filter (fun item -> item.definition.is_private) result.items
  in
  let covered = List.filter (fun item -> item.status = Covered) public_items in
  {
    total_public = List.length public_items;
    covered_public = List.length covered;
    total_private = List.length private_items;
  }

(** Calculate coverage percentage (0.0 to 100.0). *)
let coverage_percentage (summary : coverage_summary) : float =
  if summary.total_public = 0 then 100.0
  else
    float_of_int summary.covered_public
    /. float_of_int summary.total_public
    *. 100.0

(** {1 Filtering} *)

(** Get all uncovered public definitions. *)
let uncovered_public (result : coverage_result) : coverage_item list =
  List.filter
    (fun item -> (not item.definition.is_private) && item.status = Uncovered)
    result.items

(** Get all private definitions. *)
let private_definitions (result : coverage_result) : coverage_item list =
  List.filter (fun item -> item.definition.is_private) result.items

(** Get all covered public definitions. *)
let covered_public (result : coverage_result) : coverage_item list =
  List.filter
    (fun item -> (not item.definition.is_private) && item.status = Covered)
    result.items
