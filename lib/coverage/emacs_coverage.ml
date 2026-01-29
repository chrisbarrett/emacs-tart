(** Emacs C layer coverage analysis.

    Compares C definitions (DEFUNs, DEFVARs, DEFSYMs) scanned from Emacs source
    against loaded typings to determine type coverage.

    @see Spec 29, R8 for requirements. *)

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
  Sig.Search_path.load_c_core ~search_path ~env:Core.Type_env.empty

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
         (not (C_scanner.is_private item.definition.name))
         && item.status = Covered)
  |> List.sort (fun a b ->
         String.compare a.definition.C_scanner.name b.definition.C_scanner.name)

(** Get all private definitions, sorted alphabetically. *)
let private_definitions (result : c_coverage_result) : c_coverage_item list =
  result.items
  |> List.filter (fun item -> C_scanner.is_private item.definition.name)
  |> List.sort (fun a b ->
         String.compare a.definition.C_scanner.name b.definition.C_scanner.name)
