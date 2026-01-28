(** Dependency extraction from parsed files.

    Extracts dependency edges from Elisp and signature files. *)

module Sexp = Syntax.Sexp
module Sig_ast = Sig.Sig_ast
module Graph = Dependency_graph

(** {1 Internal Helpers} *)

(** Deduplicate edges by (target, kind) *)
let dedup_edges (edges : Graph.edge list) : Graph.edge list =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun edge ->
      let key = (edge.Graph.target, edge.Graph.kind) in
      if Hashtbl.mem seen key then false
      else (
        Hashtbl.add seen key ();
        true))
    edges

(** {1 Extraction from .el Files} *)

(** Extract a require edge from (require 'foo) or (require 'foo "file") *)
let extract_require (sexp : Sexp.t) : Graph.edge option =
  match sexp with
  | Sexp.List (Sexp.Symbol ("require", _) :: args, _) -> (
      match args with
      (* (require 'foo) - quoted symbol *)
      | [ Sexp.List ([ Sexp.Symbol ("quote", _); Sexp.Symbol (name, _) ], _) ]
        ->
          Some { Graph.target = name; kind = Graph.Require }
      (* (require 'foo "filename") - quoted symbol with filename *)
      | Sexp.List ([ Sexp.Symbol ("quote", _); Sexp.Symbol (name, _) ], _) :: _
        ->
          Some { Graph.target = name; kind = Graph.Require }
      | _ -> None)
  | _ -> None

(** Extract an autoload edge from (autoload 'fn "module") *)
let extract_autoload (sexp : Sexp.t) : Graph.edge option =
  match sexp with
  | Sexp.List (Sexp.Symbol ("autoload", _) :: args, _) -> (
      match args with
      (* (autoload 'fn "module" ...) *)
      | Sexp.List ([ Sexp.Symbol ("quote", _); Sexp.Symbol (_, _) ], _)
        :: Sexp.String (module_name, _)
        :: _ ->
          Some { Graph.target = module_name; kind = Graph.Autoload }
      | _ -> None)
  | _ -> None

(** Recursively extract edges from a single sexp *)
let rec extract_edges_from_sexp (sexp : Sexp.t) : Graph.edge list =
  let direct_edges =
    List.filter_map (fun f -> f sexp) [ extract_require; extract_autoload ]
  in
  let child_edges =
    match sexp with
    | Sexp.List (children, _) | Sexp.Vector (children, _) ->
        List.concat_map extract_edges_from_sexp children
    | Sexp.Cons (car, cdr, _) ->
        extract_edges_from_sexp car @ extract_edges_from_sexp cdr
    | Sexp.Int _ | Sexp.Float _ | Sexp.String _ | Sexp.Symbol _ | Sexp.Keyword _
    | Sexp.Char _ | Sexp.Error _ ->
        []
  in
  direct_edges @ child_edges

let extract_from_sexp (forms : Sexp.t list) : Graph.edge list =
  List.concat_map extract_edges_from_sexp forms |> dedup_edges

(** {1 Extraction from .tart Files} *)

(** Extract edges from a single declaration *)
let rec extract_edges_from_decl (decl : Sig_ast.decl) : Graph.edge list =
  match decl with
  | Sig_ast.DOpen (name, _) -> [ { Graph.target = name; kind = Graph.Open } ]
  | Sig_ast.DInclude (name, _) ->
      [ { Graph.target = name; kind = Graph.Include } ]
  | Sig_ast.DTypeScope { scope_decls; _ } ->
      List.concat_map extract_edges_from_decl scope_decls
  | Sig_ast.DDefun _ | Sig_ast.DDefvar _ | Sig_ast.DType _
  | Sig_ast.DImportStruct _ | Sig_ast.DData _ ->
      []

let extract_from_signature (sig_ : Sig_ast.signature) : Graph.edge list =
  List.concat_map extract_edges_from_decl sig_.sig_decls |> dedup_edges

(** {1 Sibling Edge} *)

let make_sibling_edge (target : string) : Graph.edge =
  { Graph.target; kind = Graph.Sibling }

let sibling_edge (_module_id : string) : Graph.edge option =
  (* This is a placeholder - actual file existence check should be done by caller *)
  None
