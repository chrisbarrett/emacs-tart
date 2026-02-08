(** Completion provider for the LSP server.

    Provides textDocument/completion responses with candidates from local
    definitions, loaded signatures, and the type environment. *)

module Log = Tart_log.Log

(** Extract the word prefix at the given position.

    Scans backwards from the position to find the start of the current symbol.
    Returns the prefix string and its start column. *)
let extract_prefix_at_position (text : string) (line : int) (col : int) :
    string * int =
  (* Find the line in the text *)
  let lines = String.split_on_char '\n' text in
  if line >= List.length lines then ("", col)
  else
    let line_text = List.nth lines line in
    if col > String.length line_text then ("", col)
    else
      (* Scan backwards to find word start *)
      let rec find_start i =
        if i < 0 then 0
        else
          let c = line_text.[i] in
          (* Symbol characters in elisp: alphanumeric, hyphen, underscore, etc. *)
          if
            (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
            || (c >= '0' && c <= '9')
            || c = '-' || c = '_' || c = '+' || c = '*' || c = '/' || c = '<'
            || c = '>' || c = '=' || c = '!' || c = '?' || c = '%'
          then find_start (i - 1)
          else i + 1
      in
      let start_col = find_start (col - 1) in
      let prefix = String.sub line_text start_col (col - start_col) in
      (prefix, start_col)

(** Collect completion candidates from definitions in the document.

    Extracts function and variable names from defun/defvar/defconst forms. *)
let collect_local_completions (sexps : Syntax.Sexp.t list) :
    Protocol.completion_item list =
  let open Syntax.Sexp in
  List.filter_map
    (fun sexp ->
      match sexp with
      | List (Symbol ("defun", _) :: Symbol (name, _) :: args :: _, _) ->
          let param_detail =
            match args with
            | List (params, _) ->
                let param_strs =
                  List.filter_map
                    (function
                      | Symbol (p, _)
                        when not (String.length p > 0 && p.[0] = '&') ->
                          Some p
                      | _ -> None)
                    params
                in
                if param_strs = [] then "()"
                else "(" ^ String.concat " " param_strs ^ ")"
            | _ -> "()"
          in
          Some
            {
              Protocol.ci_label = name;
              ci_kind = Some Protocol.CIKFunction;
              ci_detail = Some param_detail;
              ci_documentation = None;
              ci_insert_text = None;
            }
      | List
          ( (Symbol ("defvar", _) | Symbol ("defconst", _))
            :: Symbol (name, _)
            :: _,
            _ ) ->
          let kind =
            match sexp with
            | List (Symbol ("defconst", _) :: _, _) -> Protocol.CIKConstant
            | _ -> Protocol.CIKVariable
          in
          Some
            {
              Protocol.ci_label = name;
              ci_kind = Some kind;
              ci_detail = None;
              ci_documentation = None;
              ci_insert_text = None;
            }
      | _ -> None)
    sexps

(** Collect completion candidates from a type environment.

    Creates completion items for all bound names with their types. *)
let collect_env_completions (env : Core.Type_env.t) :
    Protocol.completion_item list =
  List.filter_map
    (fun (name, scheme) ->
      let ty =
        match scheme with
        | Core.Type_env.Mono t -> t
        | Core.Type_env.Poly (_, t) -> t
      in
      let kind =
        match ty with
        | Core.Types.TArrow _ -> Protocol.CIKFunction
        | _ -> Protocol.CIKVariable
      in
      let detail = Core.Types.to_string ty in
      Some
        {
          Protocol.ci_label = name;
          ci_kind = Some kind;
          ci_detail = Some detail;
          ci_documentation = None;
          ci_insert_text = None;
        })
    env.Core.Type_env.bindings

(** Filter completions by prefix match (case-insensitive) *)
let filter_by_prefix (prefix : string) (items : Protocol.completion_item list) :
    Protocol.completion_item list =
  if prefix = "" then items
  else
    let prefix_lower = String.lowercase_ascii prefix in
    List.filter
      (fun item ->
        let label_lower = String.lowercase_ascii item.Protocol.ci_label in
        String.length label_lower >= String.length prefix_lower
        && String.sub label_lower 0 (String.length prefix_lower) = prefix_lower)
      items

(** Deduplicate completion items by label, preferring items with types *)
let deduplicate_completions (items : Protocol.completion_item list) :
    Protocol.completion_item list =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun item ->
      match Hashtbl.find_opt tbl item.Protocol.ci_label with
      | None -> Hashtbl.add tbl item.Protocol.ci_label item
      | Some existing ->
          (* Prefer items with type information *)
          if existing.ci_detail = None && item.ci_detail <> None then
            Hashtbl.replace tbl item.Protocol.ci_label item)
    items;
  Hashtbl.fold (fun _ item acc -> item :: acc) tbl []
  |> List.sort (fun a b ->
      String.compare a.Protocol.ci_label b.Protocol.ci_label)

(** Handle textDocument/completion request.

    Returns completion items for the current position, including:
    - Local definitions (defun, defvar, defconst)
    - Functions from loaded signatures (stdlib, requires) *)
let handle ~(config : Typing.Module_check.config) ~(uri : string)
    ~(doc_text : string) ~(line : int) ~(col : int) :
    (Yojson.Safe.t, Rpc.response_error) result =
  let filename = Uri.to_filename uri in
  let prefix, _ = extract_prefix_at_position doc_text line col in
  Log.debug "Completion prefix: '%s'" prefix;

  let parse_result = Syntax.Read.parse_string ~filename doc_text in

  (* Collect local definitions *)
  let local_items = collect_local_completions parse_result.sexps in
  Log.debug "Found %d local completions" (List.length local_items);

  (* Type-check to get environment with signatures *)
  let check_result =
    Typing.Module_check.check_module ~config ~filename parse_result.sexps
  in

  (* Collect items from loaded signatures *)
  let sig_items =
    match check_result.signature_env with
    | Some env -> collect_env_completions env
    | None -> []
  in
  Log.debug "Found %d signature completions" (List.length sig_items);

  (* Collect items from final environment (includes builtins) *)
  let env_items = collect_env_completions check_result.final_env in
  Log.debug "Found %d env completions" (List.length env_items);

  (* Combine, deduplicate, and filter *)
  let all_items = local_items @ sig_items @ env_items in
  let filtered = filter_by_prefix prefix all_items in
  let deduped = deduplicate_completions filtered in
  Log.debug "Returning %d completions" (List.length deduped);

  Ok (Protocol.completion_result_to_json (Some deduped))
