(** Workspace symbol search for the LSP server.

    Provides cross-file symbol lookup across all open [.el] and [.tart]
    documents, triggered by workspace/symbol requests. *)

let range_of_span = Span_conv.range_of_span

(** Derive a container name from a URI.

    For ["file:///path/to/foo.el"], returns ["foo"]. For
    ["file:///path/to/bar.tart"], returns ["bar"]. *)
let container_name_of_uri (uri : string) : string =
  let filename = Uri.to_filename uri in
  let basename = Filename.basename filename in
  Filename.chop_extension basename

(** Extract top-level definition symbols from parsed Emacs Lisp S-expressions.

    Walks the top-level forms and collects [defun], [defvar], [defconst], and
    [defmacro] definitions as flat {!Protocol.symbol_information} items. *)
let rec extract_el_symbols ~(uri : string) ~(text : string)
    ~(container : string) (sexps : Syntax.Sexp.t list) :
    Protocol.symbol_information list =
  List.concat_map (extract_el_symbol ~uri ~text ~container) sexps

and extract_el_symbol ~(uri : string) ~(text : string) ~(container : string)
    (sexp : Syntax.Sexp.t) : Protocol.symbol_information list =
  let open Syntax.Sexp in
  match sexp with
  | List (Symbol ("defun", _) :: Symbol (name, name_span) :: _ :: body, _) ->
      let range = range_of_span ~text name_span in
      let si =
        {
          Protocol.si_name = name;
          si_kind = Protocol.SKFunction;
          si_location = { uri; range };
          si_container_name = Some container;
        }
      in
      (* Collect nested defuns from the body *)
      let nested =
        List.concat_map (function List (elems, _) -> elems | _ -> []) body
      in
      si :: extract_el_symbols ~uri ~text ~container nested
  | List (Symbol ("defvar", _) :: Symbol (name, name_span) :: _, _) ->
      let range = range_of_span ~text name_span in
      [
        {
          Protocol.si_name = name;
          si_kind = Protocol.SKVariable;
          si_location = { uri; range };
          si_container_name = Some container;
        };
      ]
  | List (Symbol ("defconst", _) :: Symbol (name, name_span) :: _, _) ->
      let range = range_of_span ~text name_span in
      [
        {
          Protocol.si_name = name;
          si_kind = Protocol.SKConstant;
          si_location = { uri; range };
          si_container_name = Some container;
        };
      ]
  | List (Symbol ("defmacro", _) :: Symbol (name, name_span) :: _, _) ->
      let range = range_of_span ~text name_span in
      [
        {
          Protocol.si_name = name;
          si_kind = Protocol.SKMethod;
          si_location = { uri; range };
          si_container_name = Some container;
        };
      ]
  | _ -> []

let symbols_from_el_doc ~(uri : string) ~(text : string) :
    Protocol.symbol_information list =
  let filename = Uri.to_filename uri in
  let parse_result = Syntax.Read.parse_string ~filename text in
  let container = container_name_of_uri uri in
  extract_el_symbols ~uri ~text ~container parse_result.sexps

(** Map a {!Sig.Sig_ast.decl} to a {!Protocol.symbol_kind}. *)
let kind_of_decl (decl : Sig.Sig_ast.decl) : Protocol.symbol_kind option =
  match decl with
  | Sig.Sig_ast.DDefun _ -> Some Protocol.SKFunction
  | Sig.Sig_ast.DDefvar _ -> Some Protocol.SKVariable
  | Sig.Sig_ast.DType { type_bindings = { tb_body = None; _ } :: _; _ } ->
      Some Protocol.SKInterface
  | Sig.Sig_ast.DType { type_bindings = { tb_body = Some _; _ } :: _; _ } ->
      Some Protocol.SKTypeParameter
  | Sig.Sig_ast.DType { type_bindings = []; _ } -> None
  | Sig.Sig_ast.DData _ -> Some Protocol.SKEnum
  | Sig.Sig_ast.DImportStruct _ -> Some Protocol.SKStruct
  | Sig.Sig_ast.DDefstruct _ -> Some Protocol.SKStruct
  | Sig.Sig_ast.DLetType _ -> Some Protocol.SKTypeParameter
  | Sig.Sig_ast.DOpen _ | Sig.Sig_ast.DInclude _ | Sig.Sig_ast.DForall _ -> None

(** Get the name of a declaration, if it has one. *)
let name_of_decl (decl : Sig.Sig_ast.decl) : string option =
  match decl with
  | Sig.Sig_ast.DDefun d -> Some d.defun_name
  | Sig.Sig_ast.DDefvar d -> Some d.defvar_name
  | Sig.Sig_ast.DType d -> (
      match d.type_bindings with b :: _ -> Some b.tb_name | [] -> None)
  | Sig.Sig_ast.DData d -> Some d.data_name
  | Sig.Sig_ast.DImportStruct d -> Some d.struct_name
  | Sig.Sig_ast.DDefstruct d -> Some d.sd_name
  | Sig.Sig_ast.DLetType d -> (
      match d.type_bindings with b :: _ -> Some b.tb_name | [] -> None)
  | Sig.Sig_ast.DOpen _ | Sig.Sig_ast.DInclude _ | Sig.Sig_ast.DForall _ -> None

(** Extract symbols from a list of signature declarations, recursing into
    [DForall] scopes. *)
let rec extract_tart_decl_symbols ~(uri : string) ~(text : string)
    ~(container : string) (decls : Sig.Sig_ast.decl list) :
    Protocol.symbol_information list =
  List.concat_map
    (fun decl ->
      match (name_of_decl decl, kind_of_decl decl) with
      | Some name, Some kind ->
          let span = Sig.Sig_ast.decl_loc decl in
          let range = range_of_span ~text span in
          [
            {
              Protocol.si_name = name;
              si_kind = kind;
              si_location = { uri; range };
              si_container_name = Some container;
            };
          ]
      | _ ->
          (* Recurse into DForall bodies *)
          let nested_decls =
            match decl with Sig.Sig_ast.DForall d -> d.forall_decls | _ -> []
          in
          extract_tart_decl_symbols ~uri ~text ~container nested_decls)
    decls

let symbols_from_tart_doc ~(uri : string) ~(text : string) :
    Protocol.symbol_information list =
  let filename = Uri.to_filename uri in
  let module_name =
    let basename = Filename.basename filename in
    if Filename.check_suffix basename ".tart" then
      Filename.chop_suffix basename ".tart"
    else basename
  in
  let parse_result = Syntax.Read.parse_string ~filename text in
  match Sig.Sig_parser.parse_signature ~module_name parse_result.sexps with
  | Ok sig_ast ->
      extract_tart_decl_symbols ~uri ~text ~container:sig_ast.sig_module
        sig_ast.sig_decls
  | Error _ -> []

(** Case-insensitive substring match. *)
let matches_query ~(query : string) (name : string) : bool =
  if query = "" then true
  else
    let lq = String.lowercase_ascii query in
    let ln = String.lowercase_ascii name in
    let qlen = String.length lq in
    let nlen = String.length ln in
    if qlen > nlen then false
    else
      let found = ref false in
      for i = 0 to nlen - qlen do
        if (not !found) && String.sub ln i qlen = lq then found := true
      done;
      !found

let handle ~(documents : Document.t) ~(query : string) :
    (Yojson.Safe.t, Rpc.response_error) result =
  let uris = Document.list_uris documents in
  let symbols =
    List.concat_map
      (fun uri ->
        match Document.get_doc documents uri with
        | None -> []
        | Some doc ->
            if Signature_tracker.is_tart_file uri then
              symbols_from_tart_doc ~uri ~text:doc.text
            else symbols_from_el_doc ~uri ~text:doc.text)
      uris
  in
  let filtered =
    if query = "" then symbols
    else
      List.filter (fun si -> matches_query ~query si.Protocol.si_name) symbols
  in
  Ok (Protocol.workspace_symbol_result_to_json (Some filtered))
