(** Folding range computation for the LSP server.

    Provides fold regions for top-level forms, let blocks, multi-line strings,
    and comment blocks, triggered by textDocument/foldingRange requests. *)

(** Top-level definition forms that should produce fold ranges. *)
let definition_forms =
  [
    "defun";
    "defvar";
    "defconst";
    "defmacro";
    "defcustom";
    "defsubst";
    "cl-defun";
    "cl-defmacro";
    "defclass";
    "cl-defstruct";
  ]

(** Binding forms that should produce fold ranges. *)
let binding_forms = [ "let"; "let*" ]

(** Check whether a span crosses multiple lines. *)
let is_multiline (span : Syntax.Location.span) : bool =
  span.start_pos.line <> span.end_pos.line

(** Convert a multi-line span to a folding range (0-based lines). *)
let range_of_span (span : Syntax.Location.span) : Protocol.folding_range =
  {
    fr_start_line = span.start_pos.line - 1;
    fr_start_character = Some span.start_pos.col;
    fr_end_line = span.end_pos.line - 1;
    fr_end_character = Some span.end_pos.col;
    fr_kind = None;
  }

(** Recursively collect fold ranges from S-expressions. *)
let rec collect_from_sexp (sexp : Syntax.Sexp.t) : Protocol.folding_range list =
  match sexp with
  | Syntax.Sexp.List (Syntax.Sexp.Symbol (head, _) :: _, span)
    when List.mem head definition_forms || List.mem head binding_forms ->
      let self = if is_multiline span then [ range_of_span span ] else [] in
      let children = collect_from_children sexp in
      self @ children
  | Syntax.Sexp.String (_, span) ->
      if is_multiline span then [ range_of_span span ] else []
  | Syntax.Sexp.List _ | Syntax.Sexp.Vector _ | Syntax.Sexp.Curly _
  | Syntax.Sexp.Cons _ ->
      collect_from_children sexp
  | Syntax.Sexp.Int _ | Syntax.Sexp.Float _ | Syntax.Sexp.Symbol _
  | Syntax.Sexp.Keyword _ | Syntax.Sexp.Char _ | Syntax.Sexp.Error _ ->
      []

(** Collect fold ranges from child nodes of a compound S-expression. *)
and collect_from_children (sexp : Syntax.Sexp.t) : Protocol.folding_range list =
  match sexp with
  | Syntax.Sexp.List (elems, _)
  | Syntax.Sexp.Vector (elems, _)
  | Syntax.Sexp.Curly (elems, _) ->
      List.concat_map collect_from_sexp elems
  | Syntax.Sexp.Cons (car, cdr, _) ->
      collect_from_sexp car @ collect_from_sexp cdr
  | _ -> []

let collect_sexp_folds (sexps : Syntax.Sexp.t list) :
    Protocol.folding_range list =
  List.concat_map collect_from_sexp sexps

let collect_comment_folds (text : string) : Protocol.folding_range list =
  let lines = String.split_on_char '\n' text in
  let is_comment_line line =
    let trimmed = String.trim line in
    String.length trimmed > 0 && trimmed.[0] = ';'
  in
  let rec scan lines line_num run_start acc =
    match lines with
    | [] ->
        (* End of input — flush any open run *)
        flush run_start line_num acc
    | line :: rest ->
        if is_comment_line line then
          let start = match run_start with Some s -> s | None -> line_num in
          scan rest (line_num + 1) (Some start) acc
        else
          let acc = flush run_start line_num acc in
          scan rest (line_num + 1) None acc
  and flush run_start current_line acc =
    match run_start with
    | Some start when current_line - start >= 2 ->
        let range : Protocol.folding_range =
          {
            fr_start_line = start;
            fr_start_character = None;
            fr_end_line = current_line - 1;
            fr_end_character = None;
            fr_kind = Some Protocol.FRComment;
          }
        in
        range :: acc
    | _ -> acc
  in
  List.rev (scan lines 0 None [])

let handle ~(uri : string) ~(doc_text : string) :
    (Yojson.Safe.t, Rpc.response_error) result =
  let _ = uri in
  let filename = "<foldingRange>" in
  let parse_result = Syntax.Read.parse_string ~filename doc_text in
  let sexp_folds = collect_sexp_folds parse_result.sexps in
  let comment_folds = collect_comment_folds doc_text in
  (* Merge and deduplicate by start line, preferring the first occurrence *)
  let all_folds = sexp_folds @ comment_folds in
  let seen = Hashtbl.create 16 in
  let deduped =
    List.filter
      (fun (r : Protocol.folding_range) ->
        if Hashtbl.mem seen r.fr_start_line then false
        else (
          Hashtbl.replace seen r.fr_start_line true;
          true))
      all_folds
  in
  Ok (Protocol.folding_range_result_to_json (Some deduped))
