(** Semantic token computation for the LSP server.

    Classifies AST nodes and comment lines into semantic tokens for
    textDocument/semanticTokens/full and textDocument/semanticTokens/full/delta
    requests. Tokens are delta-encoded as specified by the LSP protocol. *)

type raw_token = {
  rt_line : int;
  rt_col : int;
  rt_length : int;
  rt_type : Protocol.semantic_token_type;
  rt_modifiers : Protocol.semantic_token_modifier list;
}

(** Special forms that get [STKeyword] when in head position. *)
let special_forms =
  [
    "if";
    "let";
    "let*";
    "progn";
    "setq";
    "cond";
    "and";
    "or";
    "when";
    "unless";
    "while";
    "quote";
    "lambda";
    "function";
    "save-excursion";
    "save-restriction";
    "unwind-protect";
    "condition-case";
    "catch";
    "throw";
    "interactive";
    "declare";
    "require";
    "provide";
  ]

(** Definition forms: head keyword is [STKeyword], second element gets a
    type-specific token with [SMDefinition]. *)
let defun_forms =
  [ "defun"; "defsubst"; "cl-defun"; "cl-defmacro"; "defclass"; "cl-defstruct" ]

let defvar_forms = [ "defvar"; "defcustom" ]
let defconst_forms = [ "defconst" ]
let defmacro_forms = [ "defmacro" ]

let all_definition_forms =
  defun_forms @ defvar_forms @ defconst_forms @ defmacro_forms

(** Convert a span to a raw token with 0-based line and UTF-16 columns. *)
let token_of_span ~(text : string) ~(rt_type : Protocol.semantic_token_type)
    ~(rt_modifiers : Protocol.semantic_token_modifier list)
    (span : Syntax.Location.span) : raw_token =
  let line_0 = span.start_pos.line - 1 in
  let line_text = Document.line_text_at text line_0 in
  let start_utf16 =
    match line_text with
    | Some lt ->
        Document.utf16_offset_of_byte ~line_text:lt
          ~byte_offset:span.start_pos.col
    | None -> span.start_pos.col
  in
  let end_line_0 = span.end_pos.line - 1 in
  let end_utf16 =
    if end_line_0 <> line_0 then
      (* Multi-line token: length covers only the first line portion *)
      let first_line_len =
        match line_text with Some lt -> String.length lt | None -> 0
      in
      let first_line_end_utf16 =
        match line_text with
        | Some lt ->
            Document.utf16_offset_of_byte ~line_text:lt
              ~byte_offset:first_line_len
        | None -> first_line_len
      in
      first_line_end_utf16
    else
      match line_text with
      | Some lt ->
          Document.utf16_offset_of_byte ~line_text:lt
            ~byte_offset:span.end_pos.col
      | None -> span.end_pos.col
  in
  let rt_length = max 0 (end_utf16 - start_utf16) in
  { rt_line = line_0; rt_col = start_utf16; rt_length; rt_type; rt_modifiers }

(** Emit tokens for parameter symbols in a parameter list. *)
let rec tokens_of_params ~(text : string) (sexp : Syntax.Sexp.t) :
    raw_token list =
  match sexp with
  | Syntax.Sexp.List (elems, _) ->
      List.concat_map (tokens_of_params ~text) elems
  | Syntax.Sexp.Symbol (name, span) ->
      if String.length name > 0 && name.[0] = '&' then
        (* &optional, &rest — treat as keyword *)
        [ token_of_span ~text ~rt_type:STKeyword ~rt_modifiers:[] span ]
      else [ token_of_span ~text ~rt_type:STParameter ~rt_modifiers:[] span ]
  | _ -> []

(** Recursively collect semantic tokens from a single S-expression. *)
let rec collect_from_sexp ~(text : string) (sexp : Syntax.Sexp.t) :
    raw_token list =
  match sexp with
  | Syntax.Sexp.List (Syntax.Sexp.Symbol (head, head_span) :: rest, _)
    when List.mem head all_definition_forms ->
      (* Definition form: keyword token for head *)
      let head_tok =
        token_of_span ~text ~rt_type:STKeyword ~rt_modifiers:[] head_span
      in
      let name_tok, param_toks, body =
        match rest with
        | Syntax.Sexp.Symbol (_name, name_span) :: after_name ->
            let name_type, name_mods =
              if List.mem head defun_forms then
                (Protocol.STFunction, [ Protocol.SMDefinition ])
              else if List.mem head defmacro_forms then
                (Protocol.STMacro, [ Protocol.SMDefinition ])
              else if List.mem head defconst_forms then
                ( Protocol.STVariable,
                  [ Protocol.SMDefinition; Protocol.SMReadonly ] )
              else
                (* defvar, defcustom *)
                (Protocol.STVariable, [ Protocol.SMDefinition ])
            in
            let name_t =
              [
                token_of_span ~text ~rt_type:name_type ~rt_modifiers:name_mods
                  name_span;
              ]
            in
            (* Extract parameter tokens for defun-like forms *)
            let params, body_rest =
              if List.mem head defun_forms || List.mem head defmacro_forms then
                match after_name with
                | param_list :: body_rest ->
                    (tokens_of_params ~text param_list, body_rest)
                | [] -> ([], [])
              else ([], after_name)
            in
            (name_t, params, body_rest)
        | _ -> ([], [], rest)
      in
      let body_toks = List.concat_map (collect_from_sexp ~text) body in
      (head_tok :: name_tok) @ param_toks @ body_toks
  | Syntax.Sexp.List (Syntax.Sexp.Symbol (head, head_span) :: rest, _)
    when List.mem head special_forms ->
      (* Special form: keyword token for head *)
      let head_tok =
        token_of_span ~text ~rt_type:STKeyword ~rt_modifiers:[] head_span
      in
      (* For lambda, extract parameters *)
      let param_toks, body =
        if head = "lambda" then
          match rest with
          | param_list :: body_rest ->
              (tokens_of_params ~text param_list, body_rest)
          | [] -> ([], [])
        else ([], rest)
      in
      let body_toks = List.concat_map (collect_from_sexp ~text) body in
      (head_tok :: param_toks) @ body_toks
  | Syntax.Sexp.List (Syntax.Sexp.Symbol (_head, head_span) :: rest, _) ->
      (* Plain function call: head is a variable (function reference) *)
      let head_tok =
        token_of_span ~text ~rt_type:STVariable ~rt_modifiers:[] head_span
      in
      let rest_toks = List.concat_map (collect_from_sexp ~text) rest in
      head_tok :: rest_toks
  | Syntax.Sexp.List (elems, _)
  | Syntax.Sexp.Vector (elems, _)
  | Syntax.Sexp.Curly (elems, _) ->
      List.concat_map (collect_from_sexp ~text) elems
  | Syntax.Sexp.Cons (car, cdr, _) ->
      collect_from_sexp ~text car @ collect_from_sexp ~text cdr
  | Syntax.Sexp.Symbol (_name, span) ->
      [ token_of_span ~text ~rt_type:STVariable ~rt_modifiers:[] span ]
  | Syntax.Sexp.Int (_, span) ->
      [ token_of_span ~text ~rt_type:STNumber ~rt_modifiers:[] span ]
  | Syntax.Sexp.Float (_, span) ->
      [ token_of_span ~text ~rt_type:STNumber ~rt_modifiers:[] span ]
  | Syntax.Sexp.String (_, span) ->
      [ token_of_span ~text ~rt_type:STString ~rt_modifiers:[] span ]
  | Syntax.Sexp.Char (_, span) ->
      [ token_of_span ~text ~rt_type:STString ~rt_modifiers:[] span ]
  | Syntax.Sexp.Keyword (_, span) ->
      [ token_of_span ~text ~rt_type:STKeyword ~rt_modifiers:[] span ]
  | Syntax.Sexp.Error _ -> []

let collect_sexp_tokens ~(text : string) (sexps : Syntax.Sexp.t list) :
    raw_token list =
  List.concat_map (collect_from_sexp ~text) sexps

let collect_comment_tokens (text : string) : raw_token list =
  let lines = String.split_on_char '\n' text in
  let rec scan lines line_num acc =
    match lines with
    | [] -> List.rev acc
    | line :: rest ->
        let trimmed = String.trim line in
        if String.length trimmed > 0 && trimmed.[0] = ';' then
          (* Find the start of the comment (first ';') *)
          let start_byte =
            let len = String.length line in
            let rec find i =
              if i >= len then 0 else if line.[i] = ';' then i else find (i + 1)
            in
            find 0
          in
          let end_byte = String.length line in
          let line_text = Document.line_text_at text line_num in
          let start_utf16 =
            match line_text with
            | Some lt ->
                Document.utf16_offset_of_byte ~line_text:lt
                  ~byte_offset:start_byte
            | None -> start_byte
          in
          let end_utf16 =
            match line_text with
            | Some lt ->
                Document.utf16_offset_of_byte ~line_text:lt
                  ~byte_offset:end_byte
            | None -> end_byte
          in
          let tok =
            {
              rt_line = line_num;
              rt_col = start_utf16;
              rt_length = max 0 (end_utf16 - start_utf16);
              rt_type = Protocol.STComment;
              rt_modifiers = [];
            }
          in
          scan rest (line_num + 1) (tok :: acc)
        else scan rest (line_num + 1) acc
  in
  scan lines 0 []

(** Compute the modifier bitfield for a list of modifiers. *)
let modifier_bits (mods : Protocol.semantic_token_modifier list) : int =
  List.fold_left
    (fun acc m -> acc lor (1 lsl Protocol.semantic_token_modifier_bit m))
    0 mods

let delta_encode (tokens : raw_token list) : int list =
  let sorted =
    List.sort
      (fun a b ->
        let c = compare a.rt_line b.rt_line in
        if c <> 0 then c else compare a.rt_col b.rt_col)
      tokens
  in
  let rec encode prev_line prev_col tokens acc =
    match tokens with
    | [] -> List.rev acc
    | t :: rest ->
        let delta_line = t.rt_line - prev_line in
        let delta_col =
          if delta_line = 0 then t.rt_col - prev_col else t.rt_col
        in
        let type_idx = Protocol.semantic_token_type_index t.rt_type in
        let mod_bits = modifier_bits t.rt_modifiers in
        let entry =
          [ delta_line; delta_col; t.rt_length; type_idx; mod_bits ]
        in
        encode t.rt_line t.rt_col rest (List.rev_append entry acc)
  in
  encode 0 0 sorted []

(** {1 Token Cache} *)

type cache = {
  entries : (string, string * int array) Hashtbl.t;
      (** URI → (result_id, token_data) *)
  mutable next_id : int;
}
(** Cache of previous token arrays, keyed by URI. Stores the result ID and the
    flat int array from the last full or delta response. *)

let create_cache () : cache = { entries = Hashtbl.create 16; next_id = 0 }

let fresh_result_id (cache : cache) : string =
  let id = cache.next_id in
  cache.next_id <- id + 1;
  string_of_int id

(** Remove the cached entry for a URI (e.g., on document close). *)
let invalidate (cache : cache) (uri : string) : unit =
  Hashtbl.remove cache.entries uri

(** {1 Delta Computation} *)

(** Compute the minimal sequence of edits to transform [old_data] into
    [new_data]. Each edit replaces a contiguous run of elements in the old
    array. Edits are emitted from left to right with non-overlapping,
    non-adjacent changed regions. *)
let compute_edits (old_data : int array) (new_data : int array) :
    Protocol.semantic_tokens_edit list =
  let old_len = Array.length old_data in
  let new_len = Array.length new_data in
  let min_len = min old_len new_len in
  (* Find common prefix length *)
  let prefix =
    let i = ref 0 in
    while !i < min_len && old_data.(!i) = new_data.(!i) do
      incr i
    done;
    !i
  in
  (* Find common suffix length (but don't overlap with prefix) *)
  let suffix =
    let s = ref 0 in
    while
      !s < min_len - prefix
      && old_data.(old_len - 1 - !s) = new_data.(new_len - 1 - !s)
    do
      incr s
    done;
    !s
  in
  let old_mid = old_len - prefix - suffix in
  let new_mid = new_len - prefix - suffix in
  if old_mid = 0 && new_mid = 0 then []
  else
    let data =
      if new_mid > 0 then Array.to_list (Array.sub new_data prefix new_mid)
      else []
    in
    [
      {
        Protocol.ste_start = prefix;
        ste_delete_count = old_mid;
        ste_data = data;
      };
    ]

(** Compute delta-encoded token data for a document. *)
let compute_data (doc_text : string) : int list =
  let filename = "<semanticTokens>" in
  let parse_result = Syntax.Read.parse_string ~filename doc_text in
  let sexp_tokens = collect_sexp_tokens ~text:doc_text parse_result.sexps in
  let comment_tokens = collect_comment_tokens doc_text in
  let all_tokens = sexp_tokens @ comment_tokens in
  delta_encode all_tokens

(** {1 Request Handlers} *)

let handle ~(uri : string) ~(doc_text : string) ~(cache : cache) :
    (Yojson.Safe.t, Rpc.response_error) result =
  let _ = uri in
  let data = compute_data doc_text in
  let result_id = fresh_result_id cache in
  let data_arr = Array.of_list data in
  Hashtbl.replace cache.entries uri (result_id, data_arr);
  Ok
    (Protocol.semantic_tokens_result_to_json
       (Some { str_result_id = result_id; str_data = data }))

let handle_delta ~(uri : string) ~(doc_text : string)
    ~(previous_result_id : string) ~(cache : cache) :
    (Yojson.Safe.t, Rpc.response_error) result =
  let new_data = compute_data doc_text in
  let new_arr = Array.of_list new_data in
  let result_id = fresh_result_id cache in
  let response =
    match Hashtbl.find_opt cache.entries uri with
    | Some (cached_id, old_arr) when String.equal cached_id previous_result_id
      ->
        let edits = compute_edits old_arr new_arr in
        Protocol.DeltaResponse
          { stdr_result_id = result_id; stdr_edits = edits }
    | _ ->
        (* No cache hit or result ID mismatch: fall back to full response *)
        Protocol.FullResponse { str_result_id = result_id; str_data = new_data }
  in
  Hashtbl.replace cache.entries uri (result_id, new_arr);
  Ok (Protocol.semantic_tokens_delta_response_to_json (Some response))
