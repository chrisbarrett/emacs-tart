(** Inlay hint computation for the LSP server.

    Provides inline type annotations for [defun] return types and [let]/[let*]
    binding types, triggered by textDocument/inlayHint requests. Hints are
    suppressed when the type is trivially obvious (e.g., a string literal bound
    to a variable). *)

(** Convert a span's end position to an LSP position with UTF-16 encoding. *)
let position_of_span_end ~(text : string) (span : Syntax.Location.span) :
    Protocol.position =
  let line = span.end_pos.line - 1 in
  let character =
    match Document.line_text_at text line with
    | Some line_text ->
        Document.utf16_offset_of_byte ~line_text ~byte_offset:span.end_pos.col
    | None -> span.end_pos.col
  in
  { Protocol.line; character }

(** Check whether a position falls within the requested range. *)
let in_range (pos : Protocol.position) (range : Protocol.range) : bool =
  let after_start =
    pos.line > range.start.line
    || (pos.line = range.start.line && pos.character >= range.start.character)
  in
  let before_end =
    pos.line < range.end_.line
    || (pos.line = range.end_.line && pos.character <= range.end_.character)
  in
  after_start && before_end

(** Extract the base type from a potentially literal type. *)
let base_type (ty : Core.Types.typ) : Core.Types.typ =
  match Core.Types.repr ty with
  | Core.Types.TLiteral (_, base) -> Core.Types.repr base
  | other -> other

(** Check whether an init expression makes the binding type trivially obvious.

    Returns [true] when:
    - string literal and type is String
    - integer literal and type is Int
    - float literal and type is Float
    - [nil] and type is Nil
    - [t] and type is T *)
let is_trivial_type (init : Syntax.Sexp.t) (ty : Core.Types.typ) : bool =
  let bt = base_type ty in
  match init with
  | Syntax.Sexp.String _ -> Core.Types.equal bt Core.Types.Prim.string
  | Syntax.Sexp.Int _ -> Core.Types.equal bt Core.Types.Prim.int
  | Syntax.Sexp.Float _ -> Core.Types.equal bt Core.Types.Prim.float
  | Syntax.Sexp.Symbol ("nil", _) -> Core.Types.equal bt Core.Types.Prim.nil
  | Syntax.Sexp.Symbol ("t", _) -> Core.Types.equal bt Core.Types.Prim.t
  | _ -> false

(** Safely infer a type and solve constraints, returning the resolved type.

    Returns [None] on any inference or unification failure. *)
let safe_infer_and_solve (env : Core.Type_env.t) (sexp : Syntax.Sexp.t) :
    Core.Types.typ option =
  try
    let result = Typing.Infer.infer env sexp in
    (match Typing.Unify.solve result.Typing.Infer.constraints with
    | Ok () -> ()
    | Error _ -> ());
    Some (Core.Types.repr result.Typing.Infer.ty)
  with _ -> None

(** Extract the return type from a function type. *)
let return_type_of (ty : Core.Types.typ) : Core.Types.typ option =
  match Core.Types.repr ty with
  | Core.Types.TArrow (_, ret) -> Some (Core.Types.repr ret)
  | Core.Types.TForall (_, Core.Types.TArrow (_, ret)) ->
      Some (Core.Types.repr ret)
  | _ -> None

(** Check whether a function name has a [TartDeclareForm] in the check result.
*)
let has_tart_declaration (check_result : Typing.Check.check_result)
    (name : string) : bool =
  List.exists
    (fun (form : Typing.Check.form_result) ->
      match form with
      | Typing.Check.TartDeclareForm { name = n; _ } -> String.equal n name
      | _ -> false)
    check_result.forms

(** Collect inlay hints for [defun] return types from top-level forms. *)
let collect_defun_hints ~(text : string) ~(range : Protocol.range)
    ~(check_result : Typing.Check.check_result) (sexps : Syntax.Sexp.t list) :
    Protocol.inlay_hint list =
  List.filter_map
    (fun sexp ->
      match sexp with
      | Syntax.Sexp.List
          ( Syntax.Sexp.Symbol ("defun", _)
            :: Syntax.Sexp.Symbol (name, _)
            :: Syntax.Sexp.List (_, params_span)
            :: _body,
            _span )
        when not (has_tart_declaration check_result name) -> (
          Core.Types.reset_tvar_counter ();
          match Typing.Infer.infer_defun check_result.env sexp with
          | Some defun_result -> (
              (match
                 Typing.Unify.solve defun_result.Typing.Infer.defun_constraints
               with
              | Ok () -> ()
              | Error _ -> ());
              match return_type_of defun_result.Typing.Infer.fn_type with
              | Some ret_ty ->
                  let pos = position_of_span_end ~text params_span in
                  if in_range pos range then
                    Some
                      {
                        Protocol.ih_position = pos;
                        ih_label = ": " ^ Core.Types.to_string ret_ty;
                        ih_kind = Some Protocol.IHType;
                        ih_padding_left = false;
                        ih_padding_right = false;
                      }
                  else None
              | None -> None)
          | None -> None)
      | _ -> None)
    sexps

(** Recursively collect inlay hints for [let]/[let*] binding types. *)
let rec collect_let_hints ~(text : string) ~(range : Protocol.range)
    ~(env : Core.Type_env.t) (sexp : Syntax.Sexp.t) : Protocol.inlay_hint list =
  match sexp with
  | Syntax.Sexp.List
      ( Syntax.Sexp.Symbol (("let" | "let*"), _)
        :: Syntax.Sexp.List (bindings, _)
        :: body,
        _ ) ->
      let binding_hints =
        List.filter_map
          (fun binding ->
            match binding with
            | Syntax.Sexp.List
                ([ Syntax.Sexp.Symbol (_name, name_span); init_expr ], _) -> (
                Core.Types.reset_tvar_counter ();
                match safe_infer_and_solve env init_expr with
                | Some ty when not (is_trivial_type init_expr ty) ->
                    let pos = position_of_span_end ~text name_span in
                    if in_range pos range then
                      Some
                        {
                          Protocol.ih_position = pos;
                          ih_label = ": " ^ Core.Types.to_string ty;
                          ih_kind = Some Protocol.IHType;
                          ih_padding_left = false;
                          ih_padding_right = false;
                        }
                    else None
                | _ -> None)
            | _ -> None)
          bindings
      in
      let child_hints =
        List.concat_map (collect_let_hints ~text ~range ~env) bindings
        @ List.concat_map (collect_let_hints ~text ~range ~env) body
      in
      binding_hints @ child_hints
  | Syntax.Sexp.List (children, _) | Syntax.Sexp.Vector (children, _) ->
      List.concat_map (collect_let_hints ~text ~range ~env) children
  | Syntax.Sexp.Cons (car, cdr, _) ->
      collect_let_hints ~text ~range ~env car
      @ collect_let_hints ~text ~range ~env cdr
  | _ -> []

let handle ~(uri : string) ~(doc_text : string) ~(range : Protocol.range) :
    (Yojson.Safe.t, Rpc.response_error) result =
  let _ = uri in
  let filename = "<inlayHint>" in
  let parse_result = Syntax.Read.parse_string ~filename doc_text in
  let check_result = Typing.Check.check_program parse_result.sexps in
  let defun_hints =
    collect_defun_hints ~text:doc_text ~range ~check_result parse_result.sexps
  in
  let let_hints =
    List.concat_map
      (collect_let_hints ~text:doc_text ~range ~env:check_result.env)
      parse_result.sexps
  in
  let all_hints = defun_hints @ let_hints in
  Ok (Protocol.inlay_hint_result_to_json (Some all_hints))
