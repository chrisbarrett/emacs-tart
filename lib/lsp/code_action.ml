(** Code action generation for the LSP server.

    Provides quickfixes (missing signature annotation) and refactorings (extract
    function) triggered by textDocument/codeAction requests. *)

module Log = Tart_log.Log

(** {1 Type to Signature Format Conversion} *)

(** Convert a type to signature file format string.

    Signature format uses `(params) -> return` for functions, not `(-> (params)
    return)`. Type variables are rendered without the underscore prefix. *)
let rec type_to_sig_string (ty : Core.Types.typ) : string =
  match Core.Types.repr ty with
  | Core.Types.TVar tv -> (
      match !tv with
      | Core.Types.Unbound (id, _) -> Printf.sprintf "'t%d" id
      | Core.Types.Link _ -> failwith "repr should have followed link")
  | Core.Types.TCon name -> String.lowercase_ascii name
  | Core.Types.TApp (con, args) ->
      let con_str = type_to_sig_string con in
      Printf.sprintf "(%s %s)" con_str
        (String.concat " " (List.map type_to_sig_string args))
  | Core.Types.TArrow (params, ret) ->
      Printf.sprintf "((%s) -> %s)"
        (String.concat " " (List.map param_to_sig_string params))
        (type_to_sig_string ret)
  | Core.Types.TForall (vars, body) ->
      (* Don't render the forall wrapper - the type variables will be inferred *)
      type_to_sig_string_with_vars vars body
  | Core.Types.TUnion types ->
      Printf.sprintf "(%s)"
        (String.concat " | " (List.map type_to_sig_string types))
  | Core.Types.TTuple types ->
      Printf.sprintf "(tuple %s)"
        (String.concat " " (List.map type_to_sig_string types))
  | Core.Types.TRow { row_fields; row_var } -> (
      let fields_str =
        String.concat " "
          (List.map
             (fun (name, ty) ->
               Printf.sprintf "%s %s" name (type_to_sig_string ty))
             row_fields)
      in
      match row_var with
      | None -> Printf.sprintf "{%s}" fields_str
      | Some var ->
          Printf.sprintf "{%s & %s}" fields_str (type_to_sig_string var))
  | Core.Types.TLiteral (_, base) -> type_to_sig_string base

and param_to_sig_string = function
  | Core.Types.PPositional ty -> type_to_sig_string ty
  | Core.Types.POptional ty ->
      Printf.sprintf "&optional %s" (type_to_sig_string ty)
  | Core.Types.PRest ty -> Printf.sprintf "&rest %s" (type_to_sig_string ty)
  | Core.Types.PKey (name, ty) ->
      Printf.sprintf "&key :%s %s" name (type_to_sig_string ty)
  | Core.Types.PLiteral value -> Printf.sprintf "'%s" value

(** Format a polymorphic type with explicit type variable binders *)
and type_to_sig_string_with_vars (vars : string list) (body : Core.Types.typ) :
    string =
  let body_str =
    match Core.Types.repr body with
    | Core.Types.TArrow (params, ret) ->
        Printf.sprintf "(%s) -> %s"
          (String.concat " " (List.map param_to_sig_string params))
          (type_to_sig_string ret)
    | _ -> type_to_sig_string body
  in
  if vars = [] then body_str
  else Printf.sprintf "[%s] %s" (String.concat " " vars) body_str

(** Generate a defun signature declaration string.

    Given a function name and its type, generates: `(defun name (params) ->
    return)` or `(defun name [vars] (params) -> return)` for polymorphic
    functions *)
let generate_defun_signature (name : string) (ty : Core.Types.typ) : string =
  match Core.Types.repr ty with
  | Core.Types.TForall (vars, Core.Types.TArrow (params, ret)) ->
      Printf.sprintf "(defun %s [%s] (%s) -> %s)" name (String.concat " " vars)
        (String.concat " " (List.map param_to_sig_string params))
        (type_to_sig_string ret)
  | Core.Types.TArrow (params, ret) ->
      Printf.sprintf "(defun %s (%s) -> %s)" name
        (String.concat " " (List.map param_to_sig_string params))
        (type_to_sig_string ret)
  | _ ->
      (* Not a function type - shouldn't happen for defuns but handle gracefully *)
      Printf.sprintf "(defvar %s %s)" name (type_to_sig_string ty)

(** {1 Extract Function Refactoring} *)

(** Elisp special forms that introduce variable bindings *)
let binding_forms =
  [
    "let";
    "let*";
    "letrec";
    "if-let";
    "when-let";
    "if-let*";
    "when-let*";
    "pcase-let";
    "pcase-let*";
    "cl-destructuring-bind";
    "lambda";
    "defun";
    "defsubst";
    "defmacro";
    "cl-defun";
    "cl-defsubst";
    "cl-defmacro";
  ]

(** Check if a symbol is a binding form *)
let is_binding_form (name : string) : bool = List.mem name binding_forms

(** Check if a symbol is a builtin that shouldn't be extracted as a parameter *)
let is_builtin_symbol (name : string) : bool =
  (* Common elisp builtins that should not become parameters *)
  let builtins =
    [
      (* Control flow *)
      "if";
      "when";
      "unless";
      "cond";
      "and";
      "or";
      "not";
      "progn";
      "prog1";
      "prog2";
      "while";
      "dolist";
      "dotimes";
      (* Binding forms *)
      "let";
      "let*";
      "lambda";
      "defun";
      "defvar";
      "defconst";
      "setq";
      "setf";
      (* List operations *)
      "car";
      "cdr";
      "cons";
      "list";
      "append";
      "nth";
      "nthcdr";
      "length";
      "mapcar";
      "mapc";
      "member";
      "assoc";
      "assq";
      (* Arithmetic *)
      "+";
      "-";
      "*";
      "/";
      "mod";
      "1+";
      "1-";
      "max";
      "min";
      "abs";
      (* Comparison *)
      "<";
      ">";
      "<=";
      ">=";
      "=";
      "/=";
      "eq";
      "eql";
      "equal";
      "string=";
      (* String operations *)
      "concat";
      "substring";
      "string-to-number";
      "number-to-string";
      "format";
      "message";
      (* Type predicates *)
      "null";
      "consp";
      "listp";
      "stringp";
      "numberp";
      "integerp";
      "symbolp";
      "functionp";
      (* Quoting *)
      "quote";
      "function";
      "backquote";
      (* Special *)
      "t";
      "nil";
      "error";
      "signal";
      "funcall";
      "apply";
    ]
  in
  List.mem name builtins || is_binding_form name

(** Extract bound variables from a binding clause.

    For `let`/`let*`, the clause is either `(var value)` or just `var`. *)
let extract_bound_from_clause (clause : Syntax.Sexp.t) : string list =
  match clause with
  | Syntax.Sexp.List (Syntax.Sexp.Symbol (name, _) :: _, _) -> [ name ]
  | Syntax.Sexp.Symbol (name, _) -> [ name ]
  | _ -> []

(** Extract bound variables from a let-style binding list.

    Given `((x 1) (y 2) z)`, returns `["x"; "y"; "z"]`. *)
let extract_bindings_from_let (bindings : Syntax.Sexp.t) : string list =
  match bindings with
  | Syntax.Sexp.List (clauses, _) ->
      List.concat_map extract_bound_from_clause clauses
  | _ -> []

(** Extract parameter names from a lambda/defun argument list.

    Given `(x y &optional z)`, returns `["x"; "y"; "z"]`. *)
let extract_params (args : Syntax.Sexp.t) : string list =
  let rec collect = function
    | [] -> []
    | Syntax.Sexp.Symbol ("&optional", _) :: rest -> collect rest
    | Syntax.Sexp.Symbol ("&rest", _) :: rest -> collect rest
    | Syntax.Sexp.Symbol ("&key", _) :: rest -> collect rest
    | Syntax.Sexp.Symbol (name, _) :: rest -> name :: collect rest
    | _ :: rest -> collect rest
  in
  match args with Syntax.Sexp.List (elems, _) -> collect elems | _ -> []

(** Collect all symbol references in an S-expression.

    Returns the set of symbol names that appear in the expression, excluding:
    - Symbols in function position of special forms
    - Symbols bound by let/lambda/etc. within the expression *)
let collect_symbol_refs (sexp : Syntax.Sexp.t) : string list =
  let rec collect ~(bound : string list) (expr : Syntax.Sexp.t) : string list =
    match expr with
    | Syntax.Sexp.Symbol (name, _) ->
        if List.mem name bound || is_builtin_symbol name then [] else [ name ]
    | Syntax.Sexp.List
        (Syntax.Sexp.Symbol (("let" | "let*"), _) :: bindings :: body, _) ->
        (* Collect refs from binding values (before they're bound) *)
        let binding_refs = collect_from_let_bindings ~bound bindings in
        (* Then collect from body with new bindings in scope *)
        let new_bound = extract_bindings_from_let bindings @ bound in
        let body_refs = List.concat_map (collect ~bound:new_bound) body in
        binding_refs @ body_refs
    | Syntax.Sexp.List
        ( Syntax.Sexp.Symbol (("lambda" | "defun" | "defsubst" | "defmacro"), _)
          :: args :: body,
          _ ) ->
        (* For lambda/defun, first arg is params (skip if symbol for defun name) *)
        let params, actual_body =
          match args with
          | Syntax.Sexp.Symbol _ ->
              (* defun name - skip it, next is args *)
              ( (match body with [] -> [] | h :: _ -> extract_params h),
                match body with [] | [ _ ] -> [] | _ :: t -> t )
          | _ -> (extract_params args, body)
        in
        let new_bound = params @ bound in
        List.concat_map (collect ~bound:new_bound) actual_body
    | Syntax.Sexp.List (Syntax.Sexp.Symbol (form, _) :: _, _)
      when is_binding_form form ->
        (* Other binding forms - conservatively just collect from all children *)
        collect_from_children ~bound expr
    | Syntax.Sexp.List (head :: args, _) ->
        (* Regular function call - collect from head (unless it's a symbol being called)
           and all arguments *)
        let head_refs =
          match head with
          | Syntax.Sexp.Symbol _ ->
              [] (* Don't treat function name as free var *)
          | _ -> collect ~bound head
        in
        head_refs @ List.concat_map (collect ~bound) args
    | Syntax.Sexp.List ([], _) -> []
    | Syntax.Sexp.Vector (elems, _) -> List.concat_map (collect ~bound) elems
    | Syntax.Sexp.Curly (elems, _) -> List.concat_map (collect ~bound) elems
    | Syntax.Sexp.Cons (car, cdr, _) -> collect ~bound car @ collect ~bound cdr
    | Syntax.Sexp.Int _ | Syntax.Sexp.Float _ | Syntax.Sexp.String _
    | Syntax.Sexp.Keyword _ | Syntax.Sexp.Char _ | Syntax.Sexp.Error _ ->
        []
  and collect_from_let_bindings ~(bound : string list)
      (bindings : Syntax.Sexp.t) : string list =
    match bindings with
    | Syntax.Sexp.List (clauses, _) ->
        List.concat_map
          (fun clause ->
            match clause with
            | Syntax.Sexp.List (_ :: value :: _, _) -> collect ~bound value
            | _ -> [])
          clauses
    | _ -> []
  and collect_from_children ~(bound : string list) (expr : Syntax.Sexp.t) :
      string list =
    match expr with
    | Syntax.Sexp.List (elems, _) -> List.concat_map (collect ~bound) elems
    | Syntax.Sexp.Vector (elems, _) -> List.concat_map (collect ~bound) elems
    | Syntax.Sexp.Cons (car, cdr, _) -> collect ~bound car @ collect ~bound cdr
    | _ -> []
  in
  collect ~bound:[] sexp |> List.sort_uniq String.compare

(** Find the S-expression that best matches a given LSP range.

    The range is 0-based (LSP convention). Returns the first sexp whose span
    contains the start position of the range, provided the selection is
    non-trivial (not just a cursor position). *)
let find_sexp_at_range (range : Protocol.range) (sexps : Syntax.Sexp.t list) :
    Syntax.Sexp.t option =
  (* Only offer extract if there's a real selection (not just a cursor) *)
  if
    range.start.line = range.end_.line
    && range.start.character = range.end_.character
  then None
  else
    (* Find sexp at start of range *)
    Syntax.Sexp.find_at_position_in_forms ~line:range.start.line
      ~col:range.start.character sexps

(** Generate "Extract function" refactoring action.

    Creates a workspace edit that: 1. Inserts a new defun before the current
    top-level form 2. Replaces the selection with a call to the new function *)
let generate_extract_function_action ~(uri : string) ~(doc_text : string)
    ~(range_of_span : Syntax.Location.span -> Protocol.range)
    ~(sexp : Syntax.Sexp.t) : Protocol.code_action option =
  let free_vars = collect_symbol_refs sexp in
  let fn_name = "extracted-fn" in
  let span = Syntax.Sexp.span_of sexp in

  (* Generate the new function definition *)
  let params_str =
    if free_vars = [] then "()" else "(" ^ String.concat " " free_vars ^ ")"
  in
  let body_str = Syntax.Sexp.to_string sexp in
  let new_defun =
    Printf.sprintf "(defun %s %s\n  %s)\n\n" fn_name params_str body_str
  in

  (* Generate the replacement call *)
  let call_str =
    if free_vars = [] then Printf.sprintf "(%s)" fn_name
    else Printf.sprintf "(%s %s)" fn_name (String.concat " " free_vars)
  in

  (* Find position to insert new defun - at start of line containing the selection *)
  let insert_line = span.start_pos.line - 1 in
  (* Convert to 0-based *)
  let insert_pos : Protocol.position = { line = insert_line; character = 0 } in

  (* Convert span to LSP range for replacement *)
  let replace_range = range_of_span span in

  (* Create edits:
     1. Insert new defun at start of the line
     2. Replace selected expression with function call *)
  let insert_edit : Protocol.text_edit =
    {
      te_range = { start = insert_pos; end_ = insert_pos };
      new_text = new_defun;
    }
  in
  let replace_edit : Protocol.text_edit =
    { te_range = replace_range; new_text = call_str }
  in

  (* Both edits target the same document *)
  let _ = doc_text in
  let doc_edit : Protocol.text_document_edit =
    { tde_uri = uri; tde_version = None; edits = [ insert_edit; replace_edit ] }
  in
  let workspace_edit : Protocol.workspace_edit =
    { document_changes = [ doc_edit ] }
  in

  Some
    {
      Protocol.ca_title = "Extract function";
      ca_kind = Some Protocol.RefactorExtract;
      ca_diagnostics = [];
      ca_is_preferred = false;
      ca_edit = Some workspace_edit;
    }

(** {1 Code Action Generation} *)

(** Get the sibling .tart file path for an .el file *)
let get_tart_path (el_filename : string) : string =
  let dir = Filename.dirname el_filename in
  let basename = Filename.basename el_filename in
  let module_name =
    if Filename.check_suffix basename ".el" then
      Filename.chop_suffix basename ".el"
    else basename
  in
  Filename.concat dir (module_name ^ ".tart")

(** Read the content of a .tart file if it exists, or return empty string *)
let read_tart_file (tart_path : string) : string option =
  if Sys.file_exists tart_path then
    try
      let ic = In_channel.open_text tart_path in
      let content = In_channel.input_all ic in
      In_channel.close ic;
      Some content
    with _ -> None
  else None

(** Generate "Add type annotation" quickfix for a missing signature warning.

    Creates a workspace edit that appends the function signature to the .tart
    file. *)
let generate_add_signature_action ~(name : string) ~(ty : Core.Types.typ)
    ~(tart_path : string) ~(tart_content : string option) :
    Protocol.code_action option =
  let signature_str = generate_defun_signature name ty in
  let tart_uri = "file://" ^ tart_path in

  (* Calculate the position to insert - append at end of file *)
  let insert_pos, new_text =
    match tart_content with
    | Some content ->
        let lines = String.split_on_char '\n' content in
        let line_count = List.length lines in
        (* Insert on a new line at the end *)
        let last_line_len =
          match List.rev lines with [] -> 0 | last :: _ -> String.length last
        in
        ( { Protocol.line = line_count - 1; character = last_line_len },
          "\n" ^ signature_str )
    | None ->
        (* File doesn't exist - create with just the signature *)
        ({ Protocol.line = 0; character = 0 }, signature_str ^ "\n")
  in

  let edit : Protocol.text_edit =
    { te_range = { start = insert_pos; end_ = insert_pos }; new_text }
  in
  let doc_edit : Protocol.text_document_edit =
    { tde_uri = tart_uri; tde_version = None; edits = [ edit ] }
  in
  let workspace_edit : Protocol.workspace_edit =
    { document_changes = [ doc_edit ] }
  in

  Some
    {
      Protocol.ca_title =
        Printf.sprintf "Add signature for `%s` to .tart file" name;
      ca_kind = Some Protocol.QuickFix;
      ca_diagnostics = [];
      ca_is_preferred = true;
      ca_edit = Some workspace_edit;
    }

(** {1 Version Constraint Code Actions} *)

(** Extract the required version string from an E0900 diagnostic message.

    Matches the pattern: [`` `name` requires Emacs X.Y+ ``] and returns ["X.Y"].
*)
let extract_required_version (message : string) : string option =
  (* Pattern: `name` requires Emacs X.Y+ *)
  match String.split_on_char '+' message with
  | [] -> None
  | parts ->
      let before_plus = List.hd parts in
      (* Find "Emacs " and take the version after it *)
      let prefix = "Emacs " in
      let prefix_len = String.length prefix in
      let rec find_emacs i =
        if i + prefix_len > String.length before_plus then None
        else if String.sub before_plus i prefix_len = prefix then
          Some
            (String.sub before_plus (i + prefix_len)
               (String.length before_plus - i - prefix_len))
        else find_emacs (i + 1)
      in
      find_emacs 0

(** Extract the removed-after version from an E0901 diagnostic message.

    Matches the pattern: [`` `name` was removed after Emacs X.Y ``] and returns
    ["X.Y"]. *)
let extract_removed_version (message : string) : string option =
  let prefix = "Emacs " in
  let prefix_len = String.length prefix in
  let rec find_emacs i =
    if i + prefix_len > String.length message then None
    else if String.sub message i prefix_len = prefix then
      Some
        (String.sub message (i + prefix_len)
           (String.length message - i - prefix_len))
    else find_emacs (i + 1)
  in
  find_emacs 0

(** Extract the function name from a version diagnostic message.

    Matches the pattern: [`` `name` requires... ``] or
    [`` `name` was removed... ``] and returns [name]. *)
let extract_function_name (message : string) : string option =
  if String.length message > 0 && message.[0] = '`' then
    match String.index_opt message '`' with
    | Some 0 -> (
        match String.index_from_opt message 1 '`' with
        | Some end_idx -> Some (String.sub message 1 (end_idx - 1))
        | None -> None)
    | _ -> None
  else None

(** Check whether an LSP diagnostic from the client context matches a version
    diagnostic produced by the type-checker.

    Diagnostics match when they share the same error code and their ranges have
    the same start line. *)
let diagnostics_match (client_diag : Protocol.diagnostic)
    (version_diag : Protocol.diagnostic) : bool =
  client_diag.code = version_diag.code
  && client_diag.range.start.line = version_diag.range.start.line

(** Generate version constraint code actions for diagnostics in the request
    context.

    Filters [context.cac_diagnostics] for E0900/E0901 codes and matches them
    against the type-checker's [version_diagnostics] to produce quickfix
    actions. *)
let generate_version_actions
    ~(range_of_span : Syntax.Location.span -> Protocol.range)
    ~(context : Protocol.code_action_context)
    ~(version_diagnostics : Typing.Diagnostic.t list) :
    Protocol.code_action list =
  (* Convert internal version diagnostics to Protocol diagnostics for matching *)
  let proto_version_diags =
    List.filter_map
      (fun (d : Typing.Diagnostic.t) ->
        match d.code with
        | Some
            (Typing.Diagnostic.VersionTooLow | Typing.Diagnostic.VersionTooHigh)
          ->
            let range = range_of_span d.span in
            let code =
              Option.map Typing.Diagnostic.error_code_to_string d.code
            in
            Some
              ( d,
                {
                  Protocol.range;
                  severity = None;
                  code;
                  message = d.message;
                  source = None;
                  related_information = [];
                } )
        | _ -> None)
      version_diagnostics
  in
  (* For each client diagnostic with a version error code, find the matching
     internal diagnostic and generate actions *)
  List.concat_map
    (fun (client_diag : Protocol.diagnostic) ->
      match client_diag.code with
      | Some ("E0900" | "E0901") -> (
          match
            List.find_opt
              (fun (_, proto_diag) -> diagnostics_match client_diag proto_diag)
              proto_version_diags
          with
          | Some (internal_diag, _) ->
              Log.debug "Matched version diagnostic: %s" internal_diag.message;
              (* Actions will be generated in Tasks 7-9 *)
              let (_ : Typing.Diagnostic.t) = internal_diag in
              []
          | None ->
              Log.debug "No matching internal diagnostic for %s"
                client_diag.message;
              [])
      | _ -> [])
    context.cac_diagnostics

(** {1 Code Action Generation} *)

(** Handle textDocument/codeAction request.

    Returns code actions available for the given range and context. Generates
    quickfixes for:
    - Missing signature warnings: offers to add the function signature to the
      .tart file
    - Version constraint violations: offers to bump version or add feature guard
*)
let handle ~(range_of_span : Syntax.Location.span -> Protocol.range)
    ~(config : Typing.Module_check.config) ~(uri : string) ~(doc_text : string)
    ~(range : Protocol.range) ~(context : Protocol.code_action_context) :
    (Yojson.Safe.t, Rpc.response_error) result =
  let filename =
    if String.length uri > 7 && String.sub uri 0 7 = "file://" then
      String.sub uri 7 (String.length uri - 7)
    else uri
  in
  let parse_result = Syntax.Read.parse_string ~filename doc_text in

  if parse_result.sexps = [] then
    Ok (Protocol.code_action_result_to_json (Some []))
  else
    (* Type-check to get missing signature warnings *)
    let check_result =
      Typing.Module_check.check_module ~config ~filename parse_result.sexps
    in

    (* Generate code actions for missing signatures *)
    let tart_path = get_tart_path filename in
    let tart_content = read_tart_file tart_path in

    let missing_sig_actions =
      List.filter_map
        (fun (warn : Typing.Module_check.missing_signature_warning) ->
          (* Check if this warning's range overlaps with the request range *)
          let warn_range = range_of_span warn.span in
          let overlaps =
            warn_range.start.line <= range.end_.line
            && warn_range.end_.line >= range.start.line
          in
          if overlaps then (
            Log.debug "Found missing signature for: %s" warn.name;
            (* Look up the inferred type from the environment *)
            match Core.Type_env.lookup warn.name check_result.final_env with
            | Some scheme ->
                let ty =
                  match scheme with
                  | Core.Type_env.Mono t -> t
                  | Core.Type_env.Poly (vars, t) -> Core.Types.TForall (vars, t)
                in
                generate_add_signature_action ~name:warn.name ~ty ~tart_path
                  ~tart_content
            | None ->
                Log.debug "No type found for: %s" warn.name;
                None)
          else None)
        check_result.missing_signature_warnings
    in

    (* Generate "Extract function" refactoring if selection covers a sexp *)
    let extract_actions =
      match find_sexp_at_range range parse_result.sexps with
      | Some sexp -> (
          Log.debug "Found sexp for extraction: %s" (Syntax.Sexp.to_string sexp);
          match
            generate_extract_function_action ~uri ~doc_text ~range_of_span ~sexp
          with
          | Some action -> [ action ]
          | None -> [])
      | None -> []
    in

    (* Generate version constraint code actions from diagnostic context *)
    let version_actions =
      generate_version_actions ~range_of_span ~context
        ~version_diagnostics:check_result.version_diagnostics
    in

    let all_actions = missing_sig_actions @ extract_actions @ version_actions in
    Log.debug "Generated %d code actions" (List.length all_actions);
    Ok (Protocol.code_action_result_to_json (Some all_actions))
