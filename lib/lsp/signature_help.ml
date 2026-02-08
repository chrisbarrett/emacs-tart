(** Signature help provider for the LSP server.

    Provides textDocument/signatureHelp responses showing function signatures
    with the current parameter highlighted when the cursor is inside a function
    call. *)

module Log = Tart_log.Log

(** Find the enclosing function call and the argument position at the cursor.

    Given a position, finds the innermost (fn arg1 arg2 ...) list containing
    that position and returns the function name and the 0-based argument index
    where the cursor is located.

    The position is 0-based (LSP convention). Line numbers in spans are 1-based,
    so we adjust when comparing. *)
let find_call_context ~(line : int) ~(col : int) (sexps : Syntax.Sexp.t list) :
    (string * int) option =
  (* Find all list forms that contain the position, keeping track of depth *)
  let rec find_in_sexp (sexp : Syntax.Sexp.t) : (string * int) option =
    let span = Syntax.Sexp.span_of sexp in
    (* LSP position is 0-based; span line is 1-based *)
    if not (Syntax.Location.contains_position span ~line ~col) then None
    else
      match sexp with
      | Syntax.Sexp.List (Syntax.Sexp.Symbol (fn_name, _) :: args, _) ->
          (* This is a function call - check if cursor is in args *)
          (* First, try to find a more nested call in the args *)
          let nested_result = List.find_map find_in_sexp args in
          if Option.is_some nested_result then nested_result
          else
            (* Cursor is in this call's args - find which argument position *)
            let arg_index = ref 0 in
            let found = ref false in
            List.iteri
              (fun i arg ->
                if not !found then
                  let arg_span = Syntax.Sexp.span_of arg in
                  if Syntax.Location.contains_position arg_span ~line ~col then (
                    arg_index := i;
                    found := true)
                  else if
                    (* Check if position is between args (whitespace) *)
                    (* If the cursor is after this arg but before the next, it's for the next arg *)
                    arg_span.end_pos.line - 1 < line
                    || arg_span.end_pos.line - 1 = line
                       && arg_span.end_pos.col <= col
                  then arg_index := i + 1)
              args;
            Some (fn_name, !arg_index)
      | Syntax.Sexp.List (elems, _) ->
          (* Non-function-call list, search inside *)
          List.find_map find_in_sexp elems
      | Syntax.Sexp.Vector (elems, _) -> List.find_map find_in_sexp elems
      | Syntax.Sexp.Cons (car, cdr, _) -> (
          match find_in_sexp car with
          | Some _ as r -> r
          | None -> find_in_sexp cdr)
      | _ -> None
  in
  List.find_map find_in_sexp sexps

(** Convert a type parameter to a signature help parameter label.

    Returns the label string for displaying the parameter. *)
let param_to_label (param : Core.Types.param) : string =
  match param with
  | Core.Types.PPositional ty -> Core.Types.to_string ty
  | Core.Types.POptional ty ->
      Printf.sprintf "&optional %s" (Core.Types.to_string ty)
  | Core.Types.PRest ty -> Printf.sprintf "&rest %s" (Core.Types.to_string ty)
  | Core.Types.PKey (name, ty) ->
      Printf.sprintf ":%s %s" name (Core.Types.to_string ty)
  | Core.Types.PLiteral value -> Printf.sprintf "'%s" value

(** Generate signature help for a function type.

    Creates signature information from a function name and its type,
    highlighting the active parameter. *)
let signature_of_function_type (fn_name : string) (ty : Core.Types.typ)
    (active_param : int) : Protocol.signature_help option =
  (* Unwrap TForall to get the arrow type *)
  let inner_ty =
    match Core.Types.repr ty with
    | Core.Types.TForall (_, body) -> Core.Types.repr body
    | other -> other
  in
  match inner_ty with
  | Core.Types.TArrow (params, ret) ->
      (* Build parameter list *)
      let parameters =
        List.map
          (fun param : Protocol.parameter_information ->
            { pi_label = param_to_label param; pi_documentation = None })
          params
      in
      (* Build the full signature label: fn_name :: (params) -> return *)
      let params_str = String.concat " " (List.map param_to_label params) in
      let ret_str = Core.Types.to_string ret in
      let label = Printf.sprintf "(%s %s) → %s" fn_name params_str ret_str in
      (* Clamp active parameter to valid range *)
      let active =
        if active_param >= 0 && active_param < List.length params then
          Some active_param
        else if List.length params > 0 then
          (* Check if last param is rest - if so, keep highlighting it *)
          match List.rev params with
          | Core.Types.PRest _ :: _ -> Some (List.length params - 1)
          | _ -> None
        else None
      in
      Some
        {
          Protocol.sh_signatures =
            [
              {
                Protocol.si_label = label;
                si_documentation = None;
                si_parameters = parameters;
                si_active_parameter = active;
              };
            ];
          sh_active_signature = Some 0;
          sh_active_parameter = active;
        }
  | _ -> None

(** Handle textDocument/signatureHelp request.

    Returns signature help when the cursor is inside a function call, showing
    the function's signature with the current parameter highlighted.

    Takes the module config and document context instead of the full server
    type, keeping the module decoupled from server state. *)
let handle ~(config : Typing.Module_check.config) ~(uri : string)
    ~(doc_text : string) ~(line : int) ~(col : int) :
    (Yojson.Safe.t, Rpc.response_error) result =
  let filename = Uri.to_filename uri in
  let parse_result = Syntax.Read.parse_string ~filename doc_text in
  if parse_result.sexps = [] then (
    Log.debug "No S-expressions parsed";
    Ok (Protocol.signature_help_result_to_json None))
  else
    (* Find the enclosing function call and argument position *)
    match find_call_context ~line ~col parse_result.sexps with
    | None ->
        Log.debug "No function call at position";
        Ok (Protocol.signature_help_result_to_json None)
    | Some (fn_name, arg_index) -> (
        Log.debug "Found call to '%s' at arg position %d" fn_name arg_index;
        (* Type-check to get the environment with function types *)
        let check_result =
          Typing.Module_check.check_module ~config ~filename parse_result.sexps
        in
        (* Look up the function's type in the environment *)
        match Core.Type_env.lookup fn_name check_result.final_env with
        | None ->
            Log.debug "Function '%s' not found in environment" fn_name;
            Ok (Protocol.signature_help_result_to_json None)
        | Some scheme ->
            let ty =
              match scheme with
              | Core.Type_env.Mono t -> t
              | Core.Type_env.Poly (vars, t) -> Core.Types.TForall (vars, t)
            in
            Log.debug "Function type: %s" (Core.Types.to_string ty);
            let result = signature_of_function_type fn_name ty arg_index in
            Ok (Protocol.signature_help_result_to_json result))
