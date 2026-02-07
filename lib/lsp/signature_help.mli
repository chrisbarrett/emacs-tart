(** Signature help provider for the LSP server.

    Provides textDocument/signatureHelp responses showing function signatures
    with the current parameter highlighted when the cursor is inside a function
    call. *)

val find_call_context :
  line:int -> col:int -> Syntax.Sexp.t list -> (string * int) option
(** [find_call_context ~line ~col sexps] finds the enclosing function call and
    argument position at the cursor. Returns [(fn_name, arg_index)] where
    [arg_index] is 0-based. Position is 0-based (LSP convention). *)

val param_to_label : Core.Types.param -> string
(** Convert a type parameter to a display label for signature help. *)

val signature_of_function_type :
  string -> Core.Types.typ -> int -> Protocol.signature_help option
(** [signature_of_function_type fn_name ty active_param] generates signature
    help for a function type, highlighting the active parameter. *)

val handle :
  config:Typing.Module_check.config ->
  uri:string ->
  doc_text:string ->
  line:int ->
  col:int ->
  (Yojson.Safe.t, Rpc.response_error) result
(** Handle a textDocument/signatureHelp request.

    Takes the module config and document context instead of the full server
    type, keeping the module decoupled from server state. *)
