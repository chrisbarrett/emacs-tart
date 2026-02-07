(** Code action generation for the LSP server.

    Provides quickfixes (missing signature annotation) and refactorings (extract
    function) triggered by textDocument/codeAction requests. *)

val type_to_sig_string : Core.Types.typ -> string
(** Convert a type to .tart signature format string. *)

val generate_defun_signature : string -> Core.Types.typ -> string
(** Generate a defun declaration string for a .tart file. *)

val collect_symbol_refs : Syntax.Sexp.t -> string list
(** Collect free symbol references in an S-expression. *)

val find_sexp_at_range :
  Protocol.range -> Syntax.Sexp.t list -> Syntax.Sexp.t option
(** Find the S-expression that best matches an LSP range. *)

val generate_extract_function_action :
  uri:string ->
  doc_text:string ->
  range_of_span:(Syntax.Location.span -> Protocol.range) ->
  sexp:Syntax.Sexp.t ->
  Protocol.code_action option
(** Generate an "Extract function" refactoring action. *)

val get_tart_path : string -> string
(** Get the sibling .tart file path for an .el file. *)

val read_tart_file : string -> string option
(** Read the content of a .tart file if it exists. *)

val generate_add_signature_action :
  name:string ->
  ty:Core.Types.typ ->
  tart_path:string ->
  tart_content:string option ->
  Protocol.code_action option
(** Generate an "Add type annotation" quickfix for a missing signature. *)

val handle :
  range_of_span:(Syntax.Location.span -> Protocol.range) ->
  config:Typing.Module_check.config ->
  uri:string ->
  doc_text:string ->
  range:Protocol.range ->
  context:Protocol.code_action_context ->
  (Yojson.Safe.t, Rpc.response_error) result
(** Handle a textDocument/codeAction request.

    Takes utility functions and document context instead of the full server
    type, keeping the module decoupled from server state. *)
