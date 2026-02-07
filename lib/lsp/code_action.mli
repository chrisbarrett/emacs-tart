(** Code action generation for the LSP server.

    Provides quickfixes (missing signature annotation, version constraint
    violations) and refactorings (extract function) triggered by
    textDocument/codeAction requests. *)

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

(** {1 Version Constraint Helpers} *)

val extract_required_version : string -> string option
(** Extract the required version string from an E0900 diagnostic message.

    Returns the version from messages like [`` `name` requires Emacs X.Y+ ``].
*)

val extract_removed_version : string -> string option
(** Extract the removed-after version from an E0901 diagnostic message.

    Returns the version from messages like
    [`` `name` was removed after Emacs X.Y ``]. *)

val extract_function_name : string -> string option
(** Extract the function name from a version diagnostic message.

    Returns the name from messages like [`` `name` requires... ``]. *)

val diagnostics_match : Protocol.diagnostic -> Protocol.diagnostic -> bool
(** Check whether two diagnostics match by error code and start line. *)

val find_package_requires_version : string -> (int * int * int) option
(** [find_package_requires_version doc_text] locates the emacs version string
    inside the Package-Requires header.

    Returns [Some (line, col_start, col_end)] where [line] is the 0-based line
    number and [col_start]/[col_end] are byte offsets within the line delimiting
    the version string (excluding quotes). Returns [None] if no header is found.
*)

val generate_bump_version_action :
  uri:string ->
  doc_text:string ->
  required_version:string ->
  diagnostic:Protocol.diagnostic ->
  Protocol.code_action option
(** Generate a "Bump minimum Emacs version to X.Y" quickfix for E0900.

    Edits the Package-Requires header to raise the Emacs version floor. *)

val extract_text_at_range : string -> Protocol.range -> string option
(** [extract_text_at_range doc_text range] extracts the substring of [doc_text]
    covered by the 0-based LSP [range]. Returns [None] if the range is out of
    bounds. *)

val generate_wrap_guard_action :
  uri:string ->
  doc_text:string ->
  fn_name:string ->
  diagnostic:Protocol.diagnostic ->
  Protocol.code_action option
(** Generate a "Wrap in feature guard" quickfix for E0900.

    Wraps the offending call in [(when (fboundp 'fn) ...)] to guard against the
    function not being available in older Emacs versions. *)

val generate_version_actions :
  uri:string ->
  doc_text:string ->
  range_of_span:(Syntax.Location.span -> Protocol.range) ->
  context:Protocol.code_action_context ->
  version_diagnostics:Typing.Diagnostic.t list ->
  Protocol.code_action list
(** Generate version constraint code actions for diagnostics in the request
    context.

    Filters the context's diagnostics for E0900/E0901 codes and matches them
    against the type-checker's version diagnostics. *)

(** {1 Request Handler} *)

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
