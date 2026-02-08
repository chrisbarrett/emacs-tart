(** LSP Protocol message types.

    Defines request/response types for the Language Server Protocol. Phase 1:
    initialize, initialized, shutdown. *)

(** {1 Position Encoding} *)

(** Position encoding negotiated during initialization *)
type position_encoding = UTF16 | UTF32

(** {1 Initialize Request} *)

type general_client_capabilities = { position_encodings : string list option }
(** General client capabilities, including position encoding support *)

type client_capabilities = {
  text_document : text_document_client_capabilities option;
  general : general_client_capabilities option;
}
(** Client capabilities *)

and text_document_client_capabilities = {
  synchronization : text_document_sync_client_capabilities option;
}

and text_document_sync_client_capabilities = {
  dynamic_registration : bool option;
}

type initialize_params = {
  process_id : int option;
  root_uri : string option;
  capabilities : client_capabilities;
}
(** Initialize request params *)

(** Text document sync kind *)
type text_document_sync_kind = None_ | Full | Incremental

type text_document_sync_options = {
  open_close : bool;
  change : text_document_sync_kind;
  save : bool;
}
(** Text document sync options *)

type rename_options = { prepare_provider : bool }
(** Rename options with optional prepare support *)

type server_capabilities = {
  text_document_sync : text_document_sync_options option;
  hover_provider : bool;
  definition_provider : bool;
  references_provider : bool;
  code_action_provider : bool;
  document_symbol_provider : bool;
  completion_provider : bool;
  signature_help_provider : bool;
  rename_provider : rename_options option;
  folding_range_provider : bool;
  semantic_tokens_provider : bool;
  inlay_hint_provider : bool;
  type_definition_provider : bool;
  workspace_symbol_provider : bool;
}
(** Server capabilities *)

type initialize_result = {
  capabilities : server_capabilities;
  position_encoding : position_encoding;
}
(** Initialize result *)

val negotiate_position_encoding : client_capabilities -> position_encoding
(** Negotiate position encoding from client capabilities.

    Prefers UTF-32 if advertised by the client (no conversion needed for
    byte-indexed columns), otherwise defaults to UTF-16 per the LSP spec. *)

val position_encoding_to_string : position_encoding -> string
(** Convert a position encoding to the LSP protocol string. *)

(** {1 JSON Parsing} *)

val parse_initialize_params : Yojson.Safe.t -> initialize_params
(** Parse initialize params from JSON *)

(** {1 JSON Encoding} *)

val initialize_result_to_json : initialize_result -> Yojson.Safe.t
(** Encode initialize result to JSON *)

(** {1 Server Info} *)

type server_info = { name : string; version : string option }
(** Server info included in initialize response *)

val initialize_response_to_json :
  result:initialize_result -> server_info:server_info -> Yojson.Safe.t
(** Full initialize response with server info *)

(** {1 Diagnostics} *)

(** LSP diagnostic severity *)
type diagnostic_severity = Error | Warning | Information | Hint

type position = { line : int; character : int }
(** A position in a document (0-based line and character) *)

type range = { start : position; end_ : position }
(** A range in a document *)

type location = { uri : string; range : range }
(** A location in a document (uri + range) *)

type diagnostic_related_information = { location : location; message : string }
(** Related information for a diagnostic.

    Used to point to related code that helps explain the error, such as where an
    expected type originated or the other branch in a branch type mismatch. *)

type diagnostic = {
  range : range;
  severity : diagnostic_severity option;
  code : string option;
  message : string;
  source : string option;
  related_information : diagnostic_related_information list;
}
(** A diagnostic represents a compiler error or warning *)

type publish_diagnostics_params = {
  uri : string;
  version : int option;
  diagnostics : diagnostic list;
}
(** Parameters for textDocument/publishDiagnostics notification *)

val diagnostic_equal : diagnostic -> diagnostic -> bool
(** Structural equality for diagnostics.

    Compares range, severity, code, message, source, and related_information. *)

val diagnostics_equal : diagnostic list -> diagnostic list -> bool
(** Structural equality for diagnostic lists (order-sensitive). *)

val publish_diagnostics_params_to_json :
  publish_diagnostics_params -> Yojson.Safe.t
(** Encode publishDiagnostics params to JSON *)

(** {1 Hover} *)

(** Markup content kind *)
type markup_kind = PlainText | Markdown

type markup_content = { kind : markup_kind; value : string }
(** Markup content for hover *)

type hover = { contents : markup_content; range : range option }
(** Hover result *)

type hover_params = { text_document : string; (* URI *) position : position }
(** Hover params *)

val parse_hover_params : Yojson.Safe.t -> hover_params
(** Parse hover params from JSON *)

val hover_to_json : hover -> Yojson.Safe.t
(** Encode hover result to JSON *)

(** {1 Go to Definition} *)

type definition_params = { def_text_document : string; def_position : position }
(** Definition request params *)

val parse_definition_params : Yojson.Safe.t -> definition_params
(** Parse definition params from JSON *)

val parse_type_definition_params : Yojson.Safe.t -> definition_params
(** Parse type definition params from JSON (same shape as definition). *)

(** Definition result *)
type definition_result =
  | DefLocation of location  (** Single definition location *)
  | DefLocations of location list  (** Multiple definition locations *)
  | DefNull  (** No definition found *)

val definition_result_to_json : definition_result -> Yojson.Safe.t
(** Encode definition result to JSON *)

(** {1 Find References} *)

type references_params = {
  ref_text_document : string;
  ref_position : position;
  include_declaration : bool;
}
(** References request params *)

val parse_references_params : Yojson.Safe.t -> references_params
(** Parse references params from JSON *)

type references_result = location list option
(** References result *)

val references_result_to_json : references_result -> Yojson.Safe.t
(** Encode references result to JSON *)

(** {1 Code Actions} *)

(** Code action kind - categorizes the type of action *)
type code_action_kind =
  | QuickFix
  | Refactor
  | RefactorExtract
  | RefactorInline
  | RefactorRewrite
  | Source
  | SourceOrganizeImports

val code_action_kind_to_string : code_action_kind -> string
(** Convert code action kind to LSP string *)

type text_edit = { te_range : range; new_text : string }
(** Text edit for a document change *)

type text_document_edit = {
  tde_uri : string;
  tde_version : int option;
  edits : text_edit list;
}
(** Document changes within a workspace edit *)

type workspace_edit = { document_changes : text_document_edit list }
(** Workspace edit - changes across multiple documents *)

type code_action = {
  ca_title : string;
  ca_kind : code_action_kind option;
  ca_diagnostics : diagnostic list;
  ca_is_preferred : bool;
  ca_edit : workspace_edit option;
}
(** A code action represents a change that can be performed in code *)

type code_action_context = {
  cac_diagnostics : diagnostic list;
  cac_only : code_action_kind list option;
}
(** Code action context sent with the request *)

type code_action_params = {
  ca_text_document : string;
  ca_range : range;
  ca_context : code_action_context;
}
(** Code action request params *)

val parse_code_action_params : Yojson.Safe.t -> code_action_params
(** Parse code action params from JSON *)

val code_action_to_json : code_action -> Yojson.Safe.t
(** Encode a code action to JSON *)

type code_action_result = code_action list option
(** Code action result is a list of actions or null *)

val code_action_result_to_json : code_action_result -> Yojson.Safe.t
(** Encode code action result to JSON *)

(** {1 Document Symbols} *)

(** Symbol kind as defined by LSP *)
type symbol_kind =
  | SKFile
  | SKModule
  | SKNamespace
  | SKPackage
  | SKClass
  | SKMethod
  | SKProperty
  | SKField
  | SKConstructor
  | SKEnum
  | SKInterface
  | SKFunction
  | SKVariable
  | SKConstant
  | SKString
  | SKNumber
  | SKBoolean
  | SKArray
  | SKObject
  | SKKey
  | SKNull
  | SKEnumMember
  | SKStruct
  | SKEvent
  | SKOperator
  | SKTypeParameter

type document_symbol = {
  ds_name : string;
  ds_detail : string option;
  ds_kind : symbol_kind;
  ds_range : range;
  ds_selection_range : range;
  ds_children : document_symbol list;
}
(** Document symbol with hierarchical structure *)

type document_symbol_params = { dsp_text_document : string }
(** Document symbol request params *)

val parse_document_symbol_params : Yojson.Safe.t -> document_symbol_params
(** Parse document symbol params from JSON *)

type document_symbol_result = document_symbol list option
(** Document symbol result *)

val document_symbol_result_to_json : document_symbol_result -> Yojson.Safe.t
(** Encode document symbol result to JSON *)

(** {1 Completion} *)

(** Completion item kind as defined by LSP *)
type completion_item_kind =
  | CIKText
  | CIKMethod
  | CIKFunction
  | CIKConstructor
  | CIKField
  | CIKVariable
  | CIKClass
  | CIKInterface
  | CIKModule
  | CIKProperty
  | CIKUnit
  | CIKValue
  | CIKEnum
  | CIKKeyword
  | CIKSnippet
  | CIKColor
  | CIKFile
  | CIKReference
  | CIKFolder
  | CIKEnumMember
  | CIKConstant
  | CIKStruct
  | CIKEvent
  | CIKOperator
  | CIKTypeParameter

type completion_item = {
  ci_label : string;
  ci_kind : completion_item_kind option;
  ci_detail : string option;
  ci_documentation : string option;
  ci_insert_text : string option;
}
(** A completion item represents a text suggestion *)

type completion_params = { cp_text_document : string; cp_position : position }
(** Completion request params *)

val parse_completion_params : Yojson.Safe.t -> completion_params
(** Parse completion params from JSON *)

type completion_result = completion_item list option
(** Completion result is a list of items or null *)

val completion_result_to_json : completion_result -> Yojson.Safe.t
(** Encode completion result to JSON *)

(** {1 Signature Help} *)

type parameter_information = {
  pi_label : string;
  pi_documentation : string option;
}
(** Information about a single parameter of a signature *)

type signature_information = {
  si_label : string;
  si_documentation : string option;
  si_parameters : parameter_information list;
  si_active_parameter : int option;
}
(** Represents the signature of something callable *)

type signature_help = {
  sh_signatures : signature_information list;
  sh_active_signature : int option;
  sh_active_parameter : int option;
}
(** Signature help represents a list of signatures and the active one *)

type signature_help_params = {
  shp_text_document : string;
  shp_position : position;
}
(** Signature help request params *)

val parse_signature_help_params : Yojson.Safe.t -> signature_help_params
(** Parse signature help params from JSON *)

type signature_help_result = signature_help option
(** Signature help result *)

val signature_help_result_to_json : signature_help_result -> Yojson.Safe.t
(** Encode signature help result to JSON *)

(** {1 Rename} *)

type rename_params = {
  rp_text_document : string;
  rp_position : position;
  rp_new_name : string;
}
(** Rename request params *)

val parse_rename_params : Yojson.Safe.t -> rename_params
(** Parse rename params from JSON *)

type rename_result = workspace_edit option
(** Rename result is a workspace edit or null *)

val rename_result_to_json : rename_result -> Yojson.Safe.t
(** Encode rename result to JSON *)

(** {1 Prepare Rename} *)

type prepare_rename_params = {
  prp_text_document : string;
  prp_position : position;
}
(** Prepare rename request params *)

val parse_prepare_rename_params : Yojson.Safe.t -> prepare_rename_params
(** Parse prepare rename params from JSON *)

type prepare_rename_result = { prr_range : range; prr_placeholder : string }
(** Prepare rename result with range and placeholder *)

val prepare_rename_result_to_json :
  prepare_rename_result option -> Yojson.Safe.t
(** Encode prepare rename result to JSON *)

(** {1 Folding Ranges} *)

(** Folding range kind as defined by LSP *)
type folding_range_kind = FRComment | FRImports | FRRegion

type folding_range = {
  fr_start_line : int;
  fr_start_character : int option;
  fr_end_line : int;
  fr_end_character : int option;
  fr_kind : folding_range_kind option;
}
(** A folding range represents a region that can be collapsed *)

type folding_range_params = { frp_text_document : string }
(** Folding range request params *)

type folding_range_result = folding_range list option
(** Folding range result is a list of ranges or null *)

val parse_folding_range_params : Yojson.Safe.t -> folding_range_params
(** Parse folding range params from JSON *)

val folding_range_to_json : folding_range -> Yojson.Safe.t
(** Encode a folding range to JSON *)

val folding_range_result_to_json : folding_range_result -> Yojson.Safe.t
(** Encode folding range result to JSON *)

(** {1 Semantic Tokens} *)

(** Semantic token type as defined by LSP *)
type semantic_token_type =
  | STFunction
  | STVariable
  | STMacro
  | STParameter
  | STKeyword
  | STString
  | STNumber
  | STComment
  | STType

(** Semantic token modifier as defined by LSP *)
type semantic_token_modifier = SMDefinition | SMDeclaration | SMReadonly

type semantic_tokens_legend = {
  stl_token_types : string list;
  stl_token_modifiers : string list;
}
(** The legend describing token types and modifiers *)

type semantic_tokens_params = { stp_text_document : string }
(** Semantic tokens request params *)

type semantic_tokens_result = { str_data : int list }
(** The delta-encoded flat array of semantic tokens *)

val semantic_token_type_index : semantic_token_type -> int
(** Index of a token type in the legend's tokenTypes array *)

val semantic_token_modifier_bit : semantic_token_modifier -> int
(** Bit position for a token modifier (for bitfield encoding) *)

val semantic_tokens_legend : semantic_tokens_legend
(** The canonical legend advertised in server capabilities *)

val parse_semantic_tokens_params : Yojson.Safe.t -> semantic_tokens_params
(** Parse semantic tokens params from JSON *)

val semantic_tokens_legend_to_json : semantic_tokens_legend -> Yojson.Safe.t
(** Encode semantic tokens legend to JSON *)

val semantic_tokens_result_to_json :
  semantic_tokens_result option -> Yojson.Safe.t
(** Encode semantic tokens result to JSON *)

(** {1 Inlay Hints} *)

(** Inlay hint kind *)
type inlay_hint_kind = IHType | IHParameter

type inlay_hint = {
  ih_position : position;
  ih_label : string;
  ih_kind : inlay_hint_kind option;
  ih_padding_left : bool;
  ih_padding_right : bool;
}
(** An inlay hint displayed inline in the editor *)

type inlay_hint_params = { ihp_text_document : string; ihp_range : range }
(** Inlay hint request params *)

val parse_inlay_hint_params : Yojson.Safe.t -> inlay_hint_params
(** Parse inlay hint params from JSON *)

val inlay_hint_to_json : inlay_hint -> Yojson.Safe.t
(** Encode an inlay hint to JSON *)

val inlay_hint_result_to_json : inlay_hint list option -> Yojson.Safe.t
(** Encode inlay hint result to JSON *)

(** {1 Workspace Symbols} *)

type symbol_information = {
  si_name : string;
  si_kind : symbol_kind;
  si_location : location;
  si_container_name : string option;
}
(** A flat symbol information item for workspace symbol results *)

type workspace_symbol_params = { ws_query : string }
(** Workspace symbol request params *)

val parse_workspace_symbol_params : Yojson.Safe.t -> workspace_symbol_params
(** Parse workspace symbol params from JSON *)

type workspace_symbol_result = symbol_information list option
(** Workspace symbol result is a list of symbol informations or null *)

val workspace_symbol_result_to_json : workspace_symbol_result -> Yojson.Safe.t
(** Encode workspace symbol result to JSON *)
