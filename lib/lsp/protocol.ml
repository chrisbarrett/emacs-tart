(** LSP Protocol message types.

    Defines request/response types for the Language Server Protocol. Phase 1:
    initialize, initialized, shutdown. *)

(** {1 Position Encoding} *)

type position_encoding = UTF16 | UTF32

(** {1 Initialize Request} *)

type general_client_capabilities = { position_encodings : string list option }
(** General client capabilities, including position encoding support *)

type client_capabilities = {
  text_document : text_document_client_capabilities option;
  general : general_client_capabilities option;
}
(** Client capabilities. We parse just what we need and ignore the rest. *)

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

let text_document_sync_kind_to_int = function
  | None_ -> 0
  | Full -> 1
  | Incremental -> 2

type text_document_sync_options = {
  open_close : bool;
  change : text_document_sync_kind;
}
(** Text document sync options *)

type server_capabilities = {
  text_document_sync : text_document_sync_options option;
  hover_provider : bool;
  definition_provider : bool;
  references_provider : bool;
  code_action_provider : bool;
  document_symbol_provider : bool;
  completion_provider : bool;
  signature_help_provider : bool;
  rename_provider : bool;
  folding_range_provider : bool;
  semantic_tokens_provider : bool;
  inlay_hint_provider : bool;
}
(** Server capabilities *)

type initialize_result = {
  capabilities : server_capabilities;
  position_encoding : position_encoding;
}
(** Initialize result *)

(** {1 JSON Parsing} *)

let position_encoding_to_string = function
  | UTF16 -> "utf-16"
  | UTF32 -> "utf-32"

let negotiate_position_encoding (caps : client_capabilities) : position_encoding
    =
  match caps.general with
  | Some { position_encodings = Some encodings } ->
      if List.mem "utf-32" encodings then UTF32 else UTF16
  | _ -> UTF16

(** Parse client capabilities from JSON *)
let parse_client_capabilities (json : Yojson.Safe.t) : client_capabilities =
  let open Yojson.Safe.Util in
  let text_document =
    match json |> member "textDocument" with
    | `Null -> None
    | td ->
        let synchronization =
          match td |> member "synchronization" with
          | `Null -> None
          | sync ->
              let dynamic_registration =
                match sync |> member "dynamicRegistration" with
                | `Bool b -> Some b
                | _ -> None
              in
              Some { dynamic_registration }
        in
        Some { synchronization }
  in
  let general =
    match json |> member "general" with
    | `Null -> None
    | gen ->
        let position_encodings =
          match gen |> member "positionEncodings" with
          | `List encodings -> Some (List.filter_map to_string_option encodings)
          | _ -> None
        in
        Some { position_encodings }
  in
  { text_document; general }

(** Parse initialize params from JSON *)
let parse_initialize_params (json : Yojson.Safe.t) : initialize_params =
  let open Yojson.Safe.Util in
  let process_id =
    match json |> member "processId" with
    | `Int pid -> Some pid
    | `Null -> None
    | _ -> None
  in
  let root_uri =
    match json |> member "rootUri" with
    | `String uri -> Some uri
    | `Null -> None
    | _ -> None
  in
  let capabilities =
    match json |> member "capabilities" with
    | `Null -> { text_document = None; general = None }
    | caps -> parse_client_capabilities caps
  in
  { process_id; root_uri; capabilities }

(** {1 JSON Encoding} *)

(** Encode text document sync options to JSON *)
let text_document_sync_options_to_json (opts : text_document_sync_options) :
    Yojson.Safe.t =
  `Assoc
    [
      ("openClose", `Bool opts.open_close);
      ("change", `Int (text_document_sync_kind_to_int opts.change));
    ]

(** {1 Semantic Token Types} *)

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

type semantic_token_modifier = SMDefinition | SMDeclaration | SMReadonly

type semantic_tokens_legend = {
  stl_token_types : string list;
  stl_token_modifiers : string list;
}

let semantic_token_type_index = function
  | STFunction -> 0
  | STVariable -> 1
  | STMacro -> 2
  | STParameter -> 3
  | STKeyword -> 4
  | STString -> 5
  | STNumber -> 6
  | STComment -> 7
  | STType -> 8

let semantic_token_modifier_bit = function
  | SMDefinition -> 0
  | SMDeclaration -> 1
  | SMReadonly -> 2

let semantic_tokens_legend =
  {
    stl_token_types =
      [
        "function";
        "variable";
        "macro";
        "parameter";
        "keyword";
        "string";
        "number";
        "comment";
        "type";
      ];
    stl_token_modifiers = [ "definition"; "declaration"; "readonly" ];
  }

let semantic_tokens_legend_to_json (legend : semantic_tokens_legend) :
    Yojson.Safe.t =
  `Assoc
    [
      ( "tokenTypes",
        `List (List.map (fun s -> `String s) legend.stl_token_types) );
      ( "tokenModifiers",
        `List (List.map (fun s -> `String s) legend.stl_token_modifiers) );
    ]

(** Encode server capabilities to JSON *)
let server_capabilities_to_json (caps : server_capabilities) : Yojson.Safe.t =
  let fields = [] in
  let fields =
    match caps.text_document_sync with
    | Some sync ->
        ("textDocumentSync", text_document_sync_options_to_json sync) :: fields
    | None -> fields
  in
  let fields = ("hoverProvider", `Bool caps.hover_provider) :: fields in
  let fields =
    ("definitionProvider", `Bool caps.definition_provider) :: fields
  in
  let fields =
    ("referencesProvider", `Bool caps.references_provider) :: fields
  in
  let fields =
    ("codeActionProvider", `Bool caps.code_action_provider) :: fields
  in
  let fields =
    ("documentSymbolProvider", `Bool caps.document_symbol_provider) :: fields
  in
  let fields =
    if caps.completion_provider then
      ( "completionProvider",
        `Assoc [ ("triggerCharacters", `List [ `String "(" ]) ] )
      :: fields
    else fields
  in
  let fields =
    if caps.signature_help_provider then
      ( "signatureHelpProvider",
        `Assoc [ ("triggerCharacters", `List [ `String "("; `String " " ]) ] )
      :: fields
    else fields
  in
  let fields =
    if caps.rename_provider then ("renameProvider", `Bool true) :: fields
    else fields
  in
  let fields =
    if caps.folding_range_provider then
      ("foldingRangeProvider", `Bool true) :: fields
    else fields
  in
  let fields =
    if caps.semantic_tokens_provider then
      ( "semanticTokensProvider",
        `Assoc
          [
            ("full", `Bool true);
            ("legend", semantic_tokens_legend_to_json semantic_tokens_legend);
          ] )
      :: fields
    else fields
  in
  let fields =
    if caps.inlay_hint_provider then ("inlayHintProvider", `Bool true) :: fields
    else fields
  in
  `Assoc fields

(** Encode initialize result to JSON *)
let initialize_result_to_json (result : initialize_result) : Yojson.Safe.t =
  `Assoc
    [
      ("capabilities", server_capabilities_to_json result.capabilities);
      ( "positionEncoding",
        `String (position_encoding_to_string result.position_encoding) );
    ]

(** {1 Server Info} *)

type server_info = { name : string; version : string option }
(** Server info included in initialize response *)

(** Encode server info to JSON *)
let server_info_to_json (info : server_info) : Yojson.Safe.t =
  let fields = [ ("name", `String info.name) ] in
  let fields =
    match info.version with
    | Some v -> fields @ [ ("version", `String v) ]
    | None -> fields
  in
  `Assoc fields

(** Full initialize response with server info *)
let initialize_response_to_json ~(result : initialize_result)
    ~(server_info : server_info) : Yojson.Safe.t =
  `Assoc
    [
      ("capabilities", server_capabilities_to_json result.capabilities);
      ( "positionEncoding",
        `String (position_encoding_to_string result.position_encoding) );
      ("serverInfo", server_info_to_json server_info);
    ]

(** {1 Diagnostics} *)

type diagnostic_severity = Error | Warning | Information | Hint
type position = { line : int; character : int }
type range = { start : position; end_ : position }

type location = { uri : string; range : range }
(** A location in a document (uri + range) *)

type diagnostic_related_information = { location : location; message : string }
(** Related information for a diagnostic *)

type diagnostic = {
  range : range;
  severity : diagnostic_severity option;
  code : string option;
  message : string;
  source : string option;
  related_information : diagnostic_related_information list;
}

type publish_diagnostics_params = {
  uri : string;
  version : int option;
  diagnostics : diagnostic list;
}

let position_equal (a : position) (b : position) : bool =
  a.line = b.line && a.character = b.character

let range_equal (a : range) (b : range) : bool =
  position_equal a.start b.start && position_equal a.end_ b.end_

let location_equal (a : location) (b : location) : bool =
  a.uri = b.uri && range_equal a.range b.range

let diagnostic_related_information_equal (a : diagnostic_related_information)
    (b : diagnostic_related_information) : bool =
  location_equal a.location b.location && a.message = b.message

let diagnostic_equal (a : diagnostic) (b : diagnostic) : bool =
  range_equal a.range b.range
  && a.severity = b.severity && a.code = b.code && a.message = b.message
  && a.source = b.source
  && List.length a.related_information = List.length b.related_information
  && List.for_all2 diagnostic_related_information_equal a.related_information
       b.related_information

let diagnostics_equal (a : diagnostic list) (b : diagnostic list) : bool =
  List.length a = List.length b && List.for_all2 diagnostic_equal a b

let diagnostic_severity_to_int = function
  | Error -> 1
  | Warning -> 2
  | Information -> 3
  | Hint -> 4

let position_to_json (pos : position) : Yojson.Safe.t =
  `Assoc [ ("line", `Int pos.line); ("character", `Int pos.character) ]

let range_to_json (range : range) : Yojson.Safe.t =
  `Assoc
    [
      ("start", position_to_json range.start);
      ("end", position_to_json range.end_);
    ]

let location_to_json (loc : location) : Yojson.Safe.t =
  `Assoc [ ("uri", `String loc.uri); ("range", range_to_json loc.range) ]

let diagnostic_related_information_to_json
    (rel : diagnostic_related_information) : Yojson.Safe.t =
  `Assoc
    [
      ("location", location_to_json rel.location);
      ("message", `String rel.message);
    ]

let diagnostic_to_json (d : diagnostic) : Yojson.Safe.t =
  let fields =
    [ ("range", range_to_json d.range); ("message", `String d.message) ]
  in
  let fields =
    match d.severity with
    | Some sev -> ("severity", `Int (diagnostic_severity_to_int sev)) :: fields
    | None -> fields
  in
  let fields =
    match d.code with
    | Some code -> ("code", `String code) :: fields
    | None -> fields
  in
  let fields =
    match d.source with
    | Some src -> ("source", `String src) :: fields
    | None -> fields
  in
  let fields =
    match d.related_information with
    | [] -> fields
    | infos ->
        ( "relatedInformation",
          `List (List.map diagnostic_related_information_to_json infos) )
        :: fields
  in
  `Assoc fields

let publish_diagnostics_params_to_json (params : publish_diagnostics_params) :
    Yojson.Safe.t =
  let fields =
    [
      ("uri", `String params.uri);
      ("diagnostics", `List (List.map diagnostic_to_json params.diagnostics));
    ]
  in
  let fields =
    match params.version with
    | Some v -> ("version", `Int v) :: fields
    | None -> fields
  in
  `Assoc fields

(** {1 Hover} *)

(** Markup content kind *)
type markup_kind = PlainText | Markdown

type markup_content = { kind : markup_kind; value : string }
(** Markup content for hover *)

type hover = { contents : markup_content; range : range option }
(** Hover result *)

type hover_params = { text_document : string; (* URI *) position : position }
(** Hover params *)

let parse_hover_params (json : Yojson.Safe.t) : hover_params =
  let open Yojson.Safe.Util in
  let text_document =
    json |> member "textDocument" |> member "uri" |> to_string
  in
  let pos_json = json |> member "position" in
  let position =
    {
      line = pos_json |> member "line" |> to_int;
      character = pos_json |> member "character" |> to_int;
    }
  in
  { text_document; position }

let markup_kind_to_string = function
  | PlainText -> "plaintext"
  | Markdown -> "markdown"

let markup_content_to_json (content : markup_content) : Yojson.Safe.t =
  `Assoc
    [
      ("kind", `String (markup_kind_to_string content.kind));
      ("value", `String content.value);
    ]

let hover_to_json (hover : hover) : Yojson.Safe.t =
  let fields = [ ("contents", markup_content_to_json hover.contents) ] in
  let fields =
    match hover.range with
    | Some r -> ("range", range_to_json r) :: fields
    | None -> fields
  in
  `Assoc fields

(** {1 Go to Definition} *)

type definition_params = {
  def_text_document : string;
  (* URI *)
  def_position : position;
}
(** Definition request params (same structure as hover) *)

let parse_definition_params (json : Yojson.Safe.t) : definition_params =
  let open Yojson.Safe.Util in
  let text_document =
    json |> member "textDocument" |> member "uri" |> to_string
  in
  let pos_json = json |> member "position" in
  let position =
    {
      line = pos_json |> member "line" |> to_int;
      character = pos_json |> member "character" |> to_int;
    }
  in
  { def_text_document = text_document; def_position = position }

(** Definition result can be a single location, list of locations, or null *)
type definition_result =
  | DefLocation of location
  | DefLocations of location list
  | DefNull

let definition_result_to_json (result : definition_result) : Yojson.Safe.t =
  match result with
  | DefLocation loc -> location_to_json loc
  | DefLocations locs -> `List (List.map location_to_json locs)
  | DefNull -> `Null

(** {1 Find References} *)

type references_params = {
  ref_text_document : string;
  (* URI *)
  ref_position : position;
  include_declaration : bool;
}
(** References request params *)

let parse_references_params (json : Yojson.Safe.t) : references_params =
  let open Yojson.Safe.Util in
  let text_document =
    json |> member "textDocument" |> member "uri" |> to_string
  in
  let pos_json = json |> member "position" in
  let position =
    {
      line = pos_json |> member "line" |> to_int;
      character = pos_json |> member "character" |> to_int;
    }
  in
  let include_declaration =
    match json |> member "context" with
    | `Null -> true
    | ctx -> (
        match ctx |> member "includeDeclaration" with `Bool b -> b | _ -> true)
  in
  {
    ref_text_document = text_document;
    ref_position = position;
    include_declaration;
  }

type references_result = location list option
(** References result is a list of locations or null *)

let references_result_to_json (result : references_result) : Yojson.Safe.t =
  match result with
  | Some locs -> `List (List.map location_to_json locs)
  | None -> `Null

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

let code_action_kind_to_string = function
  | QuickFix -> "quickfix"
  | Refactor -> "refactor"
  | RefactorExtract -> "refactor.extract"
  | RefactorInline -> "refactor.inline"
  | RefactorRewrite -> "refactor.rewrite"
  | Source -> "source"
  | SourceOrganizeImports -> "source.organizeImports"

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

let parse_position (json : Yojson.Safe.t) : position =
  let open Yojson.Safe.Util in
  {
    line = json |> member "line" |> to_int;
    character = json |> member "character" |> to_int;
  }

let parse_range (json : Yojson.Safe.t) : range =
  let open Yojson.Safe.Util in
  {
    start = json |> member "start" |> parse_position;
    end_ = json |> member "end" |> parse_position;
  }

let parse_diagnostic_severity (json : Yojson.Safe.t) :
    diagnostic_severity option =
  match json with
  | `Int 1 -> Some Error
  | `Int 2 -> Some Warning
  | `Int 3 -> Some Information
  | `Int 4 -> Some Hint
  | _ -> None

let parse_diagnostic (json : Yojson.Safe.t) : diagnostic =
  let open Yojson.Safe.Util in
  {
    range = json |> member "range" |> parse_range;
    severity = json |> member "severity" |> parse_diagnostic_severity;
    code = (match json |> member "code" with `String s -> Some s | _ -> None);
    message =
      json |> member "message" |> to_string_option |> Option.value ~default:"";
    source =
      (match json |> member "source" with `String s -> Some s | _ -> None);
    related_information = [];
    (* We don't parse related info from client *)
  }

let parse_code_action_kind (s : string) : code_action_kind option =
  match s with
  | "quickfix" -> Some QuickFix
  | "refactor" -> Some Refactor
  | "refactor.extract" -> Some RefactorExtract
  | "refactor.inline" -> Some RefactorInline
  | "refactor.rewrite" -> Some RefactorRewrite
  | "source" -> Some Source
  | "source.organizeImports" -> Some SourceOrganizeImports
  | _ -> None

let parse_code_action_context (json : Yojson.Safe.t) : code_action_context =
  let open Yojson.Safe.Util in
  let diagnostics =
    json |> member "diagnostics" |> to_list |> List.map parse_diagnostic
  in
  let only =
    match json |> member "only" with
    | `List kinds ->
        Some
          (List.filter_map
             (fun k -> k |> to_string |> parse_code_action_kind)
             kinds)
    | _ -> None
  in
  { cac_diagnostics = diagnostics; cac_only = only }

let parse_code_action_params (json : Yojson.Safe.t) : code_action_params =
  let open Yojson.Safe.Util in
  {
    ca_text_document =
      json |> member "textDocument" |> member "uri" |> to_string;
    ca_range = json |> member "range" |> parse_range;
    ca_context = json |> member "context" |> parse_code_action_context;
  }

let text_edit_to_json (edit : text_edit) : Yojson.Safe.t =
  `Assoc
    [
      ("range", range_to_json edit.te_range); ("newText", `String edit.new_text);
    ]

let text_document_edit_to_json (tde : text_document_edit) : Yojson.Safe.t =
  let version_json =
    match tde.tde_version with Some v -> `Int v | None -> `Null
  in
  `Assoc
    [
      ( "textDocument",
        `Assoc [ ("uri", `String tde.tde_uri); ("version", version_json) ] );
      ("edits", `List (List.map text_edit_to_json tde.edits));
    ]

let workspace_edit_to_json (edit : workspace_edit) : Yojson.Safe.t =
  `Assoc
    [
      ( "documentChanges",
        `List (List.map text_document_edit_to_json edit.document_changes) );
    ]

let code_action_to_json (action : code_action) : Yojson.Safe.t =
  let fields = [ ("title", `String action.ca_title) ] in
  let fields =
    match action.ca_kind with
    | Some k -> ("kind", `String (code_action_kind_to_string k)) :: fields
    | None -> fields
  in
  let fields =
    match action.ca_diagnostics with
    | [] -> fields
    | ds -> ("diagnostics", `List (List.map diagnostic_to_json ds)) :: fields
  in
  let fields =
    if action.ca_is_preferred then ("isPreferred", `Bool true) :: fields
    else fields
  in
  let fields =
    match action.ca_edit with
    | Some e -> ("edit", workspace_edit_to_json e) :: fields
    | None -> fields
  in
  `Assoc fields

type code_action_result = code_action list option
(** Code action result is a list of actions or null *)

let code_action_result_to_json (result : code_action_result) : Yojson.Safe.t =
  match result with
  | Some actions -> `List (List.map code_action_to_json actions)
  | None -> `Null

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

let symbol_kind_to_int = function
  | SKFile -> 1
  | SKModule -> 2
  | SKNamespace -> 3
  | SKPackage -> 4
  | SKClass -> 5
  | SKMethod -> 6
  | SKProperty -> 7
  | SKField -> 8
  | SKConstructor -> 9
  | SKEnum -> 10
  | SKInterface -> 11
  | SKFunction -> 12
  | SKVariable -> 13
  | SKConstant -> 14
  | SKString -> 15
  | SKNumber -> 16
  | SKBoolean -> 17
  | SKArray -> 18
  | SKObject -> 19
  | SKKey -> 20
  | SKNull -> 21
  | SKEnumMember -> 22
  | SKStruct -> 23
  | SKEvent -> 24
  | SKOperator -> 25
  | SKTypeParameter -> 26

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

let parse_document_symbol_params (json : Yojson.Safe.t) : document_symbol_params
    =
  let open Yojson.Safe.Util in
  {
    dsp_text_document =
      json |> member "textDocument" |> member "uri" |> to_string;
  }

let rec document_symbol_to_json (sym : document_symbol) : Yojson.Safe.t =
  let fields =
    [
      ("name", `String sym.ds_name);
      ("kind", `Int (symbol_kind_to_int sym.ds_kind));
      ("range", range_to_json sym.ds_range);
      ("selectionRange", range_to_json sym.ds_selection_range);
    ]
  in
  let fields =
    match sym.ds_detail with
    | Some d -> ("detail", `String d) :: fields
    | None -> fields
  in
  let fields =
    match sym.ds_children with
    | [] -> fields
    | children ->
        ("children", `List (List.map document_symbol_to_json children))
        :: fields
  in
  `Assoc fields

type document_symbol_result = document_symbol list option
(** Document symbol result *)

let document_symbol_result_to_json (result : document_symbol_result) :
    Yojson.Safe.t =
  match result with
  | Some symbols -> `List (List.map document_symbol_to_json symbols)
  | None -> `Null

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

let completion_item_kind_to_int = function
  | CIKText -> 1
  | CIKMethod -> 2
  | CIKFunction -> 3
  | CIKConstructor -> 4
  | CIKField -> 5
  | CIKVariable -> 6
  | CIKClass -> 7
  | CIKInterface -> 8
  | CIKModule -> 9
  | CIKProperty -> 10
  | CIKUnit -> 11
  | CIKValue -> 12
  | CIKEnum -> 13
  | CIKKeyword -> 14
  | CIKSnippet -> 15
  | CIKColor -> 16
  | CIKFile -> 17
  | CIKReference -> 18
  | CIKFolder -> 19
  | CIKEnumMember -> 20
  | CIKConstant -> 21
  | CIKStruct -> 22
  | CIKEvent -> 23
  | CIKOperator -> 24
  | CIKTypeParameter -> 25

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

let parse_completion_params (json : Yojson.Safe.t) : completion_params =
  let open Yojson.Safe.Util in
  let text_document =
    json |> member "textDocument" |> member "uri" |> to_string
  in
  let pos_json = json |> member "position" in
  let position =
    {
      line = pos_json |> member "line" |> to_int;
      character = pos_json |> member "character" |> to_int;
    }
  in
  { cp_text_document = text_document; cp_position = position }

let completion_item_to_json (item : completion_item) : Yojson.Safe.t =
  let fields = [ ("label", `String item.ci_label) ] in
  let fields =
    match item.ci_kind with
    | Some k -> ("kind", `Int (completion_item_kind_to_int k)) :: fields
    | None -> fields
  in
  let fields =
    match item.ci_detail with
    | Some d -> ("detail", `String d) :: fields
    | None -> fields
  in
  let fields =
    match item.ci_documentation with
    | Some doc ->
        ( "documentation",
          `Assoc [ ("kind", `String "markdown"); ("value", `String doc) ] )
        :: fields
    | None -> fields
  in
  let fields =
    match item.ci_insert_text with
    | Some t -> ("insertText", `String t) :: fields
    | None -> fields
  in
  `Assoc fields

type completion_result = completion_item list option
(** Completion result is a list of items or null *)

let completion_result_to_json (result : completion_result) : Yojson.Safe.t =
  match result with
  | Some items -> `List (List.map completion_item_to_json items)
  | None -> `Null

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

let parse_signature_help_params (json : Yojson.Safe.t) : signature_help_params =
  let open Yojson.Safe.Util in
  let text_document =
    json |> member "textDocument" |> member "uri" |> to_string
  in
  let pos_json = json |> member "position" in
  let position =
    {
      line = pos_json |> member "line" |> to_int;
      character = pos_json |> member "character" |> to_int;
    }
  in
  { shp_text_document = text_document; shp_position = position }

let parameter_information_to_json (pi : parameter_information) : Yojson.Safe.t =
  let fields = [ ("label", `String pi.pi_label) ] in
  let fields =
    match pi.pi_documentation with
    | Some doc ->
        ( "documentation",
          `Assoc [ ("kind", `String "markdown"); ("value", `String doc) ] )
        :: fields
    | None -> fields
  in
  `Assoc fields

let signature_information_to_json (si : signature_information) : Yojson.Safe.t =
  let fields = [ ("label", `String si.si_label) ] in
  let fields =
    match si.si_documentation with
    | Some doc ->
        ( "documentation",
          `Assoc [ ("kind", `String "markdown"); ("value", `String doc) ] )
        :: fields
    | None -> fields
  in
  let fields =
    match si.si_parameters with
    | [] -> fields
    | params ->
        ("parameters", `List (List.map parameter_information_to_json params))
        :: fields
  in
  let fields =
    match si.si_active_parameter with
    | Some idx -> ("activeParameter", `Int idx) :: fields
    | None -> fields
  in
  `Assoc fields

let signature_help_to_json (sh : signature_help) : Yojson.Safe.t =
  let fields =
    [
      ( "signatures",
        `List (List.map signature_information_to_json sh.sh_signatures) );
    ]
  in
  let fields =
    match sh.sh_active_signature with
    | Some idx -> ("activeSignature", `Int idx) :: fields
    | None -> fields
  in
  let fields =
    match sh.sh_active_parameter with
    | Some idx -> ("activeParameter", `Int idx) :: fields
    | None -> fields
  in
  `Assoc fields

type signature_help_result = signature_help option
(** Signature help result *)

let signature_help_result_to_json (result : signature_help_result) :
    Yojson.Safe.t =
  match result with Some sh -> signature_help_to_json sh | None -> `Null

(** {1 Rename} *)

type rename_params = {
  rp_text_document : string;
  rp_position : position;
  rp_new_name : string;
}
(** Rename request params *)

let parse_rename_params (json : Yojson.Safe.t) : rename_params =
  let open Yojson.Safe.Util in
  let text_document =
    json |> member "textDocument" |> member "uri" |> to_string
  in
  let pos_json = json |> member "position" in
  let position =
    {
      line = pos_json |> member "line" |> to_int;
      character = pos_json |> member "character" |> to_int;
    }
  in
  let new_name = json |> member "newName" |> to_string in
  {
    rp_text_document = text_document;
    rp_position = position;
    rp_new_name = new_name;
  }

type rename_result = workspace_edit option
(** Rename result is a workspace edit or null *)

let rename_result_to_json (result : rename_result) : Yojson.Safe.t =
  match result with Some edit -> workspace_edit_to_json edit | None -> `Null

(** {1 Folding Ranges} *)

type folding_range_kind =
  | FRComment
  | FRImports
  | FRRegion  (** Folding range kind as defined by LSP *)

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

let folding_range_kind_to_string = function
  | FRComment -> "comment"
  | FRImports -> "imports"
  | FRRegion -> "region"

let parse_folding_range_params (json : Yojson.Safe.t) : folding_range_params =
  let open Yojson.Safe.Util in
  {
    frp_text_document =
      json |> member "textDocument" |> member "uri" |> to_string;
  }

let folding_range_to_json (fr : folding_range) : Yojson.Safe.t =
  let fields =
    [ ("startLine", `Int fr.fr_start_line); ("endLine", `Int fr.fr_end_line) ]
  in
  let fields =
    match fr.fr_start_character with
    | Some c -> ("startCharacter", `Int c) :: fields
    | None -> fields
  in
  let fields =
    match fr.fr_end_character with
    | Some c -> ("endCharacter", `Int c) :: fields
    | None -> fields
  in
  let fields =
    match fr.fr_kind with
    | Some k -> ("kind", `String (folding_range_kind_to_string k)) :: fields
    | None -> fields
  in
  `Assoc fields

let folding_range_result_to_json (result : folding_range_result) : Yojson.Safe.t
    =
  match result with
  | Some ranges -> `List (List.map folding_range_to_json ranges)
  | None -> `Null

(** {1 Semantic Tokens} *)

type semantic_tokens_params = { stp_text_document : string }
type semantic_tokens_result = { str_data : int list }

let parse_semantic_tokens_params (json : Yojson.Safe.t) : semantic_tokens_params
    =
  let open Yojson.Safe.Util in
  {
    stp_text_document =
      json |> member "textDocument" |> member "uri" |> to_string;
  }

let semantic_tokens_result_to_json (result : semantic_tokens_result option) :
    Yojson.Safe.t =
  match result with
  | Some r -> `Assoc [ ("data", `List (List.map (fun i -> `Int i) r.str_data)) ]
  | None -> `Null

(** {1 Inlay Hints} *)

type inlay_hint_kind = IHType | IHParameter

type inlay_hint = {
  ih_position : position;
  ih_label : string;
  ih_kind : inlay_hint_kind option;
  ih_padding_left : bool;
  ih_padding_right : bool;
}

type inlay_hint_params = { ihp_text_document : string; ihp_range : range }

let inlay_hint_kind_to_int = function IHType -> 1 | IHParameter -> 2

let parse_inlay_hint_params (json : Yojson.Safe.t) : inlay_hint_params =
  let open Yojson.Safe.Util in
  {
    ihp_text_document =
      json |> member "textDocument" |> member "uri" |> to_string;
    ihp_range = json |> member "range" |> parse_range;
  }

let inlay_hint_to_json (hint : inlay_hint) : Yojson.Safe.t =
  let fields =
    [
      ("position", position_to_json hint.ih_position);
      ("label", `String hint.ih_label);
    ]
  in
  let fields =
    match hint.ih_kind with
    | Some k -> ("kind", `Int (inlay_hint_kind_to_int k)) :: fields
    | None -> fields
  in
  let fields =
    if hint.ih_padding_left then ("paddingLeft", `Bool true) :: fields
    else fields
  in
  let fields =
    if hint.ih_padding_right then ("paddingRight", `Bool true) :: fields
    else fields
  in
  `Assoc fields

let inlay_hint_result_to_json (result : inlay_hint list option) : Yojson.Safe.t
    =
  match result with
  | Some hints -> `List (List.map inlay_hint_to_json hints)
  | None -> `Null
