(** LSP Protocol message types.

    Defines request/response types for the Language Server Protocol. Phase 1:
    initialize, initialized, shutdown. *)

(** {1 Initialize Request} *)

type client_capabilities = {
  text_document : text_document_client_capabilities option;
}
(** Client capabilities (simplified for Phase 1). We parse just what we need and
    ignore the rest. *)

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
}
(** Server capabilities *)

type initialize_result = { capabilities : server_capabilities }
(** Initialize result *)

(** {1 JSON Parsing} *)

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
  { text_document }

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
    | `Null -> { text_document = None }
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
  `Assoc fields

(** Encode initialize result to JSON *)
let initialize_result_to_json (result : initialize_result) : Yojson.Safe.t =
  `Assoc [ ("capabilities", server_capabilities_to_json result.capabilities) ]

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
      ("serverInfo", server_info_to_json server_info);
    ]

(** {1 Diagnostics} *)

type diagnostic_severity = Error | Warning | Information | Hint
type position = { line : int; character : int }
type range = { start : position; end_ : position }

type diagnostic = {
  range : range;
  severity : diagnostic_severity option;
  message : string;
  source : string option;
}

type publish_diagnostics_params = {
  uri : string;
  version : int option;
  diagnostics : diagnostic list;
}

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
    match d.source with
    | Some src -> ("source", `String src) :: fields
    | None -> fields
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
