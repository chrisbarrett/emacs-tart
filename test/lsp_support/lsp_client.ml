(** Reusable test helpers for exercising the LSP server. *)

open Lsp

(* {1 Message Construction} *)

let make_message ?(id : Yojson.Safe.t option) ~method_ ?params () : string =
  let json =
    `Assoc
      ([ ("jsonrpc", `String "2.0"); ("method", `String method_) ]
      @ (match id with Some i -> [ ("id", i) ] | None -> [])
      @ match params with Some p -> [ ("params", p) ] | None -> [])
  in
  let content = Yojson.Safe.to_string json in
  Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length content) content

let initialize_msg ?(id = 1) ?root_uri () =
  let params =
    `Assoc
      ([ ("processId", `Null); ("capabilities", `Assoc []) ]
      @
      match root_uri with
      | Some uri -> [ ("rootUri", `String uri) ]
      | None -> [])
  in
  make_message ~id:(`Int id) ~method_:"initialize" ~params ()

let initialized_msg () =
  make_message ~method_:"initialized" ~params:(`Assoc []) ()

let shutdown_msg ?(id = 99) () =
  make_message ~id:(`Int id) ~method_:"shutdown" ()

let exit_msg () = make_message ~method_:"exit" ()

let did_open_msg ~uri ?(version = 1) ~text () =
  make_message ~method_:"textDocument/didOpen"
    ~params:
      (`Assoc
         [
           ( "textDocument",
             `Assoc
               [
                 ("uri", `String uri);
                 ("languageId", `String "elisp");
                 ("version", `Int version);
                 ("text", `String text);
               ] );
         ])
    ()

let did_change_msg ~uri ~version ~changes () =
  make_message ~method_:"textDocument/didChange"
    ~params:
      (`Assoc
         [
           ( "textDocument",
             `Assoc [ ("uri", `String uri); ("version", `Int version) ] );
           ("contentChanges", `List changes);
         ])
    ()

let did_change_full_msg ~uri ~version ~text () =
  did_change_msg ~uri ~version ~changes:[ `Assoc [ ("text", `String text) ] ] ()

let did_close_msg ~uri () =
  make_message ~method_:"textDocument/didClose"
    ~params:(`Assoc [ ("textDocument", `Assoc [ ("uri", `String uri) ]) ])
    ()

let position_params ~uri ~line ~character =
  `Assoc
    [
      ("textDocument", `Assoc [ ("uri", `String uri) ]);
      ("position", `Assoc [ ("line", `Int line); ("character", `Int character) ]);
    ]

let hover_msg ~id ~uri ~line ~character () =
  make_message ~id:(`Int id) ~method_:"textDocument/hover"
    ~params:(position_params ~uri ~line ~character)
    ()

let definition_msg ~id ~uri ~line ~character () =
  make_message ~id:(`Int id) ~method_:"textDocument/definition"
    ~params:(position_params ~uri ~line ~character)
    ()

let references_msg ~id ~uri ~line ~character () =
  make_message ~id:(`Int id) ~method_:"textDocument/references"
    ~params:
      (`Assoc
         [
           ("textDocument", `Assoc [ ("uri", `String uri) ]);
           ( "position",
             `Assoc [ ("line", `Int line); ("character", `Int character) ] );
           ("context", `Assoc [ ("includeDeclaration", `Bool true) ]);
         ])
    ()

let completion_msg ~id ~uri ~line ~character () =
  make_message ~id:(`Int id) ~method_:"textDocument/completion"
    ~params:(position_params ~uri ~line ~character)
    ()

let signature_help_msg ~id ~uri ~line ~character () =
  make_message ~id:(`Int id) ~method_:"textDocument/signatureHelp"
    ~params:(position_params ~uri ~line ~character)
    ()

let document_symbol_msg ~id ~uri () =
  make_message ~id:(`Int id) ~method_:"textDocument/documentSymbol"
    ~params:(`Assoc [ ("textDocument", `Assoc [ ("uri", `String uri) ]) ])
    ()

let code_action_msg ~id ~uri ~start_line ~start_character ~end_line
    ~end_character ?(diagnostics = []) () =
  make_message ~id:(`Int id) ~method_:"textDocument/codeAction"
    ~params:
      (`Assoc
         [
           ("textDocument", `Assoc [ ("uri", `String uri) ]);
           ( "range",
             `Assoc
               [
                 ( "start",
                   `Assoc
                     [
                       ("line", `Int start_line);
                       ("character", `Int start_character);
                     ] );
                 ( "end",
                   `Assoc
                     [
                       ("line", `Int end_line); ("character", `Int end_character);
                     ] );
               ] );
           ("context", `Assoc [ ("diagnostics", `List diagnostics) ]);
         ])
    ()

let folding_range_msg ~id ~uri () =
  make_message ~id:(`Int id) ~method_:"textDocument/foldingRange"
    ~params:(`Assoc [ ("textDocument", `Assoc [ ("uri", `String uri) ]) ])
    ()

let semantic_tokens_msg ~id ~uri () =
  make_message ~id:(`Int id) ~method_:"textDocument/semanticTokens/full"
    ~params:(`Assoc [ ("textDocument", `Assoc [ ("uri", `String uri) ]) ])
    ()

let inlay_hint_msg ~id ~uri ~start_line ~start_character ~end_line
    ~end_character () =
  make_message ~id:(`Int id) ~method_:"textDocument/inlayHint"
    ~params:
      (`Assoc
         [
           ("textDocument", `Assoc [ ("uri", `String uri) ]);
           ( "range",
             `Assoc
               [
                 ( "start",
                   `Assoc
                     [
                       ("line", `Int start_line);
                       ("character", `Int start_character);
                     ] );
                 ( "end",
                   `Assoc
                     [
                       ("line", `Int end_line); ("character", `Int end_character);
                     ] );
               ] );
         ])
    ()

let type_definition_msg ~id ~uri ~line ~character () =
  make_message ~id:(`Int id) ~method_:"textDocument/typeDefinition"
    ~params:(position_params ~uri ~line ~character)
    ()

let prepare_rename_msg ~id ~uri ~line ~character () =
  make_message ~id:(`Int id) ~method_:"textDocument/prepareRename"
    ~params:(position_params ~uri ~line ~character)
    ()

let rename_msg ~id ~uri ~line ~character ~new_name () =
  make_message ~id:(`Int id) ~method_:"textDocument/rename"
    ~params:
      (`Assoc
         [
           ("textDocument", `Assoc [ ("uri", `String uri) ]);
           ( "position",
             `Assoc [ ("line", `Int line); ("character", `Int character) ] );
           ("newName", `String new_name);
         ])
    ()

let did_save_msg ~uri () =
  make_message ~method_:"textDocument/didSave"
    ~params:(`Assoc [ ("textDocument", `Assoc [ ("uri", `String uri) ]) ])
    ()

let workspace_symbol_msg ~id ~query () =
  make_message ~id:(`Int id) ~method_:"workspace/symbol"
    ~params:(`Assoc [ ("query", `String query) ])
    ()

(* {1 Message Parsing} *)

let parse_messages (output : string) : Yojson.Safe.t list =
  let rec parse_all offset acc =
    if offset >= String.length output then List.rev acc
    else
      let header_end =
        try String.index_from output offset '\r'
        with Not_found -> String.length output
      in
      if header_end >= String.length output then List.rev acc
      else
        let header = String.sub output offset (header_end - offset) in
        let re = Str.regexp "Content-Length: \\([0-9]+\\)" in
        if Str.string_match re header 0 then
          let content_length = int_of_string (Str.matched_group 1 header) in
          let content_start = header_end + 4 in
          if content_start + content_length <= String.length output then
            let content = String.sub output content_start content_length in
            let json = Yojson.Safe.from_string content in
            parse_all (content_start + content_length) (json :: acc)
          else List.rev acc
        else List.rev acc
  in
  parse_all 0 []

(* {1 Session Runners} *)

type session_result = {
  exit_code : int;
  messages : Yojson.Safe.t list;
  server : Server.t;
}

let run_session (msgs : string list) : session_result =
  let input = String.concat "" msgs in
  let in_file = Filename.temp_file "lsp_in" ".json" in
  Out_channel.with_open_bin in_file (fun oc ->
      Out_channel.output_string oc input);
  let ic = In_channel.open_bin in_file in
  let out_file = Filename.temp_file "lsp_out" ".json" in
  let oc = Out_channel.open_bin out_file in
  let server = Server.create ~ic ~oc () in
  let exit_code = Server.run server in
  In_channel.close ic;
  Out_channel.close oc;
  let output = In_channel.with_open_bin out_file In_channel.input_all in
  let messages = parse_messages output in
  { exit_code; messages; server }

let run_initialized_session (msgs : string list) : session_result =
  run_session ([ initialize_msg () ] @ msgs @ [ shutdown_msg (); exit_msg () ])

(* {1 Response Queries} *)

let find_response ~id (messages : Yojson.Safe.t list) : Yojson.Safe.t option =
  let open Yojson.Safe.Util in
  List.find_opt
    (fun json -> match json |> member "id" with `Int i -> i = id | _ -> false)
    messages

let find_notification ~method_ (messages : Yojson.Safe.t list) :
    Yojson.Safe.t option =
  let open Yojson.Safe.Util in
  List.find_opt
    (fun json ->
      match json |> member "method" with `String m -> m = method_ | _ -> false)
    messages

let find_all_notifications ~method_ (messages : Yojson.Safe.t list) :
    Yojson.Safe.t list =
  let open Yojson.Safe.Util in
  List.filter
    (fun json ->
      match json |> member "method" with `String m -> m = method_ | _ -> false)
    messages

let find_diagnostics ~uri (messages : Yojson.Safe.t list) : Yojson.Safe.t option
    =
  let open Yojson.Safe.Util in
  List.find_opt
    (fun json ->
      match json |> member "method" with
      | `String "textDocument/publishDiagnostics" ->
          let params = json |> member "params" in
          params |> member "uri" |> to_string = uri
      | _ -> false)
    messages

let find_last_diagnostics ~uri (messages : Yojson.Safe.t list) :
    Yojson.Safe.t option =
  let open Yojson.Safe.Util in
  let matching =
    List.filter
      (fun json ->
        match json |> member "method" with
        | `String "textDocument/publishDiagnostics" ->
            let params = json |> member "params" in
            params |> member "uri" |> to_string = uri
        | _ -> false)
      messages
  in
  match List.rev matching with x :: _ -> Some x | [] -> None

let response_result (json : Yojson.Safe.t) : Yojson.Safe.t =
  Yojson.Safe.Util.member "result" json

let response_error (json : Yojson.Safe.t) : Yojson.Safe.t option =
  match Yojson.Safe.Util.member "error" json with
  | `Null -> None
  | err -> Some err

(* {1 String Helpers} *)

let contains_string ~needle haystack =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false
