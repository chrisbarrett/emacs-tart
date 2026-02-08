(** JSON-RPC 2.0 protocol implementation for LSP.

    Implements the JSON-RPC 2.0 protocol over stdio with Content-Length headers
    as specified by the Language Server Protocol. *)

(** {1 Message Types} *)

type message = {
  id : Yojson.Safe.t option;
  method_ : string;
  params : Yojson.Safe.t option;
}

type response_error = {
  code : int;
  message : string;
  data : Yojson.Safe.t option;
}

type response = {
  id : Yojson.Safe.t;
  result : Yojson.Safe.t option;
  error : response_error option;
}

(** {1 Standard Error Codes} *)

let parse_error = -32700
let invalid_request = -32600
let method_not_found = -32601
let invalid_params = -32602
let internal_error = -32603

(** LSP-specific error codes *)
let server_not_initialized = -32002

let request_cancelled = -32800
let content_modified = -32801

(** {1 Reading Messages} *)

type read_error =
  | Eof
  | InvalidHeader of string
  | InvalidJson of string
  | InvalidMessage of string

(** Read headers until we get a blank line. Returns the Content-Length value. *)
let read_headers (ic : In_channel.t) : (int, read_error) result =
  let content_length = ref None in
  let rec read_header_lines () =
    match In_channel.input_line ic with
    | None -> Error Eof
    | Some "" -> (
        (* Empty line (after stripping \r) signals end of headers *)
        match !content_length with
        | Some len -> Ok len
        | None -> Error (InvalidHeader "Missing Content-Length header"))
    | Some line -> (
        (* Strip trailing \r if present (LSP uses \r\n) *)
        let line =
          if String.length line > 0 && line.[String.length line - 1] = '\r' then
            String.sub line 0 (String.length line - 1)
          else line
        in
        if line = "" then
          (* Empty after stripping means end of headers *)
          match !content_length with
          | Some len -> Ok len
          | None -> Error (InvalidHeader "Missing Content-Length header")
        else
          (* Parse header line *)
          match String.split_on_char ':' line with
          | [ name; value ] ->
              let name = String.trim name in
              let value = String.trim value in
              (if String.lowercase_ascii name = "content-length" then
                 match int_of_string_opt value with
                 | Some len -> content_length := Some len
                 | None -> ());
              read_header_lines ()
          | _ -> read_header_lines ())
  in
  read_header_lines ()

(** Read exactly n bytes from an input channel *)
let read_exactly (ic : In_channel.t) (n : int) : (string, read_error) result =
  let buf = Bytes.create n in
  let rec read_loop offset remaining =
    if remaining = 0 then Ok (Bytes.to_string buf)
    else
      match In_channel.input ic buf offset remaining with
      | 0 -> Error Eof
      | bytes_read -> read_loop (offset + bytes_read) (remaining - bytes_read)
  in
  read_loop 0 n

(** Parse a JSON-RPC message from JSON *)
let parse_message (json : Yojson.Safe.t) : (message, read_error) result =
  let open Yojson.Safe.Util in
  try
    let method_ =
      match json |> member "method" with
      | `String s -> s
      | `Null -> raise (Type_error ("Missing method field", json))
      | _ -> raise (Type_error ("Method must be a string", json))
    in
    let id = match json |> member "id" with `Null -> None | v -> Some v in
    let params =
      match json |> member "params" with `Null -> None | v -> Some v
    in
    Ok { id; method_; params }
  with
  | Type_error (msg, _) -> Error (InvalidMessage msg)
  | Yojson.Json_error msg -> Error (InvalidMessage msg)

let read_message (ic : In_channel.t) : (message, read_error) result =
  match read_headers ic with
  | Error e -> Error e
  | Ok content_length -> (
      match read_exactly ic content_length with
      | Error e -> Error e
      | Ok content -> (
          try
            let json = Yojson.Safe.from_string content in
            parse_message json
          with Yojson.Json_error msg -> Error (InvalidJson msg)))

(** {1 Writing Messages} *)

(** Convert response_error to JSON *)
let error_to_json (err : response_error) : Yojson.Safe.t =
  let fields =
    [ ("code", `Int err.code); ("message", `String err.message) ]
    @ match err.data with None -> [] | Some d -> [ ("data", d) ]
  in
  `Assoc fields

(** Convert response to JSON *)
let response_to_json (resp : response) : Yojson.Safe.t =
  let fields = [ ("jsonrpc", `String "2.0"); ("id", resp.id) ] in
  let fields =
    match resp.result with
    | Some r -> fields @ [ ("result", r) ]
    | None -> fields
  in
  let fields =
    match resp.error with
    | Some e -> fields @ [ ("error", error_to_json e) ]
    | None -> fields
  in
  `Assoc fields

(** Write JSON with Content-Length header *)
let write_json (oc : Out_channel.t) (json : Yojson.Safe.t) : unit =
  let content = Yojson.Safe.to_string json in
  let header =
    Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length content)
  in
  Out_channel.output_string oc header;
  Out_channel.output_string oc content;
  Out_channel.flush oc

let write_response (oc : Out_channel.t) (resp : response) : unit =
  write_json oc (response_to_json resp)

let write_notification (oc : Out_channel.t) ~(method_ : string)
    ~(params : Yojson.Safe.t) : unit =
  let json =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("method", `String method_);
        ("params", params);
      ]
  in
  write_json oc json

let write_request (oc : Out_channel.t) ~(id : int) ~(method_ : string)
    ~(params : Yojson.Safe.t) : unit =
  let json =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", `Int id);
        ("method", `String method_);
        ("params", params);
      ]
  in
  write_json oc json

(** {1 Response Helpers} *)

let success_response ~(id : Yojson.Safe.t) ~(result : Yojson.Safe.t) : response
    =
  { id; result = Some result; error = None }

let error_response ~(id : Yojson.Safe.t) ~(code : int) ~(message : string)
    ?(data : Yojson.Safe.t option) () : response =
  { id; result = None; error = Some { code; message; data } }

(** {1 Debugging} *)

let message_to_string (msg : message) : string =
  let id_str =
    match msg.id with
    | None -> "notification"
    | Some id -> Printf.sprintf "id=%s" (Yojson.Safe.to_string id)
  in
  let params_str =
    match msg.params with
    | None -> ""
    | Some p -> Printf.sprintf " params=%s" (Yojson.Safe.to_string p)
  in
  Printf.sprintf "<%s method=%s%s>" id_str msg.method_ params_str

let response_to_string (resp : response) : string =
  let id_str = Yojson.Safe.to_string resp.id in
  match (resp.result, resp.error) with
  | Some r, None ->
      Printf.sprintf "<response id=%s result=%s>" id_str
        (Yojson.Safe.to_string r)
  | None, Some e ->
      Printf.sprintf "<response id=%s error=%d:%s>" id_str e.code e.message
  | _ -> Printf.sprintf "<response id=%s invalid>" id_str

let read_error_to_string (err : read_error) : string =
  match err with
  | Eof -> "End of input"
  | InvalidHeader msg -> Printf.sprintf "Invalid header: %s" msg
  | InvalidJson msg -> Printf.sprintf "Invalid JSON: %s" msg
  | InvalidMessage msg -> Printf.sprintf "Invalid message: %s" msg
