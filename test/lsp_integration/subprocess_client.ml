(** Subprocess-based LSP client for integration testing. *)

type t = {
  pid : int;
  to_stdin : Unix.file_descr;
  from_stdout : Unix.file_descr;
  ic : in_channel;
}

let start ~tart_bin =
  let stdin_read, stdin_write = Unix.pipe ~cloexec:true () in
  let stdout_read, stdout_write = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process tart_bin [| tart_bin; "lsp" |] stdin_read stdout_write
      Unix.stderr
  in
  Unix.close stdin_read;
  Unix.close stdout_write;
  let ic = Unix.in_channel_of_descr stdout_read in
  { pid; to_stdin = stdin_write; from_stdout = stdout_read; ic }

let send t msg =
  let len = String.length msg in
  let written = Unix.write_substring t.to_stdin msg 0 len in
  if written <> len then
    failwith
      (Printf.sprintf "subprocess_client: short write (%d/%d)" written len)

let read_byte_with_timeout fd ~timeout_s =
  let buf = Bytes.create 1 in
  match Unix.select [ fd ] [] [] timeout_s with
  | [], _, _ -> None
  | _ ->
      let n = Unix.read fd buf 0 1 in
      if n = 0 then None else Some (Bytes.get buf 0)

let recv_with_timeout t ~timeout_s =
  (* Read "Content-Length: <n>\r\n\r\n" header *)
  let buf = Buffer.create 128 in
  let rec read_header () =
    match read_byte_with_timeout t.from_stdout ~timeout_s with
    | None -> None
    | Some c ->
        Buffer.add_char buf c;
        let s = Buffer.contents buf in
        if String.length s >= 4 && String.ends_with ~suffix:"\r\n\r\n" s then
          Some s
        else read_header ()
  in
  match read_header () with
  | None -> None
  | Some header ->
      (* Parse Content-Length *)
      let re = Str.regexp "Content-Length: \\([0-9]+\\)" in
      if not (Str.string_match re header 0) then
        failwith (Printf.sprintf "subprocess_client: bad header: %S" header);
      let content_length = int_of_string (Str.matched_group 1 header) in
      (* Read exactly content_length bytes *)
      let body = Bytes.create content_length in
      let rec read_body offset =
        if offset >= content_length then ()
        else
          let n =
            Unix.read t.from_stdout body offset (content_length - offset)
          in
          if n = 0 then
            failwith "subprocess_client: unexpected EOF reading body"
          else read_body (offset + n)
      in
      read_body 0;
      Some (Yojson.Safe.from_string (Bytes.to_string body))

let recv t =
  match recv_with_timeout t ~timeout_s:10.0 with
  | Some msg -> msg
  | None -> failwith "subprocess_client: timeout waiting for message"

let recv_all t ~timeout_ms =
  let timeout_s = Float.of_int timeout_ms /. 1000.0 in
  let rec loop acc =
    match recv_with_timeout t ~timeout_s with
    | Some msg -> loop (msg :: acc)
    | None -> List.rev acc
  in
  loop []

let shutdown t =
  Unix.close t.to_stdin;
  let _, status = Unix.waitpid [] t.pid in
  (try close_in_noerr t.ic with _ -> ());
  match status with
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED n -> 128 + n
  | Unix.WSTOPPED n -> 128 + n
