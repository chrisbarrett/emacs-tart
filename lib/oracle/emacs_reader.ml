(** Emacs subprocess invocation for oracle comparison.

    Runs Emacs in batch mode to read and print Elisp forms using
    [prin1-to-string], providing a gold-standard reference for parser testing.
*)

type emacs_error =
  | Read_error of { input : string; message : string }
  | Emacs_not_found
  | Emacs_failed of { exit_code : int; stderr : string }
  | Timeout of { timeout_ms : int }

(** {1 Emacs Discovery} *)

let find_emacs () : string option =
  let path = try Sys.getenv "PATH" with Not_found -> "" in
  let dirs = String.split_on_char ':' path in
  let rec search = function
    | [] -> None
    | dir :: rest ->
        let candidate = Filename.concat dir "emacs" in
        if Sys.file_exists candidate then Some candidate else search rest
  in
  search dirs

(** {1 Internal Helpers} *)

let read_all (ic : in_channel) : string =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  Buffer.contents buf

exception Timeout_exn

(** {1 Low-level Invocation} *)

let default_timeout_ms = 5000

let run_batch ?(timeout_ms = default_timeout_ms) (expr : string) :
    (string * string, emacs_error) result =
  match find_emacs () with
  | None -> Error Emacs_not_found
  | Some emacs_path -> (
      (* Build command: emacs --batch --quick --eval EXPR *)
      let cmd =
        Printf.sprintf "%s --batch --quick --eval %s" emacs_path
          (Filename.quote expr)
      in
      let prev_handler = ref Sys.Signal_default in
      let timed_out = ref false in
      try
        (* Set up alarm-based timeout *)
        let timeout_s = max 1 ((timeout_ms + 999) / 1000) in
        prev_handler :=
          Sys.signal Sys.sigalrm
            (Sys.Signal_handle
               (fun _ ->
                 timed_out := true;
                 raise Timeout_exn));
        ignore (Unix.alarm timeout_s);
        let stdout_ic, _stdin_oc, stderr_ic = Unix.open_process_full cmd [||] in
        let stdout_str = read_all stdout_ic in
        let stderr_str = read_all stderr_ic in
        (* Cancel alarm before closing process *)
        ignore (Unix.alarm 0);
        Sys.set_signal Sys.sigalrm !prev_handler;
        let status =
          Unix.close_process_full (stdout_ic, _stdin_oc, stderr_ic)
        in
        match status with
        | Unix.WEXITED 0 -> Ok (stdout_str, stderr_str)
        | Unix.WEXITED code ->
            Error (Emacs_failed { exit_code = code; stderr = stderr_str })
        | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
            Error (Emacs_failed { exit_code = -1; stderr = stderr_str })
      with
      | Timeout_exn ->
          ignore (Unix.alarm 0);
          Sys.set_signal Sys.sigalrm !prev_handler;
          Error (Timeout { timeout_ms })
      | Unix.Unix_error (Unix.ENOENT, _, _) ->
          ignore (Unix.alarm 0);
          Sys.set_signal Sys.sigalrm !prev_handler;
          Error Emacs_not_found
      | exn ->
          ignore (Unix.alarm 0);
          Sys.set_signal Sys.sigalrm !prev_handler;
          if !timed_out then Error (Timeout { timeout_ms })
          else
            Error
              (Emacs_failed { exit_code = -1; stderr = Printexc.to_string exn })
      )

(** {1 Reading} *)

let read_string ?(timeout_ms = default_timeout_ms) (input : string) :
    (string, emacs_error) result =
  (* Escape the input for embedding in an Elisp string literal *)
  let escaped =
    let buf = Buffer.create (String.length input * 2) in
    String.iter
      (fun c ->
        match c with
        | '\\' -> Buffer.add_string buf "\\\\"
        | '"' -> Buffer.add_string buf "\\\""
        | _ -> Buffer.add_char buf c)
      input;
    Buffer.contents buf
  in
  let expr = Printf.sprintf "(princ (prin1-to-string (read \"%s\")))" escaped in
  match run_batch ~timeout_ms expr with
  | Ok (stdout, _stderr) -> Ok stdout
  | Error e -> Error e

let read_file ?(timeout_ms = default_timeout_ms) (path : string) :
    (string list, emacs_error) result =
  (* Elisp script that reads all forms from a file and prints each one
     separated by a NUL byte delimiter *)
  (* Escape path for embedding in an Elisp string literal *)
  let escaped_path =
    let buf = Buffer.create (String.length path * 2) in
    String.iter
      (fun c ->
        match c with
        | '\\' -> Buffer.add_string buf "\\\\"
        | '"' -> Buffer.add_string buf "\\\""
        | _ -> Buffer.add_char buf c)
      path;
    Buffer.contents buf
  in
  let expr =
    Printf.sprintf
      {|(progn
  (with-temp-buffer
    (insert-file-contents "%s")
    (goto-char (point-min))
    (let ((forms nil))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (push (prin1-to-string form) forms)))
        (end-of-file nil))
      (princ (mapconcat #'identity (nreverse forms) "\0")))))|}
      escaped_path
  in
  match run_batch ~timeout_ms expr with
  | Ok (stdout, _stderr) ->
      if stdout = "" then Ok [] else Ok (String.split_on_char '\000' stdout)
  | Error e -> Error e
