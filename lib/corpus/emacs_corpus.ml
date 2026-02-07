module Log = Tart_log.Log

(** {1 Error Types} *)

type corpus_error =
  | Clone_failed of string
  | Fetch_failed of string
  | Checkout_failed of string
  | No_emacs
  | Invalid_ref of string

let corpus_error_to_string = function
  | Clone_failed msg -> Printf.sprintf "Clone failed: %s" msg
  | Fetch_failed msg -> Printf.sprintf "Fetch failed: %s" msg
  | Checkout_failed msg -> Printf.sprintf "Checkout failed: %s" msg
  | No_emacs -> "Emacs not found on PATH"
  | Invalid_ref ref -> Printf.sprintf "Invalid ref: %s" ref

let emacs_repo_url = "https://git.savannah.gnu.org/git/emacs.git"

(** {1 Path Resolution} *)

let corpus_dir () =
  Filename.concat (Cache.Content_cache.cache_dir ()) "emacs-src"

let is_cloned () = Sys.file_exists (corpus_dir ())

(** {1 Git Operations} *)

let run_git ?cwd args =
  let cmd =
    match cwd with
    | Some dir ->
        Printf.sprintf "git -C %s %s 2>&1" (Filename.quote dir)
          (String.concat " " (List.map Filename.quote args))
    | None ->
        Printf.sprintf "git %s 2>&1"
          (String.concat " " (List.map Filename.quote args))
  in
  Log.debug "corpus: running %s" cmd;
  try
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 4096 in
    (try
       while true do
         Buffer.add_channel buf ic 1
       done
     with End_of_file -> ());
    let output = Buffer.contents buf in
    let status = Unix.close_process_in ic in
    match status with
    | Unix.WEXITED 0 ->
        Log.debug "corpus: git ok";
        Ok (String.trim output)
    | Unix.WEXITED code ->
        let msg =
          Printf.sprintf "git exited %d: %s" code (String.trim output)
        in
        Log.debug "corpus: %s" msg;
        (* Classify based on first arg *)
        let err =
          match args with
          | "clone" :: _ -> Clone_failed msg
          | "fetch" :: _ -> Fetch_failed msg
          | "checkout" :: _ -> Checkout_failed msg
          | _ -> Fetch_failed msg
        in
        Error err
    | Unix.WSIGNALED n ->
        Error (Fetch_failed (Printf.sprintf "git killed by signal %d" n))
    | Unix.WSTOPPED n ->
        Error (Fetch_failed (Printf.sprintf "git stopped by signal %d" n))
  with
  | Unix.Unix_error (err, _, _) ->
      Error
        (Fetch_failed
           (Printf.sprintf "exec failed: %s" (Unix.error_message err)))
  | exn ->
      Error
        (Fetch_failed
           (Printf.sprintf "unexpected error: %s" (Printexc.to_string exn)))

let ensure_clone ?(url = emacs_repo_url) ~tag () =
  let dir = corpus_dir () in
  if Sys.file_exists dir then (
    Log.verbose "corpus: reusing existing clone at %s" dir;
    Ok ())
  else (
    Log.verbose "corpus: cloning %s (tag %s) to %s" url tag dir;
    match run_git [ "clone"; "--depth"; "1"; "--branch"; tag; url; dir ] with
    | Ok _ -> Ok ()
    | Error _ as err -> err)

let is_sha s =
  let len = String.length s in
  len >= 7 && len <= 40
  && String.to_seq s
     |> Seq.for_all (fun c ->
         (c >= '0' && c <= '9')
         || (c >= 'a' && c <= 'f')
         || (c >= 'A' && c <= 'F'))

let checkout ref_str =
  let dir = corpus_dir () in
  if not (Sys.file_exists dir) then
    Error (Checkout_failed "corpus not cloned; run checkout with a tag first")
  else if String.length ref_str = 0 then Error (Invalid_ref "empty ref")
  else
    (* Fetch the ref first *)
    let fetch_result =
      if is_sha ref_str then
        run_git ~cwd:dir [ "fetch"; "--depth"; "1"; "origin"; ref_str ]
      else
        run_git ~cwd:dir
          [ "fetch"; "--depth"; "1"; "origin"; "tag"; ref_str; "--no-tags" ]
    in
    match fetch_result with
    | Error _ as err -> err
    | Ok _ -> (
        (* Now checkout *)
        let checkout_ref = if is_sha ref_str then ref_str else ref_str in
        match run_git ~cwd:dir [ "checkout"; checkout_ref ] with
        | Ok _ -> Ok ()
        | Error _ as err -> err)

(** {1 Version Detection} *)

let version_to_tag (v : Sig.Emacs_version.version) =
  Printf.sprintf "emacs-%d.%d" v.major v.minor

let detect_tag () =
  match Sig.Emacs_version.detect () with
  | Sig.Emacs_version.Detected v ->
      let tag = version_to_tag v in
      Log.verbose "corpus: detected Emacs %s → tag %s"
        (Sig.Emacs_version.version_to_string v)
        tag;
      Ok tag
  | Sig.Emacs_version.NotFound -> Error No_emacs
  | Sig.Emacs_version.ParseError msg ->
      Error
        (Invalid_ref (Printf.sprintf "could not parse Emacs version: %s" msg))

(** {1 File Listing} *)

let list_el_files () =
  let dir = corpus_dir () in
  if not (Sys.file_exists dir) then []
  else
    let acc = ref [] in
    let rec walk path =
      try
        let entries = Sys.readdir path in
        Array.iter
          (fun entry ->
            let full = Filename.concat path entry in
            if try Sys.is_directory full with Sys_error _ -> false then
              walk full
            else if Filename.check_suffix entry ".el" then acc := full :: !acc)
          entries
      with Sys_error _ -> ()
    in
    walk dir;
    List.sort String.compare !acc

(** {1 Cache Management} *)

let dir_size path =
  let total = ref 0 in
  let rec walk p =
    try
      if Sys.is_directory p then begin
        let entries = Sys.readdir p in
        Array.iter (fun e -> walk (Filename.concat p e)) entries
      end
      else
        let stat = Unix.stat p in
        total := !total + stat.Unix.st_size
    with Sys_error _ | Unix.Unix_error _ -> ()
  in
  walk path;
  !total

let rec rm_rf path =
  try
    if Sys.is_directory path then begin
      let entries = Sys.readdir path in
      Array.iter (fun e -> rm_rf (Filename.concat path e)) entries;
      Unix.rmdir path
    end
    else Unix.unlink path
  with Sys_error _ | Unix.Unix_error _ -> ()

let clean () =
  let dir = corpus_dir () in
  if not (Sys.file_exists dir) then 0
  else
    let bytes = dir_size dir in
    Log.verbose "corpus: removing %s (%d bytes)" dir bytes;
    rm_rf dir;
    bytes
