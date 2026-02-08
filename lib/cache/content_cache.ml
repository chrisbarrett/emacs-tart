module Log = Tart_log.Log

let cache_dir () =
  let base =
    match Sys.getenv_opt "XDG_CACHE_HOME" with
    | Some dir when dir <> "" -> dir
    | _ ->
        let home =
          match Sys.getenv_opt "HOME" with Some h -> h | None -> "."
        in
        Filename.concat home ".cache"
  in
  Filename.concat base "tart"

let binary_path () =
  try Unix.realpath Sys.executable_name
  with Unix.Unix_error _ -> Sys.executable_name

let read_file_contents path =
  try
    let ic = open_in_bin path in
    let n = in_channel_length ic in
    let buf = Bytes.create n in
    really_input ic buf 0 n;
    close_in ic;
    Some (Bytes.unsafe_to_string buf)
  with _ -> None

let compute_key ~binary ~input ~deps =
  match (read_file_contents binary, read_file_contents input) with
  | Some b, Some i ->
      let dep_contents =
        List.sort String.compare deps |> List.filter_map read_file_contents
      in
      let combined = String.concat "" (b :: i :: dep_contents) in
      Digest.to_hex (Digest.string combined)
  | _ -> ""

(* Two-char prefix from key *)
let prefix_of_key key =
  if String.length key >= 2 then String.sub key 0 2 else key

(* v1/XX/XXXX...XX.json *)
let entry_path key =
  let dir = cache_dir () in
  let prefix = prefix_of_key key in
  Filename.concat
    (Filename.concat (Filename.concat dir "v1") prefix)
    (key ^ ".json")

(* Recursively create directories *)
let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let iso8601_now () =
  let t = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (t.tm_year + 1900)
    (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec

let store ~key ~data =
  try
    let path = entry_path key in
    let dir = Filename.dirname path in
    mkdir_p dir;
    let envelope =
      `Assoc
        [
          ("version", `Int 1);
          ("created_at", `String (iso8601_now ()));
          ("data", `String data);
        ]
    in
    let json_str = Yojson.Safe.to_string envelope in
    let tmp = path ^ ".tmp." ^ string_of_int (Unix.getpid ()) in
    let oc = open_out_bin tmp in
    output_string oc json_str;
    close_out oc;
    Unix.rename tmp path
  with exn -> Log.debug "cache store failed: %s" (Printexc.to_string exn)

let retrieve ~key =
  try
    let path = entry_path key in
    match read_file_contents path with
    | None -> None
    | Some contents -> (
        let json = Yojson.Safe.from_string contents in
        match json with
        | `Assoc fields -> (
            match List.assoc_opt "data" fields with
            | Some (`String data) -> Some data
            | _ -> None)
        | _ -> None)
  with _ -> None

let evict_older_than ~days =
  let now = Unix.gettimeofday () in
  let threshold = now -. (float_of_int days *. 86400.0) in
  let v1_dir = Filename.concat (cache_dir ()) "v1" in
  if Sys.file_exists v1_dir then
    try
      let prefix_dirs = Sys.readdir v1_dir in
      Array.iter
        (fun prefix ->
          let prefix_path = Filename.concat v1_dir prefix in
          if try Sys.is_directory prefix_path with Sys_error _ -> false then begin
            (try
               let files = Sys.readdir prefix_path in
               Array.iter
                 (fun file ->
                   if Filename.check_suffix file ".json" then
                     let file_path = Filename.concat prefix_path file in
                     try
                       let stat = Unix.stat file_path in
                       if stat.Unix.st_mtime < threshold then (
                         Unix.unlink file_path;
                         Log.debug "cache: evicted %s" file)
                     with exn ->
                       Log.debug "cache: evict failed for %s: %s" file
                         (Printexc.to_string exn))
                 files
             with exn ->
               Log.debug "cache: readdir failed for %s: %s" prefix
                 (Printexc.to_string exn));
            (* Remove empty prefix directory *)
            try
              let remaining = Sys.readdir prefix_path in
              if Array.length remaining = 0 then Unix.rmdir prefix_path
            with _ -> ()
          end)
        prefix_dirs
    with exn ->
      Log.debug "cache: eviction scan failed: %s" (Printexc.to_string exn)

let maybe_evict () =
  try
    let dir = cache_dir () in
    let marker = Filename.concat dir ".last-eviction" in
    let now = Unix.gettimeofday () in
    let should_run =
      try
        let stat = Unix.stat marker in
        now -. stat.Unix.st_mtime > 3600.0
      with Unix.Unix_error (Unix.ENOENT, _, _) -> true
    in
    if should_run then begin
      evict_older_than ~days:30;
      (* Touch marker file *)
      mkdir_p dir;
      let oc = open_out marker in
      close_out oc
    end
  with exn ->
    Log.debug "cache: maybe_evict failed: %s" (Printexc.to_string exn)
