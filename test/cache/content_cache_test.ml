(** Unit tests for Content_cache. *)

module Cache = Tart.Content_cache

(* =============================================================================
   Helpers
   ============================================================================= *)

(** Create a fresh temporary directory for cache isolation. *)
let make_temp_dir prefix =
  let base = Filename.get_temp_dir_name () in
  let name =
    Printf.sprintf "%s_%d_%f" prefix (Unix.getpid ()) (Unix.gettimeofday ())
  in
  let dir = Filename.concat base name in
  Unix.mkdir dir 0o755;
  dir

(** Write a temporary file with [contents] and return its path. *)
let write_temp_file ?(prefix = "cache_test") ?(suffix = ".dat") contents =
  let path = Filename.temp_file prefix suffix in
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc;
  path

(** Read file contents as a string. *)
let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.unsafe_to_string buf

(* =============================================================================
   cache_dir: XDG_CACHE_HOME and fallback
   ============================================================================= *)

let test_cache_dir_xdg () =
  let dir = make_temp_dir "xdg_test" in
  Unix.putenv "XDG_CACHE_HOME" dir;
  let result = Cache.cache_dir () in
  Alcotest.(check string) "XDG override" (Filename.concat dir "tart") result;
  (* Clean up env *)
  Unix.putenv "XDG_CACHE_HOME" ""

let test_cache_dir_fallback () =
  (* With XDG_CACHE_HOME empty, falls back to HOME/.cache/tart *)
  Unix.putenv "XDG_CACHE_HOME" "";
  let result = Cache.cache_dir () in
  let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
  let expected = Filename.concat (Filename.concat home ".cache") "tart" in
  Alcotest.(check string) "fallback to ~/.cache/tart" expected result

let cache_dir_tests =
  [
    ("XDG_CACHE_HOME override", `Quick, test_cache_dir_xdg);
    ("fallback to ~/.cache/tart", `Quick, test_cache_dir_fallback);
  ]

(* =============================================================================
   binary_path: returns valid path
   ============================================================================= *)

let test_binary_path_valid () =
  let bp = Cache.binary_path () in
  Alcotest.(check bool) "non-empty" true (String.length bp > 0);
  Alcotest.(check bool) "file exists" true (Sys.file_exists bp)

let binary_path_tests =
  [ ("returns valid executable path", `Quick, test_binary_path_valid) ]

(* =============================================================================
   compute_key: deterministic, different inputs differ, hex string
   ============================================================================= *)

let test_compute_key_deterministic () =
  let binary = write_temp_file "binary_contents" in
  let input = write_temp_file "input_contents" in
  let k1 = Cache.compute_key ~binary ~input ~deps:[] in
  let k2 = Cache.compute_key ~binary ~input ~deps:[] in
  Alcotest.(check string) "same inputs = same key" k1 k2

let test_compute_key_different_inputs () =
  let binary = write_temp_file "binary_contents" in
  let input1 = write_temp_file "input_a" in
  let input2 = write_temp_file "input_b" in
  let k1 = Cache.compute_key ~binary ~input:input1 ~deps:[] in
  let k2 = Cache.compute_key ~binary ~input:input2 ~deps:[] in
  Alcotest.(check bool) "different inputs = different keys" true (k1 <> k2)

let test_compute_key_different_binary () =
  let binary1 = write_temp_file "binary_a" in
  let binary2 = write_temp_file "binary_b" in
  let input = write_temp_file "input_contents" in
  let k1 = Cache.compute_key ~binary:binary1 ~input ~deps:[] in
  let k2 = Cache.compute_key ~binary:binary2 ~input ~deps:[] in
  Alcotest.(check bool) "different binaries = different keys" true (k1 <> k2)

let test_compute_key_hex_string () =
  let binary = write_temp_file "binary" in
  let input = write_temp_file "input" in
  let key = Cache.compute_key ~binary ~input ~deps:[] in
  Alcotest.(check int) "32-char hex" 32 (String.length key);
  let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') in
  String.iter
    (fun c ->
      Alcotest.(check bool) (Printf.sprintf "hex char '%c'" c) true (is_hex c))
    key

let test_compute_key_unreadable () =
  let binary = "/nonexistent/path/binary" in
  let input = write_temp_file "input" in
  let key = Cache.compute_key ~binary ~input ~deps:[] in
  Alcotest.(check string) "unreadable binary = empty key" "" key

let test_compute_key_deps_change_key () =
  let binary = write_temp_file "binary_contents" in
  let input = write_temp_file "input_contents" in
  let dep = write_temp_file "dep_contents" in
  let k_no_deps = Cache.compute_key ~binary ~input ~deps:[] in
  let k_with_deps = Cache.compute_key ~binary ~input ~deps:[ dep ] in
  Alcotest.(check bool) "deps change key" true (k_no_deps <> k_with_deps)

let test_compute_key_deps_deterministic () =
  let binary = write_temp_file "binary_contents" in
  let input = write_temp_file "input_contents" in
  let dep1 = write_temp_file "dep_a" in
  let dep2 = write_temp_file "dep_b" in
  let k1 = Cache.compute_key ~binary ~input ~deps:[ dep1; dep2 ] in
  let k2 = Cache.compute_key ~binary ~input ~deps:[ dep2; dep1 ] in
  Alcotest.(check string) "dep order doesn't matter" k1 k2

let test_compute_key_different_deps_differ () =
  let binary = write_temp_file "binary_contents" in
  let input = write_temp_file "input_contents" in
  let dep_a = write_temp_file "dep_version_1" in
  let dep_b = write_temp_file "dep_version_2" in
  let k1 = Cache.compute_key ~binary ~input ~deps:[ dep_a ] in
  let k2 = Cache.compute_key ~binary ~input ~deps:[ dep_b ] in
  Alcotest.(check bool) "different deps = different keys" true (k1 <> k2)

let test_compute_key_unreadable_dep_skipped () =
  let binary = write_temp_file "binary_contents" in
  let input = write_temp_file "input_contents" in
  let dep = write_temp_file "dep_contents" in
  let k1 = Cache.compute_key ~binary ~input ~deps:[ dep ] in
  let k2 =
    Cache.compute_key ~binary ~input ~deps:[ dep; "/nonexistent/dep/file" ]
  in
  Alcotest.(check string) "unreadable dep skipped" k1 k2

let compute_key_tests =
  [
    ("deterministic", `Quick, test_compute_key_deterministic);
    ("different inputs differ", `Quick, test_compute_key_different_inputs);
    ("different binaries differ", `Quick, test_compute_key_different_binary);
    ("32-char hex string", `Quick, test_compute_key_hex_string);
    ("unreadable file = empty key", `Quick, test_compute_key_unreadable);
    ("deps change key", `Quick, test_compute_key_deps_change_key);
    ("dep order doesn't matter", `Quick, test_compute_key_deps_deterministic);
    ("different deps differ", `Quick, test_compute_key_different_deps_differ);
    ("unreadable dep skipped", `Quick, test_compute_key_unreadable_dep_skipped);
  ]

(* =============================================================================
   store + retrieve round-trip
   ============================================================================= *)

let with_cache_dir f =
  let dir = make_temp_dir "cache_rt" in
  Unix.putenv "XDG_CACHE_HOME" dir;
  let result = f () in
  Unix.putenv "XDG_CACHE_HOME" "";
  result

let test_store_retrieve_roundtrip () =
  with_cache_dir (fun () ->
      let key = "abcdef0123456789abcdef0123456789" in
      Cache.store ~key ~data:"hello world";
      let result = Cache.retrieve ~key in
      Alcotest.(check (option string)) "round-trip" (Some "hello world") result)

let test_retrieve_miss () =
  with_cache_dir (fun () ->
      let result = Cache.retrieve ~key:"0000000000000000ffffffffffffffff" in
      Alcotest.(check (option string)) "miss = None" None result)

let test_retrieve_corrupted () =
  with_cache_dir (fun () ->
      (* Store something, then overwrite with garbage *)
      let key = "deadbeef0123456789abcdef01234567" in
      Cache.store ~key ~data:"good data";
      (* Find the cache file and corrupt it *)
      let cache_root = Cache.cache_dir () in
      let prefix = String.sub key 0 2 in
      let path =
        Filename.concat
          (Filename.concat (Filename.concat cache_root "v1") prefix)
          (key ^ ".json")
      in
      let oc = open_out path in
      output_string oc "not valid json {{{";
      close_out oc;
      let result = Cache.retrieve ~key in
      Alcotest.(check (option string)) "corrupted = None" None result)

let test_store_creates_dirs () =
  with_cache_dir (fun () ->
      let key = "11223344556677889900aabbccddeeff" in
      Cache.store ~key ~data:"test data";
      let result = Cache.retrieve ~key in
      Alcotest.(check (option string)) "dirs created" (Some "test data") result)

let test_store_overwrite () =
  with_cache_dir (fun () ->
      let key = "aabb000000000000000000000000ccdd" in
      Cache.store ~key ~data:"first";
      Cache.store ~key ~data:"second";
      let result = Cache.retrieve ~key in
      Alcotest.(check (option string)) "overwritten" (Some "second") result)

let store_retrieve_tests =
  [
    ("round-trip", `Quick, test_store_retrieve_roundtrip);
    ("miss = None", `Quick, test_retrieve_miss);
    ("corrupted = None", `Quick, test_retrieve_corrupted);
    ("creates directories on demand", `Quick, test_store_creates_dirs);
    ("overwrite replaces data", `Quick, test_store_overwrite);
  ]

(* =============================================================================
   JSON format: version, created_at, data fields
   ============================================================================= *)

let test_json_format () =
  with_cache_dir (fun () ->
      let key = "ff00ff00ff00ff00ff00ff00ff00ff00" in
      Cache.store ~key ~data:"format test";
      (* Read the raw file *)
      let cache_root = Cache.cache_dir () in
      let prefix = String.sub key 0 2 in
      let path =
        Filename.concat
          (Filename.concat (Filename.concat cache_root "v1") prefix)
          (key ^ ".json")
      in
      let contents = read_file path in
      let json = Yojson.Safe.from_string contents in
      match json with
      | `Assoc fields -> (
          (* version field *)
          (match List.assoc_opt "version" fields with
          | Some (`Int 1) -> ()
          | _ -> Alcotest.fail "missing or wrong version field");
          (* created_at field *)
          (match List.assoc_opt "created_at" fields with
          | Some (`String ts) ->
              Alcotest.(check bool)
                "ISO 8601 format" true
                (String.length ts > 0 && ts.[String.length ts - 1] = 'Z')
          | _ -> Alcotest.fail "missing created_at field");
          (* data field *)
          match List.assoc_opt "data" fields with
          | Some (`String d) ->
              Alcotest.(check string) "data preserved" "format test" d
          | _ -> Alcotest.fail "missing data field")
      | _ -> Alcotest.fail "not a JSON object")

let test_json_stored_under_v1 () =
  with_cache_dir (fun () ->
      let key = "ee11ee11ee11ee11ee11ee11ee11ee11" in
      Cache.store ~key ~data:"v1 test";
      let cache_root = Cache.cache_dir () in
      let v1_dir = Filename.concat cache_root "v1" in
      Alcotest.(check bool) "v1 dir exists" true (Sys.file_exists v1_dir);
      let prefix_dir = Filename.concat v1_dir (String.sub key 0 2) in
      Alcotest.(check bool)
        "prefix dir exists" true
        (Sys.file_exists prefix_dir))

let json_format_tests =
  [
    ("valid JSON with version, created_at, data", `Quick, test_json_format);
    ("stored under v1/ directory", `Quick, test_json_stored_under_v1);
  ]

(* =============================================================================
   evict_older_than: old files deleted, new files preserved
   ============================================================================= *)

let test_evict_old_files () =
  with_cache_dir (fun () ->
      (* Store two entries *)
      let old_key = "0000111122223333444455556666aaaa" in
      let new_key = "aaaa666655554444333322221111bbbb" in
      Cache.store ~key:old_key ~data:"old";
      Cache.store ~key:new_key ~data:"new";
      (* Backdate the old entry *)
      let cache_root = Cache.cache_dir () in
      let old_prefix = String.sub old_key 0 2 in
      let old_path =
        Filename.concat
          (Filename.concat (Filename.concat cache_root "v1") old_prefix)
          (old_key ^ ".json")
      in
      let past = Unix.gettimeofday () -. (31.0 *. 86400.0) in
      Unix.utimes old_path past past;
      (* Evict entries older than 30 days *)
      Cache.evict_older_than ~days:30;
      (* Old entry should be gone, new entry preserved *)
      let old_result = Cache.retrieve ~key:old_key in
      let new_result = Cache.retrieve ~key:new_key in
      Alcotest.(check (option string)) "old evicted" None old_result;
      Alcotest.(check (option string)) "new preserved" (Some "new") new_result)

let test_evict_removes_empty_dirs () =
  with_cache_dir (fun () ->
      (* Store and backdate a single entry *)
      let key = "cc00cc00cc00cc00cc00cc00cc00cc00" in
      Cache.store ~key ~data:"ephemeral";
      let cache_root = Cache.cache_dir () in
      let prefix = String.sub key 0 2 in
      let path =
        Filename.concat
          (Filename.concat (Filename.concat cache_root "v1") prefix)
          (key ^ ".json")
      in
      let past = Unix.gettimeofday () -. (31.0 *. 86400.0) in
      Unix.utimes path past past;
      Cache.evict_older_than ~days:30;
      let prefix_dir =
        Filename.concat (Filename.concat cache_root "v1") prefix
      in
      Alcotest.(check bool)
        "prefix dir removed" false
        (Sys.file_exists prefix_dir))

let test_evict_preserves_recent () =
  with_cache_dir (fun () ->
      let key = "dd00dd00dd00dd00dd00dd00dd00dd00" in
      Cache.store ~key ~data:"recent";
      Cache.evict_older_than ~days:30;
      let result = Cache.retrieve ~key in
      Alcotest.(check (option string)) "recent preserved" (Some "recent") result)

let eviction_tests =
  [
    ("old files deleted, new preserved", `Quick, test_evict_old_files);
    ("empty prefix dirs removed", `Quick, test_evict_removes_empty_dirs);
    ("recent entries preserved", `Quick, test_evict_preserves_recent);
  ]

(* =============================================================================
   maybe_evict: marker file and cooldown
   ============================================================================= *)

let test_maybe_evict_runs () =
  with_cache_dir (fun () ->
      (* Store and backdate an entry *)
      let key = "bbbb000000000000000000000000cccc" in
      Cache.store ~key ~data:"old data";
      let cache_root = Cache.cache_dir () in
      let prefix = String.sub key 0 2 in
      let path =
        Filename.concat
          (Filename.concat (Filename.concat cache_root "v1") prefix)
          (key ^ ".json")
      in
      let past = Unix.gettimeofday () -. (31.0 *. 86400.0) in
      Unix.utimes path past past;
      (* Run maybe_evict — should evict the old entry *)
      Cache.maybe_evict ();
      let result = Cache.retrieve ~key in
      Alcotest.(check (option string)) "evicted by maybe_evict" None result)

let test_maybe_evict_creates_marker () =
  with_cache_dir (fun () ->
      Cache.maybe_evict ();
      let marker = Filename.concat (Cache.cache_dir ()) ".last-eviction" in
      Alcotest.(check bool) "marker exists" true (Sys.file_exists marker))

let test_maybe_evict_skips_recent () =
  with_cache_dir (fun () ->
      (* First call creates marker *)
      Cache.maybe_evict ();
      (* Store and backdate an entry *)
      let key = "eeee000000000000000000000000ffff" in
      Cache.store ~key ~data:"should survive";
      let cache_root = Cache.cache_dir () in
      let prefix = String.sub key 0 2 in
      let path =
        Filename.concat
          (Filename.concat (Filename.concat cache_root "v1") prefix)
          (key ^ ".json")
      in
      let past = Unix.gettimeofday () -. (31.0 *. 86400.0) in
      Unix.utimes path past past;
      (* Second call should skip (marker is recent) *)
      Cache.maybe_evict ();
      (* The old entry should still be there since eviction was skipped *)
      let result = Cache.retrieve ~key in
      Alcotest.(check (option string))
        "eviction skipped" (Some "should survive") result)

let maybe_evict_tests =
  [
    ("runs and evicts old entries", `Quick, test_maybe_evict_runs);
    ("creates marker file", `Quick, test_maybe_evict_creates_marker);
    ("skips when marker is recent", `Quick, test_maybe_evict_skips_recent);
  ]

(* =============================================================================
   Best-effort: errors don't propagate
   ============================================================================= *)

let test_store_nonexistent_parent () =
  (* store with invalid XDG path shouldn't raise *)
  Unix.putenv "XDG_CACHE_HOME" "/nonexistent/path/that/cant/exist";
  Cache.store ~key:"test_key_1234567890abcdef12345678" ~data:"test";
  Unix.putenv "XDG_CACHE_HOME" "";
  (* If we get here, no exception was raised *)
  Alcotest.(check pass) "store didn't raise" () ()

let test_retrieve_never_raises () =
  Unix.putenv "XDG_CACHE_HOME" "/nonexistent/path";
  let result = Cache.retrieve ~key:"0000000000000000aaaaaaaaaaaaaaaa" in
  Unix.putenv "XDG_CACHE_HOME" "";
  Alcotest.(check (option string)) "retrieve = None" None result

let test_evict_nonexistent_dir () =
  Unix.putenv "XDG_CACHE_HOME" "/nonexistent/path/for/evict";
  Cache.evict_older_than ~days:1;
  Unix.putenv "XDG_CACHE_HOME" "";
  Alcotest.(check pass) "evict didn't raise" () ()

let test_maybe_evict_never_raises () =
  Unix.putenv "XDG_CACHE_HOME" "/nonexistent/path/for/maybe";
  Cache.maybe_evict ();
  Unix.putenv "XDG_CACHE_HOME" "";
  Alcotest.(check pass) "maybe_evict didn't raise" () ()

let best_effort_tests =
  [
    ("store with bad path doesn't raise", `Quick, test_store_nonexistent_parent);
    ("retrieve never raises", `Quick, test_retrieve_never_raises);
    ( "evict on nonexistent dir doesn't raise",
      `Quick,
      test_evict_nonexistent_dir );
    ("maybe_evict never raises", `Quick, test_maybe_evict_never_raises);
  ]

(* =============================================================================
   Runner
   ============================================================================= *)

let () =
  Alcotest.run "Content_cache"
    [
      ("cache_dir", cache_dir_tests);
      ("binary_path", binary_path_tests);
      ("compute_key", compute_key_tests);
      ("store + retrieve", store_retrieve_tests);
      ("JSON format", json_format_tests);
      ("eviction", eviction_tests);
      ("maybe_evict", maybe_evict_tests);
      ("best-effort", best_effort_tests);
    ]
