(** GC and memory statistics with human-readable formatting. *)

let word_size = float_of_int (Sys.word_size / 8)

type snapshot = {
  minor_words : float;
  major_words : float;
  minor_collections : int;
  major_collections : int;
}

type delta = { alloc_words : float; minor_gcs : int; major_gcs : int }

let snapshot () =
  let s = Gc.quick_stat () in
  {
    minor_words = s.minor_words;
    major_words = s.major_words;
    minor_collections = s.minor_collections;
    major_collections = s.major_collections;
  }

let diff ~before ~after =
  let alloc_words =
    after.minor_words -. before.minor_words +. after.major_words
    -. before.major_words
  in
  {
    alloc_words;
    minor_gcs = after.minor_collections - before.minor_collections;
    major_gcs = after.major_collections - before.major_collections;
  }

let format_bytes n =
  let abs_n = Float.abs n in
  if abs_n >= 1_073_741_824.0 then Printf.sprintf "%.1fGB" (n /. 1_073_741_824.0)
  else if abs_n >= 1_048_576.0 then Printf.sprintf "%.1fMB" (n /. 1_048_576.0)
  else if abs_n >= 1024.0 then Printf.sprintf "%.1fKB" (n /. 1024.0)
  else Printf.sprintf "%.0fB" n

let format_delta d =
  let alloc_bytes = d.alloc_words *. word_size in
  let parts = [ Printf.sprintf "%s alloc" (format_bytes alloc_bytes) ] in
  let parts =
    if d.minor_gcs > 0 then parts @ [ Printf.sprintf "%d minor GC" d.minor_gcs ]
    else parts
  in
  let parts =
    if d.major_gcs > 0 then parts @ [ Printf.sprintf "%d major GC" d.major_gcs ]
    else parts
  in
  String.concat ", " parts

let format_summary () =
  let s = Gc.stat () in
  let heap_bytes = float_of_int s.heap_words *. word_size in
  let alloc_bytes = (s.minor_words +. s.major_words) *. word_size in
  Printf.sprintf "Heap: %s, Minor GC: %d, Major GC: %d, Total alloc: %s"
    (format_bytes heap_bytes) s.minor_collections s.major_collections
    (format_bytes alloc_bytes)
