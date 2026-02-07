(** Wall-clock timing with auto-scaled human-readable formatting. *)

type t = float

let start () = Unix.gettimeofday ()
let elapsed_s t = Unix.gettimeofday () -. t
let elapsed_ms t = elapsed_s t *. 1000.0

let format_duration secs =
  if secs >= 1.0 then Printf.sprintf "%.2fs" secs
  else
    let ms = secs *. 1000.0 in
    if ms >= 1.0 then Printf.sprintf "%.1fms" ms
    else
      let us = secs *. 1_000_000.0 in
      Printf.sprintf "%.0f\u{03BC}s" us
