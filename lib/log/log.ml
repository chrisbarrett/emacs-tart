(** Structured logging with verbosity levels and switchable output format.

    See {!Log} for interface documentation. *)

type level = Quiet | Normal | Verbose | Debug
type format = Text | Json

let current_level = ref Normal
let current_format = ref Text
let set_level l = current_level := l
let set_format f = current_format := f
let level () = !current_level
let level_int = function Quiet -> 0 | Normal -> 1 | Verbose -> 2 | Debug -> 3

let level_tag = function
  | Quiet -> "quiet"
  | Normal -> "info"
  | Verbose -> "verbose"
  | Debug -> "debug"

let emit_text tag msg = Printf.eprintf "[%s] %s\n" tag msg

let emit_json tag msg ctx =
  let base = [ ("level", `String tag); ("msg", `String msg) ] in
  let obj =
    match ctx with
    | [] -> `Assoc base
    | pairs ->
        let ctx_obj = `Assoc (List.map (fun (k, v) -> (k, `String v)) pairs) in
        `Assoc (base @ [ ("ctx", ctx_obj) ])
  in
  Printf.eprintf "%s\n" (Yojson.Basic.to_string obj)

let emit lvl msg ctx =
  match !current_format with
  | Text -> emit_text (level_tag lvl) msg
  | Json -> emit_json (level_tag lvl) msg ctx

(** Zero-cost gated logging. When the level is below threshold,
    [Format.ifprintf] discards format arguments without evaluation. When
    enabled, [Format.kasprintf] captures the formatted message. *)
let gated_log min_level fmt =
  if level_int !current_level >= level_int min_level then
    Format.kasprintf (fun msg -> emit min_level msg []) fmt
  else Format.ifprintf Format.err_formatter fmt

let info fmt = gated_log Normal fmt
let verbose fmt = gated_log Verbose fmt
let debug fmt = gated_log Debug fmt

let debug_with_ctx ctx msg =
  if level_int !current_level >= level_int Debug then emit Debug msg ctx

let flush () = Stdlib.flush stderr
