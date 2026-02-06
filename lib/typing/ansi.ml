(** ANSI color codes and TTY detection for terminal output.

    Provides helpers for colorized terminal output that gracefully degrades to
    plain text when stdout is not a TTY (e.g., when piped).

    Colors per Spec 45 R11:
    - error: Red bold
    - warning: Yellow bold
    - hint: Cyan
    - Error code: Red
    - Location arrow: Blue
    - Line numbers: Blue dim
    - Underline carets: Red
    - Type names: Green
    - Help text: Cyan

    Syntax highlighting per Spec 45 R12:
    - Keywords: Magenta
    - Strings: Green
    - Comments: Gray/dim
    - Numbers: Cyan
    - Quoted symbols: Yellow *)

(** Whether colors are enabled for output. *)
let colors_enabled = ref None

(** Check if stdout is a TTY. *)
let is_tty () = try Unix.isatty Unix.stdout with Unix.Unix_error _ -> false

(** Initialize color detection. Call once at startup. *)
let init () =
  match !colors_enabled with
  | Some _ -> ()
  | None -> colors_enabled := Some (is_tty ())

(** Force colors on or off regardless of TTY detection. *)
let force_colors enable = colors_enabled := Some enable

(** Check if colors should be used. *)
let use_colors () =
  init ();
  match !colors_enabled with Some v -> v | None -> false

(** ANSI escape codes.

    Not all colors are used currently, but kept for completeness. *)
module Code = struct
  let reset = "\027[0m"
  let bold = "\027[1m"
  let dim = "\027[2m"

  (* Foreground colors *)
  let red = "\027[31m"
  let green = "\027[32m"
  let yellow = "\027[33m"
  let blue = "\027[34m"
  let magenta = "\027[35m"
  let cyan = "\027[36m"
  let gray = "\027[90m"
end

(** Apply a style to text, respecting color settings. *)
let style codes text =
  if use_colors () then String.concat "" codes ^ text ^ Code.reset else text

(** Semantic color functions for error output per Spec 45 R11. *)

let error text = style [ Code.red; Code.bold ] text
let hint text = style [ Code.cyan ] text
let location text = style [ Code.blue ] text
let line_number text = style [ Code.blue; Code.dim ] text
let underline text = style [ Code.red ] text
let type_name text = style [ Code.green ] text
let help text = style [ Code.cyan ] text

(** Syntax highlighting colors per Spec 45 R12. *)
let keyword text = style [ Code.magenta ] text

let string_lit text = style [ Code.green ] text
let comment text = style [ Code.gray ] text
let number text = style [ Code.cyan ] text
let quoted text = style [ Code.yellow ] text
