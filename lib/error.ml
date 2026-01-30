(** Unified error type system for composable, machine-readable errors. *)

module Loc = Syntax.Location
module Diagnostic = Typing.Diagnostic

type t =
  | Type of Diagnostic.t
  | Parse of { message : string; span : Loc.span }
  | Eval of { message : string; span : Loc.span }
  | Io of { path : string; message : string }
  | Cli of { message : string; hint : string option }

let is_fatal = function
  | Type _ -> false (* Recoverable: collect all type errors *)
  | Parse _ -> false (* Recoverable: continue parsing next form *)
  | Eval _ -> false (* Recoverable: report and continue *)
  | Io _ -> true (* Fatal: cannot proceed without file *)
  | Cli _ -> true (* Fatal: invalid invocation *)

let location = function
  | Type d -> Some (Diagnostic.span d)
  | Parse { span; _ } -> Some span
  | Eval { span; _ } -> Some span
  | Io _ -> None
  | Cli _ -> None

let parse_error ~message ~span = Parse { message; span }
let eval_error ~message ~span = Eval { message; span }

let io_error ~path ~exn =
  let message =
    match exn with
    | Sys_error msg -> msg
    | Unix.Unix_error (err, _, _) -> Unix.error_message err
    | exn -> Printexc.to_string exn
  in
  Io { path; message }

let cli_error ~message ?hint () = Cli { message; hint }
let of_diagnostic d = Type d
let of_diagnostics ds = List.map of_diagnostic ds
