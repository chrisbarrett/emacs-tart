(** Verbose logging utility for coverage commands.

    Provides conditional logging to stderr with a [verbose] prefix. Used by
    coverage and emacs-coverage commands for debugging output.

    See Spec 30 for requirements. *)

(** Log a message to stderr when verbose mode is enabled.

    Output format: [verbose] <message>

    Usage:
    {[
      verbose_log verbose "Typings root: %s" path
    ]} *)
let verbose_log (verbose : bool) fmt =
  if verbose then
    Printf.kfprintf (fun _ -> ()) stderr ("[verbose] " ^^ fmt ^^ "\n")
  else Printf.ifprintf stderr fmt

(** Log a message unconditionally (always outputs regardless of verbose flag).

    Useful for errors or warnings that should always be shown. *)
let always_log fmt = Printf.eprintf ("[verbose] " ^^ fmt ^^ "\n")

(** Flush stderr to ensure all verbose output is written. *)
let flush () = flush stderr
