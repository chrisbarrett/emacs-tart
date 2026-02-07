(** Round-trip test harness.

    Verifies parser/printer consistency by checking that
    parse(print(parse(file))) = parse(file), and optionally that Emacs's native
    reader agrees with tart's output. *)

(** {1 Result Types} *)

type result =
  | Pass
  | Cached
  | Parse_error of { path : string; error : string }
  | Mismatch of {
      path : string;
      form_index : int;
      expected : string;
      actual : string;
      diff : string;
    }
  | Emacs_mismatch of {
      path : string;
      tart_output : string;
      emacs_output : string;
    }

type summary = {
  total : int;
  passed : int;
  failed : int;
  cached : int;
  failures : (string * result) list;
}

(** {1 Diff} *)

val make_diff : expected:string -> actual:string -> string
(** [make_diff ~expected ~actual] produces a line-by-line diff showing expected
    vs actual output. *)

(** {1 Per-File Checks} *)

val check_file : string -> result
(** [check_file path] parses [path], prints each form, reparses, and compares
    with {!Syntax.Sexp.equal}. Returns [Pass] if all forms round-trip,
    [Parse_error] on parse failure, or [Mismatch] on the first discrepancy. *)

val check_file_with_emacs : ?timeout_ms:int -> string -> result
(** [check_file_with_emacs ?timeout_ms path] runs Emacs oracle comparison via
    {!Oracle.Compare.compare_file}. Returns [Pass] if all forms match,
    [Emacs_mismatch] on the first discrepancy. *)

(** {1 Cached Checks} *)

val check_file_cached : ?no_cache:bool -> string -> result
(** [check_file_cached ?no_cache path] checks the cache first; on hit returns
    [Cached]. On miss, runs {!check_file} and records a pass. When [no_cache] is
    true, always runs the check. *)

(** {1 Corpus Run} *)

val run_corpus :
  ?no_cache:bool ->
  ?with_emacs:bool ->
  ?timeout_ms:int ->
  string list ->
  summary
(** [run_corpus ?no_cache ?with_emacs ?timeout_ms files] checks each file,
    collecting results into a summary. When [with_emacs] is true, also runs
    oracle verification on each file. *)

(** {1 Summary} *)

val summary_to_string : summary -> string
(** Format summary as "N total, N passed, N failed, N cached". *)
