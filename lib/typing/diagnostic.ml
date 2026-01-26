(** Type error diagnostics for user-facing error messages.

    This module provides structured diagnostic information for type errors,
    including source locations, expected vs actual types, and related locations
    (provenance) for understanding where types originated.

    R10: Type error diagnostics - errors include:
    - Source location of the error
    - Expected type
    - Actual type
    - Related locations (e.g., where expected type originated) *)

module Types = Core.Types
module Loc = Syntax.Location

(** Severity level for diagnostics *)
type severity = Error | Warning | Hint

type related_location = { span : Loc.span; message : string }
(** A related location with context *)

type t = {
  severity : severity;
  span : Loc.span;  (** Primary location of the error *)
  message : string;  (** Main error message *)
  expected : Types.typ option;  (** Expected type (if applicable) *)
  actual : Types.typ option;  (** Actual type found (if applicable) *)
  related : related_location list;  (** Related locations with context *)
}
(** A structured diagnostic message *)

(** Create a type mismatch diagnostic *)
let type_mismatch ~span ~expected ~actual ?(related = []) () =
  let message =
    Printf.sprintf "Type mismatch: expected %s but found %s"
      (Types.to_string expected) (Types.to_string actual)
  in
  {
    severity = Error;
    span;
    message;
    expected = Some expected;
    actual = Some actual;
    related;
  }

(** Create an arity mismatch diagnostic *)
let arity_mismatch ~span ~expected ~actual ?(related = []) () =
  let message =
    Printf.sprintf "Arity mismatch: expected %d arguments but got %d" expected
      actual
  in
  { severity = Error; span; message; expected = None; actual = None; related }

(** Create an occurs check diagnostic *)
let occurs_check ~span ~tvar_id ~typ ?(related = []) () =
  let message =
    Printf.sprintf "Infinite type: type variable '_%d occurs in %s" tvar_id
      (Types.to_string typ)
  in
  {
    severity = Error;
    span;
    message;
    expected = None;
    actual = Some typ;
    related;
  }

(** Create a missing signature warning for a public function not in .tart file
*)
let missing_signature ~span ~name () =
  let message =
    Printf.sprintf "Function `%s` defined but not in signature file" name
  in
  {
    severity = Warning;
    span;
    message;
    expected = None;
    actual = None;
    related = [];
  }

(** Convert a unification error to a diagnostic *)
let of_unify_error (err : Unify.error) : t =
  match err with
  | Unify.TypeMismatch (expected, actual, span) ->
      type_mismatch ~span ~expected ~actual ()
  | Unify.OccursCheck (tvar_id, typ, span) ->
      occurs_check ~span ~tvar_id ~typ ()
  | Unify.ArityMismatch (expected, actual, span) ->
      arity_mismatch ~span ~expected ~actual ()

(** Convert a list of unification errors to diagnostics *)
let of_unify_errors (errors : Unify.error list) : t list =
  List.map of_unify_error errors

(** Format a source position for display *)
let format_pos (pos : Loc.pos) : string =
  if pos.file = "<generated>" then "<generated>"
  else Printf.sprintf "%s:%d:%d" pos.file pos.line (pos.col + 1)

(** Format a source span for display *)
let format_span (span : Loc.span) : string =
  let start = span.start_pos in
  let end_pos = span.end_pos in
  if start.file = end_pos.file && start.line = end_pos.line then
    Printf.sprintf "%s:%d:%d-%d" start.file start.line (start.col + 1)
      (end_pos.col + 1)
  else if start.file = end_pos.file then
    Printf.sprintf "%s:%d:%d-%d:%d" start.file start.line (start.col + 1)
      end_pos.line (end_pos.col + 1)
  else Printf.sprintf "%s-%s" (format_pos start) (format_pos end_pos)

(** Format severity for display *)
let format_severity = function
  | Error -> "error"
  | Warning -> "warning"
  | Hint -> "hint"

(** Format a diagnostic as a human-readable string.

    Output format (similar to rustc/clang):
    {[
       file.el:10:5: error: Type mismatch: expected Int but found String
         |
      10 |   (+ x "hello")
         |        ^^^^^^^
         |
         = expected: Int
         = found: String
         = note: expected type from function signature at file.el:5:1
    ]} *)
let to_string (d : t) : string =
  let buf = Buffer.create 256 in

  (* Main error line *)
  Buffer.add_string buf (format_span d.span);
  Buffer.add_string buf ": ";
  Buffer.add_string buf (format_severity d.severity);
  Buffer.add_string buf ": ";
  Buffer.add_string buf d.message;

  (* Type information *)
  (match d.expected with
  | Some ty ->
      Buffer.add_string buf "\n  = expected: ";
      Buffer.add_string buf (Types.to_string ty)
  | None -> ());
  (match d.actual with
  | Some ty ->
      Buffer.add_string buf "\n  = found: ";
      Buffer.add_string buf (Types.to_string ty)
  | None -> ());

  (* Related locations *)
  List.iter
    (fun (rel : related_location) ->
      Buffer.add_string buf "\n  = note: ";
      Buffer.add_string buf rel.message;
      Buffer.add_string buf " at ";
      Buffer.add_string buf (format_span rel.span))
    d.related;

  Buffer.contents buf

(** Format a diagnostic in a compact single-line format.

    Useful for IDE integration and machine parsing. Format: file:line:col:
    severity: message [expected: T1, found: T2] *)
let to_string_compact (d : t) : string =
  let type_info =
    match (d.expected, d.actual) with
    | Some exp, Some act ->
        Printf.sprintf " [expected: %s, found: %s]" (Types.to_string exp)
          (Types.to_string act)
    | _ -> ""
  in
  Printf.sprintf "%s: %s: %s%s" (format_span d.span)
    (format_severity d.severity)
    d.message type_info

(** Format multiple diagnostics *)
let to_string_list (ds : t list) : string =
  String.concat "\n\n" (List.map to_string ds)

(** Get the primary span of a diagnostic *)
let span (d : t) : Loc.span = d.span

(** Get all spans (primary and related) from a diagnostic *)
let all_spans (d : t) : Loc.span list =
  d.span :: List.map (fun (r : related_location) -> r.span) d.related

(** Check if a diagnostic is an error (vs warning/hint) *)
let is_error (d : t) : bool =
  match d.severity with Error -> true | Warning | Hint -> false

(** Count errors in a list of diagnostics *)
let count_errors (ds : t list) : int = List.length (List.filter is_error ds)
