(** Output formatters and serializers for diagnostics.

    Provides human-readable, compact, and JSON output formats for
    {!Diagnostic.t} values. Separated from {!Diagnostic} to keep construction
    logic distinct from presentation. *)

module Types = Core.Types
module Loc = Syntax.Location

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

(** Map severity to LSP DiagnosticSeverity integer value *)
let severity_to_int = function
  | Diagnostic.Error -> 1
  | Diagnostic.Warning -> 2
  | Diagnostic.Hint -> 4

(** Format severity for display *)
let format_severity = function
  | Diagnostic.Error -> "error"
  | Diagnostic.Warning -> "warning"
  | Diagnostic.Hint -> "hint"

(** Convert error code to error type string for header. *)
let error_type_of_code = function
  | Some Diagnostic.TypeMismatch -> "TYPE MISMATCH"
  | Some BranchMismatch -> "BRANCH MISMATCH"
  | Some InfiniteType -> "INFINITE TYPE"
  | Some SignatureMismatch -> "SIGNATURE MISMATCH"
  | Some AnnotationMismatch -> "ANNOTATION MISMATCH"
  | Some ReturnMismatch -> "RETURN MISMATCH"
  | Some UnificationFailed -> "UNIFICATION FAILED"
  | Some DisjointEquality -> "DISJOINT EQUALITY"
  | Some UndefinedVariable -> "UNDEFINED VARIABLE"
  | Some UndefinedFunction -> "UNDEFINED FUNCTION"
  | Some UndefinedType -> "UNDEFINED TYPE"
  | Some MissingSignature -> "MISSING SIGNATURE"
  | Some WrongArity -> "WRONG ARITY"
  | Some WrongTypeArity -> "WRONG TYPE ARITY"
  | Some KindMismatch -> "KIND MISMATCH"
  | Some InfiniteKind -> "INFINITE KIND"
  | Some TypeArityMismatch -> "TYPE ARITY MISMATCH"
  | Some NonExhaustive -> "NON-EXHAUSTIVE MATCH"
  | Some SignatureNotFound -> "SIGNATURE NOT FOUND"
  | Some VersionTooLow -> "VERSION TOO LOW"
  | Some VersionTooHigh -> "VERSION TOO HIGH"
  | None -> "ERROR"

(** Format a diagnostic as a human-readable string.

    Output format (similar to rustc/clang):
    {[
      error[E0308]: type mismatch: expected Int but found String
        --> file.el:10:5
        |
        = expected: Int
        = found: String
        |
      note: expected type from function signature
        --> file.el:5:1
        |
      help: convert the integer to a string: (number-to-string ...)
    ]} *)
let to_string (d : Diagnostic.t) : string =
  let buf = Buffer.create 256 in

  (* Main error line with code *)
  Buffer.add_string buf (format_severity d.severity);
  (match d.code with
  | Some code ->
      Buffer.add_string buf "[";
      Buffer.add_string buf (Diagnostic.error_code_to_string code);
      Buffer.add_string buf "]"
  | None -> ());
  Buffer.add_string buf ": ";
  Buffer.add_string buf d.message;

  (* Location *)
  Buffer.add_string buf "\n  --> ";
  Buffer.add_string buf (format_span d.span);

  (* Type information *)
  (match d.expected with
  | Some ty ->
      Buffer.add_string buf "\n   |\n   = expected: ";
      Buffer.add_string buf (Types.to_string ty)
  | None -> ());
  (match d.actual with
  | Some ty ->
      Buffer.add_string buf "\n   = found: ";
      Buffer.add_string buf (Types.to_string ty)
  | None -> ());

  (* Related locations *)
  List.iter
    (fun (rel : Diagnostic.related_location) ->
      Buffer.add_string buf "\n   |\nnote: ";
      Buffer.add_string buf rel.message;
      Buffer.add_string buf "\n  --> ";
      Buffer.add_string buf (format_span rel.span))
    d.related;

  (* Help suggestions *)
  List.iter
    (fun help ->
      Buffer.add_string buf "\n   |\nhelp: ";
      Buffer.add_string buf help)
    d.help;

  Buffer.contents buf

(** Format a diagnostic in Elm-style human-readable format with source excerpts.

    Per Spec 45 R4-R9: Shows Elm-style headers, source excerpts with underlines,
    conversational prose, and colored output when TTY is detected. *)
let to_string_human (d : Diagnostic.t) : string =
  let buf = Buffer.create 512 in

  (* R4: Elm-style header with dashes *)
  let error_type =
    match d.code with
    | None ->
        (* Clause diagnostics and other code-less diagnostics use severity *)
        String.uppercase_ascii (format_severity d.severity)
    | _ -> error_type_of_code d.code
  in
  Buffer.add_string buf (Source_excerpt.format_header ~error_type d.span);
  Buffer.add_string buf "\n\n";

  (* R5: Conversational intro *)
  let prose_ctx =
    (* Map from our diagnostic type to Source_excerpt prose context *)
    match d.code with
    | Some Diagnostic.BranchMismatch ->
        (* Branch mismatch - check if we have related info *)
        Source_excerpt.IfBranch { is_then = true }
    | Some ReturnMismatch ->
        (* Extract fn_name from message format: "`name` body doesn't match..." *)
        let fn_name =
          try
            let i = String.index d.message '`' + 1 in
            let j = String.index_from d.message i '`' in
            String.sub d.message i (j - i)
          with _ -> "function"
        in
        Source_excerpt.DeclaredReturn { fn_name }
    | Some AnnotationMismatch -> Source_excerpt.TartAnnotation
    | Some TypeMismatch
    | Some InfiniteType
    | Some SignatureMismatch
    | Some UnificationFailed
    | Some DisjointEquality
    | Some UndefinedVariable
    | Some UndefinedFunction
    | Some UndefinedType
    | Some MissingSignature
    | Some WrongArity
    | Some WrongTypeArity
    | Some KindMismatch
    | Some InfiniteKind
    | Some TypeArityMismatch
    | Some NonExhaustive
    | Some SignatureNotFound
    | Some VersionTooLow
    | Some VersionTooHigh
    | None ->
        Source_excerpt.NoContext
  in
  (* Clause diagnostics (no error code) use the message directly as the
     intro prose; other diagnostics use the structured prose context. *)
  (match d.code with
  | None when d.message <> "" -> Buffer.add_string buf d.message
  | _ -> Buffer.add_string buf (Source_excerpt.intro_prose prose_ctx));
  Buffer.add_string buf "\n\n";

  (* R1-R3: Source excerpt for primary location *)
  (match Source_excerpt.render_span d.span with
  | Some excerpt ->
      Buffer.add_string buf excerpt;
      Buffer.add_string buf "\n\n"
  | None ->
      (* R8: Fallback when source unavailable *)
      Buffer.add_string buf "(source not available)\n\n");

  (* R5: Expected type prose *)
  (match d.expected with
  | Some ty ->
      let expected_str = Ansi.type_name (Types.to_string ty) in
      Buffer.add_string buf
        (Source_excerpt.expected_prose ~expected:expected_str prose_ctx);
      Buffer.add_string buf "\n\n"
  | None -> ());

  (* R5: Actual type prose *)
  (match d.actual with
  | Some ty ->
      let actual_str = Ansi.type_name (Types.to_string ty) in
      Buffer.add_string buf
        (Source_excerpt.actual_prose ~actual:actual_str prose_ctx);
      Buffer.add_string buf "\n"
  | None -> ());

  (* R7: Related location excerpts *)
  List.iter
    (fun (rel : Diagnostic.related_location) ->
      if rel.span.start_pos.file <> "<generated>" then (
        Buffer.add_string buf "\n";
        (match Source_excerpt.render_span rel.span with
        | Some excerpt ->
            Buffer.add_string buf excerpt;
            Buffer.add_string buf "\n"
        | None -> ());
        Buffer.add_string buf (Ansi.hint rel.message);
        Buffer.add_string buf "\n")
      else (
        Buffer.add_string buf "\n";
        Buffer.add_string buf (Ansi.hint rel.message);
        Buffer.add_string buf "\n"))
    d.related;

  (* R9: Help suggestions *)
  List.iter
    (fun help_text ->
      Buffer.add_string buf "\n";
      Buffer.add_string buf (Ansi.help ("Hint: " ^ help_text)))
    d.help;

  Buffer.contents buf

(** Format a diagnostic in a compact single-line format.

    Useful for IDE integration and machine parsing. Format: file:line:col:
    severity[CODE]: message [expected: T1, found: T2] *)
let to_string_compact (d : Diagnostic.t) : string =
  let code_str =
    match d.code with
    | Some c -> "[" ^ Diagnostic.error_code_to_string c ^ "]"
    | None -> ""
  in
  let type_info =
    match (d.expected, d.actual) with
    | Some exp, Some act ->
        Printf.sprintf " [expected: %s, found: %s]" (Types.to_string exp)
          (Types.to_string act)
    | _ -> ""
  in
  Printf.sprintf "%s: %s%s: %s%s" (format_span d.span)
    (format_severity d.severity)
    code_str d.message type_info

(** Format multiple diagnostics *)
let to_string_list (ds : Diagnostic.t list) : string =
  String.concat "\n\n" (List.map to_string ds)

(** Serialize a source location to JSON. *)
let location_to_json (span : Loc.span) : Yojson.Safe.t =
  `Assoc
    [
      ("file", `String span.start_pos.file);
      ("line", `Int span.start_pos.line);
      ("column", `Int (span.start_pos.col + 1));
      ("end_line", `Int span.end_pos.line);
      ("end_column", `Int (span.end_pos.col + 1));
    ]

(** Serialize a related location to JSON. *)
let related_location_to_json (rel : Diagnostic.related_location) : Yojson.Safe.t
    =
  `Assoc
    [
      ("location", location_to_json rel.span); ("message", `String rel.message);
    ]

(** Serialize a diagnostic to JSON. *)
let to_json (d : Diagnostic.t) : Yojson.Safe.t =
  let base =
    [
      ("kind", `String "type");
      ("severity", `String (format_severity d.severity));
      ("severity_code", `Int (severity_to_int d.severity));
      ("message", `String d.message);
      ("location", location_to_json d.span);
    ]
  in
  let with_code =
    match d.code with
    | Some code ->
        ("code", `String (Diagnostic.error_code_to_string code)) :: base
    | None -> base
  in
  let with_expected =
    match d.expected with
    | Some ty -> ("expected", `String (Types.to_string ty)) :: with_code
    | None -> with_code
  in
  let with_actual =
    match d.actual with
    | Some ty -> ("actual", `String (Types.to_string ty)) :: with_expected
    | None -> with_expected
  in
  let with_related =
    if d.related = [] then with_actual
    else
      ("related", `List (List.map related_location_to_json d.related))
      :: with_actual
  in
  let with_help =
    if d.help = [] then with_related
    else ("help", `List (List.map (fun h -> `String h) d.help)) :: with_related
  in
  `Assoc with_help
