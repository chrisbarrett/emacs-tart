(** Type error diagnostics for user-facing error messages.

    This module provides structured diagnostic information for type errors,
    including source locations, expected vs actual types, and related locations
    (provenance) for understanding where types originated.

    R10: Type error diagnostics - errors include:
    - Source location of the error
    - Expected type
    - Actual type
    - Related locations (e.g., where expected type originated)

    Error codes follow Rust conventions:
    - E0308: Type mismatch
    - E0317: Incompatible branch types
    - E0061: Wrong number of arguments
    - E0106: Infinite type (occurs check) *)

module Types = Core.Types
module Loc = Syntax.Location

(** Error codes for categorizing diagnostics. *)
type error_code =
  | E0308  (** Type mismatch *)
  | E0317  (** Incompatible branch types *)
  | E0061  (** Wrong number of arguments (arity) *)
  | E0106  (** Infinite type (occurs check) *)

(** Format an error code for display. *)
let error_code_to_string = function
  | E0308 -> "E0308"
  | E0317 -> "E0317"
  | E0061 -> "E0061"
  | E0106 -> "E0106"

(** Severity level for diagnostics *)
type severity = Error | Warning | Hint

type related_location = { span : Loc.span; message : string }
(** A related location with context *)

type t = {
  severity : severity;
  code : error_code option;  (** Error code for categorization *)
  span : Loc.span;  (** Primary location of the error *)
  message : string;  (** Main error message *)
  expected : Types.typ option;  (** Expected type (if applicable) *)
  actual : Types.typ option;  (** Actual type found (if applicable) *)
  related : related_location list;  (** Related locations with context *)
  help : string list;  (** Suggested fixes *)
}
(** A structured diagnostic message *)

(** Check if a type is an Option type. *)
let is_option_type ty =
  match Types.repr ty with Types.TApp ("Option", _) -> true | _ -> false

(** Generate help suggestions for type mismatches.

    Analyzes expected and actual types to suggest common fixes. *)
let suggest_type_fix ~expected ~actual : string list =
  let expected = Types.repr expected in
  let actual = Types.repr actual in
  match (expected, actual) with
  (* Int -> String: suggest number-to-string *)
  | Types.TCon "String", Types.TCon "Int" ->
      [ "convert the integer to a string: (number-to-string ...)" ]
  (* String -> Int: suggest string-to-number *)
  | Types.TCon "Int", Types.TCon "String" ->
      [ "convert the string to a number: (string-to-number ...)" ]
  (* Option inner -> inner: suggest nil handling *)
  | _, Types.TApp ("Option", [ inner ])
    when Types.equal expected inner || Types.equal expected (Types.repr inner)
    ->
      [
        "check for nil first: (when-let ((x ...)) ...)";
        "or provide a default: (or ... default-value)";
      ]
  (* inner -> Option inner: not usually an error, but could suggest wrapping *)
  | Types.TApp ("Option", [ inner ]), _
    when Types.equal (Types.repr inner) actual ->
      [ "the value is already non-nil and can be used directly" ]
  (* Symbol -> String: suggest symbol-name *)
  | Types.TCon "String", Types.TCon "Symbol" ->
      [ "convert the symbol to a string: (symbol-name ...)" ]
  (* String -> Symbol: suggest intern *)
  | Types.TCon "Symbol", Types.TCon "String" ->
      [ "convert the string to a symbol: (intern ...)" ]
  (* List -> single element: suggest car or first *)
  | _, Types.TApp ("List", _) ->
      [ "to get a single element from a list, use (car ...) or (nth N ...)" ]
  | _ -> []

(** Create a type mismatch diagnostic with help suggestions. *)
let type_mismatch ~span ~expected ~actual ?(related = []) () =
  let help = suggest_type_fix ~expected ~actual in
  (* Detect Option/nil mismatch for better messaging *)
  let is_nil_error = is_option_type actual && not (is_option_type expected) in
  let message =
    if is_nil_error then "possible nil value"
    else
      Printf.sprintf "type mismatch: expected %s but found %s"
        (Types.to_string expected) (Types.to_string actual)
  in
  {
    severity = Error;
    code = Some E0308;
    span;
    message;
    expected = Some expected;
    actual = Some actual;
    related;
    help;
  }

(** Create an arity mismatch diagnostic *)
let arity_mismatch ~span ~expected ~actual ?(related = []) () =
  let message =
    Printf.sprintf "wrong number of arguments: expected %d but got %d" expected
      actual
  in
  {
    severity = Error;
    code = Some E0061;
    span;
    message;
    expected = None;
    actual = None;
    related;
    help = [];
  }

(** Create an occurs check diagnostic *)
let occurs_check ~span ~tvar_id ~typ ?(related = []) () =
  let message =
    Printf.sprintf "infinite type: type variable '_%d occurs in %s" tvar_id
      (Types.to_string typ)
  in
  {
    severity = Error;
    code = Some E0106;
    span;
    message;
    expected = None;
    actual = Some typ;
    related;
    help = [ "recursive types require explicit type annotations" ];
  }

(** Create a missing signature warning for a public function not in .tart file
*)
let missing_signature ~span ~name () =
  let message =
    Printf.sprintf "function `%s` defined but not in signature file" name
  in
  {
    severity = Warning;
    code = None;
    span;
    message;
    expected = None;
    actual = None;
    related = [];
    help = [];
  }

(** Format a function signature for display in error messages *)
let format_function_signature fn_name fn_type =
  Printf.sprintf "(%s %s)" fn_name (Types.to_string fn_type)

(** Generate a note about function argument expectation *)
let function_arg_note fn_name fn_type arg_index expected : related_location list
    =
  let param_name =
    match Types.repr fn_type with
    | Types.TArrow (params, _) -> (
        match List.nth_opt params arg_index with
        | Some (Types.PPositional _) ->
            Printf.sprintf "argument %d" (arg_index + 1)
        | Some (Types.POptional _) ->
            Printf.sprintf "optional argument %d" (arg_index + 1)
        | Some (Types.PRest _) -> "rest argument"
        | Some (Types.PKey (name, _)) -> Printf.sprintf "keyword :%s" name
        | None -> Printf.sprintf "argument %d" (arg_index + 1))
    | _ -> Printf.sprintf "argument %d" (arg_index + 1)
  in
  [
    {
      span = Loc.dummy_span;
      message =
        Printf.sprintf "`%s` expects %s to be %s\n      %s" fn_name param_name
          (Types.to_string expected)
          (format_function_signature fn_name fn_type);
    };
  ]

(** Generate a note about the other branch in an if expression *)
let if_branch_note ~is_then ~other_branch_span ~other_branch_type :
    related_location list =
  let this_branch = if is_then then "then" else "else" in
  let other_branch = if is_then then "else" else "then" in
  [
    {
      span = other_branch_span;
      message =
        Printf.sprintf "the %s branch has type %s" other_branch
          (Types.to_string other_branch_type);
    };
    {
      span = Loc.dummy_span;
      message =
        Printf.sprintf
          "both branches of `if` must have the same type; %s branch type does \
           not match"
          this_branch;
    };
  ]

(** Generate help suggestions for branch type mismatches *)
let suggest_branch_fix ~this_type ~other_type : string list =
  let this_type = Types.repr this_type in
  let other_type = Types.repr other_type in
  match (this_type, other_type) with
  (* Int vs String: suggest conversion *)
  | Types.TCon "Int", Types.TCon "String" ->
      [ "convert the integer to a string: (number-to-string ...)" ]
  | Types.TCon "String", Types.TCon "Int" ->
      [ "convert the string to an integer: (string-to-number ...)" ]
  (* Option vs non-Option: suggest handling nil *)
  | _, Types.TApp ("Option", [ inner ])
    when Types.equal this_type inner || Types.equal this_type (Types.repr inner)
    ->
      [ "wrap the non-optional value: (some ...)" ]
  | Types.TApp ("Option", [ inner ]), _
    when Types.equal other_type inner
         || Types.equal other_type (Types.repr inner) ->
      [ "unwrap the optional value or provide a default" ]
  | _ -> []

(** Create a branch type mismatch diagnostic (E0317) *)
let branch_mismatch ~span ~this_type ~other_branch_span ~other_type ~is_then ()
    =
  let related =
    if_branch_note ~is_then ~other_branch_span ~other_branch_type:other_type
  in
  let help = suggest_branch_fix ~this_type ~other_type in
  {
    severity = Error;
    code = Some E0317;
    span;
    message = "if branches have incompatible types";
    expected = Some other_type;
    actual = Some this_type;
    related;
    help;
  }

(** Create a type mismatch diagnostic with context *)
let type_mismatch_with_context ~span ~expected ~actual ~context () =
  match context with
  | Constraint.IfBranch { is_then; other_branch_span; other_branch_type } ->
      (* For if branches, create a branch mismatch diagnostic instead *)
      branch_mismatch ~span ~this_type:actual ~other_branch_span
        ~other_type:other_branch_type ~is_then ()
  | Constraint.FunctionArg { fn_name; fn_type; arg_index } ->
      let base_help = suggest_type_fix ~expected ~actual in
      let is_nil_error =
        is_option_type actual && not (is_option_type expected)
      in
      let message =
        if is_nil_error then "possible nil value" else "type mismatch"
      in
      let related = function_arg_note fn_name fn_type arg_index expected in
      {
        severity = Error;
        code = Some E0308;
        span;
        message;
        expected = Some expected;
        actual = Some actual;
        related;
        help = base_help;
      }
  | Constraint.NoContext ->
      let base_help = suggest_type_fix ~expected ~actual in
      let is_nil_error =
        is_option_type actual && not (is_option_type expected)
      in
      let message =
        if is_nil_error then "possible nil value" else "type mismatch"
      in
      {
        severity = Error;
        code = Some E0308;
        span;
        message;
        expected = Some expected;
        actual = Some actual;
        related = [];
        help = base_help;
      }

(** Convert a unification error to a diagnostic *)
let of_unify_error (err : Unify.error) : t =
  match err with
  | Unify.TypeMismatch (expected, actual, span, context) ->
      type_mismatch_with_context ~span ~expected ~actual ~context ()
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
let to_string (d : t) : string =
  let buf = Buffer.create 256 in

  (* Main error line with code *)
  Buffer.add_string buf (format_severity d.severity);
  (match d.code with
  | Some code ->
      Buffer.add_string buf "[";
      Buffer.add_string buf (error_code_to_string code);
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
    (fun (rel : related_location) ->
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

(** Format a diagnostic in a compact single-line format.

    Useful for IDE integration and machine parsing. Format: file:line:col:
    severity[CODE]: message [expected: T1, found: T2] *)
let to_string_compact (d : t) : string =
  let code_str =
    match d.code with
    | Some c -> "[" ^ error_code_to_string c ^ "]"
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
let to_string_list (ds : t list) : string =
  String.concat "\n\n" (List.map to_string ds)

(** Get the primary span of a diagnostic *)
let span (d : t) : Loc.span = d.span

(** Get the error code of a diagnostic *)
let code (d : t) : error_code option = d.code

(** Get the help suggestions for a diagnostic *)
let help (d : t) : string list = d.help

(** Get all spans (primary and related) from a diagnostic *)
let all_spans (d : t) : Loc.span list =
  d.span :: List.map (fun (r : related_location) -> r.span) d.related

(** Check if a diagnostic is an error (vs warning/hint) *)
let is_error (d : t) : bool =
  match d.severity with Error -> true | Warning | Hint -> false

(** Count errors in a list of diagnostics *)
let count_errors (ds : t list) : int = List.length (List.filter is_error ds)
