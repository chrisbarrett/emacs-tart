(** Type error diagnostics for user-facing error messages.

    This module provides structured diagnostic information for type errors,
    including source locations, expected vs actual types, and related locations
    (provenance) for understanding where types originated.

    R10: Type error diagnostics - errors include:
    - Source location of the error
    - Expected type
    - Actual type
    - Related locations (e.g., where expected type originated)

    Error codes follow Spec 47 (Error Code Registry):
    - Type Errors (E0001–E0099)
    - Name Errors (E0100–E0199)
    - Arity Errors (E0200–E0299)
    - Kind Errors (E0300–E0399)
    - Pattern Errors (E0400–E0499)
    - Row/Record Errors (E0500–E0599)
    - Union Errors (E0600–E0699)
    - Module Errors (E0700–E0799)
    - File Errors (E0800–E0899) *)

module Types = Core.Types
module Loc = Syntax.Location

(** Error codes for categorizing diagnostics.

    Codes are assigned per Spec 47 (Error Code Registry). Once assigned, codes
    are never reassigned. *)
type error_code =
  (* Type Errors (E0001–E0099) *)
  | E0001  (** TypeMismatch: Expected one type, found another *)
  | E0002  (** BranchMismatch: If/cond branches have incompatible types *)
  | E0003  (** InfiniteType: Occurs check failed (recursive type) *)
  | E0004
      (** SignatureMismatch: Implementation doesn't match declared signature *)
  | E0005  (** AnnotationMismatch: Expression doesn't match tart annotation *)
  | E0006  (** ReturnMismatch: Function body doesn't match declared return *)
  | E0007  (** UnificationFailed: Types cannot be unified *)
  (* Name Errors (E0100–E0199) *)
  | E0100  (** UndefinedVariable: Variable not in scope *)
  | E0101  (** UndefinedFunction: Function not in scope *)
  | E0102  (** UndefinedType: Type not in scope *)
  | E0104  (** MissingSignature: Function defined but not in .tart file *)
  (* Arity Errors (E0200–E0299) *)
  | E0200  (** WrongArity: Wrong number of arguments to function *)
  | E0201  (** WrongTypeArity: Wrong number of type arguments *)
  (* Kind Errors (E0300–E0399) *)
  | E0300  (** KindMismatch: Expected one kind, found another *)
  | E0301  (** InfiniteKind: Occurs check failed at kind level *)
  | E0302  (** TypeArityMismatch: Type constructor applied to wrong # of args *)
  (* Pattern Errors (E0400–E0499) *)
  | E0400  (** NonExhaustive: Pattern match doesn't cover all cases *)
  (* Module Errors (E0700–E0799) *)
  | E0702  (** SignatureNotFound: No .tart signature file found *)

(** Format an error code for display. *)
let error_code_to_string = function
  (* Type Errors *)
  | E0001 -> "E0001"
  | E0002 -> "E0002"
  | E0003 -> "E0003"
  | E0004 -> "E0004"
  | E0005 -> "E0005"
  | E0006 -> "E0006"
  | E0007 -> "E0007"
  (* Name Errors *)
  | E0100 -> "E0100"
  | E0101 -> "E0101"
  | E0102 -> "E0102"
  | E0104 -> "E0104"
  (* Arity Errors *)
  | E0200 -> "E0200"
  | E0201 -> "E0201"
  (* Kind Errors *)
  | E0300 -> "E0300"
  | E0301 -> "E0301"
  | E0302 -> "E0302"
  (* Pattern Errors *)
  | E0400 -> "E0400"
  (* Module Errors *)
  | E0702 -> "E0702"

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

(** Check if a type is an Option type (a | nil). *)
let is_option_type ty = Option.is_some (Types.is_option ty)

(** Generate help suggestions for type mismatches.

    Analyzes expected and actual types to suggest common fixes. *)
let suggest_type_fix ~expected ~actual : string list =
  let expected = Types.repr expected in
  let actual = Types.repr actual in
  (* Check primitive type mismatches using Prim constants (for intrinsic names) *)
  if Types.equal expected Types.Prim.string && Types.equal actual Types.Prim.int
  then
    (* Int -> String: suggest number-to-string *)
    [ "convert the integer to a string: (number-to-string ...)" ]
  else if
    Types.equal expected Types.Prim.int && Types.equal actual Types.Prim.string
  then
    (* String -> Int: suggest string-to-number *)
    [ "convert the string to a number: (string-to-number ...)" ]
  else if
    Types.equal expected Types.Prim.string
    && Types.equal actual Types.Prim.symbol
  then
    (* Symbol -> String: suggest symbol-name *)
    [ "convert the symbol to a string: (symbol-name ...)" ]
  else if
    Types.equal expected Types.Prim.symbol
    && Types.equal actual Types.Prim.string
  then
    (* String -> Symbol: suggest intern *)
    [ "convert the string to a symbol: (intern ...)" ]
  else
    match (expected, actual) with
    (* Option inner -> inner: suggest nil handling *)
    | _, _ when Option.is_some (Types.is_option actual) -> (
        match Types.is_option actual with
        | Some inner
          when Types.equal expected inner
               || Types.equal expected (Types.repr inner) ->
            [
              "check for nil first: (when-let ((x ...)) ...)";
              "or provide a default: (or ... default-value)";
            ]
        | _ -> [])
    (* inner -> Option inner: not usually an error, but could suggest wrapping *)
    | _, _ when Option.is_some (Types.is_option expected) -> (
        match Types.is_option expected with
        | Some inner when Types.equal (Types.repr inner) actual ->
            [ "the value is already non-nil and can be used directly" ]
        | _ -> [])
    (* List -> single element: suggest car or first *)
    | _, Types.TApp (con, _)
      when Types.equal con (Types.TCon (Types.intrinsic "List")) ->
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
    code = Some E0001;
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
    code = Some E0200;
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
    code = Some E0003;
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
    code = Some E0104;
    span;
    message;
    expected = None;
    actual = None;
    related = [];
    help = [];
  }

(** Create a signature mismatch diagnostic (E0004) showing both locations.

    Used when a function's implementation type doesn't match its declared
    signature. Shows the implementation location as primary, and the signature
    location as related information. *)
let signature_mismatch ~name ~impl_span ~impl_type ~sig_span ~sig_type () =
  let related =
    [
      {
        span = sig_span;
        message =
          Printf.sprintf "signature declared here: %s"
            (Types.to_string sig_type);
      };
    ]
  in
  {
    severity = Error;
    code = Some E0004;
    span = impl_span;
    message =
      Printf.sprintf "implementation of `%s` does not match signature" name;
    expected = Some sig_type;
    actual = Some impl_type;
    related;
    help = [];
  }

(** Create an undefined variable diagnostic (E0100) with typo suggestions.

    Takes the undefined name and a list of candidate names from the environment
    to generate "did you mean?" suggestions using Levenshtein distance. *)
let undefined_variable ~span ~name ~candidates () =
  let message = Printf.sprintf "variable `%s` is not defined" name in
  let suggestion = Levenshtein.suggest_name ~query:name ~candidates in
  let help =
    match suggestion with
    | Some similar ->
        [ Printf.sprintf "a variable with a similar name exists: `%s`" similar ]
    | None -> []
  in
  {
    severity = Error;
    code = Some E0100;
    span;
    message;
    expected = None;
    actual = None;
    related = [];
    help;
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

(** Format expected arity range for display.

    Takes the minimum required and optional count and formats as:
    - "1 argument" for fixed arity with 1
    - "2 arguments" for fixed arity
    - "2-3 arguments" for range with optionals
    - "2+ arguments" for functions with rest params *)
let format_arity_range min_required optionals has_rest =
  let arg_word n = if n = 1 then "argument" else "arguments" in
  if has_rest then Printf.sprintf "%d+ %s" min_required (arg_word 2)
  else if optionals = 0 then
    Printf.sprintf "%d %s" min_required (arg_word min_required)
  else
    let max = min_required + optionals in
    Printf.sprintf "%d-%d %s" min_required max (arg_word max)

(** Count required vs optional parameters in a function type *)
let count_params params =
  let rec aux min_required optionals has_rest = function
    | [] -> (min_required, optionals, has_rest)
    | Types.PPositional _ :: rest ->
        aux (min_required + 1) optionals has_rest rest
    | Types.POptional _ :: rest ->
        aux min_required (optionals + 1) has_rest rest
    | Types.PRest _ :: rest -> aux min_required optionals true rest
    | Types.PKey _ :: rest -> aux min_required optionals has_rest rest
  in
  aux 0 0 false params

(** Generate a note about function signature for arity errors *)
let arity_function_note fn_name fn_type expected : related_location list =
  let signature = format_function_signature fn_name fn_type in
  let min_required, optionals, has_rest =
    match Types.repr fn_type with
    | Types.TArrow (params, _) -> count_params params
    | _ -> (expected, 0, false)
  in
  let arity_str = format_arity_range min_required optionals has_rest in
  [
    {
      span = Loc.dummy_span;
      message =
        Printf.sprintf "`%s` expects %s\n      %s" fn_name arity_str signature;
    };
  ]

(** Create an arity mismatch diagnostic with function context *)
let arity_mismatch_with_context ~span ~expected ~actual ~context () =
  match context with
  | Constraint.FunctionArg { fn_name; fn_type; _ } ->
      let fn_related = arity_function_note fn_name fn_type expected in
      let min_required, optionals, has_rest =
        match Types.repr fn_type with
        | Types.TArrow (params, _) -> count_params params
        | _ -> (expected, 0, false)
      in
      let expected_str = format_arity_range min_required optionals has_rest in
      let actual_str =
        if actual = 1 then "1 argument"
        else Printf.sprintf "%d arguments" actual
      in
      let message =
        Printf.sprintf "wrong number of arguments: expected %s but got %s"
          expected_str actual_str
      in
      {
        severity = Error;
        code = Some E0200;
        span;
        message;
        expected = None;
        actual = None;
        related = fn_related;
        help = [];
      }
  | Constraint.TypeArgArity { fn_name; expected = exp; actual = act } ->
      let arg_word n = if n = 1 then "argument" else "arguments" in
      let message =
        Printf.sprintf "wrong number of type arguments: expected %d but got %d"
          exp act
      in
      let related =
        [
          {
            span = Loc.dummy_span;
            message =
              Printf.sprintf "`%s` has %d type %s" fn_name exp (arg_word exp);
          };
        ]
      in
      {
        severity = Error;
        code = Some E0201;
        span;
        message;
        expected = None;
        actual = None;
        related;
        help = [];
      }
  | Constraint.NoContext | Constraint.IfBranch _ | Constraint.TartAnnotation _
  | Constraint.DeclaredReturn _ | Constraint.ExplicitInstantiation _ ->
      arity_mismatch ~span ~expected ~actual ()

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
  (* Check primitive type mismatches using Prim constants *)
  if
    Types.equal this_type Types.Prim.int
    && Types.equal other_type Types.Prim.string
  then
    (* Int vs String: suggest conversion *)
    [ "convert the integer to a string: (number-to-string ...)" ]
  else if
    Types.equal this_type Types.Prim.string
    && Types.equal other_type Types.Prim.int
  then [ "convert the string to an integer: (string-to-number ...)" ]
  else
    match (this_type, other_type) with
    (* Option vs non-Option: suggest handling nil *)
    | _, _ when Option.is_some (Types.is_option other_type) -> (
        match Types.is_option other_type with
        | Some inner
          when Types.equal this_type inner
               || Types.equal this_type (Types.repr inner) ->
            [ "wrap the non-optional value: (some ...)" ]
        | _ -> [])
    | _, _ when Option.is_some (Types.is_option this_type) -> (
        match Types.is_option this_type with
        | Some inner
          when Types.equal other_type inner
               || Types.equal other_type (Types.repr inner) ->
            [ "unwrap the optional value or provide a default" ]
        | _ -> [])
    | _ -> []

(** Create a branch type mismatch diagnostic (E0002) *)
let branch_mismatch ~span ~this_type ~other_branch_span ~other_type ~is_then ()
    =
  let related =
    if_branch_note ~is_then ~other_branch_span ~other_branch_type:other_type
  in
  let help = suggest_branch_fix ~this_type ~other_type in
  {
    severity = Error;
    code = Some E0002;
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
  | Constraint.FunctionArg { fn_name; fn_type; arg_index; arg_expr_source } ->
      let base_help = suggest_type_fix ~expected ~actual in
      let is_nil_error =
        is_option_type actual && not (is_option_type expected)
      in
      let message =
        if is_nil_error then "possible nil value" else "type mismatch"
      in
      let fn_related = function_arg_note fn_name fn_type arg_index expected in
      (* Add "may return nil" note for Option types when we know the source *)
      let nil_note =
        if is_nil_error then
          match arg_expr_source with
          | Some source ->
              [
                {
                  span = Loc.dummy_span;
                  message = Printf.sprintf "`%s` may return nil" source;
                };
              ]
          | None -> []
        else []
      in
      let related = fn_related @ nil_note in
      {
        severity = Error;
        code = Some E0001;
        span;
        message;
        expected = Some expected;
        actual = Some actual;
        related;
        help = base_help;
      }
  | Constraint.TartAnnotation { declared_type } ->
      let base_help = suggest_type_fix ~expected ~actual in
      let message =
        Printf.sprintf "type annotation mismatch: expression has type %s"
          (Types.to_string actual)
      in
      let related =
        [
          {
            span = Loc.dummy_span;
            message =
              Printf.sprintf "annotation declares type %s"
                (Types.to_string declared_type);
          };
        ]
      in
      {
        severity = Error;
        code = Some E0005;
        span;
        message;
        expected = Some expected;
        actual = Some actual;
        related;
        help = base_help;
      }
  | Constraint.DeclaredReturn { fn_name; declared_type } ->
      let base_help = suggest_type_fix ~expected ~actual in
      let message =
        Printf.sprintf "function body doesn't match declared return type"
      in
      let related =
        [
          {
            span = Loc.dummy_span;
            message =
              Printf.sprintf "`%s` declared to return %s" fn_name
                (Types.to_string declared_type);
          };
        ]
      in
      {
        severity = Error;
        code = Some E0006;
        span;
        message;
        expected = Some expected;
        actual = Some actual;
        related;
        help = base_help;
      }
  | Constraint.ExplicitInstantiation { type_args; arg_index = _ } ->
      let base_help = suggest_type_fix ~expected ~actual in
      let type_args_str =
        String.concat ", " (List.map Types.to_string type_args)
      in
      (* Per R6: message should reference tart annotation and show expected from it *)
      let related =
        [
          {
            span = Loc.dummy_span;
            message =
              Printf.sprintf "expected %s (from tart instantiation [%s])"
                (Types.to_string expected) type_args_str;
          };
        ]
      in
      {
        severity = Error;
        code = Some E0001;
        span;
        message =
          "type mismatch: tart instantiation specifies incompatible type";
        expected = Some expected;
        actual = Some actual;
        related;
        help = base_help;
      }
  | Constraint.TypeArgArity { fn_name; expected = exp; actual = act } ->
      (* TypeArgArity context on a type mismatch means we generated a fake
         constraint to report the arity error. Format it properly. *)
      let arg_word n = if n = 1 then "argument" else "arguments" in
      let message =
        Printf.sprintf "wrong number of type arguments: expected %d but got %d"
          exp act
      in
      let related =
        [
          {
            span = Loc.dummy_span;
            message =
              Printf.sprintf "`%s` has %d type %s" fn_name exp (arg_word exp);
          };
        ]
      in
      {
        severity = Error;
        code = Some E0201;
        span;
        message;
        expected = None;
        actual = None;
        related;
        help = [];
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
        code = Some E0001;
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
  | Unify.ArityMismatch (expected, actual, span, context) ->
      arity_mismatch_with_context ~span ~expected ~actual ~context ()

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

(** Convert error code to error type string for header. *)
let error_type_of_code = function
  | Some E0001 -> "TYPE MISMATCH"
  | Some E0002 -> "BRANCH MISMATCH"
  | Some E0003 -> "INFINITE TYPE"
  | Some E0004 -> "SIGNATURE MISMATCH"
  | Some E0005 -> "ANNOTATION MISMATCH"
  | Some E0006 -> "RETURN MISMATCH"
  | Some E0007 -> "UNIFICATION FAILED"
  | Some E0100 -> "UNDEFINED VARIABLE"
  | Some E0101 -> "UNDEFINED FUNCTION"
  | Some E0102 -> "UNDEFINED TYPE"
  | Some E0104 -> "MISSING SIGNATURE"
  | Some E0200 -> "WRONG ARITY"
  | Some E0201 -> "WRONG TYPE ARITY"
  | Some E0300 -> "KIND MISMATCH"
  | Some E0301 -> "INFINITE KIND"
  | Some E0302 -> "TYPE ARITY MISMATCH"
  | Some E0400 -> "NON-EXHAUSTIVE MATCH"
  | Some E0702 -> "SIGNATURE NOT FOUND"
  | None -> "ERROR"

(** Format a diagnostic in Elm-style human-readable format with source excerpts.

    Per Spec 45 R4-R9: Shows Elm-style headers, source excerpts with underlines,
    conversational prose, and colored output when TTY is detected.

    Output format:
    {[
      -- TYPE MISMATCH ---------------------------------------- file.el:42:10

      I found a type mismatch in this expression:

      42 |   (upcase count)
         |           ^^^^^

      The function `upcase` expects argument 1 to be:

          String

      But this expression has type:

          Int

      Hint: convert the integer to a string: (number-to-string ...)
    ]} *)
let to_string_human (d : t) : string =
  let buf = Buffer.create 512 in

  (* R4: Elm-style header with dashes *)
  let error_type = error_type_of_code d.code in
  Buffer.add_string buf (Source_excerpt.format_header ~error_type d.span);
  Buffer.add_string buf "\n\n";

  (* R5: Conversational intro *)
  let prose_ctx =
    (* Map from our diagnostic type to Source_excerpt prose context *)
    match d.code with
    | Some E0002 ->
        (* Branch mismatch - check if we have related info *)
        Source_excerpt.IfBranch { is_then = true }
        (* Default *)
    | Some E0006 ->
        (* Return mismatch - try to extract fn_name from message *)
        Source_excerpt.DeclaredReturn { fn_name = "function" }
    | Some E0005 -> Source_excerpt.TartAnnotation
    | _ -> Source_excerpt.NoContext
  in
  Buffer.add_string buf (Source_excerpt.intro_prose prose_ctx);
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
    (fun (rel : related_location) ->
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

(** Create a non-exhaustive pattern match warning (E0400).

    Used when a pcase expression doesn't cover all constructors of an ADT. *)
let non_exhaustive_match ~span ~message () =
  {
    severity = Warning;
    code = Some E0400;
    span;
    message;
    expected = None;
    actual = None;
    related = [];
    help = [ "add a wildcard pattern (_) to handle remaining cases" ];
  }

(** Create a kind mismatch diagnostic (E0300).

    Used when a type application has mismatched kinds.

    Output format follows R6 from spec 17:
    {v
      Error: kind mismatch
        expected: * -> *
        found: *
        in type application: (f a)
    v} *)
let kind_mismatch ~span ~expected ~found ~location () =
  let related =
    [ { span = Loc.dummy_span; message = Printf.sprintf "in %s" location } ]
  in
  let help =
    match (expected, found) with
    | Kind.KArrow _, Kind.KStar ->
        [ "this type variable is used as a type constructor but has kind *" ]
    | Kind.KStar, Kind.KArrow _ ->
        [ "this type constructor is applied to too few arguments" ]
    | _ -> []
  in
  {
    severity = Error;
    code = Some E0300;
    span;
    message = "kind mismatch";
    expected = None;
    actual = None;
    related =
      [
        {
          span = Loc.dummy_span;
          message = Printf.sprintf "expected: %s" (Kind.to_string expected);
        };
        {
          span = Loc.dummy_span;
          message = Printf.sprintf "found: %s" (Kind.to_string found);
        };
      ]
      @ related;
    help;
  }

(** Convert a kind inference error to a diagnostic. *)
let of_kind_error span (err : Kind_infer.kind_error) : t =
  match err with
  | Kind_infer.KindMismatch { expected; found; location } ->
      kind_mismatch ~span ~expected ~found ~location ()
  | Kind_infer.OccursCheckFailed { kvar_id; kind } ->
      {
        severity = Error;
        code = Some E0301;
        span;
        message = "infinite kind";
        expected = None;
        actual = None;
        related =
          [
            {
              span = Loc.dummy_span;
              message =
                Printf.sprintf "kind variable ?k%d occurs in %s" kvar_id
                  (Kind.to_string kind);
            };
          ];
        help = [ "recursive kind constraints are not supported" ];
      }
  | Kind_infer.ArityMismatch { type_con; expected; found } ->
      let arg_word n = if n = 1 then "argument" else "arguments" in
      {
        severity = Error;
        code = Some E0302;
        span;
        message =
          Printf.sprintf
            "type constructor `%s` applied to wrong number of arguments"
            type_con;
        expected = None;
        actual = None;
        related =
          [
            {
              span = Loc.dummy_span;
              message =
                Printf.sprintf "expected %d %s, found %d" expected
                  (arg_word expected) found;
            };
          ];
        help = [];
      }

(** Create a missing type class instance diagnostic.

    Used when a function with type class constraints is called but no instance
    exists for the required constraint.

    Note: Type classes are not yet fully implemented. Error code will be
    assigned when type class support is added to the type system.

    Output format follows R8 from spec 21:
    {v
      Error: No instance of `Eq buffer` found
        Required by: (elem current buffers)
        At: file.el:42:3
    v} *)
let missing_instance ~span ~class_name ~typ () =
  {
    severity = Error;
    code = None;
    span;
    message =
      Printf.sprintf "no instance of `%s %s` found" class_name
        (Types.to_string typ);
    expected = None;
    actual = None;
    related = [];
    help =
      [
        Printf.sprintf "add an instance declaration: (instance (%s %s) ...)"
          class_name (Types.to_string typ);
      ];
  }

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
let related_location_to_json (rel : related_location) : Yojson.Safe.t =
  `Assoc
    [
      ("location", location_to_json rel.span); ("message", `String rel.message);
    ]

(** Serialize a diagnostic to JSON. *)
let to_json (d : t) : Yojson.Safe.t =
  let base =
    [
      ("kind", `String "type");
      ("severity", `String (format_severity d.severity));
      ("message", `String d.message);
      ("location", location_to_json d.span);
    ]
  in
  let with_code =
    match d.code with
    | Some code -> ("code", `String (error_code_to_string code)) :: base
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
