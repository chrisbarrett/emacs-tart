(** S-expression AST for Emacs Lisp *)

open Location

(** S-expression node types.

    Each node carries its source span for error reporting and IDE features.
    Reader macros are desugared to their canonical forms:
    - 'x -> (quote x)
    - `x -> (backquote x)
    - ,x -> (unquote x)
    - ,@x -> (unquote-splicing x)
    - #'f -> (function f) *)
type t =
  | Int of int * span
  | Float of float * span
  | String of string * span
  | Symbol of string * span
  | Keyword of string * span  (** Keywords like :foo *)
  | Char of int * span  (** Character literals ?a, ?\n, etc. *)
  | List of t list * span
  | Vector of t list * span  (** #(...) vectors *)
  | Curly of t list * span  (** Curly braces {...} for row types *)
  | Cons of t * t * span  (** Dotted pairs (a . b) *)
  | Error of string * span  (** Error node for recovery *)
[@@deriving show, eq]

(** Get the source span of any S-expression *)
let span_of = function
  | Int (_, s)
  | Float (_, s)
  | String (_, s)
  | Symbol (_, s)
  | Keyword (_, s)
  | Char (_, s)
  | List (_, s)
  | Vector (_, s)
  | Curly (_, s)
  | Cons (_, _, s)
  | Error (_, s) ->
      s

(** Set the span of an S-expression *)
let with_span sexp span =
  match sexp with
  | Int (v, _) -> Int (v, span)
  | Float (v, _) -> Float (v, span)
  | String (v, _) -> String (v, span)
  | Symbol (v, _) -> Symbol (v, span)
  | Keyword (v, _) -> Keyword (v, span)
  | Char (v, _) -> Char (v, span)
  | List (v, _) -> List (v, span)
  | Vector (v, _) -> Vector (v, span)
  | Curly (v, _) -> Curly (v, span)
  | Cons (a, b, _) -> Cons (a, b, span)
  | Error (v, _) -> Error (v, span)

(** Split a list of S-expressions at the first [->] symbol.

    Returns [Some (before, after)] where [before] is everything before the arrow
    and [after] is everything after it, or [None] if no arrow is found. *)
let find_arrow sexps =
  let rec go before = function
    | [] -> None
    | Symbol ("->", _) :: after -> Some (List.rev before, after)
    | x :: rest -> go (x :: before) rest
  in
  go [] sexps

(** Pretty-print an S-expression (without locations) *)
let rec to_string = function
  | Int (n, _) -> string_of_int n
  | Float (f, _) -> string_of_float f
  | String (s, _) -> Printf.sprintf "%S" s
  | Symbol (s, _) -> s
  | Keyword (s, _) -> ":" ^ s
  | Char (c, _) ->
      if c = Char.code '\\' then "?\\\\"
      else if c >= 32 && c < 127 then Printf.sprintf "?%c" (Char.chr c)
      else Printf.sprintf "?\\x%02x" c
  | List (elts, _) -> (
      (* Check for special forms that were reader macros *)
      match elts with
      | [ Symbol ("quote", _); x ] -> "'" ^ to_string x
      | [ Symbol ("backquote", _); x ] -> "`" ^ to_string x
      | [ Symbol ("unquote", _); x ] -> "," ^ to_string x
      | [ Symbol ("unquote-splicing", _); x ] -> ",@" ^ to_string x
      | [ Symbol ("function", _); x ] -> "#'" ^ to_string x
      | _ -> "(" ^ String.concat " " (List.map to_string elts) ^ ")")
  | Vector (elts, _) -> "#(" ^ String.concat " " (List.map to_string elts) ^ ")"
  | Curly (elts, _) -> "{" ^ String.concat " " (List.map to_string elts) ^ "}"
  | Cons (car, cdr, _) ->
      (* Handle improper lists: collect elements until we hit a non-Cons tail *)
      let rec collect acc = function
        | Cons (a, b, _) -> collect (a :: acc) b
        | tail -> (List.rev acc, tail)
      in
      let elts, tail = collect [ car ] cdr in
      "("
      ^ String.concat " " (List.map to_string elts)
      ^ " . " ^ to_string tail ^ ")"
  | Error (msg, _) -> Printf.sprintf "#<error: %s>" msg

(** Find the innermost S-expression containing the given position. Position is
    0-based (LSP convention). *)
let rec find_at_position ~(line : int) ~(col : int) (sexp : t) : t option =
  let span = span_of sexp in
  if not (Location.contains_position span ~line ~col) then None
  else
    (* Position is within this sexp; check children for a more specific match *)
    let children_result =
      match sexp with
      | List (children, _) ->
          List.find_map (find_at_position ~line ~col) children
      | Vector (children, _) ->
          List.find_map (find_at_position ~line ~col) children
      | Curly (children, _) ->
          List.find_map (find_at_position ~line ~col) children
      | Cons (car, cdr, _) -> (
          match find_at_position ~line ~col car with
          | Some _ as result -> result
          | None -> find_at_position ~line ~col cdr)
      | Int _ | Float _ | String _ | Symbol _ | Keyword _ | Char _ | Error _ ->
          None
    in
    (* Return the most specific match, or this sexp if no child matched *)
    match children_result with
    | Some _ as result -> result
    | None -> Some sexp

(** Find the innermost S-expression at a position across multiple forms. *)
let find_at_position_in_forms ~(line : int) ~(col : int) (forms : t list) :
    t option =
  List.find_map (find_at_position ~line ~col) forms

type position_context = {
  target : t;  (** The innermost sexp at the position *)
  enclosing_application : t option;
      (** The enclosing (fn args...) if target is fn *)
}
(** Result of find_with_context: the target sexp and optionally the enclosing
    application if the target is in function position. *)

(** Find the innermost S-expression at a position, tracking if it's in function
    position of an application. *)
let rec find_with_context ~(line : int) ~(col : int) (sexp : t) :
    position_context option =
  let span = span_of sexp in
  if not (Location.contains_position span ~line ~col) then None
  else
    match sexp with
    | List ((fn :: _args as _children), _) -> (
        (* Check if we're on the function position *)
        let fn_span = span_of fn in
        if Location.contains_position fn_span ~line ~col then
          (* Recurse into the function to find the most specific match *)
          match find_with_context ~line ~col fn with
          | Some ctx ->
              (* If we found something inside fn, check if it has an enclosing app already *)
              if Option.is_some ctx.enclosing_application then Some ctx
              else
                (* The innermost target is inside fn; this list is the enclosing app *)
                Some { target = ctx.target; enclosing_application = Some sexp }
          | None ->
              (* fn itself is the target, and this list is the enclosing app *)
              Some { target = fn; enclosing_application = Some sexp }
        else
          (* Not in function position, search all children normally *)
          let children_result =
            List.find_map (find_with_context ~line ~col) _children
          in
          match children_result with
          | Some _ as result -> result
          | None -> Some { target = sexp; enclosing_application = None })
    | List ([], _) -> Some { target = sexp; enclosing_application = None }
    | Vector (children, _) -> (
        let children_result =
          List.find_map (find_with_context ~line ~col) children
        in
        match children_result with
        | Some _ as result -> result
        | None -> Some { target = sexp; enclosing_application = None })
    | Curly (children, _) -> (
        let children_result =
          List.find_map (find_with_context ~line ~col) children
        in
        match children_result with
        | Some _ as result -> result
        | None -> Some { target = sexp; enclosing_application = None })
    | Cons (car, cdr, _) -> (
        match find_with_context ~line ~col car with
        | Some _ as result -> result
        | None -> (
            match find_with_context ~line ~col cdr with
            | Some _ as result -> result
            | None -> Some { target = sexp; enclosing_application = None }))
    | Int _ | Float _ | String _ | Symbol _ | Keyword _ | Char _ | Error _ ->
        Some { target = sexp; enclosing_application = None }

(** Find sexp with context across multiple forms. *)
let find_with_context_in_forms ~(line : int) ~(col : int) (forms : t list) :
    position_context option =
  List.find_map (find_with_context ~line ~col) forms
