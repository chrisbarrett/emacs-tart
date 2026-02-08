(** Call hierarchy for the LSP server.

    Provides incoming and outgoing call relationships across open [.el]
    documents. *)

let range_of_span = Span_conv.range_of_span

type defun_info = {
  name : string;
  uri : string;
  range : Protocol.range;
  selection_range : Protocol.range;
  body : Syntax.Sexp.t list;
}

let extract_defun_infos ~(uri : string) ~(text : string)
    (sexps : Syntax.Sexp.t list) : defun_info list =
  List.filter_map
    (fun sexp ->
      let open Syntax.Sexp in
      match sexp with
      | List (Symbol ("defun", _) :: Symbol (name, name_span) :: _ :: body, span)
        ->
          Some
            {
              name;
              uri;
              range = range_of_span ~text span;
              selection_range = range_of_span ~text name_span;
              body;
            }
      | _ -> None)
    sexps

(** Walk sexps and collect spans where [callee] appears in function position. *)
let find_call_sites ~(callee : string) (sexps : Syntax.Sexp.t list) :
    Syntax.Location.span list =
  let open Syntax.Sexp in
  let rec walk acc sexp =
    match sexp with
    | List (Symbol (name, span) :: args, _) when name = callee ->
        let acc = span :: acc in
        List.fold_left walk acc args
    | List (elems, _) -> List.fold_left walk acc elems
    | Vector (elems, _) -> List.fold_left walk acc elems
    | Curly (elems, _) -> List.fold_left walk acc elems
    | Cons (car, cdr, _) -> walk (walk acc car) cdr
    | Symbol _ | Int _ | Float _ | String _ | Char _ | Keyword _ | Error _ ->
        acc
  in
  List.fold_left walk [] sexps |> List.rev

(** Walk sexps and collect all symbols that appear in function position. *)
let find_callees_in_body (body : Syntax.Sexp.t list) :
    (string * Syntax.Location.span) list =
  let open Syntax.Sexp in
  let rec walk acc sexp =
    match sexp with
    | List (Symbol (name, span) :: args, _) ->
        let acc = (name, span) :: acc in
        List.fold_left walk acc args
    | List (elems, _) -> List.fold_left walk acc elems
    | Vector (elems, _) -> List.fold_left walk acc elems
    | Curly (elems, _) -> List.fold_left walk acc elems
    | Cons (car, cdr, _) -> walk (walk acc car) cdr
    | Symbol _ | Int _ | Float _ | String _ | Char _ | Keyword _ | Error _ ->
        acc
  in
  List.fold_left walk [] body |> List.rev
