(** Code lens for the LSP server.

    Provides code lenses above top-level definitions showing reference counts
    and signature status. *)

let range_of_span = Span_conv.range_of_span

type definition_kind = Defun | Defvar | Defconst

type definition_info = {
  name : string;
  kind : definition_kind;
  range : Protocol.range;
  selection_range : Protocol.range;
}

let extract_definitions ~(text : string) (sexps : Syntax.Sexp.t list) :
    definition_info list =
  List.filter_map
    (fun sexp ->
      let open Syntax.Sexp in
      match sexp with
      | List (Symbol ("defun", _) :: Symbol (name, name_span) :: _, span) ->
          Some
            {
              name;
              kind = Defun;
              range = range_of_span ~text span;
              selection_range = range_of_span ~text name_span;
            }
      | List (Symbol ("defvar", _) :: Symbol (name, name_span) :: _, span) ->
          Some
            {
              name;
              kind = Defvar;
              range = range_of_span ~text span;
              selection_range = range_of_span ~text name_span;
            }
      | List (Symbol ("defconst", _) :: Symbol (name, name_span) :: _, span) ->
          Some
            {
              name;
              kind = Defconst;
              range = range_of_span ~text span;
              selection_range = range_of_span ~text name_span;
            }
      | _ -> None)
    sexps
