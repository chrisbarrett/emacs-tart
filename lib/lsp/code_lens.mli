(** Code lens for the LSP server.

    Provides code lenses above top-level definitions showing reference counts
    and signature status. *)

type definition_kind =
  | Defun
  | Defvar
  | Defconst  (** The kind of top-level definition. *)

type definition_info = {
  name : string;
  kind : definition_kind;
  range : Protocol.range;
  selection_range : Protocol.range;
}
(** A top-level definition with its source location. *)

val extract_definitions :
  text:string -> Syntax.Sexp.t list -> definition_info list
(** [extract_definitions ~text sexps] extracts all top-level [defun], [defvar],
    and [defconst] definitions from the parsed forms. *)
