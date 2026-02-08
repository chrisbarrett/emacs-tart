(** Call hierarchy for the LSP server.

    Provides incoming and outgoing call relationships across open [.el]
    documents, triggered by [callHierarchy/prepare],
    [callHierarchy/incomingCalls], and [callHierarchy/outgoingCalls] requests.
*)

type defun_info = {
  name : string;
  uri : string;
  range : Protocol.range;
  selection_range : Protocol.range;
  body : Syntax.Sexp.t list;
}
(** A function definition with its source location and body. *)

val extract_defun_infos :
  uri:string -> text:string -> Syntax.Sexp.t list -> defun_info list
(** [extract_defun_infos ~uri ~text sexps] extracts all top-level [defun]
    definitions from the parsed forms, including their bodies for outgoing call
    analysis. *)

val find_call_sites :
  callee:string -> Syntax.Sexp.t list -> Syntax.Location.span list
(** [find_call_sites ~callee sexps] walks the S-expression tree and collects
    spans of all call sites where [callee] appears in function position (head of
    a list form). *)

val find_callees_in_body :
  Syntax.Sexp.t list -> (string * Syntax.Location.span) list
(** [find_callees_in_body body] walks the S-expression list and collects all
    symbols that appear in function position (head of a list form), returning
    [(name, call_span)] pairs. *)
