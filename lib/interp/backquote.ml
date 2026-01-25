(** Backquote (quasiquote) evaluation.

    Handles the expansion of backquoted forms with unquote and unquote-splicing.
    Backquote allows constructing list structures with evaluated parts:

    - `x             -> (backquote x) -> quote x (if x has no unquotes)
    - `,y            -> (unquote y) -> evaluate y
    - `,@ys          -> (unquote-splicing ys) -> splice evaluated ys
    - `(a ,b ,@c d)  -> (list 'a b c 'd) with c spliced in

    This module is used during macro expansion to process backquoted
    template forms. *)

open Value
open Syntax

(** Evaluate a backquoted form in the given environment *)
let rec eval_backquote env global depth sexp =
  match sexp with
  (* Nested backquote - increase depth *)
  | Sexp.List ([ Sexp.Symbol ("backquote", _); inner ], _span) ->
      let inner_result = eval_backquote env global (depth + 1) inner in
      Value.Cons (Value.Symbol "backquote", Value.Cons (inner_result, Nil))
  (* Unquote at current depth - evaluate *)
  | Sexp.List ([ Sexp.Symbol ("unquote", _); inner ], _span) ->
      if depth = 1 then Eval.eval env global inner
      else
        let inner_result = eval_backquote env global (depth - 1) inner in
        Value.Cons (Value.Symbol "unquote", Value.Cons (inner_result, Nil))
  (* Unquote-splicing - should only appear in list context *)
  | Sexp.List ([ Sexp.Symbol ("unquote-splicing", _); _ ], _span) ->
      (* This case is handled specially in list processing below *)
      Eval.sexp_to_value sexp
  (* List - process each element, handling splicing *)
  | Sexp.List (elts, _span) ->
      let result = eval_list_elements env global depth elts in
      result
  (* Vector - process elements *)
  | Sexp.Vector (elts, _span) ->
      let items = eval_vector_elements env global depth elts in
      Value.Vector (Array.of_list items)
  (* Cons cell *)
  | Sexp.Cons (car, cdr, _span) ->
      let car_val = eval_backquote env global depth car in
      let cdr_val = eval_backquote env global depth cdr in
      Value.Cons (car_val, cdr_val)
  (* Everything else - quote it *)
  | _ -> Eval.sexp_to_value sexp

(** Evaluate list elements, handling unquote-splicing *)
and eval_list_elements env global depth elts =
  let rec loop acc = function
    | [] -> of_list (List.rev acc)
    | Sexp.List ([ Sexp.Symbol ("unquote-splicing", _); inner ], _span) :: rest
      when depth = 1 ->
        (* Evaluate and splice *)
        let spliced = Eval.eval env global inner in
        (match to_list spliced with
        | Some items -> loop (List.rev_append items acc) rest
        | None ->
            (* If not a proper list, just append as single element *)
            loop (spliced :: acc) rest)
    | Sexp.List ([ Sexp.Symbol ("unquote-splicing", _); inner ], _span) :: rest ->
        (* Nested backquote - decrease depth *)
        let inner_result = eval_backquote env global (depth - 1) inner in
        let form =
          Value.Cons
            (Value.Symbol "unquote-splicing", Value.Cons (inner_result, Nil))
        in
        loop (form :: acc) rest
    | elt :: rest ->
        let val_ = eval_backquote env global depth elt in
        loop (val_ :: acc) rest
  in
  loop [] elts

(** Evaluate vector elements (no splicing in vectors) *)
and eval_vector_elements env global depth elts =
  List.map (eval_backquote env global depth) elts

(** Entry point for backquote evaluation from the evaluator *)
let eval_bq env global sexp = eval_backquote env global 1 sexp
