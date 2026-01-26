(** Macro expansion entry point.

    This module provides the public API for macro expansion, hiding the internal
    evaluation machinery. *)

open Value
open Syntax

(** Expansion result with potential errors *)
type expansion_result =
  | Expanded of Sexp.t
  | Expansion_error of { message : string; span : Location.span }

(** Expand all macros in a form *)
let expand_all global sexp =
  try Expanded (Eval.macroexpand_all global sexp)
  with Eval.Eval_error e ->
    Expansion_error { message = e.message; span = e.span }

(** Expand a single macro call (one step) *)
let expand_1 global sexp =
  match sexp with
  | Sexp.List (Sexp.Symbol (name, _) :: _, span) when Env.is_macro name global
    -> (
      try Expanded (Eval.expand_macro_1 global span name sexp)
      with Eval.Eval_error e ->
        Expansion_error { message = e.message; span = e.span })
  | _ -> Expanded sexp

(** Load macro definitions from a parsed file *)
let load_macros global sexps =
  let _env = push_scope empty_env in
  List.iter
    (fun sexp ->
      match sexp with
      | Sexp.List (Sexp.Symbol ("defmacro", _) :: _, _) -> (
          match Eval.eval_toplevel global sexp with
          | Ok _ -> ()
          | Result.Error _ -> ())
      | _ -> ())
    sexps

(** Check if a form is a macro call *)
let is_macro_call global = function
  | Sexp.List (Sexp.Symbol (name, _) :: _, _) -> Env.is_macro name global
  | _ -> false

(** Register a macro from value representation *)
let register_macro global name params body env =
  let macro =
    {
      macro_name = name;
      macro_params = params;
      macro_body = body;
      macro_env = env;
    }
  in
  Env.define_macro name macro global
