(** Core evaluator for the Elisp interpreter.

    Implements evaluation of Elisp expressions in a pure environment. Special
    forms are handled directly; function calls go through apply. *)

open Value
open Syntax.Sexp
open Syntax.Location

type eval_error = { message : string; span : span }
(** Evaluation error with source location *)

exception Eval_error of eval_error

(** Raise an evaluation error *)
let error span message = raise (Eval_error { message; span })

(** Raise an evaluation error with formatted message *)
let errorf span fmt = Printf.ksprintf (error span) fmt

(** Convert S-expression to value (for quote) *)
let rec sexp_to_value = function
  | Int (n, _) -> Value.Int n
  | Float (f, _) -> Value.Float f
  | String (s, _) -> Value.String s
  | Symbol ("nil", _) -> Nil
  | Symbol ("t", _) -> T
  | Symbol (s, _) -> Value.Symbol s
  | Keyword (s, _) -> Value.Keyword s
  | Char (c, _) -> Value.Int c
  | List ([], _) -> Nil
  | List (elts, _) -> of_list (List.map sexp_to_value elts)
  | Vector (elts, _) ->
      Value.Vector (Array.of_list (List.map sexp_to_value elts))
  | Curly (elts, _) ->
      (* Curly braces are for type syntax; convert to list for runtime *)
      of_list (List.map sexp_to_value elts)
  | Cons (car, cdr, _) -> Value.Cons (sexp_to_value car, sexp_to_value cdr)
  | Error (msg, _span) -> Opaque ("parse-error: " ^ msg)

(** Convert value back to S-expression (for macro expansion) *)
let rec value_to_sexp span = function
  | Nil -> List ([], span)
  | T -> Symbol ("t", span)
  | Value.Int n -> Int (n, span)
  | Value.Float f -> Float (f, span)
  | Value.String s -> String (s, span)
  | Value.Symbol s -> Symbol (s, span)
  | Value.Keyword s -> Keyword (s, span)
  | Value.Cons (car, cdr) -> (
      match to_list (Value.Cons (car, cdr)) with
      | Some items -> List (List.map (value_to_sexp span) items, span)
      | None -> Cons (value_to_sexp span car, value_to_sexp span cdr, span))
  | Value.Vector arr ->
      Vector (Array.to_list (Array.map (value_to_sexp span) arr), span)
  | Closure _ -> Symbol ("#<closure>", span)
  | Builtin b -> Symbol (Printf.sprintf "#<builtin:%s>" b.builtin_name, span)
  | Macro m -> Symbol (Printf.sprintf "#<macro:%s>" m.macro_name, span)
  | Opaque desc -> Symbol (Printf.sprintf "#<opaque:%s>" desc, span)

(** Parse a lambda parameter list *)
let parse_params span params_sexp =
  let rec loop required optional rest = function
    | [] -> { required = List.rev required; optional = List.rev optional; rest }
    | Symbol ("&optional", _) :: elts ->
        parse_optional required optional rest elts
    | Symbol ("&rest", _) :: elts -> parse_rest required optional elts
    | Symbol (name, _) :: elts -> loop (name :: required) optional rest elts
    | sexp :: _ -> error (span_of sexp) "invalid parameter"
  and parse_optional required optional rest = function
    | [] -> loop required optional rest []
    | Symbol ("&rest", _) :: elts -> parse_rest required optional elts
    | Symbol (name, _) :: elts ->
        loop required ((name, None) :: optional) rest elts
    | List ([ Symbol (name, _); _default ], _) :: elts ->
        (* We'd need to evaluate default at call time, for now just store None *)
        loop required ((name, None) :: optional) rest elts
    | sexp :: _ -> error (span_of sexp) "invalid optional parameter"
  and parse_rest required optional = function
    | [ Symbol (name, _) ] ->
        {
          required = List.rev required;
          optional = List.rev optional;
          rest = Some name;
        }
    | _ -> error span "invalid &rest parameter"
  in
  match params_sexp with
  | List (elts, _) -> loop [] [] None elts
  | Symbol ("nil", _) -> { required = []; optional = []; rest = None }
  | _ -> error span "parameters must be a list"

(** Evaluate an expression *)
let rec eval env global sexp =
  match sexp with
  (* Literals *)
  | Int (n, _) -> Value.Int n
  | Float (f, _) -> Value.Float f
  | String (s, _) -> Value.String s
  | Keyword (s, _) -> Value.Keyword s
  | Char (c, _) -> Value.Int c
  | Vector (elts, _span) ->
      Value.Vector (Array.of_list (List.map (eval env global) elts))
  | Curly (_, span) ->
      error span "curly braces are for type syntax, not expressions"
  | Error (msg, span) -> error span msg
  (* Special symbols *)
  | Symbol ("nil", _) -> Nil
  | Symbol ("t", _) -> T
  (* Variable reference *)
  | Symbol (name, span) -> (
      match Env.lookup_var name env global with
      | Some v -> v
      | None -> errorf span "void variable: %s" name)
  (* Dotted pair - shouldn't appear at top level *)
  | Cons (_, _, span) -> error span "dotted pair in expression position"
  (* Empty list is nil *)
  | List ([], _) -> Nil
  (* Special forms and function calls *)
  | List (Symbol (name, _) :: args, span) -> (
      match name with
      (* Core special forms *)
      | "quote" -> eval_quote span args
      | "backquote" -> eval_backquote env global span args
      | "function" -> eval_function env global span args
      | "if" -> eval_if env global span args
      | "and" -> eval_and env global args
      | "or" -> eval_or env global args
      | "progn" -> eval_progn env global args
      | "prog1" -> eval_prog1 env global span args
      | "prog2" -> eval_prog2 env global span args
      | "let" -> eval_let env global span args
      | "let*" -> eval_let_star env global span args
      | "lambda" -> eval_lambda env span args
      | "setq" -> eval_setq env global span args
      | "while" -> eval_while env global span args
      | "cond" -> eval_cond env global args
      (* Definition forms *)
      | "defun" -> eval_defun env global span args
      | "defmacro" -> eval_defmacro env global span args
      | "defvar" -> eval_defvar global span args
      | "defconst" -> eval_defconst global span args
      (* Higher-order functions (need evaluator access) *)
      | "funcall" -> eval_funcall env global span args
      | "apply" -> eval_apply env global span args
      | "mapcar" -> eval_mapcar env global span args
      | "mapc" -> eval_mapc env global span args
      (* Macro expansion *)
      | "macroexpand" -> eval_macroexpand global span args
      | "macroexpand-1" -> eval_macroexpand_1 global span args
      (* Check for macro call *)
      | _ when Env.is_macro name global ->
          let expanded = expand_macro_1 global span name sexp in
          eval env global expanded
      (* Regular function call *)
      | _ -> eval_call env global span name args)
  (* Function call with non-symbol in car position *)
  | List (car :: args, span) -> (
      let fn = eval env global car in
      let arg_values = List.map (eval env global) args in
      match apply global fn arg_values with
      | Ok v -> v
      | Error msg -> error span msg)

(** Evaluate multiple expressions, return last *)
and eval_progn env global = function
  | [] -> Nil
  | [ e ] -> eval env global e
  | e :: es ->
      let _ = eval env global e in
      eval_progn env global es

(** quote - return argument unevaluated *)
and eval_quote span = function
  | [ arg ] -> sexp_to_value arg
  | _ -> error span "quote requires exactly one argument"

(** backquote - quasiquote with unquote and unquote-splicing *)
and eval_backquote env global span = function
  | [ arg ] -> eval_bq env global arg
  | _ -> error span "backquote requires exactly one argument"

(** Evaluate a backquoted form with depth tracking *)
and eval_bq env global sexp = eval_bq_depth env global 1 sexp

and eval_bq_depth env global depth sexp =
  match sexp with
  (* Nested backquote - increase depth *)
  | List ([ Symbol ("backquote", _); inner ], _span) ->
      let inner_result = eval_bq_depth env global (depth + 1) inner in
      Value.Cons (Value.Symbol "backquote", Value.Cons (inner_result, Nil))
  (* Unquote at current depth - evaluate *)
  | List ([ Symbol ("unquote", _); inner ], _span) ->
      if depth = 1 then eval env global inner
      else
        let inner_result = eval_bq_depth env global (depth - 1) inner in
        Value.Cons (Value.Symbol "unquote", Value.Cons (inner_result, Nil))
  (* Unquote-splicing - should only appear in list context *)
  | List ([ Symbol ("unquote-splicing", _); _ ], _span) ->
      (* This case is handled specially in list processing below *)
      sexp_to_value sexp
  (* List - process each element, handling splicing *)
  | List (elts, _span) -> eval_bq_list env global depth elts
  (* Vector - process elements *)
  | Vector (elts, _span) ->
      let items = List.map (eval_bq_depth env global depth) elts in
      Value.Vector (Array.of_list items)
  (* Cons cell *)
  | Cons (car, cdr, _span) ->
      let car_val = eval_bq_depth env global depth car in
      let cdr_val = eval_bq_depth env global depth cdr in
      Value.Cons (car_val, cdr_val)
  (* Everything else - quote it *)
  | _ -> sexp_to_value sexp

(** Evaluate list elements in backquote context, handling unquote-splicing *)
and eval_bq_list env global depth elts =
  let rec loop acc = function
    | [] -> of_list (List.rev acc)
    | List ([ Symbol ("unquote-splicing", _); inner ], _span) :: rest
      when depth = 1 -> (
        (* Evaluate and splice *)
        let spliced = eval env global inner in
        match to_list spliced with
        | Some items -> loop (List.rev_append items acc) rest
        | None ->
            (* If not a proper list, just append as single element *)
            loop (spliced :: acc) rest)
    | List ([ Symbol ("unquote-splicing", _); inner ], _span) :: rest ->
        (* Nested backquote - decrease depth *)
        let inner_result = eval_bq_depth env global (depth - 1) inner in
        let form =
          Value.Cons
            (Value.Symbol "unquote-splicing", Value.Cons (inner_result, Nil))
        in
        loop (form :: acc) rest
    | elt :: rest ->
        let val_ = eval_bq_depth env global depth elt in
        loop (val_ :: acc) rest
  in
  loop [] elts

(** function - create closure or get function value *)
and eval_function env global span = function
  | [ Symbol (name, name_span) ] -> (
      match Env.lookup_var name env global with
      | Some ((Closure _ | Builtin _) as f) -> f
      | Some _ -> errorf name_span "%s is not a function" name
      | None -> errorf name_span "void function: %s" name)
  | [ (List (Symbol ("lambda", _) :: _, _) as lambda) ] ->
      eval env global lambda
  | _ -> error span "invalid function form"

(** if - conditional *)
and eval_if env global span = function
  | test :: then_ :: else_ ->
      let test_val = eval env global test in
      if is_truthy test_val then eval env global then_
      else eval_progn env global else_
  | _ -> error span "if requires at least 2 arguments"

(** and - short-circuit and *)
and eval_and env global = function
  | [] -> T
  | [ e ] -> eval env global e
  | e :: es ->
      let v = eval env global e in
      if is_nil v then Nil else eval_and env global es

(** or - short-circuit or *)
and eval_or env global = function
  | [] -> Nil
  | [ e ] -> eval env global e
  | e :: es ->
      let v = eval env global e in
      if is_truthy v then v else eval_or env global es

(** prog1 - evaluate all, return first *)
and eval_prog1 env global span = function
  | first :: rest ->
      let result = eval env global first in
      let _ = eval_progn env global rest in
      result
  | [] -> error span "prog1 requires at least one argument"

(** prog2 - evaluate all, return second *)
and eval_prog2 env global span = function
  | first :: second :: rest ->
      let _ = eval env global first in
      let result = eval env global second in
      let _ = eval_progn env global rest in
      result
  | _ -> error span "prog2 requires at least two arguments"

(** let - parallel binding *)
and eval_let env global span = function
  | bindings :: body ->
      let binding_list =
        match bindings with
        | List (l, _) -> l
        | Symbol ("nil", _) -> []
        | _ -> error span "let bindings must be a list"
      in
      let values =
        List.map
          (fun b ->
            match b with
            | Symbol (name, _) -> (name, Nil)
            | List ([ Symbol (name, _) ], _) -> (name, Nil)
            | List ([ Symbol (name, _); init ], _) ->
                (name, eval env global init)
            | _ -> error (span_of b) "invalid let binding")
          binding_list
      in
      let new_env = push_scope env in
      let new_env = bind_all values new_env in
      eval_progn new_env global body
  | [] -> error span "let requires bindings"

(** let* - sequential binding *)
and eval_let_star env global span = function
  | bindings :: body ->
      let binding_list =
        match bindings with
        | List (l, _) -> l
        | Symbol ("nil", _) -> []
        | _ -> error span "let* bindings must be a list"
      in
      let new_env =
        List.fold_left
          (fun env b ->
            match b with
            | Symbol (name, _) -> bind name Nil env
            | List ([ Symbol (name, _) ], _) -> bind name Nil env
            | List ([ Symbol (name, _); init ], _) ->
                let v = eval env global init in
                bind name v env
            | _ -> error (span_of b) "invalid let* binding")
          (push_scope env) binding_list
      in
      eval_progn new_env global body
  | [] -> error span "let* requires bindings"

(** lambda - create closure *)
and eval_lambda env span = function
  | params :: body ->
      let params = parse_params span params in
      Closure { params; body; env; name = None }
  | [] -> error span "lambda requires parameter list"

(** setq - variable assignment *)
and eval_setq env global span args =
  let rec loop last = function
    | [] -> last
    | [ _ ] -> error span "setq requires even number of arguments"
    | Symbol (name, _) :: value :: rest ->
        let v = eval env global value in
        Env.set_var name v env global;
        loop v rest
    | sexp :: _ -> error (span_of sexp) "setq target must be a symbol"
  in
  loop Nil args

(** while - loop *)
and eval_while env global span = function
  | test :: body ->
      while is_truthy (eval env global test) do
        ignore (eval_progn env global body)
      done;
      Nil
  | [] -> error span "while requires test"

(** cond - conditional *)
and eval_cond env global = function
  | [] -> Nil
  | List (test :: body, _) :: rest ->
      let test_val = eval env global test in
      if is_truthy test_val then
        if body = [] then test_val else eval_progn env global body
      else eval_cond env global rest
  | sexp :: _ -> error (span_of sexp) "invalid cond clause"

(** defun - define function *)
and eval_defun env global span = function
  | Symbol (name, _) :: params :: body ->
      let params = parse_params span params in
      let closure = Closure { params; body; env; name = Some name } in
      Env.define_global name closure global;
      Value.Symbol name
  | _ -> error span "invalid defun form"

(** defmacro - define macro *)
and eval_defmacro env global span = function
  | Symbol (name, _) :: params :: body ->
      let macro_params = parse_params span params in
      let macro =
        { macro_name = name; macro_params; macro_body = body; macro_env = env }
      in
      Env.define_macro name macro global;
      Value.Symbol name
  | _ -> error span "invalid defmacro form"

(** defvar - define variable *)
and eval_defvar global span = function
  | Symbol (name, _) :: _init :: _ ->
      (if not (Hashtbl.mem global.Env.globals name) then
         let v = Nil in
         (* defvar doesn't eval init if already defined *)
         Env.define_global name v global);
      Value.Symbol name
  | Symbol (name, _) :: [] ->
      Env.define_global name Nil global;
      Value.Symbol name
  | _ -> error span "invalid defvar form"

(** defconst - define constant *)
and eval_defconst global span = function
  | Symbol (name, _) :: _init :: _ ->
      let v = Nil in
      (* Would need env to eval init *)
      Env.define_global name v global;
      Value.Symbol name
  | _ -> error span "invalid defconst form"

(** Resolve a value that might be a symbol to a function *)
and resolve_function env global span v =
  match v with
  | Value.Symbol name -> (
      match Env.lookup_var name env global with
      | Some ((Closure _ | Builtin _) as f) -> f
      | Some _ -> errorf span "%s is not a function" name
      | None -> errorf span "void function: %s" name)
  | Closure _ | Builtin _ -> v
  | _ -> error span "invalid function"

(** funcall - call function with evaluated args *)
and eval_funcall env global span = function
  | fn :: args -> (
      let fn_val = eval env global fn in
      let fn_val = resolve_function env global span fn_val in
      let arg_vals = List.map (eval env global) args in
      match apply global fn_val arg_vals with
      | Ok v -> v
      | Error msg -> error span msg)
  | [] -> error span "funcall requires a function"

(** apply - call function with last arg as list *)
and eval_apply env global span = function
  | fn :: args when List.length args >= 1 -> (
      let fn_val = eval env global fn in
      let fn_val = resolve_function env global span fn_val in
      let rev_args = List.rev args in
      let last_arg = List.hd rev_args in
      let other_args = List.rev (List.tl rev_args) in
      let other_vals = List.map (eval env global) other_args in
      let last_val = eval env global last_arg in
      let final_args =
        match to_list last_val with
        | Some l -> other_vals @ l
        | None -> error span "apply: last argument must be a list"
      in
      match apply global fn_val final_args with
      | Ok v -> v
      | Error msg -> error span msg)
  | _ -> error span "apply requires function and arguments"

(** mapcar - map function over list *)
and eval_mapcar env global span = function
  | [ fn; lst ] -> (
      let fn_val = eval env global fn in
      let fn_val = resolve_function env global span fn_val in
      let lst_val = eval env global lst in
      match to_list lst_val with
      | Some items ->
          let results =
            List.map
              (fun item ->
                match apply global fn_val [ item ] with
                | Ok v -> v
                | Error msg -> error span msg)
              items
          in
          of_list results
      | None -> error span "mapcar: second argument must be a list")
  | _ -> error span "mapcar requires exactly 2 arguments"

(** mapc - map function over list, return list *)
and eval_mapc env global span = function
  | [ fn; lst ] -> (
      let fn_val = eval env global fn in
      let fn_val = resolve_function env global span fn_val in
      let lst_val = eval env global lst in
      match to_list lst_val with
      | Some items ->
          List.iter
            (fun item ->
              match apply global fn_val [ item ] with
              | Ok _ -> ()
              | Error msg -> error span msg)
            items;
          lst_val
      | None -> error span "mapc: second argument must be a list")
  | _ -> error span "mapc requires exactly 2 arguments"

(** macroexpand - fully expand a macro *)
and eval_macroexpand global span = function
  | [ form ] ->
      let value = sexp_to_value form in
      let sexp = value_to_sexp span value in
      let expanded = macroexpand_all global sexp in
      sexp_to_value expanded
  | _ -> error span "macroexpand requires exactly 1 argument"

(** macroexpand-1 - expand macro once *)
and eval_macroexpand_1 global span = function
  | [ form ] -> (
      let value = sexp_to_value form in
      let sexp = value_to_sexp span value in
      match sexp with
      | List (Symbol (name, _) :: _, _) when Env.is_macro name global ->
          sexp_to_value (expand_macro_1 global span name sexp)
      | _ -> sexp_to_value sexp)
  | _ -> error span "macroexpand-1 requires exactly 1 argument"

(** Expand a single macro call *)
and expand_macro_1 global _span name form =
  match Env.lookup_macro name global with
  | None -> form (* Not a macro *)
  | Some macro -> (
      match form with
      | List (_ :: args, call_span) -> (
          (* Bind macro parameters to unevaluated arguments *)
          let arg_values = List.map sexp_to_value args in
          let macro_env = push_scope macro.macro_env in
          match Env.make_call_env macro.macro_params arg_values macro_env with
          | Ok env ->
              (* Evaluate macro body *)
              let result = eval_progn env global macro.macro_body in
              (* Convert result back to sexp, preserving call site location *)
              value_to_sexp call_span result
          | Error msg -> error call_span msg)
      | _ -> form)

(** Fully expand all macros in a form *)
and macroexpand_all global sexp =
  match sexp with
  | List (Symbol (name, _) :: _args, span) when Env.is_macro name global ->
      (* First expand this macro *)
      let expanded = expand_macro_1 global span name sexp in
      (* Then recursively expand the result *)
      macroexpand_all global expanded
  | List (Symbol ("quote", _) :: _, _) ->
      (* Don't expand inside quote *)
      sexp
  | List (elts, span) ->
      (* Recursively expand in subforms *)
      List (List.map (macroexpand_all global) elts, span)
  | Vector (elts, span) -> Vector (List.map (macroexpand_all global) elts, span)
  | _ -> sexp

(** Evaluate a function call *)
and eval_call env global span name args =
  match Env.lookup_var name env global with
  | Some fn -> (
      let arg_values = List.map (eval env global) args in
      match apply global fn arg_values with
      | Ok v -> v
      | Error msg -> error span msg)
  | None -> errorf span "void function: %s" name

(** Apply a function to arguments *)
and apply global fn args =
  match fn with
  | Closure c -> (
      match Env.make_call_env c.params args c.env with
      | Ok env -> Ok (eval_progn env global c.body)
      | Error msg -> Error msg)
  | Builtin b -> b.builtin_fn args
  | v -> Error (Printf.sprintf "invalid function: %s" (type_name v))

(** Evaluate a top-level form *)
let eval_toplevel global sexp =
  let env = push_scope empty_env in
  try Result.Ok (eval env global sexp) with Eval_error e -> Result.Error e

(** Evaluate a string of code *)
let eval_string global source =
  let result = Syntax.Read.parse_string source in
  match result.errors with
  | err :: _ -> Result.Error { message = err.message; span = err.span }
  | [] -> (
      try
        let env = push_scope empty_env in
        let value = eval_progn env global result.sexps in
        Result.Ok value
      with Eval_error e -> Result.Error e)

(** Create a fresh interpreter state with builtins *)
let make_interpreter () =
  let global = Env.make_global () in
  Builtin.init_globals global;
  global
