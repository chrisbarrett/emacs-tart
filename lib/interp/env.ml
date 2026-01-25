(** Environment management for the Elisp interpreter.

    The environment tracks:
    - Lexical bindings (let, lambda parameters)
    - Global variable definitions (defvar, defconst)
    - Macro definitions (defmacro)
    - Special variable declarations

    This module provides a higher-level interface over Value.env. *)

open Value

(** The global interpreter state *)
type global = {
  mutable globals : (string, value) Hashtbl.t;  (** Global variable bindings *)
  mutable macros : (string, macro) Hashtbl.t;  (** Macro definitions *)
  mutable specials : (string, bool) Hashtbl.t;  (** Special (dynamic) variables *)
}

(** Create a fresh global state *)
let make_global () =
  {
    globals = Hashtbl.create 256;
    macros = Hashtbl.create 64;
    specials = Hashtbl.create 32;
  }

(** Default global state *)
let default_global = make_global ()

(** Look up a variable, checking lexical env first, then globals *)
let lookup_var name (env : env) global =
  match lookup name env with Some v -> Some v | None -> Hashtbl.find_opt global.globals name

(** Set a variable - lexical if bound, otherwise global *)
let set_var name value (env : env) global =
  if set name value env then ()
  else Hashtbl.replace global.globals name value

(** Define a global variable *)
let define_global name value global = Hashtbl.replace global.globals name value

(** Define a macro *)
let define_macro name macro global = Hashtbl.replace global.macros name macro

(** Look up a macro *)
let lookup_macro name global = Hashtbl.find_opt global.macros name

(** Check if a symbol is a macro *)
let is_macro name global = Hashtbl.mem global.macros name

(** Declare a variable as special (dynamically scoped) *)
let declare_special name global = Hashtbl.replace global.specials name true

(** Check if a variable is special *)
let is_special name global = Hashtbl.mem global.specials name

(** Create a new lexical environment for a function call *)
let make_call_env params args env =
  let rec bind_params req opt rest args bindings =
    match (req, args) with
    | r :: rs, a :: as_ -> bind_params rs opt rest as_ ((r, a) :: bindings)
    | r :: _, [] -> Error (Printf.sprintf "missing required argument: %s" r)
    | [], args -> (
        (* All required params bound, now handle optional *)
        match (opt, args) with
        | (o, _default) :: os, a :: as_ ->
            bind_params [] os rest as_ ((o, a) :: bindings)
        | (o, default_val) :: os, [] ->
            let v = Option.value default_val ~default:Nil in
            bind_params [] os rest [] ((o, v) :: bindings)
        | [], args -> (
            (* All optional bound, handle rest *)
            match rest with
            | Some r -> Ok ((r, of_list args) :: bindings)
            | None when args = [] -> Ok bindings
            | None -> Error "too many arguments"))
  in
  match bind_params params.required params.optional params.rest args [] with
  | Ok bindings ->
      let scope = List.map (fun (n, v) -> (n, ref v)) bindings in
      Ok (scope :: env)
  | Error msg -> Error msg
