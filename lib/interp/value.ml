(** Runtime value representation for Elisp interpreter *)

(** Parameter specification for functions *)
type params = {
  required : string list;  (** Required positional parameters *)
  optional : (string * value option) list;  (** &optional params with defaults *)
  rest : string option;  (** &rest parameter name *)
}

(** Lexical environment - a list of scopes, each scope is a list of bindings *)
and env = (string * value ref) list list

(** A closure captures its parameter list, body, and lexical environment *)
and closure = {
  params : params;
  body : Syntax.Sexp.t list;  (** Body forms *)
  env : env;  (** Captured lexical environment *)
  name : string option;  (** Optional name for error messages *)
}

(** A built-in function implemented in OCaml *)
and builtin = {
  builtin_name : string;
  builtin_arity : int * int option;  (** (min, max) - None means variadic *)
  builtin_fn : value list -> (value, string) result;
}

(** Macro representation *)
and macro = {
  macro_name : string;
  macro_params : params;
  macro_body : Syntax.Sexp.t list;
  macro_env : env;
}

(** Elisp runtime values.

    The interpreter uses a tagged value representation that mirrors Emacs's
    internal value types. Closures capture their lexical environment for
    proper scoping semantics.

    Note: This is a pure interpreter - values that require effects (buffers,
    processes, etc.) are represented as opaque boundaries. *)
and value =
  | Nil
  | T
  | Int of int
  | Float of float
  | String of string
  | Symbol of string
  | Keyword of string  (** Keywords like :foo *)
  | Cons of value * value
  | Vector of value array
  | Closure of closure
  | Builtin of builtin
  | Macro of macro
  | Opaque of string  (** Opaque boundary - requires type annotation *)

(** Alias for value for external use *)
type t = value

(** Builtins compare by name only *)
let equal_builtin b1 b2 = String.equal b1.builtin_name b2.builtin_name

let pp_builtin fmt b = Format.fprintf fmt "#<builtin:%s>" b.builtin_name

(** Basic equality for params *)
let equal_params p1 p2 =
  p1.required = p2.required
  && List.length p1.optional = List.length p2.optional
  && p1.rest = p2.rest

let pp_params fmt p =
  Format.fprintf fmt "(%s%s%s)"
    (String.concat " " p.required)
    (if p.optional = [] then ""
     else " &optional " ^ String.concat " " (List.map fst p.optional))
    (match p.rest with Some r -> " &rest " ^ r | None -> "")

(** Value equality (structural, except builtins compare by name) *)
let rec equal v1 v2 =
  match (v1, v2) with
  | Nil, Nil -> true
  | T, T -> true
  | Int a, Int b -> a = b
  | Float a, Float b -> a = b
  | String a, String b -> String.equal a b
  | Symbol a, Symbol b -> String.equal a b
  | Keyword a, Keyword b -> String.equal a b
  | Cons (a1, a2), Cons (b1, b2) -> equal a1 b1 && equal a2 b2
  | Vector a, Vector b ->
      Array.length a = Array.length b
      && Array.for_all2 equal a b
  | Closure _, Closure _ -> false  (* Closures don't compare equal *)
  | Builtin a, Builtin b -> equal_builtin a b
  | Macro a, Macro b -> String.equal a.macro_name b.macro_name
  | Opaque a, Opaque b -> String.equal a b
  | _ -> false

(** Closure equality - just compare by identity *)
let equal_closure _ _ = false

(** Macro equality by name *)
let equal_macro m1 m2 = String.equal m1.macro_name m2.macro_name

(** Environment equality - for deriving, not used in practice *)
let rec equal_env e1 e2 =
  match (e1, e2) with
  | [], [] -> true
  | s1 :: r1, s2 :: r2 ->
      List.length s1 = List.length s2
      && List.for_all2 (fun (n1, _) (n2, _) -> String.equal n1 n2) s1 s2
      && equal_env r1 r2
  | _ -> false

(** Pretty print value *)
let rec pp fmt = function
  | Nil -> Format.fprintf fmt "nil"
  | T -> Format.fprintf fmt "t"
  | Int n -> Format.fprintf fmt "%d" n
  | Float f -> Format.fprintf fmt "%g" f
  | String s -> Format.fprintf fmt "%S" s
  | Symbol s -> Format.fprintf fmt "%s" s
  | Keyword s -> Format.fprintf fmt ":%s" s
  | Cons (car, cdr) as v -> (
      match to_list v with
      | Some items ->
          Format.fprintf fmt "(%a)"
            (Format.pp_print_list ~pp_sep:Format.pp_print_space pp)
            items
      | None ->
          Format.fprintf fmt "(%a . %a)" pp car pp cdr)
  | Vector arr ->
      Format.fprintf fmt "#(%a)"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space pp)
        (Array.to_list arr)
  | Closure c -> (
      match c.name with
      | Some n -> Format.fprintf fmt "#<closure:%s>" n
      | None -> Format.fprintf fmt "#<closure>")
  | Builtin b -> pp_builtin fmt b
  | Macro m -> Format.fprintf fmt "#<macro:%s>" m.macro_name
  | Opaque desc -> Format.fprintf fmt "#<opaque:%s>" desc

and to_list = function
  | Nil -> Some []
  | Cons (car, cdr) -> (
      match to_list cdr with Some rest -> Some (car :: rest) | None -> None)
  | _ -> None

let pp_closure fmt c =
  match c.name with
  | Some n -> Format.fprintf fmt "#<closure:%s>" n
  | None -> Format.fprintf fmt "#<closure>"

let pp_macro fmt m = Format.fprintf fmt "#<macro:%s>" m.macro_name

let pp_env fmt env =
  let pp_binding fmt (name, _) = Format.fprintf fmt "%s" name in
  let pp_scope fmt scope =
    Format.fprintf fmt "[%a]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_binding)
      scope
  in
  Format.fprintf fmt "(%a)"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") pp_scope)
    env

let show v =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt v;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

(** Create an empty environment *)
let empty_env : env = []

(** Push a new scope onto the environment *)
let push_scope (env : env) : env = [] :: env

(** Pop the innermost scope *)
let pop_scope (env : env) : env =
  match env with [] -> [] | _ :: rest -> rest

(** Bind a variable in the innermost scope *)
let bind (name : string) (v : value) (env : env) : env =
  match env with
  | [] -> [ [ (name, ref v) ] ]
  | scope :: rest -> ((name, ref v) :: scope) :: rest

(** Bind multiple variables in the innermost scope *)
let bind_all (bindings : (string * value) list) (env : env) : env =
  List.fold_left (fun e (name, v) -> bind name v e) env bindings

(** Look up a variable in the environment *)
let rec lookup (name : string) (env : env) : value option =
  match env with
  | [] -> None
  | scope :: rest -> (
      match List.assoc_opt name scope with
      | Some r -> Some !r
      | None -> lookup name rest)

(** Set a variable in the environment (for setq) *)
let rec set (name : string) (v : value) (env : env) : bool =
  match env with
  | [] -> false
  | scope :: rest -> (
      match List.assoc_opt name scope with
      | Some r ->
          r := v;
          true
      | None -> set name v rest)

(** Check if a value is truthy (non-nil) *)
let is_truthy = function Nil -> false | _ -> true

(** Check if a value is nil *)
let is_nil = function Nil -> true | _ -> false

(** Convert a list of values to a proper list value *)
let rec of_list = function
  | [] -> Nil
  | x :: xs -> Cons (x, of_list xs)

(** Convert value to association list (list of cons cells) *)
let to_alist v =
  match to_list v with
  | None -> None
  | Some items ->
      let pairs =
        List.filter_map
          (function Cons (k, v) -> Some (k, v) | _ -> None)
          items
      in
      if List.length pairs = List.length items then Some pairs else None

(** Get the type name of a value (for error messages) *)
let type_name = function
  | Nil -> "nil"
  | T -> "t"
  | Int _ -> "integer"
  | Float _ -> "float"
  | String _ -> "string"
  | Symbol _ -> "symbol"
  | Keyword _ -> "keyword"
  | Cons _ -> "cons"
  | Vector _ -> "vector"
  | Closure _ -> "function"
  | Builtin _ -> "builtin-function"
  | Macro _ -> "macro"
  | Opaque _ -> "opaque"

(** Pretty-print a value (Elisp-style) *)
let rec to_string = function
  | Nil -> "nil"
  | T -> "t"
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | String s -> Printf.sprintf "%S" s
  | Symbol s -> s
  | Keyword s -> ":" ^ s
  | Cons (car, cdr) -> (
      (* Try to print as a proper list *)
      match to_list (Cons (car, cdr)) with
      | Some items ->
          "(" ^ String.concat " " (List.map to_string items) ^ ")"
      | None ->
          (* Print as dotted pair/improper list *)
          let rec collect acc = function
            | Cons (a, b) -> collect (a :: acc) b
            | tail -> (List.rev acc, tail)
          in
          let items, tail = collect [] (Cons (car, cdr)) in
          "("
          ^ String.concat " " (List.map to_string items)
          ^ " . " ^ to_string tail ^ ")")
  | Vector arr ->
      "#("
      ^ String.concat " " (Array.to_list (Array.map to_string arr))
      ^ ")"
  | Closure c -> (
      match c.name with
      | Some n -> Printf.sprintf "#<closure:%s>" n
      | None -> "#<closure>")
  | Builtin b -> Printf.sprintf "#<builtin:%s>" b.builtin_name
  | Macro m -> Printf.sprintf "#<macro:%s>" m.macro_name
  | Opaque desc -> Printf.sprintf "#<opaque:%s>" desc
