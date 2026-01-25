(** Pure built-in functions for the Elisp interpreter.

    This module provides implementations of core Elisp functions that are
    pure (no side effects beyond the interpreter's state). Functions that
    require I/O, buffers, or other Emacs-specific state are not included
    and hit opaque boundaries. *)

open Value

(** Helper to create a builtin *)
let make name arity fn =
  Builtin { builtin_name = name; builtin_arity = arity; builtin_fn = fn }

(** Type error helper *)
let type_error expected got =
  Error (Printf.sprintf "expected %s, got %s" expected (type_name got))

(** Arity error helper *)
let arity_error name expected got =
  Error (Printf.sprintf "%s: expected %d arguments, got %d" name expected got)

(* =============================================================================
   List operations
   ============================================================================= *)

let car = function
  | [ Nil ] -> Ok Nil
  | [ Cons (a, _) ] -> Ok a
  | [ v ] -> type_error "list" v
  | args -> arity_error "car" 1 (List.length args)

let cdr = function
  | [ Nil ] -> Ok Nil
  | [ Cons (_, d) ] -> Ok d
  | [ v ] -> type_error "list" v
  | args -> arity_error "cdr" 1 (List.length args)

let cons = function
  | [ a; b ] -> Ok (Cons (a, b))
  | args -> arity_error "cons" 2 (List.length args)

let list args = Ok (of_list args)

let length = function
  | [ Nil ] -> Ok (Int 0)
  | [ Cons _ as lst ] -> (
      match to_list lst with
      | Some items -> Ok (Int (List.length items))
      | None -> Error "length: improper list")
  | [ String s ] -> Ok (Int (String.length s))
  | [ Vector arr ] -> Ok (Int (Array.length arr))
  | [ v ] -> type_error "sequence" v
  | args -> arity_error "length" 1 (List.length args)

let nth = function
  | [ Int n; lst ] -> (
      match to_list lst with
      | Some items -> (
          match List.nth_opt items n with Some v -> Ok v | None -> Ok Nil)
      | None -> Error "nth: not a proper list")
  | [ v; _ ] -> type_error "integer" v
  | args -> arity_error "nth" 2 (List.length args)

let nthcdr = function
  | [ Int n; lst ] ->
      let rec drop n l =
        if n <= 0 then l
        else match l with Cons (_, cdr) -> drop (n - 1) cdr | _ -> Nil
      in
      Ok (drop n lst)
  | [ v; _ ] -> type_error "integer" v
  | args -> arity_error "nthcdr" 2 (List.length args)

let append args =
  let rec append_two a b =
    match a with
    | Nil -> b
    | Cons (car, cdr) -> Cons (car, append_two cdr b)
    | _ -> Cons (a, b)  (* improper list case *)
  in
  Ok (List.fold_right append_two args Nil)

let reverse = function
  | [ lst ] -> (
      match to_list lst with
      | Some items -> Ok (of_list (List.rev items))
      | None -> Error "reverse: not a proper list")
  | args -> arity_error "reverse" 1 (List.length args)

let member = function
  | [ elt; lst ] ->
      let rec find = function
        | Nil -> Nil
        | Cons (car, cdr) as l -> if equal car elt then l else find cdr
        | _ -> Nil
      in
      Ok (find lst)
  | args -> arity_error "member" 2 (List.length args)

let memq = function
  | [ elt; lst ] ->
      let rec find = function
        | Nil -> Nil
        | Cons (car, cdr) as l ->
            if
              match (car, elt) with
              | Symbol a, Symbol b -> String.equal a b
              | Int a, Int b -> a = b
              | Nil, Nil -> true
              | T, T -> true
              | _ -> car == elt
            then l
            else find cdr
        | _ -> Nil
      in
      Ok (find lst)
  | args -> arity_error "memq" 2 (List.length args)

let assoc = function
  | [ key; alist ] ->
      let rec find = function
        | Nil -> Nil
        | Cons (Cons (k, _v) as pair, rest) ->
            if equal k key then pair else find rest
        | Cons (_, rest) -> find rest
        | _ -> Nil
      in
      Ok (find alist)
  | args -> arity_error "assoc" 2 (List.length args)

let assq = function
  | [ key; alist ] ->
      let rec find = function
        | Nil -> Nil
        | Cons (Cons (k, _v) as pair, rest) -> (
            match (k, key) with
            | Symbol a, Symbol b when String.equal a b -> pair
            | Int a, Int b when a = b -> pair
            | Nil, Nil -> pair
            | T, T -> pair
            | _ -> find rest)
        | Cons (_, rest) -> find rest
        | _ -> Nil
      in
      Ok (find alist)
  | args -> arity_error "assq" 2 (List.length args)

(* =============================================================================
   Arithmetic operations
   ============================================================================= *)

let numeric_binop name int_op float_op args =
  let rec loop acc = function
    | [] -> Ok acc
    | Int n :: rest -> (
        match acc with
        | Int a -> loop (Int (int_op a n)) rest
        | Float a -> loop (Float (float_op a (float_of_int n))) rest
        | _ -> type_error "number" acc)
    | Float f :: rest -> (
        match acc with
        | Int a -> loop (Float (float_op (float_of_int a) f)) rest
        | Float a -> loop (Float (float_op a f)) rest
        | _ -> type_error "number" acc)
    | v :: _ -> type_error "number" v
  in
  match args with
  | [] -> Error (Printf.sprintf "%s: requires at least one argument" name)
  | Int n :: rest -> loop (Int n) rest
  | Float f :: rest -> loop (Float f) rest
  | v :: _ -> type_error "number" v

let plus = function
  | [] -> Ok (Int 0)
  | args -> numeric_binop "+" ( + ) ( +. ) args

let minus = function
  | [] -> Error "-: requires at least one argument"
  | [ Int n ] -> Ok (Int (-n))
  | [ Float f ] -> Ok (Float (-.f))
  | args -> numeric_binop "-" ( - ) ( -. ) args

let times = function
  | [] -> Ok (Int 1)
  | args -> numeric_binop "*" ( * ) ( *. ) args

let divide = function
  | [] -> Error "/: requires at least one argument"
  | [ Int n ] -> Ok (Int (1 / n))
  | [ Float f ] -> Ok (Float (1.0 /. f))
  | Int a :: rest ->
      let rec loop acc = function
        | [] -> Ok acc
        | Int 0 :: _ -> Error "division by zero"
        | Int n :: rest -> loop (acc / n) rest
        | Float f :: _rest when f = 0.0 -> Error "division by zero"
        | Float f :: rest -> loop (int_of_float (float_of_int acc /. f)) rest
        | v :: _ -> type_error "number" v
      in
      Result.map (fun n -> Int n) (loop a rest)
  | Float a :: rest ->
      let rec loop acc = function
        | [] -> Ok acc
        | Float f :: _ when f = 0.0 -> Error "division by zero"
        | Float f :: rest -> loop (acc /. f) rest
        | Int 0 :: _ -> Error "division by zero"
        | Int n :: rest -> loop (acc /. float_of_int n) rest
        | v :: _ -> type_error "number" v
      in
      Result.map (fun f -> Float f) (loop a rest)
  | v :: _ -> type_error "number" v

let modulo = function
  | [ Int a; Int b ] when b <> 0 -> Ok (Int (a mod b))
  | [ Int _; Int _ ] -> Error "mod: division by zero"
  | [ _; Int _ ] | [ Int _; _ ] -> type_error "integer" Nil
  | args -> arity_error "mod" 2 (List.length args)

let abs_ = function
  | [ Int n ] -> Ok (Int (abs n))
  | [ Float f ] -> Ok (Float (Float.abs f))
  | [ v ] -> type_error "number" v
  | args -> arity_error "abs" 1 (List.length args)

let max_ = function
  | [] -> Error "max: requires at least one argument"
  | Int n :: rest ->
      let rec loop acc = function
        | [] -> Ok (Int acc)
        | Int m :: rest -> loop (max acc m) rest
        | v :: _ -> type_error "integer" v
      in
      loop n rest
  | Float f :: rest ->
      let rec loop acc = function
        | [] -> Ok (Float acc)
        | Float g :: rest -> loop (Float.max acc g) rest
        | Int n :: rest -> loop (Float.max acc (float_of_int n)) rest
        | v :: _ -> type_error "number" v
      in
      loop f rest
  | v :: _ -> type_error "number" v

let min_ = function
  | [] -> Error "min: requires at least one argument"
  | Int n :: rest ->
      let rec loop acc = function
        | [] -> Ok (Int acc)
        | Int m :: rest -> loop (min acc m) rest
        | v :: _ -> type_error "integer" v
      in
      loop n rest
  | Float f :: rest ->
      let rec loop acc = function
        | [] -> Ok (Float acc)
        | Float g :: rest -> loop (Float.min acc g) rest
        | Int n :: rest -> loop (Float.min acc (float_of_int n)) rest
        | v :: _ -> type_error "number" v
      in
      loop f rest
  | v :: _ -> type_error "number" v

(* =============================================================================
   Comparison operations
   ============================================================================= *)

let num_compare _name op = function
  | [] | [ _ ] -> Ok T
  | first :: rest ->
      let rec loop prev = function
        | [] -> Ok T
        | curr :: rest -> (
            match (prev, curr) with
            | Int a, Int b ->
                if op (float_of_int a) (float_of_int b) then loop curr rest
                else Ok Nil
            | Float a, Float b ->
                if op a b then loop curr rest else Ok Nil
            | Int a, Float b ->
                if op (float_of_int a) b then loop curr rest else Ok Nil
            | Float a, Int b ->
                if op a (float_of_int b) then loop curr rest else Ok Nil
            | v, _ -> type_error "number" v)
      in
      loop first rest

let lt = num_compare "<" ( < )
let gt = num_compare ">" ( > )
let le = num_compare "<=" ( <= )
let ge = num_compare ">=" ( >= )

let eq_num = function
  | [] | [ _ ] -> Ok T
  | first :: rest ->
      let rec loop prev = function
        | [] -> Ok T
        | curr :: rest -> (
            match (prev, curr) with
            | Int a, Int b -> if a = b then loop curr rest else Ok Nil
            | Float a, Float b -> if a = b then loop curr rest else Ok Nil
            | Int a, Float b ->
                if float_of_int a = b then loop curr rest else Ok Nil
            | Float a, Int b ->
                if a = float_of_int b then loop curr rest else Ok Nil
            | v, _ -> type_error "number" v)
      in
      loop first rest

(* =============================================================================
   Predicates
   ============================================================================= *)

let null_ = function
  | [ Nil ] -> Ok T
  | [ _ ] -> Ok Nil
  | args -> arity_error "null" 1 (List.length args)

let atom = function
  | [ Cons _ ] -> Ok Nil
  | [ _ ] -> Ok T
  | args -> arity_error "atom" 1 (List.length args)

let listp = function
  | [ Nil ] | [ Cons _ ] -> Ok T
  | [ _ ] -> Ok Nil
  | args -> arity_error "listp" 1 (List.length args)

let consp = function
  | [ Cons _ ] -> Ok T
  | [ _ ] -> Ok Nil
  | args -> arity_error "consp" 1 (List.length args)

let symbolp = function
  | [ Symbol _ ] | [ Nil ] | [ T ] -> Ok T
  | [ _ ] -> Ok Nil
  | args -> arity_error "symbolp" 1 (List.length args)

let stringp = function
  | [ String _ ] -> Ok T
  | [ _ ] -> Ok Nil
  | args -> arity_error "stringp" 1 (List.length args)

let numberp = function
  | [ Int _ ] | [ Float _ ] -> Ok T
  | [ _ ] -> Ok Nil
  | args -> arity_error "numberp" 1 (List.length args)

let integerp = function
  | [ Int _ ] -> Ok T
  | [ _ ] -> Ok Nil
  | args -> arity_error "integerp" 1 (List.length args)

let floatp = function
  | [ Float _ ] -> Ok T
  | [ _ ] -> Ok Nil
  | args -> arity_error "floatp" 1 (List.length args)

let vectorp = function
  | [ Vector _ ] -> Ok T
  | [ _ ] -> Ok Nil
  | args -> arity_error "vectorp" 1 (List.length args)

let functionp = function
  | [ Closure _ ] | [ Builtin _ ] -> Ok T
  | [ _ ] -> Ok Nil
  | args -> arity_error "functionp" 1 (List.length args)

let eq_ = function
  | [ a; b ] -> (
      match (a, b) with
      | Symbol sa, Symbol sb -> Ok (if String.equal sa sb then T else Nil)
      | Int ia, Int ib -> Ok (if ia = ib then T else Nil)
      | Nil, Nil | T, T -> Ok T
      | _ -> Ok (if a == b then T else Nil))
  | args -> arity_error "eq" 2 (List.length args)

let equal_ = function
  | [ a; b ] -> Ok (if equal a b then T else Nil)
  | args -> arity_error "equal" 2 (List.length args)

let not_ = function
  | [ Nil ] -> Ok T
  | [ _ ] -> Ok Nil
  | args -> arity_error "not" 1 (List.length args)

(* =============================================================================
   String operations
   ============================================================================= *)

let concat args =
  let rec loop acc = function
    | [] -> Ok (String (String.concat "" (List.rev acc)))
    | String s :: rest -> loop (s :: acc) rest
    | v :: _ -> type_error "string" v
  in
  loop [] args

let substring = function
  | [ String s; Int start ] ->
      let len = String.length s in
      if start < 0 || start > len then Error "substring: index out of range"
      else Ok (String (String.sub s start (len - start)))
  | [ String s; Int start; Int end_ ] ->
      let len = String.length s in
      if start < 0 || start > len || end_ < start || end_ > len then
        Error "substring: index out of range"
      else Ok (String (String.sub s start (end_ - start)))
  | [ String _; v ] | [ String _; _; v ] -> type_error "integer" v
  | [ v; _ ] | [ v; _; _ ] -> type_error "string" v
  | args -> arity_error "substring" 2 (List.length args)

let string_length = function
  | [ String s ] -> Ok (Int (String.length s))
  | [ v ] -> type_error "string" v
  | args -> arity_error "string-length" 1 (List.length args)

let upcase = function
  | [ String s ] -> Ok (String (String.uppercase_ascii s))
  | [ v ] -> type_error "string" v
  | args -> arity_error "upcase" 1 (List.length args)

let downcase = function
  | [ String s ] -> Ok (String (String.lowercase_ascii s))
  | [ v ] -> type_error "string" v
  | args -> arity_error "downcase" 1 (List.length args)

let string_to_list = function
  | [ String s ] ->
      let chars = List.init (String.length s) (fun i -> Int (Char.code s.[i])) in
      Ok (of_list chars)
  | [ v ] -> type_error "string" v
  | args -> arity_error "string-to-list" 1 (List.length args)

let format_ = function
  | String fmt :: args ->
      (* Very simplified format - just handles %s and %d *)
      let buf = Buffer.create (String.length fmt) in
      let rec loop i args =
        if i >= String.length fmt then Ok (String (Buffer.contents buf))
        else if fmt.[i] = '%' && i + 1 < String.length fmt then
          match (fmt.[i + 1], args) with
          | 's', String s :: rest ->
              Buffer.add_string buf s;
              loop (i + 2) rest
          | 's', v :: rest ->
              Buffer.add_string buf (to_string v);
              loop (i + 2) rest
          | 'd', Int n :: rest ->
              Buffer.add_string buf (string_of_int n);
              loop (i + 2) rest
          | '%', _ ->
              Buffer.add_char buf '%';
              loop (i + 2) args
          | c, [] -> Error (Printf.sprintf "format: not enough arguments for %%%c" c)
          | c, _v :: _ -> Error (Printf.sprintf "format: wrong type for %%%c" c)
        else (
          Buffer.add_char buf fmt.[i];
          loop (i + 1) args)
      in
      loop 0 args
  | v :: _ -> type_error "string" v
  | [] -> arity_error "format" 1 0

(* =============================================================================
   Vector operations
   ============================================================================= *)

let vector args = Ok (Vector (Array.of_list args))

let aref = function
  | [ Vector arr; Int i ] ->
      if i >= 0 && i < Array.length arr then Ok arr.(i)
      else Error "aref: index out of range"
  | [ v; Int _ ] -> type_error "vector" v
  | [ _; v ] -> type_error "integer" v
  | args -> arity_error "aref" 2 (List.length args)

let aset = function
  | [ Vector arr; Int i; v ] ->
      if i >= 0 && i < Array.length arr then (
        arr.(i) <- v;
        Ok v)
      else Error "aset: index out of range"
  | [ v; Int _; _ ] -> type_error "vector" v
  | [ _; v; _ ] -> type_error "integer" v
  | args -> arity_error "aset" 3 (List.length args)

(* =============================================================================
   Symbol operations
   ============================================================================= *)

let symbol_name = function
  | [ Symbol s ] -> Ok (String s)
  | [ Nil ] -> Ok (String "nil")
  | [ T ] -> Ok (String "t")
  | [ v ] -> type_error "symbol" v
  | args -> arity_error "symbol-name" 1 (List.length args)

(* =============================================================================
   Type coercion
   ============================================================================= *)

let number_to_string = function
  | [ Int n ] -> Ok (String (string_of_int n))
  | [ Float f ] -> Ok (String (string_of_float f))
  | [ v ] -> type_error "number" v
  | args -> arity_error "number-to-string" 1 (List.length args)

let string_to_number = function
  | [ String s ] -> (
      try Ok (Int (int_of_string s))
      with _ -> (
        try Ok (Float (float_of_string s)) with _ -> Ok (Int 0)))
  | [ v ] -> type_error "string" v
  | args -> arity_error "string-to-number" 1 (List.length args)

(* =============================================================================
   Higher-order functions (stubs - actual implementation in eval.ml)
   ============================================================================= *)

(** Note: mapcar, mapc, apply, funcall require access to the evaluator,
    so they are implemented as special forms in eval.ml rather than here. *)

(* =============================================================================
   Built-in registry
   ============================================================================= *)

(** All pure built-in functions *)
let builtins =
  [
    (* List operations *)
    make "car" (1, Some 1) car;
    make "cdr" (1, Some 1) cdr;
    make "cons" (2, Some 2) cons;
    make "list" (0, None) list;
    make "length" (1, Some 1) length;
    make "nth" (2, Some 2) nth;
    make "nthcdr" (2, Some 2) nthcdr;
    make "append" (0, None) append;
    make "reverse" (1, Some 1) reverse;
    make "member" (2, Some 2) member;
    make "memq" (2, Some 2) memq;
    make "assoc" (2, Some 2) assoc;
    make "assq" (2, Some 2) assq;
    (* Arithmetic *)
    make "+" (0, None) plus;
    make "-" (1, None) minus;
    make "*" (0, None) times;
    make "/" (1, None) divide;
    make "mod" (2, Some 2) modulo;
    make "%" (2, Some 2) modulo;
    make "abs" (1, Some 1) abs_;
    make "max" (1, None) max_;
    make "min" (1, None) min_;
    make "1+" (1, Some 1) (function
      | [ Int n ] -> Ok (Int (n + 1))
      | [ Float f ] -> Ok (Float (f +. 1.0))
      | [ v ] -> type_error "number" v
      | args -> arity_error "1+" 1 (List.length args));
    make "1-" (1, Some 1) (function
      | [ Int n ] -> Ok (Int (n - 1))
      | [ Float f ] -> Ok (Float (f -. 1.0))
      | [ v ] -> type_error "number" v
      | args -> arity_error "1-" 1 (List.length args));
    (* Comparisons *)
    make "<" (1, None) lt;
    make ">" (1, None) gt;
    make "<=" (1, None) le;
    make ">=" (1, None) ge;
    make "=" (1, None) eq_num;
    (* Predicates *)
    make "null" (1, Some 1) null_;
    make "atom" (1, Some 1) atom;
    make "listp" (1, Some 1) listp;
    make "consp" (1, Some 1) consp;
    make "symbolp" (1, Some 1) symbolp;
    make "stringp" (1, Some 1) stringp;
    make "numberp" (1, Some 1) numberp;
    make "integerp" (1, Some 1) integerp;
    make "floatp" (1, Some 1) floatp;
    make "vectorp" (1, Some 1) vectorp;
    make "functionp" (1, Some 1) functionp;
    make "eq" (2, Some 2) eq_;
    make "equal" (2, Some 2) equal_;
    make "not" (1, Some 1) not_;
    (* Strings *)
    make "concat" (0, None) concat;
    make "substring" (2, Some 3) substring;
    make "string-length" (1, Some 1) string_length;
    make "upcase" (1, Some 1) upcase;
    make "downcase" (1, Some 1) downcase;
    make "string-to-list" (1, Some 1) string_to_list;
    make "format" (1, None) format_;
    (* Vectors *)
    make "vector" (0, None) vector;
    make "aref" (2, Some 2) aref;
    make "aset" (3, Some 3) aset;
    (* Symbols *)
    make "symbol-name" (1, Some 1) symbol_name;
    (* Type coercion *)
    make "number-to-string" (1, Some 1) number_to_string;
    make "string-to-number" (1, Some 1) string_to_number;
  ]

(** Initialize global environment with builtins *)
let init_globals global =
  List.iter
    (fun b ->
      match b with
      | Builtin bi -> Hashtbl.replace global.Env.globals bi.builtin_name b
      | _ -> ())
    builtins;
  (* Add nil and t as globals *)
  Hashtbl.replace global.Env.globals "nil" Nil;
  Hashtbl.replace global.Env.globals "t" T
