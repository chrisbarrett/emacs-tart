(** Parser for signature file syntax.

    This module converts S-expressions into signature AST nodes. It handles the
    type syntax including:
    - Primitive types: int, string, nil, t, truthy, never, etc.
    - Type variables: a, b (when in scope of quantifiers)
    - Function types: (params) -> return
    - Type applications: (list int), (option string)
    - Union types: (int | string)
    - Quantifiers: [a b] at start of arrows or after defun name
    - Bounded quantifiers: [(a : truthy)] *)

open Sig_ast
module Sexp = Syntax.Sexp
module Loc = Syntax.Location

(** {1 Parse Errors} *)

type parse_error = { message : string; span : Loc.span }
(** Parse error with location *)

type 'a result = ('a, parse_error) Result.t
(** Result type for parsing *)

(** Create an error *)
let error message span : 'a result = Error { message; span }

(** {1 Kind Parsing} *)

(** Parse a kind expression.

    Grammar:
    - kind ::= * | (kind -> kind)

    Examples:
    - * - concrete type kind
    - (* -> *) - unary type constructor
    - (* -> * -> *) - binary type constructor *)
let rec parse_kind (sexp : Sexp.t) : Sig_ast.sig_kind result =
  match sexp with
  | Sexp.Symbol ("*", _) -> Ok Sig_ast.SKStar
  | Sexp.List (contents, span) -> parse_kind_arrow contents span
  | _ -> error "Expected kind expression (* or (* -> *))" (Sexp.span_of sexp)

(** Parse an arrow kind: (* -> *) or (* -> * -> *) etc. *)
and parse_kind_arrow (contents : Sexp.t list) (span : Loc.span) :
    Sig_ast.sig_kind result =
  match Sexp.find_arrow contents with
  | None -> error "Expected -> in kind expression" span
  | Some ([ left ], right) -> (
      match parse_kind left with
      | Error e -> Error e
      | Ok k1 -> (
          (* Right side: either a single kind or another arrow *)
          match right with
          | [ single ] -> (
              match parse_kind single with
              | Error e -> Error e
              | Ok k2 -> Ok (Sig_ast.SKArrow (k1, k2)))
          | _ -> (
              (* Multiple items on right: treat as nested arrow *)
              match parse_kind_arrow right span with
              | Error e -> Error e
              | Ok k2 -> Ok (Sig_ast.SKArrow (k1, k2)))))
  | Some (_, _) -> error "Expected single kind before ->" span

(** {1 Primitive Type Names} *)

(** Check if a name is a primitive type *)
let is_primitive_type name =
  match name with
  | "int" | "float" | "num" | "string" | "symbol" | "keyword" | "nil" | "t"
  | "bool" | "truthy" | "any" | "never" ->
      true
  | _ -> false

(** Check if a name is a type constructor *)
let is_type_constructor name =
  match name with
  | "list" | "vector" | "cons" | "option" | "hash-table" | "tuple" | "alist"
  | "plist" | "map" ->
      true
  | _ -> false

(** {1 Type Syntax Parsing} *)

(** Check if an S-expression looks like a kind expression (starts with [*] or is
    a list containing [->] with [*] on the left). *)
let rec is_kind_expr (sexp : Sexp.t) : bool =
  match sexp with
  | Sexp.Symbol ("*", _) -> true
  | Sexp.List (contents, _) -> (
      (* Check if this looks like [* -> ...] *)
      match contents with
      | Sexp.Symbol ("*", _) :: _ -> true
      | first :: _ -> is_kind_expr first
      | [] -> false)
  | _ -> false

(** Parse a type variable binder.

    Syntax:
    - [a] - unbounded
    - [(a : bound)] - bounded by a type
    - [(f : ([*] -> [*]))] - kind-annotated *)
let rec parse_tvar_binder (sexp : Sexp.t) : tvar_binder result =
  match sexp with
  | Sexp.Symbol (name, span) ->
      Ok { name; bound = None; kind = None; loc = span }
  | Sexp.List ([ Sexp.Symbol (name, _); Sexp.Symbol (":", _); annot_sexp ], span)
    -> (
      if
        (* Distinguish between type bound and kind annotation *)
        is_kind_expr annot_sexp
      then
        match parse_kind annot_sexp with
        | Ok kind -> Ok { name; bound = None; kind = Some kind; loc = span }
        | Error e -> Error e
      else
        match parse_sig_type annot_sexp with
        | Ok bound -> Ok { name; bound = Some bound; kind = None; loc = span }
        | Error e -> Error e)
  | Sexp.List (_, span)
  | Sexp.Vector (_, span)
  | Sexp.Curly (_, span)
  | Sexp.Keyword (_, span)
  | Sexp.Int (_, span)
  | Sexp.Float (_, span)
  | Sexp.String (_, span)
  | Sexp.Char (_, span)
  | Sexp.Cons (_, _, span)
  | Sexp.Error (_, span) ->
      error
        "Expected type variable binder (name, (name : bound), or (name : kind))"
        span

(** Parse a list of type variable binders enclosed in [...] *)
and parse_tvar_binders (sexp : Sexp.t) : tvar_binder list result =
  match sexp with
  | Sexp.Vector (items, _span) ->
      let rec parse_all acc = function
        | [] -> Ok (List.rev acc)
        | x :: xs -> (
            match parse_tvar_binder x with
            | Ok binder -> parse_all (binder :: acc) xs
            | Error e -> Error e)
      in
      parse_all [] items
  | _ -> error "Expected type variable binders [a b ...]" (Sexp.span_of sexp)

(** Parse a type expression.

    Grammar (simplified):
    - type ::= symbol ; primitive or type var | (type-con type...) ; type
      application | (params) -> type ; function type | [vars] (params) -> type ;
      polymorphic function | (type | type | ...) ; union type | (tuple type type
      ...) ; tuple type *)
and parse_sig_type (sexp : Sexp.t) : sig_type result =
  match sexp with
  (* Inferred type placeholder: _ or _foo *)
  | Sexp.Symbol ("_", span) -> Ok (STInfer (None, span))
  | Sexp.Symbol (name, span) when String.length name > 0 && name.[0] = '_' ->
      Ok (STInfer (Some name, span))
  (* Type variable or type constant (symbol) *)
  | Sexp.Symbol (name, span) ->
      if is_primitive_type name then Ok (STCon (name, span))
      else
        (* Could be a type variable or user-defined type - treat as var for now,
           resolution happens during loading *)
        Ok (STVar (name, span))
  (* Parenthesized form - could be application, union, arrow, or forall *)
  | Sexp.List (contents, span) -> parse_list_type contents span
  (* Curly braces for row types: {name string age int} or {name string & r} *)
  | Sexp.Curly (contents, span) -> parse_row_type contents span
  (* Vector syntax [a b] is for quantifiers, not valid as standalone type *)
  | Sexp.Vector (_, span) ->
      error "Unexpected [...] in type position (quantifiers go before ->)" span
  | _ -> error "Invalid type expression" (Sexp.span_of sexp)

(** Parse a list form as a type expression *)
and parse_list_type (contents : Sexp.t list) (span : Loc.span) : sig_type result
    =
  match contents with
  | [] ->
      (* Empty list: nil type *)
      Ok (STCon ("nil", span))
  (* Check for tuple type *)
  | Sexp.Symbol ("tuple", _) :: args -> parse_tuple_type args span
  (* Check for union type: (type | type | ...) *)
  | _ when has_pipe_symbol contents -> parse_union_type contents span
  (* Check for subtraction type: (type - type) *)
  | _ when has_minus_symbol contents -> parse_subtract_type contents span
  (* Check for arrow type: (params) -> return or [vars] (params) -> return *)
  | _ when has_arrow_symbol contents -> parse_arrow_type contents span
  (* Type application: (type-con args...) *)
  | Sexp.Symbol (name, _) :: args
    when is_type_constructor name || not (is_primitive_type name) -> (
      let rec parse_args acc = function
        | [] -> Ok (List.rev acc)
        | x :: xs -> (
            match parse_sig_type x with
            | Ok ty -> parse_args (ty :: acc) xs
            | Error e -> Error e)
      in
      match parse_args [] args with
      | Ok arg_types ->
          if is_type_constructor name then Ok (STApp (name, arg_types, span))
          else
            (* User-defined type constructor *)
            Ok (STApp (name, arg_types, span))
      | Error e -> Error e)
  (* Single type in parens - just parse it *)
  | [ single ] -> parse_sig_type single
  (* Could be params list for an arrow type *)
  | _ ->
      (* If it looks like a list of types, it might be params waiting for -> *)
      parse_params_as_type contents span

(** Check if a list contains the arrow symbol -> *)
and has_arrow_symbol contents =
  List.exists (function Sexp.Symbol ("->", _) -> true | _ -> false) contents

(** Check if a list contains the pipe symbol | *)
and has_pipe_symbol contents =
  List.exists (function Sexp.Symbol ("|", _) -> true | _ -> false) contents

(** Check if a list contains the minus symbol - (for type subtraction) *)
and has_minus_symbol contents =
  List.exists (function Sexp.Symbol ("-", _) -> true | _ -> false) contents

(** Parse a union type: (type1 | type2 | ...) *)
and parse_union_type (contents : Sexp.t list) (span : Loc.span) :
    sig_type result =
  (* Split by | symbols *)
  let rec split_by_pipe acc current = function
    | [] ->
        if current = [] then List.rev acc else List.rev (List.rev current :: acc)
    | Sexp.Symbol ("|", _) :: rest ->
        split_by_pipe (List.rev current :: acc) [] rest
    | x :: rest -> split_by_pipe acc (x :: current) rest
  in
  let parts = split_by_pipe [] [] contents in
  (* Each part should be a single type (or empty) *)
  let rec parse_parts acc = function
    | [] -> Ok (List.rev acc)
    | [ single ] :: rest -> (
        match parse_sig_type single with
        | Ok ty -> parse_parts (ty :: acc) rest
        | Error e -> Error e)
    | [] :: _ -> error "Empty type in union" span
    | _ :: _ -> error "Invalid union type syntax" span
  in
  match parse_parts [] parts with
  | Ok types when List.length types >= 2 -> Ok (STUnion (types, span))
  | Ok _ -> error "Union type requires at least two alternatives" span
  | Error e -> Error e

(** Parse a type subtraction: (type1 - type2)

    Type subtraction removes type2 from union type1. For example:
    - ((int | string) - int) => string
    - ((truthy | nil) - nil) => truthy *)
and parse_subtract_type (contents : Sexp.t list) (span : Loc.span) :
    sig_type result =
  (* Find the - and split *)
  let rec find_minus before = function
    | [] -> None
    | Sexp.Symbol ("-", _) :: after -> Some (List.rev before, after)
    | x :: rest -> find_minus (x :: before) rest
  in
  match find_minus [] contents with
  | None -> error "Expected - in type subtraction" span
  | Some ([ minuend_sexp ], [ subtrahend_sexp ]) -> (
      match parse_sig_type minuend_sexp with
      | Error e -> Error e
      | Ok minuend -> (
          match parse_sig_type subtrahend_sexp with
          | Error e -> Error e
          | Ok subtrahend -> Ok (STSubtract (minuend, subtrahend, span))))
  | Some (_, _) ->
      error "Type subtraction requires exactly two types: (a - b)" span

(** Parse a tuple type: (tuple type1 type2 ...) *)
and parse_tuple_type (args : Sexp.t list) (span : Loc.span) : sig_type result =
  let rec parse_args acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs -> (
        match parse_sig_type x with
        | Ok ty -> parse_args (ty :: acc) xs
        | Error e -> Error e)
  in
  match parse_args [] args with
  | Ok types when List.length types >= 2 -> Ok (STTuple (types, span))
  | Ok _ -> error "Tuple type requires at least two elements" span
  | Error e -> Error e

(** Parse a row type: {name type name type ...} or {name type & var}

    Row syntax:
    - [{name string age int}] - closed row with alist field names
    - [{name string & r}] - open row with row variable
    - [{:name string :age int}] - closed row with plist keyword field names
    - [{:name string & r}] - open row with plist field names

    Fields are pairs of name (symbol or keyword) and type. The optional [& var]
    at the end specifies a row variable for open rows (row polymorphism). *)
and parse_row_type (contents : Sexp.t list) (span : Loc.span) : sig_type result
    =
  (* Check if there's a row variable (& symbol at end) *)
  let rec split_at_ampersand acc = function
    | [] -> (List.rev acc, None)
    | [ Sexp.Symbol ("&", _); Sexp.Symbol (var_name, _) ] ->
        (List.rev acc, Some var_name)
    | x :: rest -> split_at_ampersand (x :: acc) rest
  in
  let fields_sexp, row_var = split_at_ampersand [] contents in
  (* Parse fields as pairs of name/type *)
  let rec parse_fields acc = function
    | [] -> Ok (List.rev acc)
    | Sexp.Symbol (name, _) :: type_sexp :: rest -> (
        (* Alist-style field: name type *)
        match parse_sig_type type_sexp with
        | Ok ty -> parse_fields ((name, ty) :: acc) rest
        | Error e -> Error e)
    | Sexp.Keyword (name, _) :: type_sexp :: rest -> (
        (* Plist-style field: :name type - prefix with : for plist semantics *)
        match parse_sig_type type_sexp with
        | Ok ty -> parse_fields ((":" ^ name, ty) :: acc) rest
        | Error e -> Error e)
    | [ Sexp.Symbol (_, sp) ] | [ Sexp.Keyword (_, sp) ] ->
        error "Row field missing type" sp
    | other :: _ ->
        error "Expected field name (symbol or keyword)" (Sexp.span_of other)
  in
  match parse_fields [] fields_sexp with
  | Ok fields ->
      let row = { srow_fields = fields; srow_var = row_var } in
      Ok (STRow (row, span))
  | Error e -> Error e

(** Parse an arrow type: (params) -> return or [vars] (params) -> return *)
and parse_arrow_type (contents : Sexp.t list) (span : Loc.span) :
    sig_type result =
  match Sexp.find_arrow contents with
  | None -> error "Expected -> in function type" span
  | Some (before, [ return_sexp ]) -> (
      (* Parse return type *)
      match parse_sig_type return_sexp with
      | Error e -> Error e
      | Ok return_type -> (
          (* Check if before starts with quantifiers [a b] *)
          match before with
          | (Sexp.Vector (_, _) as quant) :: params_sexp -> (
              (* Polymorphic arrow *)
              match parse_tvar_binders quant with
              | Error e -> Error e
              | Ok binders -> (
                  match parse_params_list params_sexp span with
                  | Error e -> Error e
                  | Ok params ->
                      let arrow = STArrow (params, return_type, span) in
                      Ok (STForall (binders, arrow, span))))
          | _ -> (
              (* Monomorphic arrow *)
              match parse_params_list before span with
              | Error e -> Error e
              | Ok params -> Ok (STArrow (params, return_type, span)))))
  | Some (_, _) -> error "Expected single return type after ->" span

(** Parse parameter list for arrow types *)
and parse_params_list (params_sexp : Sexp.t list) (span : Loc.span) :
    sig_param list result =
  match params_sexp with
  | [] ->
      (* No params: () -> return *)
      Ok []
  | [ Sexp.List (items, _) ] ->
      (* Single list: (params) -> return *)
      parse_params items
  | [ single ] -> (
      (* Single type: type -> return *)
      match parse_sig_type single with
      | Ok ty -> Ok [ SPPositional (None, ty) ]
      | Error e -> Error e)
  | _ -> error "Expected parameter list before ->" span

(** Parse function parameters.

    Parameters can be:
    - Just a type: [int] → SPPositional (None, int)
    - Optional: [&optional int] → SPOptional (None, int)
    - Rest: [&rest any] → SPRest any
    - Keyword: [&key :name int] → SPKey ("name", int) *)
and parse_params (params : Sexp.t list) : sig_param list result =
  let rec loop acc mode = function
    | [] -> Ok (List.rev acc)
    | Sexp.Symbol ("&optional", _) :: rest -> loop acc `Optional rest
    | Sexp.Symbol ("&rest", _) :: rest -> loop acc `Rest rest
    | Sexp.Symbol ("&key", _) :: rest -> loop acc `Key rest
    | Sexp.Keyword (name, _) :: ty_sexp :: rest when mode = `Key -> (
        match parse_sig_type ty_sexp with
        | Ok ty -> loop (SPKey (name, ty) :: acc) `Key rest
        | Error e -> Error e)
    (* Literal keyword parameter for clause matching: :name *)
    | Sexp.Keyword (name, span) :: rest when mode = `Positional ->
        loop (SPLiteral (":" ^ name, span) :: acc) `Positional rest
    (* Literal quoted symbol parameter for clause matching: 'foo *)
    | Sexp.List ([ Sexp.Symbol ("quote", _); Sexp.Symbol (name, _) ], span)
      :: rest
      when mode = `Positional ->
        loop (SPLiteral (name, span) :: acc) `Positional rest
    (* Unnamed parameter: just a type *)
    | ty_sexp :: rest -> (
        match parse_sig_type ty_sexp with
        | Ok ty ->
            let param =
              match mode with
              | `Positional -> SPPositional (None, ty)
              | `Optional -> SPOptional (None, ty)
              | `Rest -> SPRest ty
              | `Key -> SPPositional (None, ty)
              (* Shouldn't happen, but fallback *)
            in
            loop (param :: acc) mode rest
        | Error e -> Error e)
  in
  loop [] `Positional params

(** Try to parse a list of types as just a sequence (fallback for ambiguous
    cases) *)
and parse_params_as_type (contents : Sexp.t list) (span : Loc.span) :
    sig_type result =
  (* If it's a single item, parse it as a type *)
  match contents with
  | [ single ] -> parse_sig_type single
  | _ ->
      (* Multiple items without -> is ambiguous *)
      error "Expected type expression" span

(** {1 Declaration Parsing} *)

(** Parse a single clause form: ((params) -> return) *)
let parse_clause (sexp : Sexp.t) (_outer_span : Loc.span) : defun_clause result
    =
  match sexp with
  | Sexp.List (contents, clause_span) -> (
      match Sexp.find_arrow contents with
      | None -> error "Expected -> in clause" clause_span
      | Some (params_sexp, [ return_sexp ]) -> (
          match parse_params_list params_sexp clause_span with
          | Error e -> Error e
          | Ok params -> (
              match parse_sig_type return_sexp with
              | Error e -> Error e
              | Ok return_type ->
                  Ok
                    {
                      clause_params = params;
                      clause_return = return_type;
                      clause_loc = clause_span;
                    }))
      | Some (_, _) -> error "Expected single return type after ->" clause_span)
  | _ -> error "Expected clause ((params) -> return)" (Sexp.span_of sexp)

(** Parse a defun declaration.

    Syntax:
    - (defun name (params) -> return) - single clause
    - (defun name [vars] (params) -> return) - single clause with quantifiers
    - (defun name ((params) -> ret) ((params) -> ret) ...) - multi-clause
    - (defun name [vars] ((params) -> ret) ...) - multi-clause with quantifiers
*)
let parse_defun (contents : Sexp.t list) (span : Loc.span) : decl result =
  match contents with
  | Sexp.Symbol ("defun", _) :: Sexp.Symbol (name, _) :: rest -> (
      (* Check for quantifiers *)
      let binders, after_binders =
        match rest with
        | (Sexp.Vector (_, _) as quant) :: rest' -> (
            match parse_tvar_binders quant with
            | Ok b -> (b, rest')
            | Error _ -> ([], rest) (* Fall through to parse error below *))
        | _ -> ([], rest)
      in
      (* Detect single-clause vs multi-clause: top-level -> present means single *)
      match Sexp.find_arrow after_binders with
      | Some (params_sexp, [ return_sexp ]) -> (
          (* Single-clause: (defun name (params) -> return) *)
          match parse_params_list params_sexp span with
          | Error e -> Error e
          | Ok params -> (
              match parse_sig_type return_sexp with
              | Error e -> Error e
              | Ok return_type ->
                  let clause =
                    {
                      clause_params = params;
                      clause_return = return_type;
                      clause_loc = span;
                    }
                  in
                  Ok
                    (DDefun
                       {
                         defun_name = name;
                         defun_tvar_binders = binders;
                         defun_clauses = [ clause ];
                         defun_loc = span;
                       })))
      | Some (_, _) -> error "Expected single return type after ->" span
      | None -> (
          (* Multi-clause: remaining forms are ((params) -> ret) clauses *)
          match after_binders with
          | [] -> error "Expected -> or clauses in defun signature" span
          | clause_sexps -> (
              let rec parse_clauses acc = function
                | [] -> Ok (List.rev acc)
                | sexp :: rest -> (
                    match parse_clause sexp span with
                    | Ok clause -> parse_clauses (clause :: acc) rest
                    | Error e -> Error e)
              in
              match parse_clauses [] clause_sexps with
              | Error e -> Error e
              | Ok clauses ->
                  Ok
                    (DDefun
                       {
                         defun_name = name;
                         defun_tvar_binders = binders;
                         defun_clauses = clauses;
                         defun_loc = span;
                       }))))
  | Sexp.Symbol ("defun", _) :: _ ->
      error "Expected function name after defun" span
  | _ -> error "Invalid defun syntax" span

(** Parse a defvar declaration.

    Syntax: (defvar name type) *)
let parse_defvar (contents : Sexp.t list) (span : Loc.span) : decl result =
  match contents with
  | [ Sexp.Symbol ("defvar", _); Sexp.Symbol (name, _); type_sexp ] -> (
      match parse_sig_type type_sexp with
      | Ok ty ->
          Ok
            (DDefvar { defvar_name = name; defvar_type = ty; defvar_loc = span })
      | Error e -> Error e)
  | _ -> error "Expected (defvar name type)" span

(** Parse a type declaration.

    Syntax:
    - (type name) - opaque
    - (type name body) - alias
    - (type name [params] body) - parameterized alias
    - (type name [params]) - opaque with phantom params *)
let parse_type_decl (contents : Sexp.t list) (span : Loc.span) : decl result =
  match contents with
  | [ Sexp.Symbol ("type", _); Sexp.Symbol (name, _) ] ->
      (* Opaque type with no params *)
      Ok
        (DType
           {
             type_name = name;
             type_params = [];
             type_body = None;
             type_loc = span;
           })
  | [ Sexp.Symbol ("type", _); Sexp.Symbol (name, _); body_sexp ] -> (
      (* Could be params or body *)
      match body_sexp with
      | Sexp.Vector (_, _) -> (
          (* Opaque type with phantom params *)
          match parse_tvar_binders body_sexp with
          | Ok params ->
              Ok
                (DType
                   {
                     type_name = name;
                     type_params = params;
                     type_body = None;
                     type_loc = span;
                   })
          | Error e -> Error e)
      | _ -> (
          (* Alias with no params *)
          match parse_sig_type body_sexp with
          | Ok body ->
              Ok
                (DType
                   {
                     type_name = name;
                     type_params = [];
                     type_body = Some body;
                     type_loc = span;
                   })
          | Error e -> Error e))
  | [ Sexp.Symbol ("type", _); Sexp.Symbol (name, _); params_sexp; body_sexp ]
    -> (
      (* Parameterized type alias *)
      match parse_tvar_binders params_sexp with
      | Error e -> Error e
      | Ok params -> (
          match parse_sig_type body_sexp with
          | Ok body ->
              Ok
                (DType
                   {
                     type_name = name;
                     type_params = params;
                     type_body = Some body;
                     type_loc = span;
                   })
          | Error e -> Error e))
  | Sexp.Symbol ("type", _) :: _ -> error "Invalid type declaration syntax" span
  | _ -> error "Expected type declaration" span

(** Parse an open directive.

    Syntax: (open 'module) *)
let parse_open (contents : Sexp.t list) (span : Loc.span) : decl result =
  match contents with
  | [
   Sexp.Symbol ("open", _);
   Sexp.List ([ Sexp.Symbol ("quote", _); Sexp.Symbol (name, _) ], _);
  ] ->
      Ok (DOpen (name, span))
  | _ -> error "Expected (open 'module-name)" span

(** Parse an include directive.

    Syntax: (include 'module) *)
let parse_include (contents : Sexp.t list) (span : Loc.span) : decl result =
  match contents with
  | [
   Sexp.Symbol ("include", _);
   Sexp.List ([ Sexp.Symbol ("quote", _); Sexp.Symbol (name, _) ], _);
  ] ->
      Ok (DInclude (name, span))
  | _ -> error "Expected (include 'module-name)" span

(** Parse an import-struct declaration.

    Syntax: (import-struct name :slots ((slot-name type) ...)) *)
let parse_import_struct (contents : Sexp.t list) (span : Loc.span) : decl result
    =
  match contents with
  | [
   Sexp.Symbol ("import-struct", _);
   Sexp.Symbol (name, _);
   Sexp.Keyword ("slots", _);
   Sexp.List (slots, _);
  ] -> (
      let rec parse_slots acc = function
        | [] -> Ok (List.rev acc)
        | Sexp.List ([ Sexp.Symbol (slot_name, _); type_sexp ], _) :: rest -> (
            match parse_sig_type type_sexp with
            | Ok ty -> parse_slots ((slot_name, ty) :: acc) rest
            | Error e -> Error e)
        | _ :: _ -> error "Expected (slot-name type) in :slots" span
      in
      match parse_slots [] slots with
      | Ok slot_list ->
          Ok
            (DImportStruct
               {
                 struct_name = name;
                 struct_slots = slot_list;
                 struct_loc = span;
               })
      | Error e -> Error e)
  | Sexp.Symbol ("import-struct", _) :: _ ->
      error "Expected (import-struct name :slots ((slot type) ...))" span
  | _ -> error "Invalid import-struct syntax" span

(** Parse a constructor declaration.

    Syntax:
    - (Name) - nullary constructor
    - (Name type) - single-field constructor
    - (Name type type ...) - multi-field constructor *)
let parse_ctor (sexp : Sexp.t) : ctor_decl result =
  match sexp with
  | Sexp.List (Sexp.Symbol (name, _) :: field_sexps, span) -> (
      let rec parse_fields acc = function
        | [] -> Ok (List.rev acc)
        | x :: xs -> (
            match parse_sig_type x with
            | Ok ty -> parse_fields (ty :: acc) xs
            | Error e -> Error e)
      in
      match parse_fields [] field_sexps with
      | Ok fields ->
          Ok { ctor_name = name; ctor_fields = fields; ctor_loc = span }
      | Error e -> Error e)
  | Sexp.Symbol (name, span) ->
      (* Bare symbol: nullary constructor without parens *)
      Ok { ctor_name = name; ctor_fields = []; ctor_loc = span }
  | _ -> error "Expected constructor (Name field-types...)" (Sexp.span_of sexp)

(** Parse a data declaration.

    Syntax:
    - (data Name (Ctor1) (Ctor2 type)) - without type params
    - (data Name [a] (Ctor1 a) (Ctor2)) - with type params *)
let parse_data (contents : Sexp.t list) (span : Loc.span) : decl result =
  match contents with
  | Sexp.Symbol ("data", _) :: Sexp.Symbol (name, _) :: rest -> (
      (* Check for optional type parameters [a b ...] *)
      let params, ctor_sexps =
        match rest with
        | (Sexp.Vector (_, _) as quant) :: rest' -> (
            match parse_tvar_binders quant with
            | Ok p -> (p, rest')
            | Error _ -> ([], rest) (* Fall through; will fail on ctors *))
        | _ -> ([], rest)
      in
      (* Parse constructor declarations *)
      let rec parse_ctors acc = function
        | [] -> Ok (List.rev acc)
        | x :: xs -> (
            match parse_ctor x with
            | Ok ctor -> parse_ctors (ctor :: acc) xs
            | Error e -> Error e)
      in
      match parse_ctors [] ctor_sexps with
      | Ok ctors when List.length ctors > 0 ->
          Ok
            (DData
               {
                 data_name = name;
                 data_params = params;
                 data_ctors = ctors;
                 data_loc = span;
               })
      | Ok _ -> error "Data declaration requires at least one constructor" span
      | Error e -> Error e)
  | Sexp.Symbol ("data", _) :: _ -> error "Expected type name after data" span
  | _ -> error "Invalid data declaration syntax" span

(** {1 Top-Level Parsing} *)

(** Parse a single declaration *)
let rec parse_decl (sexp : Sexp.t) : decl result =
  match sexp with
  | Sexp.List ((Sexp.Symbol ("defun", _) :: _ as contents), span) ->
      parse_defun contents span
  | Sexp.List ((Sexp.Symbol ("defvar", _) :: _ as contents), span) ->
      parse_defvar contents span
  | Sexp.List ((Sexp.Symbol ("type", _) :: _ as contents), span) ->
      parse_type_decl contents span
  | Sexp.List ((Sexp.Symbol ("open", _) :: _ as contents), span) ->
      parse_open contents span
  | Sexp.List ((Sexp.Symbol ("include", _) :: _ as contents), span) ->
      parse_include contents span
  | Sexp.List ((Sexp.Symbol ("import-struct", _) :: _ as contents), span) ->
      parse_import_struct contents span
  | Sexp.List ((Sexp.Symbol ("data", _) :: _ as contents), span) ->
      parse_data contents span
  | Sexp.List ((Sexp.Symbol ("type-scope", _) :: _ as contents), span) ->
      parse_type_scope contents span
  | Sexp.List ((Sexp.Symbol ("let", _) :: _ as contents), span) ->
      parse_let contents span
  | _ ->
      error
        "Expected declaration (defun, defvar, type, data, let, open, include, \
         import-struct, or type-scope)"
        (Sexp.span_of sexp)

(** Parse a type-scope declaration.

    Grammar: (type-scope [vars] decl...)

    Examples:
    - (type-scope [a] (defun iter-next ((iter a)) -> (a | nil)) (defun iter-peek
      ((iter a)) -> (a | nil))) *)
and parse_type_scope (contents : Sexp.t list) (span : Loc.span) : decl result =
  match contents with
  | [ Sexp.Symbol ("type-scope", _) ] ->
      error "Expected type variables [a b ...] after type-scope" span
  | [ Sexp.Symbol ("type-scope", _); _ ] ->
      error "Expected declarations after type-scope [vars]" span
  | Sexp.Symbol ("type-scope", _) :: (Sexp.Vector (_, _) as binders) :: decls
    -> (
      match parse_tvar_binders binders with
      | Ok tvar_binders -> (
          (* Parse each inner declaration *)
          let rec parse_all acc = function
            | [] -> Ok (List.rev acc)
            | sexp :: rest -> (
                match parse_decl sexp with
                | Ok d -> parse_all (d :: acc) rest
                | Error e -> Error e)
          in
          match parse_all [] decls with
          | Ok scope_decls ->
              Ok
                (DTypeScope
                   {
                     scope_tvar_binders = tvar_binders;
                     scope_decls;
                     scope_loc = span;
                   })
          | Error e -> Error e)
      | Error e -> Error e)
  | Sexp.Symbol ("type-scope", _) :: _ ->
      error "Expected type variables [a b ...] after type-scope" span
  | _ -> error "Invalid type-scope declaration syntax" span

(** Parse a let declaration for local type aliases.

    Grammar: (let [(type name body) (type name [vars] body) ...] decl...)

    Examples:
    - (let [(type pair (cons int int))] (defun swap-pair (pair) -> pair) (defun
      make-pair (int int) -> pair)) *)
and parse_let (contents : Sexp.t list) (span : Loc.span) : decl result =
  match contents with
  | [ Sexp.Symbol ("let", _) ] -> error "Expected bindings list after let" span
  | [ Sexp.Symbol ("let", _); _ ] ->
      error "Expected declarations after let bindings" span
  | Sexp.Symbol ("let", _) :: Sexp.List (bindings_sexp, _) :: body_sexp -> (
      (* Parse each binding: (type name body) or (type name [vars] body) *)
      let rec parse_bindings acc = function
        | [] -> Ok (List.rev acc)
        | sexp :: rest -> (
            match parse_let_binding sexp with
            | Ok binding -> parse_bindings (binding :: acc) rest
            | Error e -> Error e)
      in
      match parse_bindings [] bindings_sexp with
      | Error e -> Error e
      | Ok bindings -> (
          (* Parse body declarations *)
          let rec parse_body acc = function
            | [] -> Ok (List.rev acc)
            | sexp :: rest -> (
                match parse_decl sexp with
                | Ok d -> parse_body (d :: acc) rest
                | Error e -> Error e)
          in
          match parse_body [] body_sexp with
          | Error e -> Error e
          | Ok body ->
              Ok
                (DLet
                   { let_bindings = bindings; let_body = body; let_loc = span })
          ))
  | Sexp.Symbol ("let", _) :: _ ->
      error "Expected bindings list [(type name def)...] after let" span
  | _ -> error "Invalid let syntax" span

(** Parse a single let type binding.

    Grammar:
    - (type name body) - simple alias
    - (type name [vars] body) - parameterized alias *)
and parse_let_binding (sexp : Sexp.t) : let_type_binding result =
  match sexp with
  | Sexp.List (contents, span) -> (
      match contents with
      | [ Sexp.Symbol ("type", _); Sexp.Symbol (name, _); body_sexp ] -> (
          (* Simple alias: (type name body) *)
          match body_sexp with
          | Sexp.Vector (_, _) ->
              error "Expected type body, not parameter list"
                (Sexp.span_of body_sexp)
          | _ -> (
              match parse_sig_type body_sexp with
              | Ok body ->
                  Ok
                    {
                      ltb_name = name;
                      ltb_params = [];
                      ltb_body = body;
                      ltb_loc = span;
                    }
              | Error e -> Error e))
      | [
       Sexp.Symbol ("type", _); Sexp.Symbol (name, _); params_sexp; body_sexp;
      ] -> (
          (* Parameterized alias: (type name [vars] body) *)
          match parse_tvar_binders params_sexp with
          | Error e -> Error e
          | Ok params -> (
              match parse_sig_type body_sexp with
              | Ok body ->
                  Ok
                    {
                      ltb_name = name;
                      ltb_params = params;
                      ltb_body = body;
                      ltb_loc = span;
                    }
              | Error e -> Error e))
      | Sexp.Symbol ("type", _) :: _ ->
          error "Expected (type name [vars] body)" span
      | _ -> error "Let binding must be a (type name body) form" span)
  | _ -> error "Expected (type name body) in let bindings" (Sexp.span_of sexp)

(** Parse a signature file from S-expressions.

    @param module_name The module name (derived from filename)
    @param sexps The parsed S-expressions from the file *)
let parse_signature ~module_name (sexps : Sexp.t list) :
    (signature, parse_error list) Result.t =
  let rec parse_all decls errors = function
    | [] ->
        if errors = [] then
          let span =
            match sexps with
            | [] -> Loc.dummy_span
            | first :: _ ->
                let first_span = Sexp.span_of first in
                let last_span = Sexp.span_of (List.hd (List.rev sexps)) in
                { first_span with end_pos = last_span.end_pos }
          in
          Ok
            {
              sig_module = module_name;
              sig_decls = List.rev decls;
              sig_loc = span;
            }
        else Error (List.rev errors)
    | sexp :: rest -> (
        match parse_decl sexp with
        | Ok decl -> parse_all (decl :: decls) errors rest
        | Error e -> parse_all decls (e :: errors) rest)
  in
  parse_all [] [] sexps
