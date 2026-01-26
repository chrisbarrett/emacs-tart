(** Parser for signature file syntax.

    This module converts S-expressions into signature AST nodes.
    It handles the type syntax including:
    - Primitive types: int, string, nil, t, truthy, never, etc.
    - Type variables: a, b (when in scope of quantifiers)
    - Function types: (params) -> return
    - Type applications: (list int), (option string)
    - Union types: (int | string)
    - Quantifiers: [a b] at start of arrows or after defun name
    - Bounded quantifiers: [(a : truthy)]
*)

open Sig_ast
module Sexp = Syntax.Sexp
module Loc = Syntax.Location

(** {1 Parse Errors} *)

(** Parse error with location *)
type parse_error = {
  message : string;
  span : Loc.span;
}

(** Result type for parsing *)
type 'a result = ('a, parse_error) Result.t

(** Create an error *)
let error message span : 'a result = Error { message; span }

(** {1 Primitive Type Names} *)

(** Check if a name is a primitive type *)
let is_primitive_type name =
  match name with
  | "int" | "float" | "num" | "string" | "symbol" | "keyword"
  | "nil" | "t" | "bool" | "truthy" | "any" | "never" -> true
  | _ -> false

(** Check if a name is a type constructor *)
let is_type_constructor name =
  match name with
  | "list" | "vector" | "pair" | "option" | "hash-table" | "tuple" -> true
  | _ -> false

(** {1 Type Syntax Parsing} *)

(** Parse a type variable binder.

    Syntax:
    - [a] - unbounded
    - [(a : bound)] - bounded *)
let rec parse_tvar_binder (sexp : Sexp.t) : tvar_binder result =
  match sexp with
  | Sexp.Symbol (name, span) ->
      Ok { name; bound = None; loc = span }
  | Sexp.List ([Sexp.Symbol (name, _); Sexp.Keyword (":", _); bound_sexp], span) ->
      (match parse_sig_type bound_sexp with
       | Ok bound -> Ok { name; bound = Some bound; loc = span }
       | Error e -> Error e)
  | _ ->
      error "Expected type variable binder (name or (name : bound))" (Sexp.span_of sexp)

(** Parse a list of type variable binders enclosed in [...] *)
and parse_tvar_binders (sexp : Sexp.t) : tvar_binder list result =
  match sexp with
  | Sexp.Vector (items, _span) ->
      let rec parse_all acc = function
        | [] -> Ok (List.rev acc)
        | x :: xs ->
            (match parse_tvar_binder x with
             | Ok binder -> parse_all (binder :: acc) xs
             | Error e -> Error e)
      in
      parse_all [] items
  | _ ->
      error "Expected type variable binders [a b ...]" (Sexp.span_of sexp)

(** Parse a type expression.

    Grammar (simplified):
    - type ::= symbol                    ; primitive or type var
             | (type-con type...)        ; type application
             | (params) -> type          ; function type
             | [vars] (params) -> type   ; polymorphic function
             | (type | type | ...)       ; union type
             | (tuple type type ...)     ; tuple type
*)
and parse_sig_type (sexp : Sexp.t) : sig_type result =
  match sexp with
  (* Type variable or type constant (symbol) *)
  | Sexp.Symbol (name, span) ->
      if is_primitive_type name then
        Ok (STCon (name, span))
      else
        (* Could be a type variable or user-defined type - treat as var for now,
           resolution happens during loading *)
        Ok (STVar (name, span))

  (* Parenthesized form - could be application, union, arrow, or forall *)
  | Sexp.List (contents, span) ->
      parse_list_type contents span

  (* Vector syntax [a b] is for quantifiers, not valid as standalone type *)
  | Sexp.Vector (_, span) ->
      error "Unexpected [...] in type position (quantifiers go before ->)" span

  | _ ->
      error "Invalid type expression" (Sexp.span_of sexp)

(** Parse a list form as a type expression *)
and parse_list_type (contents : Sexp.t list) (span : Loc.span) : sig_type result =
  match contents with
  | [] ->
      (* Empty list: nil type *)
      Ok (STCon ("nil", span))

  (* Check for tuple type *)
  | Sexp.Symbol ("tuple", _) :: args ->
      parse_tuple_type args span

  (* Check for union type: (type | type | ...) *)
  | _ when has_pipe_symbol contents ->
      parse_union_type contents span

  (* Check for arrow type: (params) -> return or [vars] (params) -> return *)
  | _ when has_arrow_symbol contents ->
      parse_arrow_type contents span

  (* Type application: (type-con args...) *)
  | Sexp.Symbol (name, _) :: args when is_type_constructor name || not (is_primitive_type name) ->
      let rec parse_args acc = function
        | [] -> Ok (List.rev acc)
        | x :: xs ->
            (match parse_sig_type x with
             | Ok ty -> parse_args (ty :: acc) xs
             | Error e -> Error e)
      in
      (match parse_args [] args with
       | Ok arg_types ->
           if is_type_constructor name then
             Ok (STApp (name, arg_types, span))
           else
             (* User-defined type constructor *)
             Ok (STApp (name, arg_types, span))
       | Error e -> Error e)

  (* Single type in parens - just parse it *)
  | [single] ->
      parse_sig_type single

  (* Could be params list for an arrow type *)
  | _ ->
      (* If it looks like a list of types, it might be params waiting for -> *)
      parse_params_as_type contents span

(** Check if a list contains the arrow symbol -> *)
and has_arrow_symbol contents =
  List.exists (function
    | Sexp.Symbol ("->", _) -> true
    | _ -> false) contents

(** Check if a list contains the pipe symbol | *)
and has_pipe_symbol contents =
  List.exists (function
    | Sexp.Symbol ("|", _) -> true
    | _ -> false) contents

(** Parse a union type: (type1 | type2 | ...) *)
and parse_union_type (contents : Sexp.t list) (span : Loc.span) : sig_type result =
  (* Split by | symbols *)
  let rec split_by_pipe acc current = function
    | [] ->
        if current = [] then List.rev acc
        else List.rev (List.rev current :: acc)
    | Sexp.Symbol ("|", _) :: rest ->
        split_by_pipe (List.rev current :: acc) [] rest
    | x :: rest ->
        split_by_pipe acc (x :: current) rest
  in
  let parts = split_by_pipe [] [] contents in
  (* Each part should be a single type (or empty) *)
  let rec parse_parts acc = function
    | [] -> Ok (List.rev acc)
    | [single] :: rest ->
        (match parse_sig_type single with
         | Ok ty -> parse_parts (ty :: acc) rest
         | Error e -> Error e)
    | [] :: _ ->
        error "Empty type in union" span
    | _ :: _ ->
        error "Invalid union type syntax" span
  in
  match parse_parts [] parts with
  | Ok types when List.length types >= 2 ->
      Ok (STUnion (types, span))
  | Ok _ ->
      error "Union type requires at least two alternatives" span
  | Error e -> Error e

(** Parse a tuple type: (tuple type1 type2 ...) *)
and parse_tuple_type (args : Sexp.t list) (span : Loc.span) : sig_type result =
  let rec parse_args acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs ->
        (match parse_sig_type x with
         | Ok ty -> parse_args (ty :: acc) xs
         | Error e -> Error e)
  in
  match parse_args [] args with
  | Ok types when List.length types >= 2 ->
      Ok (STTuple (types, span))
  | Ok _ ->
      error "Tuple type requires at least two elements" span
  | Error e -> Error e

(** Parse an arrow type: (params) -> return or [vars] (params) -> return *)
and parse_arrow_type (contents : Sexp.t list) (span : Loc.span) : sig_type result =
  (* Find the -> and split *)
  let rec find_arrow before = function
    | [] -> None
    | Sexp.Symbol ("->", _) :: after -> Some (List.rev before, after)
    | x :: rest -> find_arrow (x :: before) rest
  in
  match find_arrow [] contents with
  | None ->
      error "Expected -> in function type" span
  | Some (before, [return_sexp]) ->
      (* Parse return type *)
      (match parse_sig_type return_sexp with
       | Error e -> Error e
       | Ok return_type ->
           (* Check if before starts with quantifiers [a b] *)
           (match before with
            | Sexp.Vector (_, _) as quant :: params_sexp ->
                (* Polymorphic arrow *)
                (match parse_tvar_binders quant with
                 | Error e -> Error e
                 | Ok binders ->
                     (match parse_params_list params_sexp span with
                      | Error e -> Error e
                      | Ok params ->
                          let arrow = STArrow (params, return_type, span) in
                          Ok (STForall (binders, arrow, span))))
            | _ ->
                (* Monomorphic arrow *)
                (match parse_params_list before span with
                 | Error e -> Error e
                 | Ok params ->
                     Ok (STArrow (params, return_type, span)))))
  | Some (_, _) ->
      error "Expected single return type after ->" span

(** Parse parameter list for arrow types *)
and parse_params_list (params_sexp : Sexp.t list) (span : Loc.span) : sig_param list result =
  match params_sexp with
  | [] ->
      (* No params: () -> return *)
      Ok []
  | [Sexp.List (items, _)] ->
      (* Single list: (params) -> return *)
      parse_params items
  | [single] ->
      (* Single type: type -> return *)
      (match parse_sig_type single with
       | Ok ty -> Ok [SPPositional ty]
       | Error e -> Error e)
  | _ ->
      error "Expected parameter list before ->" span

(** Parse function parameters *)
and parse_params (params : Sexp.t list) : sig_param list result =
  let rec loop acc mode = function
    | [] -> Ok (List.rev acc)
    | Sexp.Symbol ("&optional", _) :: rest ->
        loop acc `Optional rest
    | Sexp.Symbol ("&rest", _) :: rest ->
        loop acc `Rest rest
    | Sexp.Symbol ("&key", _) :: rest ->
        loop acc `Key rest
    | Sexp.Keyword (name, _) :: ty_sexp :: rest when mode = `Key ->
        (match parse_sig_type ty_sexp with
         | Ok ty -> loop (SPKey (name, ty) :: acc) `Key rest
         | Error e -> Error e)
    | ty_sexp :: rest ->
        (match parse_sig_type ty_sexp with
         | Ok ty ->
             let param = match mode with
               | `Positional -> SPPositional ty
               | `Optional -> SPOptional ty
               | `Rest -> SPRest ty
               | `Key -> SPPositional ty  (* Shouldn't happen, but fallback *)
             in
             loop (param :: acc) mode rest
         | Error e -> Error e)
  in
  loop [] `Positional params

(** Try to parse a list of types as just a sequence (fallback for ambiguous cases) *)
and parse_params_as_type (contents : Sexp.t list) (span : Loc.span) : sig_type result =
  (* If it's a single item, parse it as a type *)
  match contents with
  | [single] -> parse_sig_type single
  | _ ->
      (* Multiple items without -> is ambiguous *)
      error "Expected type expression" span

(** {1 Declaration Parsing} *)

(** Parse a defun declaration.

    Syntax:
    - (defun name (params) -> return)
    - (defun name [vars] (params) -> return)
*)
let parse_defun (contents : Sexp.t list) (span : Loc.span) : decl result =
  match contents with
  | Sexp.Symbol ("defun", _) :: Sexp.Symbol (name, _) :: rest ->
      (* Check for quantifiers *)
      let binders, params_and_return = match rest with
        | (Sexp.Vector (_, _) as quant) :: rest' ->
            (match parse_tvar_binders quant with
             | Ok b -> (b, rest')
             | Error _ -> ([], rest))  (* Fall through to parse error below *)
        | _ -> ([], rest)
      in
      (* Find -> and parse params/return *)
      let rec find_arrow before = function
        | [] -> None
        | Sexp.Symbol ("->", _) :: after -> Some (List.rev before, after)
        | x :: rest' -> find_arrow (x :: before) rest'
      in
      (match find_arrow [] params_and_return with
       | None ->
           error "Expected -> in defun signature" span
       | Some (params_sexp, [return_sexp]) ->
           (match parse_params_list params_sexp span with
            | Error e -> Error e
            | Ok params ->
                (match parse_sig_type return_sexp with
                 | Error e -> Error e
                 | Ok return_type ->
                     Ok (DDefun {
                       defun_name = name;
                       defun_tvar_binders = binders;
                       defun_params = params;
                       defun_return = return_type;
                       defun_loc = span;
                     })))
       | Some (_, _) ->
           error "Expected single return type after ->" span)
  | Sexp.Symbol ("defun", _) :: _ ->
      error "Expected function name after defun" span
  | _ ->
      error "Invalid defun syntax" span

(** Parse a defvar declaration.

    Syntax: (defvar name type) *)
let parse_defvar (contents : Sexp.t list) (span : Loc.span) : decl result =
  match contents with
  | [Sexp.Symbol ("defvar", _); Sexp.Symbol (name, _); type_sexp] ->
      (match parse_sig_type type_sexp with
       | Ok ty ->
           Ok (DDefvar {
             defvar_name = name;
             defvar_type = ty;
             defvar_loc = span;
           })
       | Error e -> Error e)
  | _ ->
      error "Expected (defvar name type)" span

(** Parse a type declaration.

    Syntax:
    - (type name) - opaque
    - (type name body) - alias
    - (type name [params] body) - parameterized alias
    - (type name [params]) - opaque with phantom params *)
let parse_type_decl (contents : Sexp.t list) (span : Loc.span) : decl result =
  match contents with
  | [Sexp.Symbol ("type", _); Sexp.Symbol (name, _)] ->
      (* Opaque type with no params *)
      Ok (DType {
        type_name = name;
        type_params = [];
        type_body = None;
        type_loc = span;
      })
  | [Sexp.Symbol ("type", _); Sexp.Symbol (name, _); body_sexp] ->
      (* Could be params or body *)
      (match body_sexp with
       | Sexp.Vector (_, _) ->
           (* Opaque type with phantom params *)
           (match parse_tvar_binders body_sexp with
            | Ok params ->
                Ok (DType {
                  type_name = name;
                  type_params = params;
                  type_body = None;
                  type_loc = span;
                })
            | Error e -> Error e)
       | _ ->
           (* Alias with no params *)
           (match parse_sig_type body_sexp with
            | Ok body ->
                Ok (DType {
                  type_name = name;
                  type_params = [];
                  type_body = Some body;
                  type_loc = span;
                })
            | Error e -> Error e))
  | [Sexp.Symbol ("type", _); Sexp.Symbol (name, _); params_sexp; body_sexp] ->
      (* Parameterized type alias *)
      (match parse_tvar_binders params_sexp with
       | Error e -> Error e
       | Ok params ->
           (match parse_sig_type body_sexp with
            | Ok body ->
                Ok (DType {
                  type_name = name;
                  type_params = params;
                  type_body = Some body;
                  type_loc = span;
                })
            | Error e -> Error e))
  | Sexp.Symbol ("type", _) :: _ ->
      error "Invalid type declaration syntax" span
  | _ ->
      error "Expected type declaration" span

(** Parse an open directive.

    Syntax: (open 'module) *)
let parse_open (contents : Sexp.t list) (span : Loc.span) : decl result =
  match contents with
  | [Sexp.Symbol ("open", _); Sexp.List ([Sexp.Symbol ("quote", _); Sexp.Symbol (name, _)], _)] ->
      Ok (DOpen (name, span))
  | _ ->
      error "Expected (open 'module-name)" span

(** Parse an include directive.

    Syntax: (include 'module) *)
let parse_include (contents : Sexp.t list) (span : Loc.span) : decl result =
  match contents with
  | [Sexp.Symbol ("include", _); Sexp.List ([Sexp.Symbol ("quote", _); Sexp.Symbol (name, _)], _)] ->
      Ok (DInclude (name, span))
  | _ ->
      error "Expected (include 'module-name)" span

(** Parse an import-struct declaration.

    Syntax: (import-struct name :slots ((slot-name type) ...)) *)
let parse_import_struct (contents : Sexp.t list) (span : Loc.span) : decl result =
  match contents with
  | [Sexp.Symbol ("import-struct", _); Sexp.Symbol (name, _);
     Sexp.Keyword ("slots", _); Sexp.List (slots, _)] ->
      let rec parse_slots acc = function
        | [] -> Ok (List.rev acc)
        | Sexp.List ([Sexp.Symbol (slot_name, _); type_sexp], _) :: rest ->
            (match parse_sig_type type_sexp with
             | Ok ty -> parse_slots ((slot_name, ty) :: acc) rest
             | Error e -> Error e)
        | _ :: _ ->
            error "Expected (slot-name type) in :slots" span
      in
      (match parse_slots [] slots with
       | Ok slot_list ->
           Ok (DImportStruct {
             struct_name = name;
             struct_slots = slot_list;
             struct_loc = span;
           })
       | Error e -> Error e)
  | Sexp.Symbol ("import-struct", _) :: _ ->
      error "Expected (import-struct name :slots ((slot type) ...))" span
  | _ ->
      error "Invalid import-struct syntax" span

(** {1 Top-Level Parsing} *)

(** Parse a single declaration *)
let parse_decl (sexp : Sexp.t) : decl result =
  match sexp with
  | Sexp.List (Sexp.Symbol ("defun", _) :: _ as contents, span) ->
      parse_defun contents span
  | Sexp.List (Sexp.Symbol ("defvar", _) :: _ as contents, span) ->
      parse_defvar contents span
  | Sexp.List (Sexp.Symbol ("type", _) :: _ as contents, span) ->
      parse_type_decl contents span
  | Sexp.List (Sexp.Symbol ("open", _) :: _ as contents, span) ->
      parse_open contents span
  | Sexp.List (Sexp.Symbol ("include", _) :: _ as contents, span) ->
      parse_include contents span
  | Sexp.List (Sexp.Symbol ("import-struct", _) :: _ as contents, span) ->
      parse_import_struct contents span
  | _ ->
      error "Expected declaration (defun, defvar, type, open, include, or import-struct)"
        (Sexp.span_of sexp)

(** Parse a signature file from S-expressions.

    @param module_name The module name (derived from filename)
    @param sexps The parsed S-expressions from the file *)
let parse_signature ~module_name (sexps : Sexp.t list) : (signature, parse_error list) Result.t =
  let rec parse_all decls errors = function
    | [] ->
        if errors = [] then
          let span = match sexps with
            | [] -> Loc.dummy_span
            | first :: _ ->
                let first_span = Sexp.span_of first in
                let last_span = Sexp.span_of (List.hd (List.rev sexps)) in
                { first_span with end_pos = last_span.end_pos }
          in
          Ok { sig_module = module_name; sig_decls = List.rev decls; sig_loc = span }
        else
          Error (List.rev errors)
    | sexp :: rest ->
        (match parse_decl sexp with
         | Ok decl -> parse_all (decl :: decls) errors rest
         | Error e -> parse_all decls (e :: errors) rest)
  in
  parse_all [] [] sexps
