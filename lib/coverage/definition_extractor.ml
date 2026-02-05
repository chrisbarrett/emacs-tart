(** Extract definitions from Emacs Lisp files.

    This module parses `.el` files and extracts all public definitions including:
    - Functions (defun, defsubst, cl-defun, defmacro, cl-defmacro)
    - Variables (defvar, defcustom, defconst, defvar-local)
    - Structs (cl-defstruct with generated accessors)
    - Classes (defclass with slots)
    - Faces (defface)

    See Spec 28 for full requirements. *)

(** {1 Types} *)

type def_kind =
  | Function  (** defun, defsubst, cl-defun *)
  | Macro  (** defmacro, cl-defmacro *)
  | Variable  (** defvar, defcustom, defconst, defvar-local *)
  | Face  (** defface *)
  | Struct  (** cl-defstruct type/constructor *)
  | StructAccessor  (** cl-defstruct field accessor *)
  | StructPredicate  (** cl-defstruct -p predicate *)
  | StructCopier  (** cl-defstruct copy- function *)
  | Class  (** defclass type *)
  | ClassAccessor  (** defclass slot accessor *)
  | ClassPredicate  (** defclass -p predicate *)

type definition = {
  name : string;
  kind : def_kind;
  span : Syntax.Location.span;
  is_private : bool;  (** Contains -- in name *)
}
(** A definition extracted from source code. *)

type extraction_result = { filename : string; definitions : definition list }
(** Result of extracting definitions from a file. *)

(** {1 Private Identifier Detection} *)

(** Check if a name is private (contains --). *)
let is_private_name (name : string) : bool =
  try
    let _ = Str.search_forward (Str.regexp_string "--") name 0 in
    true
  with Not_found -> false

(** {1 Struct Expansion} *)

(** Generate definitions from a cl-defstruct.

    Given (cl-defstruct name field1 field2), generates:
    - name (constructor)
    - make-name (constructor alias)
    - name-p (predicate)
    - copy-name (copier)
    - name-field1, name-field2 (accessors) *)
let expand_struct ~(struct_name : string) ~(fields : string list)
    ~(span : Syntax.Location.span) : definition list =
  let is_private = is_private_name struct_name in
  let base =
    [
      { name = struct_name; kind = Struct; span; is_private };
      {
        name = "make-" ^ struct_name;
        kind = Struct;
        span;
        is_private = is_private_name ("make-" ^ struct_name);
      };
      {
        name = struct_name ^ "-p";
        kind = StructPredicate;
        span;
        is_private = is_private_name (struct_name ^ "-p");
      };
      {
        name = "copy-" ^ struct_name;
        kind = StructCopier;
        span;
        is_private = is_private_name ("copy-" ^ struct_name);
      };
    ]
  in
  let accessors =
    List.map
      (fun field ->
        let accessor_name = struct_name ^ "-" ^ field in
        {
          name = accessor_name;
          kind = StructAccessor;
          span;
          is_private = is_private_name accessor_name;
        })
      fields
  in
  base @ accessors

(** {1 Class Expansion} *)

(** Generate definitions from a defclass.

    Given (defclass name () ((slot :accessor slot-name))), generates:
    - name (class/constructor)
    - name-p (predicate)
    - accessors from :accessor keywords *)
let expand_class ~(class_name : string) ~(accessors : string list)
    ~(span : Syntax.Location.span) : definition list =
  let is_private = is_private_name class_name in
  let base =
    [
      { name = class_name; kind = Class; span; is_private };
      {
        name = class_name ^ "-p";
        kind = ClassPredicate;
        span;
        is_private = is_private_name (class_name ^ "-p");
      };
    ]
  in
  let accessor_defs =
    List.map
      (fun name ->
        { name; kind = ClassAccessor; span; is_private = is_private_name name })
      accessors
  in
  base @ accessor_defs

(** {1 Extraction} *)

(** Extract the name from a definition form. *)
let extract_name (sexp : Syntax.Sexp.t) : string option =
  match sexp with Syntax.Sexp.Symbol (name, _) -> Some name | _ -> None

(** Extract struct field names from slot specs.

    Handles both simple symbols and lists with options. *)
let extract_struct_fields (args : Syntax.Sexp.t list) : string list =
  let extract_field sexp =
    match sexp with
    | Syntax.Sexp.Symbol (name, _) ->
        (* Skip keywords and special options *)
        if String.length name > 0 && name.[0] = ':' then None else Some name
    | Syntax.Sexp.List (Syntax.Sexp.Symbol (name, _) :: _, _) ->
        (* (field-name :type ...) form *)
        if String.length name > 0 && name.[0] = ':' then None else Some name
    | _ -> None
  in
  List.filter_map extract_field args

(** Extract accessor names from defclass slots.

    Looks for :accessor keyword in slot specifications. Note: Keywords are
    stored without the leading colon by the lexer. *)
let extract_class_accessors (slots : Syntax.Sexp.t list) : string list =
  let extract_from_slot sexp =
    match sexp with
    | Syntax.Sexp.List (elems, _) ->
        let rec find_accessor = function
          | [] -> []
          (* Keyword is stored without the colon by the lexer *)
          | Syntax.Sexp.Keyword ("accessor", _)
            :: Syntax.Sexp.Symbol (name, _)
            :: rest ->
              name :: find_accessor rest
          | _ :: rest -> find_accessor rest
        in
        find_accessor elems
    | _ -> []
  in
  List.concat_map extract_from_slot slots

(** Extract a definition from a single S-expression. *)
let extract_from_sexp (sexp : Syntax.Sexp.t) : definition list =
  let open Syntax.Sexp in
  match sexp with
  (* Function definitions *)
  | List
      (Symbol (("defun" | "defsubst" | "cl-defun"), _) :: name_sexp :: _, span)
    -> (
      match extract_name name_sexp with
      | Some name ->
          [ { name; kind = Function; span; is_private = is_private_name name } ]
      | None -> [])
  (* Macro definitions *)
  | List (Symbol (("defmacro" | "cl-defmacro"), _) :: name_sexp :: _, span) -> (
      match extract_name name_sexp with
      | Some name ->
          [ { name; kind = Macro; span; is_private = is_private_name name } ]
      | None -> [])
  (* Variable definitions *)
  | List
      ( Symbol (("defvar" | "defcustom" | "defconst" | "defvar-local"), _)
        :: name_sexp :: _,
        span ) -> (
      match extract_name name_sexp with
      | Some name ->
          [ { name; kind = Variable; span; is_private = is_private_name name } ]
      | None -> [])
  (* Face definitions *)
  | List (Symbol ("defface", _) :: name_sexp :: _, span) -> (
      match extract_name name_sexp with
      | Some name ->
          [ { name; kind = Face; span; is_private = is_private_name name } ]
      | None -> [])
  (* Struct definitions *)
  | List (Symbol ("cl-defstruct", _) :: name_sexp :: rest, span) -> (
      match name_sexp with
      | Symbol (struct_name, _) ->
          let fields = extract_struct_fields rest in
          expand_struct ~struct_name ~fields ~span
      | List (Symbol (struct_name, _) :: _, _) ->
          (* (cl-defstruct (name options) ...) form *)
          let fields = extract_struct_fields rest in
          expand_struct ~struct_name ~fields ~span
      | _ -> [])
  (* Class definitions *)
  | List
      (Symbol ("defclass", _) :: name_sexp :: _parents :: slots_sexp :: _, span)
    -> (
      match (name_sexp, slots_sexp) with
      | Symbol (class_name, _), List (slots, _) ->
          let accessors = extract_class_accessors slots in
          expand_class ~class_name ~accessors ~span
      | _ -> [])
  (* defadvice *)
  | List (Symbol ("defadvice", _) :: name_sexp :: _, span) -> (
      match extract_name name_sexp with
      | Some name ->
          [ { name; kind = Function; span; is_private = is_private_name name } ]
      | None -> [])
  (* cl-defmethod - extract generic name *)
  | List (Symbol ("cl-defmethod", _) :: name_sexp :: _, span) -> (
      match extract_name name_sexp with
      | Some name ->
          [ { name; kind = Function; span; is_private = is_private_name name } ]
      | None -> [])
  (* cl-defgeneric *)
  | List (Symbol ("cl-defgeneric", _) :: name_sexp :: _, span) -> (
      match extract_name name_sexp with
      | Some name ->
          [ { name; kind = Function; span; is_private = is_private_name name } ]
      | None -> [])
  | _ -> []

(** Extract all definitions from a list of S-expressions. *)
let extract_definitions (sexps : Syntax.Sexp.t list) : definition list =
  List.concat_map extract_from_sexp sexps

(** Extract definitions from a file. *)
let extract_from_file (filename : string) : extraction_result option =
  if not (Sys.file_exists filename) then None
  else
    try
      let ic = In_channel.open_text filename in
      let content = In_channel.input_all ic in
      In_channel.close ic;
      let parse_result = Syntax.Read.parse_string ~filename content in
      let definitions = extract_definitions parse_result.sexps in
      Some { filename; definitions }
    with _ -> None

(** {1 Utilities} *)

(** Pretty-print a definition kind. *)
let kind_to_string (kind : def_kind) : string =
  match kind with
  | Function -> "function"
  | Macro -> "macro"
  | Variable -> "variable"
  | Face -> "face"
  | Struct -> "struct"
  | StructAccessor -> "struct-accessor"
  | StructPredicate -> "struct-predicate"
  | StructCopier -> "struct-copier"
  | Class -> "class"
  | ClassAccessor -> "class-accessor"
  | ClassPredicate -> "class-predicate"

(** Count public definitions. *)
let count_public (defs : definition list) : int =
  List.length (List.filter (fun d -> not d.is_private) defs)

(** Count private definitions. *)
let count_private (defs : definition list) : int =
  List.length (List.filter (fun d -> d.is_private) defs)
