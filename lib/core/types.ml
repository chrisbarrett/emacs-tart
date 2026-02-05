(** Type representation for the Tart type system.

    This module defines the core type representation used for type inference and
    checking. It uses mutable type variables with union-find for efficient
    unification.

    Based on levels-based Hindley-Milner inference with union-find for
    near-linear generalization performance. *)

type tvar_id = int [@@deriving show, eq]
(** Unique identifier for type variables *)

(** Type variable state - uses union-find representation.

    [Unbound (id, level)] is an unresolved type variable. The level tracks the
    scope depth for let-generalization: variables with level > current scope
    level can be generalized.

    [Link ty] indicates this variable has been unified with [ty]. *)
type tvar = Unbound of tvar_id * int  (** id, level *) | Link of typ

(** Type representation.

    - [TVar] - Mutable type variable for union-find unification
    - [TCon] - Type constant (Int, String, Nil, T, etc.)
    - [TApp] - Type application (List a, Option a, etc.). The first element is
      the type constructor (TCon for concrete types, TVar for higher-kinded)
    - [TArrow] - Function type with grouped parameters
    - [TForall] - Universally quantified type
    - [TUnion] - Union types (Or a b)
    - [TTuple] - Fixed-length heterogeneous tuples
    - [TRow] - Row type for record-style map types (alist, plist, hash-table) *)
and typ =
  | TVar of tvar ref
  | TCon of string
  | TApp of typ * typ list
  | TArrow of param list * typ
  | TForall of string list * typ
  | TUnion of typ list
  | TTuple of typ list
  | TRow of row

and row = {
  row_fields : (string * typ) list;  (** Named fields in declaration order *)
  row_var : typ option;  (** Optional row variable (TVar) for open rows *)
}
(** Row type representation for record-style typing of
    alists/plists/hash-tables.

    A row is a collection of field name-type pairs, optionally with a row
    variable for polymorphism (open rows).

    - [{name string age int}] - closed row, exactly these fields
    - [{name string & r}] - open row, at least name field, r captures the rest
*)

(** Function parameter kinds.

    Elisp functions can have positional, optional, rest, and keyword arguments.
*)
and param =
  | PPositional of typ
  | POptional of typ  (** Type should be (Option a) *)
  | PRest of typ  (** Type should be the element type; desugars to (List a) *)
  | PKey of string * typ  (** :keyword name and type *)

(** Global counter for fresh type variable IDs *)
let tvar_counter = ref 0

(** Reset the type variable counter (for testing) *)
let reset_tvar_counter () = tvar_counter := 0

(** Create a fresh unbound type variable at the given level *)
let fresh_tvar level =
  let id = !tvar_counter in
  incr tvar_counter;
  TVar (ref (Unbound (id, level)))

(** Get the ID of an unbound type variable, or None if linked *)
let tvar_id tv = match !tv with Unbound (id, _) -> Some id | Link _ -> None

(** Get the level of an unbound type variable, or None if linked *)
let tvar_level tv =
  match !tv with Unbound (_, level) -> Some level | Link _ -> None

(** Follow links to find the representative type.

    Path compression: updates intermediate links to point directly to the
    representative for amortized near-constant lookup. *)
let rec repr ty =
  match ty with
  | TVar tv -> (
      match !tv with
      | Link ty' ->
          let result = repr ty' in
          (* Path compression *)
          tv := Link result;
          result
      | Unbound _ -> ty)
  | _ -> ty

(** Check if a type is a type variable (after following links) *)
let is_tvar ty = match repr ty with TVar _ -> true | _ -> false

(** Intrinsic prefix for built-in types.

    All built-in types use this prefix to distinguish them from user-defined
    types. The prelude (tart-prelude.tart) bridges these intrinsic names to
    user-friendly names like `int`, `string`, etc. *)
let intrinsic_prefix = "%tart-intrinsic%"

(** Create an intrinsic type name *)
let intrinsic name = intrinsic_prefix ^ name

(** Check if a type name is an intrinsic *)
let is_intrinsic_name name =
  String.length name > String.length intrinsic_prefix
  && String.sub name 0 (String.length intrinsic_prefix) = intrinsic_prefix

(** Get the base name from an intrinsic (e.g., "%tart-intrinsic%Int" -> "Int")
*)
let intrinsic_base_name name =
  if is_intrinsic_name name then
    String.sub name
      (String.length intrinsic_prefix)
      (String.length name - String.length intrinsic_prefix)
  else name

(** Convert intrinsic base name to user-friendly display name. E.g., "HashTable"
    -> "hash-table", "Int" -> "int" *)
let intrinsic_display_name base_name =
  match base_name with
  | "HashTable" -> "hash-table"
  | other -> String.lowercase_ascii other

(** Primitive type constants.

    These use the %tart-intrinsic% prefix to distinguish them from user types.
    The prelude bridges these to user-friendly names. *)
module Prim = struct
  (** Intrinsic type names for comparison *)
  let int_name = intrinsic "Int"

  let float_name = intrinsic "Float"
  let num_name = intrinsic "Num"

  (** Type constructors *)
  let int = TCon int_name

  let float = TCon float_name
  let num = TCon num_name
  let string = TCon (intrinsic "String")
  let symbol = TCon (intrinsic "Symbol")
  let keyword = TCon (intrinsic "Keyword")
  let nil = TCon (intrinsic "Nil")
  let t = TCon (intrinsic "T")
  let truthy = TCon (intrinsic "Truthy")
  let bool = TUnion [ t; nil ]
  let any = TUnion [ truthy; nil ]
  let never = TCon (intrinsic "Never")
end

(** Construct common types *)
let list_of elem = TApp (TCon (intrinsic "List"), [ elem ])

let vector_of elem = TApp (TCon (intrinsic "Vector"), [ elem ])
let option_of elem = TUnion [ elem; Prim.nil ]
let pair_of a b = TApp (TCon (intrinsic "Pair"), [ a; b ])
let hash_table_of k v = TApp (TCon (intrinsic "HashTable"), [ k; v ])

(** Check if a type is the "any" type (truthy | nil). Used in unification to
    maintain top-type semantics. *)
let is_any ty =
  let truthy_name = intrinsic "Truthy" in
  let nil_name = intrinsic "Nil" in
  match repr ty with
  | TUnion [ t1; t2 ] -> (
      match (repr t1, repr t2) with
      | TCon n1, TCon n2
        when (n1 = truthy_name && n2 = nil_name)
             || (n1 = nil_name && n2 = truthy_name) ->
          true
      | _ -> false)
  | _ -> false

(** Check if a type is an option type (a | nil) and return the inner type. Used
    in diagnostics to detect nullable types. *)
let is_option ty =
  let nil_name = intrinsic "Nil" in
  match repr ty with
  | TUnion [ t1; t2 ] -> (
      match (repr t1, repr t2) with
      | _, TCon n when n = nil_name -> Some t1
      | TCon n, _ when n = nil_name -> Some t2
      | _ -> None)
  | _ -> None

(** Check if a type is a union type *)
let is_union ty = match repr ty with TUnion _ -> true | _ -> false

(** Create a function type with positional parameters *)
let arrow params ret = TArrow (List.map (fun p -> PPositional p) params, ret)

(** Create a simple polymorphic type *)
let forall vars ty = TForall (vars, ty)

(** Create a closed row type (no row variable) *)
let closed_row fields = TRow { row_fields = fields; row_var = None }

(** Create an open row type with a row variable for polymorphism *)
let open_row fields var = TRow { row_fields = fields; row_var = Some var }

(** Look up a field in a row, returning its type if present *)
let row_lookup row field = List.assoc_opt field row.row_fields

(** Check if a row has a given field *)
let row_has_field row field = List.mem_assoc field row.row_fields

(** Check if a type is a row type *)
let is_row ty = match repr ty with TRow _ -> true | _ -> false

(** Check if a type is an open row (has a row variable) *)
let is_open_row ty =
  match repr ty with TRow { row_var = Some _; _ } -> true | _ -> false

(** Pretty-print a type to string.

    For user-facing output, intrinsic types are shown with their base name
    (lowercase) rather than the internal %tart-intrinsic% prefix. *)
let rec to_string ty =
  match repr ty with
  | TVar tv -> (
      match !tv with
      | Unbound (id, _) -> Printf.sprintf "'_%d" id
      | Link _ -> failwith "repr should have followed link")
  | TCon name ->
      (* Show user-friendly names for intrinsics *)
      if is_intrinsic_name name then
        intrinsic_display_name (intrinsic_base_name name)
      else name
  | TApp (con, args) ->
      Printf.sprintf "(%s %s)" (to_string con)
        (String.concat " " (List.map to_string args))
  | TArrow (params, ret) ->
      Printf.sprintf "(-> (%s) %s)"
        (String.concat " " (List.map param_to_string params))
        (to_string ret)
  | TForall (vars, body) ->
      Printf.sprintf "(forall (%s) %s)" (String.concat " " vars)
        (to_string body)
  | TUnion types ->
      Printf.sprintf "(Or %s)" (String.concat " " (List.map to_string types))
  | TTuple types ->
      Printf.sprintf "(Tuple %s)" (String.concat " " (List.map to_string types))
  | TRow { row_fields; row_var } -> (
      let fields_str =
        String.concat " "
          (List.map
             (fun (name, ty) -> Printf.sprintf "%s %s" name (to_string ty))
             row_fields)
      in
      match row_var with
      | None -> Printf.sprintf "{%s}" fields_str
      | Some var -> Printf.sprintf "{%s & %s}" fields_str (to_string var))

and param_to_string = function
  | PPositional ty -> to_string ty
  | POptional ty -> Printf.sprintf "&optional %s" (to_string ty)
  | PRest ty -> Printf.sprintf "&rest %s" (to_string ty)
  | PKey (name, ty) -> Printf.sprintf "&key %s %s" name (to_string ty)

(** Check if two types are syntactically equal (after following links). This
    does NOT unify - it only checks structural equality. Type variables are
    equal only if they have the same identity. *)
let rec equal t1 t2 =
  match (repr t1, repr t2) with
  | TVar tv1, TVar tv2 -> tv1 == tv2 (* Physical equality *)
  | TCon n1, TCon n2 -> n1 = n2
  | TApp (c1, args1), TApp (c2, args2) ->
      equal c1 c2
      && List.length args1 = List.length args2
      && List.for_all2 equal args1 args2
  | TArrow (ps1, r1), TArrow (ps2, r2) ->
      List.length ps1 = List.length ps2
      && List.for_all2 param_equal ps1 ps2
      && equal r1 r2
  | TForall (vs1, b1), TForall (vs2, b2) ->
      (* Simple check: same variable names and equal body *)
      vs1 = vs2 && equal b1 b2
  | TUnion ts1, TUnion ts2 ->
      List.length ts1 = List.length ts2 && List.for_all2 equal ts1 ts2
  | TTuple ts1, TTuple ts2 ->
      List.length ts1 = List.length ts2 && List.for_all2 equal ts1 ts2
  | TRow r1, TRow r2 -> row_equal r1 r2
  | _ -> false

and row_equal r1 r2 =
  (* Fields must match exactly in order and types *)
  List.length r1.row_fields = List.length r2.row_fields
  && List.for_all2
       (fun (n1, t1) (n2, t2) -> n1 = n2 && equal t1 t2)
       r1.row_fields r2.row_fields
  &&
  match (r1.row_var, r2.row_var) with
  | None, None -> true
  | Some v1, Some v2 -> equal v1 v2
  | _ -> false

and param_equal p1 p2 =
  match (p1, p2) with
  | PPositional t1, PPositional t2 -> equal t1 t2
  | POptional t1, POptional t2 -> equal t1 t2
  | PRest t1, PRest t2 -> equal t1 t2
  | PKey (n1, t1), PKey (n2, t2) -> n1 = n2 && equal t1 t2
  | _ -> false

(** Truthiness checking for the Option type constraint.

    In Elisp, truthiness is fundamental: nil is the only falsy value. The type
    hierarchy is:
    - Nil: the only falsy type
    - Truthy: anything that is not Nil (primitive supertype)
    - Prim.any = Truthy | Nil (top type, defined as union)
    - Prim.bool = T | Nil (defined as union)
    - option_of a = a | Nil (defined as union)

    For option types, we require the inner type to be Truthy. This ensures
    "some" and "none" cases are always distinguishable. Without this constraint,
    option_of Nil would be indistinguishable from Nil itself, breaking pattern
    matching semantics. *)

(** Errors that can occur during type construction/validation *)
type validation_error =
  | NonTruthyOptionArg of typ
      (** Option argument must be truthy - contains the offending type *)

(** Check if a type is definitely truthy (cannot be nil).

    A type is truthy if:
    - It's a concrete non-nil primitive (Int, String, T, Symbol, etc.)
    - It's the Truthy primitive type itself
    - It's a type application other than Option (List, Vector, Pair, etc.)
    - It's a function type (functions are always truthy)
    - It's a tuple type (tuples are always truthy)
    - It's a forall type where the body is truthy

    A type is NOT truthy if:
    - It's Nil
    - It's a union containing Nil (includes Prim.bool, Prim.any, option_of)
    - It's an unresolved type variable (conservatively false) *)
let rec is_truthy ty =
  match repr ty with
  | TVar _ ->
      (* Unresolved type variables: we don't know yet, so conservatively false.
         During unification, this will be resolved. *)
      false
  | TCon name ->
      (* Check if it's the Nil intrinsic - only falsy type *)
      if name = intrinsic "Nil" then false
        (* Check if it's a known truthy intrinsic *)
      else if is_intrinsic_name name then
        let base = intrinsic_base_name name in
        match base with
        | "Int" | "Float" | "Num" | "String" | "Symbol" | "Keyword" | "T"
        | "Truthy" | "Never" | "List" | "Vector" | "Pair" | "HashTable" ->
            true
        | _ -> true (* Unknown intrinsics: assume truthy *)
      else
        (* User-defined types: conservatively assume truthy *)
        true
  | TApp (con, _args) -> (
      match repr con with
      | TCon name when is_intrinsic_name name -> (
          (* Intrinsic container types are truthy *)
          let base = intrinsic_base_name name in
          match base with
          | "List" | "Vector" | "Pair" | "HashTable" -> true
          | _ -> true
          (* Unknown intrinsic apps: assume truthy *))
      | TCon _ ->
          (* Unknown type constructors: assume truthy *)
          true
      | TVar _ ->
          (* HK type variable applied to args - conservatively false *)
          false
      | _ ->
          (* Other constructor types: assume truthy *)
          true)
  | TArrow _ ->
      (* Functions are always truthy *)
      true
  | TForall (_, body) ->
      (* A forall type is truthy if its body is truthy *)
      is_truthy body
  | TUnion types ->
      (* A union is truthy only if ALL members are truthy *)
      List.for_all is_truthy types
  | TTuple _ ->
      (* Tuples (vectors) are always truthy *)
      true
  | TRow _ ->
      (* Rows are always truthy - they represent non-nil map structures *)
      true

(** Validate that a type is suitable as an Option argument.

    Returns Ok () if the type is truthy, or Error with the problematic type. *)
let validate_option_arg ty =
  if is_truthy ty then Ok () else Error (NonTruthyOptionArg ty)

(** Create an Option type with validation.

    Returns Error if the argument type is not truthy. *)
let option_of_checked elem =
  match validate_option_arg elem with
  | Ok () -> Ok (option_of elem)
  | Error e -> Error e

(** Format a validation error as a string *)
let validation_error_to_string = function
  | NonTruthyOptionArg ty ->
      Printf.sprintf "Option argument must be truthy, but got: %s"
        (to_string ty)
