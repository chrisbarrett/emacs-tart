(** Type representation for the Tart type system.

    This module defines the core type representation used for type inference
    and checking. It uses mutable type variables with union-find for efficient
    unification.

    Based on levels-based Hindley-Milner inference with union-find for
    near-linear generalization performance.
*)

(** Unique identifier for type variables *)
type tvar_id = int [@@deriving show, eq]

(** Type variable state - uses union-find representation.

    [Unbound (id, level)] is an unresolved type variable. The level tracks
    the scope depth for let-generalization: variables with level > current
    scope level can be generalized.

    [Link ty] indicates this variable has been unified with [ty].
*)
type tvar =
  | Unbound of tvar_id * int  (** id, level *)
  | Link of typ

(** Type representation.

    - [TVar] - Mutable type variable for union-find unification
    - [TCon] - Type constant (Int, String, Nil, T, etc.)
    - [TApp] - Type application (List a, Option a, etc.)
    - [TArrow] - Function type with grouped parameters
    - [TForall] - Universally quantified type
    - [TUnion] - Union types (Or a b)
    - [TTuple] - Fixed-length heterogeneous tuples
*)
and typ =
  | TVar of tvar ref
  | TCon of string
  | TApp of string * typ list
  | TArrow of param list * typ
  | TForall of string list * typ
  | TUnion of typ list
  | TTuple of typ list

(** Function parameter kinds.

    Elisp functions can have positional, optional, rest, and keyword arguments.
*)
and param =
  | PPositional of typ
  | POptional of typ  (** Type should be (Option a) *)
  | PRest of typ      (** Type should be the element type; desugars to (List a) *)
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
let tvar_id tv =
  match !tv with
  | Unbound (id, _) -> Some id
  | Link _ -> None

(** Get the level of an unbound type variable, or None if linked *)
let tvar_level tv =
  match !tv with
  | Unbound (_, level) -> Some level
  | Link _ -> None

(** Follow links to find the representative type.

    Path compression: updates intermediate links to point directly to the
    representative for amortized near-constant lookup.
*)
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
let is_tvar ty =
  match repr ty with
  | TVar _ -> true
  | _ -> false

(** Primitive type constants *)
module Prim = struct
  let int = TCon "Int"
  let float = TCon "Float"
  let num = TCon "Num"
  let string = TCon "String"
  let symbol = TCon "Symbol"
  let keyword = TCon "Keyword"
  let nil = TCon "Nil"
  let t = TCon "T"
  let truthy = TCon "Truthy"
  let bool = TCon "Bool"
  let any = TCon "Any"
  let never = TCon "Never"
end

(** Construct common types *)
let list_of elem = TApp ("List", [ elem ])
let vector_of elem = TApp ("Vector", [ elem ])
let option_of elem = TApp ("Option", [ elem ])
let pair_of a b = TApp ("Pair", [ a; b ])
let hash_table_of k v = TApp ("HashTable", [ k; v ])

(** Create a function type with positional parameters *)
let arrow params ret = TArrow (List.map (fun p -> PPositional p) params, ret)

(** Create a simple polymorphic type *)
let forall vars ty = TForall (vars, ty)

(** Pretty-print a type to string *)
let rec to_string ty =
  match repr ty with
  | TVar tv -> (
      match !tv with
      | Unbound (id, _) -> Printf.sprintf "'_%d" id
      | Link _ -> failwith "repr should have followed link")
  | TCon name -> name
  | TApp (con, args) ->
      Printf.sprintf "(%s %s)" con (String.concat " " (List.map to_string args))
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

and param_to_string = function
  | PPositional ty -> to_string ty
  | POptional ty -> Printf.sprintf "&optional %s" (to_string ty)
  | PRest ty -> Printf.sprintf "&rest %s" (to_string ty)
  | PKey (name, ty) -> Printf.sprintf "&key %s %s" name (to_string ty)

(** Check if two types are syntactically equal (after following links).
    This does NOT unify - it only checks structural equality.
    Type variables are equal only if they have the same identity.
*)
let rec equal t1 t2 =
  match (repr t1, repr t2) with
  | TVar tv1, TVar tv2 -> tv1 == tv2  (* Physical equality *)
  | TCon n1, TCon n2 -> n1 = n2
  | TApp (c1, args1), TApp (c2, args2) ->
      c1 = c2 && List.length args1 = List.length args2
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
  | _ -> false

and param_equal p1 p2 =
  match (p1, p2) with
  | PPositional t1, PPositional t2 -> equal t1 t2
  | POptional t1, POptional t2 -> equal t1 t2
  | PRest t1, PRest t2 -> equal t1 t2
  | PKey (n1, t1), PKey (n2, t2) -> n1 = n2 && equal t1 t2
  | _ -> false
