(** Kind representation for higher-kinded types.

    Kinds classify types the way types classify values:
    - [*] (KStar) is the kind of concrete types like Int, String, (List Int)
    - [* -> *] is the kind of type constructors like List, Option
    - [* -> * -> *] is the kind of two-parameter constructors like HashTable

    This enables polymorphism over type constructors, allowing functions like:
    {[
      (defun fmap [f a b] (((a -> b)) (f a)) -> (f b))
    ]}
    where [f] has kind [* -> *]. *)

(** {1 Kind Representation} *)

type kind =
  | KStar  (** Concrete type kind *)
  | KArrow of kind * kind  (** Type constructor kind *)
[@@deriving show, eq]

(** {1 Kind Variables} *)

type kvar_id = int
type kvar = KUnbound of kvar_id | KLink of kind
type kind_scheme = KConcrete of kind | KVar of kvar ref

(** {1 Construction} *)

let star = KStar
let ( @-> ) k1 k2 = KArrow (k1, k2)
let rec arity n = if n <= 0 then KStar else KArrow (KStar, arity (n - 1))

(** {1 Kind Variables} *)

let kvar_counter = ref 0
let reset_kvar_counter () = kvar_counter := 0

let fresh_kvar () =
  let id = !kvar_counter in
  incr kvar_counter;
  KVar (ref (KUnbound id))

(** {1 Operations} *)

let repr_scheme ks =
  match ks with
  | KConcrete _ -> ks
  | KVar kv -> (
      match !kv with
      | KLink k ->
          (* Path compression *)
          let result = KConcrete k in
          result
      | KUnbound _ -> ks)

let to_kind ks =
  match repr_scheme ks with KConcrete k -> Some k | KVar _ -> None

let default_to_star ks =
  match repr_scheme ks with
  | KConcrete k -> k
  | KVar kv -> (
      match !kv with
      | KLink k -> k
      | KUnbound _ ->
          kv := KLink KStar;
          KStar)

(** {1 Pretty-printing} *)

let rec to_string = function
  | KStar -> "*"
  | KArrow (k1, k2) ->
      let left =
        match k1 with KArrow _ -> "(" ^ to_string k1 ^ ")" | _ -> to_string k1
      in
      left ^ " -> " ^ to_string k2

let scheme_to_string ks =
  match repr_scheme ks with
  | KConcrete k -> to_string k
  | KVar kv -> (
      match !kv with
      | KUnbound id -> Printf.sprintf "?k%d" id
      | KLink k -> to_string k)

(** {1 Kind Environment}

    A kind environment maps type variable names to their kind schemes. This is
    used during kind inference to track the kinds of type variables.

    When a type variable is looked up but not found, it defaults to kind [*].
    This ensures backward compatibility: existing signatures that don't use
    higher-kinded types continue to work unchanged. *)

type env = (string * kind_scheme) list
(** Kind environment mapping type variable names to kind schemes. *)

(** Empty kind environment. *)
let empty_env : env = []

let extend_env (name : string) (ks : kind_scheme) (env : env) : env =
  (name, ks) :: env

(** Extend environment with a type variable at kind [*]. *)
let extend_star (name : string) (env : env) : env =
  extend_env name (KConcrete KStar) env

(** Extend environment with fresh kind variables for multiple type variables. *)
let extend_fresh (names : string list) (env : env) : env =
  List.fold_left (fun e n -> extend_env n (fresh_kvar ()) e) env names

(** Look up a type variable's kind scheme. Returns a fresh kind variable if not
    found. *)
let lookup (name : string) (env : env) : kind_scheme =
  match List.assoc_opt name env with
  | Some ks -> ks
  | None ->
      (* Default: create a fresh kind variable that will be defaulted to * *)
      fresh_kvar ()

(** Look up a type variable's kind, defaulting unconstrained variables to [*].
    This is the main interface for getting the final kind of a type variable. *)
let lookup_defaulted (name : string) (env : env) : kind =
  let ks = lookup name env in
  default_to_star ks

(** Default all unresolved kind variables in the environment to [*]. Call this
    after kind inference is complete to finalize all kinds. *)
let default_all (env : env) : unit =
  List.iter (fun (_, ks) -> ignore (default_to_star ks)) env

(** Get all names in the environment. *)
let names (env : env) : string list = List.map fst env
