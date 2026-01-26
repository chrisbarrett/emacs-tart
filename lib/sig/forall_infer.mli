(** Forall quantifier inference.

    This module infers universal quantifiers from unquantified type signatures.
    When a signature contains lowercase type variables but no explicit [vars]
    quantifier, the variables are collected in left-to-right first-occurrence
    order and quantified automatically.

    Examples:
    - [(defun seq-map (((a -> b)) (seq a)) -> (list b))] infers [a b]
    - [(defun compose (((b -> c)) ((a -> b))) -> ((a -> c)))] infers [b c a]

    When explicit quantifiers are provided, inference is disabled and any
    unbound type variables produce an error during validation.
*)

open Sig_ast

(** {1 Type Variable Collection} *)

(** Collect type variables from a sig_type.
    Returns variables in left-to-right first-occurrence order.
    [bound] contains already bound variables to exclude.
    [known_types] contains user-defined type names to exclude. *)
val collect_sig_type :
  bound:string list ->
  known_types:string list ->
  sig_type ->
  string list

(** Collect type variables from parameters (left-to-right order) *)
val collect_params :
  bound:string list ->
  known_types:string list ->
  sig_param list ->
  string list

(** {1 Inference for Declarations} *)

(** Infer quantifiers for a defun declaration.
    If defun_tvar_binders is empty, collect all type variables from the
    signature and create binders for them.
    Returns the declaration with binders filled in. *)
val infer_defun : known_types:string list -> defun_decl -> defun_decl

(** Infer quantifiers for a type declaration.
    If type_params is empty and there's a body, collect type variables.
    Returns the declaration with params filled in. *)
val infer_type_decl : known_types:string list -> type_decl -> type_decl

(** Infer quantifiers for a single declaration *)
val infer_decl : known_types:string list -> decl -> decl

(** {1 Signature Inference} *)

(** Get all type names defined in a signature *)
val get_type_names : signature -> string list

(** Infer quantifiers for all declarations in a signature file.
    Type names defined in the signature are treated as known types,
    not type variables.
    Returns the signature with inferred quantifiers. *)
val infer_signature : signature -> signature
