(** Kind inference algorithm.

    This module infers kinds for type variables from their usage patterns. When
    a type variable is applied to arguments (e.g., [f a] in a signature), we
    infer that it must have kind [* -> *].

    The algorithm follows the same pattern as type inference: 1. Assign fresh
    kind variables to type parameters 2. Collect kind constraints from type
    expressions 3. Unify kind constraints 4. Default unconstrained kind
    variables to [*]

    Examples:
    - [(defun identity [a] (a) -> a)] - [a : *] (used only in type positions)
    - [(defun fmap [f a b] (((a -> b)) (f a)) -> (f b))] - [f : * -> *],
      [a : *], [b : *]
    - [(defun bimap [f a b c d] (((a -> b)) ((c -> d)) (f a c)) -> (f b d))] -
      [f : * -> * -> *] *)

(** {1 Kind Inference Errors} *)

type kind_error =
  | KindMismatch of {
      expected : Kind.kind;
      found : Kind.kind;
      location : string;
    }  (** Two kinds failed to unify *)
  | OccursCheckFailed of { kvar_id : Kind.kvar_id; kind : Kind.kind }
      (** A kind variable occurs in the kind it's being unified with *)
  | ArityMismatch of { type_con : string; expected : int; found : int }
      (** Type constructor applied to wrong number of arguments *)

val kind_error_to_string : kind_error -> string
(** Pretty-print a kind error. *)

(** {1 Kind Inference for Signature Types} *)

val infer_sig_type_kind :
  Kind.env -> Sig.Sig_ast.sig_type -> (Kind.kind_scheme, kind_error) result
(** Infer the kind of a signature type expression.

    @param env Kind environment mapping type variable names to kind schemes
    @param ty The signature type to infer the kind of
    @return The kind scheme of the type, or an error *)

(** {1 High-Level Interface} *)

type infer_result = {
  kind_env : Kind.env;  (** Final kind environment with inferred kinds *)
  errors : kind_error list;  (** Any errors encountered *)
}
(** Result of kind inference for a declaration. *)

val infer_defun_kinds : Sig.Sig_ast.defun_decl -> infer_result
(** Infer kinds for a defun declaration's type parameters.

    Example: For [(defun fmap [f a b] (((a -> b)) (f a)) -> (f b))], infers
    [f : * -> *], [a : *], [b : *].

    @param d The defun declaration to analyze
    @return The inferred kind environment and any errors *)

val infer_type_decl_kinds : Sig.Sig_ast.type_decl -> infer_result
(** Infer kinds for a type declaration's type parameters.

    @param d The type declaration to analyze
    @return The inferred kind environment and any errors *)

val infer_data_kinds : Sig.Sig_ast.data_decl -> infer_result
(** Infer kinds for a data declaration's type parameters.

    @param d The data declaration to analyze
    @return The inferred kind environment and any errors *)

(** {1 Inference with Scope Context}

    Functions to infer kinds for declarations within a type-scope, where the
    scope provides additional type variables with potentially explicit kind
    annotations. *)

val infer_defun_kinds_with_scope :
  Kind.env -> Sig.Sig_ast.defun_decl -> infer_result
(** Infer kinds for a defun declaration within a scope context.

    The scope_env contains kind bindings for type variables from enclosing
    type-scope blocks. These are combined with the defun's own type parameters.

    @param scope_env Kind environment from enclosing type-scope
    @param d The defun declaration to analyze
    @return The inferred kind environment and any errors *)

val infer_type_decl_kinds_with_scope :
  Kind.env -> Sig.Sig_ast.type_decl -> infer_result
(** Infer kinds for a type declaration within a scope context.

    @param scope_env Kind environment from enclosing type-scope
    @param d The type declaration to analyze
    @return The inferred kind environment and any errors *)

val infer_data_kinds_with_scope :
  Kind.env -> Sig.Sig_ast.data_decl -> infer_result
(** Infer kinds for a data declaration within a scope context.

    @param scope_env Kind environment from enclosing type-scope
    @param d The data declaration to analyze
    @return The inferred kind environment and any errors *)

(** {1 Kind Lookup} *)

val lookup_kind : infer_result -> string -> Kind.kind
(** Look up the kind of a type parameter after inference.

    @param result The inference result
    @param name The type parameter name
    @return The inferred kind *)
