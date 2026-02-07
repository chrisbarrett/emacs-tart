(** Signature-to-core type conversion.

    Converts signature AST types ({!Sig_ast.sig_type}) to core types
    ({!Core.Types.typ}). Handles alias expansion, opaque type resolution,
    intrinsic name canonicalization, and parameterized type substitution. *)

(** {1 Type Alias Context} *)

type alias_param = {
  ap_name : string;  (** Parameter name (e.g., "a") *)
  ap_bound : Sig_ast.sig_type option;  (** Upper bound (e.g., truthy) *)
}
(** A type parameter with optional bound *)

type type_alias = {
  alias_params : alias_param list;  (** Type parameters with optional bounds *)
  alias_body : Sig_ast.sig_type;  (** The definition body *)
}
(** A type alias definition with optional parameters *)

type alias_context = (string * type_alias) list
(** Context for type alias expansion *)

val empty_aliases : alias_context
(** Empty alias context *)

val lookup_alias : string -> alias_context -> type_alias option
(** Look up a type alias *)

val add_alias : string -> type_alias -> alias_context -> alias_context
(** Add a type alias to the context *)

val alias_names : alias_context -> string list
(** Get all alias names from the context *)

val binder_to_alias_param : Sig_ast.tvar_binder -> alias_param
(** Convert a tvar_binder to an alias_param *)

val build_alias_context : Sig_ast.signature -> alias_context
(** Build alias context from signature declarations. Only includes type
    declarations with bodies (aliases, not opaque types). *)

(** {1 Opaque Type Context} *)

type opaque_type = {
  opaque_params : string list;
      (** Phantom type parameters (e.g., [a] in tagged) *)
  opaque_con : string;  (** The generated type constructor name *)
}
(** An opaque type declaration with its parameters *)

type opaque_context = (string * opaque_type) list
(** Context for opaque type lookup *)

val empty_opaques : opaque_context
(** Empty opaque context *)

val lookup_opaque : string -> opaque_context -> opaque_type option
(** Look up an opaque type *)

val opaque_con_name : string -> string -> string
(** Generate a unique type constructor name for an opaque type. Format:
    module_name/type_name *)

val add_opaque : string -> opaque_type -> opaque_context -> opaque_context
(** Add an opaque type to the context *)

val build_opaque_context : string -> Sig_ast.signature -> opaque_context
(** Build opaque context from signature declarations. Only includes type
    declarations without bodies (opaque types). *)

(** {1 Type Context} *)

type type_context = { tc_aliases : alias_context; tc_opaques : opaque_context }
(** Combined context for type resolution during signature loading. Contains both
    aliases and opaque types. *)

val empty_type_context : type_context
(** Empty type context with no aliases or opaques *)

(** {1 Bound Checking} *)

val satisfies_bound : Core.Types.typ -> Core.Types.typ -> bool
(** [satisfies_bound arg_typ bound_typ] checks if [arg_typ] satisfies the bound
    constraint [bound_typ].

    Supports:
    - truthy bound: the argument type must not contain nil
    - union bounds: the argument type must be a member of the union *)

(** {1 Signature AST Substitution} *)

val substitute_sig_type :
  (string * Sig_ast.sig_type) list -> Sig_ast.sig_type -> Sig_ast.sig_type
(** Substitute type variables in a sig_type with other sig_types. Used for
    expanding parameterized type aliases. *)

val substitute_sig_param :
  (string * Sig_ast.sig_type) list -> Sig_ast.sig_param -> Sig_ast.sig_param
(** Substitute type variables in a sig_param. *)

(** {1 Name Canonicalization} *)

val canonicalize_type_name : string -> string
(** Canonicalize a type constructor name. Maps container type names to their
    intrinsic equivalents. *)

val sig_name_to_prim : string -> Core.Types.typ
(** Convert a signature type name to a core type. Applies canonicalization. *)

(** {1 Type Conversion} *)

val sig_type_to_typ_with_ctx :
  ?scope_tvars:(string * Core.Types.typ) list ->
  type_context ->
  string list ->
  Sig_ast.sig_type ->
  Core.Types.typ
(** Convert a signature type to a core type with full type context. *)

val sig_param_to_param_with_ctx :
  ?scope_tvars:(string * Core.Types.typ) list ->
  type_context ->
  string list ->
  Sig_ast.sig_param ->
  Core.Types.param
(** Convert a signature parameter to a core parameter with full type context. *)

val sig_type_to_typ_with_aliases :
  alias_context -> string list -> Sig_ast.sig_type -> Core.Types.typ
(** Convert a signature type to a core type with alias expansion. *)

val sig_param_to_param_with_aliases :
  alias_context -> string list -> Sig_ast.sig_param -> Core.Types.param
(** Convert a signature parameter to a core parameter with alias expansion. *)

val sig_type_to_typ : string list -> Sig_ast.sig_type -> Core.Types.typ
(** Convert a signature type to a core type (without alias expansion). *)

val sig_param_to_param : string list -> Sig_ast.sig_param -> Core.Types.param
(** Convert a signature parameter to a core parameter. *)
