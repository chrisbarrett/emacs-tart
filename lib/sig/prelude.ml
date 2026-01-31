(** Prelude loading and management.

    The prelude is a set of implicit utility types that are automatically
    available in all .tart files without explicit import. The prelude types
    bridge lowercase names to built-in uppercase type constructors.

    Prelude types:
    - t: The symbol 't (Elisp's canonical truthy value)
    - any: Alias for built-in Any (truthy | nil)
    - bool: Alias for built-in Bool (t | nil)
    - list: Alias for built-in List (parameterized)
    - option: Alias for built-in Option (parameterized, with truthy bound)

    The prelude is loaded before any other .tart file and its bindings cannot be
    shadowed (per Spec 07 R17). *)

module Loc = Syntax.Location

(** {1 Prelude Types}

    The prelude type aliases, defined inline for bootstrap loading. These
    correspond to typings/tart-prelude.tart but are available without file I/O
    for robustness. *)

(** Make a dummy span for prelude types *)
let prelude_span =
  {
    Loc.start_pos = { file = "<prelude>"; line = 0; col = 0; offset = 0 };
    end_pos = { file = "<prelude>"; line = 0; col = 0; offset = 0 };
  }

(** Make a sig_type from a name *)
let tcon name = Sig_ast.STCon (name, prelude_span)

(** Make a type variable reference *)
let tvar name = Sig_ast.STVar (name, prelude_span)

(** Make a type application *)
let tapp name args = Sig_ast.STApp (name, args, prelude_span)

(** Make a type binder (unused but available for future prelude extensions) *)
let _binder ?bound name =
  { Sig_ast.name; bound; kind = None; loc = prelude_span }

(** The prelude type alias definitions.

    These aliases bridge user-facing lowercase names to built-in uppercase type
    constructors. The type system uses uppercase names internally (List, Option,
    etc.) but signature files use lowercase names (list, option).

    Note: We define these as Sig_loader.type_alias values rather than sig_ast to
    integrate directly with the loader's alias context. *)
let prelude_aliases : (string * Sig_loader.type_alias) list =
  [
    (* (type t 't) - maps to symbol literal 't *)
    ("t", { Sig_loader.alias_params = []; alias_body = tcon "'t" });
    (* (type any Any) - maps lowercase any to built-in Any *)
    ("any", { Sig_loader.alias_params = []; alias_body = tcon "Any" });
    (* (type bool Bool) - maps lowercase bool to built-in Bool *)
    ("bool", { Sig_loader.alias_params = []; alias_body = tcon "Bool" });
    (* (type list [a] (List a)) - maps lowercase list to built-in List *)
    ( "list",
      {
        Sig_loader.alias_params = [ "a" ];
        alias_body = tapp "List" [ tvar "a" ];
      } );
    (* (type option [a] (Option a)) - maps lowercase option to built-in Option *)
    ( "option",
      {
        Sig_loader.alias_params = [ "a" ];
        alias_body = tapp "Option" [ tvar "a" ];
      } );
  ]

(** Names of prelude types (for shadowing checks) *)
let prelude_type_names : string list = List.map fst prelude_aliases

(** {1 Prelude Context}

    Build a type context containing prelude aliases for use during signature
    loading. *)

(** Build an alias context containing prelude type aliases *)
let prelude_alias_context () : Sig_loader.alias_context =
  List.fold_left
    (fun ctx (name, alias) -> Sig_loader.add_alias name alias ctx)
    Sig_loader.empty_aliases prelude_aliases

(** Build a type context containing prelude types *)
let prelude_type_context () : Sig_loader.type_context =
  {
    Sig_loader.tc_aliases = prelude_alias_context ();
    tc_opaques = Sig_loader.empty_opaques;
  }

(** {1 Shadowing Checks} *)

(** Check if a name is a prelude type *)
let is_prelude_type (name : string) : bool = List.mem name prelude_type_names
