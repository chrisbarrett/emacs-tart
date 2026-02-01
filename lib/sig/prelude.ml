(** Prelude loading and management.

    The prelude is a set of implicit utility types that are automatically
    available in all .tart files without explicit import. The prelude types
    bridge lowercase names to built-in uppercase type constructors.

    Prelude types:
    - t: The symbol 't (Elisp's canonical truthy value)
    - any: Union type (truthy | nil)
    - bool: Union type (t | nil)
    - list: Alias for built-in List (parameterized)
    - option: Union type (a | nil) with truthy bound on a
    - is: Type subtraction (a - nil) - removes nil from a type
    - nonempty: Non-empty list (is (list a)) - list without nil

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

(** Make a type subtraction *)
let tsubtract minuend subtrahend =
  Sig_ast.STSubtract (minuend, subtrahend, prelude_span)

(** Create an alias parameter without bound *)
let param name : Sig_loader.alias_param =
  { Sig_loader.ap_name = name; ap_bound = None }

(** Create an alias parameter with truthy bound *)
let param_truthy name : Sig_loader.alias_param =
  { Sig_loader.ap_name = name; ap_bound = Some (tcon "Truthy") }

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
    (* (type any (truthy | nil)) - top type as a union *)
    ( "any",
      {
        Sig_loader.alias_params = [];
        alias_body =
          Sig_ast.STUnion ([ tcon "Truthy"; tcon "Nil" ], prelude_span);
      } );
    (* (type bool (t | nil)) - boolean as a union, not a built-in *)
    ( "bool",
      {
        Sig_loader.alias_params = [];
        alias_body = Sig_ast.STUnion ([ tcon "T"; tcon "Nil" ], prelude_span);
      } );
    (* (type list [a] (List a)) - maps lowercase list to built-in List *)
    ( "list",
      {
        Sig_loader.alias_params = [ param "a" ];
        alias_body = tapp "List" [ tvar "a" ];
      } );
    (* (type option [(a : truthy)] (a | nil)) - optional as union, truthy bound
       prevents nested option: (option (option x)) is an error since
       (option x) = (x | nil) is not <: truthy. *)
    ( "option",
      {
        Sig_loader.alias_params = [ param_truthy "a" ];
        alias_body = Sig_ast.STUnion ([ tvar "a"; tcon "Nil" ], prelude_span);
      } );
    (* (type is [a] (a - nil)) - removes nil from a type via subtraction *)
    ( "is",
      {
        Sig_loader.alias_params = [ param "a" ];
        alias_body = tsubtract (tvar "a") (tcon "Nil");
      } );
    (* (type nonempty [a] (is (list a))) - non-empty list, equivalent to
       ((list a) - nil) which yields (cons a (list a)) *)
    ( "nonempty",
      {
        Sig_loader.alias_params = [ param "a" ];
        alias_body = tsubtract (tapp "List" [ tvar "a" ]) (tcon "Nil");
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
