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
  {
    Sig_loader.ap_name = name;
    ap_bound = Some (tcon (Core.Types.intrinsic "Truthy"));
  }

(** Helper to create intrinsic type name *)
let intrinsic name = Core.Types.intrinsic name

(** The prelude type alias definitions.

    These aliases bridge user-facing lowercase names to intrinsic type
    constructors. The type system uses intrinsic names internally
    (%tart-intrinsic%Int, etc.) but signature files use lowercase names (int,
    string, etc.).

    Note: We define these as Sig_loader.type_alias values rather than sig_ast to
    integrate directly with the loader's alias context. *)
let prelude_aliases : (string * Sig_loader.type_alias) list =
  [
    (* Primitive type bridges from user names to intrinsics *)
    ( "int",
      { Sig_loader.alias_params = []; alias_body = tcon (intrinsic "Int") } );
    ( "float",
      { Sig_loader.alias_params = []; alias_body = tcon (intrinsic "Float") } );
    ( "num",
      { Sig_loader.alias_params = []; alias_body = tcon (intrinsic "Num") } );
    ( "string",
      { Sig_loader.alias_params = []; alias_body = tcon (intrinsic "String") }
    );
    ( "symbol",
      { Sig_loader.alias_params = []; alias_body = tcon (intrinsic "Symbol") }
    );
    ( "keyword",
      { Sig_loader.alias_params = []; alias_body = tcon (intrinsic "Keyword") }
    );
    ( "nil",
      { Sig_loader.alias_params = []; alias_body = tcon (intrinsic "Nil") } );
    ( "truthy",
      { Sig_loader.alias_params = []; alias_body = tcon (intrinsic "Truthy") }
    );
    ( "never",
      { Sig_loader.alias_params = []; alias_body = tcon (intrinsic "Never") } );
    (* (type t 't) - maps to symbol literal 't (which becomes intrinsic T) *)
    ("t", { Sig_loader.alias_params = []; alias_body = tcon (intrinsic "T") });
    (* (type any (truthy | nil)) - top type as a union *)
    ( "any",
      {
        Sig_loader.alias_params = [];
        alias_body =
          Sig_ast.STUnion
            ([ tcon (intrinsic "Truthy"); tcon (intrinsic "Nil") ], prelude_span);
      } );
    (* (type bool (t | nil)) - boolean as a union *)
    ( "bool",
      {
        Sig_loader.alias_params = [];
        alias_body =
          Sig_ast.STUnion
            ([ tcon (intrinsic "T"); tcon (intrinsic "Nil") ], prelude_span);
      } );
    (* (type list [a] (List a)) - maps lowercase list to intrinsic List *)
    ( "list",
      {
        Sig_loader.alias_params = [ param "a" ];
        alias_body = tapp (intrinsic "List") [ tvar "a" ];
      } );
    (* (type vector [a] (Vector a)) - maps vector to intrinsic Vector *)
    ( "vector",
      {
        Sig_loader.alias_params = [ param "a" ];
        alias_body = tapp (intrinsic "Vector") [ tvar "a" ];
      } );
    (* (type pair [a b] (Pair a b)) - maps pair/cons to intrinsic Pair *)
    ( "pair",
      {
        Sig_loader.alias_params = [ param "a"; param "b" ];
        alias_body = tapp (intrinsic "Pair") [ tvar "a"; tvar "b" ];
      } );
    (* Also add "cons" as alias for pair - elisp uses cons cells *)
    ( "cons",
      {
        Sig_loader.alias_params = [ param "a"; param "b" ];
        alias_body = tapp (intrinsic "Pair") [ tvar "a"; tvar "b" ];
      } );
    (* (type hash-table [k v] (HashTable k v)) - maps to intrinsic HashTable *)
    ( "hash-table",
      {
        Sig_loader.alias_params = [ param "k"; param "v" ];
        alias_body = tapp (intrinsic "HashTable") [ tvar "k"; tvar "v" ];
      } );
    (* (type option [(a : truthy)] (a | nil)) - optional as union, truthy bound
       prevents nested option: (option (option x)) is an error since
       (option x) = (x | nil) is not <: truthy. *)
    ( "option",
      {
        Sig_loader.alias_params = [ param_truthy "a" ];
        alias_body =
          Sig_ast.STUnion ([ tvar "a"; tcon (intrinsic "Nil") ], prelude_span);
      } );
    (* (type is [a] (a - nil)) - removes nil from a type via subtraction *)
    ( "is",
      {
        Sig_loader.alias_params = [ param "a" ];
        alias_body = tsubtract (tvar "a") (tcon (intrinsic "Nil"));
      } );
    (* (type nonempty [a] (is (list a))) - non-empty list, equivalent to
       ((list a) - nil) which yields (cons a (list a)) *)
    ( "nonempty",
      {
        Sig_loader.alias_params = [ param "a" ];
        alias_body =
          tsubtract
            (tapp (intrinsic "List") [ tvar "a" ])
            (tcon (intrinsic "Nil"));
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
