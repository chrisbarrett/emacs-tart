(** Type signatures for built-in Elisp functions.

    This module defines the types for pure built-in functions that are available
    in the interpreter. These types are loaded into the initial type environment
    used for type checking.

    The signatures follow the type system defined in Spec 03:
    - Uses Option for nullable returns (e.g., car on a list)
    - Uses polymorphic types where appropriate (e.g., car, cons)
    - Numeric operations use Int for simplicity (could extend to Num later) *)

open Core.Types
module Env = Core.Type_env

(** Helper to create a polymorphic type variable reference. This creates a named
    type variable for use in forall types. *)
let tvar name = TCon name

(** Helper to create a list type *)
let list_t elem = TApp (TCon "List", [ elem ])

(** Helper to create a vector type *)
let vector_t elem = TApp (TCon "Vector", [ elem ])

(** Helper to create an option type *)
let option_t elem = TApp (TCon "Option", [ elem ])

(** Helper for a simple function type with positional args *)
let fn params ret = TArrow (List.map (fun t -> PPositional t) params, ret)

(** Helper for a function with rest args *)
let fn_rest elem_ty ret = TArrow ([ PRest elem_ty ], ret)

(** Create a polymorphic scheme *)
let poly vars ty = Env.Poly (vars, ty)

(** Create a monomorphic scheme *)
let mono ty = Env.Mono ty

(** All built-in function type signatures *)
let signatures : (string * Env.scheme) list =
  [
    (* =========================================================================
     List operations
     ========================================================================= *)

    (* car : (forall (a) (-> ((List a)) (Option a))) *)
    ("car", poly [ "a" ] (fn [ list_t (tvar "a") ] (option_t (tvar "a"))));
    (* cdr : (forall (a) (-> ((List a)) (List a))) *)
    ("cdr", poly [ "a" ] (fn [ list_t (tvar "a") ] (list_t (tvar "a"))));
    (* cons : (forall (a) (-> (a (List a)) (List a))) *)
    ( "cons",
      poly [ "a" ] (fn [ tvar "a"; list_t (tvar "a") ] (list_t (tvar "a"))) );
    (* list : (forall (a) (-> (&rest a) (List a))) *)
    ("list", poly [ "a" ] (fn_rest (tvar "a") (list_t (tvar "a"))));
    (* length : (forall (a) (-> ((List a)) Int)) *)
    ("length", poly [ "a" ] (fn [ list_t (tvar "a") ] Prim.int));
    (* nth : (forall (a) (-> (Int (List a)) (Option a))) *)
    ( "nth",
      poly [ "a" ] (fn [ Prim.int; list_t (tvar "a") ] (option_t (tvar "a"))) );
    (* nthcdr : (forall (a) (-> (Int (List a)) (List a))) *)
    ( "nthcdr",
      poly [ "a" ] (fn [ Prim.int; list_t (tvar "a") ] (list_t (tvar "a"))) );
    (* append : (forall (a) (-> (&rest (List a)) (List a))) *)
    ( "append",
      poly [ "a" ] (TArrow ([ PRest (list_t (tvar "a")) ], list_t (tvar "a")))
    );
    (* reverse : (forall (a) (-> ((List a)) (List a))) *)
    ("reverse", poly [ "a" ] (fn [ list_t (tvar "a") ] (list_t (tvar "a"))));
    (* last : (forall (a) (-> ((List a)) (List a))) - returns last cons cell *)
    ("last", poly [ "a" ] (fn [ list_t (tvar "a") ] (list_t (tvar "a"))));
    (* mapcar : (forall (a b) (-> ((-> (a) b) (List a)) (List b))) *)
    ( "mapcar",
      poly [ "a"; "b" ]
        (fn
           [ fn [ tvar "a" ] (tvar "b"); list_t (tvar "a") ]
           (list_t (tvar "b"))) );
    (* member : (forall (a) (-> (a (List a)) (List a))) *)
    (* Returns the tail starting from the element, or nil *)
    ( "member",
      poly [ "a" ] (fn [ tvar "a"; list_t (tvar "a") ] (list_t (tvar "a"))) );
    (* memq : (forall (a) (-> (a (List a)) (List a))) *)
    ( "memq",
      poly [ "a" ] (fn [ tvar "a"; list_t (tvar "a") ] (list_t (tvar "a"))) );
    (* assoc : (forall (k v) (-> (k (List (List k v))) (Option (List k v)))) *)
    (* Simplified: association list returns option of pair *)
    ( "assoc",
      poly [ "k"; "v" ]
        (fn [ tvar "k"; list_t (list_t Prim.any) ] (option_t (list_t Prim.any)))
    );
    (* assq : similar to assoc *)
    ( "assq",
      poly [ "k"; "v" ]
        (fn [ tvar "k"; list_t (list_t Prim.any) ] (option_t (list_t Prim.any)))
    );
    (* =========================================================================
     Arithmetic operations
     ========================================================================= *)

    (* + : (-> (&rest Int) Int) *)
    ("+", mono (fn_rest Prim.int Prim.int));
    (* - : (-> (Int &rest Int) Int) *)
    ("-", mono (TArrow ([ PPositional Prim.int; PRest Prim.int ], Prim.int)));
    (* * : (-> (&rest Int) Int) *)
    ("*", mono (fn_rest Prim.int Prim.int));
    (* / : (-> (Int &rest Int) Int) *)
    ("/", mono (TArrow ([ PPositional Prim.int; PRest Prim.int ], Prim.int)));
    (* mod : (-> (Int Int) Int) *)
    ("mod", mono (fn [ Prim.int; Prim.int ] Prim.int));
    ("%", mono (fn [ Prim.int; Prim.int ] Prim.int));
    (* abs : (-> (Int) Int) *)
    ("abs", mono (fn [ Prim.int ] Prim.int));
    (* max : (-> (Int &rest Int) Int) *)
    ("max", mono (TArrow ([ PPositional Prim.int; PRest Prim.int ], Prim.int)));
    (* min : (-> (Int &rest Int) Int) *)
    ("min", mono (TArrow ([ PPositional Prim.int; PRest Prim.int ], Prim.int)));
    (* 1+ : (-> (Int) Int) *)
    ("1+", mono (fn [ Prim.int ] Prim.int));
    (* 1- : (-> (Int) Int) *)
    ("1-", mono (fn [ Prim.int ] Prim.int));
    (* logand : (-> (&rest Int) Int) - bitwise AND *)
    ("logand", mono (fn_rest Prim.int Prim.int));
    (* logior : (-> (&rest Int) Int) - bitwise inclusive OR *)
    ("logior", mono (fn_rest Prim.int Prim.int));
    (* logxor : (-> (&rest Int) Int) - bitwise exclusive OR *)
    ("logxor", mono (fn_rest Prim.int Prim.int));
    (* lognot : (-> (Int) Int) - bitwise NOT *)
    ("lognot", mono (fn [ Prim.int ] Prim.int));
    (* =========================================================================
     Comparison operations
     ========================================================================= *)

    (* < : (-> (Int &rest Int) Bool) *)
    ("<", mono (TArrow ([ PPositional Prim.int; PRest Prim.int ], Prim.bool)));
    (* > : (-> (Int &rest Int) Bool) *)
    (">", mono (TArrow ([ PPositional Prim.int; PRest Prim.int ], Prim.bool)));
    (* <= : (-> (Int &rest Int) Bool) *)
    ("<=", mono (TArrow ([ PPositional Prim.int; PRest Prim.int ], Prim.bool)));
    (* >= : (-> (Int &rest Int) Bool) *)
    (">=", mono (TArrow ([ PPositional Prim.int; PRest Prim.int ], Prim.bool)));
    (* = : (-> (Int &rest Int) Bool) *)
    ("=", mono (TArrow ([ PPositional Prim.int; PRest Prim.int ], Prim.bool)));
    (* =========================================================================
     Predicates
     ========================================================================= *)

    (* null : (-> (Any) Bool) *)
    ("null", mono (fn [ Prim.any ] Prim.bool));
    (* atom : (-> (Any) Bool) *)
    ("atom", mono (fn [ Prim.any ] Prim.bool));
    (* listp : (-> (Any) Bool) *)
    ("listp", mono (fn [ Prim.any ] Prim.bool));
    (* consp : (-> (Any) Bool) *)
    ("consp", mono (fn [ Prim.any ] Prim.bool));
    (* symbolp : (-> (Any) Bool) *)
    ("symbolp", mono (fn [ Prim.any ] Prim.bool));
    (* stringp : (-> (Any) Bool) *)
    ("stringp", mono (fn [ Prim.any ] Prim.bool));
    (* numberp : (-> (Any) Bool) *)
    ("numberp", mono (fn [ Prim.any ] Prim.bool));
    (* integerp : (-> (Any) Bool) *)
    ("integerp", mono (fn [ Prim.any ] Prim.bool));
    (* floatp : (-> (Any) Bool) *)
    ("floatp", mono (fn [ Prim.any ] Prim.bool));
    (* vectorp : (-> (Any) Bool) *)
    ("vectorp", mono (fn [ Prim.any ] Prim.bool));
    (* functionp : (-> (Any) Bool) *)
    ("functionp", mono (fn [ Prim.any ] Prim.bool));
    (* eq : (-> (Any Any) Bool) *)
    ("eq", mono (fn [ Prim.any; Prim.any ] Prim.bool));
    (* equal : (-> (Any Any) Bool) *)
    ("equal", mono (fn [ Prim.any; Prim.any ] Prim.bool));
    (* not : (-> (Any) Bool) *)
    ("not", mono (fn [ Prim.any ] Prim.bool));
    (* =========================================================================
     String operations
     ========================================================================= *)

    (* concat : (-> (&rest String) String) *)
    ("concat", mono (fn_rest Prim.string Prim.string));
    (* substring : (-> (String Int &optional Int) String) *)
    ( "substring",
      mono
        (TArrow
           ( [
               PPositional Prim.string; PPositional Prim.int; POptional Prim.int;
             ],
             Prim.string )) );
    (* string-length : (-> (String) Int) *)
    ("string-length", mono (fn [ Prim.string ] Prim.int));
    (* upcase : (-> (String) String) *)
    ("upcase", mono (fn [ Prim.string ] Prim.string));
    (* downcase : (-> (String) String) *)
    ("downcase", mono (fn [ Prim.string ] Prim.string));
    (* string-to-list : (-> (String) (List Int)) *)
    ("string-to-list", mono (fn [ Prim.string ] (list_t Prim.int)));
    (* format : (-> (String &rest Any) String) *)
    ( "format",
      mono (TArrow ([ PPositional Prim.string; PRest Prim.any ], Prim.string))
    );
    (* =========================================================================
     Vector operations
     ========================================================================= *)

    (* vector : (forall (a) (-> (&rest a) (Vector a))) *)
    ("vector", poly [ "a" ] (fn_rest (tvar "a") (vector_t (tvar "a"))));
    (* aref : (forall (a) (-> ((Vector a) Int) a)) *)
    ("aref", poly [ "a" ] (fn [ vector_t (tvar "a"); Prim.int ] (tvar "a")));
    (* aset : (forall (a) (-> ((Vector a) Int a) a)) *)
    ( "aset",
      poly [ "a" ] (fn [ vector_t (tvar "a"); Prim.int; tvar "a" ] (tvar "a"))
    );
    (* =========================================================================
     Symbol operations
     ========================================================================= *)

    (* symbol-name : (-> (Symbol) String) *)
    ("symbol-name", mono (fn [ Prim.symbol ] Prim.string));
    (* =========================================================================
     Type coercion
     ========================================================================= *)

    (* number-to-string : (-> (Int) String) *)
    ("number-to-string", mono (fn [ Prim.int ] Prim.string));
    (* string-to-number : (-> (String) Int) *)
    ("string-to-number", mono (fn [ Prim.string ] Prim.int));
    (* =========================================================================
     Control flow and function application
     ========================================================================= *)

    (* funcall : (forall (r) (-> (Any &rest Any) r)) - variadic, takes function + args *)
    ( "funcall",
      poly [ "r" ] (TArrow ([ PPositional Prim.any; PRest Prim.any ], tvar "r"))
    );
    (* apply : (forall (r) (-> (Any &rest Any) r)) - variadic, takes function + args *)
    ( "apply",
      poly [ "r" ] (TArrow ([ PPositional Prim.any; PRest Prim.any ], tvar "r"))
    );
    (* function : returns the function object, (forall (a) (-> (a) a)) for #'fn *)
    ("function", poly [ "a" ] (fn [ tvar "a" ] (tvar "a")));
    (* run-hooks : (-> (&rest Symbol) Nil) *)
    ("run-hooks", mono (fn_rest Prim.symbol Prim.nil));
    (* run-hook-with-args : (-> (Symbol &rest Any) Nil) *)
    ( "run-hook-with-args",
      mono (TArrow ([ PPositional Prim.symbol; PRest Prim.any ], Prim.nil)) );
    (* commandp : (-> (Any) Bool) - predicate for interactive commands *)
    ("commandp", mono (fn [ Prim.any ] Prim.bool));
    (* macroexpand : (-> (Any) Any) *)
    ("macroexpand", mono (fn [ Prim.any ] Prim.any));
    (* backtrace-frames : (-> () (List Any)) *)
    ("backtrace-frames", mono (fn [] (list_t Prim.any)));
  ]

(** Create a type environment with all built-in function signatures *)
let initial_env () : Env.t =
  List.fold_left
    (fun env (name, scheme) -> Env.extend name scheme env)
    Env.empty signatures
