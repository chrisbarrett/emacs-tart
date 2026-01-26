(** Parser for signature file syntax.

    This module converts S-expressions into signature AST nodes. It handles the
    type syntax including:
    - Primitive types: int, string, nil, t, truthy, never, etc.
    - Type variables: a, b (when in scope of quantifiers)
    - Function types: (params) -> return
    - Type applications: (list int), (option string)
    - Union types: (int | string)
    - Quantifiers: [a b] at start of arrows or after defun name
    - Bounded quantifiers: [(a : truthy)] *)

(** {1 Parse Errors} *)

type parse_error = { message : string; span : Syntax.Location.span }
(** Parse error with location *)

(** {1 Type Parsing} *)

val parse_sig_type : Syntax.Sexp.t -> (Sig_ast.sig_type, parse_error) result
(** Parse a type expression from an S-expression *)

val parse_tvar_binders :
  Syntax.Sexp.t -> (Sig_ast.tvar_binder list, parse_error) result
(** Parse a list of type variable binders from a vector [...] *)

(** {1 Declaration Parsing} *)

val parse_decl : Syntax.Sexp.t -> (Sig_ast.decl, parse_error) result
(** Parse a single declaration from an S-expression *)

(** {1 Signature File Parsing} *)

val parse_signature :
  module_name:string ->
  Syntax.Sexp.t list ->
  (Sig_ast.signature, parse_error list) result
(** Parse a complete signature file from S-expressions.

    @param module_name The module name (derived from filename)
    @param sexps The parsed S-expressions from the file
    @return Either the parsed signature or a list of parse errors *)
