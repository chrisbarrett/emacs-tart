(** Tart: A type checker for Emacs Lisp.

    Tart provides static type analysis for Emacs Lisp, based on Hindley-Milner
    type inference with levels-based generalization.

    {1 Quick Start}

    {[
      (* Parse and type-check an expression *)
      let sexp = Tart.Read.parse_one_exn "(+ 1 2)" in
      let ty, errors = Tart.Check.check_expr sexp in
      assert (errors = []);
      assert (Tart.Types.equal ty Tart.Types.Prim.int)

      (* Type-check a program with defuns *)
      let sexps = Tart.Read.parse_string_exn "
        (defun double (x) (+ x x))
        (double 21)
      " in
      let result = Tart.Check.check_program sexps in
      assert (result.errors = [])
    ]}

    {1 Modules}

    The library is organized into several sub-libraries:
    - {!Syntax} - Parsing and S-expression AST
    - {!Interp} - Pure Elisp interpreter for macro expansion
    - {!Core} - Type representation
    - {!Typing} - Type inference and checking *)

val version : string
(** Library version. *)

(** {1 Syntax} *)

module Location = Syntax.Location
module Sexp = Syntax.Sexp
module Lexer = Syntax.Lexer
module Parser = Syntax.Parser
module Read = Syntax.Read

(** {1 Interpreter} *)

module Value = Interp.Value
module Env = Interp.Env
module Builtin = Interp.Builtin
module Eval = Interp.Eval
module Expand = Interp.Expand

(** {1 Types} *)

module Types = Core.Types
module Type_env = Core.Type_env

(** {1 Type Checking} *)

module Constraint = Typing.Constraint
module Infer = Typing.Infer
module Unify = Typing.Unify
module Generalize = Typing.Generalize
module Check = Typing.Check
module Module_check = Typing.Module_check
module Diagnostic = Typing.Diagnostic
module Levenshtein = Typing.Levenshtein

(** {1 Signatures} *)

module Sig_ast = Sig.Sig_ast
module Sig_parser = Sig.Sig_parser
module Sig_loader = Sig.Sig_loader
module Search_path = Sig.Search_path

(** {1 LSP} *)

module Rpc = Lsp.Rpc
module Protocol = Lsp.Protocol
module Server = Lsp.Server
