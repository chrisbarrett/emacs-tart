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

(** {1 Errors} *)

module Error = Error
(** Unified error type for all subsystems. *)

(** {1 Syntax} *)

module Location = Syntax.Location
module Sexp = Syntax.Sexp
module Lexer = Syntax.Lexer
module Parser = Syntax.Parser
module Read = Syntax.Read
module Print = Syntax.Print

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
module Narrow = Typing.Narrow
module Generalize = Typing.Generalize
module Check = Typing.Check
module Module_check = Typing.Module_check
module Diagnostic = Typing.Diagnostic
module Levenshtein = Typing.Levenshtein
module Exhaustiveness = Typing.Exhaustiveness
module Kind = Typing.Kind
module Kind_infer = Typing.Kind_infer

(** {1 Signatures} *)

module Sig_ast = Sig.Sig_ast
module Sig_parser = Sig.Sig_parser
module Sig_loader = Sig.Sig_loader
module Search_path = Sig.Search_path
module Emacs_version = Sig.Emacs_version

(** {1 LSP} *)

module Rpc = Lsp.Rpc
module Protocol = Lsp.Protocol
module Server = Lsp.Server

(** {1 Graph} *)

module Dependency_graph = Graph.Dependency_graph
module Graph_builder = Graph.Graph_builder

(** {1 File Errors} *)

module File_error = Errors.File_error

(** {1 Coverage} *)

module Definition_extractor = Coverage.Definition_extractor
module File_scanner = Coverage.File_scanner
module Coverage_report = Coverage.Coverage_report
module Report_format = Coverage.Report_format
module Emacs_source = Coverage.Emacs_source
module C_scanner = Coverage.C_scanner
module Emacs_coverage = Coverage.Emacs_coverage

(** {1 Logging} *)

module Log = Tart_log.Log
