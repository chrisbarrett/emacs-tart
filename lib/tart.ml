(** Tart: A type checker for Emacs Lisp *)

let version = "0.1.0"

module Error = Error
(** Re-export unified error module *)

module Location = Syntax.Location
(** Re-export syntax modules *)

module Sexp = Syntax.Sexp
module Lexer = Syntax.Lexer
module Parser = Syntax.Parser
module Read = Syntax.Read
module Print = Syntax.Print

module Value = Interp.Value
(** Re-export interpreter modules *)

module Env = Interp.Env
module Builtin = Interp.Builtin
module Eval = Interp.Eval
module Expand = Interp.Expand

module Types = Core.Types
(** Re-export core type system modules *)

module Type_env = Core.Type_env

module Constraint = Typing.Constraint
(** Re-export typing modules *)

module Infer = Typing.Infer
module Unify = Typing.Unify
module Narrow = Typing.Narrow
module Generalize = Typing.Generalize
module Check = Typing.Check
module Module_check = Typing.Module_check
module Diagnostic = Typing.Diagnostic
module Diagnostic_format = Typing.Diagnostic_format
module Levenshtein = Typing.Levenshtein
module Exhaustiveness = Typing.Exhaustiveness
module Clause_dispatch = Typing.Clause_dispatch
module Row_dispatch = Typing.Row_dispatch
module Kind = Typing.Kind
module Kind_infer = Typing.Kind_infer

module Sig_ast = Sig.Sig_ast
(** Re-export signature modules *)

module Sig_parser = Sig.Sig_parser
module Sig_loader = Sig.Sig_loader
module Search_path = Sig.Search_path
module Emacs_version = Sig.Emacs_version
module Package_header = Sig.Package_header

module Rpc = Lsp.Rpc
(** Re-export LSP modules *)

module Protocol = Lsp.Protocol
module Server = Lsp.Server

module Dependency_graph = Graph.Dependency_graph
(** Re-export graph modules *)

module Graph_builder = Graph.Graph_builder

module File_error = Errors.File_error
(** Re-export file error module *)

module Definition_extractor = Coverage.Definition_extractor
(** Re-export coverage modules *)

module File_scanner = Coverage.File_scanner
module Coverage_report = Coverage.Coverage_report
module Report_format = Coverage.Report_format
module Emacs_source = Coverage.Emacs_source
module C_scanner = Coverage.C_scanner
module Emacs_coverage = Coverage.Emacs_coverage

module Emacs_reader = Oracle.Emacs_reader
(** Re-export oracle modules *)

module Log = Tart_log.Log
(** Re-export structured logging module *)

module Timing = Tart_timing.Timing
(** Re-export timing module *)

module Memory_stats = Tart_memory_stats.Memory_stats
(** Re-export memory statistics module *)
