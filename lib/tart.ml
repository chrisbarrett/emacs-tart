(** Tart: A type checker for Emacs Lisp *)

let version = "0.1.0"

module Location = Syntax.Location
(** Re-export syntax modules *)

module Sexp = Syntax.Sexp
module Lexer = Syntax.Lexer
module Parser = Syntax.Parser
module Read = Syntax.Read

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
module Generalize = Typing.Generalize
module Check = Typing.Check
module Module_check = Typing.Module_check
module Diagnostic = Typing.Diagnostic

module Sig_ast = Sig.Sig_ast
(** Re-export signature modules *)

module Sig_parser = Sig.Sig_parser
module Sig_loader = Sig.Sig_loader
module Search_path = Sig.Search_path

module Rpc = Lsp.Rpc
(** Re-export LSP modules *)

module Protocol = Lsp.Protocol
module Server = Lsp.Server
