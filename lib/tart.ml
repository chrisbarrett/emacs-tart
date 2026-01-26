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
module Diagnostic = Typing.Diagnostic

module Rpc = Lsp.Rpc
(** Re-export LSP modules *)

module Protocol = Lsp.Protocol
module Server = Lsp.Server
