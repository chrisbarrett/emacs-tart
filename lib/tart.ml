(** Tart: A type checker for Emacs Lisp *)

let version = "0.1.0"

module Location = Syntax.Location
(** Re-export syntax modules *)

module Sexp = Syntax.Sexp
module Lexer = Syntax.Lexer
module Parser = Syntax.Parser
module Read = Syntax.Read

(** Re-export interpreter modules *)
module Value = Interp.Value
module Env = Interp.Env
module Builtin = Interp.Builtin
module Eval = Interp.Eval
module Expand = Interp.Expand

(** Re-export core type system modules *)
module Types = Core.Types
module Type_env = Core.Type_env

(** Re-export typing modules *)
module Constraint = Typing.Constraint
module Infer = Typing.Infer
module Unify = Typing.Unify
module Generalize = Typing.Generalize
module Check = Typing.Check
module Diagnostic = Typing.Diagnostic

(** Re-export LSP modules *)
module Rpc = Lsp.Rpc
