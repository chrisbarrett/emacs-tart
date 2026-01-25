(** Tart: A type checker for Emacs Lisp *)

let version = "0.1.0"

module Location = Syntax.Location
(** Re-export syntax modules *)

module Sexp = Syntax.Sexp
module Lexer = Syntax.Lexer
module Parser = Syntax.Parser
module Read = Syntax.Read
