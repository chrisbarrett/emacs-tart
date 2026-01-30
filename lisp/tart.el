;;; tart.el --- Type annotation macros for the Tart type checker  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Barrett

;; Author: Chris Barrett
;; Keywords: languages, lisp, tools
;; Package-Requires: ((emacs "24.1"))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Type annotation macros for the Tart type system for Emacs Lisp.
;;
;; This package provides macros that are recognized by the Tart type checker
;; but expand to no-ops at runtime.  This allows you to add inline type
;; annotations to your Emacs Lisp code without affecting runtime behavior.
;;
;; The macros are:
;;
;; - `tart': Assert that an expression has a specific type, or explicitly
;;           instantiate a polymorphic function call when first arg is a vector
;; - `tart-type': Define a file-local type alias
;; - `tart-declare': Declare a variable's type
;;
;; This package has NO dependencies and is safe to require in any Emacs Lisp
;; file.  For development tools (LSP integration, REPL, etc.), see `tart-mode'.

;;; Code:

;;;###autoload
(defmacro tart (type-or-types &rest args)
  "Type annotation macro for the Tart type checker.

Two forms are supported:

1. Type assertion: (tart TYPE FORM)
   TYPE is a type expression (ignored at runtime).
   FORM is evaluated and returned unchanged.
   At compile time, this asserts that FORM has the specified TYPE.

2. Explicit instantiation: (tart [TYPES...] FN ARGS...)
   TYPES is a vector of type arguments to apply.
   FN is the polymorphic function to call.
   ARGS are the arguments to pass to FN.
   The underscore `_' can be used as a placeholder for inference.

At runtime, both forms simply evaluate to their result.

Examples:
  (tart string \"hello\")              ; Assert type
  (tart int (+ 1 2))                  ; Assert type
  (tart [int] identity 42)            ; Instantiate at int
  (tart [list int string] fmap f xs)  ; Instantiate with explicit types
  (tart [_ string] pair 1 \"hi\")       ; Infer first type, fix second"
  (if (vectorp type-or-types)
      ;; Explicit instantiation: (tart [types...] fn args...)
      (let ((fn (car args))
            (fn-args (cdr args)))
        `(,fn ,@fn-args))
    ;; Type assertion: (tart type form)
    (car args)))

;;;###autoload
(defmacro tart-type (_name &rest _definition)
  "Define a file-local type alias for the Tart type checker.
NAME is the alias name.
DEFINITION is the type expression, optionally preceded by [VARS]
for type parameters.

This form is recognized by the Tart type checker to define a type alias
that is available within the current file.  At runtime, this expands to nil.

Example:
  (tart-type int-pair (tuple int int))
  (tart-type predicate [a] ((a) -> bool))"
  nil)

;;;###autoload
(defmacro tart-declare (_name _type)
  "Declare a variable's type for the Tart type checker.
NAME is the variable name.
TYPE is the type expression.

This form declares that NAME has the specified TYPE, allowing the
type checker to verify subsequent reads and writes.  At runtime,
this expands to nil.

Example:
  (tart-declare my-buffer buffer)
  (defvar my-buffer)"
  nil)

(provide 'tart)
;;; tart.el ends here
