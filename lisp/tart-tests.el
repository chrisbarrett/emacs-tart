;;; tart-tests.el --- Tests for tart.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Barrett

;; Author: Chris Barrett
;; Keywords: languages, lisp, tools

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

;; ERT tests for tart.el type annotation macros and other functionality.

;;; Code:

(require 'ert)
(require 'tart)

;;; Macro Expansion Tests (R8)

(ert-deftest tart-macro-expands-to-form ()
  "The `tart' macro should expand to its FORM argument."
  (should (equal (macroexpand '(tart string x)) 'x))
  (should (equal (macroexpand '(tart int 42)) 42))
  (should (equal (macroexpand '(tart (list int) (list 1 2 3)))
                 '(list 1 2 3))))

(ert-deftest tart-macro-evaluates-form ()
  "The `tart' macro should evaluate FORM at runtime."
  (should (equal (tart string "hello") "hello"))
  (should (equal (tart int (+ 1 2)) 3))
  (should (equal (tart (list int) (list 1 2 3)) '(1 2 3))))

(ert-deftest tart-type-macro-expands-to-nil ()
  "The `tart-type' macro should expand to nil."
  (should (eq (macroexpand '(tart-type foo int)) nil))
  (should (eq (macroexpand '(tart-type int-pair (tuple int int))) nil))
  (should (eq (macroexpand '(tart-type predicate [a] ((a) -> bool))) nil)))

(ert-deftest tart-type-macro-evaluates-to-nil ()
  "The `tart-type' macro should evaluate to nil at runtime."
  (should (eq (tart-type foo int) nil))
  (should (eq (tart-type int-pair (tuple int int)) nil))
  (should (eq (tart-type mapping [k v] (hash-table k v)) nil)))

(ert-deftest tart-declare-macro-expands-to-nil ()
  "The `tart-declare' macro should expand to nil."
  (should (eq (macroexpand '(tart-declare x int)) nil))
  (should (eq (macroexpand '(tart-declare my-buffer buffer)) nil))
  (should (eq (macroexpand '(tart-declare handler ((string) -> nil))) nil)))

(ert-deftest tart-declare-macro-evaluates-to-nil ()
  "The `tart-declare' macro should evaluate to nil at runtime."
  (should (eq (tart-declare x int) nil))
  (should (eq (tart-declare my-buffer buffer) nil))
  (should (eq (tart-declare handler ((string) -> nil)) nil)))

;;; tart-tests.el ends here
