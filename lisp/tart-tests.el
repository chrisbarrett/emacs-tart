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
(require 'tart-test-helpers)

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

;;; Explicit Instantiation Tests

(ert-deftest tart-instantiation-expands-to-call ()
  "The `tart' macro with vector should expand to a function call."
  (should (equal (macroexpand '(tart [int] identity 42))
                 '(identity 42)))
  (should (equal (macroexpand '(tart [list int string] fmap f xs))
                 '(fmap f xs)))
  (should (equal (macroexpand '(tart [_ string] pair 1 "hi"))
                 '(pair 1 "hi"))))

(ert-deftest tart-instantiation-evaluates-call ()
  "The `tart' macro with vector should evaluate the function call at runtime."
  (should (equal (tart [int] identity 42) 42))
  (should (equal (tart [string] identity "hello") "hello"))
  (should (equal (tart [int] + 1 2 3) 6)))

(ert-deftest tart-instantiation-with-no-type-args ()
  "The `tart' macro with empty vector should work with nullary functions."
  (should (equal (macroexpand '(tart [] point))
                 '(point))))

;;; Version Skip Tests

(ert-deftest tart-test-skip-unless-version-expands ()
  "The `tart-test-skip-unless-version' macro should expand to a version check."
  (let ((expansion (macroexpand '(tart-test-skip-unless-version 29))))
    (should (equal expansion
                   '(when (< emacs-major-version 29)
                      (ert-skip (format "Requires Emacs %d+" 29)))))))

(ert-deftest tart-test-skip-unless-version-passes-current ()
  "The macro should not skip when the version is satisfied."
  ;; emacs-major-version is always >= 28 for any version we support
  (tart-test-skip-unless-version 28)
  (should t))

;;; tart-tests.el ends here
