;;; valid.el --- Test fixture with correct types  -*- lexical-binding: t; -*-

;;; Commentary:

;; A simple module with correct type annotations for testing.

;;; Code:

(defun greet (name)
  "Greet NAME."
  (concat "Hello, " name))

(defun add-numbers (a b)
  "Add A and B."
  (+ a b))

(defvar greeting-prefix "Hello"
  "The greeting prefix.")

(provide 'valid)
;;; valid.el ends here
