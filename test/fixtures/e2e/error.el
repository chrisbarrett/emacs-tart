;;; error.el --- Test fixture with type errors  -*- lexical-binding: t; -*-

;;; Commentary:

;; A module with intentional type errors for testing diagnostics.

;;; Code:

(defun broken-upcase (x)
  "Try to upcase X, but pass wrong type."
  (upcase 42))  ; error: upcase expects string, got int

(defun wrong-return ()
  "Return wrong type."
  "hello")  ; declared as int, returns string

(provide 'error)
;;; error.el ends here
