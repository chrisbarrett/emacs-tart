;; Test: funcall detects type errors in arguments
;; test: emacs-version 31.0

(defun add-one (n)
  (1+ n))

;; Error: passing string to function expecting int
(funcall #'add-one "not a number")
