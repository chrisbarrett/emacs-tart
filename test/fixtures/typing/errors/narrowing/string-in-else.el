;; Predicate narrowing error: string function used in else-branch of stringp
;; In the else-branch, x is narrowed from (string | int) to int,
;; so string-to-char should fail.
;; test: emacs-version 31.0

(defun wrong-branch (x)
  (declare (tart ((string | int)) -> int))
  (if (stringp x)
      0
    (string-to-char x)))
