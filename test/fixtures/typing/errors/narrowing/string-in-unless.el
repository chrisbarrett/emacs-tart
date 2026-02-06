;; Predicate narrowing error: string function used in unless body
;; In unless (stringp x), x is narrowed from (string | int) to int,
;; so string-to-char should fail.
;; test: emacs-version 31.0

(defun wrong-unless (x)
  (declare (tart ((string | int)) -> int))
  (unless (stringp x)
    (string-to-char x)))
