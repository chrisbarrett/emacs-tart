;; Literal type widened to base type produces mismatch with different base
;; A string literal "hello" widens to string, which mismatches declared int
;; test: emacs-version 31.0

(defun wrong-return-type ()
  (declare (tart () -> int))
  "hello")
