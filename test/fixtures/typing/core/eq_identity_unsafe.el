;; eq/eql with identity-unsafe types: produces warnings
;; test: emacs-version 31.0

;; eq with strings warns about identity comparison
(defun eq-strings (a b)
  (declare (tart (string string) -> bool))
  (eq a b))

;; eql with strings warns similarly
(defun eql-strings (a b)
  (declare (tart (string string) -> bool))
  (eql a b))
