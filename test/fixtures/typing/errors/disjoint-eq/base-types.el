;; Disjoint equality: int and symbol can never be eq
;; test: emacs-version 31.0

(defun always-nil ()
  (declare (tart () -> bool))
  (eq 1 'foo))
