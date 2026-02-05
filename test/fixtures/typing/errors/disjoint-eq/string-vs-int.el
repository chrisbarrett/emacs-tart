;; Disjoint equality: string and int can never be eq
;; test: emacs-version 31.0

(defun compare-wrong (s n)
  (declare (tart (string int) -> bool))
  (eq s n))
