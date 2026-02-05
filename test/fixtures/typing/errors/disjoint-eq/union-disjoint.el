;; Disjoint: (int | float) is disjoint from symbol
;; test: emacs-version 31.0

(defun compare-num-sym (n sym)
  (declare (tart (num symbol) -> bool))
  (eq n sym))
