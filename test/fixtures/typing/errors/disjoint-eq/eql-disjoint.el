;; Disjoint equality: string and symbol can never be eql
;; test: emacs-version 31.0

(defun wrong-eql (s sym)
  (declare (tart (string symbol) -> bool))
  (eql s sym))
