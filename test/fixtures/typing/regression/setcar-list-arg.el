;; Regression: setcar should accept (list a) in addition to (cons b c)
;; Fixed: Spec 76 Iteration 4 (validation against Emacs lisp/)
;; test: emacs-version 31.0

;; A list variable should be accepted by setcar's (list a) clause.
(defun set-head (xs val)
  (declare (tart ((list int) int) -> int))
  (setcar xs val))

;; setcar with (cons a b) should still work for heterogeneous cons cells.
(defun set-cons-car (cell val)
  (declare (tart ((cons int string) int) -> int))
  (setcar cell val))
