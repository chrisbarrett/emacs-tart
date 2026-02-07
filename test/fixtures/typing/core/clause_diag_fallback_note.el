;; test: emacs-version 31.0
;;
;; Spec 57 R5/R6: Multi-clause defun with diagnostic on fallback clause.
;; When functionp is called with a non-function type, the wildcard clause
;; matches and emits a note.

(defun test-fn (x)
  (declare (tart (int) -> nil))
  (functionp x))
