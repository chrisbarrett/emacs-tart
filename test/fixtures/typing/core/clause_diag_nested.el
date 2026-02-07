;; test: emacs-version 31.0
;;
;; Spec 57 R7: Clause diagnostics propagate through nested expressions.
;; A diagnostic from interactive-p inside a let binding still appears.

(defun test-fn ()
  (declare (tart () -> bool))
  (let ((result (interactive-p)))
    result))
