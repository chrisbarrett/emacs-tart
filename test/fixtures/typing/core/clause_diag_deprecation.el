;; test: emacs-version 31.0
;;
;; Spec 57 R1: Single-clause defun with warn diagnostic.
;; Calling a deprecated function emits a warning.

(defun test-fn ()
  (declare (tart () -> bool))
  (interactive-p))
