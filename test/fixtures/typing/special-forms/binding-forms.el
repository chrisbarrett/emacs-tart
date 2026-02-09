;; gv-letplace and macroexp-let2 expand to let — bindings should be visible
;; test: emacs-version 31.0

(defun use-gv-letplace ()
  (gv-letplace (getter setter) 42
    (+ getter 1)))

(defun use-macroexp-let2 ()
  (macroexp-let2 macroexp-copyable-p v 10
    (+ v 1)))
