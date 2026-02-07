;; test: emacs-version 31.0
;; Feature guard unlocks non-autoloaded names in then-branch (Spec 49 R1)
;;
;; hidden-fn is in testmod.tart but autoload doesn't discover it.
;; A featurep guard makes it available in the when body.

(defun maybe-use-hidden ()
  "Guarded access to hidden-fn — should pass."
  (when (featurep 'testmod)
    (hidden-fn 42)))
