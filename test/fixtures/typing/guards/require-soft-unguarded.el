;; test: emacs-version 31.0
;; Negated guard: else branch does NOT have guarded names (Spec 49 R12)
;;
;; hidden-fn is in testmod.tart but its name doesn't match the testmod
;; prefix, so autoload detection can't find it. Only a guard or require
;; makes it available.

(defun use-hidden-else ()
  "Call in else-branch of guard — should be an error."
  (if (featurep 'testmod)
      (hidden-fn 42)
    (hidden-fn 99)))
