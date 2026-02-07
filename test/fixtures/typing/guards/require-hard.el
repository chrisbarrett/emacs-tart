;; test: emacs-version 31.0
;; Hard require extends env for subsequent forms (Spec 49 R5)
;;
;; hidden-fn is in testmod.tart but autoload can't find it by prefix.
;; A hard require loads all names from the module.

(require 'testmod)

(defun use-hidden-after-require ()
  "Call hidden-fn after hard require — should pass."
  (hidden-fn 42))
