;; Test: hook arity mismatch (Spec 92)
;; test: emacs-version 31.0
;;
;; add-hook rejects function with wrong arity for a typed hook.

(defvar my-change-hook nil)
(tart-declare my-change-hook (list ((int int int) -> any)))

;; lambda () has 0 params, hook expects (int int int) -> any
(add-hook 'my-change-hook (lambda () (message "changed")))
