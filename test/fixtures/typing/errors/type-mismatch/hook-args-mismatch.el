;; Test: run-hook-with-args type mismatch (Spec 92)
;; test: emacs-version 31.0
;;
;; run-hook-with-args rejects wrong argument types for a typed hook.

(defvar my-filter-hook nil)
(tart-declare my-filter-hook (list ((string) -> any)))

;; Passing int where string expected
(run-hook-with-args 'my-filter-hook 42)
