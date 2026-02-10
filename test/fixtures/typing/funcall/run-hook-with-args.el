;; Test: run-hook-with-args arity checking (Spec 92)
;; test: emacs-version 31.0
;;
;; run-hook-with-args validates arguments against hook contract.

(defvar my-change-hook nil)
(tart-declare my-change-hook (list ((int int int) -> any)))

(defvar my-filter-hook nil)
(tart-declare my-filter-hook (list ((string) -> any)))

(defvar my-untyped-hook nil)
(tart-declare my-untyped-hook (list any))

;; Case 1: correct number and type of args → PASS
(run-hook-with-args 'my-change-hook 1 2 3)

;; Case 2: correct single-arg hook → PASS
(run-hook-with-args 'my-filter-hook "hello")

;; Case 3: untyped hook, any args → PASS (opt-out)
(run-hook-with-args 'my-untyped-hook 1 "two" nil)

;; Case 4: non-quoted hook name → PASS (skip validation)
(defun run-dynamic-hook (hook-name)
  (declare (tart (symbol) -> any))
  (run-hook-with-args hook-name 1 2 3))

;; Case 5: run-hook-with-args-until-success → PASS
(run-hook-with-args-until-success 'my-change-hook 1 2 3)

;; Case 6: run-hook-with-args-until-failure → PASS
(run-hook-with-args-until-failure 'my-change-hook 1 2 3)
