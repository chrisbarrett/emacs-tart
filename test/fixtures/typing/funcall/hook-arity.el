;; Test: hook arity checking (Spec 92)
;; test: emacs-version 31.0
;;
;; add-hook validates function argument against hook contract when the
;; hook variable type is (list F) where F is a function type.

;; Set up typed hook variables for testing
(defvar my-normal-hook nil)
(tart-declare my-normal-hook (list ((nil) -> any)))

(defvar my-change-hook nil)
(tart-declare my-change-hook (list ((int int int) -> any)))

(defvar my-untyped-hook nil)
(tart-declare my-untyped-hook (list any))

;; Case 1: add-hook with matching zero-arg function → PASS
(add-hook 'my-normal-hook (lambda () (message "saved")))

;; Case 2: add-hook with matching three-arg function → PASS
(add-hook 'my-change-hook (lambda (beg end len) (message "%d" len)))

;; Case 3: add-hook with untyped hook → PASS (opt-out)
(add-hook 'my-untyped-hook (lambda () (message "done")))

;; Case 4: add-hook with non-quoted hook → PASS (skip validation)
(defun add-to-hook (hook fn)
  (declare (tart (symbol ((&rest any) -> any)) -> nil))
  (add-hook hook fn))

;; Case 5: add-hook with named function reference → PASS
(defun my-handler (beg end len)
  (declare (tart (int int int) -> any))
  (message "changed %d" len))

(add-hook 'my-change-hook #'my-handler)
