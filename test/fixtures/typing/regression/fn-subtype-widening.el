;; Test: function subtype widening (Spec 83)
;; test: emacs-version 31.0
;;
;; Fixed-arity functions are subtypes of rest-parameter functions:
;;   (a₁ ... aₙ) -> r  <:  (&rest T) -> R  when each aᵢ <: T and r <: R
;;
;; This is the core issue: generalized defun types (TForall) must
;; unify with expected arrow types via implicit instantiation.

;; Case 1: defun with &rest referenced via #' in add-hook
;; Generalized type: (forall (a) (-> (any &rest a) string))
;; Expected: (symbol | ((&rest any) -> any))
(defun fn-with-rest (x &rest args)
  (message "x=%s" x))
(add-hook 'some-hook #'fn-with-rest)

;; Case 2: defun with &optional referenced via #'
(defun fn-with-opt (x &optional y)
  (message "x=%s y=%s" x y))
(add-hook 'some-hook #'fn-with-opt)

;; Case 3: defun with both &optional and &rest
(defun fn-complex (x &optional y &rest args)
  (message "x=%s y=%s" x y))
(add-hook 'some-hook #'fn-complex)

;; Case 4: zero-arity lambda
(add-hook 'after-init-hook (lambda () (message "started")))

;; Case 5: fixed-arity lambda
(add-hook 'some-hook (lambda (x) (message "got %s" x)))

;; Case 6: multi-arg lambda
(add-hook 'some-hook (lambda (x y) (message "%s %s" x y)))

;; Case 7: run-hook-wrapped with zero-arity lambda
(run-hook-wrapped 'my-hook (lambda () t))

;; Case 8: remove-hook with generalized function
(defun my-change-fn (beg end len)
  (message "%d %d %d" beg end len))
(remove-hook 'after-change-functions #'my-change-fn)
