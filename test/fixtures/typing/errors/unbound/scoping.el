;; Unbound identifier: variable used outside its let scope
;; Error: Variable defined in let is not visible outside

(defun use-out-of-scope ()
  "Try to use a let-bound variable after the let form."
  (let ((local-var 42))
    (+ local-var 1))
  ;; local-var is no longer in scope here
  local-var)
