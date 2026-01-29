;; Unbound identifier: reference to undefined variable
;; Error: Referencing a variable that was never defined

(defun use-undefined-var ()
  "Try to use a variable that doesn't exist."
  (+ undefined-var 1))
