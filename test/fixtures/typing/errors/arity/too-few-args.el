;; Arity error: missing required arguments
;; Error: % requires exactly 2 arguments (modulo operation)

(defun broken-modulo ()
  "Call % with only one argument."
  (% 10))
