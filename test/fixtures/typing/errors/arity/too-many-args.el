;; Arity error: excess arguments to fixed-arity function
;; Error: 1+ takes exactly 1 argument (increment by one)

(defun broken-increment ()
  "Call 1+ with too many arguments."
  (1+ 5 10))
