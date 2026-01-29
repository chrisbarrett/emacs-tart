;; Type mismatch: incompatible instantiations of type variable
;; Error: cons expects the element type to match the list's element type

(defun broken-cons ()
  "Try to cons a string onto a list of integers."
  (let ((numbers (list 1 2 3)))
    (cons "not a number" numbers)))
