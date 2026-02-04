;; Type mismatch: incompatible instantiations of type variable
;; Error: mapcar requires the function arg type to match list element type

(defun broken-mapcar ()
  "Try to map a number function over a list of strings."
  (mapcar #'1+ (list "a" "b" "c")))
